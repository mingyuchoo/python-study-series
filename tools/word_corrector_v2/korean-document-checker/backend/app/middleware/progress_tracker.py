"""
진행률 추적 미들웨어

장시간 실행되는 작업의 진행 상황을 추적하고
클라이언트에게 실시간 피드백을 제공하는 미들웨어입니다.
"""

import asyncio
import logging
from typing import Dict, Any, Optional, Callable
from datetime import datetime, timedelta
from fastapi import Request
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.responses import Response

logger = logging.getLogger(__name__)


class ProgressTracker:
    """진행률 추적 클래스"""
    
    def __init__(self):
        self._progress_data: Dict[str, Dict[str, Any]] = {}
        self._lock = asyncio.Lock()
    
    async def start_progress(
        self,
        task_id: str,
        total_steps: int,
        description: str = ""
    ) -> None:
        """
        진행률 추적을 시작합니다.
        
        Args:
            task_id: 작업 고유 ID
            total_steps: 전체 단계 수
            description: 작업 설명
        """
        async with self._lock:
            self._progress_data[task_id] = {
                "task_id": task_id,
                "description": description,
                "total_steps": total_steps,
                "current_step": 0,
                "progress": 0.0,
                "status": "started",
                "start_time": datetime.now(),
                "last_update": datetime.now(),
                "estimated_remaining": None,
                "current_step_name": "",
                "message": "작업을 시작합니다..."
            }
        
        logger.info(f"Progress tracking started for task: {task_id}")
    
    async def update_progress(
        self,
        task_id: str,
        current_step: int,
        step_name: str = "",
        message: str = "",
        estimated_remaining: Optional[int] = None
    ) -> None:
        """
        진행률을 업데이트합니다.
        
        Args:
            task_id: 작업 고유 ID
            current_step: 현재 단계
            step_name: 현재 단계 이름
            message: 진행 상태 메시지
            estimated_remaining: 예상 남은 시간 (초)
        """
        async with self._lock:
            if task_id not in self._progress_data:
                logger.warning(f"Progress data not found for task: {task_id}")
                return
            
            data = self._progress_data[task_id]
            data["current_step"] = current_step
            data["progress"] = current_step / data["total_steps"] if data["total_steps"] > 0 else 0
            data["current_step_name"] = step_name
            data["message"] = message or f"단계 {current_step}/{data['total_steps']} 진행 중"
            data["last_update"] = datetime.now()
            data["status"] = "in_progress"
            
            if estimated_remaining is not None:
                data["estimated_remaining"] = estimated_remaining
            
            # 자동으로 예상 시간 계산
            if current_step > 0:
                elapsed = (datetime.now() - data["start_time"]).total_seconds()
                avg_time_per_step = elapsed / current_step
                remaining_steps = data["total_steps"] - current_step
                data["estimated_remaining"] = int(avg_time_per_step * remaining_steps)
        
        logger.info(f"Progress updated for task {task_id}: {current_step}/{data['total_steps']} - {step_name}")
    
    async def complete_progress(self, task_id: str, message: str = "작업이 완료되었습니다") -> None:
        """
        진행률 추적을 완료합니다.
        
        Args:
            task_id: 작업 고유 ID
            message: 완료 메시지
        """
        async with self._lock:
            if task_id not in self._progress_data:
                logger.warning(f"Progress data not found for task: {task_id}")
                return
            
            data = self._progress_data[task_id]
            data["current_step"] = data["total_steps"]
            data["progress"] = 1.0
            data["status"] = "completed"
            data["message"] = message
            data["last_update"] = datetime.now()
            data["estimated_remaining"] = 0
            
            # 완료된 작업은 5분 후 자동 삭제
            asyncio.create_task(self._cleanup_task(task_id, delay=300))
        
        logger.info(f"Progress completed for task: {task_id}")
    
    async def fail_progress(self, task_id: str, error_message: str) -> None:
        """
        진행률 추적을 실패로 표시합니다.
        
        Args:
            task_id: 작업 고유 ID
            error_message: 오류 메시지
        """
        async with self._lock:
            if task_id not in self._progress_data:
                logger.warning(f"Progress data not found for task: {task_id}")
                return
            
            data = self._progress_data[task_id]
            data["status"] = "failed"
            data["message"] = error_message
            data["last_update"] = datetime.now()
            
            # 실패한 작업은 1분 후 자동 삭제
            asyncio.create_task(self._cleanup_task(task_id, delay=60))
        
        logger.error(f"Progress failed for task {task_id}: {error_message}")
    
    async def get_progress(self, task_id: str) -> Optional[Dict[str, Any]]:
        """
        진행률 정보를 조회합니다.
        
        Args:
            task_id: 작업 고유 ID
            
        Returns:
            진행률 정보 또는 None
        """
        async with self._lock:
            return self._progress_data.get(task_id)
    
    async def _cleanup_task(self, task_id: str, delay: int = 300) -> None:
        """
        지정된 시간 후 작업 데이터를 정리합니다.
        
        Args:
            task_id: 작업 고유 ID
            delay: 지연 시간 (초)
        """
        await asyncio.sleep(delay)
        async with self._lock:
            if task_id in self._progress_data:
                del self._progress_data[task_id]
                logger.info(f"Progress data cleaned up for task: {task_id}")


# 전역 진행률 추적기 인스턴스
progress_tracker = ProgressTracker()


class ProgressTrackingMiddleware(BaseHTTPMiddleware):
    """진행률 추적 미들웨어"""
    
    async def dispatch(self, request: Request, call_next):
        """
        요청을 처리하고 진행률 추적 정보를 헤더에 추가합니다.
        
        Args:
            request: FastAPI Request 객체
            call_next: 다음 미들웨어 또는 엔드포인트
            
        Returns:
            응답 객체
        """
        # 진행률 추적이 필요한 엔드포인트 확인
        if request.url.path.startswith("/api/check/"):
            # 작업 ID를 요청 헤더나 URL에서 추출
            task_id = request.headers.get("X-Task-ID")
            if not task_id:
                # URL에서 파일 ID를 작업 ID로 사용
                path_parts = request.url.path.split("/")
                if len(path_parts) >= 4:
                    task_id = f"check_{path_parts[3]}"
            
            # 요청 상태에 작업 ID 저장
            request.state.task_id = task_id
        
        # 다음 미들웨어/엔드포인트 실행
        response = await call_next(request)
        
        # 진행률 정보를 응답 헤더에 추가
        if hasattr(request.state, 'task_id') and request.state.task_id:
            progress_info = await progress_tracker.get_progress(request.state.task_id)
            if progress_info:
                response.headers["X-Progress"] = str(progress_info["progress"])
                response.headers["X-Progress-Status"] = progress_info["status"]
                response.headers["X-Progress-Message"] = progress_info["message"]
        
        return response


# 편의 함수들
async def start_task_progress(task_id: str, total_steps: int, description: str = "") -> None:
    """작업 진행률 추적을 시작하는 편의 함수"""
    await progress_tracker.start_progress(task_id, total_steps, description)


async def update_task_progress(
    task_id: str,
    current_step: int,
    step_name: str = "",
    message: str = ""
) -> None:
    """작업 진행률을 업데이트하는 편의 함수"""
    await progress_tracker.update_progress(task_id, current_step, step_name, message)


async def complete_task_progress(task_id: str, message: str = "작업이 완료되었습니다") -> None:
    """작업 진행률을 완료로 표시하는 편의 함수"""
    await progress_tracker.complete_progress(task_id, message)


async def fail_task_progress(task_id: str, error_message: str) -> None:
    """작업 진행률을 실패로 표시하는 편의 함수"""
    await progress_tracker.fail_progress(task_id, error_message)


async def get_task_progress(task_id: str) -> Optional[Dict[str, Any]]:
    """작업 진행률을 조회하는 편의 함수"""
    return await progress_tracker.get_progress(task_id)