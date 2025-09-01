"""
진행률 조회 API 엔드포인트

클라이언트가 장시간 실행되는 작업의 진행 상황을 
실시간으로 조회할 수 있는 API를 제공합니다.
"""

from fastapi import APIRouter, HTTPException, Path
from typing import Dict, Any, Optional
import logging

from app.middleware.progress_tracker import get_task_progress
from app.models.response import ProgressUpdate, ErrorResponse
from app.core.exceptions import create_success_response, create_error_response

logger = logging.getLogger(__name__)

router = APIRouter()


@router.get("/progress/{task_id}", response_model=Dict[str, Any])
async def get_progress(
    task_id: str = Path(..., description="작업 고유 ID")
) -> Dict[str, Any]:
    """
    지정된 작업의 진행 상황을 조회합니다.
    
    Args:
        task_id: 작업 고유 ID
        
    Returns:
        진행률 정보
        
    Raises:
        HTTPException: 작업을 찾을 수 없는 경우
    """
    try:
        progress_info = await get_task_progress(task_id)
        
        if not progress_info:
            raise HTTPException(
                status_code=404,
                detail=f"작업을 찾을 수 없습니다: {task_id}"
            )
        
        # 진행률 정보를 표준 형식으로 변환
        progress_data = {
            "task_id": progress_info["task_id"],
            "description": progress_info["description"],
            "current_step": progress_info["current_step"],
            "total_steps": progress_info["total_steps"],
            "progress": progress_info["progress"],
            "status": progress_info["status"],
            "message": progress_info["message"],
            "current_step_name": progress_info.get("current_step_name", ""),
            "start_time": progress_info["start_time"].isoformat(),
            "last_update": progress_info["last_update"].isoformat(),
            "estimated_remaining": progress_info.get("estimated_remaining")
        }
        
        return create_success_response(
            data=progress_data,
            message="진행률 정보를 성공적으로 조회했습니다"
        )
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"진행률 조회 오류: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail="진행률 정보를 조회하는 중 오류가 발생했습니다"
        )


@router.get("/progress", response_model=Dict[str, Any])
async def list_active_tasks() -> Dict[str, Any]:
    """
    현재 진행 중인 모든 작업의 목록을 조회합니다.
    
    Returns:
        활성 작업 목록
    """
    try:
        # 이 기능은 향후 구현 예정
        # 현재는 빈 목록 반환
        return create_success_response(
            data={"active_tasks": []},
            message="활성 작업 목록을 조회했습니다"
        )
        
    except Exception as e:
        logger.error(f"활성 작업 목록 조회 오류: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail="활성 작업 목록을 조회하는 중 오류가 발생했습니다"
        )


@router.delete("/progress/{task_id}", response_model=Dict[str, Any])
async def cancel_task(
    task_id: str = Path(..., description="취소할 작업 ID")
) -> Dict[str, Any]:
    """
    진행 중인 작업을 취소합니다.
    
    Args:
        task_id: 취소할 작업 ID
        
    Returns:
        취소 결과
        
    Note:
        현재는 진행률 데이터만 삭제하며, 실제 작업 취소는 향후 구현 예정
    """
    try:
        progress_info = await get_task_progress(task_id)
        
        if not progress_info:
            raise HTTPException(
                status_code=404,
                detail=f"작업을 찾을 수 없습니다: {task_id}"
            )
        
        # 현재는 진행률 데이터만 삭제
        # 실제 작업 취소 로직은 향후 구현
        from app.middleware.progress_tracker import fail_task_progress
        await fail_task_progress(task_id, "사용자에 의해 취소됨")
        
        return create_success_response(
            data={"task_id": task_id, "status": "cancelled"},
            message="작업이 취소되었습니다"
        )
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"작업 취소 오류: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail="작업을 취소하는 중 오류가 발생했습니다"
        )