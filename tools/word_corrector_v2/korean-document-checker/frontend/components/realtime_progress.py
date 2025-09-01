"""
실시간 진행률 표시 컴포넌트

백엔드에서 진행 중인 작업의 상태를 실시간으로 조회하고
사용자에게 시각적으로 표시하는 컴포넌트입니다.
"""

import streamlit as st
import time
import asyncio
from typing import Optional, Dict, Any, Callable
from datetime import datetime, timedelta
import logging

from services.api_client import APIClient, APIClientError

logger = logging.getLogger(__name__)


class RealTimeProgressTracker:
    """실시간 진행률 추적 컴포넌트"""
    
    def __init__(self, api_client: APIClient):
        """
        실시간 진행률 추적기를 초기화합니다.
        
        Args:
            api_client: API 클라이언트 인스턴스
        """
        self.api_client = api_client
        self.is_tracking = False
        self.last_progress = None
        
    def track_progress(
        self,
        task_id: str,
        update_interval: float = 2.0,
        timeout: int = 300,
        on_complete: Optional[Callable] = None,
        on_error: Optional[Callable] = None,
        show_cancel: bool = True
    ) -> Optional[Dict[str, Any]]:
        """
        작업 진행률을 실시간으로 추적합니다.
        
        Args:
            task_id: 추적할 작업 ID
            update_interval: 업데이트 간격 (초)
            timeout: 최대 대기 시간 (초)
            on_complete: 완료 시 콜백 함수
            on_error: 오류 시 콜백 함수
            show_cancel: 취소 버튼 표시 여부
            
        Returns:
            최종 진행률 정보 또는 None (취소/오류 시)
        """
        self.is_tracking = True
        start_time = time.time()
        
        # UI 플레이스홀더 생성
        progress_container = st.empty()
        status_container = st.empty()
        details_container = st.empty()
        action_container = st.empty()
        
        try:
            while self.is_tracking:
                # 진행률 조회
                try:
                    progress_info = self.api_client.get_progress(task_id)
                    
                    if progress_info.get("success"):
                        data = progress_info["data"]
                        self.last_progress = data
                        
                        # UI 업데이트
                        self._update_progress_ui(
                            progress_container,
                            status_container,
                            details_container,
                            data
                        )
                        
                        # 완료 확인
                        if data["status"] == "completed":
                            self.is_tracking = False
                            if on_complete:
                                on_complete(data)
                            return data
                        
                        # 실패 확인
                        elif data["status"] == "failed":
                            self.is_tracking = False
                            if on_error:
                                on_error(data)
                            return None
                    
                    else:
                        # 진행률 정보를 찾을 수 없음 (작업이 완료되었거나 존재하지 않음)
                        self.is_tracking = False
                        return self.last_progress
                
                except APIClientError as e:
                    logger.warning(f"진행률 조회 실패: {str(e)}")
                    # 네트워크 오류 등은 계속 재시도
                    pass
                
                # 취소 버튼 표시
                if show_cancel:
                    with action_container.container():
                        col1, col2, col3 = st.columns([1, 1, 2])
                        with col1:
                            if st.button("❌ 취소", key=f"cancel_{task_id}_{int(time.time())}"):
                                self._cancel_task(task_id)
                                self.is_tracking = False
                                return None
                        
                        with col2:
                            if st.button("🔄 새로고침", key=f"refresh_{task_id}_{int(time.time())}"):
                                # 즉시 업데이트
                                continue
                
                # 타임아웃 확인
                if time.time() - start_time > timeout:
                    self.is_tracking = False
                    st.error(f"작업 대기 시간이 초과되었습니다 ({timeout}초)")
                    return None
                
                # 대기
                if self.is_tracking:
                    time.sleep(update_interval)
            
            return self.last_progress
            
        except Exception as e:
            logger.error(f"진행률 추적 중 오류: {str(e)}")
            self.is_tracking = False
            if on_error:
                on_error({"error": str(e)})
            return None
        
        finally:
            # UI 정리
            if not self.last_progress or self.last_progress.get("status") != "completed":
                progress_container.empty()
                status_container.empty()
                details_container.empty()
                action_container.empty()
    
    def _update_progress_ui(
        self,
        progress_container,
        status_container,
        details_container,
        data: Dict[str, Any]
    ) -> None:
        """
        진행률 UI를 업데이트합니다.
        
        Args:
            progress_container: 진행률 바 컨테이너
            status_container: 상태 메시지 컨테이너
            details_container: 세부 정보 컨테이너
            data: 진행률 데이터
        """
        # 진행률 바
        progress = data.get("progress", 0.0)
        with progress_container.container():
            st.progress(progress)
            
            # 진행률 텍스트
            percentage = int(progress * 100)
            current_step = data.get("current_step", 0)
            total_steps = data.get("total_steps", 1)
            st.caption(f"진행률: {percentage}% ({current_step}/{total_steps})")
        
        # 상태 메시지
        with status_container.container():
            status = data.get("status", "unknown")
            message = data.get("message", "")
            current_step_name = data.get("current_step_name", "")
            
            if status == "in_progress":
                if current_step_name:
                    st.info(f"🔄 **{current_step_name}**\n\n{message}")
                else:
                    st.info(f"🔄 {message}")
            elif status == "completed":
                st.success(f"✅ **완료**\n\n{message}")
            elif status == "failed":
                st.error(f"❌ **실패**\n\n{message}")
            else:
                st.info(f"ℹ️ {message}")
        
        # 세부 정보
        with details_container.container():
            col1, col2, col3 = st.columns(3)
            
            with col1:
                # 경과 시간
                start_time_str = data.get("start_time", "")
                if start_time_str:
                    try:
                        start_time = datetime.fromisoformat(start_time_str.replace('Z', '+00:00'))
                        elapsed = datetime.now() - start_time.replace(tzinfo=None)
                        elapsed_seconds = int(elapsed.total_seconds())
                        st.metric("경과 시간", f"{elapsed_seconds}초")
                    except:
                        pass
            
            with col2:
                # 예상 남은 시간
                estimated_remaining = data.get("estimated_remaining")
                if estimated_remaining and estimated_remaining > 0:
                    st.metric("예상 남은 시간", f"{estimated_remaining}초")
                elif status == "in_progress":
                    st.metric("예상 남은 시간", "계산 중...")
            
            with col3:
                # 작업 설명
                description = data.get("description", "")
                if description:
                    st.metric("작업", description.split(":")[0] if ":" in description else description)
    
    def _cancel_task(self, task_id: str) -> None:
        """
        작업을 취소합니다.
        
        Args:
            task_id: 취소할 작업 ID
        """
        try:
            result = self.api_client.cancel_task(task_id)
            if result.get("success"):
                st.warning("작업이 취소되었습니다.")
            else:
                st.error("작업 취소에 실패했습니다.")
        except APIClientError as e:
            logger.error(f"작업 취소 실패: {str(e)}")
            st.error(f"작업 취소 중 오류가 발생했습니다: {str(e)}")


def track_document_check_progress(
    api_client: APIClient,
    file_id: str,
    on_complete: Optional[Callable] = None,
    on_error: Optional[Callable] = None
) -> Optional[Dict[str, Any]]:
    """
    문서 검사 진행률을 추적하는 편의 함수
    
    Args:
        api_client: API 클라이언트
        file_id: 파일 ID
        on_complete: 완료 시 콜백
        on_error: 오류 시 콜백
        
    Returns:
        최종 진행률 정보
    """
    task_id = f"check_{file_id}"
    tracker = RealTimeProgressTracker(api_client)
    
    st.subheader("🔍 문서 검사 진행 상황")
    st.info("문서 검사가 진행 중입니다. 잠시만 기다려주세요...")
    
    return tracker.track_progress(
        task_id=task_id,
        update_interval=2.0,
        timeout=300,  # 5분 타임아웃
        on_complete=on_complete,
        on_error=on_error,
        show_cancel=True
    )


def show_progress_with_fallback(
    api_client: APIClient,
    task_id: str,
    fallback_message: str = "작업을 처리하고 있습니다...",
    fallback_duration: int = 30
) -> None:
    """
    진행률 추적을 시도하고, 실패 시 기본 로딩 메시지를 표시합니다.
    
    Args:
        api_client: API 클라이언트
        task_id: 작업 ID
        fallback_message: 대체 메시지
        fallback_duration: 대체 메시지 표시 시간 (초)
    """
    try:
        tracker = RealTimeProgressTracker(api_client)
        result = tracker.track_progress(
            task_id=task_id,
            update_interval=2.0,
            timeout=60,
            show_cancel=False
        )
        
        if not result:
            # 진행률 추적 실패 시 기본 로딩
            with st.spinner(fallback_message):
                time.sleep(fallback_duration)
    
    except Exception as e:
        logger.warning(f"진행률 추적 실패, 기본 로딩으로 대체: {str(e)}")
        with st.spinner(fallback_message):
            time.sleep(fallback_duration)


class ProgressNotificationManager:
    """진행률 알림 관리자"""
    
    def __init__(self):
        self.notifications = []
    
    def add_notification(self, message: str, type: str = "info") -> None:
        """
        알림을 추가합니다.
        
        Args:
            message: 알림 메시지
            type: 알림 유형 (info, success, warning, error)
        """
        notification = {
            "message": message,
            "type": type,
            "timestamp": datetime.now()
        }
        self.notifications.append(notification)
        
        # 최대 10개까지만 유지
        if len(self.notifications) > 10:
            self.notifications = self.notifications[-10:]
    
    def show_notifications(self) -> None:
        """저장된 알림들을 표시합니다."""
        if not self.notifications:
            return
        
        with st.expander("📢 알림", expanded=False):
            for notification in reversed(self.notifications):
                timestamp = notification["timestamp"].strftime("%H:%M:%S")
                message = f"[{timestamp}] {notification['message']}"
                
                if notification["type"] == "success":
                    st.success(message)
                elif notification["type"] == "warning":
                    st.warning(message)
                elif notification["type"] == "error":
                    st.error(message)
                else:
                    st.info(message)
    
    def clear_notifications(self) -> None:
        """모든 알림을 삭제합니다."""
        self.notifications.clear()


# 전역 알림 관리자 인스턴스
notification_manager = ProgressNotificationManager()