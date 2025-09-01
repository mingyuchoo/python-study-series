"""
로딩 상태 표시 및 진행률 피드백 컴포넌트

사용자에게 현재 진행 상황을 시각적으로 표시하고,
예상 소요 시간 및 진행률 정보를 제공하는 UI 컴포넌트들입니다.
"""

import streamlit as st
import time
from typing import Optional, List, Dict, Any, Callable
from datetime import datetime, timedelta
import logging

logger = logging.getLogger(__name__)


class LoadingDisplayComponent:
    """로딩 상태 표시 컴포넌트"""
    
    # 로딩 애니메이션 스타일
    LOADING_ANIMATIONS = {
        "spinner": "⏳",
        "dots": "⋯",
        "pulse": "💫",
        "gear": "⚙️",
        "robot": "🤖",
        "document": "📄",
        "search": "🔍"
    }
    
    def __init__(self):
        """로딩 표시 컴포넌트를 초기화합니다."""
        self.start_time = None
        self.current_step = None
        self.total_steps = 0
    
    def show_simple_loading(
        self,
        message: str = "처리 중입니다...",
        animation: str = "spinner"
    ) -> None:
        """
        간단한 로딩 메시지를 표시합니다.
        
        Args:
            message: 로딩 메시지
            animation: 애니메이션 타입
        """
        icon = self.LOADING_ANIMATIONS.get(animation, "⏳")
        st.info(f"{icon} {message}")
    
    def show_progress_bar(
        self,
        progress: float,
        message: str = "",
        show_percentage: bool = True
    ) -> None:
        """
        진행률 바를 표시합니다.
        
        Args:
            progress: 진행률 (0.0 - 1.0)
            message: 진행 상태 메시지
            show_percentage: 백분율 표시 여부
        """
        # 진행률 바
        progress_bar = st.progress(progress)
        
        # 메시지와 백분율
        if message or show_percentage:
            display_text = ""
            if message:
                display_text += message
            if show_percentage:
                percentage = int(progress * 100)
                if message:
                    display_text += f" ({percentage}%)"
                else:
                    display_text = f"{percentage}%"
            
            st.caption(display_text)
    
    def show_step_progress(
        self,
        current_step: int,
        total_steps: int,
        step_name: str,
        step_description: Optional[str] = None
    ) -> None:
        """
        단계별 진행 상황을 표시합니다.
        
        Args:
            current_step: 현재 단계 (1부터 시작)
            total_steps: 전체 단계 수
            step_name: 현재 단계 이름
            step_description: 단계 설명
        """
        # 진행률 계산
        progress = (current_step - 1) / total_steps if total_steps > 0 else 0
        
        # 단계 표시
        st.subheader(f"📋 단계 {current_step}/{total_steps}: {step_name}")
        
        if step_description:
            st.write(step_description)
        
        # 진행률 바
        self.show_progress_bar(progress, f"단계 {current_step}/{total_steps} 진행 중")
    
    def show_file_upload_progress(
        self,
        filename: str,
        uploaded_bytes: int,
        total_bytes: int,
        upload_speed: Optional[float] = None
    ) -> None:
        """
        파일 업로드 진행 상황을 표시합니다.
        
        Args:
            filename: 파일명
            uploaded_bytes: 업로드된 바이트 수
            total_bytes: 전체 파일 크기
            upload_speed: 업로드 속도 (bytes/sec)
        """
        progress = uploaded_bytes / total_bytes if total_bytes > 0 else 0
        
        st.subheader(f"📤 파일 업로드: {filename}")
        
        # 진행률 바
        self.show_progress_bar(progress)
        
        # 상세 정보
        col1, col2, col3 = st.columns(3)
        
        with col1:
            st.metric(
                "업로드됨",
                f"{uploaded_bytes / (1024*1024):.1f} MB",
                f"{total_bytes / (1024*1024):.1f} MB 중"
            )
        
        with col2:
            if upload_speed:
                speed_mbps = upload_speed / (1024 * 1024)
                st.metric("속도", f"{speed_mbps:.1f} MB/s")
        
        with col3:
            if upload_speed and uploaded_bytes < total_bytes:
                remaining_bytes = total_bytes - uploaded_bytes
                eta_seconds = remaining_bytes / upload_speed
                eta = timedelta(seconds=int(eta_seconds))
                st.metric("예상 남은 시간", str(eta))


class DocumentCheckProgressDisplay:
    """문서 검사 진행 상황 표시 컴포넌트"""
    
    # 검사 단계별 정보
    CHECK_STEPS = {
        "file_processing": {
            "name": "파일 처리",
            "description": "문서에서 텍스트를 추출하고 구조를 분석합니다",
            "icon": "📄",
            "estimated_time": 5
        },
        "grammar_check": {
            "name": "구문 검사",
            "description": "한국어 문법 오류를 검사합니다",
            "icon": "✏️",
            "estimated_time": 15
        },
        "korean_spell_check": {
            "name": "한국어 맞춤법 검사",
            "description": "한국어 맞춤법과 띄어쓰기를 검사합니다",
            "icon": "🔤",
            "estimated_time": 10
        },
        "english_spell_check": {
            "name": "영어 맞춤법 검사",
            "description": "문서 내 영어 단어의 철자를 검사합니다",
            "icon": "🔠",
            "estimated_time": 8
        },
        "consistency_check": {
            "name": "일관성 검사",
            "description": "문서의 형식과 용어 일관성을 검사합니다",
            "icon": "📋",
            "estimated_time": 12
        },
        "report_generation": {
            "name": "보고서 생성",
            "description": "검사 결과를 종합하여 보고서를 생성합니다",
            "icon": "📊",
            "estimated_time": 3
        }
    }
    
    def __init__(self):
        """문서 검사 진행 표시 컴포넌트를 초기화합니다."""
        self.start_time = datetime.now()
        self.completed_steps = []
        self.current_step = None
    
    def show_check_overview(self, check_types: List[str]) -> None:
        """
        검사 개요를 표시합니다.
        
        Args:
            check_types: 수행할 검사 유형 목록
        """
        st.subheader("🔍 문서 검사 진행 상황")
        
        # 예상 소요 시간 계산
        total_estimated_time = sum(
            self.CHECK_STEPS.get(check_type, {}).get("estimated_time", 5)
            for check_type in check_types
        )
        
        st.info(f"**예상 소요 시간:** 약 {total_estimated_time}초")
        
        # 검사 단계 목록
        st.write("**수행할 검사:**")
        for check_type in check_types:
            step_info = self.CHECK_STEPS.get(check_type, {})
            icon = step_info.get("icon", "🔍")
            name = step_info.get("name", check_type)
            st.write(f"- {icon} {name}")
    
    def show_current_step(
        self,
        current_step: str,
        progress: float = 0.0,
        message: Optional[str] = None
    ) -> None:
        """
        현재 진행 중인 단계를 표시합니다.
        
        Args:
            current_step: 현재 단계
            progress: 단계 내 진행률
            message: 추가 메시지
        """
        step_info = self.CHECK_STEPS.get(current_step, {})
        icon = step_info.get("icon", "🔍")
        name = step_info.get("name", current_step)
        description = step_info.get("description", "")
        
        # 현재 단계 표시
        st.write(f"### {icon} {name}")
        if description:
            st.caption(description)
        
        # 진행률 바
        if progress > 0:
            st.progress(progress)
        
        # 추가 메시지
        if message:
            st.info(message)
        
        # 경과 시간
        elapsed = datetime.now() - self.start_time
        st.caption(f"경과 시간: {elapsed.seconds}초")
    
    def show_step_completion(self, completed_step: str) -> None:
        """
        완료된 단계를 표시합니다.
        
        Args:
            completed_step: 완료된 단계
        """
        step_info = self.CHECK_STEPS.get(completed_step, {})
        icon = step_info.get("icon", "✅")
        name = step_info.get("name", completed_step)
        
        st.success(f"{icon} {name} 완료")
        
        if completed_step not in self.completed_steps:
            self.completed_steps.append(completed_step)
    
    def show_overall_progress(self, check_types: List[str], current_step: str) -> None:
        """
        전체 진행 상황을 표시합니다.
        
        Args:
            check_types: 전체 검사 유형 목록
            current_step: 현재 단계
        """
        # 현재 단계의 인덱스 찾기
        try:
            current_index = check_types.index(current_step)
            overall_progress = current_index / len(check_types)
        except ValueError:
            overall_progress = 0.0
        
        # 전체 진행률 표시
        st.subheader("📊 전체 진행률")
        st.progress(overall_progress)
        st.caption(f"{current_index + 1}/{len(check_types)} 단계 진행 중")
        
        # 완료된 단계들 표시
        if self.completed_steps:
            with st.expander("✅ 완료된 검사", expanded=False):
                for step in self.completed_steps:
                    step_info = self.CHECK_STEPS.get(step, {})
                    icon = step_info.get("icon", "✅")
                    name = step_info.get("name", step)
                    st.write(f"- {icon} {name}")


class InteractiveLoadingDisplay:
    """인터랙티브 로딩 표시 컴포넌트"""
    
    def __init__(self):
        """인터랙티브 로딩 표시 컴포넌트를 초기화합니다."""
        self.placeholder = None
        self.progress_placeholder = None
        self.message_placeholder = None
    
    def create_placeholders(self) -> None:
        """UI 플레이스홀더들을 생성합니다."""
        self.placeholder = st.empty()
        self.progress_placeholder = st.empty()
        self.message_placeholder = st.empty()
    
    def update_progress(
        self,
        progress: float,
        title: str,
        message: str = "",
        show_cancel: bool = False,
        cancel_callback: Optional[Callable] = None
    ) -> bool:
        """
        진행 상황을 업데이트합니다.
        
        Args:
            progress: 진행률 (0.0 - 1.0)
            title: 제목
            message: 메시지
            show_cancel: 취소 버튼 표시 여부
            cancel_callback: 취소 콜백 함수
            
        Returns:
            취소 버튼 클릭 여부
        """
        with self.placeholder.container():
            st.subheader(title)
            
            # 진행률 바
            self.progress_placeholder.progress(progress)
            
            # 메시지
            if message:
                self.message_placeholder.info(message)
            
            # 취소 버튼
            if show_cancel and cancel_callback:
                if st.button("❌ 취소", key=f"cancel_{int(time.time())}"):
                    cancel_callback()
                    return True
        
        return False
    
    def show_completion(self, title: str, message: str, show_confetti: bool = True) -> None:
        """
        완료 상태를 표시합니다.
        
        Args:
            title: 제목
            message: 완료 메시지
            show_confetti: 축하 애니메이션 표시 여부
        """
        with self.placeholder.container():
            st.success(f"✅ {title}")
            st.info(message)
            
            if show_confetti:
                st.balloons()
    
    def clear(self) -> None:
        """플레이스홀더들을 정리합니다."""
        if self.placeholder:
            self.placeholder.empty()
        if self.progress_placeholder:
            self.progress_placeholder.empty()
        if self.message_placeholder:
            self.message_placeholder.empty()


# 편의 함수들
def show_file_upload_loading(filename: str) -> None:
    """파일 업로드 로딩을 표시하는 편의 함수"""
    with st.spinner(f"📤 {filename} 업로드 중..."):
        time.sleep(0.1)  # UI 업데이트를 위한 짧은 대기


def show_document_check_loading(check_types: List[str]) -> DocumentCheckProgressDisplay:
    """문서 검사 로딩을 시작하는 편의 함수"""
    progress_display = DocumentCheckProgressDisplay()
    progress_display.show_check_overview(check_types)
    return progress_display


def show_simple_spinner(message: str, duration: float = 1.0) -> None:
    """간단한 스피너를 표시하는 편의 함수"""
    with st.spinner(message):
        time.sleep(duration)