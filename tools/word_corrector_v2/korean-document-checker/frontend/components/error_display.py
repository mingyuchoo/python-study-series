"""
에러 표시 및 사용자 안내 컴포넌트

다양한 유형의 에러를 사용자 친화적으로 표시하고,
적절한 해결 방법을 안내하는 UI 컴포넌트들을 제공합니다.
"""

import streamlit as st
from typing import Optional, Dict, Any, List, Callable
from datetime import datetime
import logging

logger = logging.getLogger(__name__)


class ErrorDisplayComponent:
    """에러 표시 컴포넌트 클래스"""
    
    # 에러 유형별 아이콘과 색상
    ERROR_STYLES = {
        "FILE_VALIDATION_ERROR": {"icon": "📁", "color": "orange"},
        "FILE_PROCESSING_ERROR": {"icon": "⚙️", "color": "red"},
        "FILE_NOT_FOUND": {"icon": "🔍", "color": "orange"},
        "AZURE_OPENAI_ERROR": {"icon": "🤖", "color": "red"},
        "CONFIGURATION_ERROR": {"icon": "⚙️", "color": "red"},
        "RATE_LIMIT_EXCEEDED": {"icon": "⏱️", "color": "orange"},
        "VALIDATION_ERROR": {"icon": "✏️", "color": "orange"},
        "INTERNAL_SERVER_ERROR": {"icon": "🔧", "color": "red"},
        "NETWORK_ERROR": {"icon": "🌐", "color": "red"},
        "TIMEOUT_ERROR": {"icon": "⏰", "color": "orange"},
        "UNKNOWN_ERROR": {"icon": "❓", "color": "red"}
    }
    
    # 에러 유형별 해결 방법
    SOLUTION_GUIDES = {
        "FILE_VALIDATION_ERROR": [
            "파일 형식이 .docx인지 확인하세요",
            "파일 크기가 10MB 이하인지 확인하세요",
            "파일이 손상되지 않았는지 확인하세요"
        ],
        "FILE_PROCESSING_ERROR": [
            "다른 .docx 파일로 시도해보세요",
            "파일을 다시 저장한 후 업로드하세요",
            "파일에 특수 문자나 이미지가 너무 많지 않은지 확인하세요"
        ],
        "FILE_NOT_FOUND": [
            "파일을 다시 업로드하세요",
            "브라우저를 새로고침한 후 다시 시도하세요"
        ],
        "AZURE_OPENAI_ERROR": [
            "잠시 후 다시 시도하세요",
            "인터넷 연결을 확인하세요",
            "문제가 지속되면 관리자에게 문의하세요"
        ],
        "RATE_LIMIT_EXCEEDED": [
            "1-2분 후 다시 시도하세요",
            "동시에 여러 파일을 처리하지 마세요"
        ],
        "NETWORK_ERROR": [
            "인터넷 연결을 확인하세요",
            "백엔드 서버가 실행 중인지 확인하세요",
            "방화벽 설정을 확인하세요"
        ],
        "TIMEOUT_ERROR": [
            "파일 크기를 줄여보세요",
            "잠시 후 다시 시도하세요",
            "인터넷 연결 상태를 확인하세요"
        ]
    }
    
    def __init__(self):
        """에러 표시 컴포넌트를 초기화합니다."""
        pass
    
    def display_error(
        self,
        error_code: str,
        message: str,
        details: Optional[Dict[str, Any]] = None,
        show_details: bool = False,
        show_solutions: bool = True,
        retry_callback: Optional[Callable] = None
    ) -> None:
        """
        에러를 사용자 친화적으로 표시합니다.
        
        Args:
            error_code: 에러 코드
            message: 에러 메시지
            details: 에러 세부 정보
            show_details: 세부 정보 표시 여부
            show_solutions: 해결 방법 표시 여부
            retry_callback: 재시도 콜백 함수
        """
        # 에러 스타일 가져오기
        style = self.ERROR_STYLES.get(error_code, self.ERROR_STYLES["UNKNOWN_ERROR"])
        
        # 메인 에러 메시지 표시
        st.error(f"{style['icon']} **오류 발생**\n\n{message}")
        
        # 에러 코드 표시
        st.caption(f"오류 코드: `{error_code}`")
        
        # 해결 방법 표시
        if show_solutions:
            self._display_solutions(error_code)
        
        # 세부 정보 표시 (선택적)
        if show_details and details:
            self._display_error_details(details)
        
        # 액션 버튼들
        self._display_action_buttons(retry_callback)
    
    def _display_solutions(self, error_code: str) -> None:
        """
        에러 유형에 따른 해결 방법을 표시합니다.
        
        Args:
            error_code: 에러 코드
        """
        solutions = self.SOLUTION_GUIDES.get(error_code, [])
        
        if solutions:
            st.info("**💡 해결 방법:**")
            for i, solution in enumerate(solutions, 1):
                st.write(f"{i}. {solution}")
    
    def _display_error_details(self, details: Dict[str, Any]) -> None:
        """
        에러 세부 정보를 표시합니다.
        
        Args:
            details: 에러 세부 정보
        """
        with st.expander("🔍 세부 정보", expanded=False):
            for key, value in details.items():
                if key == "timestamp":
                    st.write(f"**{key}:** {value}")
                elif isinstance(value, dict):
                    st.write(f"**{key}:**")
                    st.json(value)
                else:
                    st.write(f"**{key}:** {value}")
    
    def _display_action_buttons(self, retry_callback: Optional[Callable] = None) -> None:
        """
        액션 버튼들을 표시합니다.
        
        Args:
            retry_callback: 재시도 콜백 함수
        """
        col1, col2, col3 = st.columns([1, 1, 2])
        
        with col1:
            if retry_callback and st.button("🔄 다시 시도", type="primary"):
                retry_callback()
        
        with col2:
            if st.button("🏠 처음으로"):
                st.session_state.clear()
                st.rerun()
        
        with col3:
            if st.button("📞 도움말", help="문제 해결 가이드 보기"):
                self._show_help_dialog()
    
    def _show_help_dialog(self) -> None:
        """도움말 다이얼로그를 표시합니다."""
        st.info("""
        **📞 추가 도움이 필요하신가요?**
        
        1. **일반적인 문제**: 사이드바의 도움말 섹션을 확인하세요
        2. **파일 문제**: .docx 파일이 Microsoft Word에서 정상적으로 열리는지 확인하세요
        3. **연결 문제**: 백엔드 서버 URL이 올바른지 확인하세요
        4. **지속적인 문제**: 브라우저를 새로고침하거나 다른 브라우저를 사용해보세요
        """)


class NetworkErrorHandler:
    """네트워크 에러 전용 핸들러"""
    
    @staticmethod
    def display_connection_error(backend_url: str) -> None:
        """
        백엔드 연결 오류를 표시합니다.
        
        Args:
            backend_url: 백엔드 서버 URL
        """
        st.error("""
        🌐 **백엔드 서버에 연결할 수 없습니다**
        
        서버가 실행되지 않았거나 네트워크 문제가 있을 수 있습니다.
        """)
        
        st.info(f"**연결 시도 URL:** `{backend_url}`")
        
        st.warning("""
        **해결 방법:**
        1. 백엔드 서버가 실행 중인지 확인하세요
        2. 사이드바에서 서버 URL을 확인하세요
        3. 방화벽이나 보안 소프트웨어를 확인하세요
        4. 인터넷 연결을 확인하세요
        """)
        
        col1, col2 = st.columns(2)
        with col1:
            if st.button("🔄 연결 재시도", type="primary"):
                st.rerun()
        
        with col2:
            if st.button("⚙️ 설정 변경"):
                st.sidebar.info("👈 사이드바에서 백엔드 URL을 수정하세요")


class ValidationErrorHandler:
    """입력 검증 에러 전용 핸들러"""
    
    @staticmethod
    def display_file_validation_error(
        filename: str,
        file_size: int,
        error_details: Dict[str, Any]
    ) -> None:
        """
        파일 검증 오류를 표시합니다.
        
        Args:
            filename: 파일명
            file_size: 파일 크기
            error_details: 에러 세부 정보
        """
        st.error(f"📁 **파일 검증 실패: {filename}**")
        
        # 파일 정보 표시
        col1, col2 = st.columns(2)
        with col1:
            st.write(f"**파일명:** {filename}")
            st.write(f"**파일 크기:** {file_size:,} bytes ({file_size/(1024*1024):.2f} MB)")
        
        with col2:
            if "max_size" in error_details:
                max_size_mb = error_details["max_size"] / (1024 * 1024)
                st.write(f"**최대 허용 크기:** {max_size_mb:.0f} MB")
            
            if "allowed_extensions" in error_details:
                st.write(f"**허용 확장자:** {', '.join(error_details['allowed_extensions'])}")
        
        # 해결 방법
        st.info("""
        **💡 해결 방법:**
        1. .docx 형식의 파일인지 확인하세요
        2. 파일 크기를 10MB 이하로 줄이세요
        3. 파일이 손상되지 않았는지 확인하세요
        """)


def display_loading_error(error_message: str, retry_callback: Optional[Callable] = None) -> None:
    """
    로딩 중 발생한 에러를 표시하는 편의 함수
    
    Args:
        error_message: 에러 메시지
        retry_callback: 재시도 콜백 함수
    """
    error_handler = ErrorDisplayComponent()
    error_handler.display_error(
        error_code="LOADING_ERROR",
        message=error_message,
        retry_callback=retry_callback
    )


def display_api_error(
    error_response: Dict[str, Any],
    retry_callback: Optional[Callable] = None
) -> None:
    """
    API 에러 응답을 표시하는 편의 함수
    
    Args:
        error_response: API 에러 응답
        retry_callback: 재시도 콜백 함수
    """
    error_handler = ErrorDisplayComponent()
    
    error_code = error_response.get("error_code", "UNKNOWN_ERROR")
    message = error_response.get("message", "알 수 없는 오류가 발생했습니다")
    details = error_response.get("details", {})
    
    error_handler.display_error(
        error_code=error_code,
        message=message,
        details=details,
        retry_callback=retry_callback
    )


def display_success_message(
    message: str,
    details: Optional[Dict[str, Any]] = None,
    show_confetti: bool = False
) -> None:
    """
    성공 메시지를 표시하는 편의 함수
    
    Args:
        message: 성공 메시지
        details: 추가 세부 정보
        show_confetti: 축하 애니메이션 표시 여부
    """
    st.success(f"✅ {message}")
    
    if show_confetti:
        st.balloons()
    
    if details:
        with st.expander("📋 세부 정보", expanded=False):
            for key, value in details.items():
                st.write(f"**{key}:** {value}")