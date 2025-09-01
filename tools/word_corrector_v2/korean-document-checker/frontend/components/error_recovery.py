"""
에러 복구 및 자동 재시도 시스템

다양한 유형의 오류에 대해 자동 복구를 시도하고,
사용자에게 적절한 복구 옵션을 제공하는 컴포넌트입니다.
"""

import streamlit as st
import time
import logging
from typing import Optional, Dict, Any, Callable, List
from datetime import datetime, timedelta
from enum import Enum

from services.api_client import APIClient, APIClientError

logger = logging.getLogger(__name__)


class ErrorSeverity(Enum):
    """에러 심각도 레벨"""
    LOW = "low"           # 자동 복구 가능
    MEDIUM = "medium"     # 사용자 개입 필요
    HIGH = "high"         # 즉시 중단 필요
    CRITICAL = "critical" # 시스템 오류


class ErrorRecoveryStrategy(Enum):
    """에러 복구 전략"""
    RETRY = "retry"                    # 재시도
    FALLBACK = "fallback"             # 대체 방법 사용
    USER_ACTION = "user_action"       # 사용자 액션 필요
    RESET = "reset"                   # 세션 리셋
    ABORT = "abort"                   # 작업 중단


class ErrorRecoveryManager:
    """에러 복구 관리자"""
    
    # 에러 코드별 복구 전략 매핑
    RECOVERY_STRATEGIES = {
        "NETWORK_ERROR": {
            "severity": ErrorSeverity.MEDIUM,
            "strategies": [ErrorRecoveryStrategy.RETRY, ErrorRecoveryStrategy.USER_ACTION],
            "max_retries": 3,
            "retry_delay": 2.0,
            "user_message": "네트워크 연결을 확인하고 다시 시도해주세요."
        },
        "TIMEOUT_ERROR": {
            "severity": ErrorSeverity.MEDIUM,
            "strategies": [ErrorRecoveryStrategy.RETRY, ErrorRecoveryStrategy.FALLBACK],
            "max_retries": 2,
            "retry_delay": 5.0,
            "user_message": "요청 시간이 초과되었습니다. 파일 크기를 확인하고 다시 시도해주세요."
        },
        "FILE_VALIDATION_ERROR": {
            "severity": ErrorSeverity.HIGH,
            "strategies": [ErrorRecoveryStrategy.USER_ACTION],
            "max_retries": 0,
            "user_message": "파일 형식이나 크기에 문제가 있습니다. 다른 파일을 선택해주세요."
        },
        "FILE_PROCESSING_ERROR": {
            "severity": ErrorSeverity.MEDIUM,
            "strategies": [ErrorRecoveryStrategy.RETRY, ErrorRecoveryStrategy.USER_ACTION],
            "max_retries": 2,
            "retry_delay": 3.0,
            "user_message": "파일 처리 중 오류가 발생했습니다. 파일이 손상되지 않았는지 확인해주세요."
        },
        "AZURE_OPENAI_ERROR": {
            "severity": ErrorSeverity.MEDIUM,
            "strategies": [ErrorRecoveryStrategy.RETRY, ErrorRecoveryStrategy.FALLBACK],
            "max_retries": 3,
            "retry_delay": 10.0,
            "user_message": "AI 서비스에 일시적인 문제가 있습니다. 잠시 후 다시 시도해주세요."
        },
        "RATE_LIMIT_EXCEEDED": {
            "severity": ErrorSeverity.LOW,
            "strategies": [ErrorRecoveryStrategy.RETRY],
            "max_retries": 5,
            "retry_delay": 60.0,
            "user_message": "요청 한도를 초과했습니다. 잠시 후 자동으로 재시도됩니다."
        },
        "INTERNAL_SERVER_ERROR": {
            "severity": ErrorSeverity.HIGH,
            "strategies": [ErrorRecoveryStrategy.RETRY, ErrorRecoveryStrategy.RESET],
            "max_retries": 2,
            "retry_delay": 5.0,
            "user_message": "서버에 문제가 발생했습니다. 잠시 후 다시 시도하거나 새로 시작해주세요."
        }
    }
    
    def __init__(self):
        """에러 복구 관리자를 초기화합니다."""
        self.retry_counts = {}
        self.last_errors = {}
        
    def handle_error(
        self,
        error_code: str,
        error_message: str,
        error_details: Optional[Dict[str, Any]] = None,
        operation_callback: Optional[Callable] = None,
        reset_callback: Optional[Callable] = None
    ) -> bool:
        """
        에러를 처리하고 복구를 시도합니다.
        
        Args:
            error_code: 에러 코드
            error_message: 에러 메시지
            error_details: 에러 세부 정보
            operation_callback: 재시도할 작업 콜백
            reset_callback: 리셋 콜백
            
        Returns:
            복구 성공 여부
        """
        # 에러 정보 저장
        error_key = f"{error_code}_{int(time.time() / 60)}"  # 분 단위로 그룹화
        self.last_errors[error_key] = {
            "code": error_code,
            "message": error_message,
            "details": error_details or {},
            "timestamp": datetime.now()
        }
        
        # 복구 전략 가져오기
        strategy_info = self.RECOVERY_STRATEGIES.get(
            error_code, 
            self._get_default_strategy(error_code)
        )
        
        severity = strategy_info["severity"]
        strategies = strategy_info["strategies"]
        max_retries = strategy_info.get("max_retries", 1)
        retry_delay = strategy_info.get("retry_delay", 2.0)
        user_message = strategy_info.get("user_message", error_message)
        
        # 재시도 횟수 확인
        retry_count = self.retry_counts.get(error_code, 0)
        
        # UI에 에러 표시
        self._display_error_with_recovery(
            error_code, user_message, severity, strategies, 
            retry_count, max_retries, error_details
        )
        
        # 복구 전략 실행
        for strategy in strategies:
            if strategy == ErrorRecoveryStrategy.RETRY and retry_count < max_retries:
                return self._attempt_retry(
                    error_code, retry_delay, operation_callback
                )
            
            elif strategy == ErrorRecoveryStrategy.FALLBACK:
                return self._attempt_fallback(error_code, operation_callback)
            
            elif strategy == ErrorRecoveryStrategy.USER_ACTION:
                return self._request_user_action(
                    error_code, user_message, operation_callback, reset_callback
                )
            
            elif strategy == ErrorRecoveryStrategy.RESET:
                return self._perform_reset(reset_callback)
            
            elif strategy == ErrorRecoveryStrategy.ABORT:
                self._abort_operation(error_message)
                return False
        
        return False
    
    def _get_default_strategy(self, error_code: str) -> Dict[str, Any]:
        """
        알 수 없는 에러에 대한 기본 전략을 반환합니다.
        
        Args:
            error_code: 에러 코드
            
        Returns:
            기본 복구 전략
        """
        return {
            "severity": ErrorSeverity.MEDIUM,
            "strategies": [ErrorRecoveryStrategy.RETRY, ErrorRecoveryStrategy.USER_ACTION],
            "max_retries": 2,
            "retry_delay": 3.0,
            "user_message": f"알 수 없는 오류가 발생했습니다: {error_code}"
        }
    
    def _display_error_with_recovery(
        self,
        error_code: str,
        message: str,
        severity: ErrorSeverity,
        strategies: List[ErrorRecoveryStrategy],
        retry_count: int,
        max_retries: int,
        details: Optional[Dict[str, Any]] = None
    ) -> None:
        """
        복구 옵션과 함께 에러를 표시합니다.
        
        Args:
            error_code: 에러 코드
            message: 에러 메시지
            severity: 심각도
            strategies: 복구 전략 목록
            retry_count: 현재 재시도 횟수
            max_retries: 최대 재시도 횟수
            details: 에러 세부 정보
        """
        # 심각도에 따른 아이콘과 색상
        severity_icons = {
            ErrorSeverity.LOW: "⚠️",
            ErrorSeverity.MEDIUM: "🔶",
            ErrorSeverity.HIGH: "🔴",
            ErrorSeverity.CRITICAL: "💥"
        }
        
        icon = severity_icons.get(severity, "❓")
        
        # 에러 메시지 표시
        if severity in [ErrorSeverity.LOW, ErrorSeverity.MEDIUM]:
            st.warning(f"{icon} **오류 발생**\n\n{message}")
        else:
            st.error(f"{icon} **심각한 오류**\n\n{message}")
        
        # 재시도 정보 표시
        if ErrorRecoveryStrategy.RETRY in strategies and max_retries > 0:
            if retry_count < max_retries:
                st.info(f"🔄 자동 재시도: {retry_count + 1}/{max_retries + 1}")
            else:
                st.warning(f"⚠️ 최대 재시도 횟수에 도달했습니다 ({max_retries}회)")
        
        # 세부 정보 표시 (선택적)
        if details and st.checkbox("🔍 세부 정보 보기", key=f"details_{error_code}_{retry_count}"):
            with st.expander("에러 세부 정보", expanded=True):
                st.json(details)
    
    def _attempt_retry(
        self,
        error_code: str,
        delay: float,
        operation_callback: Optional[Callable]
    ) -> bool:
        """
        재시도를 수행합니다.
        
        Args:
            error_code: 에러 코드
            delay: 재시도 지연 시간
            operation_callback: 재시도할 작업
            
        Returns:
            재시도 성공 여부
        """
        if not operation_callback:
            return False
        
        # 재시도 횟수 증가
        self.retry_counts[error_code] = self.retry_counts.get(error_code, 0) + 1
        
        # 지연 시간 표시
        if delay > 0:
            with st.spinner(f"🔄 {delay}초 후 자동으로 재시도합니다..."):
                time.sleep(delay)
        
        try:
            # 작업 재시도
            result = operation_callback()
            
            # 성공 시 재시도 횟수 리셋
            if result:
                self.retry_counts[error_code] = 0
                st.success("✅ 재시도가 성공했습니다!")
                return True
        
        except Exception as e:
            logger.error(f"재시도 중 오류 발생: {str(e)}")
        
        return False
    
    def _attempt_fallback(
        self,
        error_code: str,
        operation_callback: Optional[Callable]
    ) -> bool:
        """
        대체 방법을 시도합니다.
        
        Args:
            error_code: 에러 코드
            operation_callback: 대체 작업
            
        Returns:
            대체 방법 성공 여부
        """
        st.info("🔄 대체 방법을 시도하고 있습니다...")
        
        # 현재는 기본적인 대체 방법만 구현
        # 향후 에러 유형별로 구체적인 대체 방법 추가 예정
        
        if error_code == "TIMEOUT_ERROR":
            st.info("⏱️ 더 짧은 타임아웃으로 재시도합니다...")
        elif error_code == "AZURE_OPENAI_ERROR":
            st.info("🤖 다른 AI 모델로 재시도합니다...")
        
        return False  # 현재는 대체 방법 미구현
    
    def _request_user_action(
        self,
        error_code: str,
        message: str,
        operation_callback: Optional[Callable],
        reset_callback: Optional[Callable]
    ) -> bool:
        """
        사용자 액션을 요청합니다.
        
        Args:
            error_code: 에러 코드
            message: 안내 메시지
            operation_callback: 재시도 콜백
            reset_callback: 리셋 콜백
            
        Returns:
            사용자 액션 결과
        """
        st.info(f"👤 **사용자 액션 필요**\n\n{message}")
        
        # 액션 버튼들
        col1, col2, col3 = st.columns(3)
        
        with col1:
            if st.button("🔄 다시 시도", key=f"retry_{error_code}"):
                if operation_callback:
                    try:
                        result = operation_callback()
                        if result:
                            st.success("✅ 문제가 해결되었습니다!")
                            return True
                    except Exception as e:
                        st.error(f"재시도 실패: {str(e)}")
        
        with col2:
            if st.button("🏠 처음으로", key=f"reset_{error_code}"):
                if reset_callback:
                    reset_callback()
                    st.info("🔄 새로 시작합니다...")
                    return True
        
        with col3:
            if st.button("❓ 도움말", key=f"help_{error_code}"):
                self._show_help_for_error(error_code)
        
        return False
    
    def _perform_reset(self, reset_callback: Optional[Callable]) -> bool:
        """
        세션을 리셋합니다.
        
        Args:
            reset_callback: 리셋 콜백
            
        Returns:
            리셋 성공 여부
        """
        st.warning("🔄 세션을 초기화합니다...")
        
        if reset_callback:
            reset_callback()
            st.success("✅ 세션이 초기화되었습니다. 새로 시작해주세요.")
            return True
        
        return False
    
    def _abort_operation(self, message: str) -> None:
        """
        작업을 중단합니다.
        
        Args:
            message: 중단 메시지
        """
        st.error(f"❌ **작업 중단**\n\n{message}")
        st.info("관리자에게 문의하거나 나중에 다시 시도해주세요.")
    
    def _show_help_for_error(self, error_code: str) -> None:
        """
        에러별 도움말을 표시합니다.
        
        Args:
            error_code: 에러 코드
        """
        help_messages = {
            "NETWORK_ERROR": """
            **네트워크 연결 문제 해결 방법:**
            1. 인터넷 연결 상태를 확인하세요
            2. 백엔드 서버가 실행 중인지 확인하세요
            3. 방화벽이나 보안 소프트웨어 설정을 확인하세요
            4. 사이드바에서 백엔드 URL이 올바른지 확인하세요
            """,
            "FILE_VALIDATION_ERROR": """
            **파일 문제 해결 방법:**
            1. .docx 형식의 파일인지 확인하세요
            2. 파일 크기가 10MB 이하인지 확인하세요
            3. 파일이 손상되지 않았는지 확인하세요
            4. Microsoft Word에서 파일을 열어 정상 작동하는지 확인하세요
            """,
            "AZURE_OPENAI_ERROR": """
            **AI 서비스 문제 해결 방법:**
            1. 잠시 후 다시 시도해보세요
            2. 인터넷 연결을 확인하세요
            3. Azure OpenAI 서비스 상태를 확인하세요
            4. 문제가 지속되면 관리자에게 문의하세요
            """
        }
        
        help_text = help_messages.get(error_code, "이 오류에 대한 구체적인 도움말이 없습니다.")
        st.info(help_text)
    
    def get_error_statistics(self) -> Dict[str, Any]:
        """
        에러 통계를 반환합니다.
        
        Returns:
            에러 통계 정보
        """
        return {
            "total_errors": len(self.last_errors),
            "retry_counts": dict(self.retry_counts),
            "recent_errors": list(self.last_errors.values())[-5:]  # 최근 5개
        }
    
    def clear_error_history(self) -> None:
        """에러 기록을 초기화합니다."""
        self.retry_counts.clear()
        self.last_errors.clear()


# 전역 에러 복구 관리자 인스턴스
error_recovery_manager = ErrorRecoveryManager()


def handle_api_error_with_recovery(
    error: APIClientError,
    operation_callback: Optional[Callable] = None,
    reset_callback: Optional[Callable] = None
) -> bool:
    """
    API 에러를 복구 시스템으로 처리하는 편의 함수
    
    Args:
        error: API 클라이언트 에러
        operation_callback: 재시도할 작업
        reset_callback: 리셋 콜백
        
    Returns:
        복구 성공 여부
    """
    return error_recovery_manager.handle_error(
        error_code=error.error_code,
        error_message=error.message,
        error_details=error.details,
        operation_callback=operation_callback,
        reset_callback=reset_callback
    )


def show_error_recovery_panel() -> None:
    """에러 복구 패널을 표시하는 편의 함수"""
    with st.sidebar:
        st.header("🔧 에러 복구")
        
        stats = error_recovery_manager.get_error_statistics()
        
        if stats["total_errors"] > 0:
            st.metric("총 에러 수", stats["total_errors"])
            
            if stats["recent_errors"]:
                with st.expander("최근 에러", expanded=False):
                    for error in stats["recent_errors"]:
                        timestamp = error["timestamp"].strftime("%H:%M:%S")
                        st.write(f"[{timestamp}] {error['code']}: {error['message'][:50]}...")
            
            if st.button("🗑️ 에러 기록 삭제"):
                error_recovery_manager.clear_error_history()
                st.success("에러 기록이 삭제되었습니다.")
        else:
            st.info("에러 기록이 없습니다.")