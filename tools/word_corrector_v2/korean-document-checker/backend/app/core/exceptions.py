"""
커스텀 예외 클래스들과 에러 처리 유틸리티

애플리케이션 전반에서 사용되는 커스텀 예외들과
표준화된 에러 응답을 위한 유틸리티 함수들을 제공합니다.
"""

from typing import Optional, Dict, Any
from datetime import datetime


class DocumentCheckerException(Exception):
    """기본 애플리케이션 예외 클래스"""
    
    def __init__(
        self,
        message: str,
        error_code: str = "GENERAL_ERROR",
        details: Optional[Dict[str, Any]] = None,
        status_code: int = 500
    ):
        self.message = message
        self.error_code = error_code
        self.details = details or {}
        self.status_code = status_code
        self.timestamp = datetime.now()
        super().__init__(self.message)


class FileValidationError(DocumentCheckerException):
    """파일 검증 관련 예외"""
    
    def __init__(self, message: str, details: Optional[Dict[str, Any]] = None):
        super().__init__(
            message=message,
            error_code="FILE_VALIDATION_ERROR",
            details=details,
            status_code=400
        )


class FileProcessingError(DocumentCheckerException):
    """파일 처리 관련 예외"""
    
    def __init__(self, message: str, details: Optional[Dict[str, Any]] = None):
        super().__init__(
            message=message,
            error_code="FILE_PROCESSING_ERROR",
            details=details,
            status_code=422
        )


class FileNotFoundError(DocumentCheckerException):
    """파일을 찾을 수 없는 경우의 예외"""
    
    def __init__(self, file_id: str):
        super().__init__(
            message=f"파일을 찾을 수 없습니다: {file_id}",
            error_code="FILE_NOT_FOUND",
            details={"file_id": file_id},
            status_code=404
        )


class AzureOpenAIError(DocumentCheckerException):
    """Azure OpenAI 관련 예외"""
    
    def __init__(self, message: str, details: Optional[Dict[str, Any]] = None):
        super().__init__(
            message=message,
            error_code="AZURE_OPENAI_ERROR",
            details=details,
            status_code=503
        )


class ConfigurationError(DocumentCheckerException):
    """설정 관련 예외"""
    
    def __init__(self, message: str, details: Optional[Dict[str, Any]] = None):
        super().__init__(
            message=message,
            error_code="CONFIGURATION_ERROR",
            details=details,
            status_code=500
        )


class RateLimitError(DocumentCheckerException):
    """API 요청 한도 초과 예외"""
    
    def __init__(self, message: str = "API 요청 한도를 초과했습니다", details: Optional[Dict[str, Any]] = None):
        super().__init__(
            message=message,
            error_code="RATE_LIMIT_EXCEEDED",
            details=details,
            status_code=429
        )


class ValidationError(DocumentCheckerException):
    """입력 데이터 검증 예외"""
    
    def __init__(self, message: str, field: Optional[str] = None, details: Optional[Dict[str, Any]] = None):
        validation_details = details or {}
        if field:
            validation_details["field"] = field
            
        super().__init__(
            message=message,
            error_code="VALIDATION_ERROR",
            details=validation_details,
            status_code=400
        )


def create_error_response(
    error_code: str,
    message: str,
    details: Optional[Dict[str, Any]] = None,
    timestamp: Optional[datetime] = None
) -> Dict[str, Any]:
    """
    표준화된 에러 응답을 생성합니다.
    
    Args:
        error_code: 에러 코드
        message: 에러 메시지
        details: 추가 세부 정보
        timestamp: 에러 발생 시간
        
    Returns:
        표준화된 에러 응답 딕셔너리
    """
    return {
        "error": True,
        "error_code": error_code,
        "message": message,
        "details": details or {},
        "timestamp": (timestamp or datetime.now()).isoformat(),
        "success": False
    }


def create_success_response(
    data: Any,
    message: str = "요청이 성공적으로 처리되었습니다",
    timestamp: Optional[datetime] = None
) -> Dict[str, Any]:
    """
    표준화된 성공 응답을 생성합니다.
    
    Args:
        data: 응답 데이터
        message: 성공 메시지
        timestamp: 응답 생성 시간
        
    Returns:
        표준화된 성공 응답 딕셔너리
    """
    return {
        "success": True,
        "message": message,
        "data": data,
        "timestamp": (timestamp or datetime.now()).isoformat(),
        "error": False
    }