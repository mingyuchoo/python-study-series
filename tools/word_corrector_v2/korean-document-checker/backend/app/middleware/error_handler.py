"""
에러 처리 미들웨어

애플리케이션 전반의 예외를 처리하고 표준화된 에러 응답을 제공합니다.
로깅, 에러 추적, 사용자 친화적 메시지 변환 등을 담당합니다.
"""

import logging
import traceback
from typing import Dict, Any
from datetime import datetime

from fastapi import Request, HTTPException
from fastapi.responses import JSONResponse
from starlette.middleware.base import BaseHTTPMiddleware

from app.core.exceptions import (
    DocumentCheckerException,
    FileValidationError,
    FileProcessingError,
    FileNotFoundError,
    AzureOpenAIError,
    ConfigurationError,
    RateLimitError,
    ValidationError,
    create_error_response
)

logger = logging.getLogger(__name__)


class ErrorHandlingMiddleware(BaseHTTPMiddleware):
    """에러 처리 미들웨어 클래스"""
    
    async def dispatch(self, request: Request, call_next):
        """
        요청을 처리하고 예외를 캐치하여 표준화된 응답을 반환합니다.
        
        Args:
            request: FastAPI Request 객체
            call_next: 다음 미들웨어 또는 엔드포인트
            
        Returns:
            응답 객체
        """
        try:
            # 요청 시작 시간 기록
            start_time = datetime.now()
            
            # 요청 로깅 (민감한 정보 제외)
            safe_headers = {k: v for k, v in request.headers.items() 
                          if k.lower() not in ['authorization', 'cookie', 'x-api-key']}
            logger.info(f"Request: {request.method} {request.url.path} - Headers: {safe_headers}")
            
            # 다음 미들웨어/엔드포인트 실행
            response = await call_next(request)
            
            # 응답 시간 계산
            process_time = (datetime.now() - start_time).total_seconds()
            
            # 성공적인 응답 로깅
            logger.info(f"Response: {response.status_code} - {process_time:.3f}s")
            
            # 응답 헤더에 처리 시간 추가
            response.headers["X-Process-Time"] = str(process_time)
            
            return response
            
        except DocumentCheckerException as e:
            # 커스텀 애플리케이션 예외 처리
            error_id = log_error_details(e, request, {"error_type": "application"})
            
            # 사용자 친화적 메시지로 변환
            user_message = get_user_friendly_message(e.error_code, e.message)
            
            return JSONResponse(
                status_code=e.status_code,
                content=create_error_response(
                    error_code=e.error_code,
                    message=user_message,
                    details={**e.details, "error_id": error_id},
                    timestamp=e.timestamp
                ),
                headers={"X-Error-ID": error_id}
            )
            
        except HTTPException as e:
            # FastAPI HTTPException 처리
            error_id = log_error_details(e, request, {"error_type": "http"})
            
            error_code = f"HTTP_{e.status_code}"
            user_message = get_user_friendly_message(error_code, str(e.detail))
            
            return JSONResponse(
                status_code=e.status_code,
                content=create_error_response(
                    error_code=error_code,
                    message=user_message,
                    details={"status_code": e.status_code, "error_id": error_id}
                ),
                headers={"X-Error-ID": error_id}
            )
            
        except Exception as e:
            # 예상치 못한 예외 처리
            error_id = log_error_details(e, request, {"error_type": "unexpected"})
            
            # 개발 환경에서는 상세한 에러 정보 제공
            details = {
                "error_id": error_id,
                "type": type(e).__name__
            }
            
            # 프로덕션 환경에서는 민감한 정보 숨김
            user_message = get_user_friendly_message("INTERNAL_SERVER_ERROR", str(e))
            
            return JSONResponse(
                status_code=500,
                content=create_error_response(
                    error_code="INTERNAL_SERVER_ERROR",
                    message=user_message,
                    details=details
                ),
                headers={"X-Error-ID": error_id}
            )


def get_user_friendly_message(error_code: str, original_message: str) -> str:
    """
    에러 코드에 따라 사용자 친화적인 메시지를 반환합니다.
    
    Args:
        error_code: 에러 코드
        original_message: 원본 에러 메시지
        
    Returns:
        사용자 친화적인 에러 메시지
    """
    user_messages = {
        "FILE_VALIDATION_ERROR": "업로드된 파일에 문제가 있습니다. 파일 형식과 크기를 확인해주세요.",
        "FILE_PROCESSING_ERROR": "파일을 처리하는 중 오류가 발생했습니다. 파일이 손상되었을 수 있습니다.",
        "FILE_NOT_FOUND": "요청한 파일을 찾을 수 없습니다. 파일이 삭제되었거나 만료되었을 수 있습니다.",
        "AZURE_OPENAI_ERROR": "AI 서비스에 일시적인 문제가 발생했습니다. 잠시 후 다시 시도해주세요.",
        "CONFIGURATION_ERROR": "서비스 설정에 문제가 있습니다. 관리자에게 문의해주세요.",
        "RATE_LIMIT_EXCEEDED": "요청이 너무 많습니다. 잠시 후 다시 시도해주세요.",
        "VALIDATION_ERROR": "입력 데이터에 오류가 있습니다. 입력 내용을 확인해주세요.",
        "INTERNAL_SERVER_ERROR": "서버에 일시적인 문제가 발생했습니다. 잠시 후 다시 시도해주세요."
    }
    
    return user_messages.get(error_code, original_message)


def log_error_details(
    error: Exception,
    request: Request,
    additional_context: Dict[str, Any] = None
) -> str:
    """
    에러 세부 정보를 로깅합니다.
    
    Args:
        error: 발생한 예외
        request: FastAPI Request 객체
        additional_context: 추가 컨텍스트 정보
        
    Returns:
        에러 ID
    """
    error_id = f"ERR_{int(datetime.now().timestamp())}"
    
    context = {
        "error_id": error_id,
        "error_type": type(error).__name__,
        "error_message": str(error),
        "request_method": request.method,
        "request_url": str(request.url),
        "request_headers": dict(request.headers),
        "timestamp": datetime.now().isoformat()
    }
    
    if additional_context:
        context.update(additional_context)
    
    logger.error(f"Error details [{error_id}]: {context}")
    
    return error_id


class RequestValidationMiddleware(BaseHTTPMiddleware):
    """요청 검증 미들웨어"""
    
    async def dispatch(self, request: Request, call_next):
        """
        요청을 검증하고 처리합니다.
        
        Args:
            request: FastAPI Request 객체
            call_next: 다음 미들웨어 또는 엔드포인트
            
        Returns:
            응답 객체
        """
        try:
            # Content-Length 검증
            content_length = request.headers.get("content-length")
            if content_length:
                content_length = int(content_length)
                max_size = 50 * 1024 * 1024  # 50MB
                
                if content_length > max_size:
                    raise ValidationError(
                        message=f"요청 크기가 너무 큽니다. 최대 {max_size // (1024*1024)}MB까지 허용됩니다.",
                        details={"content_length": content_length, "max_size": max_size}
                    )
            
            # User-Agent 검증 (선택적)
            user_agent = request.headers.get("user-agent", "")
            if not user_agent and request.url.path.startswith("/api/"):
                logger.warning(f"Request without User-Agent: {request.url.path}")
            
            return await call_next(request)
            
        except ValidationError:
            raise
        except Exception as e:
            logger.error(f"Request validation error: {str(e)}")
            raise ValidationError(
                message="요청 검증 중 오류가 발생했습니다.",
                details={"validation_error": str(e)}
            )