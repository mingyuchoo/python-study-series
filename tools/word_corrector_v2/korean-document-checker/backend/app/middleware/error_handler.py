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
            logger.info(f"Request started: {request.method} {request.url.path}")
            
            # 다음 미들웨어 또는 엔드포인트 호출
            response = await call_next(request)
            
            # 요청 완료 시간 계산
            end_time = datetime.now()
            duration = (end_time - start_time).total_seconds()
            
            logger.info(f"Request completed: {request.method} {request.url.path} - {response.status_code} ({duration:.3f}s)")
            
            return response
            
        except HTTPException as e:
            # FastAPI HTTPException은 그대로 전파
            logger.warning(f"HTTP Exception: {e.status_code} - {e.detail}")
            raise e
            
        except DocumentCheckerException as e:
            # 커스텀 예외 처리
            logger.error(f"Document Checker Exception: {e.error_code} - {e.message}")
            return JSONResponse(
                status_code=e.status_code,
                content=create_error_response(
                    error_code=e.error_code,
                    message=e.message,
                    details=e.details
                )
            )
            
        except Exception as e:
            # 예상치 못한 예외 처리
            error_id = f"error_{datetime.now().strftime('%Y%m%d_%H%M%S_%f')}"
            
            logger.error(f"Unexpected error [{error_id}]: {str(e)}")
            logger.error(f"Traceback: {traceback.format_exc()}")
            
            return JSONResponse(
                status_code=500,
                content=create_error_response(
                    error_code="INTERNAL_SERVER_ERROR",
                    message="서버 내부 오류가 발생했습니다",
                    details={
                        "error_id": error_id,
                        "error_type": type(e).__name__
                    }
                )
            )


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
            # 요청 크기 제한 검사 (10MB)
            content_length = request.headers.get("content-length")
            if content_length and int(content_length) > 10 * 1024 * 1024:
                return JSONResponse(
                    status_code=413,
                    content=create_error_response(
                        error_code="REQUEST_TOO_LARGE",
                        message="요청 크기가 너무 큽니다 (최대 10MB)"
                    )
                )
            
            # Content-Type 검증 (파일 업로드 엔드포인트의 경우)
            if request.url.path.startswith("/api/upload"):
                content_type = request.headers.get("content-type", "")
                if not content_type.startswith("multipart/form-data"):
                    return JSONResponse(
                        status_code=400,
                        content=create_error_response(
                            error_code="INVALID_CONTENT_TYPE",
                            message="파일 업로드는 multipart/form-data 형식이어야 합니다"
                        )
                    )
            
            return await call_next(request)
            
        except Exception as e:
            logger.error(f"Request validation error: {str(e)}")
            return JSONResponse(
                status_code=400,
                content=create_error_response(
                    error_code="REQUEST_VALIDATION_ERROR",
                    message="요청 검증 중 오류가 발생했습니다",
                    details={"error": str(e)}
                )
            )