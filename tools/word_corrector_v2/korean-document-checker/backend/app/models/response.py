"""
Response models for the Korean Document Checker API
"""

from pydantic import BaseModel, Field
from typing import List, Optional, Dict, Any, Union
from datetime import datetime


class ErrorDetail(BaseModel):
    """Model for individual error details"""
    location: str = Field(..., description="Location of the error in the document")
    error_type: str = Field(..., description="Type of error (grammar, spelling, etc.)")
    current_text: str = Field(..., description="Current text with error")
    suggested_text: str = Field(..., description="Suggested correction")
    confidence: float = Field(..., description="Confidence score of the suggestion (0.0-1.0)")
    explanation: str = Field(..., description="Explanation of the error and correction")


class CheckResult(BaseModel):
    """Model for individual check results"""
    check_type: str = Field(..., description="Type of check performed")
    errors_found: int = Field(..., description="Number of errors found")
    suggestions: List[ErrorDetail] = Field(..., description="List of error details and suggestions")
    summary: str = Field(..., description="Summary of the check results")


class ComprehensiveReport(BaseModel):
    """Model for comprehensive document check report"""
    file_id: str = Field(..., description="Unique identifier of the checked file")
    filename: str = Field(..., description="Original filename")
    total_errors: int = Field(..., description="Total number of errors found across all checks")
    check_results: List[CheckResult] = Field(..., description="Results from individual checks")
    priority_issues: List[ErrorDetail] = Field(..., description="High-priority issues requiring attention")
    overall_score: float = Field(..., description="Overall document quality score (0.0-1.0)")
    recommendations: List[str] = Field(..., description="General recommendations for document improvement")
    processing_time: str = Field(..., description="Processing completion timestamp")


class DocumentInfo(BaseModel):
    """Model for document information"""
    file_id: str = Field(..., description="Unique file identifier")
    original_filename: str = Field(..., description="Original filename")
    size: int = Field(..., description="File size in bytes")
    upload_time: str = Field(..., description="Upload timestamp")
    processed: bool = Field(..., description="Whether the document has been processed")
    processing_time: Optional[str] = Field(None, description="Processing timestamp if processed")


class HealthCheckResponse(BaseModel):
    """Model for health check response"""
    status: str = Field(..., description="Service status")
    service: str = Field(..., description="Service name")
    version: str = Field(..., description="Service version")
    timestamp: str = Field(..., description="Health check timestamp")
    azure_openai_config: str = Field(..., description="Azure OpenAI configuration status")
    azure_openai_connection: Optional[str] = Field(None, description="Azure OpenAI connection status")
    azure_openai_error: Optional[str] = Field(None, description="Azure OpenAI connection error if any")

class ErrorResponse(BaseModel):
    """표준화된 에러 응답 모델"""
    error: bool = Field(True, description="에러 발생 여부")
    error_code: str = Field(..., description="에러 코드")
    message: str = Field(..., description="에러 메시지")
    details: Optional[Dict[str, Any]] = Field(None, description="에러 세부 정보")
    timestamp: str = Field(..., description="에러 발생 시간")
    success: bool = Field(False, description="요청 성공 여부")


class SuccessResponse(BaseModel):
    """표준화된 성공 응답 모델"""
    success: bool = Field(True, description="요청 성공 여부")
    message: str = Field(..., description="성공 메시지")
    data: Any = Field(..., description="응답 데이터")
    timestamp: str = Field(..., description="응답 생성 시간")
    error: bool = Field(False, description="에러 발생 여부")


class ValidationErrorDetail(BaseModel):
    """검증 오류 세부 정보"""
    field: Optional[str] = Field(None, description="오류가 발생한 필드")
    message: str = Field(..., description="검증 오류 메시지")
    invalid_value: Optional[Any] = Field(None, description="잘못된 값")


class ProgressUpdate(BaseModel):
    """진행 상태 업데이트 모델"""
    step: str = Field(..., description="현재 진행 단계")
    progress: float = Field(..., description="진행률 (0.0-1.0)")
    message: str = Field(..., description="진행 상태 메시지")
    timestamp: str = Field(..., description="업데이트 시간")
    estimated_remaining: Optional[int] = Field(None, description="예상 남은 시간 (초)")


class FileUploadProgress(BaseModel):
    """파일 업로드 진행 상태"""
    file_id: Optional[str] = Field(None, description="파일 ID")
    filename: str = Field(..., description="파일명")
    uploaded_bytes: int = Field(..., description="업로드된 바이트 수")
    total_bytes: int = Field(..., description="전체 파일 크기")
    progress: float = Field(..., description="업로드 진행률 (0.0-1.0)")
    status: str = Field(..., description="업로드 상태")
    message: str = Field(..., description="상태 메시지")


class CheckProgress(BaseModel):
    """문서 검사 진행 상태"""
    file_id: str = Field(..., description="파일 ID")
    current_check: str = Field(..., description="현재 수행 중인 검사")
    completed_checks: List[str] = Field(..., description="완료된 검사 목록")
    total_checks: int = Field(..., description="전체 검사 수")
    progress: float = Field(..., description="전체 진행률 (0.0-1.0)")
    estimated_remaining: Optional[int] = Field(None, description="예상 남은 시간 (초)")
    message: str = Field(..., description="진행 상태 메시지")