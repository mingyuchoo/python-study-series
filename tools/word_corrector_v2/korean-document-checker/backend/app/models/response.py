"""
Response models for the Korean Document Checker API
"""

from pydantic import BaseModel, Field
from typing import List, Optional, Dict, Any
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