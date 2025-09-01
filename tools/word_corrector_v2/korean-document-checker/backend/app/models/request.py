"""
Request models for the Korean Document Checker API
"""

from pydantic import BaseModel, Field
from typing import List, Optional
from datetime import datetime


class FileUploadResponse(BaseModel):
    """Response model for file upload endpoint"""
    success: bool = Field(..., description="Whether the upload was successful")
    file_id: Optional[str] = Field(None, description="Unique identifier for the uploaded file")
    filename: Optional[str] = Field(None, description="Original filename")
    size: Optional[int] = Field(None, description="File size in bytes")
    upload_time: Optional[str] = Field(None, description="Upload timestamp in ISO format")
    message: str = Field(..., description="Success or error message")
    errors: Optional[List[str]] = Field(None, description="List of validation errors if any")


class CheckRequest(BaseModel):
    """Request model for document checking"""
    file_id: str = Field(..., description="Unique identifier of the uploaded file")
    check_types: List[str] = Field(
        default=["all"], 
        description="Types of checks to perform: grammar, english_spell, korean_spell, layout, terminology, or all"
    )


class ErrorResponse(BaseModel):
    """Standard error response model"""
    error_code: str = Field(..., description="Error code identifier")
    message: str = Field(..., description="Human-readable error message")
    details: Optional[dict] = Field(None, description="Additional error details")
    timestamp: str = Field(..., description="Error timestamp in ISO format")