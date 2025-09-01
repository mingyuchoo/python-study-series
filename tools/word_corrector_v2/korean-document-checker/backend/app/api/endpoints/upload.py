"""
파일 업로드 API 엔드포인트
"""

import logging
from fastapi import APIRouter, UploadFile, File, HTTPException, Depends
from fastapi.responses import JSONResponse

from app.models.request import FileUploadResponse, ErrorResponse
from app.services.file_manager import FileManager
from app.core.config import get_settings
from app.core.exceptions import (
    FileValidationError,
    FileProcessingError,
    ValidationError,
    create_success_response
)

logger = logging.getLogger(__name__)

router = APIRouter()

# FileManager 인스턴스를 의존성으로 제공
def get_file_manager() -> FileManager:
    """Get FileManager instance with configuration"""
    settings = get_settings()
    return FileManager(
        max_file_size=settings.max_file_size,
        temp_dir=settings.temp_dir
    )


@router.post("/upload", response_model=FileUploadResponse, tags=["upload"])
async def upload_file(
    file: UploadFile = File(..., description="DOCX file to upload for checking"),
    file_manager: FileManager = Depends(get_file_manager)
) -> FileUploadResponse:
    """
    파일 업로드 엔드포인트
    
    한국어 DOCX 파일을 업로드하고 검증합니다.
    업로드가 성공하면 파일 ID를 반환하여 후속 검사 작업에 사용할 수 있습니다.
    
    Args:
        file: 업로드할 DOCX 파일
        file_manager: 파일 관리 서비스 (의존성 주입)
    
    Returns:
        FileUploadResponse: 업로드 결과 (성공 시 file_id 포함)
    
    Raises:
        HTTPException: 파일 업로드 또는 검증 실패 시
    """
    try:
        logger.info(f"File upload request received: {file.filename}")
        
        # 파일이 제공되었는지 확인
        if not file or not file.filename:
            raise ValidationError(
                message="파일이 제공되지 않았습니다",
                field="file"
            )
        
        # 파일 내용 읽기
        try:
            file_content = await file.read()
        except Exception as e:
            logger.error(f"Error reading file content: {str(e)}")
            raise HTTPException(
                status_code=400,
                detail="Failed to read file content"
            )
        
        # 파일이 비어있는지 확인
        if not file_content:
            raise HTTPException(
                status_code=400,
                detail="Empty file provided"
            )
        
        # 파일 업로드 및 검증 처리
        upload_result = file_manager.upload_and_validate_file(
            file_content=file_content,
            filename=file.filename
        )
        
        if upload_result["success"]:
            # 성공 응답
            response = FileUploadResponse(
                success=True,
                file_id=upload_result["file_id"],
                filename=upload_result["filename"],
                size=upload_result["size"],
                upload_time=upload_result["upload_time"],
                message=upload_result["message"]
            )
            
            logger.info(f"File upload successful: {upload_result['file_id']}")
            return response
        else:
            # 검증 실패 응답
            response = FileUploadResponse(
                success=False,
                message=upload_result["message"],
                errors=upload_result["errors"]
            )
            
            logger.warning(f"File upload validation failed: {upload_result['errors']}")
            return response
            
    except HTTPException:
        # FastAPI HTTPException은 그대로 전파
        raise
    except Exception as e:
        logger.error(f"Unexpected error in file upload: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail="Internal server error during file upload"
        )


@router.get("/upload/status/{file_id}", response_model=dict, tags=["upload"])
async def get_upload_status(
    file_id: str,
    file_manager: FileManager = Depends(get_file_manager)
) -> dict:
    """
    업로드된 파일의 상태 확인
    
    Args:
        file_id: 파일 고유 식별자
        file_manager: 파일 관리 서비스 (의존성 주입)
    
    Returns:
        dict: 파일 상태 정보
    
    Raises:
        HTTPException: 파일을 찾을 수 없는 경우
    """
    try:
        logger.info(f"File status request for: {file_id}")
        
        file_info = file_manager.get_file_info(file_id)
        
        if not file_info:
            raise HTTPException(
                status_code=404,
                detail=f"File not found: {file_id}"
            )
        
        # 파일 상태 정보 반환
        status_info = {
            "file_id": file_id,
            "original_filename": file_info["original_filename"],
            "size": file_info["file_info"]["size"],
            "upload_time": file_info["upload_time"].isoformat(),
            "processed": file_info["processed"],
            "processing_time": file_info.get("processing_time", {}).isoformat() if file_info.get("processing_time") else None,
            "status": "processed" if file_info["processed"] else "uploaded"
        }
        
        logger.info(f"File status retrieved: {file_id}")
        return status_info
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error retrieving file status for {file_id}: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail="Internal server error while retrieving file status"
        )