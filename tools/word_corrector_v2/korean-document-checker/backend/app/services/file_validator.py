"""
File Validation Service

This module provides functionality to validate uploaded files including
format checking, size limits, and MIME type verification.
"""

import os
import uuid
import mimetypes
from pathlib import Path
from typing import Dict, Optional, Tuple
import logging
import tempfile
import shutil

logger = logging.getLogger(__name__)


class FileValidator:
    """
    File validator for checking file format, size, and MIME type.
    Handles temporary file storage with unique IDs.
    """

    # Supported file extensions
    SUPPORTED_EXTENSIONS = {'.docx'}
    
    # Supported MIME types
    SUPPORTED_MIME_TYPES = {
        'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
    }
    
    # Default maximum file size (10MB)
    DEFAULT_MAX_SIZE = 10 * 1024 * 1024  # 10MB in bytes

    def __init__(self, max_file_size: int = DEFAULT_MAX_SIZE, temp_dir: Optional[str] = None):
        """
        Initialize the file validator.
        
        Args:
            max_file_size (int): Maximum allowed file size in bytes
            temp_dir (str, optional): Temporary directory for file storage
        """
        self.max_file_size = max_file_size
        self.temp_dir = temp_dir or tempfile.gettempdir()
        
        # Create temp directory if it doesn't exist
        os.makedirs(self.temp_dir, exist_ok=True)
        
        logger.info(f"FileValidator initialized with max_size={max_file_size}, temp_dir={self.temp_dir}")

    def validate_file_extension(self, filename: str) -> bool:
        """
        Validate file extension.
        
        Args:
            filename (str): Name of the file
            
        Returns:
            bool: True if extension is supported, False otherwise
        """
        if not filename:
            return False
        
        file_path = Path(filename)
        extension = file_path.suffix.lower()
        
        is_valid = extension in self.SUPPORTED_EXTENSIONS
        logger.debug(f"Extension validation for '{filename}': {is_valid} (extension: {extension})")
        
        return is_valid

    def validate_mime_type(self, file_path: str) -> bool:
        """
        Validate MIME type of the file.
        
        Args:
            file_path (str): Path to the file
            
        Returns:
            bool: True if MIME type is supported, False otherwise
        """
        try:
            mime_type, _ = mimetypes.guess_type(file_path)
            
            # Also check the file content for more accurate MIME type detection
            if mime_type is None:
                # For docx files, check the file signature
                with open(file_path, 'rb') as f:
                    header = f.read(4)
                    # DOCX files start with PK (ZIP signature)
                    if header.startswith(b'PK'):
                        mime_type = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
            
            is_valid = mime_type in self.SUPPORTED_MIME_TYPES
            logger.debug(f"MIME type validation for '{file_path}': {is_valid} (type: {mime_type})")
            
            return is_valid
            
        except Exception as e:
            logger.error(f"Error validating MIME type for {file_path}: {str(e)}")
            return False

    def validate_file_size(self, file_path: str) -> bool:
        """
        Validate file size.
        
        Args:
            file_path (str): Path to the file
            
        Returns:
            bool: True if file size is within limits, False otherwise
        """
        try:
            if not os.path.exists(file_path):
                logger.error(f"File not found for size validation: {file_path}")
                return False
            
            file_size = os.path.getsize(file_path)
            is_valid = file_size <= self.max_file_size
            
            logger.debug(f"Size validation for '{file_path}': {is_valid} "
                        f"(size: {file_size} bytes, limit: {self.max_file_size} bytes)")
            
            return is_valid
            
        except Exception as e:
            logger.error(f"Error validating file size for {file_path}: {str(e)}")
            return False

    def generate_unique_file_id(self) -> str:
        """
        Generate a unique file ID.
        
        Returns:
            str: Unique file identifier
        """
        return str(uuid.uuid4())

    def save_temporary_file(self, file_content: bytes, original_filename: str) -> Tuple[str, str]:
        """
        Save uploaded file content to temporary storage with unique ID.
        
        Args:
            file_content (bytes): File content as bytes
            original_filename (str): Original filename
            
        Returns:
            Tuple[str, str]: (file_id, temp_file_path)
            
        Raises:
            ValueError: If validation fails
            Exception: If file saving fails
        """
        try:
            # Generate unique file ID
            file_id = self.generate_unique_file_id()
            
            # Validate filename extension
            if not self.validate_file_extension(original_filename):
                raise ValueError(f"Unsupported file extension. Supported: {', '.join(self.SUPPORTED_EXTENSIONS)}")
            
            # Create temporary file path
            file_extension = Path(original_filename).suffix.lower()
            temp_filename = f"{file_id}{file_extension}"
            temp_file_path = os.path.join(self.temp_dir, temp_filename)
            
            # Save file content
            with open(temp_file_path, 'wb') as f:
                f.write(file_content)
            
            # Validate file size
            if not self.validate_file_size(temp_file_path):
                # Clean up the file if validation fails
                self.cleanup_temporary_file(temp_file_path)
                raise ValueError(f"File size exceeds limit of {self.max_file_size} bytes")
            
            # Validate MIME type
            if not self.validate_mime_type(temp_file_path):
                # Clean up the file if validation fails
                self.cleanup_temporary_file(temp_file_path)
                raise ValueError(f"Unsupported file type. Supported: {', '.join(self.SUPPORTED_MIME_TYPES)}")
            
            logger.info(f"File saved successfully: {file_id} -> {temp_file_path}")
            return file_id, temp_file_path
            
        except ValueError:
            # Re-raise validation errors
            raise
        except Exception as e:
            logger.error(f"Error saving temporary file: {str(e)}")
            raise Exception(f"Failed to save file: {str(e)}")

    def get_file_info(self, file_path: str) -> Dict:
        """
        Get detailed information about a file.
        
        Args:
            file_path (str): Path to the file
            
        Returns:
            Dict: File information
        """
        try:
            if not os.path.exists(file_path):
                raise FileNotFoundError(f"File not found: {file_path}")
            
            file_stat = os.stat(file_path)
            mime_type, _ = mimetypes.guess_type(file_path)
            
            return {
                "path": file_path,
                "filename": os.path.basename(file_path),
                "size": file_stat.st_size,
                "mime_type": mime_type,
                "extension": Path(file_path).suffix.lower(),
                "created_time": file_stat.st_ctime,
                "modified_time": file_stat.st_mtime,
                "is_valid_extension": self.validate_file_extension(file_path),
                "is_valid_size": self.validate_file_size(file_path),
                "is_valid_mime": self.validate_mime_type(file_path)
            }
            
        except Exception as e:
            logger.error(f"Error getting file info for {file_path}: {str(e)}")
            raise Exception(f"Failed to get file information: {str(e)}")

    def cleanup_temporary_file(self, file_path: str) -> bool:
        """
        Clean up a temporary file.
        
        Args:
            file_path (str): Path to the temporary file
            
        Returns:
            bool: True if cleanup was successful, False otherwise
        """
        try:
            if os.path.exists(file_path):
                os.remove(file_path)
                logger.info(f"Temporary file cleaned up: {file_path}")
                return True
            else:
                logger.warning(f"File not found for cleanup: {file_path}")
                return False
                
        except Exception as e:
            logger.error(f"Error cleaning up temporary file {file_path}: {str(e)}")
            return False

    def validate_complete_file(self, file_content: bytes, filename: str) -> Dict:
        """
        Perform complete validation on file content and filename.
        
        Args:
            file_content (bytes): File content as bytes
            filename (str): Original filename
            
        Returns:
            Dict: Validation results
        """
        validation_result = {
            "is_valid": False,
            "errors": [],
            "warnings": [],
            "file_info": {}
        }
        
        try:
            # Check filename extension
            if not self.validate_file_extension(filename):
                validation_result["errors"].append(
                    f"Unsupported file extension. Supported: {', '.join(self.SUPPORTED_EXTENSIONS)}"
                )
            
            # Check file size (approximate check on content length)
            if len(file_content) > self.max_file_size:
                validation_result["errors"].append(
                    f"File size ({len(file_content)} bytes) exceeds limit of {self.max_file_size} bytes"
                )
            
            # If basic checks pass, save temporarily for detailed validation
            if not validation_result["errors"]:
                try:
                    file_id, temp_path = self.save_temporary_file(file_content, filename)
                    validation_result["file_info"] = self.get_file_info(temp_path)
                    validation_result["file_id"] = file_id
                    validation_result["temp_path"] = temp_path
                    validation_result["is_valid"] = True
                    
                except Exception as e:
                    validation_result["errors"].append(str(e))
            
            return validation_result
            
        except Exception as e:
            logger.error(f"Error in complete file validation: {str(e)}")
            validation_result["errors"].append(f"Validation error: {str(e)}")
            return validation_result