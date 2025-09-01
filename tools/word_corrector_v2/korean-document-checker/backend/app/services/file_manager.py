"""
File Manager Service

This module provides a high-level interface for managing file uploads,
validation, and document processing operations.
"""

import os
import logging
from typing import Dict, Optional, Tuple
from datetime import datetime, timedelta

from .document_processor import DocumentProcessor
from .file_validator import FileValidator

logger = logging.getLogger(__name__)


class FileManager:
    """
    High-level file manager that combines file validation and document processing.
    Manages the complete lifecycle of uploaded files.
    """

    def __init__(self, max_file_size: int = 10 * 1024 * 1024, temp_dir: Optional[str] = None):
        """
        Initialize the file manager.
        
        Args:
            max_file_size (int): Maximum allowed file size in bytes
            temp_dir (str, optional): Temporary directory for file storage
        """
        self.validator = FileValidator(max_file_size=max_file_size, temp_dir=temp_dir)
        self.processor = DocumentProcessor()
        self.file_registry = {}  # In-memory registry for tracking files
        
        logger.info("FileManager initialized")

    def upload_and_validate_file(self, file_content: bytes, filename: str) -> Dict:
        """
        Upload and validate a file, returning comprehensive results.
        
        Args:
            file_content (bytes): File content as bytes
            filename (str): Original filename
            
        Returns:
            Dict: Upload and validation results
        """
        try:
            logger.info(f"Processing file upload: {filename}")
            
            # Perform complete validation
            validation_result = self.validator.validate_complete_file(file_content, filename)
            
            if validation_result["is_valid"]:
                # Register the file
                file_id = validation_result["file_id"]
                temp_path = validation_result["temp_path"]
                
                self.file_registry[file_id] = {
                    "original_filename": filename,
                    "temp_path": temp_path,
                    "upload_time": datetime.now(),
                    "file_info": validation_result["file_info"],
                    "processed": False
                }
                
                result = {
                    "success": True,
                    "file_id": file_id,
                    "filename": filename,
                    "size": validation_result["file_info"]["size"],
                    "upload_time": self.file_registry[file_id]["upload_time"].isoformat(),
                    "message": "File uploaded and validated successfully"
                }
                
                logger.info(f"File upload successful: {file_id}")
                return result
            else:
                result = {
                    "success": False,
                    "errors": validation_result["errors"],
                    "message": "File validation failed"
                }
                
                logger.warning(f"File upload failed for {filename}: {validation_result['errors']}")
                return result
                
        except Exception as e:
            logger.error(f"Error in file upload process: {str(e)}")
            return {
                "success": False,
                "errors": [f"Upload error: {str(e)}"],
                "message": "File upload failed due to internal error"
            }

    def get_file_info(self, file_id: str) -> Optional[Dict]:
        """
        Get information about a registered file.
        
        Args:
            file_id (str): Unique file identifier
            
        Returns:
            Dict or None: File information if found, None otherwise
        """
        return self.file_registry.get(file_id)

    def process_document(self, file_id: str) -> Dict:
        """
        Process a document to extract text, structure, and formatting information.
        
        Args:
            file_id (str): Unique file identifier
            
        Returns:
            Dict: Document processing results
        """
        try:
            # Check if file exists in registry
            if file_id not in self.file_registry:
                return {
                    "success": False,
                    "error": "File not found in registry",
                    "message": f"No file found with ID: {file_id}"
                }
            
            file_info = self.file_registry[file_id]
            temp_path = file_info["temp_path"]
            
            # Check if file still exists on disk
            if not os.path.exists(temp_path):
                return {
                    "success": False,
                    "error": "File not found on disk",
                    "message": f"Temporary file has been removed: {temp_path}"
                }
            
            logger.info(f"Processing document: {file_id}")
            
            # Process the document
            document_summary = self.processor.get_document_summary(temp_path)
            
            # Mark as processed
            self.file_registry[file_id]["processed"] = True
            self.file_registry[file_id]["processing_time"] = datetime.now()
            
            result = {
                "success": True,
                "file_id": file_id,
                "original_filename": file_info["original_filename"],
                "document_summary": document_summary,
                "processing_time": self.file_registry[file_id]["processing_time"].isoformat(),
                "message": "Document processed successfully"
            }
            
            logger.info(f"Document processing successful: {file_id}")
            return result
            
        except Exception as e:
            logger.error(f"Error processing document {file_id}: {str(e)}")
            return {
                "success": False,
                "error": f"Processing error: {str(e)}",
                "message": "Document processing failed"
            }

    def cleanup_file(self, file_id: str) -> bool:
        """
        Clean up a file and remove it from registry.
        
        Args:
            file_id (str): Unique file identifier
            
        Returns:
            bool: True if cleanup was successful, False otherwise
        """
        try:
            if file_id in self.file_registry:
                file_info = self.file_registry[file_id]
                temp_path = file_info["temp_path"]
                
                # Clean up the temporary file
                cleanup_success = self.validator.cleanup_temporary_file(temp_path)
                
                # Remove from registry
                del self.file_registry[file_id]
                
                logger.info(f"File cleanup completed: {file_id}")
                return cleanup_success
            else:
                logger.warning(f"File not found in registry for cleanup: {file_id}")
                return False
                
        except Exception as e:
            logger.error(f"Error cleaning up file {file_id}: {str(e)}")
            return False

    def cleanup_old_files(self, max_age_hours: int = 1) -> int:
        """
        Clean up files older than specified age.
        
        Args:
            max_age_hours (int): Maximum age in hours before cleanup
            
        Returns:
            int: Number of files cleaned up
        """
        try:
            current_time = datetime.now()
            cutoff_time = current_time - timedelta(hours=max_age_hours)
            
            files_to_cleanup = []
            for file_id, file_info in self.file_registry.items():
                if file_info["upload_time"] < cutoff_time:
                    files_to_cleanup.append(file_id)
            
            cleanup_count = 0
            for file_id in files_to_cleanup:
                if self.cleanup_file(file_id):
                    cleanup_count += 1
            
            logger.info(f"Cleaned up {cleanup_count} old files")
            return cleanup_count
            
        except Exception as e:
            logger.error(f"Error during old file cleanup: {str(e)}")
            return 0

    def get_registry_status(self) -> Dict:
        """
        Get current status of the file registry.
        
        Returns:
            Dict: Registry status information
        """
        try:
            total_files = len(self.file_registry)
            processed_files = sum(1 for info in self.file_registry.values() if info.get("processed", False))
            
            return {
                "total_files": total_files,
                "processed_files": processed_files,
                "pending_files": total_files - processed_files,
                "registry_keys": list(self.file_registry.keys())
            }
            
        except Exception as e:
            logger.error(f"Error getting registry status: {str(e)}")
            return {
                "total_files": 0,
                "processed_files": 0,
                "pending_files": 0,
                "registry_keys": [],
                "error": str(e)
            }