"""
Document Processor Service

This module provides functionality to extract text, structure, and formatting
information from docx files using python-docx library.
"""

import os
from typing import Dict, List, Optional, Tuple
from pathlib import Path
import logging

from docx import Document
from docx.shared import Inches
from docx.enum.style import WD_STYLE_TYPE
from docx.enum.text import WD_PARAGRAPH_ALIGNMENT


logger = logging.getLogger(__name__)


class DocumentProcessor:
    """
    Document processor for extracting text, structure, and formatting information
    from docx files.
    """

    def __init__(self):
        """Initialize the document processor."""
        pass

    def extract_text_from_docx(self, file_path: str) -> str:
        """
        Extract plain text content from a docx file.
        
        Args:
            file_path (str): Path to the docx file
            
        Returns:
            str: Extracted text content
            
        Raises:
            FileNotFoundError: If the file doesn't exist
            Exception: If the file cannot be processed
        """
        try:
            if not os.path.exists(file_path):
                raise FileNotFoundError(f"File not found: {file_path}")
            
            doc = Document(file_path)
            text_content = []
            
            # Extract text from paragraphs
            for paragraph in doc.paragraphs:
                if paragraph.text.strip():
                    text_content.append(paragraph.text.strip())
            
            # Extract text from tables
            for table in doc.tables:
                for row in table.rows:
                    for cell in row.cells:
                        if cell.text.strip():
                            text_content.append(cell.text.strip())
            
            return "\n".join(text_content)
            
        except Exception as e:
            logger.error(f"Error extracting text from {file_path}: {str(e)}")
            raise Exception(f"Failed to extract text from document: {str(e)}")

    def extract_structure_info(self, file_path: str) -> Dict:
        """
        Extract document structure information including headings, styles, and layout.
        
        Args:
            file_path (str): Path to the docx file
            
        Returns:
            Dict: Document structure information
        """
        try:
            if not os.path.exists(file_path):
                raise FileNotFoundError(f"File not found: {file_path}")
            
            doc = Document(file_path)
            structure_info = {
                "headings": [],
                "paragraph_styles": [],
                "table_count": 0,
                "total_paragraphs": 0,
                "heading_hierarchy": [],
                "style_usage": {}
            }
            
            # Extract heading information and paragraph styles
            for i, paragraph in enumerate(doc.paragraphs):
                structure_info["total_paragraphs"] += 1
                
                style_name = paragraph.style.name if paragraph.style else "Normal"
                
                # Count style usage
                if style_name in structure_info["style_usage"]:
                    structure_info["style_usage"][style_name] += 1
                else:
                    structure_info["style_usage"][style_name] = 1
                
                # Check if it's a heading
                if "Heading" in style_name or "제목" in style_name:
                    heading_info = {
                        "text": paragraph.text.strip(),
                        "style": style_name,
                        "level": self._extract_heading_level(style_name),
                        "paragraph_index": i,
                        "alignment": self._get_alignment_name(paragraph.alignment)
                    }
                    structure_info["headings"].append(heading_info)
                    structure_info["heading_hierarchy"].append({
                        "level": heading_info["level"],
                        "text": heading_info["text"]
                    })
                
                # Store paragraph style info
                paragraph_info = {
                    "index": i,
                    "style": style_name,
                    "text_length": len(paragraph.text),
                    "alignment": self._get_alignment_name(paragraph.alignment)
                }
                structure_info["paragraph_styles"].append(paragraph_info)
            
            # Count tables
            structure_info["table_count"] = len(doc.tables)
            
            return structure_info
            
        except Exception as e:
            logger.error(f"Error extracting structure from {file_path}: {str(e)}")
            raise Exception(f"Failed to extract document structure: {str(e)}")

    def get_formatting_details(self, file_path: str) -> Dict:
        """
        Extract detailed formatting information from the document.
        
        Args:
            file_path (str): Path to the docx file
            
        Returns:
            Dict: Detailed formatting information
        """
        try:
            if not os.path.exists(file_path):
                raise FileNotFoundError(f"File not found: {file_path}")
            
            doc = Document(file_path)
            formatting_details = {
                "styles_defined": [],
                "font_usage": {},
                "paragraph_formatting": [],
                "table_formatting": [],
                "document_properties": {}
            }
            
            # Extract defined styles
            for style in doc.styles:
                if style.type == WD_STYLE_TYPE.PARAGRAPH:
                    style_info = {
                        "name": style.name,
                        "type": "paragraph",
                        "builtin": style.builtin
                    }
                    formatting_details["styles_defined"].append(style_info)
            
            # Extract paragraph formatting details
            for i, paragraph in enumerate(doc.paragraphs):
                para_format = {
                    "index": i,
                    "style_name": paragraph.style.name if paragraph.style else "Normal",
                    "alignment": self._get_alignment_name(paragraph.alignment),
                    "runs_count": len(paragraph.runs),
                    "font_details": []
                }
                
                # Extract font information from runs
                for run in paragraph.runs:
                    if run.text.strip():
                        font_info = {
                            "text": run.text[:50] + "..." if len(run.text) > 50 else run.text,
                            "font_name": run.font.name if run.font.name else "Default",
                            "font_size": str(run.font.size) if run.font.size else "Default",
                            "bold": run.bold if run.bold is not None else False,
                            "italic": run.italic if run.italic is not None else False
                        }
                        para_format["font_details"].append(font_info)
                        
                        # Count font usage
                        font_name = font_info["font_name"]
                        if font_name in formatting_details["font_usage"]:
                            formatting_details["font_usage"][font_name] += 1
                        else:
                            formatting_details["font_usage"][font_name] = 1
                
                formatting_details["paragraph_formatting"].append(para_format)
            
            # Extract table formatting
            for i, table in enumerate(doc.tables):
                table_info = {
                    "index": i,
                    "rows": len(table.rows),
                    "columns": len(table.columns) if table.rows else 0,
                    "style": table.style.name if table.style else "Default"
                }
                formatting_details["table_formatting"].append(table_info)
            
            # Extract document properties
            core_props = doc.core_properties
            formatting_details["document_properties"] = {
                "title": core_props.title or "",
                "author": core_props.author or "",
                "subject": core_props.subject or "",
                "created": str(core_props.created) if core_props.created else "",
                "modified": str(core_props.modified) if core_props.modified else ""
            }
            
            return formatting_details
            
        except Exception as e:
            logger.error(f"Error extracting formatting from {file_path}: {str(e)}")
            raise Exception(f"Failed to extract document formatting: {str(e)}")

    def _extract_heading_level(self, style_name: str) -> int:
        """
        Extract heading level from style name.
        
        Args:
            style_name (str): Style name
            
        Returns:
            int: Heading level (1-9, or 0 if not a heading)
        """
        if not style_name:
            return 0
        
        # Handle English heading styles
        if "Heading" in style_name:
            try:
                # Extract number from "Heading 1", "Heading 2", etc.
                parts = style_name.split()
                if len(parts) > 1 and parts[1].isdigit():
                    return int(parts[1])
            except (ValueError, IndexError):
                pass
        
        # Handle Korean heading styles
        if "제목" in style_name:
            try:
                # Extract number from Korean heading styles
                import re
                numbers = re.findall(r'\d+', style_name)
                if numbers:
                    return int(numbers[0])
            except ValueError:
                pass
        
        return 0

    def _get_alignment_name(self, alignment) -> str:
        """
        Convert alignment enum to readable string.
        
        Args:
            alignment: Paragraph alignment enum
            
        Returns:
            str: Alignment name
        """
        if alignment is None:
            return "left"
        
        alignment_map = {
            WD_PARAGRAPH_ALIGNMENT.LEFT: "left",
            WD_PARAGRAPH_ALIGNMENT.CENTER: "center",
            WD_PARAGRAPH_ALIGNMENT.RIGHT: "right",
            WD_PARAGRAPH_ALIGNMENT.JUSTIFY: "justify"
        }
        
        return alignment_map.get(alignment, "left")

    def get_document_summary(self, file_path: str) -> Dict:
        """
        Get a comprehensive summary of the document including text, structure, and formatting.
        
        Args:
            file_path (str): Path to the docx file
            
        Returns:
            Dict: Complete document summary
        """
        try:
            text_content = self.extract_text_from_docx(file_path)
            structure_info = self.extract_structure_info(file_path)
            formatting_details = self.get_formatting_details(file_path)
            
            # Calculate basic statistics
            word_count = len(text_content.split()) if text_content else 0
            char_count = len(text_content) if text_content else 0
            
            summary = {
                "file_path": file_path,
                "file_size": os.path.getsize(file_path) if os.path.exists(file_path) else 0,
                "text_content": text_content,
                "word_count": word_count,
                "character_count": char_count,
                "structure": structure_info,
                "formatting": formatting_details,
                "processing_timestamp": str(Path(file_path).stat().st_mtime) if os.path.exists(file_path) else ""
            }
            
            return summary
            
        except Exception as e:
            logger.error(f"Error creating document summary for {file_path}: {str(e)}")
            raise Exception(f"Failed to create document summary: {str(e)}")