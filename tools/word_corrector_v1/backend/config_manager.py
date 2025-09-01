import toml
import os
from typing import Dict, Any, List
import logging

class ConfigManager:
    """TOML 설정 파일을 관리하는 클래스"""
    
    def __init__(self, config_path: str = "config.toml"):
        self.config_path = config_path
        self.config = self._load_config()
        self._setup_logging()
    
    def _load_config(self) -> Dict[str, Any]:
        """설정 파일을 로드합니다."""
        try:
            if os.path.exists(self.config_path):
                with open(self.config_path, 'r', encoding='utf-8') as f:
                    return toml.load(f)
            else:
                logging.warning(f"Config file {self.config_path} not found. Using default settings.")
                return self._get_default_config()
        except Exception as e:
            logging.error(f"Error loading config: {e}. Using default settings.")
            return self._get_default_config()
    
    def _get_default_config(self) -> Dict[str, Any]:
        """기본 설정을 반환합니다."""
        return {
            "analysis_settings": {"enabled": True},
            "syntax_errors": {
                "enabled": True,
                "max_errors_per_paragraph": 3,
                "ignore_short_paragraphs": True,
                "min_paragraph_length": 10,
                "check_punctuation": True,
                "check_punctuation_capitalization": False,
                "check_brackets": True,
                "check_quotes": True,
                "ignore_patterns": []
            },
            "spelling_errors": {
                "enabled": True,
                "korean_enabled": True,
                "korean_max_errors_per_paragraph": 5,
                "korean_max_text_length": 500,
                "korean_min_korean_chars": 5,
                "korean_api_delay": 0.1,
                "korean_max_consecutive_errors": 10,
                "english_enabled": True,
                "english_max_errors_per_paragraph": 3,
                "english_min_word_length": 3,
                "spacing_enabled": True,
                "spacing_max_errors_per_paragraph": 2,
                "spacing_min_text_length": 10,
                "ignore_words": [],
                "technical_terms": [],
                "korean_skip_patterns": [
                    "^[a-zA-Z0-9\\s\\.,!?]+$",
                    "^\\d+[\\.,]?\\d*$",
                    "^[\\s\\.,!?\\-_()\\[\\]{}]+$"
                ]
            },
            "style_consistency": {
                "enabled": True,
                "heading_structure_enabled": True,
                "max_heading_level_skip": 1,
                "allow_empty_headings": False,
                "font_consistency_enabled": True,
                "max_different_fonts": 3,
                "max_different_font_sizes": 5,
                "min_font_usage_threshold": 2
            },
            "document_structure": {
                "enabled": True,
                "max_empty_paragraph_ratio": 0.4,
                "min_paragraphs_for_analysis": 5,
                "analyze_images": True,
                "analyze_tables": True,
                "ignore_single_line_paragraphs": True
            },
            "error_handling": {
                "continue_on_error": True,
                "log_errors": True,
                "max_analysis_time_seconds": 30,
                "syntax_timeout": 10,
                "spelling_timeout": 15,
                "style_timeout": 10,
                "structure_timeout": 5
            },
            "recommendations": {
                "enabled": True,
                "max_recommendations": 10,
                "priority_order": ["syntax_errors", "spelling_errors", "style_consistency", "document_structure"],
                "severity_thresholds": {"critical": 10, "warning": 5, "info": 1}
            }
        }
    
    def _setup_logging(self):
        """로깅을 설정합니다."""
        if self.get("error_handling.log_errors", True):
            logging.basicConfig(
                level=logging.INFO,
                format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
            )
    
    def get(self, key: str, default: Any = None) -> Any:
        """점 표기법으로 설정값을 가져옵니다. (예: 'syntax_errors.enabled')"""
        keys = key.split('.')
        value = self.config
        
        try:
            for k in keys:
                value = value[k]
            return value
        except (KeyError, TypeError):
            return default
    
    def is_enabled(self, section: str) -> bool:
        """특정 섹션이 활성화되어 있는지 확인합니다."""
        return self.get(f"{section}.enabled", True)
    
    def get_ignore_patterns(self, section: str) -> List[str]:
        """특정 섹션의 무시 패턴을 가져옵니다."""
        return self.get(f"{section}.ignore_patterns", [])
    
    def get_ignore_words(self) -> List[str]:
        """맞춤법 검사에서 무시할 단어 목록을 가져옵니다."""
        ignore_words = self.get("spelling_errors.ignore_words", [])
        technical_terms = self.get("spelling_errors.technical_terms", [])
        return ignore_words + technical_terms
    
    def should_ignore_short_paragraphs(self) -> bool:
        """짧은 문단을 무시할지 여부를 반환합니다."""
        return self.get("syntax_errors.ignore_short_paragraphs", True)
    
    def get_min_paragraph_length(self) -> int:
        """최소 문단 길이를 반환합니다."""
        return self.get("syntax_errors.min_paragraph_length", 10)
    
    def get_max_errors_per_paragraph(self, section: str) -> int:
        """문단당 최대 오류 수를 반환합니다."""
        return self.get(f"{section}.max_errors_per_paragraph", 5)
    
    def reload_config(self):
        """설정 파일을 다시 로드합니다."""
        self.config = self._load_config()
        logging.info("Configuration reloaded")

# 전역 설정 인스턴스
config = ConfigManager()