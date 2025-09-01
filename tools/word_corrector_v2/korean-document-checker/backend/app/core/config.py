"""
환경 설정 관리 모듈
Azure OpenAI 및 애플리케이션 설정을 환경변수로 관리
"""

from pydantic_settings import BaseSettings
from typing import Optional
import os


class Settings(BaseSettings):
    """
    애플리케이션 설정 클래스
    환경변수를 통해 설정값을 관리
    """
    
    # Azure OpenAI 설정
    azure_openai_endpoint: Optional[str] = None
    azure_openai_api_key: Optional[str] = None
    azure_openai_api_version: str = "2024-12-01-preview"
    azure_openai_deployment_name: str = "gpt-4"
    
    # 파일 처리 설정
    max_file_size: int = 10 * 1024 * 1024  # 10MB
    temp_file_retention_hours: int = 1
    allowed_file_extensions: list = [".docx"]
    temp_dir: Optional[str] = None  # 임시 파일 저장 디렉터리 (None이면 시스템 기본값 사용)
    
    # 서버 설정
    backend_host: str = "0.0.0.0"
    backend_port: int = 8000
    frontend_url: str = "http://localhost:8501"
    
    # 로깅 설정
    log_level: str = "INFO"
    
    # 개발 모드 설정
    debug: bool = False
    
    class Config:
        env_file = ".env"
        env_file_encoding = "utf-8"
        case_sensitive = False


# 전역 설정 인스턴스
settings = Settings()


def get_settings() -> Settings:
    """
    설정 인스턴스를 반환하는 함수
    FastAPI 의존성 주입에서 사용
    """
    return settings


def validate_azure_openai_config() -> bool:
    """
    Azure OpenAI 설정이 올바르게 구성되었는지 검증
    """
    required_fields = [
        settings.azure_openai_endpoint,
        settings.azure_openai_api_key,
        settings.azure_openai_deployment_name
    ]
    
    return all(field for field in required_fields)


def get_temp_file_path(file_id: str) -> str:
    """
    임시 파일 경로 생성
    """
    temp_dir = os.path.join(os.getcwd(), "temp")
    os.makedirs(temp_dir, exist_ok=True)
    return os.path.join(temp_dir, f"{file_id}.docx")