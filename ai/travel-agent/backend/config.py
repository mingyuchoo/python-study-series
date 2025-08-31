from pydantic_settings import BaseSettings, SettingsConfigDict
from pydantic import field_validator
from typing import List, Any

class Settings(BaseSettings):
    """애플리케이션 설정"""

    # 기본 설정
    app_name: str = "Travel AI Consultation API"
    app_version: str = "1.0.0"
    debug: bool = False

    # 데이터베이스 설정
    sqlite_db_path: str = "./data/database.sqlite3"
    vector_store_path: str = "./data/vector_store"

    # API 설정
    api_prefix: str = "/api/v1"

    # CORS 설정
    # Any로 먼저 수신하여 env 소스의 사전 JSON 파싱을 우회하고, validator에서 리스트로 변환
    allowed_origins: Any = ["*"]
    allowed_credentials: bool = True
    allowed_methods: Any = ["*"]
    allowed_headers: Any = ["*"]

    # 리스트형 환경변수 파서: CSV("a,b") 또는 JSON(["a","b"]) 또는 단일따옴표 JSON(['a','b']) 허용
    @field_validator("allowed_origins", "allowed_methods", "allowed_headers", mode="before")
    @classmethod
    def _parse_list_from_env(cls, v: Any):
        if v is None:
            return v
        if isinstance(v, list):
            return v
        if isinstance(v, str):
            s = v.strip()
            # 빈 문자열은 무시
            if not s:
                return None
            # JSON 시도 (단일따옴표를 이중따옴표로 보정)
            if (s.startswith("[") and s.endswith("]")) or (s.startswith("\"") and s.endswith("\"")):
                try:
                    import json
                    try:
                        return json.loads(s)
                    except json.JSONDecodeError:
                        # 단일따옴표 사용 케이스 보정
                        return json.loads(s.replace("'", '"'))
                except Exception:
                    pass
            # CSV 파싱
            return [item.strip() for item in s.split(",") if item.strip()]
        return v

    # Pydantic v2 설정: .env 사용 및 정의되지 않은 환경 변수 무시
    model_config = SettingsConfigDict(
        env_file=".env",
        env_ignore_empty=True,
        extra="ignore",
    )


settings = Settings()
