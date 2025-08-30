from pydantic_settings import BaseSettings


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
    allowed_origins: list = ["*"]
    allowed_credentials: bool = True
    allowed_methods: list = ["*"]
    allowed_headers: list = ["*"]

    class Config:
        env_file = ".env"


settings = Settings()
