"""
FastAPI 애플리케이션 메인 모듈
한국어 문서 검사 서비스의 백엔드 API 서버
"""

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from fastapi import HTTPException
import uvicorn
import logging
from datetime import datetime

# 미들웨어 임포트
from app.middleware.error_handler import ErrorHandlingMiddleware, RequestValidationMiddleware
from app.middleware.progress_tracker import ProgressTrackingMiddleware

# API 라우터 임포트
from app.api.endpoints import upload
from app.api.endpoints import check
from app.api.endpoints import progress

# 로깅 설정
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

app = FastAPI(
    title="Korean Document Checker API",
    description="한국어 문서 맞춤법 및 품질 검사 AI 서비스",
    version="0.1.0",
    docs_url="/docs",
    redoc_url="/redoc"
)

# 미들웨어 설정
# 1. 에러 처리 미들웨어 (가장 바깥쪽)
app.add_middleware(ErrorHandlingMiddleware)

# 2. 진행률 추적 미들웨어
app.add_middleware(ProgressTrackingMiddleware)

# 3. 요청 검증 미들웨어
app.add_middleware(RequestValidationMiddleware)

# 3. CORS 미들웨어 설정 - 프론트엔드 연동을 위한 설정
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:8501", "http://127.0.0.1:8501"],  # Streamlit 기본 포트
    allow_credentials=True,
    allow_methods=["GET", "POST", "PUT", "DELETE"],
    allow_headers=["*"],
)

# 헬스체크 엔드포인트
@app.get("/api/health")
async def health_check():
    """
    서비스 상태 확인 엔드포인트
    Azure OpenAI 연결 상태도 포함
    """
    from app.core.azure_client import get_azure_client
    from app.core.config import validate_azure_openai_config
    
    health_status = {
        "status": "healthy",
        "service": "Korean Document Checker API",
        "version": "0.1.0",
        "timestamp": datetime.now().isoformat(),
        "azure_openai_config": "valid" if validate_azure_openai_config() else "invalid"
    }
    
    # Azure OpenAI 연결 테스트 (선택적)
    try:
        client = await get_azure_client()
        connection_test = await client.test_connection()
        health_status["azure_openai_connection"] = connection_test["status"]
    except Exception as e:
        health_status["azure_openai_connection"] = "error"
        health_status["azure_openai_error"] = str(e)
    
    return health_status

# 루트 엔드포인트
@app.get("/")
async def root():
    """
    API 루트 엔드포인트
    """
    return {
        "message": "Korean Document Checker API",
        "version": "0.1.0",
        "docs": "/docs",
        "health": "/api/health"
    }

# API 라우터 등록
app.include_router(upload.router, prefix="/api", tags=["upload"])
app.include_router(check.router, prefix="/api", tags=["check"])
app.include_router(progress.router, prefix="/api", tags=["progress"])

if __name__ == "__main__":
    uvicorn.run(
        "app.main:app",
        host="0.0.0.0",
        port=8000,
        reload=True,
        log_level="info"
    )