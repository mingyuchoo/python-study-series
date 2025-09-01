"""
FastAPI 애플리케이션 메인 모듈
한국어 문서 검사 서비스의 백엔드 API 서버
"""

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from fastapi import HTTPException
import uvicorn
from datetime import datetime

# API 라우터 임포트 (향후 구현될 예정)
# from app.api.endpoints import upload, check

app = FastAPI(
    title="Korean Document Checker API",
    description="한국어 문서 맞춤법 및 품질 검사 AI 서비스",
    version="0.1.0",
    docs_url="/docs",
    redoc_url="/redoc"
)

# CORS 미들웨어 설정 - 프론트엔드 연동을 위한 설정
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:8501", "http://127.0.0.1:8501"],  # Streamlit 기본 포트
    allow_credentials=True,
    allow_methods=["GET", "POST", "PUT", "DELETE"],
    allow_headers=["*"],
)

# 전역 예외 핸들러
@app.exception_handler(HTTPException)
async def http_exception_handler(request, exc):
    return JSONResponse(
        status_code=exc.status_code,
        content={
            "error_code": f"HTTP_{exc.status_code}",
            "message": exc.detail,
            "timestamp": datetime.now().isoformat()
        }
    )

@app.exception_handler(Exception)
async def general_exception_handler(request, exc):
    return JSONResponse(
        status_code=500,
        content={
            "error_code": "INTERNAL_SERVER_ERROR",
            "message": "내부 서버 오류가 발생했습니다.",
            "timestamp": datetime.now().isoformat()
        }
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

# API 라우터 등록 (향후 구현될 예정)
# app.include_router(upload.router, prefix="/api", tags=["upload"])
# app.include_router(check.router, prefix="/api", tags=["check"])

if __name__ == "__main__":
    uvicorn.run(
        "app.main:app",
        host="0.0.0.0",
        port=8000,
        reload=True,
        log_level="info"
    )