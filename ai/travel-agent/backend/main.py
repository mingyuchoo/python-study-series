from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from contextlib import asynccontextmanager
import uvicorn

from backend.config import settings
from backend.vector_store import TravelVectorStore
from backend.routers import questions, sessions, recommendations

# Vector Store 초기화
vector_store = None

@asynccontextmanager
async def lifespan(backend: FastAPI):
    """앱 생명주기 관리"""
    global vector_store
    
    # 시작 시 Vector Store 초기화
    print("Initializing Vector Store...")
    vector_store = TravelVectorStore(
        settings.sqlite_db_path,
        settings.vector_store_path
    )
    
    # 데이터 로딩 (최초 1회만)
    try:
        vector_store.load_sqlite_data_to_vector_store()
        print("Vector Store initialized successfully!")
    except Exception as e:
        print(f"Warning: Could not load vector store data: {e}")
    
    yield
    
    # 종료 시 정리 작업
    print("Shutting down...")

# FastAPI 앱 생성
backend = FastAPI(
    title=settings.app_name,
    version=settings.app_version,
    description="AI 기반 해외여행 상담 서비스 API",
    lifespan=lifespan
)

# CORS 설정
backend.add_middleware(
    CORSMiddleware,
    allow_origins=settings.allowed_origins,
    allow_credentials=settings.allowed_credentials,
    allow_methods=settings.allowed_methods,
    allow_headers=settings.allowed_headers,
)

# 라우터 등록
backend.include_router(questions.router, prefix=settings.api_prefix)
backend.include_router(sessions.router, prefix=settings.api_prefix)
backend.include_router(recommendations.router, prefix=settings.api_prefix)

@backend.get("/")
async def root():
    """루트 엔드포인트"""
    return {
        "message": "Travel AI Consultation API",
        "version": settings.app_version,
        "status": "running"
    }

@backend.get("/health")
async def health_check():
    """헬스체크 엔드포인트"""
    return {"status": "healthy", "timestamp": "2024-01-01T00:00:00Z"}

if __name__ == "__main__":
    uvicorn.run(
        "backend.main:backend",
        host="0.0.0.0",
        port=8000,
        reload=settings.debug
    )