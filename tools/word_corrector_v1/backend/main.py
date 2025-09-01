from fastapi import FastAPI, File, UploadFile, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
import uvicorn
from document_analyzer import DocumentAnalyzer
from config_manager import config
import tempfile
import os

app = FastAPI(title="Document Review API", version="1.0.0")

# CORS 설정
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

analyzer = DocumentAnalyzer()

@app.post("/analyze-document/")
async def analyze_document(file: UploadFile = File(...)):
    """워드 문서를 업로드하고 분석 결과를 반환합니다."""
    
    # 파일 확장자 검증 - .docx만 지원
    if not file.filename.lower().endswith('.docx'):
        raise HTTPException(status_code=400, detail="Only .docx files are supported. Please convert .doc files to .docx format.")
    
    try:
        # 임시 파일로 저장
        with tempfile.NamedTemporaryFile(delete=False, suffix=os.path.splitext(file.filename)[1]) as tmp_file:
            content = await file.read()
            tmp_file.write(content)
            tmp_file_path = tmp_file.name
        
        # 문서 분석
        analysis_result = analyzer.analyze_document(tmp_file_path)
        
        # 임시 파일 삭제
        os.unlink(tmp_file_path)
        
        return JSONResponse(content=analysis_result)
        
    except Exception as e:
        # 임시 파일이 있다면 삭제
        if 'tmp_file_path' in locals():
            try:
                os.unlink(tmp_file_path)
            except:
                pass
        raise HTTPException(status_code=500, detail=f"Document analysis failed: {str(e)}")

@app.get("/")
async def root():
    return {"message": "Document Review API is running"}

@app.get("/config")
async def get_config():
    """현재 설정을 반환합니다."""
    return {
        "config": config.config,
        "message": "Current configuration settings"
    }

@app.post("/config/reload")
async def reload_config():
    """설정 파일을 다시 로드합니다."""
    try:
        config.reload_config()
        return {"message": "Configuration reloaded successfully"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to reload config: {str(e)}")

@app.get("/config/status")
async def get_config_status():
    """각 분석 모듈의 활성화 상태를 반환합니다."""
    return {
        "analysis_enabled": config.get("analysis_settings.enabled", True),
        "syntax_errors_enabled": config.is_enabled("syntax_errors"),
        "spelling_errors_enabled": config.is_enabled("spelling_errors"),
        "style_consistency_enabled": config.is_enabled("style_consistency"),
        "document_structure_enabled": config.is_enabled("document_structure"),
        "recommendations_enabled": config.is_enabled("recommendations"),
        "korean_spell_check": config.get("spelling_errors.korean_enabled", True),
        "english_spell_check": config.get("spelling_errors.english_enabled", True),
        "spacing_check": config.get("spelling_errors.spacing_enabled", True)
    }

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)