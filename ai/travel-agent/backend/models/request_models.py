from typing import Any, Dict, Optional

from pydantic import BaseModel, Field


class SessionCreateRequest(BaseModel):
    """
    세션 생성 요청
    """
    user_ip: Optional[str] = None
    user_agent: Optional[str] = None


class AnswerRequest(BaseModel):
    """
    답변 제출 요청
    """
    session_id: str = Field(..., description="세션 ID")
    question_id: int = Field(..., description="질문 ID")
    answer_text: Optional[str] = Field(None, description="답변 텍스트")
    answer_value: Optional[str] = Field(None, description="답변 값")


class RecommendationRequest(BaseModel):
    """
    추천 요청
    """
    session_id: str = Field(..., description="세션 ID")
    limit: Optional[int] = Field(5, description="추천 결과 개수", ge=1, le=20)


class SearchRequest(BaseModel):
    """
    검색 요청
    """
    query: str = Field(..., description="검색 쿼리", min_length=1)
    limit: Optional[int] = Field(10, description="검색 결과 개수", ge=1, le=50)
    filters: Optional[Dict[str, Any]] = Field(None, description="검색 필터")


class AskAIRequest(BaseModel):
    """
    질문-선택값으로 AI에게 물어보기 (스트리밍)
    """
    session_id: str = Field(..., description="세션 ID")
    selected_text: Optional[str] = Field(None, description="질문에서 사용자가 선택한 항목의 표시 텍스트")
    selected_value: Optional[str] = Field(None, description="질문에서 사용자가 선택한 항목의 값")
