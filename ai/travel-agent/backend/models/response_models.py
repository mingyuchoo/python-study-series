from pydantic import BaseModel
from typing import List, Optional, Dict, Any
from datetime import datetime

class QuestionOption(BaseModel):
    """질문 선택지"""
    option_id: int
    option_text: str
    option_value: Optional[str]
    option_order: int

class Question(BaseModel):
    """질문"""
    question_id: int
    question_text: str
    question_type: str
    is_required: bool
    category_name: str
    options: List[QuestionOption] = []

class QuestionsResponse(BaseModel):
    """질문 목록 응답"""
    questions: List[Question]
    total_count: int

class SessionResponse(BaseModel):
    """세션 응답"""
    session_id: str
    created_date: datetime
    completed: bool

class TravelPackage(BaseModel):
    """여행 상품"""
    package_id: int
    package_name: str
    country: str
    city: str
    duration_days: int
    min_price: int
    max_price: int
    package_type: str
    description: str
    highlights: str
    similarity_score: Optional[float] = None
    final_score: Optional[float] = None

class RecommendationResponse(BaseModel):
    """추천 응답"""
    session_id: str
    recommendations: List[TravelPackage]
    total_count: int

class APIResponse(BaseModel):
    """공통 API 응답"""
    success: bool
    message: str
    data: Optional[Any] = None