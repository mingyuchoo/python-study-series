from fastapi import APIRouter, HTTPException

from backend.models.response_models import APIResponse, QuestionsResponse
from backend.services.question_service import question_service

router = APIRouter(prefix="/questions", tags=["Questions"])


@router.get("/", response_model=QuestionsResponse)
async def get_questions():
    """
    모든 질문 조회
    """
    try:
        questions = question_service.get_all_questions()
        return QuestionsResponse(questions=questions, total_count=len(questions))
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{question_id}")
async def get_question(question_id: int):
    """
    특정 질문 조회
    """
    try:
        question = question_service.get_question_by_id(question_id)
        if not question:
            raise HTTPException(status_code=404, detail="Question not found")
        return APIResponse(success=True, message="Success", data=question)
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
