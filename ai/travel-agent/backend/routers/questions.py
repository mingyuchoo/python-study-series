from fastapi import APIRouter, HTTPException
from sse_starlette.sse import EventSourceResponse

from backend.models.response_models import APIResponse, QuestionsResponse
from backend.models.request_models import AskAIRequest
from backend.services.question_service import question_service
from backend.services.ai_service import ai_service
from backend.services.recommendation_service import recommendation_service

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


@router.post("/{question_id}/ask")
async def ask_question(question_id: int, ask: AskAIRequest):
    """
    특정 질문과 사용자가 선택한 항목을 바탕으로 Azure OpenAI에게 질의하고
    SSE(EventSource)로 스트리밍 응답을 반환합니다.
    """
    try:
        # 백엔드의 단일 VectorStore 사용
        vstore = recommendation_service.vector_store

        def event_generator():
            yield from ai_service.stream_answer(
                vector_store=vstore,
                question_id=question_id,
                session_id=ask.session_id,
                selected_text=ask.selected_text,
                selected_value=ask.selected_value,
            )

        return EventSourceResponse(event_generator())
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
