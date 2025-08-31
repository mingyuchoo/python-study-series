from fastapi import APIRouter, HTTPException, Request
from sse_starlette.sse import EventSourceResponse

from backend.models.request_models import AnswerRequest, SessionCreateRequest, SessionSummaryRequest
from backend.models.response_models import APIResponse
from backend.services.recommendation_service import recommendation_service
from backend.services.ai_service import ai_service

router = APIRouter(prefix="/sessions", tags=["Sessions"])


@router.post("/create")
async def create_session(
    request: Request, session_request: SessionCreateRequest = None
):
    """
    새 세션 생성
    """
    try:
        # 클라이언트 정보 추출
        user_ip = request.client.host
        user_agent = request.headers.get("user-agent")

        if session_request:
            user_ip = session_request.user_ip or user_ip
            user_agent = session_request.user_agent or user_agent

        session = recommendation_service.create_session(user_ip, user_agent)
        return APIResponse(success=True, message="Session created", data=session)
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{session_id}/summary-recommend")
async def stream_session_summary_and_recommend(session_id: str, body: SessionSummaryRequest):
    """
    세션 답변 요약 및 후보 상품 기반 추천을 SSE로 스트리밍 반환
    - 첫 이벤트로 answers_json을 전송
    - 이후 LLM 응답을 스트리밍
    """
    try:
        vstore = recommendation_service.vector_store

        def event_generator():
            yield from ai_service.stream_summary_and_recommend(
                vector_store=vstore, session_id=session_id, limit=body.limit or 5
            )

        return EventSourceResponse(event_generator())
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/answer")
async def submit_answer(answer_request: AnswerRequest):
    """
    답변 제출
    """
    try:
        success = recommendation_service.save_answer(
            answer_request.session_id,
            answer_request.question_id,
            answer_request.answer_text,
            answer_request.answer_value,
        )

        if success:
            return APIResponse(success=True, message="Answer saved")
        else:
            raise HTTPException(status_code=400, detail="Failed to save answer")
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{session_id}/answers")
async def get_session_answers(session_id: str):
    """
    세션의 답변들 조회
    """
    try:
        answers = recommendation_service.get_user_answers(session_id)
        return APIResponse(success=True, message="Success", data=answers)
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/recent")
async def get_recent_sessions(limit: int = 20):
    """
    최근 세션 ID 목록 조회
    """
    try:
        sessions = recommendation_service.get_recent_sessions(limit=limit)
        return APIResponse(success=True, message="Success", data=sessions)
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/clear")
async def clear_all_sessions():
    """
    모든 세션 기록 삭제 (recommendations, user_answers, user_sessions)
    """
    try:
        recommendation_service.delete_all_sessions()
        return APIResponse(success=True, message="All sessions cleared", data=True)
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
