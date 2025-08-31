from typing import AsyncGenerator, Generator, Optional

from openai import AzureOpenAI

from backend.config import settings
from backend.vector_store import TravelVectorStore
from backend.services.recommendation_service import recommendation_service
from backend.services.question_service import question_service


class AIService:
    """
    Azure OpenAI를 사용하여 컨텍스트 기반 답변을 스트리밍으로 생성
    """

    def __init__(self):
        self.client = AzureOpenAI(
            api_key=settings.AZURE_OPENAI_API_KEY,
            api_version=settings.AZURE_OPENAI_API_VERSION,
            azure_endpoint=settings.AZURE_OPENAI_ENDPOINT,
        )
        self.deployment = settings.AZURE_OPENAI_CHAT_DEPLOYMENT_NAME

    def stream_answer(
        self,
        vector_store: TravelVectorStore,
        question_id: int,
        session_id: str,
        selected_text: Optional[str] = None,
        selected_value: Optional[str] = None,
    ) -> Generator[str, None, None]:
        """
        SSE 바디로 보낼 'data: <chunk>' 라인을 생성하는 제너레이터를 반환
        """
        context = vector_store.build_qna_context(
            question_id=question_id,
            selected_text=selected_text,
            selected_value=selected_value,
        )

        # 이전 답변 가져오기
        previous_answers = recommendation_service.get_user_answers(session_id)
        previous_context = ""
        if previous_answers:
            previous_context = "[이전 답변]\n"
            for q_id, a_text in previous_answers.items():
                q = question_service.get_question_by_id(q_id)
                q_text = q.question_text if q else f"질문 ID {q_id}"
                previous_context += f"질문: {q_text}\n답변: {a_text}\n\n"
            previous_context += "\n"

        system_prompt = (
            "너는 여행 상담 전문가야. 아래 컨텍스트를 바탕으로 사용자의 선택(또는 질문)에 대해 "
            "명확하고 간결하게 한국어로 답변해줘. 필요하면 목록이나 번호를 사용하고, 과도한 추측은 피하고, "
            "모르는 내용은 솔직히 모른다고 말해."
        )
        user_prompt = (
            f"[세션] {session_id}\n"
            f"{previous_context}"
            f"[컨텍스트]\n{context}\n\n"
            f"[요청]\n질문 ID={question_id}에 대해 선택/관심에 대한 설명과 팁을 제공해줘."
        )

        stream = self.client.chat.completions.create(
            model=self.deployment,
            stream=True,
            temperature=0.3,
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ],
        )

        try:
            for event in stream:
                delta = event.choices[0].delta.content if event.choices else None
                if delta:
                    yield delta
            # 종료 시 신호
            yield "[DONE]"
        except Exception as e:
            yield f"[ERROR] {str(e)}"
            yield "[DONE]"


ai_service = AIService()
