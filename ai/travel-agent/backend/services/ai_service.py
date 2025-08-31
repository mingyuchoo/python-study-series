from typing import Generator, Optional

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


    def stream_summary_and_recommend(
        self,
        vector_store: TravelVectorStore,
        session_id: str,
        limit: int = 5,
    ) -> Generator[str, None, None]:
        """
        세션의 답변을 질문 텍스트와 매핑해 JSON으로 보여주고,
        ChromaDB/SQLite에서 읽어온 후보 상품을 바탕으로 Azure OpenAI가 요약/추천을 생성하도록 스트리밍합니다.
        """
        try:
            # 1) 세션 답변 수집 및 질문 텍스트 매핑
            user_answers = recommendation_service.get_user_answers(session_id)
            mapped_items = []
            for q_id, ans in sorted(user_answers.items()):
                q = question_service.get_question_by_id(q_id)
                q_text = q.question_text if q else f"질문 ID {q_id}"
                mapped_items.append({
                    "question_id": q_id,
                    "question_text": q_text,
                    "answer": ans,
                })

            # 2) 후보 상품 조회 (벡터스토어 기반 후보 리스트)
            candidates = vector_store.get_personalized_recommendations(user_answers)
            # 프롬프트 크기 절약을 위해 상위 10개만 컨텍스트로 전달
            top_candidates = candidates[: min(10, len(candidates))]

            # 3) 시스템/유저 프롬프트 구성
            system_prompt = (
                "너는 여행 컨시어지야. 주어진 세션 답변과 후보 상품을 바탕으로 한국어로 간결하게 요약하고, "
                "사용자 선호에 가장 잘 맞는 여행 상품을 최대 {limit}개 추천해. 추천 사유를 각 항목별로 짧게 설명해. "
                "가격/기간/유형/도시 등의 핵심 스펙도 함께 표로 설명해."
                "웹 화면에 렌더링 될 수 있도록 HTML 형태로 전달해."
            )
            # 컨텍스트 JSON 문자열화 (안전한 간단 직렬화)
            import json as _json
            ctx = {
                "session_id": session_id,
                "answers_mapping": mapped_items,
                "candidates": [
                    {
                        "package_id": c.get("package_id"),
                        "package_name": c.get("package_name"),
                        "country": c.get("country"),
                        "city": c.get("city"),
                        "duration_days": c.get("duration_days"),
                        "min_price": c.get("min_price"),
                        "max_price": c.get("max_price"),
                        "package_type": (c.get("metadata", {}) or {}).get("package_type"),
                        "description": (c.get("metadata", {}) or {}).get("description"),
                        "highlights": (c.get("metadata", {}) or {}).get("highlights"),
                        "similarity_score": c.get("similarity_score"),
                        "final_score": c.get("final_score"),
                    }
                    for c in top_candidates
                ],
            }

            user_prompt = (
                "[세션 답변 JSON]\n" + _json.dumps(ctx.get("answers_mapping", []), ensure_ascii=False, indent=2) +
                "\n\n[후보 상품 JSON]\n" + _json.dumps(ctx.get("candidates", []), ensure_ascii=False, indent=2) +
                f"\n\n[요청]\n1) 세션 답변을 간단히 요약 정리\n2) 답변 항목과 후보 상품을 매칭해 최적의 추천 {limit}개 선정\n3) 각 추천에 대해 이유/핵심 스펙 요약\n4) 마지막에 한 줄 제안문 제공"
            )

            stream = self.client.chat.completions.create(
                model=self.deployment,
                stream=True,
                temperature=0.3,
                messages=[
                    {"role": "system", "content": system_prompt.format(limit=limit)},
                    {"role": "user", "content": user_prompt},
                ],
            )

            # 4) 스트리밍 전, 먼저 answers_mapping JSON을 그대로 흘려보내 사용자에게 표시
            yield _json.dumps({"type": "answers_json", "data": mapped_items}, ensure_ascii=False)

            for event in stream:
                delta = event.choices[0].delta.content if event.choices else None
                if delta:
                    yield delta
            yield "[DONE]"
        except Exception as e:
            yield f"[ERROR] {str(e)}"
            yield "[DONE]"

ai_service = AIService()
