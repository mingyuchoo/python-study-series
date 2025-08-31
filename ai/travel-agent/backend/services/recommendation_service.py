import uuid
from datetime import datetime
from typing import Dict, List

from backend.config import settings
from backend.database import db_manager
from backend.models.response_models import SessionResponse, TravelPackage
from backend.vector_store import TravelVectorStore


class RecommendationService:
    """
    추천 관련 비즈니스 로직
    """

    def __init__(self):
        self.vector_store = TravelVectorStore(
            settings.sqlite_db_path, settings.vector_store_path
        )
        # 유사도 최소 기준
        self._min_similarity_threshold = 0.4

    def create_session(
        self, user_ip: str = None, user_agent: str = None
    ) -> SessionResponse:
        """
        새 세션 생성
        """
        session_id = f"session_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{str(uuid.uuid4())[:8]}"

        query = """
        INSERT INTO 
            user_sessions (session_id, user_ip, user_agent)
        VALUES
            (?, ?, ?)
        """
        db_manager.execute_insert(query, (session_id, user_ip, user_agent))

        return SessionResponse(
            session_id=session_id, created_date=datetime.now(), completed=False
        )

    def save_answer(
        self,
        session_id: str,
        question_id: int,
        answer_text: str = None,
        answer_value: str = None,
    ) -> bool:
        """
        답변 저장
        """
        # 기존 답변 확인
        check_query = """
        SELECT
            answer_id
        FROM
            user_answers 
        WHERE
            session_id = ?
        AND
            question_id = ?
        """
        existing = db_manager.execute_query(check_query, (session_id, question_id))

        if existing:
            # 기존 답변 업데이트
            query = """
            UPDATE user_answers 
            SET
                answer_text = ?,
                answer_value = ?,
                answered_date = CURRENT_TIMESTAMP
            WHERE
                session_id = ?
            AND
                question_id = ?
            """
            db_manager.execute_insert(
                query, (answer_text, answer_value, session_id, question_id)
            )
        else:
            # 새 답변 삽입
            query = """
            INSERT INTO
                user_answers (session_id, question_id, answer_text, answer_value)
            VALUES
                (?, ?, ?, ?)
            """
            db_manager.execute_insert(
                query, (session_id, question_id, answer_text, answer_value)
            )

        return True

    def get_user_answers(self, session_id: str) -> Dict[int, str]:
        """
        사용자 답변 조회
        """
        query = """
        SELECT
            question_id,
            answer_value,
            answer_text
        FROM
            user_answers
        WHERE
            session_id = ?
        """
        answers_data = db_manager.execute_query(query, (session_id,))

        answers = {}
        for answer in answers_data:
            answers[answer["question_id"]] = (
                answer["answer_value"] or answer["answer_text"]
            )

        return answers

    def get_recent_sessions(self, limit: int = 20) -> List[str]:
        """
        최근 생성된 세션 ID 목록 조회
        """
        query = """
        SELECT
            session_id
        FROM
            user_sessions
        ORDER BY
            datetime(created_date) DESC,
            session_id DESC
        LIMIT ?
        """
        rows = db_manager.execute_query(query, (limit,))
        return [row["session_id"] for row in rows]

    def delete_all_sessions(self) -> bool:
        """
        모든 세션 기록을 삭제 (추천, 답변, 세션)
        """
        # 주의: 외래키 CASCADE가 없으므로 수동 삭제 순서 유지
        db_manager.execute_insert("DELETE FROM recommendations")
        db_manager.execute_insert("DELETE FROM user_answers")
        db_manager.execute_insert("DELETE FROM user_sessions")
        return True

    def get_recommendations(
        self, session_id: str, limit: int = 5
    ) -> List[TravelPackage]:
        """
        개인화된 추천 생성
        """
        # 사용자 답변 조회
        user_answers = self.get_user_answers(session_id)

        if not user_answers:
            return []

        # Vector Store를 통한 추천
        recommendations = self.vector_store.get_personalized_recommendations(
            user_answers
        )

        # 유사도 기준(>= threshold)으로 필터링 후 limit 적용
        filtered_recs = [
            rec
            for rec in recommendations
            if rec.get("similarity_score") is not None
            and rec["similarity_score"] >= self._min_similarity_threshold
        ]
        top_recs = filtered_recs[:limit]

        # TravelPackage 객체로 변환
        travel_packages = []
        for rec in top_recs:
            package = TravelPackage(
                package_id=rec["package_id"],
                package_name=rec["package_name"],
                country=rec["country"],
                city=rec["city"],
                duration_days=rec["duration_days"],
                min_price=rec["min_price"],
                max_price=rec["max_price"],
                package_type=rec["metadata"].get("package_type", ""),
                description=rec["metadata"].get("description", ""),
                highlights=rec["metadata"].get("highlights", ""),
                similarity_score=rec["similarity_score"],
                final_score=rec["final_score"],
            )
            travel_packages.append(package)

        # 추천 결과를 데이터베이스에 저장
        self.vector_store.save_user_session(session_id, user_answers, top_recs)

        return travel_packages


recommendation_service = RecommendationService()
