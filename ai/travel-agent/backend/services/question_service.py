from typing import Dict, List

from backend.database import db_manager
from backend.models.response_models import Question, QuestionOption


class QuestionService:
    """
    질문 관련 비즈니스 로직
    """

    @staticmethod
    def get_all_questions() -> List[Question]:
        """
        모든 질문 조회
        """
        # 질문 조회
        question_query = """
        SELECT 
            q.question_id,
            q.question_text,
            q.question_type,
            q.is_required,
            qc.category_name
        FROM
            questions q
        JOIN
            question_categories qc ON q.category_id = qc.category_id
        ORDER BY
            q.question_order
        """
        questions_data = db_manager.execute_query(question_query)

        # 선택지 조회
        options_query = """
        SELECT 
            qo.question_id,
            qo.option_id,
            qo.option_text,
            qo.option_value,
            qo.option_order
        FROM
            question_options qo
        ORDER BY
            qo.question_id,
            qo.option_order
        """
        options_data = db_manager.execute_query(options_query)

        # 선택지를 질문별로 그룹화
        options_by_question = {}
        for option in options_data:
            question_id = option["question_id"]
            if question_id not in options_by_question:
                options_by_question[question_id] = []
            options_by_question[question_id].append(QuestionOption(**option))

        # Question 객체 생성
        questions = []
        for q_data in questions_data:
            question_id = q_data["question_id"]
            question = Question(
                **q_data, options=options_by_question.get(question_id, [])
            )
            questions.append(question)

        return questions

    @staticmethod
    def get_question_by_id(question_id: int) -> Question:
        """
        특정 질문 조회
        """
        questions = QuestionService.get_all_questions()
        for question in questions:
            if question.question_id == question_id:
                return question
        return None


question_service = QuestionService()
