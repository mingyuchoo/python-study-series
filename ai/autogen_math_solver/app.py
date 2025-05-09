import os
import time
from typing import Any, Callable, Dict, List, Optional, Tuple

import streamlit as st

from agents.example_creator import create_example_creator_agents
# Import our custom modules
from agents.math_solver import create_math_solver_agents
from agents.search_agent import create_search_agents
from database.db_manager import (add_problem, add_solution, get_problem,
                                 get_recent_problems, get_solution,
                                 get_solutions_for_problem, initialize_db)
# 종료 처리를 위한 함수형 API 가져오기
from utils.signal_handler import create_shutdown_handler
from utils.openai_config import get_azure_openai_client


# 함수형 스타일로 종료 처리기 생성
def setup_app_shutdown() -> Dict[str, Any]:
    """애플리케이션 종료 처리를 위한 함수형 설정

    Returns:
        종료 처리 관련 함수들을 포함하는 딕셔너리
    """
    # 종료 처리기 생성
    shutdown_handler = create_shutdown_handler({"exit_message": "수학 문제 해결 앱 자원을 정리하고 종료하는 중..."})

    # 애플리케이션 특화 정리 함수
    def app_cleanup() -> None:
        """수학 문제 해결 앱 특화 정리 작업"""
        # 데이터베이스 연결 등 특화 정리 작업 추가 가능
        pass

    # 종료 시 정리 함수 등록
    shutdown_handler["register_cleanup"](app_cleanup)

    return shutdown_handler


# 데이터베이스 초기화 및 가져오기 함수
@st.cache_resource
def initialize_and_get_db(db_path: str = "math_problems.db") -> str:
    """데이터베이스 초기화 및 경로 반환

    Args:
        db_path: 데이터베이스 파일 경로

    Returns:
        초기화된 데이터베이스 경로
    """
    initialize_db(db_path)
    return db_path


# 에이전트 초기화 함수들 (캐싱 적용)
@st.cache_resource
def get_math_solver_agents() -> Dict[str, Any]:
    """수학 문제 해결 에이전트 생성 및 반환

    Returns:
        수학 문제 해결 에이전트 딕셔너리
    """
    return create_math_solver_agents()


@st.cache_resource
def get_example_creator_agents() -> Dict[str, Any]:
    """실생활 예시 생성 에이전트 생성 및 반환

    Returns:
        실생활 예시 생성 에이전트 딕셔너리
    """
    return create_example_creator_agents()


@st.cache_resource
def get_search_agents() -> Dict[str, Any]:
    """검색 에이전트 생성 및 반환

    Returns:
        검색 에이전트 딕셔너리
    """
    return create_search_agents()


# 앱 설정 함수
def setup_app_config() -> None:
    """앱 기본 설정 구성"""
    # 페이지 설정
    st.set_page_config(
        page_title="수학 문제 해결 멀티 에이전트 시스템",
        page_icon="🧮",
        layout="wide",
        initial_sidebar_state="expanded",
    )

    # 애플리케이션 종료 처리 설정
    return setup_app_shutdown()


# UI 설정 및 스타일 함수
def setup_ui_styles() -> None:
    """앱 UI에 사용되는 CSS 스타일 정의"""
    st.markdown(
        """
<style>
    .main-header {
        font-size: 2.5rem;
        color: #1E88E5;
        text-align: center;
        margin-bottom: 1rem;
    }
    .sub-header {
        font-size: 1.5rem;
        color: #0D47A1;
        margin-top: 2rem;
        margin-bottom: 1rem;
    }
    .agent-message {
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 15px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        line-height: 1.5;
        overflow-wrap: break-word;
    }
    
    /* 에이전트 대화 로그 영역 스타일 */
    .stExpander {
        border: none !important;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
    }
    
    /* 시스템 메시지 스타일 */
    .System {
        background-color: #f1f3f4;
        border-left: 5px solid #607D8B;
        color: #37474F;
        font-style: italic;
    }
    .Math_Analyzer {
        background-color: #E3F2FD;
        border-left: 5px solid #2196F3;
    }
    .Equation_Formulator {
        background-color: #E8F5E9;
        border-left: 5px solid #4CAF50;
    }
    .Solution_Calculator {
        background-color: #FFF3E0;
        border-left: 5px solid #FF9800;
    }
    .Solution_Verifier {
        background-color: #F3E5F5;
        border-left: 5px solid #9C27B0;
    }
    .Explanation_Generator {
        background-color: #FFEBEE;
        border-left: 5px solid #F44336;
    }
    .User_Proxy {
        background-color: #E0F7FA;
        border-left: 5px solid #00BCD4;
    }
    .Example_Creator {
        background-color: #FFF8E1;
        border-left: 5px solid #FFC107;
    }
    .Internet_Searcher {
        background-color: #FAFAFA;
        border-left: 5px solid #9E9E9E;
    }
</style>
""",
        unsafe_allow_html=True,
    )


# 대화 로그 관리 함수
def create_conversation_logger(placeholder):
    """에이전트 대화 로그 관리를 위한 클로저 함수 생성

    Args:
        placeholder: 로그를 표시할 Streamlit placeholder 객체

    Returns:
        로그 업데이트 함수
    """
    # 로그 저장을 위한 내부 상태
    conversation_log = []

    def update_conversation_log(agent_name, message):
        """대화 로그 업데이트 함수

        Args:
            agent_name: 에이전트 이름
            message: 로그 메시지
        """
        conversation_log.append({"agent": agent_name, "message": message})

        # 로그 표시 형식 생성
        log_display = ""
        for entry in conversation_log:
            agent = entry["agent"]
            msg = entry["message"]
            # 에이전트별 스타일 적용
            log_display += f'<div class="agent-message {agent}">'
            log_display += f"<strong>{agent}:</strong><br>{msg}"
            log_display += "</div>"

        # 스트림릿 화면 업데이트
        placeholder.markdown(log_display, unsafe_allow_html=True)

    return update_conversation_log


# 사이드바 설정 함수
def setup_sidebar(db_path: str) -> Optional[int]:
    """사이드바 설정 및 최근 문제 표시

    Args:
        db_path: 데이터베이스 경로

    Returns:
        선택된 문제 ID (없으면 None)
    """
    st.sidebar.title("📋 최근 문제")

    # 최근 문제 가져오기
    recent_problems = get_recent_problems(db_path, limit=10)
    selected_problem_id = None

    # 사이드바에 최근 문제 표시
    if recent_problems:
        st.sidebar.write("이전에 해결한 문제를 선택하세요:")
        for problem in recent_problems:
            if st.sidebar.button(f"{problem['problem_text'][:50]}...", key=f"prob_{problem['id']}"):
                selected_problem_id = problem["id"]
    else:
        st.sidebar.write("아직 해결한 문제가 없습니다.")

    return selected_problem_id


# 문제 해결 탭 설정 함수
def setup_problem_solving_tab(db_path: str):
    """문제 해결 탭 설정 및 화면 구성

    Args:
        db_path: 데이터베이스 경로

    Returns:
        문제 텍스트, 검색 사용 여부, 검색 쿼리, 해결 버튼 상태
    """
    st.markdown("<h2 class='sub-header'>📝 수학 문제 입력</h2>", unsafe_allow_html=True)

    # 문제 입력 영역
    problem_text = st.text_area(
        "수학 문제를 입력하세요:", height=150, placeholder="예: x^2 + 5x + 6 = 0 방정식의 해를 구하시오."
    )

    # 검색 옵션
    use_search = st.checkbox("인터넷 검색 활용하기", value=False)
    search_query = ""

    if use_search:
        search_query = st.text_input(
            "검색 쿼리 (비워두면 문제 텍스트를 사용합니다):", placeholder="예: 이차방정식 해법"
        )

    # 해결 버튼
    solve_button = st.button("문제 해결하기", type="primary")

    return problem_text, use_search, search_query, solve_button


# 대화 로그 영역 설정 함수
def setup_conversation_area():
    """에이전트 대화 로그 영역 설정

    Returns:
        대화 로그 업데이트 함수, 대화 로그 영역
    """
    # 에이전트 대화 로그를 표시할 영역 추가
    agent_conversation_area = st.expander("에이전트 대화 로그", expanded=True)
    conversation_placeholder = agent_conversation_area.empty()

    # 대화 로그 업데이트 함수 생성
    update_conversation_log = create_conversation_logger(conversation_placeholder)

    return update_conversation_log, agent_conversation_area


# 수학 문제 해결 함수
def solve_problem_with_logging(agents, problem_text, update_conversation_log):
    """수학 문제 해결 함수 (로깅 기능 포함)

    Args:
        agents: 수학 문제 해결 에이전트 딕셔너리
        problem_text: 해결할 문제 텍스트
        update_conversation_log: 대화 로그 업데이트 함수

    Returns:
        해결 결과를 포함하는 딕셔너리
    """
    # 그룹챗 설정 가져오기
    group_chat, manager = agents["setup_group_chat"](agents, agents["llm_config"])

    # 메시지 모니터링을 위한 원래 채팅 기록 길이
    original_message_count = len(group_chat.messages)

    # 처음 메시지 추가
    update_conversation_log("System", f"수학 문제 해결 시작: {problem_text}")

    # 채팅 시작 - 메시지 수신 모니터링을 위한 콜백 함수 정의
    def on_new_message_callback(message, agent, to_agent=None):
        # 터미널에 출력되는 형식과 동일하게 메시지 형식화
        if to_agent:
            agent_name = f"{agent} (to {to_agent})"
        else:
            agent_name = agent

        # 스트림릿 화면에 메시지 추가
        update_conversation_log(agent_name, message)

    # 메시지 모니터링을 위한 그룹챗 관리자 후크 설정
    original_initiate_chat = manager.initiate_chat

    def initiate_chat_with_logging(sender, message=None, **kwargs):
        # 처음 메시지 로그 추가
        problem_message = f"Please solve this mathematical problem collaboratively: {problem_text}"
        on_new_message_callback(problem_message, sender.name, "GroupChat")

        # 원래 채팅 시작 함수 호출
        # message 매개변수가 없을 경우 오류 방지
        if message is None:
            result = original_initiate_chat(sender, **kwargs)
        else:
            result = original_initiate_chat(sender, message=message, **kwargs)

        # 채팅 결과에서 메시지 추출 및 로그 추가
        for i, msg in enumerate(group_chat.messages):
            if i >= original_message_count:
                # 메시지 정보 추출
                if "name" in msg and "content" in msg:
                    agent_name = msg["name"]
                    content = msg["content"]

                    # 에이전트 메시지 로그 추가
                    on_new_message_callback(content, agent_name, None)

        return result

    # 채팅 시작 함수 대체
    manager.initiate_chat = initiate_chat_with_logging

    # 채팅 시작
    chat_result = manager.initiate_chat(
        agents["user_proxy"], message=f"Please solve this mathematical problem collaboratively: {problem_text}"
    )

    # 대화 기록 추출 및 처리
    conversation = [{"agent": message["name"], "content": message["content"]} for message in group_chat.messages]

    # 결과 추출
    final_messages = [msg for msg in group_chat.messages if msg["name"] == "Explanation_Generator"]
    explanation = final_messages[-1]["content"] if final_messages else ""
    solution_messages = [msg for msg in group_chat.messages if msg["name"] == "Solution_Calculator"]
    solution = solution_messages[-1]["content"] if solution_messages else ""

    # 최종 결과 표시
    update_conversation_log("System", "수학 문제 해결 완료")

    return {"solution": solution, "explanation": explanation, "conversation": conversation}


# 실생활 예시 생성 함수
def create_example_with_logging(agents, problem_text, solution_text, update_conversation_log):
    """실생활 예시 생성 함수 (로깅 기능 포함)

    Args:
        agents: 예시 생성 에이전트 딕셔너리
        problem_text: 문제 텍스트
        solution_text: 해결책 텍스트
        update_conversation_log: 대화 로그 업데이트 함수

    Returns:
        생성된 예시 메시지
    """
    update_conversation_log("System", f"실생활 예시 생성 시작")

    # 예시 생성 요청 메시지 작성
    message = f"Please create a practical, engaging real-world example that demonstrates the \
    application of the following mathematical problem and its solution:\n\nProblem: {problem_text}\n\nSolution: {solution_text}\n\nCreate an example that shows how this mathematical concept can be applied in a real-world \
    situation. Make it concrete, relatable, and educational. Explain how the mathematical \
    formulas and solution process would be used in this situation.\n"

    # 메시지 모니터링을 위한 콜백 함수 정의
    def on_message_callback(sender, receiver, message):
        # 터미널에 출력되는 형식과 동일하게 메시지 표시
        if receiver:
            sender_name = f"{sender} (to {receiver})"
        else:
            sender_name = sender

        # 스트림릿 화면에 메시지 추가
        update_conversation_log(sender_name, message)

    # 원래 메시지 전송 메서드 참조 저장
    original_initiate_chat = agents["user_proxy"].initiate_chat

    # 메시지 전송 후크 설정
    def initiate_chat_with_logging(recipient, message, **kwargs):
        # 요청 로그 추가
        on_message_callback("User_Proxy", recipient.name, message)

        # 응답 처리를 위한 콜백 정의
        original_receive = recipient.receive

        def receive_with_logging(message, sender, **kwargs):
            # 응답 로그 추가
            on_message_callback(recipient.name, "User_Proxy", message)
            return original_receive(message, sender, **kwargs)

        # 응답 함수 대체
        recipient.receive = receive_with_logging

        # 원래 채팅 시작 함수 호출
        result = original_initiate_chat(recipient, message, **kwargs)

        # 원래 함수 복원
        recipient.receive = original_receive

        return result

    # 메시지 전송 함수 대체
    agents["user_proxy"].initiate_chat = initiate_chat_with_logging

    try:
        # 채팅 시작
        chat_result = agents["user_proxy"].initiate_chat(agents["creator"], message=message)

        # 결과 추출
        example_message = None
        for msg in chat_result.chat_history:
            if msg["role"] == "assistant":
                example_message = msg["content"]
    finally:
        # 원래 함수 복원
        agents["user_proxy"].initiate_chat = original_initiate_chat

    update_conversation_log("System", "실생활 예시 생성 완료")
    return example_message


# 문제 해결 과정 함수
def solve_math_problem(problem_text, use_search, search_query, db_path):
    """수학 문제 해결 과정 전체 관리 함수

    Args:
        problem_text: 해결할 문제 텍스트
        use_search: 검색 사용 여부
        search_query: 검색 쿼리
        db_path: 데이터베이스 경로

    Returns:
        해결 결과 딕셔너리
    """
    # UI 요소 초기화
    progress_bar = st.progress(0)
    status_text = st.empty()

    # 대화 로그 영역 설정
    update_conversation_log, agent_conversation_area = setup_conversation_area()

    # Step 1: 문제를 데이터베이스에 저장
    status_text.text("문제를 데이터베이스에 저장 중...")
    problem_id = add_problem(db_path, problem_text)
    progress_bar.progress(10)

    # Step 2: 인터넷 검색 수행 (요청시)
    search_results = None
    if use_search:
        status_text.text("인터넷 검색 중...")
        search_agents = get_search_agents()
        search_query_text = search_query if search_query.strip() else problem_text
        search_results = search_agents["search_internet"](search_query_text)
        progress_bar.progress(30)
        # 검색 결과 표시
        update_conversation_log("System", f"검색 결과: {search_results}")

    # Step 3: 수학 문제 해결
    status_text.text("수학 문제 해결 중...")
    math_agents = get_math_solver_agents()

    # 검색 결과 포함 처리
    enhanced_problem = (
        f"{problem_text}\n\nAdditional Information from Internet Search:\n{search_results}"
        if search_results
        else problem_text
    )

    # 문제 해결 함수 호출
    solution_result = solve_problem_with_logging(math_agents, enhanced_problem, update_conversation_log)
    progress_bar.progress(70)

    # Step 4: 실생활 예시 생성
    status_text.text("실생활 예시 생성 중...")
    example_agents = get_example_creator_agents()
    # 로깅 기능이 추가된 함수로 예시 생성
    real_world_example = create_example_with_logging(
        example_agents, problem_text, solution_result["solution"], update_conversation_log
    )
    progress_bar.progress(90)

    # Step 5: 결과를 데이터베이스에 저장
    status_text.text("결과 저장 중...")

    # 데이터베이스 저장 로그 추가
    update_conversation_log("System", "문제 해결 결과 저장 중...")

    solution_id = add_solution(
        db_path,
        problem_id,
        solution_result["solution"],
        solution_result["explanation"],
        real_world_example,
        solution_result["conversation"],
    )
    progress_bar.progress(100)

    # 저장 완료 로그 추가
    update_conversation_log("System", f"문제 해결 결과 저장 완료 (해결책 ID: {solution_id})")

    # 상태 요소 정리
    status_text.empty()
    time.sleep(0.5)  # UI 피드백을 위한 짧은 일시 정지
    progress_bar.empty()

    # 결과 반환
    return {
        "problem_id": problem_id,
        "solution_id": solution_id,
        "problem_text": problem_text,
        "solution": solution_result["solution"],
        "explanation": solution_result["explanation"],
        "real_world_example": real_world_example,
        "conversation": solution_result["conversation"],
    }


# 결과 표시 함수
def display_solution_results(result_data):
    """문제 해결 결과를 화면에 표시하는 함수

    Args:
        result_data: 해결 결과 데이터 딕셔너리
    """
    # 성공 메시지 표시
    st.success("문제가 성공적으로 해결되었습니다!")

    # 해결 결과 표시
    st.markdown("<h3 class='sub-header'>🔢 문제 해결 결과</h3>", unsafe_allow_html=True)
    st.markdown(f"**문제:**\n{result_data['problem_text']}")

    # 해답 및 설명
    st.markdown("<h4>📊 해답</h4>", unsafe_allow_html=True)
    st.markdown(result_data["solution"])

    st.markdown("<h4>📝 설명</h4>", unsafe_allow_html=True)
    st.markdown(result_data["explanation"])

    # 실생활 예시
    st.markdown("<h4>🌍 실생활 예시</h4>", unsafe_allow_html=True)
    st.markdown(result_data["real_world_example"])

    # 에이전트 대화 기록
    st.markdown("<h4>💬 에이전트 대화 기록</h4>", unsafe_allow_html=True)
    for message in result_data["conversation"]:
        agent_name = message["agent"]
        content = message["content"]
        st.markdown(
            f"<div class='agent-message {agent_name}'><strong>{agent_name}:</strong> {content}</div>",
            unsafe_allow_html=True,
        )


# 문제 기록 탭 함수
def setup_problem_history_tab(db_path: str, selected_problem_id: Optional[int] = None):
    """문제 기록 탭 설정 및 표시

    Args:
        db_path: 데이터베이스 경로
        selected_problem_id: 선택된 문제 ID (없으면 None)
    """
    st.markdown("<h2 class='sub-header'>📚 문제 기록</h2>", unsafe_allow_html=True)

    # 사이드바에서 문제가 선택된 경우
    if selected_problem_id:
        display_selected_problem(db_path, selected_problem_id)
    else:
        # 문제가 선택되지 않은 경우 모든 문제의 요약 표시
        display_problem_summary(db_path)


# 선택된 문제 표시 함수
def display_selected_problem(db_path: str, problem_id: int):
    """선택된 문제와 해결책 표시

    Args:
        db_path: 데이터베이스 경로
        problem_id: 표시할 문제 ID
    """
    problem = get_problem(db_path, problem_id)
    solutions = get_solutions_for_problem(db_path, problem_id)

    st.markdown(f"**선택한 문제:**\n{problem['problem_text']}")

    if solutions:
        st.markdown(f"**해결 횟수:** {len(solutions)}")

        # 최근 해결책 표시
        latest_solution = solutions[0]

        st.markdown("<h3 class='sub-header'>🔢 최근 해결 결과</h3>", unsafe_allow_html=True)

        # 해답 및 설명
        st.markdown("<h4>📊 해답</h4>", unsafe_allow_html=True)
        st.markdown(latest_solution["solution_text"])

        st.markdown("<h4>📝 설명</h4>", unsafe_allow_html=True)
        st.markdown(latest_solution["explanation"])

        # 실생활 예시
        st.markdown("<h4>🌍 실생활 예시</h4>", unsafe_allow_html=True)
        st.markdown(latest_solution["real_world_example"])

        # 에이전트 대화 기록
        with st.expander("💬 에이전트 대화 기록 보기"):
            for message in latest_solution["agent_conversation"]:
                agent_name = message["agent"]
                content = message["content"]
                st.markdown(
                    f"<div class='agent-message {agent_name}'><strong>{agent_name}:</strong> {content}</div>",
                    unsafe_allow_html=True,
                )
    else:
        st.info("이 문제에 대한 해결 기록이 없습니다.")


# 문제 요약 표시 함수
def display_problem_summary(db_path: str):
    """모든 문제의 요약 표시

    Args:
        db_path: 데이터베이스 경로
    """
    recent_problems = get_recent_problems(db_path, limit=10)

    if recent_problems:
        st.write("왼쪽 사이드바에서 문제를 선택하여 자세한 정보를 확인하세요.")

        # 모든 문제의 테이블 생성
        problem_data = []
        for problem in recent_problems:
            solutions = get_solutions_for_problem(db_path, problem["id"])
            problem_data.append(
                {
                    "ID": problem["id"],
                    "문제": problem["problem_text"][:50] + "...",
                    "생성 시간": problem["created_at"],
                    "해결 횟수": len(solutions),
                }
            )

        st.table(problem_data)
    else:
        st.info("아직 해결한 문제가 없습니다. '문제 해결' 탭에서 새 문제를 입력해보세요.")


# 에이전트 정보 탭 함수
def setup_agent_info_tab():
    """에이전트 정보 탭 설정 및 표시"""
    st.markdown("<h2 class='sub-header'>🤖 에이전트 정보</h2>", unsafe_allow_html=True)

    st.write("이 시스템은 여러 AI 에이전트가 협업하여 수학 문제를 해결합니다. 각 에이전트는 특정 역할을 담당합니다.")

    # 에이전트 설명을 위한 컨테이너 생성
    display_agent_descriptions()

    # 기술 스택 정보 표시
    display_tech_stack()


# 에이전트 설명 표시 함수
def display_agent_descriptions():
    """에이전트 설명 표시"""
    # 컨테이너 생성
    col1, col2 = st.columns(2)

    with col1:
        st.markdown("<h3>수학 문제 해결 에이전트</h3>", unsafe_allow_html=True)

        st.markdown(
            "<div class='agent-message Math_Analyzer'><strong>Math_Analyzer:</strong> 수학 문제를 분석하고 해결 방법을 제안합니다.</div>",
            unsafe_allow_html=True,
        )
        st.markdown(
            "<div class='agent-message Equation_Formulator'><strong>Equation_Formulator:</strong> 문제를 수학적 방정식으로 변환합니다.</div>",
            unsafe_allow_html=True,
        )
        st.markdown(
            "<div class='agent-message Solution_Calculator'><strong>Solution_Calculator:</strong> 방정식을 풀고 계산합니다.</div>",
            unsafe_allow_html=True,
        )
        st.markdown(
            "<div class='agent-message Solution_Verifier'><strong>Solution_Verifier:</strong> 해답의 정확성을 검증합니다.</div>",
            unsafe_allow_html=True,
        )
        st.markdown(
            "<div class='agent-message Explanation_Generator'><strong>Explanation_Generator:</strong> 해답을 이해하기 쉽게 설명합니다.</div>",
            unsafe_allow_html=True,
        )

    with col2:
        st.markdown("<h3>보조 에이전트</h3>", unsafe_allow_html=True)

        st.markdown(
            "<div class='agent-message Example_Creator'><strong>Example_Creator:</strong> 수학 개념의 실생활 예시를 생성합니다.</div>",
            unsafe_allow_html=True,
        )
        st.markdown(
            "<div class='agent-message Internet_Searcher'><strong>Internet_Searcher:</strong> 인터넷에서 관련 정보를 검색합니다.</div>",
            unsafe_allow_html=True,
        )
        st.markdown(
            "<div class='agent-message User_Proxy'><strong>User_Proxy:</strong> 사용자를 대신하여 코드 실행 및 대화 관리를 담당합니다.</div>",
            unsafe_allow_html=True,
        )


# 기술 스택 표시 함수
def display_tech_stack():
    """기술 스택 정보 표시"""
    st.markdown("<h3 class='sub-header'>🔧 기술 스택</h3>", unsafe_allow_html=True)

    tech_col1, tech_col2 = st.columns(2)

    with tech_col1:
        st.markdown("**프론트엔드 & 백엔드:**")
        st.markdown("- Streamlit: 대화형 웹 인터페이스")
        st.markdown("- SQLite: 문제 및 해답 저장")

    with tech_col2:
        st.markdown("**AI & 데이터:**")
        st.markdown("- Azure OpenAI: GPT 모델 제공")
        st.markdown("- AutoGen: 멀티 에이전트 프레임워크")
        st.markdown("- Tavily API: 인터넷 검색 기능")


# 앱 실행 기본 함수
def main():
    """수학 문제 해결 앱 메인 함수

    전체 앱 흐름을 관리하고 각 기능을 함수형으로 연결합니다.
    """
    # 앱 기본 설정
    shutdown_handler = setup_app_config()

    # UI 스타일 설정
    setup_ui_styles()

    # 앱 제목 표시
    st.markdown("<h1 class='main-header'>🧮 수학 문제 해결 멀티 에이전트 시스템</h1>", unsafe_allow_html=True)

    # 데이터베이스 초기화
    db_path = initialize_and_get_db()

    # 사이드바 설정
    selected_problem_id = setup_sidebar(db_path)

    # 탭 영역 생성
    tabs = st.tabs(["문제 해결", "문제 기록", "에이전트 정보"])

    # 문제 해결 탭
    with tabs[0]:
        problem_text, use_search, search_query, solve_button = setup_problem_solving_tab(db_path)

        if solve_button:
            if not problem_text.strip():
                st.error("문제를 입력해주세요.")
            else:
                # 처리 중 스피너 표시
                with st.spinner("에이전트들이 문제를 해결하고 있습니다..."):
                    # 문제 해결 과정 실행
                    result = solve_math_problem(problem_text, use_search, search_query, db_path)

                    # 결과 표시
                    display_solution_results(result)

    # 문제 기록 탭
    with tabs[1]:
        setup_problem_history_tab(db_path, selected_problem_id)

    # 에이전트 정보 탭
    with tabs[2]:
        setup_agent_info_tab()


# 앱 실행
if __name__ == "__main__":
    main()
