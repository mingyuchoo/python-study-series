import os
from typing import Dict, List, TypedDict

from dotenv import load_dotenv
from langchain_community.tools.tavily_search import TavilySearchResults
from langchain_core.prompts import ChatPromptTemplate
from langchain_openai import AzureChatOpenAI
from langgraph.graph import END, StateGraph

# 환경 변수 로드 (Azure OpenAI 및 Tavily)
load_dotenv()

# Azure OpenAI 환경 변수 매핑 (이미 .env에서 가져옴을 가정)
os.environ["AZURE_API_KEY"] = os.environ["AZURE_OPENAI_API_KEY"]
os.environ["AZURE_API_BASE"] = os.environ["AZURE_OPENAI_ENDPOINT"]
os.environ["AZURE_API_VERSION"] = os.environ["AZURE_OPENAI_API_VERSION"]
os.environ["AZURE_API_MODEL"] = os.environ["AZURE_OPENAI_CHAT_DEPLOYMENT_NAME"]

# LLM 및 검색 도구 초기화
tavily_search_tool = TavilySearchResults(max_results=5)
llm = AzureChatOpenAI(api_version=os.environ["AZURE_API_VERSION"], model=os.environ["AZURE_API_MODEL"])

# 상태 정의: 워크플로우에서 공유되는 데이터
topic_default = "AI의 지속 가능성에 대한 영향"


class ResearchState(TypedDict):
    topic: str
    keywords: List[str]
    collected_data: List[Dict]
    draft: str
    feedback: str
    final_report: str


# 1. 주제 조사 에이전트
def research_topic(state: ResearchState) -> ResearchState:
    prompt = ChatPromptTemplate.from_template(
        "주제: {topic}\n이 주제에 대해 연구 보고서를 작성하기 위해 필요한 핵심 키워드와 주요 질문을 3개씩 생성하세요."
    )
    chain = prompt | llm
    response = chain.invoke({"topic": state["topic"]})
    # 키워드/질문 추출 로직(예시: 첫 3개 줄을 키워드로)
    lines = [line.strip() for line in response.content.split("\n") if line.strip()]
    keywords = lines[:3] if len(lines) >= 3 else lines
    state["keywords"] = keywords
    return state


# 2. 자료 수집 에이전트
def collect_data(state: ResearchState) -> ResearchState:
    query = f"{state['topic']} {' '.join(state['keywords'])}"
    results = tavily_search_tool.invoke({"query": query})
    state["collected_data"] = results if isinstance(results, list) else [results]
    return state


# 3. 초안 작성 에이전트
def write_draft(state: ResearchState) -> ResearchState:
    prompt = ChatPromptTemplate.from_template(
        "주제: {topic}\n수집된 자료: {data}\n이 자료를 바탕으로 300단어 이내의 연구 보고서 초안을 작성하세요."
    )
    chain = prompt | llm
    data_summary = "\n".join([f"- {item.get('content', str(item))}" for item in state["collected_data"]])
    response = chain.invoke({"topic": state["topic"], "data": data_summary})
    state["draft"] = response.content
    return state


# 4. 검토 에이전트
def review_draft(state: ResearchState) -> ResearchState:
    prompt = ChatPromptTemplate.from_template(
        "다음 초안을 검토하고, 개선할 점과 구체적인 피드백을 제공하세요.\n초안: {draft}"
    )
    chain = prompt | llm
    response = chain.invoke({"draft": state["draft"]})
    state["feedback"] = response.content
    return state


# 5. 최종 작성 에이전트
def finalize_report(state: ResearchState) -> ResearchState:
    prompt = ChatPromptTemplate.from_template(
        "초안: {draft}\n검토 피드백: {feedback}\n피드백을 반영하여 최종 연구 보고서를 작성하세요."
    )
    chain = prompt | llm
    response = chain.invoke({"draft": state["draft"], "feedback": state["feedback"]})
    state["final_report"] = response.content
    return state


# LangGraph 워크플로우 정의
workflow = StateGraph(ResearchState)
workflow.add_node("research_topic", research_topic)
workflow.add_node("collect_data", collect_data)
workflow.add_node("write_draft", write_draft)
workflow.add_node("review_draft", review_draft)
workflow.add_node("finalize_report", finalize_report)
workflow.add_edge("research_topic", "collect_data")
workflow.add_edge("collect_data", "write_draft")
workflow.add_edge("write_draft", "review_draft")
workflow.add_edge("review_draft", "finalize_report")
workflow.add_edge("finalize_report", END)
workflow.set_entry_point("research_topic")
graph = workflow.compile()


def get_user_topic() -> str:
    try:
        return input("연구할 주제를 입력하세요: ").strip()
    except KeyboardInterrupt:
        print("\nCtrl+C 입력으로 시스템을 종료합니다.")
        return ""


def print_agent_step(node: str, value: dict, agent_names: dict) -> None:
    print(f"{agent_names[node]} 작업 중...")
    if node == "research_topic":
        print(f"  → 키워드: {value.get('keywords', [])}")
    elif node == "collect_data":
        print(f"  → 자료 {len(value.get('collected_data', []))}건 수집")
    elif node == "write_draft":
        print(f"  → 초안 일부: {value.get('draft', '')[:120]} ...")
        print("\n[초안 전체 내용]\n" + value.get("draft", ""))
    elif node == "review_draft":
        print(f"  → 피드백 요약: {value.get('feedback', '')[:120]} ...")
        print("\n[피드백 전체 내용]\n" + value.get("feedback", ""))
    elif node == "finalize_report":
        print(f"  → 최종 보고서 일부: {value.get('final_report', '')[:120]} ...")
        print("\n[최종 보고서 전체 내용]\n" + value.get("final_report", ""))


def human_edit_step(node: str, value: dict) -> dict:
    if node == "write_draft":
        user_action = input("초안을 수정하려면 'edit', 승인하려면 Enter를 누르세요: ").strip().lower()
        if user_action == "edit":
            print("에디터를 종료하려면 빈 줄에서 Enter를 두 번 누르세요.")
            print("--- 초안 편집 시작 ---")
            edited_lines = []
            while True:
                line = input()
                if line == "":
                    if edited_lines and edited_lines[-1] == "":
                        break
                edited_lines.append(line)
            edited_draft = "\n".join([l for l in edited_lines if l != ""])
            value["draft"] = edited_draft
            print("--- 편집된 초안이 저장되었습니다. ---\n")
    elif node == "review_draft":
        user_action = input("피드백을 수정하려면 'edit', 승인하려면 Enter를 누르세요: ").strip().lower()
        if user_action == "edit":
            print("에디터를 종료하려면 빈 줄에서 Enter를 두 번 누르세요.")
            print("--- 피드백 편집 시작 ---")
            edited_lines = []
            while True:
                line = input()
                if line == "":
                    if edited_lines and edited_lines[-1] == "":
                        break
                edited_lines.append(line)
            edited_feedback = "\n".join([l for l in edited_lines if l != ""])
            value["feedback"] = edited_feedback
            print("--- 편집된 피드백이 저장되었습니다. ---\n")
    elif node == "finalize_report":
        user_action = input("최종 보고서를 수정하려면 'edit', 승인하려면 Enter를 누르세요: ").strip().lower()
        if user_action == "edit":
            print("에디터를 종료하려면 빈 줄에서 Enter를 두 번 누르세요.")
            print("--- 최종 보고서 편집 시작 ---")
            edited_lines = []
            while True:
                line = input()
                if line == "":
                    if edited_lines and edited_lines[-1] == "":
                        break
                edited_lines.append(line)
            edited_report = "\n".join([l for l in edited_lines if l != ""])
            value["final_report"] = edited_report
            print("--- 편집된 최종 보고서가 저장되었습니다. ---\n")
    return value


def run_workflow_for_topic(topic: str, graph, agent_names: dict) -> None:
    state: ResearchState = {
        "topic": topic,
        "keywords": [],
        "collected_data": [],
        "draft": "",
        "feedback": "",
        "final_report": "",
    }
    print(f"\n[시작] '{topic}'에 대한 연구 보고서 작성 진행 중...\n")
    latest_state = dict(state)
    for event in graph.stream(state):
        for node, value in event.items():
            if node in agent_names:
                print_agent_step(node, value, agent_names)
                value = human_edit_step(node, value)
                latest_state.update(value)
    print("\n=== 최종 보고서 ===")
    print(latest_state.get("final_report", "(최종 보고서가 생성되지 않았습니다.)"))
    print("\n============================================\n")


def main() -> None:
    print("연구 보고서 자동화 시스템에 오신 것을 환영합니다.")
    print("종료하려면 'quit' 또는 'exit'를 입력하세요.\n")
    agent_names = {
        "research_topic": "[주제 조사 에이전트]",
        "collect_data": "[자료 수집 에이전트]",
        "write_draft": "[초안 작성 에이전트]",
        "review_draft": "[검토 에이전트]",
        "finalize_report": "[최종 작성 에이전트]",
    }
    try:
        while True:
            topic = get_user_topic()
            if not topic or topic.lower() in ["quit", "exit"]:
                print("시스템을 종료합니다.")
                break
            run_workflow_for_topic(topic, graph, agent_names)
    except KeyboardInterrupt:
        print("\nCtrl+C 입력으로 시스템을 종료합니다.")


if __name__ == "__main__":
    main()
