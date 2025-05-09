import streamlit as st

from cli_main import ResearchState, graph


def input_topic_form():
    with st.form(key="topic_form"):
        topic = st.text_input("연구할 주제를 입력하세요", "")
        submitted = st.form_submit_button("연구 시작")
    return topic.strip(), submitted


def get_agent_names():
    return {
        "research_topic": "[주제 조사 에이전트]",
        "collect_data": "[자료 수집 에이전트]",
        "write_draft": "[초안 작성 에이전트]",
        "review_draft": "[검토 에이전트]",
        "finalize_report": "[최종 작성 에이전트]",
    }


def run_workflow(topic):
    state: ResearchState = {
        "topic": topic,
        "keywords": [],
        "collected_data": [],
        "draft": "",
        "feedback": "",
        "final_report": "",
    }
    agent_names = get_agent_names()
    latest_state = dict(state)
    st.info(f"[시작] '{topic}'에 대한 연구 보고서 작성 진행 중...")
    for event in graph.stream(state):
        for node, value in event.items():
            if node in agent_names:
                with st.expander(f"{agent_names[node]} 결과 보기", expanded=True):
                    if node == "research_topic":
                        st.write(f"**키워드:** {value.get('keywords', [])}")
                    elif node == "collect_data":
                        st.write(f"**자료 {len(value.get('collected_data', []))}건 수집**")
                    elif node == "write_draft":
                        st.write(f"**초안 일부:** {value.get('draft', '')[:120]} ...")
                        st.text_area(
                            "초안 전체 내용",
                            value=value.get("draft", ""),
                            key=f"draft_edit_{node}",
                            height=200,
                            disabled=True
                        )
                    elif node == "review_draft":
                        st.write(f"**피드백 요약:** {value.get('feedback', '')[:120]} ...")
                        st.text_area(
                            "피드백 전체 내용",
                            value=value.get("feedback", ""),
                            key=f"feedback_edit_{node}",
                            height=200,
                            disabled=True
                        )
                    elif node == "finalize_report":
                        st.write(f"**최종 보고서 일부:** {value.get('final_report', '')[:120]} ...")
                        st.text_area(
                            "최종 보고서 전체 내용",
                            value=value.get("final_report", ""),
                            key=f"final_report_edit_{node}",
                            height=300,
                            disabled=True
                        )
                latest_state.update(value)
    return latest_state


def show_history():
    if st.session_state.get("history"):
        st.header("이전 보고서 기록")
        for idx, item in enumerate(reversed(st.session_state["history"])):
            with st.expander(f"{item['topic']} (최종 보고서)"):
                st.write(item["report"])


def main():
    st.set_page_config(page_title="연구 보고서 자동화 시스템", layout="wide")
    st.title("연구 보고서 자동화 시스템")
    if "history" not in st.session_state:
        st.session_state["history"] = []
    topic, submitted = input_topic_form()
    if submitted and topic:
        latest_state = run_workflow(topic)
        st.session_state["history"].append(
            {"topic": topic, "report": latest_state.get("final_report", "(최종 보고서가 생성되지 않았습니다.)")}
        )
        st.success(
            "\n=== 최종 보고서 ===\n\n" + latest_state.get("final_report", "(최종 보고서가 생성되지 않았습니다.)")
        )
    show_history()


if __name__ == "__main__":
    main()
