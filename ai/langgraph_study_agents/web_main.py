import sqlite3
from contextlib import closing

import streamlit as st

from cli_main import ResearchState, graph

DB_PATH = "reports.db"


def init_db():
    with closing(sqlite3.connect(DB_PATH)) as conn:
        with conn:
            conn.execute(
                """
                CREATE TABLE IF NOT EXISTS reports (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    topic TEXT NOT NULL,
                    report TEXT NOT NULL,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
                """
            )


def save_report_to_db(topic, report):
    with closing(sqlite3.connect(DB_PATH)) as conn:
        with conn:
            conn.execute("INSERT INTO reports (topic, report) VALUES (?, ?)", (topic, report))


def load_reports_from_db():
    with closing(sqlite3.connect(DB_PATH)) as conn:
        cur = conn.execute("SELECT topic, report, created_at FROM reports ORDER BY created_at DESC")
        return cur.fetchall()


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


def render_research_topic(value):
    st.write(f"**키워드:** {value.get('keywords', [])}")


def render_collect_data(value):
    st.write(f"**자료 {len(value.get('collected_data', []))}건 수집**")
    collected_data = value.get("collected_data", [])
    for i, item in enumerate(collected_data):
        content = item.get("content", str(item))
        source = item.get("source") or item.get("url")
        text = content
        if source:
            text += f"\n\n출처: {source}"
        st.text_area(f"자료 {i+1}", value=text, key=f"collected_data_{i}_collect_data", height=120, disabled=True)


def render_write_draft(value):
    st.write(f"**초안 일부:** {value.get('draft', '')[:120]} ...")
    st.text_area(
        "초안 전체 내용",
        value=value.get("draft", ""),
        key="draft_edit_write_draft",
        height=200,
        disabled=True,
    )


def render_review_draft(value):
    st.write(f"**피드백 요약:** {value.get('feedback', '')[:120]} ...")
    st.text_area(
        "피드백 전체 내용",
        value=value.get("feedback", ""),
        key="feedback_edit_review_draft",
        height=200,
        disabled=True,
    )


def render_finalize_report(value):
    st.write(f"**최종 보고서 일부:** {value.get('final_report', '')[:120]} ...")
    st.text_area(
        "최종 보고서 전체 내용",
        value=value.get("final_report", ""),
        key="final_report_edit_finalize_report",
        height=300,
        disabled=True,
    )


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
    render_map = {
        "research_topic": render_research_topic,
        "collect_data": render_collect_data,
        "write_draft": render_write_draft,
        "review_draft": render_review_draft,
        "finalize_report": render_finalize_report,
    }
    for event in graph.stream(state):
        for node, value in event.items():
            if node in agent_names:
                with st.expander(f"{agent_names[node]} 결과 보기", expanded=True):
                    render_func = render_map.get(node)
                    if render_func:
                        render_func(value)
                latest_state.update(value)
    return latest_state


def show_history():
    reports = load_reports_from_db()
    if reports:
        st.header("이전 보고서 기록")
        for idx, (topic, report, created_at) in enumerate(reports):
            with st.expander(f"{topic} (최종 보고서, {created_at})"):
                st.write(report)
        st.markdown("---")
        if st.button("새 연구 시작"):
            st.rerun()


def main():
    st.set_page_config(page_title="연구 보고서 자동화 시스템", layout="wide")
    st.title("연구 보고서 자동화 시스템")
    init_db()
    topic, submitted = input_topic_form()
    if submitted and topic:
        latest_state = run_workflow(topic)
        final_report = latest_state.get("final_report", "(최종 보고서가 생성되지 않았습니다.)")
        save_report_to_db(topic, final_report)
        st.success("\n=== 최종 보고서 ===\n\n" + final_report)
    show_history()


if __name__ == "__main__":
    main()
