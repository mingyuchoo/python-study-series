import os
from typing import Any, Dict, List, Optional

import requests
import streamlit as st

# -----------------------------
# 기본 설정
# -----------------------------
DEFAULT_BACKEND_URL = os.environ.get("BACKEND_BASE_URL", "http://127.0.0.1:8000/api/v1")

st.set_page_config(page_title="여행 상담 AI", page_icon="✈️", layout="wide")

# -----------------------------
# 상태 관리
# -----------------------------
if "backend_base_url" not in st.session_state:
    st.session_state.backend_base_url = DEFAULT_BACKEND_URL
if "session_id" not in st.session_state:
    st.session_state.session_id = None
if "questions" not in st.session_state:
    st.session_state.questions = []
if "answers" not in st.session_state:
    st.session_state.answers = {}
if "answers_draft" not in st.session_state:
    # {question_id: {"answer_text": str|None, "answer_value": str|None}}
    st.session_state.answers_draft = {}
if "session_history" not in st.session_state:
    # 최근 사용된 세션 ID 목록 (최신순)
    st.session_state.session_history = []
if "loaded_from_history" not in st.session_state:
    # 세션 이력 선택으로 로딩했는지 여부
    st.session_state.loaded_from_history = False
if "session_history_loaded" not in st.session_state:
    # 앱 시작 후 백엔드에서 세션 이력을 1회 자동 로딩했는지 여부
    st.session_state.session_history_loaded = False
if "session_applied" not in st.session_state:
    # 사용자가 "세션 ID 적용" 버튼으로 현재 세션을 명시적으로 적용했는지 여부
    st.session_state.session_applied = False

# -----------------------------
# 유틸 함수 (API 호출)
# -----------------------------


def api_get(path: str, params: Optional[Dict[str, Any]] = None):
    url = f"{st.session_state.backend_base_url.rstrip('/')}{path}"
    try:
        res = requests.get(url, params=params, timeout=30)
        res.raise_for_status()
        return res.json()
    except requests.RequestException as e:
        st.error(f"요청 실패: GET {url} - {e}")
        return None


def api_post(path: str, json: Optional[Dict[str, Any]] = None):
    url = f"{st.session_state.backend_base_url.rstrip('/')}{path}"
    try:
        res = requests.post(url, json=json, timeout=60)
        res.raise_for_status()
        return res.json()
    except requests.RequestException as e:
        st.error(f"요청 실패: POST {url} - {e}")
        return None


# -----------------------------
# 도우미 함수
# -----------------------------


def ensure_questions_loaded():
    """질문 목록이 없으면 백엔드에서 불러와 초기 draft를 구성"""
    if not st.session_state.questions:
        resp = api_get("/questions/")
        if resp:
            st.session_state.questions = resp.get("questions", [])
            for q in st.session_state.questions:
                qid = q.get("question_id")
                st.session_state.answers_draft.setdefault(
                    qid, {"answer_text": None, "answer_value": None}
                )


def apply_session_answers_to_draft(session_id: str):
    """세션의 답변을 불러와 `answers_draft`에 반영"""
    if not session_id:
        return
    data = api_get(f"/sessions/{session_id}/answers")
    if not (data and data.get("success")):
        st.warning("세션 답변을 불러오지 못했습니다.")
        return
    answers_map = data.get("data", {}) or {}
    # 질문이 없다면 먼저 로드
    ensure_questions_loaded()
    # 각 질문 draft에 값 반영 (텍스트/값 동일 문자열로 저장)
    for q in st.session_state.questions:
        qid = q.get("question_id")
        val = answers_map.get(qid)
        if val is not None:
            st.session_state.answers_draft[qid] = {
                "answer_text": str(val),
                "answer_value": str(val),
            }
    st.success("선택한 세션의 답변 이력을 로드했습니다.")


def maintain_session_history(new_session_id: Optional[str]):
    """세션 이력을 최신순으로 유지하고 중복 제거"""
    if not new_session_id:
        return
    hist = st.session_state.session_history
    # 중복 제거
    hist = [s for s in hist if s != new_session_id]
    # 맨 앞에 추가
    hist.insert(0, new_session_id)
    # 길이 제한 (최근 20개)
    st.session_state.session_history = hist[:20]


def load_session_history_from_backend(limit: int = 20):
    """백엔드로부터 최근 세션 이력을 가져와 session_history를 갱신"""
    resp = api_get("/sessions/recent", params={"limit": limit})
    if resp and resp.get("success"):
        sessions = resp.get("data") or []
        if isinstance(sessions, list):
            st.session_state.session_history = sessions
            return True
    return False


# -----------------------------
# 사이드바
# -----------------------------
with st.sidebar:
    st.header("설정")
    st.session_state.backend_base_url = st.text_input(
        "백엔드 URL",
        value=st.session_state.backend_base_url,
        help="FastAPI 서버 주소 (예: http://127.0.0.1:8000)",
    )
    st.caption("환경변수 BACKEND_BASE_URL 로도 설정 가능합니다.")

    st.markdown("---")
    st.subheader("세션")
    if st.button("새 상담 세션 생성", type="primary"):
        payload = {}  # 서버가 Request에서 IP/UA를 추출하므로 본문은 선택사항
        data = api_post("/sessions/create", json=payload)
        if data and data.get("success"):
            session = data.get("data")
            st.session_state.session_id = (
                session.get("session_id") if isinstance(session, dict) else session
            )
            st.success(f"세션 생성 완료: {st.session_state.session_id}")
            # 세션 이력 업데이트
            maintain_session_history(st.session_state.session_id)
            # 생성만으로는 적용되지 않음
            st.session_state.session_applied = False
        else:
            st.error("세션 생성에 실패했습니다.")

    st.text_input(
        "현재 세션 ID", value=st.session_state.session_id or "", key="session_id_input"
    )
    if st.button("세션 ID 적용"):
        st.session_state.session_id = st.session_state.session_id_input.strip() or None
        if st.session_state.session_id:
            maintain_session_history(st.session_state.session_id)
            apply_session_answers_to_draft(st.session_state.session_id)
            # 수동 적용 시에는 히스토리 로딩 상태로 보지 않음
            st.session_state.loaded_from_history = False
            # 명시적 적용 완료
            st.session_state.session_applied = True
        else:
            # 유효하지 않은 입력 적용 시 비활성화 유지
            st.session_state.session_applied = False

    # 세션 이력 UI
    st.markdown("---")
    # 최초 1회 자동 로드
    if not st.session_state.session_history_loaded:
        load_session_history_from_backend()
        st.session_state.session_history_loaded = True

    hdr_cols = st.columns([5, 1, 1])
    with hdr_cols[0]:
        st.subheader("세션 이력")
    with hdr_cols[1]:
        if st.button("🔄", key="session_hist_refresh", help="세션 이력 새로고침"):
            load_session_history_from_backend()
    with hdr_cols[2]:
        if st.button("🗑️", key="session_hist_clear", help="세션 이력 모두 삭제"):
            resp = api_post("/sessions/clear")
            if resp and resp.get("success"):
                st.session_state.session_history = []
                st.session_state._last_session_history_selected = None
                st.session_state.loaded_from_history = False
                load_session_history_from_backend()
                st.session_state.session_hist_msg = (
                    "success",
                    "서버의 세션 이력을 모두 삭제했습니다.",
                )
            else:
                st.session_state.session_hist_msg = (
                    "error",
                    "세션 이력 삭제에 실패했습니다.",
                )
    # 메시지를 컬럼 바깥(사이드바 전체 너비)에서 표시
    msg = st.session_state.get("session_hist_msg")
    if msg:
        level, text = msg
        if level == "success":
            st.success(text)
        elif level == "warning":
            st.warning(text)
        else:
            st.error(text)
        # 일회성 표시 후 제거
        del st.session_state["session_hist_msg"]
    if st.session_state.session_history:
        selected_hist = st.radio(
            "최근 세션 선택",
            options=st.session_state.session_history,
            index=0,
            key="session_history_radio",
            help="과거 세션을 선택하면 해당 세션의 답변 이력이 적용됩니다.",
        )
        # 선택 변경 시 자동 적용
        prev = st.session_state.get("_last_session_history_selected")
        if selected_hist and selected_hist != prev:
            st.session_state.session_id = selected_hist
            apply_session_answers_to_draft(selected_hist)
            st.session_state._last_session_history_selected = selected_hist
            st.session_state.loaded_from_history = True
            # 히스토리 선택은 자동 적용이므로 버튼 활성화 조건을 만족하지 않음
            st.session_state.session_applied = False
    else:
        st.caption("세션 이력이 없습니다. 세션을 생성하거나 ID를 적용해 보세요.")

# -----------------------------
# 메인 UI
# -----------------------------
st.title("✈️ 해외여행 상담 AI")
st.write("백엔드의 질문/답변/추천/검색 API를 활용한 상담형 프론트엔드")

# 탭 구성: 질문+답변, 추천, 검색, 세션답변
tab_qna, tab_session, tab_reco, tab_search = st.tabs(
    [
        "질문 · 답변",
        "세션 답변 보기",
        "추천 받기",
        "상품 검색",
    ]
)

# -----------------------------
# 탭: 질문 · 답변 (통합)
# -----------------------------
with tab_qna:
    st.subheader("질문 확인 및 답변 작성")
    if not st.session_state.session_id:
        st.warning("먼저 사이드바에서 세션을 생성하거나 세션 ID를 입력하세요.")
    # 1) "질문 불러오기" 버튼
    disable_load = not (
        bool(st.session_state.get("session_applied", False))
        and bool(st.session_state.get("session_id"))
    )
    if st.button("질문 불러오기", type="primary", disabled=disable_load):
        resp = api_get("/questions/")
        if resp:
            st.session_state.questions = resp.get("questions", [])
            # 초깃값 준비
            for q in st.session_state.questions:
                qid = q.get("question_id")
                st.session_state.answers_draft.setdefault(
                    qid, {"answer_text": None, "answer_value": None}
                )
            # 2) 로드된 결과 메시지
            st.success(f"{len(st.session_state.questions)}개 질문 로드")
    if st.session_state.get("loaded_from_history", False) and st.session_state.get(
        "session_id"
    ):
        st.caption(f"현재 세션 ID: {st.session_state.session_id}")

    # 3) 질문 목록 나열
    if st.session_state.questions:
        for q in st.session_state.questions:
            qid = q.get("question_id")
            qtext = q.get("question_text")
            opts = (
                sorted(q.get("options", []), key=lambda x: x.get("option_order", 0))
                if q.get("options")
                else []
            )

            with st.expander(f"Q{qid}. {qtext}", expanded=True):
                if opts:
                    # 선택지 기반 답변: 텍스트/값 동시 설정
                    labels = [f"{o.get('option_text')}" for o in opts]
                    # 현재 선택된 값 찾기
                    current_value = st.session_state.answers_draft.get(qid, {}).get(
                        "answer_value"
                    )
                    index_default = 0
                    if current_value is not None:
                        for i, o in enumerate(opts):
                            if str(o.get("option_value")) == str(current_value):
                                index_default = i
                                break
                    selected_label = st.radio(
                        label="선택지",
                        options=labels,
                        index=index_default if labels else 0,
                        key=f"q_{qid}_radio",
                        horizontal=False,
                    )
                    # 선택한 옵션으로 draft 업데이트
                    sel_opt = next(
                        (o for o in opts if o.get("option_text") == selected_label),
                        None,
                    )
                    if sel_opt:
                        st.session_state.answers_draft[qid] = {
                            "answer_text": sel_opt.get("option_text"),
                            "answer_value": (
                                str(sel_opt.get("option_value"))
                                if sel_opt.get("option_value") is not None
                                else None
                            ),
                        }
                else:
                    # 자유 입력형 질문
                    at = st.text_input(
                        "답변 텍스트",
                        value=(
                            st.session_state.answers_draft.get(qid, {}).get(
                                "answer_text"
                            )
                            or ""
                        ),
                        key=f"q_{qid}_text",
                    )
                    av = st.text_input(
                        "답변 값(선택)",
                        value=(
                            st.session_state.answers_draft.get(qid, {}).get(
                                "answer_value"
                            )
                            or ""
                        ),
                        key=f"q_{qid}_val",
                    )
                    st.session_state.answers_draft[qid] = {
                        "answer_text": at or None,
                        "answer_value": av or None,
                    }

                if (
                    st.button("이 질문 답변 제출", key=f"submit_q_{qid}")
                    and st.session_state.session_id
                ):
                    draft = st.session_state.answers_draft.get(qid) or {}
                    payload = {
                        "session_id": st.session_state.session_id,
                        "question_id": int(qid),
                        "answer_text": draft.get("answer_text"),
                        "answer_value": draft.get("answer_value"),
                    }
                    resp = api_post("/sessions/answer", json=payload)
                    if resp and resp.get("success"):
                        st.success("저장되었습니다")
                    else:
                        st.error("저장 실패")
    else:
        st.info("아직 로드된 질문이 없습니다. '질문 불러오기'를 눌러주세요.")

    # 4) 모든답변일괄 제출 버튼 (하단)
    if st.button("모든답변일괄 제출") and st.session_state.session_id:
        submitted = 0
        for q in st.session_state.questions:
            qid = q.get("question_id")
            draft = st.session_state.answers_draft.get(qid) or {}
            if draft.get("answer_text") or draft.get("answer_value"):
                payload = {
                    "session_id": st.session_state.session_id,
                    "question_id": int(qid),
                    "answer_text": draft.get("answer_text"),
                    "answer_value": draft.get("answer_value"),
                }
                resp = api_post("/sessions/answer", json=payload)
                if resp and resp.get("success"):
                    submitted += 1
        st.success(f"{submitted}개 답변 제출 완료")

# -----------------------------
# 탭 3: 추천 받기
# -----------------------------
with tab_reco:
    st.subheader("개인화 추천")
    if not st.session_state.session_id:
        st.warning("먼저 사이드바에서 세션을 생성하거나 세션 ID를 입력하세요.")
    else:
        limit = st.slider("추천 개수", min_value=1, max_value=20, value=5)
        if st.button("추천 요청"):
            # 1) 현재 작성 중인 draft 답변을 먼저 백엔드에 자동 저장
            #    (세션의 기존 답변 + 현재 작성 중 답변 모두 반영되도록)
            ensure_questions_loaded()
            submitted = 0
            for q in st.session_state.questions:
                qid = q.get("question_id")
                draft = st.session_state.answers_draft.get(qid) or {}
                if draft.get("answer_text") or draft.get("answer_value"):
                    save_payload = {
                        "session_id": st.session_state.session_id,
                        "question_id": int(qid),
                        "answer_text": draft.get("answer_text"),
                        "answer_value": draft.get("answer_value"),
                    }
                    save_resp = api_post("/sessions/answer", json=save_payload)
                    if save_resp and save_resp.get("success"):
                        submitted += 1
            if submitted:
                st.caption(f"임시 답변 {submitted}개를 세션에 반영했습니다.")

            # 2) 추천 요청 호출 (세션의 답변 내역 + 방금 저장된 draft 기반)
            payload = {
                "session_id": st.session_state.session_id,
                "limit": int(limit),
            }
            resp = api_post("/recommendations/", json=payload)
            if resp:
                # RecommendationResponse 구조 예상
                recos = resp.get("recommendations") or resp.get("data", {}).get(
                    "recommendations"
                )
                total = resp.get("total_count") or resp.get("data", {}).get(
                    "total_count"
                )
                if recos is None:
                    # 일부 구현에서 APIResponse로 감쌀 수 있으므로 방어적 처리
                    recos = (
                        resp.get("data") if isinstance(resp.get("data"), list) else []
                    )
                st.write(f"총 {total if total is not None else len(recos)}건")
                if recos:
                    for p in recos:
                        with st.expander(
                            f"[{p.get('package_id')}] {p.get('package_name')}"
                        ):
                            cols = st.columns(3)
                            with cols[0]:
                                st.write(
                                    f"국가/도시: {p.get('country')} / {p.get('city')}"
                                )
                                st.write(f"기간(일): {p.get('duration_days')}")
                            with cols[1]:
                                st.write(
                                    f"가격대: {p.get('min_price')} ~ {p.get('max_price')}"
                                )
                                st.write(f"유형: {p.get('package_type')}")
                            with cols[2]:
                                st.write(f"유사도: {p.get('similarity_score')}")
                                st.write(f"최종점수: {p.get('final_score')}")
                            st.markdown("---")
                            st.write(p.get("description"))
                            st.write(p.get("highlights"))
                else:
                    st.info("추천 결과가 없습니다.")

# -----------------------------
# 탭 4: 상품 검색
# -----------------------------
with tab_search:
    st.subheader("여행 상품 검색")
    query = st.text_input("검색어 (도시/국가/키워드)")
    limit = st.slider("검색 결과 수", min_value=1, max_value=50, value=10)
    with st.expander("필터 (선택)"):
        st.caption("필요한 키-값을 자유롭게 입력하세요.")
        col1, col2 = st.columns(2)
        with col1:
            f_country = st.text_input("국가", key="f_country")
            f_city = st.text_input("도시", key="f_city")
            f_type = st.text_input("상품 유형", key="f_type")
        with col2:
            f_min_price = st.number_input(
                "최소 가격", min_value=0, step=1, key="f_min_price"
            )
            f_max_price = st.number_input(
                "최대 가격", min_value=0, step=1, key="f_max_price"
            )
            f_days = st.number_input("여행 일수", min_value=0, step=1, key="f_days")

    filters: Dict[str, Any] = {}
    if f_country:
        filters["country"] = f_country
    if f_city:
        filters["city"] = f_city
    if f_type:
        filters["package_type"] = f_type
    if f_min_price:
        filters["min_price"] = int(f_min_price)
    if f_max_price:
        filters["max_price"] = int(f_max_price)
    if f_days:
        filters["duration_days"] = int(f_days)

    if st.button("검색 실행"):
        if not query:
            st.warning("검색어를 입력하세요.")
        else:
            payload = {
                "query": query,
                "limit": int(limit),
                "filters": filters if filters else None,
            }
            resp = api_post("/recommendations/search", json=payload)
            if resp and resp.get("success"):
                data = resp.get("data", {})
                pkgs: List[Dict[str, Any]] = data.get("packages", [])
                st.write(f"총 {data.get('total_count', len(pkgs))}건")
                for p in pkgs:
                    with st.expander(
                        f"[{p.get('package_id')}] {p.get('package_name')}"
                    ):
                        cols = st.columns(3)
                        with cols[0]:
                            st.write(f"국가/도시: {p.get('country')} / {p.get('city')}")
                            st.write(f"기간(일): {p.get('duration_days')}")
                        with cols[1]:
                            st.write(
                                f"가격대: {p.get('min_price')} ~ {p.get('max_price')}"
                            )
                            st.write(f"유형: {p.get('package_type')}")
                        with cols[2]:
                            st.write(f"유사도: {p.get('similarity_score')}")
                            st.write(f"최종점수: {p.get('final_score')}")
                        st.markdown("---")
                        st.write(p.get("description"))
                        st.write(p.get("highlights"))
            else:
                st.error("검색에 실패했습니다.")

# -----------------------------
# 탭 5: 세션 답변 보기
# -----------------------------
with tab_session:
    st.subheader("세션의 답변 내역")
    if not st.session_state.session_id:
        st.info("세션 ID가 필요합니다.")
    else:
        data = api_get(f"/sessions/{st.session_state.session_id}/answers")
        if data and data.get("success"):
            answers = data.get("data", [])
            if answers:
                st.json(answers)
            else:
                st.info("저장된 답변이 없습니다.")
        else:
            st.error("세션 답변 조회 실패")
