import streamlit as st
import requests
import json
from typing import Dict, Any
import pandas as pd

# 페이지 설정
st.set_page_config(
    page_title="문서 검토 서비스",
    page_icon="📄",
    layout="wide"
)

# API 서버 URL
API_BASE_URL = "http://localhost:8000"

@st.cache_data(ttl=30)  # 30초 캐시
def get_config_status():
    """백엔드에서 설정 상태를 가져옵니다."""
    try:
        response = requests.get(f"{API_BASE_URL}/config/status")
        if response.status_code == 200:
            return response.json()
        else:
            return None
    except:
        return None

def main():
    st.title("📄 마이크로소프트 워드 문서 검토 서비스")
    st.markdown("워드 문서를 업로드하여 구문 오류, 맞춤법, 스타일 일관성을 검토하세요.")
    
    # 설정 상태 확인
    config_status = get_config_status()
    
    # 사이드바
    with st.sidebar:
        st.header("📋 검토 항목")
        st.markdown("""
        - **구문 오류**: 괄호, 따옴표, 문장부호 검사
        - **맞춤법**: 한국어/영어 맞춤법 및 띄어쓰기
        - **스타일 일관성**: 제목 스타일, 폰트 일관성
        - **문서 구조**: 목차 구조 및 전체적인 문서 구성
        """)
        
        st.header("⚠️ 주의사항")
        st.markdown("""
        - **.docx 파일만 지원**됩니다
        - .doc 파일은 먼저 .docx로 변환해주세요
        - Git이 설치되어 있어야 한국어 패키지가 정상 작동합니다
        """)
        
        # 설정 상태 표시
        if config_status:
            st.header("⚙️ 현재 설정 상태")
            status_emoji = "✅" if config_status.get("analysis_enabled", True) else "❌"
            st.write(f"{status_emoji} 전체 분석: {'활성화' if config_status.get('analysis_enabled', True) else '비활성화'}")
            
            st.write("**세부 기능:**")
            st.write(f"{'✅' if config_status.get('syntax_errors_enabled', True) else '❌'} 구문 오류 검사")
            st.write(f"{'✅' if config_status.get('spelling_errors_enabled', True) else '❌'} 맞춤법 검사")
            st.write(f"{'✅' if config_status.get('style_consistency_enabled', True) else '❌'} 스타일 일관성")
            st.write(f"{'✅' if config_status.get('document_structure_enabled', True) else '❌'} 문서 구조")
            
            if st.button("🔄 설정 새로고침"):
                st.experimental_rerun()
    
    # 파일 업로드
    uploaded_file = st.file_uploader(
        "워드 문서를 선택하세요 (.docx만 지원)",
        type=['docx'],
        help="마이크로소프트 워드 .docx 형식만 지원됩니다. .doc 파일은 먼저 .docx로 변환해주세요."
    )
    
    if uploaded_file is not None:
        # 파일 정보 표시
        st.success(f"파일 업로드 완료: {uploaded_file.name}")
        
        # 분석 버튼
        if st.button("📊 문서 분석 시작", type="primary"):
            with st.spinner("문서를 분석하고 있습니다..."):
                try:
                    # API 호출
                    files = {"file": (uploaded_file.name, uploaded_file.getvalue(), uploaded_file.type)}
                    response = requests.post(f"{API_BASE_URL}/analyze-document/", files=files)
                    
                    if response.status_code == 200:
                        analysis_result = response.json()
                        display_analysis_results(analysis_result)
                    else:
                        st.error(f"분석 중 오류가 발생했습니다: {response.text}")
                        
                except requests.exceptions.ConnectionError:
                    st.error("API 서버에 연결할 수 없습니다. 백엔드 서버가 실행 중인지 확인해주세요.")
                except Exception as e:
                    st.error(f"예상치 못한 오류가 발생했습니다: {str(e)}")

def display_analysis_results(result: Dict[str, Any]):
    """분석 결과를 표시합니다."""
    
    # 분석 설정 정보 표시
    if "analysis_config" in result:
        with st.expander("⚙️ 분석 설정 정보"):
            config_info = result["analysis_config"]
            col1, col2 = st.columns(2)
            with col1:
                st.write(f"구문 오류 검사: {'✅' if config_info.get('syntax_enabled', True) else '❌'}")
                st.write(f"맞춤법 검사: {'✅' if config_info.get('spelling_enabled', True) else '❌'}")
            with col2:
                st.write(f"스타일 일관성: {'✅' if config_info.get('style_enabled', True) else '❌'}")
                st.write(f"문서 구조: {'✅' if config_info.get('structure_enabled', True) else '❌'}")
            
            if "analysis_time" in result:
                st.write(f"분석 소요 시간: {result['analysis_time']:.2f}초")
    
    # 전체 요약
    st.header("📊 분석 결과 요약")
    
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        syntax_errors = len(result["syntax_errors"])
        st.metric("구문 오류", syntax_errors, delta=None if syntax_errors == 0 else f"-{syntax_errors}")
    
    with col2:
        total_spelling = (
            len(result["spelling_errors"]["korean"]) +
            len(result["spelling_errors"]["english"]) +
            len(result["spelling_errors"]["spacing"])
        )
        st.metric("맞춤법 오류", total_spelling, delta=None if total_spelling == 0 else f"-{total_spelling}")
    
    with col3:
        toc_valid = result["style_consistency"]["toc_structure"]["valid"]
        st.metric("목차 구조", "정상" if toc_valid else "문제", delta="✅" if toc_valid else "❌")
    
    with col4:
        total_chars = result["statistics"]["total_characters"]
        st.metric("총 문자 수", f"{total_chars:,}")
    
    # 추천사항
    st.header("💡 추천사항")
    for i, recommendation in enumerate(result["recommendations"], 1):
        st.info(f"{i}. {recommendation}")
    
    # 탭으로 상세 결과 표시
    tab1, tab2, tab3, tab4, tab5 = st.tabs(["구문 오류", "맞춤법 검사", "스타일 일관성", "문서 구조", "통계"])
    
    with tab1:
        display_syntax_errors(result["syntax_errors"])
    
    with tab2:
        display_spelling_errors(result["spelling_errors"])
    
    with tab3:
        display_style_consistency(result["style_consistency"])
    
    with tab4:
        display_document_structure(result["document_structure"])
    
    with tab5:
        display_statistics(result["statistics"])

def display_syntax_errors(syntax_errors):
    """구문 오류를 표시합니다."""
    st.subheader("🔍 구문 오류 검사 결과")
    
    if not syntax_errors:
        st.success("구문 오류가 발견되지 않았습니다! ✅")
        return
    
    st.warning(f"총 {len(syntax_errors)}개의 구문 오류가 발견되었습니다.")
    
    for i, error in enumerate(syntax_errors, 1):
        with st.expander(f"오류 {i}: {error['type']} (문단 {error['paragraph']})"):
            st.write(f"**설명**: {error['description']}")
            st.code(error['text'], language=None)

def display_spelling_errors(spelling_errors):
    """맞춤법 오류를 표시합니다."""
    st.subheader("✏️ 맞춤법 검사 결과")
    
    # 한국어 맞춤법
    st.write("### 한국어 맞춤법")
    if spelling_errors["korean"]:
        for error in spelling_errors["korean"]:
            with st.expander(f"문단 {error['paragraph']}: {error['original']} → {error['suggestion']}"):
                st.code(error['context'], language=None)
    else:
        st.success("한국어 맞춤법 오류가 없습니다.")
    
    # 영어 맞춤법
    st.write("### 영어 맞춤법")
    if spelling_errors["english"]:
        for error in spelling_errors["english"]:
            suggestions = ", ".join(error['suggestions']) if error['suggestions'] else "제안 없음"
            with st.expander(f"문단 {error['paragraph']}: {error['word']} → {suggestions}"):
                st.code(error['context'], language=None)
    else:
        st.success("영어 맞춤법 오류가 없습니다.")
    
    # 띄어쓰기
    st.write("### 띄어쓰기")
    if spelling_errors["spacing"]:
        for error in spelling_errors["spacing"]:
            with st.expander(f"문단 {error['paragraph']}: 띄어쓰기 교정 제안"):
                st.write("**원본:**")
                st.code(error['original'], language=None)
                st.write("**교정:**")
                st.code(error['corrected'], language=None)
    else:
        st.success("띄어쓰기 오류가 없습니다.")

def display_style_consistency(style_consistency):
    """스타일 일관성을 표시합니다."""
    st.subheader("🎨 스타일 일관성 검사")
    
    # 제목 구조
    st.write("### 제목 구조")
    headings = style_consistency["headings"]
    if headings:
        df_headings = pd.DataFrame(headings)
        st.dataframe(df_headings, use_container_width=True)
        
        # 목차 구조 검증
        toc = style_consistency["toc_structure"]
        if toc["valid"]:
            st.success("목차 구조가 올바릅니다! ✅")
        else:
            st.warning("목차 구조에 문제가 있습니다:")
            for issue in toc["issues"]:
                st.write(f"- {issue['description']}")
    else:
        st.info("제목이 발견되지 않았습니다.")
    
    # 폰트 일관성
    st.write("### 폰트 사용 현황")
    font_info = style_consistency["font_consistency"]
    
    col1, col2 = st.columns(2)
    with col1:
        st.write("**사용된 폰트:**")
        if font_info["fonts_used"]:
            for font, count in font_info["fonts_used"].items():
                st.write(f"- {font}: {count}회")
        else:
            st.write("폰트 정보를 찾을 수 없습니다.")
    
    with col2:
        st.write("**사용된 폰트 크기:**")
        if font_info["font_sizes_used"]:
            for size, count in font_info["font_sizes_used"].items():
                st.write(f"- {size}: {count}회")
        else:
            st.write("폰트 크기 정보를 찾을 수 없습니다.")

def display_document_structure(structure):
    """문서 구조를 표시합니다."""
    st.subheader("📋 문서 구조 분석")
    
    col1, col2 = st.columns(2)
    
    with col1:
        st.metric("전체 문단 수", structure["total_paragraphs"])
        st.metric("내용이 있는 문단", structure["non_empty_paragraphs"])
        st.metric("빈 문단", structure["empty_paragraphs"])
    
    with col2:
        st.metric("표 개수", structure["table_count"])
        st.metric("추정 이미지 개수", structure["estimated_image_count"])
    
    # 문단 비율 차트
    if structure["total_paragraphs"] > 0:
        empty_ratio = structure["empty_paragraphs"] / structure["total_paragraphs"] * 100
        st.write(f"빈 문단 비율: {empty_ratio:.1f}%")
        
        if empty_ratio > 30:
            st.warning("빈 문단이 너무 많습니다. 문서 정리를 권장합니다.")

def display_statistics(statistics):
    """문서 통계를 표시합니다."""
    st.subheader("📈 문서 통계")
    
    col1, col2 = st.columns(2)
    
    with col1:
        st.metric("한국어 글자 수", f"{statistics['korean_characters']:,}")
        st.metric("영어 단어 수", f"{statistics['english_words']:,}")
        st.metric("총 문자 수", f"{statistics['total_characters']:,}")
    
    with col2:
        st.metric("문장 수", f"{statistics['sentences']:,}")
        st.metric("예상 읽기 시간", f"{statistics['estimated_reading_time_minutes']}분")
    
    # 문서 구성 비율
    if statistics['total_characters'] > 0:
        korean_ratio = statistics['korean_characters'] / statistics['total_characters'] * 100
        st.write(f"한국어 비율: {korean_ratio:.1f}%")

if __name__ == "__main__":
    main()