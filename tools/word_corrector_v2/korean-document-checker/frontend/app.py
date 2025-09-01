"""
한국어 문서 검사 AI 서비스 - Streamlit 프론트엔드

메인 애플리케이션 진입점으로 페이지 레이아웃, UI 컴포넌트 설정,
상태 관리 및 세션 처리를 담당합니다.
"""

import streamlit as st
import asyncio
import time
from typing import Optional, Dict, Any
import logging

from services.api_client import APIClient, APIClientError
from components.file_upload import render_file_upload_component
from components.error_display import (
    ErrorDisplayComponent, 
    NetworkErrorHandler, 
    display_api_error,
    display_success_message
)
from components.loading_display import (
    LoadingDisplayComponent,
    DocumentCheckProgressDisplay,
    InteractiveLoadingDisplay,
    show_document_check_loading
)
from components.realtime_progress import (
    track_document_check_progress,
    notification_manager
)
from components.error_recovery import (
    handle_api_error_with_recovery,
    show_error_recovery_panel,
    error_recovery_manager
)

# 로깅 설정
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# 페이지 설정
st.set_page_config(
    page_title="한국어 문서 검사 AI 서비스",
    page_icon="📝",
    layout="wide",
    initial_sidebar_state="collapsed"
)

# 세션 상태 초기화
def initialize_session_state():
    """세션 상태 변수들을 초기화합니다."""
    if 'uploaded_file_id' not in st.session_state:
        st.session_state.uploaded_file_id = None
    
    if 'uploaded_filename' not in st.session_state:
        st.session_state.uploaded_filename = None
    
    if 'check_results' not in st.session_state:
        st.session_state.check_results = None
    
    if 'current_step' not in st.session_state:
        st.session_state.current_step = 'upload'  # 'upload', 'checking', 'results'
    
    if 'error_message' not in st.session_state:
        st.session_state.error_message = None
    
    if 'backend_url' not in st.session_state:
        st.session_state.backend_url = "http://localhost:8000"

def reset_session():
    """세션을 초기 상태로 리셋합니다."""
    st.session_state.uploaded_file_id = None
    st.session_state.uploaded_filename = None
    st.session_state.check_results = None
    st.session_state.current_step = 'upload'
    st.session_state.error_message = None

def render_header():
    """애플리케이션 헤더를 렌더링합니다."""
    st.title("📝 한국어 문서 검사 AI 서비스")
    st.markdown("""
    Azure OpenAI GPT-4.1을 활용한 종합적인 한국어 문서 품질 검사 서비스입니다.
    
    **지원 기능:**
    - 🔍 구문 검사 (문법 오류 검출)
    - ✏️ 맞춤법 검사 (한국어/영어)
    - 📋 문서 일관성 검사 (레이아웃, 용어 통일)
    - 📊 종합 품질 보고서
    """)
    st.divider()

def render_progress_indicator():
    """현재 진행 단계를 표시합니다."""
    steps = {
        'upload': '1️⃣ 파일 업로드',
        'checking': '2️⃣ 문서 검사 중',
        'results': '3️⃣ 결과 확인'
    }
    
    current_step = st.session_state.current_step
    
    cols = st.columns(3)
    for i, (step_key, step_name) in enumerate(steps.items()):
        with cols[i]:
            if step_key == current_step:
                st.markdown(f"**{step_name}** ✅")
            elif (step_key == 'checking' and current_step == 'results') or \
                 (step_key == 'upload' and current_step in ['checking', 'results']):
                st.markdown(f"{step_name} ✅")
            else:
                st.markdown(f"{step_name}")

def render_error_message():
    """에러 메시지를 표시합니다."""
    if st.session_state.error_message:
        st.error(st.session_state.error_message)
        if st.button("오류 해결 후 다시 시도"):
            st.session_state.error_message = None
            st.rerun()

def render_sidebar():
    """사이드바를 렌더링합니다."""
    with st.sidebar:
        st.header("⚙️ 설정")
        
        # 백엔드 URL 설정
        backend_url = st.text_input(
            "백엔드 서버 URL",
            value=st.session_state.backend_url,
            help="FastAPI 백엔드 서버의 URL을 입력하세요"
        )
        
        if backend_url != st.session_state.backend_url:
            st.session_state.backend_url = backend_url
        
        st.divider()
        
        # 세션 정보
        st.header("📊 세션 정보")
        st.write(f"**현재 단계:** {st.session_state.current_step}")
        
        if st.session_state.uploaded_filename:
            st.write(f"**업로드된 파일:** {st.session_state.uploaded_filename}")
        
        if st.session_state.uploaded_file_id:
            st.write(f"**파일 ID:** {st.session_state.uploaded_file_id[:8]}...")
        
        st.divider()
        
        # 세션 리셋 버튼
        if st.button("🔄 새로 시작", type="secondary"):
            reset_session()
            st.rerun()
        
        # 테스트 모드
        st.header("🧪 테스트 모드")
        st.markdown("백엔드 없이 프론트엔드 테스트")
        
        if st.button("📊 샘플 결과 보기", help="샘플 검사 결과로 UI 테스트"):
            from components.sample_data import generate_sample_check_results
            st.session_state.check_results = generate_sample_check_results("테스트_문서.docx")
            st.session_state.current_step = 'results'
            st.session_state.uploaded_filename = "테스트_문서.docx"
            st.rerun()
        
        col1, col2 = st.columns(2)
        with col1:
            if st.button("✅ 완벽한 문서", help="문제없는 문서 결과"):
                from components.sample_data import generate_perfect_document_result
                st.session_state.check_results = generate_perfect_document_result("완벽한_문서.docx")
                st.session_state.current_step = 'results'
                st.session_state.uploaded_filename = "완벽한_문서.docx"
                st.rerun()
        
        with col2:
            if st.button("❌ 문제 많은 문서", help="문제가 많은 문서 결과"):
                from components.sample_data import generate_problematic_document_result
                st.session_state.check_results = generate_problematic_document_result("문제_문서.docx")
                st.session_state.current_step = 'results'
                st.session_state.uploaded_filename = "문제_문서.docx"
                st.rerun()
        
        st.divider()
        
        # 에러 복구 패널
        show_error_recovery_panel()
        
        # 도움말
        st.header("❓ 도움말")
        with st.expander("지원 파일 형식"):
            st.write("- Microsoft Word 문서 (.docx)")
            st.write("- 최대 파일 크기: 10MB")
        
        with st.expander("검사 항목"):
            st.write("- **구문 검사**: 한국어 문법 오류")
            st.write("- **맞춤법 검사**: 한국어/영어 철자")
            st.write("- **일관성 검사**: 용어, 레이아웃 통일")

def main():
    """메인 애플리케이션 함수"""
    try:
        # 세션 상태 초기화
        initialize_session_state()
        
        # 헤더 렌더링
        render_header()
        
        # 진행 상태 표시
        render_progress_indicator()
        
        # 에러 메시지 표시
        render_error_message()
        
        # 사이드바 렌더링
        render_sidebar()
        
        # 메인 컨텐츠 영역
        main_container = st.container()
        
        with main_container:
            # 현재 단계에 따른 컴포넌트 렌더링
            if st.session_state.current_step == 'upload':
                st.header("📁 문서 업로드")
                
                # 업로드된 파일이 있는지 확인
                if st.session_state.get('uploaded_file_id'):
                    st.success("✅ 파일 업로드가 완료되었습니다!")
                else:
                    st.info("검사할 한국어 .docx 파일을 업로드해주세요.")
                
                # API 클라이언트 생성
                try:
                    api_client = APIClient(st.session_state.backend_url)
                    
                    # 백엔드 연결 상태 확인 (선택적)
                    if st.checkbox("백엔드 서버 연결 확인", value=True, help="체크 해제 시 연결 확인을 건너뜁니다"):
                        with st.spinner("백엔드 서버 연결 확인 중..."):
                            try:
                                health_status = api_client.check_health()
                                display_success_message("백엔드 서버 연결 성공", health_status)
                            except APIClientError as e:
                                NetworkErrorHandler.display_connection_error(st.session_state.backend_url)
                                st.info("사이드바에서 백엔드 서버 URL을 확인하거나, 위 체크박스를 해제하고 계속 진행할 수 있습니다.")
                    else:
                        st.info("ℹ️ 백엔드 서버 연결 확인을 건너뜁니다.")
                    
                    # 파일 업로드 컴포넌트 렌더링
                    file_id, filename = render_file_upload_component(api_client)
                    
                    # 검사 시작 버튼이 클릭된 경우
                    if file_id and filename:
                        st.session_state.current_step = 'checking'
                        st.session_state.uploaded_file_id = file_id
                        st.session_state.uploaded_filename = filename
                        # check_started 상태 초기화
                        if 'check_started' in st.session_state:
                            del st.session_state.check_started
                        st.rerun()
                        
                except Exception as e:
                    logger.error(f"파일 업로드 컴포넌트 오류: {str(e)}")
                    st.error(f"파일 업로드 컴포넌트 오류: {str(e)}")
                
            elif st.session_state.current_step == 'checking':
                st.header("🔍 문서 검사 중")
                
                try:
                    # API 클라이언트 생성
                    api_client = APIClient(st.session_state.backend_url)
                    
                    # 검사 시작 알림
                    notification_manager.add_notification("문서 검사를 시작합니다...", "info")
                    
                    # 실시간 진행률 추적을 사용한 문서 검사
                    def on_check_complete(progress_data):
                        """검사 완료 시 콜백"""
                        notification_manager.add_notification("문서 검사가 완료되었습니다!", "success")
                        st.session_state.current_step = 'results'
                        st.rerun()
                    
                    def on_check_error(error_data):
                        """검사 오류 시 콜백"""
                        error_message = error_data.get("message", "알 수 없는 오류가 발생했습니다")
                        notification_manager.add_notification(f"검사 중 오류 발생: {error_message}", "error")
                        st.session_state.error_message = error_message
                    
                    # 백그라운드에서 검사 시작
                    if 'check_started' not in st.session_state:
                        st.session_state.check_started = True
                        
                        # 백엔드 연결 확인
                        try:
                            # 백엔드 연결 테스트
                            health_status = api_client.check_health()
                            
                            # 검사 시작 표시
                            with st.spinner("문서 검사를 시작하고 있습니다..."):
                                # 실제 API 호출
                                check_results = api_client.check_document(st.session_state.uploaded_file_id)
                                st.session_state.check_results = check_results
                                st.session_state.current_step = 'results'
                                st.rerun()
                                
                        except APIClientError as e:
                            # 백엔드 연결 실패 시 샘플 데이터 사용
                            logger.warning(f"백엔드 연결 실패, 샘플 데이터 사용: {str(e)}")
                            st.warning("⚠️ 백엔드 서버에 연결할 수 없어 샘플 데이터를 표시합니다.")
                            
                            from components.sample_data import generate_sample_check_results
                            st.session_state.check_results = generate_sample_check_results(st.session_state.uploaded_filename)
                            st.session_state.current_step = 'results'
                            st.rerun()
                            
                        except Exception as e:
                            logger.error(f"문서 검사 시작 실패: {str(e)}")
                            
                            # 에러 복구 시스템 사용
                            def retry_check():
                                if 'check_started' in st.session_state:
                                    del st.session_state.check_started
                                st.rerun()
                                return True
                            
                            recovery_success = handle_api_error_with_recovery(
                                error=e,
                                operation_callback=retry_check,
                                reset_callback=reset_session
                            )
                            
                            if not recovery_success:
                                # 복구 실패 시 추가 옵션 제공
                                col1, col2 = st.columns(2)
                                with col1:
                                    if st.button("📁 새 파일 업로드"):
                                        reset_session()
                                        st.rerun()
                                with col2:
                                    if st.button("⚙️ 설정 확인"):
                                        st.sidebar.info("👈 사이드바에서 백엔드 URL을 확인하세요")
                            return
                    
                    # 실시간 진행률 추적
                    progress_result = track_document_check_progress(
                        api_client=api_client,
                        file_id=st.session_state.uploaded_file_id,
                        on_complete=on_check_complete,
                        on_error=on_check_error
                    )
                    
                    # 진행률 추적이 완료되면 결과 확인
                    if progress_result and progress_result.get("status") == "completed":
                        # 검사 결과가 아직 세션에 없다면 다시 조회
                        if not st.session_state.check_results:
                            try:
                                # 검사가 완료되었으므로 결과를 다시 요청하지 않고 결과 페이지로 이동
                                st.session_state.current_step = 'results'
                                st.rerun()
                            except Exception as e:
                                logger.error(f"검사 결과 조회 실패: {str(e)}")
                                st.error("검사는 완료되었지만 결과를 가져오는데 실패했습니다.")
                        else:
                            st.session_state.current_step = 'results'
                            st.rerun()
                    
                    elif progress_result is None:
                        # 진행률 추적 실패 또는 취소
                        st.warning("작업이 취소되었거나 진행률을 추적할 수 없습니다.")
                        
                        col1, col2 = st.columns(2)
                        with col1:
                            if st.button("🔄 다시 시도"):
                                if 'check_started' in st.session_state:
                                    del st.session_state.check_started
                                st.rerun()
                        with col2:
                            if st.button("📁 새 파일 업로드"):
                                reset_session()
                                st.rerun()
                    
                    # 알림 표시
                    notification_manager.show_notifications()
                        
                except Exception as e:
                    logger.error(f"검사 프로세스 오류: {str(e)}")
                    
                    # 예상치 못한 오류 표시
                    error_handler = ErrorDisplayComponent()
                    error_handler.display_error(
                        error_code="UNEXPECTED_ERROR",
                        message=f"예상치 못한 오류가 발생했습니다: {str(e)}",
                        details={"exception_type": type(e).__name__},
                        retry_callback=lambda: st.rerun()
                    )
                
            elif st.session_state.current_step == 'results':
                # 검사 결과 표시
                if st.session_state.check_results:
                    # 결과 표시 컴포넌트 import 및 렌더링
                    from components.result_display import render_check_results
                    
                    try:
                        render_check_results(st.session_state.check_results)
                    except Exception as e:
                        logger.error(f"결과 표시 오류: {str(e)}")
                        st.error(f"결과를 표시하는 중 오류가 발생했습니다: {str(e)}")
                        
                        # 원시 데이터 표시 (디버깅용)
                        with st.expander("원시 검사 결과 데이터 (디버깅용)", expanded=False):
                            st.json(st.session_state.check_results)
                else:
                    st.warning("검사 결과를 불러올 수 없습니다.")
                    
                    # 다시 검사하기 버튼
                    if st.button("🔄 다시 검사하기"):
                        if st.session_state.uploaded_file_id:
                            st.session_state.current_step = 'checking'
                            st.rerun()
                        else:
                            st.session_state.current_step = 'upload'
                            st.rerun()
        
        # 푸터
        st.divider()
        st.markdown("""
        <div style='text-align: center; color: #666; font-size: 0.8em;'>
        한국어 문서 검사 AI 서비스 | Powered by Azure OpenAI GPT-4.1
        </div>
        """, unsafe_allow_html=True)
        
    except Exception as e:
        logger.error(f"애플리케이션 오류: {str(e)}")
        st.error(f"애플리케이션 오류가 발생했습니다: {str(e)}")
        
        if st.button("애플리케이션 재시작"):
            reset_session()
            st.rerun()

if __name__ == "__main__":
    main()