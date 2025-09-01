"""
파일 업로드 UI 컴포넌트

Streamlit file_uploader를 활용한 파일 업로드 인터페이스,
파일 검증, 업로드 상태 표시 및 검사 시작 버튼 활성화 기능을 제공합니다.
"""

import streamlit as st
from typing import Optional, Tuple, Any
import logging
from pathlib import Path
import mimetypes

from services.api_client import APIClient, APIClientError
from components.error_display import display_api_error, ValidationErrorHandler
from components.loading_display import show_file_upload_loading
from components.error_recovery import handle_api_error_with_recovery

logger = logging.getLogger(__name__)

class FileUploadComponent:
    """파일 업로드 UI 컴포넌트 클래스"""
    
    # 지원하는 파일 형식
    SUPPORTED_EXTENSIONS = ['.docx']
    SUPPORTED_MIME_TYPES = [
        'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
    ]
    
    # 파일 크기 제한 (10MB)
    MAX_FILE_SIZE = 10 * 1024 * 1024
    
    def __init__(self, api_client: APIClient):
        """
        파일 업로드 컴포넌트를 초기화합니다.
        
        Args:
            api_client: 백엔드 API 클라이언트
        """
        self.api_client = api_client
    
    def validate_file(self, uploaded_file) -> Tuple[bool, Optional[str]]:
        """
        업로드된 파일을 검증합니다.
        
        Args:
            uploaded_file: Streamlit UploadedFile 객체
            
        Returns:
            (검증 성공 여부, 오류 메시지)
        """
        if not uploaded_file:
            return False, "파일이 선택되지 않았습니다."
        
        # 파일 확장자 검증
        file_extension = Path(uploaded_file.name).suffix.lower()
        if file_extension not in self.SUPPORTED_EXTENSIONS:
            return False, f"지원하지 않는 파일 형식입니다. 지원 형식: {', '.join(self.SUPPORTED_EXTENSIONS)}"
        
        # 파일 크기 검증
        if uploaded_file.size > self.MAX_FILE_SIZE:
            size_mb = uploaded_file.size / (1024 * 1024)
            max_size_mb = self.MAX_FILE_SIZE / (1024 * 1024)
            return False, f"파일 크기가 너무 큽니다. (현재: {size_mb:.1f}MB, 최대: {max_size_mb}MB)"
        
        # MIME 타입 검증 (선택적)
        if hasattr(uploaded_file, 'type') and uploaded_file.type:
            if uploaded_file.type not in self.SUPPORTED_MIME_TYPES:
                logger.warning(f"예상과 다른 MIME 타입: {uploaded_file.type}")
        
        return True, None
    
    def render_file_uploader(self) -> Optional[st.runtime.uploaded_file_manager.UploadedFile]:
        """
        파일 업로드 인터페이스를 렌더링합니다.
        
        Returns:
            업로드된 파일 객체 (없으면 None)
        """
        st.subheader("📁 파일 선택")
        
        # 파일 업로드 위젯
        uploaded_file = st.file_uploader(
            label="검사할 한국어 문서를 선택하세요",
            type=['docx'],
            help=f"지원 형식: .docx | 최대 크기: {self.MAX_FILE_SIZE // (1024*1024)}MB",
            accept_multiple_files=False,
            key="document_uploader"
        )
        
        return uploaded_file
    
    def render_file_info(self, uploaded_file) -> None:
        """
        업로드된 파일 정보를 표시합니다.
        
        Args:
            uploaded_file: 업로드된 파일 객체
        """
        if not uploaded_file:
            return
        
        st.subheader("📋 파일 정보")
        
        col1, col2 = st.columns(2)
        
        with col1:
            st.write("**파일명:**", uploaded_file.name)
            st.write("**파일 크기:**", f"{uploaded_file.size:,} bytes ({uploaded_file.size / (1024*1024):.2f} MB)")
        
        with col2:
            if hasattr(uploaded_file, 'type') and uploaded_file.type:
                st.write("**파일 형식:**", uploaded_file.type)
            
            file_extension = Path(uploaded_file.name).suffix.lower()
            st.write("**확장자:**", file_extension)
    
    def render_validation_status(self, is_valid: bool, error_message: Optional[str]) -> None:
        """
        파일 검증 상태를 표시합니다.
        
        Args:
            is_valid: 검증 성공 여부
            error_message: 오류 메시지 (있는 경우)
        """
        if is_valid:
            st.success("✅ 파일 검증 완료! 업로드할 수 있습니다.")
        else:
            st.error(f"❌ 파일 검증 실패: {error_message}")
    
    def render_upload_button(self, uploaded_file, is_valid: bool) -> bool:
        """
        업로드 버튼을 렌더링하고 클릭 여부를 반환합니다.
        
        Args:
            uploaded_file: 업로드된 파일 객체
            is_valid: 파일 검증 성공 여부
            
        Returns:
            업로드 버튼 클릭 여부
        """
        if not uploaded_file or not is_valid:
            st.button("📤 파일 업로드", disabled=True, help="유효한 파일을 선택해주세요")
            return False
        
        return st.button(
            "📤 파일 업로드",
            type="primary",
            help="파일을 서버에 업로드하고 검사를 준비합니다"
        )
    
    def upload_file_to_server(self, uploaded_file) -> Tuple[bool, Optional[dict], Optional[str]]:
        """
        파일을 서버에 업로드합니다.
        
        Args:
            uploaded_file: 업로드할 파일 객체
            
        Returns:
            (성공 여부, 업로드 결과, 오류 메시지)
        """
        try:
            # 파일 내용을 바이트로 읽기
            file_content = uploaded_file.getvalue()
            
            # 서버에 업로드
            result = self.api_client.upload_file(file_content, uploaded_file.name)
            
            logger.info(f"파일 업로드 성공: {uploaded_file.name} -> {result.get('file_id')}")
            return True, result, None
            
        except APIClientError as e:
            logger.error(f"파일 업로드 실패: {str(e)}")
            
            # 에러 복구 시스템 사용
            def retry_upload():
                try:
                    result = self.api_client.upload_file(file_content, uploaded_file.name)
                    return True, result, None
                except Exception:
                    return False, None, None
            
            recovery_success = handle_api_error_with_recovery(
                error=e,
                operation_callback=retry_upload,
                reset_callback=None
            )
            
            if recovery_success:
                # 복구 성공 시 다시 업로드 시도
                try:
                    result = self.api_client.upload_file(file_content, uploaded_file.name)
                    return True, result, None
                except:
                    pass
            
            return False, None, {
                "error_code": e.error_code,
                "message": e.message,
                "details": e.details,
                "status_code": e.status_code
            }
        except Exception as e:
            logger.error(f"예상치 못한 오류: {str(e)}")
            return False, None, {
                "error_code": "UPLOAD_ERROR",
                "message": f"파일 업로드 중 오류가 발생했습니다: {str(e)}",
                "details": {"exception_type": type(e).__name__}
            }
    
    def render_upload_progress(self) -> None:
        """업로드 진행 상태를 표시합니다."""
        progress_bar = st.progress(0)
        status_text = st.empty()
        
        # 간단한 진행 상태 표시
        status_text.text("파일을 서버로 업로드하는 중...")
        progress_bar.progress(50)
        
        # 실제 업로드는 upload_file_to_server에서 처리됨
    
    def render_upload_result(self, success: bool, result: Optional[dict], error_info: Optional[Any]) -> None:
        """
        업로드 결과를 표시합니다.
        
        Args:
            success: 업로드 성공 여부
            result: 업로드 결과 데이터
            error_info: 오류 정보 (실패 시)
        """
        if success and result:
            st.success("🎉 파일 업로드가 완료되었습니다!")
            
            # 업로드 결과 정보 표시
            with st.expander("업로드 세부 정보", expanded=False):
                st.write("**파일 ID:**", result.get('file_id', 'N/A'))
                st.write("**파일명:**", result.get('filename', 'N/A'))
                st.write("**파일 크기:**", f"{result.get('size', 0):,} bytes")
                if 'upload_time' in result:
                    st.write("**업로드 시간:**", result['upload_time'])
        else:
            # 에러 정보가 딕셔너리인 경우 (새로운 형식)
            if isinstance(error_info, dict):
                display_api_error(error_info, retry_callback=lambda: st.rerun())
            else:
                # 기존 문자열 에러 메시지 처리
                st.error(f"❌ 파일 업로드 실패: {error_info}")
    
    def render_check_button(self, file_id: Optional[str]) -> bool:
        """
        문서 검사 시작 버튼을 렌더링합니다.
        
        Args:
            file_id: 업로드된 파일의 ID
            
        Returns:
            검사 시작 버튼 클릭 여부
        """
        if not file_id:
            st.button("🔍 문서 검사 시작", disabled=True, help="먼저 파일을 업로드해주세요")
            return False
        
        # 검사 시작 버튼을 더 눈에 띄게 표시
        st.markdown("### 📋 문서 검사")
        st.info("업로드된 문서에 대해 종합적인 품질 검사를 시작할 준비가 되었습니다.")
        
        return st.button(
            "🔍 문서 검사 시작",
            type="primary",
            help="업로드된 문서에 대해 종합적인 품질 검사를 시작합니다",
            use_container_width=True
        )

def render_file_upload_component(api_client: APIClient) -> Tuple[Optional[str], Optional[str]]:
    """
    파일 업로드 컴포넌트를 렌더링하는 편의 함수
    
    Args:
        api_client: 백엔드 API 클라이언트
        
    Returns:
        (업로드된 파일 ID, 파일명)
    """
    component = FileUploadComponent(api_client)
    
    # 파일 업로드 인터페이스
    uploaded_file = component.render_file_uploader()
    
    if uploaded_file:
        # 파일 정보 표시
        component.render_file_info(uploaded_file)
        
        # 파일 검증
        is_valid, error_message = component.validate_file(uploaded_file)
        component.render_validation_status(is_valid, error_message)
        
        # 업로드 버튼
        if component.render_upload_button(uploaded_file, is_valid):
            # 업로드 진행 상태 표시
            with st.spinner("파일을 업로드하는 중입니다..."):
                success, result, upload_error = component.upload_file_to_server(uploaded_file)
            
            # 업로드 결과 표시
            component.render_upload_result(success, result, upload_error)
            
            if success and result:
                file_id = result.get('file_id')
                filename = result.get('filename')
                
                # 세션 상태 업데이트
                st.session_state.uploaded_file_id = file_id
                st.session_state.uploaded_filename = filename
                
                st.divider()
                
                # 검사 시작 버튼
                if component.render_check_button(file_id):
                    return file_id, filename
    
    # 이미 업로드된 파일이 있는 경우 검사 버튼만 표시
    elif st.session_state.get('uploaded_file_id'):
        st.success(f"✅ 업로드 완료: {st.session_state.get('uploaded_filename', 'Unknown')}")
        
        st.divider()
        
        col1, col2 = st.columns([3, 1])
        
        with col1:
            if component.render_check_button(st.session_state.uploaded_file_id):
                return st.session_state.uploaded_file_id, st.session_state.uploaded_filename
        
        with col2:
            if st.button("🗑️ 파일 제거", help="업로드된 파일을 제거하고 새 파일을 선택합니다"):
                st.session_state.uploaded_file_id = None
                st.session_state.uploaded_filename = None
                st.rerun()
    
    return None, None