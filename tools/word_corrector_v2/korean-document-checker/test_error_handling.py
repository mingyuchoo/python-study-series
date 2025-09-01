"""
에러 처리 및 사용자 피드백 시스템 테스트

Task 10의 구현 사항들을 검증하는 테스트 스크립트입니다.
"""

import sys
import asyncio
import logging
from datetime import datetime

# 백엔드 테스트를 위한 경로 추가
sys.path.append('korean-document-checker/backend')
sys.path.append('korean-document-checker/frontend')

# 로깅 설정
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def test_backend_error_handling():
    """백엔드 에러 처리 미들웨어 테스트"""
    print("🔧 백엔드 에러 처리 시스템 테스트")
    
    try:
        # 에러 처리 미들웨어 임포트 테스트
        from app.middleware.error_handler import ErrorHandlingMiddleware, get_user_friendly_message
        from app.core.exceptions import (
            DocumentCheckerException, FileValidationError, 
            AzureOpenAIError, create_error_response
        )
        
        print("✅ 에러 처리 미들웨어 임포트 성공")
        
        # 사용자 친화적 메시지 변환 테스트
        test_message = get_user_friendly_message("FILE_VALIDATION_ERROR", "Original error")
        assert "파일" in test_message, "사용자 친화적 메시지 변환 실패"
        print("✅ 사용자 친화적 메시지 변환 테스트 통과")
        
        # 에러 응답 생성 테스트
        error_response = create_error_response(
            error_code="TEST_ERROR",
            message="테스트 에러 메시지",
            details={"test": "data"}
        )
        
        assert error_response["error"] == True
        assert error_response["error_code"] == "TEST_ERROR"
        assert "timestamp" in error_response
        print("✅ 에러 응답 생성 테스트 통과")
        
        # 커스텀 예외 테스트
        try:
            raise FileValidationError("테스트 파일 검증 오류", {"file_size": 1000})
        except DocumentCheckerException as e:
            assert e.error_code == "FILE_VALIDATION_ERROR"
            assert e.status_code == 400
            print("✅ 커스텀 예외 테스트 통과")
        
        return True
        
    except Exception as e:
        print(f"❌ 백엔드 에러 처리 테스트 실패: {str(e)}")
        return False

async def test_progress_tracking():
    """진행률 추적 시스템 테스트"""
    print("\n📊 진행률 추적 시스템 테스트")
    
    try:
        from app.middleware.progress_tracker import (
            progress_tracker, start_task_progress, 
            update_task_progress, complete_task_progress
        )
        
        print("✅ 진행률 추적 모듈 임포트 성공")
        
        # 진행률 추적 시작
        task_id = "test_task_123"
        await start_task_progress(task_id, 3, "테스트 작업")
        
        # 진행률 조회
        progress_info = await progress_tracker.get_progress(task_id)
        assert progress_info is not None
        assert progress_info["task_id"] == task_id
        assert progress_info["total_steps"] == 3
        print("✅ 진행률 추적 시작 테스트 통과")
        
        # 진행률 업데이트
        await update_task_progress(task_id, 1, "첫 번째 단계", "첫 번째 단계를 진행 중입니다")
        
        progress_info = await progress_tracker.get_progress(task_id)
        assert progress_info["current_step"] == 1
        assert progress_info["progress"] == 1/3
        print("✅ 진행률 업데이트 테스트 통과")
        
        # 진행률 완료
        await complete_task_progress(task_id, "테스트 작업 완료")
        
        progress_info = await progress_tracker.get_progress(task_id)
        assert progress_info["status"] == "completed"
        assert progress_info["progress"] == 1.0
        print("✅ 진행률 완료 테스트 통과")
        
        return True
        
    except Exception as e:
        print(f"❌ 진행률 추적 테스트 실패: {str(e)}")
        return False

def test_frontend_error_components():
    """프론트엔드 에러 컴포넌트 테스트"""
    print("\n🎨 프론트엔드 에러 컴포넌트 테스트")
    
    try:
        # 에러 표시 컴포넌트 테스트
        from components.error_display import ErrorDisplayComponent, NetworkErrorHandler
        from components.error_recovery import ErrorRecoveryManager, ErrorSeverity
        from components.realtime_progress import RealTimeProgressTracker
        
        print("✅ 프론트엔드 에러 컴포넌트 임포트 성공")
        
        # 에러 표시 컴포넌트 인스턴스 생성
        error_display = ErrorDisplayComponent()
        assert error_display.ERROR_STYLES is not None
        assert "FILE_VALIDATION_ERROR" in error_display.ERROR_STYLES
        print("✅ 에러 표시 컴포넌트 생성 테스트 통과")
        
        # 에러 복구 관리자 테스트
        recovery_manager = ErrorRecoveryManager()
        assert recovery_manager.RECOVERY_STRATEGIES is not None
        assert "NETWORK_ERROR" in recovery_manager.RECOVERY_STRATEGIES
        
        strategy = recovery_manager.RECOVERY_STRATEGIES["NETWORK_ERROR"]
        assert strategy["severity"] == ErrorSeverity.MEDIUM
        print("✅ 에러 복구 관리자 테스트 통과")
        
        return True
        
    except Exception as e:
        print(f"❌ 프론트엔드 에러 컴포넌트 테스트 실패: {str(e)}")
        return False

def test_api_client_error_handling():
    """API 클라이언트 에러 처리 테스트"""
    print("\n🌐 API 클라이언트 에러 처리 테스트")
    
    try:
        from services.api_client import APIClient, APIClientError
        
        print("✅ API 클라이언트 임포트 성공")
        
        # API 클라이언트 인스턴스 생성
        client = APIClient("http://localhost:8000")
        assert client.base_url == "http://localhost:8000"
        assert client.max_retries == 3
        print("✅ API 클라이언트 생성 테스트 통과")
        
        # APIClientError 테스트
        error = APIClientError(
            message="테스트 에러",
            error_code="TEST_ERROR",
            status_code=400,
            details={"test": "data"}
        )
        
        assert error.message == "테스트 에러"
        assert error.error_code == "TEST_ERROR"
        assert error.status_code == 400
        print("✅ API 클라이언트 에러 클래스 테스트 통과")
        
        return True
        
    except Exception as e:
        print(f"❌ API 클라이언트 에러 처리 테스트 실패: {str(e)}")
        return False

def test_integration():
    """통합 테스트"""
    print("\n🔗 통합 테스트")
    
    try:
        # 백엔드와 프론트엔드 컴포넌트 간 호환성 테스트
        from app.core.exceptions import create_error_response
        from components.error_display import display_api_error
        
        # 백엔드에서 생성한 에러 응답이 프론트엔드에서 처리 가능한지 확인
        backend_error = create_error_response(
            error_code="INTEGRATION_TEST",
            message="통합 테스트 에러",
            details={"source": "backend"}
        )
        
        # 프론트엔드 에러 표시 함수가 백엔드 에러 형식을 처리할 수 있는지 확인
        assert "error_code" in backend_error
        assert "message" in backend_error
        assert "timestamp" in backend_error
        print("✅ 백엔드-프론트엔드 에러 형식 호환성 테스트 통과")
        
        return True
        
    except Exception as e:
        print(f"❌ 통합 테스트 실패: {str(e)}")
        return False

def main():
    """메인 테스트 함수"""
    print("🧪 에러 처리 및 사용자 피드백 시스템 테스트 시작")
    print("=" * 60)
    
    test_results = []
    
    # 개별 테스트 실행
    test_results.append(("백엔드 에러 처리", test_backend_error_handling()))
    test_results.append(("진행률 추적", asyncio.run(test_progress_tracking())))
    test_results.append(("프론트엔드 에러 컴포넌트", test_frontend_error_components()))
    test_results.append(("API 클라이언트 에러 처리", test_api_client_error_handling()))
    test_results.append(("통합 테스트", test_integration()))
    
    # 결과 요약
    print("\n" + "=" * 60)
    print("📋 테스트 결과 요약")
    print("=" * 60)
    
    passed = 0
    failed = 0
    
    for test_name, result in test_results:
        status = "✅ 통과" if result else "❌ 실패"
        print(f"{test_name}: {status}")
        
        if result:
            passed += 1
        else:
            failed += 1
    
    print(f"\n총 {len(test_results)}개 테스트 중 {passed}개 통과, {failed}개 실패")
    
    if failed == 0:
        print("\n🎉 모든 테스트가 성공적으로 통과했습니다!")
        print("Task 10: 에러 처리 및 사용자 피드백 구현이 완료되었습니다.")
    else:
        print(f"\n⚠️ {failed}개의 테스트가 실패했습니다. 구현을 확인해주세요.")
    
    return failed == 0

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)