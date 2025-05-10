import atexit
import threading
from typing import Any, Callable, Dict, Optional


def create_shutdown_handler(config: Dict[str, Any] = None) -> Dict[str, Any]:
    """함수형 스타일로 종료 처리 핸들러를 생성합니다.

    Args:
        config: 설정 옵션을 포함하는 딕셔너리 (선택적)

    Returns:
        종료 처리 함수들을 포함하는 딕셔너리
    """
    # 기본 설정 값 설정
    if config is None:
        config = {}

    exit_message = config.get("exit_message", "애플리케이션을 정상적으로 종료하는 중...")

    def cleanup_resources(cleanup_callback: Optional[Callable] = None) -> None:
        """애플리케이션 종료 시 자원을 정리하는 함수

        Args:
            cleanup_callback: 추가 정리 작업을 위한 콜백 함수
        """
        print(exit_message)

        # 추가 정리 콜백 실행
        if cleanup_callback:
            try:
                cleanup_callback()
            except Exception as e:
                print(f"정리 함수 실행 중 오류 발생: {e}")

        # 실행 중인 스레드 정리 시도
        try:
            for thread in threading.enumerate():
                if thread is not threading.current_thread() and thread.daemon is False:
                    if hasattr(thread, "_stop") and callable(getattr(thread, "_stop", None)):
                        try:
                            thread._stop()
                        except:
                            pass
        except Exception as e:
            print(f"스레드 정리 중 오류: {e}")

    def register_cleanup(cleanup_callback: Optional[Callable] = None) -> None:
        """종료 시 실행할 정리 함수를 등록합니다.

        Args:
            cleanup_callback: 종료 시 실행할 정리 함수
        """

        # 클로저를 사용하여 cleanup_callback을 캡처
        def cleanup_wrapper():
            cleanup_resources(cleanup_callback)

        atexit.register(cleanup_wrapper)

    # 함수형 API 반환
    return {"cleanup_resources": cleanup_resources, "register_cleanup": register_cleanup}
