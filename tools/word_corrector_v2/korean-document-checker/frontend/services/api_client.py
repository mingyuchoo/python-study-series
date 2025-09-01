"""
백엔드 API와 통신하는 클라이언트 클래스

FastAPI 백엔드 서버와의 HTTP 통신을 담당하며,
파일 업로드, 문서 검사 요청, 에러 처리 및 재시도 로직을 포함합니다.
"""

import asyncio
import aiohttp
import requests
from typing import Optional, Dict, Any, BinaryIO
import logging
import time
from pathlib import Path

logger = logging.getLogger(__name__)

class APIClientError(Exception):
    """API 클라이언트 관련 예외"""
    
    def __init__(
        self,
        message: str,
        error_code: str = "API_ERROR",
        status_code: Optional[int] = None,
        details: Optional[Dict[str, Any]] = None
    ):
        self.message = message
        self.error_code = error_code
        self.status_code = status_code
        self.details = details or {}
        super().__init__(self.message)

class APIClient:
    """백엔드 API와 통신하는 클라이언트 클래스"""
    
    def __init__(self, base_url: str = "http://localhost:8000"):
        """
        API 클라이언트를 초기화합니다.
        
        Args:
            base_url: 백엔드 서버의 기본 URL
        """
        self.base_url = base_url.rstrip('/')
        self.timeout = 30  # 기본 타임아웃 30초
        self.max_retries = 3  # 최대 재시도 횟수
        self.retry_delay = 1  # 재시도 간격 (초)
        
    def _get_headers(self) -> Dict[str, str]:
        """기본 HTTP 헤더를 반환합니다."""
        return {
            'User-Agent': 'Korean-Document-Checker-Frontend/1.0'
        }
    
    def _handle_response(self, response: requests.Response) -> Dict[str, Any]:
        """
        HTTP 응답을 처리하고 JSON 데이터를 반환합니다.
        
        Args:
            response: requests Response 객체
            
        Returns:
            응답 JSON 데이터
            
        Raises:
            APIClientError: API 오류 발생 시
        """
        try:
            response.raise_for_status()
            return response.json()
        except requests.exceptions.HTTPError as e:
            try:
                error_data = response.json()
                error_code = error_data.get('error_code', f'HTTP_{response.status_code}')
                error_message = error_data.get('message', str(e))
                error_details = error_data.get('details', {})
            except:
                error_code = f'HTTP_{response.status_code}'
                error_message = str(e)
                error_details = {}
            
            logger.error(f"HTTP 오류 {response.status_code}: {error_message}")
            raise APIClientError(
                message=error_message,
                error_code=error_code,
                status_code=response.status_code,
                details=error_details
            )
        except requests.exceptions.JSONDecodeError:
            logger.error("응답 JSON 파싱 실패")
            raise APIClientError(
                message="서버 응답을 해석할 수 없습니다",
                error_code="JSON_DECODE_ERROR",
                status_code=response.status_code if hasattr(response, 'status_code') else None
            )
    
    def _retry_request(self, func, *args, **kwargs) -> Any:
        """
        요청을 재시도 로직과 함께 실행합니다.
        
        Args:
            func: 실행할 함수
            *args: 함수 인자
            **kwargs: 함수 키워드 인자
            
        Returns:
            함수 실행 결과
            
        Raises:
            APIClientError: 모든 재시도 실패 시
        """
        last_exception = None
        
        for attempt in range(self.max_retries + 1):
            try:
                return func(*args, **kwargs)
            except (requests.exceptions.ConnectionError, 
                    requests.exceptions.Timeout,
                    requests.exceptions.RequestException) as e:
                last_exception = e
                
                if attempt < self.max_retries:
                    delay = self.retry_delay * (2 ** attempt)  # 지수 백오프
                    logger.warning(f"요청 실패 (시도 {attempt + 1}/{self.max_retries + 1}), "
                                 f"{delay}초 후 재시도: {str(e)}")
                    time.sleep(delay)
                else:
                    logger.error(f"모든 재시도 실패: {str(e)}")
        
        # 네트워크 오류 유형에 따른 구체적인 에러 코드 설정
        if isinstance(last_exception, requests.exceptions.ConnectionError):
            error_code = "NETWORK_ERROR"
            message = "서버에 연결할 수 없습니다. 네트워크 연결을 확인해주세요."
        elif isinstance(last_exception, requests.exceptions.Timeout):
            error_code = "TIMEOUT_ERROR"
            message = "요청 시간이 초과되었습니다. 잠시 후 다시 시도해주세요."
        else:
            error_code = "CONNECTION_ERROR"
            message = f"서버 연결 실패: {str(last_exception)}"
        
        raise APIClientError(
            message=message,
            error_code=error_code,
            details={"original_error": str(last_exception)}
        )
    
    def check_health(self) -> Dict[str, Any]:
        """
        백엔드 서버의 상태를 확인합니다.
        
        Returns:
            서버 상태 정보
            
        Raises:
            APIClientError: 서버 연결 실패 시
        """
        def _make_request():
            url = f"{self.base_url}/api/health"
            response = requests.get(
                url,
                headers=self._get_headers(),
                timeout=self.timeout
            )
            return self._handle_response(response)
        
        return self._retry_request(_make_request)
    
    def upload_file(self, file_content: bytes, filename: str) -> Dict[str, Any]:
        """
        파일을 백엔드 서버에 업로드합니다.
        
        Args:
            file_content: 업로드할 파일의 바이트 데이터
            filename: 파일명
            
        Returns:
            업로드 결과 (file_id, filename, size, upload_time 포함)
            
        Raises:
            APIClientError: 업로드 실패 시
        """
        def _make_request():
            url = f"{self.base_url}/api/upload"
            
            files = {
                'file': (filename, file_content, 'application/vnd.openxmlformats-officedocument.wordprocessingml.document')
            }
            
            response = requests.post(
                url,
                files=files,
                headers=self._get_headers(),
                timeout=self.timeout
            )
            return self._handle_response(response)
        
        logger.info(f"파일 업로드 시작: {filename} ({len(file_content)} bytes)")
        result = self._retry_request(_make_request)
        logger.info(f"파일 업로드 완료: {result.get('file_id')}")
        return result
    
    def check_document(self, file_id: str, check_types: Optional[list] = None) -> Dict[str, Any]:
        """
        업로드된 문서에 대해 검사를 요청합니다.
        
        Args:
            file_id: 업로드된 파일의 ID
            check_types: 수행할 검사 유형 목록 (기본값: ["all"])
            
        Returns:
            검사 결과
            
        Raises:
            APIClientError: 검사 요청 실패 시
        """
        if check_types is None:
            check_types = ["all"]
        
        def _make_request():
            url = f"{self.base_url}/api/check/{file_id}"
            
            data = {
                "check_types": check_types
            }
            
            # 작업 ID를 헤더에 추가
            task_id = f"check_{file_id}"
            headers = {
                **self._get_headers(), 
                'Content-Type': 'application/json',
                'X-Task-ID': task_id
            }
            
            response = requests.post(
                url,
                json=data,
                headers=headers,
                timeout=120  # 문서 검사는 더 긴 타임아웃
            )
            return self._handle_response(response)
        
        logger.info(f"문서 검사 시작: {file_id}, 검사 유형: {check_types}")
        result = self._retry_request(_make_request)
        logger.info(f"문서 검사 완료: {file_id}")
        return result
    
    def get_progress(self, task_id: str) -> Dict[str, Any]:
        """
        작업 진행률을 조회합니다.
        
        Args:
            task_id: 작업 ID
            
        Returns:
            진행률 정보
            
        Raises:
            APIClientError: 진행률 조회 실패 시
        """
        def _make_request():
            url = f"{self.base_url}/api/progress/{task_id}"
            response = requests.get(
                url,
                headers=self._get_headers(),
                timeout=10
            )
            return self._handle_response(response)
        
        return self._retry_request(_make_request)
    
    def cancel_task(self, task_id: str) -> Dict[str, Any]:
        """
        진행 중인 작업을 취소합니다.
        
        Args:
            task_id: 취소할 작업 ID
            
        Returns:
            취소 결과
            
        Raises:
            APIClientError: 작업 취소 실패 시
        """
        def _make_request():
            url = f"{self.base_url}/api/progress/{task_id}"
            response = requests.delete(
                url,
                headers=self._get_headers(),
                timeout=10
            )
            return self._handle_response(response)
        
        return self._retry_request(_make_request)

class AsyncAPIClient:
    """비동기 API 클라이언트 (향후 확장용)"""
    
    def __init__(self, base_url: str = "http://localhost:8000"):
        """
        비동기 API 클라이언트를 초기화합니다.
        
        Args:
            base_url: 백엔드 서버의 기본 URL
        """
        self.base_url = base_url.rstrip('/')
        self.timeout = aiohttp.ClientTimeout(total=30)
        self.max_retries = 3
        self.retry_delay = 1
    
    async def _handle_response(self, response: aiohttp.ClientResponse) -> Dict[str, Any]:
        """
        비동기 HTTP 응답을 처리합니다.
        
        Args:
            response: aiohttp ClientResponse 객체
            
        Returns:
            응답 JSON 데이터
            
        Raises:
            APIClientError: API 오류 발생 시
        """
        try:
            response.raise_for_status()
            return await response.json()
        except aiohttp.ClientResponseError as e:
            try:
                error_data = await response.json()
                error_message = error_data.get('message', str(e))
            except:
                error_message = str(e)
            
            logger.error(f"HTTP 오류 {response.status}: {error_message}")
            raise APIClientError(f"서버 오류 ({response.status}): {error_message}")
        except aiohttp.ContentTypeError:
            logger.error("응답 JSON 파싱 실패")
            raise APIClientError("서버 응답을 해석할 수 없습니다")
    
    async def upload_file_async(self, file_content: bytes, filename: str) -> Dict[str, Any]:
        """
        파일을 비동기적으로 업로드합니다.
        
        Args:
            file_content: 업로드할 파일의 바이트 데이터
            filename: 파일명
            
        Returns:
            업로드 결과
            
        Raises:
            APIClientError: 업로드 실패 시
        """
        url = f"{self.base_url}/api/upload"
        
        data = aiohttp.FormData()
        data.add_field('file', file_content, filename=filename, 
                      content_type='application/vnd.openxmlformats-officedocument.wordprocessingml.document')
        
        async with aiohttp.ClientSession(timeout=self.timeout) as session:
            for attempt in range(self.max_retries + 1):
                try:
                    async with session.post(url, data=data) as response:
                        return await self._handle_response(response)
                except (aiohttp.ClientError, asyncio.TimeoutError) as e:
                    if attempt < self.max_retries:
                        delay = self.retry_delay * (2 ** attempt)
                        logger.warning(f"업로드 실패 (시도 {attempt + 1}/{self.max_retries + 1}), "
                                     f"{delay}초 후 재시도: {str(e)}")
                        await asyncio.sleep(delay)
                    else:
                        logger.error(f"모든 재시도 실패: {str(e)}")
                        raise APIClientError(f"파일 업로드 실패: {str(e)}")
    
    async def check_document_async(self, file_id: str, check_types: Optional[list] = None) -> Dict[str, Any]:
        """
        문서를 비동기적으로 검사합니다.
        
        Args:
            file_id: 업로드된 파일의 ID
            check_types: 수행할 검사 유형 목록
            
        Returns:
            검사 결과
            
        Raises:
            APIClientError: 검사 요청 실패 시
        """
        if check_types is None:
            check_types = ["all"]
        
        url = f"{self.base_url}/api/check/{file_id}"
        data = {"check_types": check_types}
        
        timeout = aiohttp.ClientTimeout(total=60)  # 문서 검사용 긴 타임아웃
        
        async with aiohttp.ClientSession(timeout=timeout) as session:
            for attempt in range(self.max_retries + 1):
                try:
                    async with session.post(url, json=data) as response:
                        return await self._handle_response(response)
                except (aiohttp.ClientError, asyncio.TimeoutError) as e:
                    if attempt < self.max_retries:
                        delay = self.retry_delay * (2 ** attempt)
                        logger.warning(f"검사 요청 실패 (시도 {attempt + 1}/{self.max_retries + 1}), "
                                     f"{delay}초 후 재시도: {str(e)}")
                        await asyncio.sleep(delay)
                    else:
                        logger.error(f"모든 재시도 실패: {str(e)}")
                        raise APIClientError(f"문서 검사 실패: {str(e)}")

# 편의 함수들
def create_api_client(base_url: str = "http://localhost:8000") -> APIClient:
    """
    API 클라이언트 인스턴스를 생성합니다.
    
    Args:
        base_url: 백엔드 서버 URL
        
    Returns:
        APIClient 인스턴스
    """
    return APIClient(base_url)

def test_connection(base_url: str = "http://localhost:8000") -> bool:
    """
    백엔드 서버 연결을 테스트합니다.
    
    Args:
        base_url: 백엔드 서버 URL
        
    Returns:
        연결 성공 여부
    """
    try:
        client = APIClient(base_url)
        client.check_health()
        return True
    except APIClientError:
        return False