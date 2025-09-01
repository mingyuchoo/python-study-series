"""
Azure OpenAI 클라이언트 모듈
GPT-4.1 모델을 활용한 문서 분석 기능 제공
"""

import asyncio
import json
import logging
from typing import Dict, List, Optional, Any
from openai import AsyncAzureOpenAI
from openai.types.chat import ChatCompletion
from app.core.config import settings

logger = logging.getLogger(__name__)


class AzureOpenAIClient:
    """
    Azure OpenAI 서비스와 통신하는 클라이언트 클래스
    """
    
    def __init__(self):
        """
        Azure OpenAI 클라이언트 초기화
        """
        self.deployment_name = settings.azure_openai_deployment_name
        self.client = None
        
        # Azure OpenAI 설정이 있는 경우에만 클라이언트 초기화
        if settings.azure_openai_endpoint and settings.azure_openai_api_key:
            try:
                self.client = AsyncAzureOpenAI(
                    azure_endpoint=settings.azure_openai_endpoint,
                    api_key=settings.azure_openai_api_key,
                    api_version=settings.azure_openai_api_version
                )
            except Exception as e:
                logger.error(f"Azure OpenAI 클라이언트 초기화 실패: {str(e)}")
                self.client = None
        
    async def test_connection(self) -> Dict[str, Any]:
        """
        Azure OpenAI 연결 테스트
        
        Returns:
            Dict: 연결 상태 정보
        """
        if not self.client:
            return {
                "status": "error",
                "message": "Azure OpenAI 클라이언트가 초기화되지 않았습니다. 환경변수를 확인해주세요.",
                "model": self.deployment_name
            }
        
        try:
            response = await self.client.chat.completions.create(
                model=self.deployment_name,
                messages=[
                    {"role": "user", "content": "안녕하세요. 연결 테스트입니다."}
                ],
                max_tokens=10,
                temperature=0.1
            )
            
            return {
                "status": "success",
                "message": "Azure OpenAI 연결 성공",
                "model": self.deployment_name,
                "response_id": response.id
            }
            
        except Exception as e:
            logger.error(f"Azure OpenAI 연결 테스트 실패: {str(e)}")
            return {
                "status": "error",
                "message": f"Azure OpenAI 연결 실패: {str(e)}",
                "model": self.deployment_name
            }
    
    async def analyze_document(self, content: str, check_type: str) -> Dict[str, Any]:
        """
        문서 내용을 분석하여 특정 유형의 검사 수행
        
        Args:
            content: 분석할 문서 내용
            check_type: 검사 유형 (grammar, spelling, consistency 등)
            
        Returns:
            Dict: 분석 결과
        """
        if not self.client:
            return {
                "status": "error",
                "message": "Azure OpenAI 클라이언트가 초기화되지 않았습니다.",
                "check_type": check_type
            }
        
        try:
            prompt = self._get_prompt_for_check_type(check_type, content)
            
            response = await self.client.chat.completions.create(
                model=self.deployment_name,
                messages=[
                    {"role": "system", "content": self._get_system_prompt()},
                    {"role": "user", "content": prompt}
                ],
                max_tokens=2000,
                temperature=0.1,
                response_format={"type": "json_object"}
            )
            
            result = json.loads(response.choices[0].message.content)
            
            return {
                "status": "success",
                "check_type": check_type,
                "result": result,
                "usage": {
                    "prompt_tokens": response.usage.prompt_tokens,
                    "completion_tokens": response.usage.completion_tokens,
                    "total_tokens": response.usage.total_tokens
                }
            }
            
        except json.JSONDecodeError as e:
            logger.error(f"JSON 파싱 오류: {str(e)}")
            return {
                "status": "error",
                "message": f"응답 파싱 오류: {str(e)}",
                "check_type": check_type
            }
            
        except Exception as e:
            logger.error(f"문서 분석 오류 ({check_type}): {str(e)}")
            return {
                "status": "error",
                "message": f"문서 분석 실패: {str(e)}",
                "check_type": check_type
            }
    
    async def get_comprehensive_analysis(self, content: str) -> Dict[str, Any]:
        """
        문서에 대한 종합적인 분석 수행
        
        Args:
            content: 분석할 문서 내용
            
        Returns:
            Dict: 종합 분석 결과
        """
        try:
            # 모든 검사 유형을 병렬로 실행
            check_types = ["grammar", "korean_spelling", "english_spelling", "consistency"]
            
            tasks = [
                self.analyze_document(content, check_type) 
                for check_type in check_types
            ]
            
            results = await asyncio.gather(*tasks, return_exceptions=True)
            
            # 결과 정리
            comprehensive_result = {
                "status": "success",
                "total_checks": len(check_types),
                "results": {},
                "summary": {
                    "total_errors": 0,
                    "priority_issues": [],
                    "recommendations": []
                }
            }
            
            for i, result in enumerate(results):
                check_type = check_types[i]
                
                if isinstance(result, Exception):
                    comprehensive_result["results"][check_type] = {
                        "status": "error",
                        "message": str(result)
                    }
                else:
                    comprehensive_result["results"][check_type] = result
                    
                    # 오류 개수 집계
                    if result.get("status") == "success" and "result" in result:
                        errors = result["result"].get("errors", [])
                        comprehensive_result["summary"]["total_errors"] += len(errors)
            
            return comprehensive_result
            
        except Exception as e:
            logger.error(f"종합 분석 오류: {str(e)}")
            return {
                "status": "error",
                "message": f"종합 분석 실패: {str(e)}"
            }
    
    def _get_system_prompt(self) -> str:
        """
        시스템 프롬프트 반환
        """
        return """
        당신은 한국어 문서 검사 전문가입니다. 
        주어진 문서를 분석하여 문법, 맞춤법, 일관성 등의 문제를 찾아 JSON 형식으로 결과를 제공해주세요.
        
        응답은 반드시 다음 JSON 구조를 따라야 합니다:
        {
            "errors": [
                {
                    "location": "문제가 발견된 위치",
                    "error_type": "오류 유형",
                    "current_text": "현재 텍스트",
                    "suggested_text": "수정 제안",
                    "confidence": 0.95,
                    "explanation": "오류 설명"
                }
            ],
            "summary": "검사 결과 요약"
        }
        """
    
    def _get_prompt_for_check_type(self, check_type: str, content: str) -> str:
        """
        검사 유형별 프롬프트 생성
        
        Args:
            check_type: 검사 유형
            content: 문서 내용
            
        Returns:
            str: 생성된 프롬프트
        """
        prompts = {
            "grammar": f"""
            다음 한국어 문서의 문법 오류를 검사해주세요:
            
            {content}
            
            문법 오류, 어순 문제, 조사 사용 오류 등을 찾아 JSON 형식으로 결과를 제공해주세요.
            """,
            
            "korean_spelling": f"""
            다음 한국어 문서의 맞춤법과 띄어쓰기 오류를 검사해주세요:
            
            {content}
            
            한국어 맞춤법 규칙에 따른 오류와 띄어쓰기 문제를 찾아 JSON 형식으로 결과를 제공해주세요.
            """,
            
            "english_spelling": f"""
            다음 문서에서 영어 단어들의 맞춤법을 검사해주세요:
            
            {content}
            
            영어 단어의 철자 오류를 찾아 JSON 형식으로 결과를 제공해주세요.
            """,
            
            "consistency": f"""
            다음 문서의 용어 일관성과 형식 통일성을 검사해주세요:
            
            {content}
            
            전문용어 사용의 일관성, 제목 스타일의 통일성 등을 검사하여 JSON 형식으로 결과를 제공해주세요.
            """
        }
        
        return prompts.get(check_type, f"다음 문서를 검사해주세요:\n\n{content}")


# 전역 클라이언트 인스턴스
azure_client = AzureOpenAIClient()


async def get_azure_client() -> AzureOpenAIClient:
    """
    Azure OpenAI 클라이언트 인스턴스 반환
    FastAPI 의존성 주입에서 사용
    """
    return azure_client