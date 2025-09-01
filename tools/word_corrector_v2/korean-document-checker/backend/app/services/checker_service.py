"""
문서 검사 서비스 모듈
Azure OpenAI를 활용한 다양한 문서 검사 기능 제공
"""

import logging
from typing import Dict, List, Optional, Any
from app.core.azure_client import AzureOpenAIClient
from app.services.document_processor import DocumentProcessor

logger = logging.getLogger(__name__)


class CheckerService:
    """
    문서 검사 서비스 클래스
    Azure OpenAI를 활용하여 문법, 맞춤법, 일관성 등을 검사
    """
    
    def __init__(self, azure_client: AzureOpenAIClient):
        """
        CheckerService 초기화
        
        Args:
            azure_client: Azure OpenAI 클라이언트 인스턴스
        """
        self.azure_client = azure_client
        self.document_processor = DocumentProcessor()
        
        # 프롬프트 템플릿 정의
        self.prompt_templates = {
            "grammar": {
                "system": """당신은 한국어 문법 검사 전문가입니다. 
주어진 텍스트에서 문법 오류를 찾아 정확한 수정 제안을 제공해주세요.

다음 문법 요소들을 체계적으로 검사해주세요:

1. 조사 사용 오류:
   - 주격조사: 이/가, 께서
   - 목적격조사: 을/를
   - 보격조사: 이/가
   - 부사격조사: 에, 에서, 로/으로, 와/과, 한테, 에게

2. 어미 활용 오류:
   - 용언의 활용 (동사, 형용사)
   - 불규칙 활용 오류
   - 어간과 어미의 결합 오류

3. 어순 문제:
   - 주어-목적어-서술어 순서
   - 관형어와 피수식어의 위치
   - 부사어의 적절한 위치

4. 주어와 서술어 호응:
   - 수 일치 (단수/복수)
   - 인칭 일치
   - 높임법 일치

5. 높임법 사용:
   - 주체 높임법 (-시-)
   - 객체 높임법 (드리다, 받다)
   - 상대 높임법 (해요체, 하십시오체)

6. 시제 표현:
   - 과거, 현재, 미래 시제의 적절한 사용
   - 시제 일치

각 오류에 대해 정확한 위치와 구체적인 수정 제안을 제공해주세요.
응답은 반드시 JSON 형식으로 제공해주세요.""",
                
                "user": """다음 한국어 텍스트의 문법을 체계적으로 검사해주세요:

{content}

각 문장을 분석하여 문법 오류를 찾고, 다음 JSON 형식으로 응답해주세요:

{{
    "errors": [
        {{
            "location": "오류가 발견된 정확한 문장 또는 구문",
            "error_type": "구체적인 문법 오류 유형 (예: 조사 사용 오류, 어미 활용 오류 등)",
            "current_text": "현재 잘못된 텍스트 부분",
            "suggested_text": "문법적으로 올바른 수정 제안",
            "confidence": 0.95,
            "explanation": "왜 이것이 오류인지, 어떤 문법 규칙을 위반했는지 상세 설명",
            "grammar_rule": "관련된 한국어 문법 규칙",
            "sentence_position": "문장 내에서의 위치 (시작/중간/끝)"
        }}
    ],
    "summary": "전체 문법 검사 결과 요약 (총 문장 수, 오류 발견 문장 수, 주요 문법 문제 유형)"
}}"""
            },
            
            "korean_spelling": {
                "system": """당신은 한국어 맞춤법 및 띄어쓰기 검사 전문가입니다.
한글 맞춤법 통일안과 표준국어대사전을 기준으로 정확한 검사를 수행해주세요.

다음 항목들을 체계적으로 검사해주세요:

1. 한글 맞춤법:
   - 소리에 따른 표기 (예: 굳이 → 구지 X)
   - 어간과 어미의 결합 (예: 같아 → 가타 X)
   - 준말 표기 (예: 이렇게 → 이렇케 X)
   - 된소리 표기 (예: 씁쓸하다 → 습쓸하다 X)

2. 띄어쓰기:
   - 조사와 어미는 붙여 쓰기
   - 의존명사는 띄어 쓰기 (것, 수, 때, 바 등)
   - 단위를 나타내는 명사는 띄어 쓰기
   - 보조용언은 띄어 쓰기 (하다, 되다, 있다 등)

3. 외래어 표기법:
   - 외래어 표기 용례집 기준
   - 원어의 발음에 가까운 표기
   - 이미 굳어진 관용 표기 존중

4. 복합어와 합성어:
   - 고유어끼리의 합성
   - 한자어끼리의 합성
   - 고유어와 한자어의 합성

5. 한자어 표기:
   - 표준 한자 표기
   - 동음이의어 구별

각 오류에 대해 정확한 근거와 함께 수정 제안을 제공해주세요.""",
                
                "user": """다음 한국어 텍스트의 맞춤법과 띄어쓰기를 검사해주세요:

{content}

각 단어와 구문을 분석하여 다음 JSON 형식으로 응답해주세요:

{{
    "errors": [
        {{
            "location": "오류가 발견된 정확한 위치",
            "error_type": "구체적인 오류 유형 (맞춤법/띄어쓰기/외래어표기/복합어표기)",
            "current_text": "현재 잘못된 텍스트",
            "suggested_text": "올바른 표기",
            "confidence": 0.95,
            "explanation": "오류 근거와 관련 맞춤법 규칙",
            "rule_reference": "관련 맞춤법 규정 또는 표준",
            "word_type": "단어 유형 (고유어/한자어/외래어/합성어)"
        }}
    ],
    "summary": "맞춤법 검사 결과 요약 (총 단어 수, 오류 개수, 주요 오류 유형)"
}}"""
            },
            
            "english_spelling": {
                "system": """당신은 영어 맞춤법 검사 전문가입니다.
한국어 텍스트 내에 포함된 영어 단어들을 식별하고 정확한 맞춤법 검사를 수행해주세요.

다음 항목들을 체계적으로 검사해주세요:

1. 영어 단어 철자:
   - 일반적인 철자 오류 (예: recieve → receive)
   - 이중 자음 오류 (예: occured → occurred)
   - 모음 조합 오류 (예: seperate → separate)

2. 대소문자 사용:
   - 고유명사의 첫 글자 대문자
   - 문장 시작 대문자
   - 약어의 적절한 대소문자 사용

3. 문법적 형태:
   - 복수형 표기 (-s, -es, 불규칙 복수)
   - 동사 활용 (-ed, -ing, 불규칙 동사)
   - 비교급, 최상급 형태

4. 전문용어 및 기술용어:
   - IT 용어의 정확한 표기
   - 학술 용어의 표준 표기
   - 브랜드명의 정확한 표기

5. 약어와 줄임말:
   - 표준 약어 형태
   - 마침표 사용 규칙

영어 단어만을 대상으로 하며, 한국어 텍스트는 무시해주세요.""",
                
                "user": """다음 텍스트에서 영어 단어들만을 식별하여 맞춤법을 검사해주세요:

{content}

영어 단어만을 대상으로 분석하여 다음 JSON 형식으로 응답해주세요:

{{
    "errors": [
        {{
            "location": "오류가 발견된 위치 (주변 한국어 텍스트 포함)",
            "error_type": "구체적인 영어 오류 유형 (철자/대소문자/문법형태/전문용어)",
            "current_text": "현재 잘못된 영어 단어",
            "suggested_text": "올바른 영어 단어",
            "confidence": 0.95,
            "explanation": "오류 설명 및 올바른 사용법",
            "word_category": "단어 분류 (일반어/전문용어/고유명사/약어)",
            "context": "단어가 사용된 문맥"
        }}
    ],
    "english_words_found": ["발견된", "모든", "영어", "단어들"],
    "summary": "영어 맞춤법 검사 결과 요약 (총 영어 단어 수, 오류 개수)"
}}"""
            },
            
            "consistency": {
                "system": """당신은 문서 일관성 및 통일성 검사 전문가입니다.
문서 전체의 일관성을 체계적으로 분석하여 통일성 문제를 찾아주세요.

다음 항목들을 세밀하게 검사해주세요:

1. 전문용어 일관성:
   - 동일한 개념에 대한 용어 통일 (예: 사용자/유저, 프로그램/소프트웨어)
   - 외래어와 한국어 표기의 일관성
   - 약어 사용의 통일성
   - 기술용어의 표준화

2. 문체 일관성:
   - 존댓말/반말의 통일
   - 문장 종결어미의 일관성 (-다/-습니다/-요)
   - 호칭과 지칭의 통일성
   - 시점(1인칭/3인칭)의 일관성

3. 표기법 통일성:
   - 숫자 표기 (아라비아 숫자/한글 숫자)
   - 날짜 및 시간 표기 형식
   - 단위 표기의 통일성
   - 괄호 사용법의 일관성

4. 서식 일관성:
   - 목록 표기 방식 (번호/기호)
   - 강조 표현의 통일성
   - 인용 표기법
   - 각주 및 참조 형식

5. 구조적 일관성:
   - 제목 체계의 논리성
   - 문단 구성의 일관성
   - 내용 전개 방식의 통일성

각 일관성 문제에 대해 구체적인 표준안을 제시해주세요.""",
                
                "user": """다음 텍스트의 용어 일관성과 형식 통일성을 종합적으로 검사해주세요:

{content}

문서 전체를 분석하여 다음 JSON 형식으로 응답해주세요:

{{
    "errors": [
        {{
            "location": "일관성 문제가 발견된 위치",
            "error_type": "구체적인 일관성 문제 유형 (용어/문체/표기법/서식/구조)",
            "current_text": "현재 일관되지 않은 표현",
            "suggested_text": "통일성을 위한 표준 표현",
            "confidence": 0.95,
            "explanation": "왜 일관성 문제인지, 어떻게 통일해야 하는지 설명",
            "consistency_rule": "적용해야 할 일관성 원칙",
            "affected_instances": ["문서", "내", "다른", "유사한", "사례들"]
        }}
    ],
    "terminology_analysis": {{
        "inconsistent_terms": [
            {{
                "concept": "개념명",
                "variations": ["변형1", "변형2"],
                "recommended": "권장 표준 용어"
            }}
        ]
    }},
    "style_analysis": {{
        "writing_style": "주요 문체 (존댓말/반말)",
        "inconsistencies": ["발견된", "문체", "불일치", "사례들"]
    }},
    "summary": "일관성 검사 종합 결과 (총 문제 수, 주요 일관성 이슈)"
}}"""
            }
        }
    
    async def grammar_check(self, content: str) -> Dict[str, Any]:
        """
        한국어 문법 검사 수행
        GPT-4.1 모델을 활용한 정밀한 구문 분석
        
        Args:
            content: 검사할 텍스트 내용
            
        Returns:
            Dict: 문법 검사 결과
        """
        try:
            logger.info("문법 검사 시작")
            
            # 텍스트를 문장 단위로 분할하여 더 정확한 분석
            sentences = self._split_into_sentences(content)
            logger.info(f"총 {len(sentences)}개 문장 분석 예정")
            
            # 긴 텍스트의 경우 청크 단위로 처리
            if len(content) > 3000:  # 3000자 이상인 경우
                return await self._process_long_text_grammar(content, sentences)
            
            # 프롬프트 생성
            system_prompt = self.prompt_templates["grammar"]["system"]
            user_prompt = self.prompt_templates["grammar"]["user"].format(content=content)
            
            # Azure OpenAI 호출
            result = await self._call_azure_openai(
                system_prompt=system_prompt,
                user_prompt=user_prompt,
                check_type="grammar"
            )
            
            # 결과 후처리 및 오류 위치 정밀화
            if result.get("status") == "success":
                result = self._post_process_grammar_result(result, content, sentences)
            
            logger.info(f"문법 검사 완료: {result.get('summary', {}).get('total_errors', 0)}개 오류 발견")
            return result
            
        except Exception as e:
            logger.error(f"문법 검사 오류: {str(e)}")
            return {
                "status": "error",
                "message": f"문법 검사 실패: {str(e)}",
                "check_type": "grammar"
            }
    
    async def _process_long_text_grammar(self, content: str, sentences: List[str]) -> Dict[str, Any]:
        """
        긴 텍스트에 대한 문법 검사 (청크 단위 처리)
        
        Args:
            content: 전체 텍스트
            sentences: 문장 리스트
            
        Returns:
            Dict: 통합된 문법 검사 결과
        """
        try:
            chunk_size = 10  # 한 번에 처리할 문장 수
            chunks = [sentences[i:i + chunk_size] for i in range(0, len(sentences), chunk_size)]
            
            all_results = []
            
            for i, chunk in enumerate(chunks):
                chunk_text = " ".join(chunk)
                logger.info(f"청크 {i+1}/{len(chunks)} 처리 중")
                
                system_prompt = self.prompt_templates["grammar"]["system"]
                user_prompt = self.prompt_templates["grammar"]["user"].format(content=chunk_text)
                
                chunk_result = await self._call_azure_openai(
                    system_prompt=system_prompt,
                    user_prompt=user_prompt,
                    check_type=f"grammar_chunk_{i+1}"
                )
                
                if chunk_result.get("status") == "success":
                    all_results.append(chunk_result)
            
            # 청크 결과들을 통합
            return self._merge_grammar_results(all_results, content, sentences)
            
        except Exception as e:
            logger.error(f"긴 텍스트 문법 검사 오류: {str(e)}")
            return {
                "status": "error",
                "message": f"긴 텍스트 문법 검사 실패: {str(e)}",
                "check_type": "grammar"
            }
    
    def _split_into_sentences(self, text: str) -> List[str]:
        """
        텍스트를 문장 단위로 분할
        
        Args:
            text: 분할할 텍스트
            
        Returns:
            List[str]: 문장 리스트
        """
        import re
        
        # 한국어 문장 종결 패턴
        sentence_endings = r'[.!?](?=\s|$)|[。！？](?=\s|$)'
        sentences = re.split(sentence_endings, text)
        
        # 빈 문장 제거 및 정리
        sentences = [s.strip() for s in sentences if s.strip()]
        
        return sentences
    
    def _post_process_grammar_result(self, result: Dict[str, Any], original_text: str, sentences: List[str]) -> Dict[str, Any]:
        """
        문법 검사 결과 후처리 및 오류 위치 정밀화
        
        Args:
            result: 원본 결과
            original_text: 원본 텍스트
            sentences: 문장 리스트
            
        Returns:
            Dict: 후처리된 결과
        """
        try:
            if "result" in result and isinstance(result["result"], dict):
                errors = result["result"].get("errors", [])
                
                # 각 오류에 대해 정확한 위치 정보 추가
                for error in errors:
                    error = self._enhance_error_location(error, original_text, sentences)
                    error["priority"] = self._calculate_grammar_priority(error)
                
                # 오류를 우선순위별로 정렬
                errors.sort(key=lambda x: x.get("priority", 0), reverse=True)
                
                # 문법 검사 특화 요약 정보 추가
                result["summary"] = {
                    "total_errors": len(errors),
                    "high_priority_errors": len([e for e in errors if e.get("priority", 0) >= 0.8]),
                    "error_types": self._categorize_grammar_errors(errors),
                    "total_sentences": len(sentences),
                    "error_sentences": len(set(e.get("sentence_index", -1) for e in errors if e.get("sentence_index", -1) >= 0)),
                    "check_type": "grammar",
                    "description": result["result"].get("summary", "")
                }
            
            return result
            
        except Exception as e:
            logger.error(f"문법 결과 후처리 오류: {str(e)}")
            return result
    
    def _enhance_error_location(self, error: Dict[str, Any], original_text: str, sentences: List[str]) -> Dict[str, Any]:
        """
        오류 위치 정보 강화
        
        Args:
            error: 오류 정보
            original_text: 원본 텍스트
            sentences: 문장 리스트
            
        Returns:
            Dict: 위치 정보가 강화된 오류
        """
        try:
            current_text = error.get("current_text", "")
            
            if current_text:
                # 원본 텍스트에서 오류 위치 찾기
                start_pos = original_text.find(current_text)
                if start_pos != -1:
                    error["char_position"] = start_pos
                    error["char_end_position"] = start_pos + len(current_text)
                
                # 해당 문장 찾기
                for i, sentence in enumerate(sentences):
                    if current_text in sentence:
                        error["sentence_index"] = i
                        error["sentence_text"] = sentence
                        break
            
            return error
            
        except Exception as e:
            logger.error(f"오류 위치 강화 실패: {str(e)}")
            return error
    
    def _calculate_grammar_priority(self, error: Dict[str, Any]) -> float:
        """
        문법 오류 우선순위 계산
        
        Args:
            error: 오류 정보
            
        Returns:
            float: 우선순위 점수
        """
        base_confidence = error.get("confidence", 0.5)
        error_type = error.get("error_type", "").lower()
        
        # 문법 오류 유형별 가중치
        type_weights = {
            "조사": 0.9,
            "어미": 0.85,
            "어순": 0.8,
            "호응": 0.9,
            "높임법": 0.7,
            "시제": 0.75
        }
        
        # 오류 유형에 따른 가중치 적용
        weight = 0.5
        for key, value in type_weights.items():
            if key in error_type:
                weight = value
                break
        
        return min(base_confidence * weight, 1.0)
    
    def _categorize_grammar_errors(self, errors: List[Dict[str, Any]]) -> Dict[str, int]:
        """
        문법 오류 유형별 분류
        
        Args:
            errors: 오류 리스트
            
        Returns:
            Dict: 오류 유형별 개수
        """
        categories = {}
        
        for error in errors:
            error_type = error.get("error_type", "기타")
            categories[error_type] = categories.get(error_type, 0) + 1
        
        return categories
    
    def _merge_grammar_results(self, chunk_results: List[Dict[str, Any]], original_text: str, sentences: List[str]) -> Dict[str, Any]:
        """
        청크별 문법 검사 결과 통합
        
        Args:
            chunk_results: 청크별 결과 리스트
            original_text: 원본 텍스트
            sentences: 문장 리스트
            
        Returns:
            Dict: 통합된 결과
        """
        try:
            all_errors = []
            total_usage = {"prompt_tokens": 0, "completion_tokens": 0, "total_tokens": 0}
            
            for chunk_result in chunk_results:
                if chunk_result.get("status") == "success" and "result" in chunk_result:
                    errors = chunk_result["result"].get("errors", [])
                    all_errors.extend(errors)
                    
                    # 사용량 통계 합산
                    usage = chunk_result.get("usage", {})
                    for key in total_usage:
                        total_usage[key] += usage.get(key, 0)
            
            # 중복 오류 제거
            unique_errors = self._remove_duplicate_errors(all_errors)
            
            # 통합 결과 생성
            merged_result = {
                "status": "success",
                "check_type": "grammar",
                "result": {
                    "errors": unique_errors,
                    "summary": f"총 {len(sentences)}개 문장에서 {len(unique_errors)}개의 문법 오류 발견"
                },
                "usage": total_usage
            }
            
            # 후처리 적용
            return self._post_process_grammar_result(merged_result, original_text, sentences)
            
        except Exception as e:
            logger.error(f"문법 결과 통합 오류: {str(e)}")
            return {
                "status": "error",
                "message": f"결과 통합 실패: {str(e)}",
                "check_type": "grammar"
            }
    
    def _remove_duplicate_errors(self, errors: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        중복 오류 제거
        
        Args:
            errors: 오류 리스트
            
        Returns:
            List: 중복이 제거된 오류 리스트
        """
        seen = set()
        unique_errors = []
        
        for error in errors:
            # 현재 텍스트와 제안 텍스트를 기준으로 중복 판단
            key = (error.get("current_text", ""), error.get("suggested_text", ""))
            
            if key not in seen:
                seen.add(key)
                unique_errors.append(error)
        
        return unique_errors
    
    def _extract_korean_text(self, content: str) -> str:
        """
        텍스트에서 한국어 부분만 추출
        
        Args:
            content: 원본 텍스트
            
        Returns:
            str: 한국어만 포함된 텍스트
        """
        import re
        
        # 영어 단어 패턴 (알파벳으로 구성된 단어)
        english_pattern = r'\b[A-Za-z]+\b'
        
        # 영어 단어를 임시 플레이스홀더로 대체
        korean_text = re.sub(english_pattern, '[영어단어]', content)
        
        return korean_text
    
    def _extract_english_words(self, content: str) -> List[str]:
        """
        텍스트에서 영어 단어들 추출
        
        Args:
            content: 원본 텍스트
            
        Returns:
            List[str]: 발견된 영어 단어 리스트
        """
        import re
        
        # 영어 단어 패턴 (최소 2글자 이상의 알파벳)
        english_pattern = r'\b[A-Za-z]{2,}\b'
        
        english_words = re.findall(english_pattern, content)
        
        # 중복 제거 및 정렬
        unique_words = list(set(english_words))
        unique_words.sort()
        
        return unique_words
    
    async def _process_long_text_korean_spelling(self, original_content: str, korean_content: str) -> Dict[str, Any]:
        """
        긴 텍스트에 대한 한국어 맞춤법 검사
        
        Args:
            original_content: 원본 텍스트
            korean_content: 한국어만 추출된 텍스트
            
        Returns:
            Dict: 맞춤법 검사 결과
        """
        try:
            # 문단 단위로 분할
            paragraphs = korean_content.split('\n\n')
            paragraphs = [p.strip() for p in paragraphs if p.strip()]
            
            chunk_size = 5  # 한 번에 처리할 문단 수
            chunks = [paragraphs[i:i + chunk_size] for i in range(0, len(paragraphs), chunk_size)]
            
            all_results = []
            
            for i, chunk in enumerate(chunks):
                chunk_text = '\n\n'.join(chunk)
                logger.info(f"한국어 맞춤법 청크 {i+1}/{len(chunks)} 처리 중")
                
                system_prompt = self.prompt_templates["korean_spelling"]["system"]
                user_prompt = self.prompt_templates["korean_spelling"]["user"].format(content=chunk_text)
                
                chunk_result = await self._call_azure_openai(
                    system_prompt=system_prompt,
                    user_prompt=user_prompt,
                    check_type=f"korean_spelling_chunk_{i+1}"
                )
                
                if chunk_result.get("status") == "success":
                    all_results.append(chunk_result)
            
            # 청크 결과들을 통합
            return self._merge_spelling_results(all_results, original_content, "korean")
            
        except Exception as e:
            logger.error(f"긴 텍스트 한국어 맞춤법 검사 오류: {str(e)}")
            return {
                "status": "error",
                "message": f"긴 텍스트 한국어 맞춤법 검사 실패: {str(e)}",
                "check_type": "korean_spelling"
            }
    
    def _post_process_spelling_result(self, result: Dict[str, Any], original_text: str, language: str) -> Dict[str, Any]:
        """
        맞춤법 검사 결과 후처리
        
        Args:
            result: 원본 결과
            original_text: 원본 텍스트
            language: 언어 유형 (korean/english)
            
        Returns:
            Dict: 후처리된 결과
        """
        try:
            if "result" in result and isinstance(result["result"], dict):
                errors = result["result"].get("errors", [])
                
                # 각 오류에 대해 우선순위 계산
                for error in errors:
                    error["priority"] = self._calculate_spelling_priority(error, language)
                
                # 오류를 우선순위별로 정렬
                errors.sort(key=lambda x: x.get("priority", 0), reverse=True)
                
                # 맞춤법 검사 특화 요약 정보 추가
                error_types = {}
                for error in errors:
                    error_type = error.get("error_type", "기타")
                    error_types[error_type] = error_types.get(error_type, 0) + 1
                
                result["summary"] = {
                    "total_errors": len(errors),
                    "high_priority_errors": len([e for e in errors if e.get("priority", 0) >= 0.8]),
                    "error_types": error_types,
                    "language": language,
                    "check_type": f"{language}_spelling",
                    "description": result["result"].get("summary", "")
                }
                
                if language == "english":
                    english_words = result["result"].get("english_words_found", [])
                    result["summary"]["english_words_count"] = len(english_words)
            
            return result
            
        except Exception as e:
            logger.error(f"맞춤법 결과 후처리 오류 ({language}): {str(e)}")
            return result
    
    def _calculate_spelling_priority(self, error: Dict[str, Any], language: str) -> float:
        """
        맞춤법 오류 우선순위 계산
        
        Args:
            error: 오류 정보
            language: 언어 유형
            
        Returns:
            float: 우선순위 점수
        """
        base_confidence = error.get("confidence", 0.5)
        error_type = error.get("error_type", "").lower()
        
        if language == "korean":
            # 한국어 맞춤법 오류 유형별 가중치
            type_weights = {
                "맞춤법": 0.9,
                "띄어쓰기": 0.85,
                "외래어표기": 0.7,
                "복합어표기": 0.75
            }
        else:  # english
            # 영어 맞춤법 오류 유형별 가중치
            type_weights = {
                "철자": 0.9,
                "대소문자": 0.6,
                "문법형태": 0.8,
                "전문용어": 0.85
            }
        
        # 오류 유형에 따른 가중치 적용
        weight = 0.5
        for key, value in type_weights.items():
            if key in error_type:
                weight = value
                break
        
        return min(base_confidence * weight, 1.0)
    
    def _merge_spelling_results(self, chunk_results: List[Dict[str, Any]], original_text: str, language: str) -> Dict[str, Any]:
        """
        청크별 맞춤법 검사 결과 통합
        
        Args:
            chunk_results: 청크별 결과 리스트
            original_text: 원본 텍스트
            language: 언어 유형
            
        Returns:
            Dict: 통합된 결과
        """
        try:
            all_errors = []
            total_usage = {"prompt_tokens": 0, "completion_tokens": 0, "total_tokens": 0}
            
            for chunk_result in chunk_results:
                if chunk_result.get("status") == "success" and "result" in chunk_result:
                    errors = chunk_result["result"].get("errors", [])
                    all_errors.extend(errors)
                    
                    # 사용량 통계 합산
                    usage = chunk_result.get("usage", {})
                    for key in total_usage:
                        total_usage[key] += usage.get(key, 0)
            
            # 중복 오류 제거
            unique_errors = self._remove_duplicate_errors(all_errors)
            
            # 통합 결과 생성
            merged_result = {
                "status": "success",
                "check_type": f"{language}_spelling",
                "result": {
                    "errors": unique_errors,
                    "summary": f"{language} 맞춤법 검사에서 {len(unique_errors)}개의 오류 발견"
                },
                "usage": total_usage
            }
            
            # 후처리 적용
            return self._post_process_spelling_result(merged_result, original_text, language)
            
        except Exception as e:
            logger.error(f"맞춤법 결과 통합 오류 ({language}): {str(e)}")
            return {
                "status": "error",
                "message": f"결과 통합 실패: {str(e)}",
                "check_type": f"{language}_spelling"
            }
    
    def _analyze_document_structure(self, structure: Dict[str, Any]) -> Dict[str, Any]:
        """
        문서 구조 분석
        
        Args:
            structure: 문서 구조 정보
            
        Returns:
            Dict: 구조 분석 결과
        """
        try:
            analysis = {
                "heading_levels": [],
                "style_usage": {},
                "structure_issues": [],
                "total_headings": 0
            }
            
            if "headings" in structure:
                headings = structure["headings"]
                analysis["total_headings"] = len(headings)
                
                # 제목 레벨 분석
                levels = [h.get("level", 1) for h in headings]
                analysis["heading_levels"] = sorted(set(levels))
                
                # 레벨 순서 검사
                for i in range(1, len(levels)):
                    if levels[i] > levels[i-1] + 1:
                        analysis["structure_issues"].append(f"제목 레벨 건너뛰기: {levels[i-1]} → {levels[i]}")
            
            if "styles" in structure:
                analysis["style_usage"] = structure["styles"]
            
            return analysis
            
        except Exception as e:
            logger.error(f"문서 구조 분석 오류: {str(e)}")
            return {"error": str(e)}
    
    def _structure_to_detailed_text(self, structure: Dict[str, Any], analysis: Dict[str, Any]) -> str:
        """
        문서 구조를 상세 텍스트로 변환
        
        Args:
            structure: 문서 구조 정보
            analysis: 구조 분석 결과
            
        Returns:
            str: 상세 구조 정보 텍스트
        """
        try:
            text_parts = []
            
            # 구조 분석 요약
            text_parts.append("=== 문서 구조 분석 ===")
            text_parts.append(f"총 제목 수: {analysis.get('total_headings', 0)}")
            text_parts.append(f"사용된 제목 레벨: {analysis.get('heading_levels', [])}")
            
            if analysis.get("structure_issues"):
                text_parts.append("구조적 문제:")
                for issue in analysis["structure_issues"]:
                    text_parts.append(f"  - {issue}")
            
            # 제목 구조 상세 정보
            if "headings" in structure:
                text_parts.append("\n=== 제목 구조 상세 ===")
                for i, heading in enumerate(structure["headings"]):
                    level = heading.get("level", 1)
                    text = heading.get("text", "")
                    style = heading.get("style", "")
                    text_parts.append(f"{i+1}. 레벨 {level}: '{text}' (스타일: {style})")
            
            # 스타일 사용 통계
            if "styles" in structure:
                text_parts.append("\n=== 스타일 사용 통계 ===")
                for style_name, count in structure["styles"].items():
                    text_parts.append(f"{style_name}: {count}회 사용")
            
            return "\n".join(text_parts)
            
        except Exception as e:
            logger.error(f"상세 구조 텍스트 변환 오류: {str(e)}")
            return str(structure)
    
    def _analyze_terminology(self, content: str) -> Dict[str, Any]:
        """
        용어 사용 분석
        
        Args:
            content: 텍스트 내용
            
        Returns:
            Dict: 용어 분석 결과
        """
        try:
            import re
            from collections import Counter
            
            analysis = {
                "potential_terms": [],
                "repeated_phrases": [],
                "style_patterns": [],
                "word_frequency": {}
            }
            
            # 잠재적 전문용어 추출 (대문자로 시작하는 단어, 영어 단어 등)
            terms_pattern = r'\b[A-Z][a-zA-Z]+\b|\b[가-힣]{3,}\b'
            potential_terms = re.findall(terms_pattern, content)
            
            # 빈도 분석
            term_counter = Counter(potential_terms)
            analysis["word_frequency"] = dict(term_counter.most_common(20))
            
            # 반복되는 구문 찾기 (2회 이상 등장)
            repeated = [term for term, count in term_counter.items() if count >= 2]
            analysis["repeated_phrases"] = repeated[:10]
            
            # 문체 패턴 분석
            if "습니다" in content and "다." in content:
                analysis["style_patterns"].append("존댓말과 평서문 혼용")
            
            return analysis
            
        except Exception as e:
            logger.error(f"용어 분석 오류: {str(e)}")
            return {"error": str(e)}
    
    def _enhance_content_with_terminology(self, content: str, terminology_analysis: Dict[str, Any]) -> str:
        """
        용어 분석 결과를 포함하여 내용 강화
        
        Args:
            content: 원본 내용
            terminology_analysis: 용어 분석 결과
            
        Returns:
            str: 강화된 내용
        """
        try:
            enhanced_parts = [content]
            
            if terminology_analysis.get("repeated_phrases"):
                enhanced_parts.append("\n\n=== 반복 사용된 용어들 ===")
                for phrase in terminology_analysis["repeated_phrases"][:5]:
                    enhanced_parts.append(f"- {phrase}")
            
            if terminology_analysis.get("style_patterns"):
                enhanced_parts.append("\n=== 발견된 문체 패턴 ===")
                for pattern in terminology_analysis["style_patterns"]:
                    enhanced_parts.append(f"- {pattern}")
            
            return "\n".join(enhanced_parts)
            
        except Exception as e:
            logger.error(f"내용 강화 오류: {str(e)}")
            return content
    
    async def _process_long_text_consistency(self, content: str, terminology_analysis: Dict[str, Any]) -> Dict[str, Any]:
        """
        긴 텍스트에 대한 일관성 검사
        
        Args:
            content: 원본 텍스트
            terminology_analysis: 용어 분석 결과
            
        Returns:
            Dict: 일관성 검사 결과
        """
        try:
            # 문단 단위로 분할하되 용어 일관성을 위해 겹치는 부분 포함
            paragraphs = content.split('\n\n')
            paragraphs = [p.strip() for p in paragraphs if p.strip()]
            
            chunk_size = 3  # 한 번에 처리할 문단 수
            overlap = 1     # 겹치는 문단 수
            
            chunks = []
            for i in range(0, len(paragraphs), chunk_size - overlap):
                chunk = paragraphs[i:i + chunk_size]
                chunks.append('\n\n'.join(chunk))
            
            all_results = []
            
            for i, chunk in enumerate(chunks):
                logger.info(f"일관성 검사 청크 {i+1}/{len(chunks)} 처리 중")
                
                enhanced_chunk = self._enhance_content_with_terminology(chunk, terminology_analysis)
                
                system_prompt = self.prompt_templates["consistency"]["system"]
                user_prompt = self.prompt_templates["consistency"]["user"].format(content=enhanced_chunk)
                
                chunk_result = await self._call_azure_openai(
                    system_prompt=system_prompt,
                    user_prompt=user_prompt,
                    check_type=f"consistency_chunk_{i+1}"
                )
                
                if chunk_result.get("status") == "success":
                    all_results.append(chunk_result)
            
            # 청크 결과들을 통합
            return self._merge_consistency_results(all_results, content, terminology_analysis)
            
        except Exception as e:
            logger.error(f"긴 텍스트 일관성 검사 오류: {str(e)}")
            return {
                "status": "error",
                "message": f"긴 텍스트 일관성 검사 실패: {str(e)}",
                "check_type": "terminology_consistency"
            }
    
    def _post_process_consistency_result(self, result: Dict[str, Any], consistency_type: str, analysis: Dict[str, Any]) -> Dict[str, Any]:
        """
        일관성 검사 결과 후처리
        
        Args:
            result: 원본 결과
            consistency_type: 일관성 검사 유형 (layout/terminology)
            analysis: 분석 결과
            
        Returns:
            Dict: 후처리된 결과
        """
        try:
            if "result" in result and isinstance(result["result"], dict):
                errors = result["result"].get("errors", [])
                
                # 각 오류에 대해 우선순위 계산
                for error in errors:
                    error["priority"] = self._calculate_consistency_priority(error, consistency_type)
                
                # 오류를 우선순위별로 정렬
                errors.sort(key=lambda x: x.get("priority", 0), reverse=True)
                
                # 일관성 검사 특화 요약 정보 추가
                error_types = {}
                for error in errors:
                    error_type = error.get("error_type", "기타")
                    error_types[error_type] = error_types.get(error_type, 0) + 1
                
                summary_info = {
                    "total_errors": len(errors),
                    "high_priority_errors": len([e for e in errors if e.get("priority", 0) >= 0.8]),
                    "error_types": error_types,
                    "consistency_type": consistency_type,
                    "check_type": f"{consistency_type}_consistency",
                    "description": result["result"].get("summary", "")
                }
                
                # 분석 결과 추가
                if consistency_type == "terminology" and not analysis.get("error"):
                    summary_info["terminology_stats"] = {
                        "repeated_phrases_count": len(analysis.get("repeated_phrases", [])),
                        "style_patterns_count": len(analysis.get("style_patterns", []))
                    }
                elif consistency_type == "layout" and not analysis.get("error"):
                    summary_info["layout_stats"] = {
                        "total_headings": analysis.get("total_headings", 0),
                        "heading_levels": analysis.get("heading_levels", []),
                        "structure_issues_count": len(analysis.get("structure_issues", []))
                    }
                
                result["summary"] = summary_info
            
            return result
            
        except Exception as e:
            logger.error(f"일관성 결과 후처리 오류 ({consistency_type}): {str(e)}")
            return result
    
    def _calculate_consistency_priority(self, error: Dict[str, Any], consistency_type: str) -> float:
        """
        일관성 오류 우선순위 계산
        
        Args:
            error: 오류 정보
            consistency_type: 일관성 유형
            
        Returns:
            float: 우선순위 점수
        """
        base_confidence = error.get("confidence", 0.5)
        error_type = error.get("error_type", "").lower()
        
        if consistency_type == "terminology":
            # 용어 일관성 오류 유형별 가중치
            type_weights = {
                "용어": 0.9,
                "문체": 0.85,
                "표기법": 0.8,
                "서식": 0.7
            }
        else:  # layout
            # 레이아웃 일관성 오류 유형별 가중치
            type_weights = {
                "제목체계": 0.9,
                "스타일": 0.8,
                "구조": 0.85,
                "형식": 0.75
            }
        
        # 오류 유형에 따른 가중치 적용
        weight = 0.5
        for key, value in type_weights.items():
            if key in error_type:
                weight = value
                break
        
        return min(base_confidence * weight, 1.0)
    
    def _merge_consistency_results(self, chunk_results: List[Dict[str, Any]], original_text: str, terminology_analysis: Dict[str, Any]) -> Dict[str, Any]:
        """
        청크별 일관성 검사 결과 통합
        
        Args:
            chunk_results: 청크별 결과 리스트
            original_text: 원본 텍스트
            terminology_analysis: 용어 분석 결과
            
        Returns:
            Dict: 통합된 결과
        """
        try:
            all_errors = []
            total_usage = {"prompt_tokens": 0, "completion_tokens": 0, "total_tokens": 0}
            
            for chunk_result in chunk_results:
                if chunk_result.get("status") == "success" and "result" in chunk_result:
                    errors = chunk_result["result"].get("errors", [])
                    all_errors.extend(errors)
                    
                    # 사용량 통계 합산
                    usage = chunk_result.get("usage", {})
                    for key in total_usage:
                        total_usage[key] += usage.get(key, 0)
            
            # 중복 오류 제거 (일관성 검사에서는 더 엄격하게)
            unique_errors = self._remove_duplicate_consistency_errors(all_errors)
            
            # 통합 결과 생성
            merged_result = {
                "status": "success",
                "check_type": "terminology_consistency",
                "result": {
                    "errors": unique_errors,
                    "summary": f"용어 일관성 검사에서 {len(unique_errors)}개의 문제 발견"
                },
                "usage": total_usage
            }
            
            # 후처리 적용
            return self._post_process_consistency_result(merged_result, "terminology", terminology_analysis)
            
        except Exception as e:
            logger.error(f"일관성 결과 통합 오류: {str(e)}")
            return {
                "status": "error",
                "message": f"결과 통합 실패: {str(e)}",
                "check_type": "terminology_consistency"
            }
    
    def _remove_duplicate_consistency_errors(self, errors: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        일관성 검사에서 중복 오류 제거 (더 엄격한 기준)
        
        Args:
            errors: 오류 리스트
            
        Returns:
            List: 중복이 제거된 오류 리스트
        """
        seen = set()
        unique_errors = []
        
        for error in errors:
            # 현재 텍스트, 제안 텍스트, 오류 유형을 모두 고려하여 중복 판단
            key = (
                error.get("current_text", ""),
                error.get("suggested_text", ""),
                error.get("error_type", "")
            )
            
            if key not in seen:
                seen.add(key)
                unique_errors.append(error)
        
        return unique_errors
    
    async def korean_spell_check(self, content: str) -> Dict[str, Any]:
        """
        한국어 맞춤법 및 띄어쓰기 검사 수행
        
        Args:
            content: 검사할 텍스트 내용
            
        Returns:
            Dict: 맞춤법 검사 결과
        """
        try:
            logger.info("한국어 맞춤법 검사 시작")
            
            # 텍스트 전처리 - 영어 단어 임시 제거하여 한국어만 검사
            korean_only_content = self._extract_korean_text(content)
            
            # 긴 텍스트의 경우 청크 단위로 처리
            if len(korean_only_content) > 2500:
                return await self._process_long_text_korean_spelling(content, korean_only_content)
            
            # 프롬프트 생성
            system_prompt = self.prompt_templates["korean_spelling"]["system"]
            user_prompt = self.prompt_templates["korean_spelling"]["user"].format(content=korean_only_content)
            
            # Azure OpenAI 호출
            result = await self._call_azure_openai(
                system_prompt=system_prompt,
                user_prompt=user_prompt,
                check_type="korean_spelling"
            )
            
            # 결과 후처리
            if result.get("status") == "success":
                result = self._post_process_spelling_result(result, content, "korean")
            
            logger.info(f"한국어 맞춤법 검사 완료: {result.get('summary', {}).get('total_errors', 0)}개 오류 발견")
            return result
            
        except Exception as e:
            logger.error(f"한국어 맞춤법 검사 오류: {str(e)}")
            return {
                "status": "error",
                "message": f"한국어 맞춤법 검사 실패: {str(e)}",
                "check_type": "korean_spelling"
            }
    
    async def english_spell_check(self, content: str) -> Dict[str, Any]:
        """
        영어 맞춤법 검사 수행
        한국어 텍스트 내의 영어 단어들을 식별하고 검사
        
        Args:
            content: 검사할 텍스트 내용
            
        Returns:
            Dict: 영어 맞춤법 검사 결과
        """
        try:
            logger.info("영어 맞춤법 검사 시작")
            
            # 영어 단어 사전 식별
            english_words = self._extract_english_words(content)
            
            if not english_words:
                logger.info("영어 단어가 발견되지 않음")
                return {
                    "status": "success",
                    "check_type": "english_spelling",
                    "result": {
                        "errors": [],
                        "english_words_found": [],
                        "summary": "영어 단어가 발견되지 않았습니다."
                    },
                    "summary": {
                        "total_errors": 0,
                        "english_words_count": 0,
                        "check_type": "english_spelling"
                    }
                }
            
            logger.info(f"{len(english_words)}개의 영어 단어 발견")
            
            # 프롬프트 생성
            system_prompt = self.prompt_templates["english_spelling"]["system"]
            user_prompt = self.prompt_templates["english_spelling"]["user"].format(content=content)
            
            # Azure OpenAI 호출
            result = await self._call_azure_openai(
                system_prompt=system_prompt,
                user_prompt=user_prompt,
                check_type="english_spelling"
            )
            
            # 결과 후처리
            if result.get("status") == "success":
                result = self._post_process_spelling_result(result, content, "english")
                # 발견된 영어 단어 정보 추가
                if "result" in result:
                    result["result"]["english_words_found"] = english_words
            
            logger.info(f"영어 맞춤법 검사 완료: {result.get('summary', {}).get('total_errors', 0)}개 오류 발견")
            return result
            
        except Exception as e:
            logger.error(f"영어 맞춤법 검사 오류: {str(e)}")
            return {
                "status": "error",
                "message": f"영어 맞춤법 검사 실패: {str(e)}",
                "check_type": "english_spelling"
            }
    
    async def layout_consistency_check(self, structure: Dict[str, Any]) -> Dict[str, Any]:
        """
        문서 레이아웃 및 제목 스타일 일관성 검사
        
        Args:
            structure: 문서 구조 정보
            
        Returns:
            Dict: 레이아웃 일관성 검사 결과
        """
        try:
            logger.info("레이아웃 일관성 검사 시작")
            
            # 문서 구조 분석
            structure_analysis = self._analyze_document_structure(structure)
            
            # 문서 구조를 상세 텍스트로 변환
            structure_text = self._structure_to_detailed_text(structure, structure_analysis)
            
            # 프롬프트 생성
            system_prompt = """당신은 문서 레이아웃 및 형식 일관성 검사 전문가입니다.
주어진 문서 구조 정보를 체계적으로 분석하여 형식의 일관성을 검사해주세요.

다음 항목들을 세밀하게 검사해주세요:

1. 제목 체계 일관성:
   - 제목 레벨의 논리적 순서 (1단계 → 2단계 → 3단계)
   - 제목 스타일의 통일성
   - 제목 번호 매기기 체계
   - 제목 길이와 형식의 일관성

2. 문서 구조 일관성:
   - 섹션 구성의 논리성
   - 하위 항목의 일관된 구성
   - 목록 형식의 통일성

3. 스타일 적용 일관성:
   - 동일한 레벨 제목의 스타일 통일
   - 본문 스타일의 일관성
   - 강조 표현의 통일성

4. 형식 규칙 준수:
   - 문서 형식 가이드라인 준수
   - 표준 문서 구조 원칙 적용

각 문제에 대해 구체적인 수정 방안을 제시해주세요."""
            
            user_prompt = f"""다음 문서 구조의 레이아웃 일관성을 종합적으로 검사해주세요:

{structure_text}

문서 구조를 분석하여 다음 JSON 형식으로 응답해주세요:

{{
    "errors": [
        {{
            "location": "문제가 발견된 구체적 위치",
            "error_type": "구체적인 레이아웃 문제 유형 (제목체계/스타일/구조/형식)",
            "current_text": "현재 문제가 있는 형식",
            "suggested_text": "일관성을 위한 표준 형식",
            "confidence": 0.95,
            "explanation": "왜 이것이 일관성 문제인지, 어떻게 수정해야 하는지 설명",
            "layout_rule": "적용해야 할 레이아웃 규칙",
            "severity": "문제의 심각도 (high/medium/low)"
        }}
    ],
    "structure_analysis": {{
        "heading_levels_used": [1, 2, 3],
        "style_inconsistencies": ["발견된", "스타일", "불일치"],
        "structural_issues": ["구조적", "문제점들"]
    }},
    "recommendations": [
        "전체적인 개선 권장사항들"
    ],
    "summary": "레이아웃 일관성 검사 종합 결과"
}}"""
            
            # Azure OpenAI 호출
            result = await self._call_azure_openai(
                system_prompt=system_prompt,
                user_prompt=user_prompt,
                check_type="layout_consistency"
            )
            
            # 결과 후처리
            if result.get("status") == "success":
                result = self._post_process_consistency_result(result, "layout", structure_analysis)
            
            logger.info(f"레이아웃 일관성 검사 완료: {result.get('summary', {}).get('total_errors', 0)}개 문제 발견")
            return result
            
        except Exception as e:
            logger.error(f"레이아웃 일관성 검사 오류: {str(e)}")
            return {
                "status": "error",
                "message": f"레이아웃 일관성 검사 실패: {str(e)}",
                "check_type": "layout_consistency"
            }
    
    async def terminology_consistency_check(self, content: str) -> Dict[str, Any]:
        """
        전문용어 일관성 검사 수행
        
        Args:
            content: 검사할 텍스트 내용
            
        Returns:
            Dict: 전문용어 일관성 검사 결과
        """
        try:
            logger.info("전문용어 일관성 검사 시작")
            
            # 용어 사전 분석
            terminology_analysis = self._analyze_terminology(content)
            
            # 긴 텍스트의 경우 청크 단위로 처리하되 용어 일관성 유지
            if len(content) > 4000:
                return await self._process_long_text_consistency(content, terminology_analysis)
            
            # 프롬프트 생성 (용어 분석 결과 포함)
            enhanced_content = self._enhance_content_with_terminology(content, terminology_analysis)
            
            system_prompt = self.prompt_templates["consistency"]["system"]
            user_prompt = self.prompt_templates["consistency"]["user"].format(content=enhanced_content)
            
            # Azure OpenAI 호출
            result = await self._call_azure_openai(
                system_prompt=system_prompt,
                user_prompt=user_prompt,
                check_type="terminology_consistency"
            )
            
            # 결과 후처리
            if result.get("status") == "success":
                result = self._post_process_consistency_result(result, "terminology", terminology_analysis)
            
            logger.info(f"전문용어 일관성 검사 완료: {result.get('summary', {}).get('total_errors', 0)}개 문제 발견")
            return result
            
        except Exception as e:
            logger.error(f"전문용어 일관성 검사 오류: {str(e)}")
            return {
                "status": "error",
                "message": f"전문용어 일관성 검사 실패: {str(e)}",
                "check_type": "terminology_consistency"
            }
    
    async def comprehensive_check(self, file_path: str) -> Dict[str, Any]:
        """
        파일에 대한 종합적인 검사 수행
        
        Args:
            file_path: 검사할 파일 경로
            
        Returns:
            Dict: 종합 검사 결과
        """
        try:
            logger.info(f"종합 검사 시작: {file_path}")
            
            # 문서 내용 및 구조 추출
            content = self.document_processor.extract_text_from_docx(file_path)
            structure = self.document_processor.extract_structure_info(file_path)
            
            # 모든 검사 수행
            results = {}
            
            # 텍스트 기반 검사들을 병렬로 실행
            import asyncio
            text_checks = await asyncio.gather(
                self.grammar_check(content),
                self.korean_spell_check(content),
                self.english_spell_check(content),
                self.terminology_consistency_check(content),
                return_exceptions=True
            )
            
            check_names = ["grammar", "korean_spelling", "english_spelling", "terminology_consistency"]
            for i, result in enumerate(text_checks):
                if isinstance(result, Exception):
                    results[check_names[i]] = {
                        "status": "error",
                        "message": str(result)
                    }
                else:
                    results[check_names[i]] = result
            
            # 레이아웃 검사 수행
            layout_result = await self.layout_consistency_check(structure)
            results["layout_consistency"] = layout_result
            
            # 종합 결과 생성
            comprehensive_result = self._generate_comprehensive_summary(results, file_path)
            
            logger.info(f"종합 검사 완료: 총 {comprehensive_result.get('summary', {}).get('total_errors', 0)}개 문제 발견")
            return comprehensive_result
            
        except Exception as e:
            logger.error(f"종합 검사 오류: {str(e)}")
            return {
                "status": "error",
                "message": f"종합 검사 실패: {str(e)}",
                "file_path": file_path
            }
    
    async def _call_azure_openai(self, system_prompt: str, user_prompt: str, check_type: str) -> Dict[str, Any]:
        """
        Azure OpenAI API 호출
        
        Args:
            system_prompt: 시스템 프롬프트
            user_prompt: 사용자 프롬프트
            check_type: 검사 유형
            
        Returns:
            Dict: API 호출 결과
        """
        try:
            import json
            from openai import AsyncAzureOpenAI
            
            if not self.azure_client.client:
                return {
                    "status": "error",
                    "message": "Azure OpenAI 클라이언트가 초기화되지 않았습니다.",
                    "check_type": check_type
                }
            
            response = await self.azure_client.client.chat.completions.create(
                model=self.azure_client.deployment_name,
                messages=[
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": user_prompt}
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
            logger.error(f"JSON 파싱 오류 ({check_type}): {str(e)}")
            return {
                "status": "error",
                "message": f"응답 파싱 오류: {str(e)}",
                "check_type": check_type
            }
            
        except Exception as e:
            logger.error(f"Azure OpenAI 호출 오류 ({check_type}): {str(e)}")
            return {
                "status": "error",
                "message": f"API 호출 실패: {str(e)}",
                "check_type": check_type
            }
    
    def _post_process_result(self, result: Dict[str, Any], check_type: str) -> Dict[str, Any]:
        """
        검사 결과 후처리
        
        Args:
            result: 원본 결과
            check_type: 검사 유형
            
        Returns:
            Dict: 후처리된 결과
        """
        try:
            if "result" in result and isinstance(result["result"], dict):
                errors = result["result"].get("errors", [])
                
                # 오류 우선순위 설정
                for error in errors:
                    error["priority"] = self._calculate_error_priority(error, check_type)
                
                # 오류를 우선순위별로 정렬
                errors.sort(key=lambda x: x.get("priority", 0), reverse=True)
                
                # 요약 정보 추가
                result["summary"] = {
                    "total_errors": len(errors),
                    "high_priority_errors": len([e for e in errors if e.get("priority", 0) >= 0.8]),
                    "check_type": check_type,
                    "description": result["result"].get("summary", "")
                }
            
            return result
            
        except Exception as e:
            logger.error(f"결과 후처리 오류 ({check_type}): {str(e)}")
            return result
    
    def _calculate_error_priority(self, error: Dict[str, Any], check_type: str) -> float:
        """
        오류 우선순위 계산
        
        Args:
            error: 오류 정보
            check_type: 검사 유형
            
        Returns:
            float: 우선순위 점수 (0.0 ~ 1.0)
        """
        base_confidence = error.get("confidence", 0.5)
        
        # 검사 유형별 가중치
        type_weights = {
            "grammar": 0.9,
            "korean_spelling": 0.8,
            "english_spelling": 0.7,
            "terminology_consistency": 0.6,
            "layout_consistency": 0.5
        }
        
        type_weight = type_weights.get(check_type, 0.5)
        
        return min(base_confidence * type_weight, 1.0)
    
    def _structure_to_text(self, structure: Dict[str, Any]) -> str:
        """
        문서 구조를 텍스트로 변환
        
        Args:
            structure: 문서 구조 정보
            
        Returns:
            str: 구조 정보 텍스트
        """
        try:
            text_parts = []
            
            if "headings" in structure:
                text_parts.append("=== 제목 구조 ===")
                for heading in structure["headings"]:
                    level = heading.get("level", 1)
                    text = heading.get("text", "")
                    style = heading.get("style", "")
                    text_parts.append(f"레벨 {level}: {text} (스타일: {style})")
            
            if "styles" in structure:
                text_parts.append("\n=== 스타일 정보 ===")
                for style_name, count in structure["styles"].items():
                    text_parts.append(f"{style_name}: {count}회 사용")
            
            return "\n".join(text_parts)
            
        except Exception as e:
            logger.error(f"구조 텍스트 변환 오류: {str(e)}")
            return str(structure)
    
    def _generate_comprehensive_summary(self, results: Dict[str, Any], file_path: str) -> Dict[str, Any]:
        """
        종합 검사 결과 요약 생성
        
        Args:
            results: 각 검사별 결과
            file_path: 파일 경로
            
        Returns:
            Dict: 종합 요약 결과
        """
        try:
            total_errors = 0
            priority_issues = []
            recommendations = []
            
            for check_type, result in results.items():
                if result.get("status") == "success" and "result" in result:
                    errors = result["result"].get("errors", [])
                    total_errors += len(errors)
                    
                    # 높은 우선순위 문제 수집
                    high_priority = [e for e in errors if e.get("priority", 0) >= 0.8]
                    priority_issues.extend(high_priority[:3])  # 상위 3개만
                    
                    # 검사별 권장사항 생성
                    if errors:
                        recommendations.append(f"{check_type}: {len(errors)}개 문제 발견 - 수정 권장")
            
            # 전체 점수 계산 (100점 만점)
            max_possible_errors = 50  # 가정된 최대 오류 수
            overall_score = max(0, (max_possible_errors - total_errors) / max_possible_errors * 100)
            
            return {
                "status": "success",
                "file_path": file_path,
                "total_checks": len(results),
                "results": results,
                "summary": {
                    "total_errors": total_errors,
                    "priority_issues": priority_issues[:5],  # 상위 5개
                    "overall_score": round(overall_score, 1),
                    "recommendations": recommendations
                }
            }
            
        except Exception as e:
            logger.error(f"종합 요약 생성 오류: {str(e)}")
            return {
                "status": "error",
                "message": f"종합 요약 생성 실패: {str(e)}",
                "file_path": file_path
            }