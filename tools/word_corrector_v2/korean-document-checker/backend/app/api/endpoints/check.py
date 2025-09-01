"""
문서 검사 API 엔드포인트
Azure OpenAI를 활용한 종합적인 문서 품질 검사
"""

import logging
from datetime import datetime
from typing import Dict, Any, List, Optional
from fastapi import APIRouter, HTTPException, Depends
from fastapi.responses import JSONResponse

from app.models.request import CheckRequest, ErrorResponse
from app.models.response import ComprehensiveReport, CheckResult, ErrorDetail
from app.services.file_manager import FileManager
from app.services.checker_service import CheckerService
from app.core.azure_client import get_azure_client
from app.core.config import get_settings
from app.middleware.progress_tracker import (
    start_task_progress, 
    update_task_progress, 
    complete_task_progress, 
    fail_task_progress
)

logger = logging.getLogger(__name__)
router = APIRouter()

# 전역 파일 매니저 인스턴스
file_manager = FileManager()


async def get_checker_service() -> CheckerService:
    """CheckerService 의존성 주입"""
    azure_client = await get_azure_client()
    return CheckerService(azure_client)


@router.post("/check/{file_id}", response_model=ComprehensiveReport)
async def check_document(
    file_id: str,
    request: Optional[CheckRequest] = None,
    checker_service: CheckerService = Depends(get_checker_service)
):
    """
    문서 검사 API 엔드포인트
    
    업로드된 문서에 대해 종합적인 품질 검사를 수행합니다.
    - 구문 검사 (문법 오류)
    - 한국어 맞춤법 검사
    - 영어 맞춤법 검사
    - 문서 일관성 검사
    
    Args:
        file_id: 업로드된 파일의 고유 식별자
        request: 검사 옵션 (선택사항)
        
    Returns:
        ComprehensiveReport: 종합 검사 결과
        
    Raises:
        HTTPException: 파일을 찾을 수 없거나 검사 실패 시
    """
    try:
        logger.info(f"문서 검사 시작: {file_id}")
        
        # 요청 파라미터 기본값 설정
        if request is None:
            request = CheckRequest(file_id=file_id, check_types=["all"])
        elif not request.check_types:
            request.check_types = ["all"]
        
        # 진행률 추적 시작
        task_id = f"check_{file_id}"
        check_types = request.check_types
        if "all" in check_types:
            check_types = ["grammar", "korean_spell", "english_spell", "consistency"]
        
        total_steps = len(check_types) + 2  # 파일 처리 + 검사들 + 보고서 생성
        await start_task_progress(task_id, total_steps, f"문서 검사: {file_id}")
        
        current_step = 0
        
        # 1단계: 파일 정보 확인
        current_step += 1
        await update_task_progress(task_id, current_step, "파일 확인", "업로드된 파일 정보를 확인하고 있습니다...")
        
        file_info = file_manager.get_file_info(file_id)
        if not file_info:
            await fail_task_progress(task_id, f"파일을 찾을 수 없음: {file_id}")
            logger.error(f"파일을 찾을 수 없음: {file_id}")
            raise HTTPException(
                status_code=404,
                detail=f"File not found: {file_id}"
            )
        
        # 2단계: 문서 처리 (텍스트 추출)
        current_step += 1
        await update_task_progress(task_id, current_step, "문서 처리", "문서에서 텍스트를 추출하고 구조를 분석하고 있습니다...")
        
        processing_result = file_manager.process_document(file_id)
        if not processing_result.get("success"):
            await fail_task_progress(task_id, f"문서 처리 실패: {processing_result.get('error')}")
            logger.error(f"문서 처리 실패: {processing_result.get('error')}")
            raise HTTPException(
                status_code=400,
                detail=f"Document processing failed: {processing_result.get('error')}"
            )
        
        document_summary = processing_result["document_summary"]
        content = document_summary.get("content", "")
        structure = document_summary.get("structure", {})
        
        if not content.strip():
            await fail_task_progress(task_id, f"문서에서 텍스트를 추출할 수 없음: {file_id}")
            logger.error(f"문서에서 텍스트를 추출할 수 없음: {file_id}")
            raise HTTPException(
                status_code=400,
                detail="No text content found in document"
            )
        
        logger.info(f"수행할 검사 유형: {check_types}")
        
        # 각 검사 수행
        check_results = []
        all_errors = []
        
        for i, check_type in enumerate(check_types):
            try:
                # 진행률 업데이트
                current_step += 1
                check_names = {
                    "grammar": "구문 검사",
                    "korean_spell": "한국어 맞춤법 검사", 
                    "english_spell": "영어 맞춤법 검사",
                    "consistency": "일관성 검사"
                }
                check_name = check_names.get(check_type, check_type)
                await update_task_progress(task_id, current_step, check_name, f"{check_name}를 수행하고 있습니다...")
                
                logger.info(f"{check_type} 검사 시작")
                
                if check_type == "grammar":
                    result = await checker_service.grammar_check(content)
                elif check_type == "korean_spell":
                    result = await checker_service.korean_spell_check(content)
                elif check_type == "english_spell":
                    result = await checker_service.english_spell_check(content)
                elif check_type == "consistency":
                    result = await checker_service.layout_consistency_check(structure)
                else:
                    logger.warning(f"알 수 없는 검사 유형: {check_type}")
                    continue
                
                # 결과 처리
                if result.get("status") == "success":
                    check_result = _process_check_result(result, check_type)
                    check_results.append(check_result)
                    
                    # 모든 오류를 수집
                    if "result" in result and "errors" in result["result"]:
                        errors = result["result"]["errors"]
                        for error in errors:
                            error_detail = _convert_to_error_detail(error, check_type)
                            all_errors.append(error_detail)
                    
                    logger.info(f"{check_type} 검사 완료: {check_result.errors_found}개 오류 발견")
                else:
                    logger.error(f"{check_type} 검사 실패: {result.get('message')}")
                    # 실패한 검사도 결과에 포함 (오류 0개로)
                    check_results.append(CheckResult(
                        check_type=check_type,
                        errors_found=0,
                        suggestions=[],
                        summary=f"{check_type} 검사 실패: {result.get('message', '알 수 없는 오류')}"
                    ))
                    
            except Exception as e:
                logger.error(f"{check_type} 검사 중 오류 발생: {str(e)}")
                check_results.append(CheckResult(
                    check_type=check_type,
                    errors_found=0,
                    suggestions=[],
                    summary=f"{check_type} 검사 중 오류 발생: {str(e)}"
                ))
        
        # 마지막 단계: 보고서 생성
        current_step += 1
        await update_task_progress(task_id, current_step, "보고서 생성", "검사 결과를 종합하여 보고서를 생성하고 있습니다...")
        
        # 우선순위 이슈 선별 (상위 10개)
        priority_issues = _get_priority_issues(all_errors, limit=10)
        
        # 전체 점수 계산
        overall_score = _calculate_overall_score(all_errors, len(content.split()))
        
        # 권장사항 생성
        recommendations = _generate_recommendations(check_results, all_errors)
        
        # 종합 보고서 생성
        comprehensive_report = ComprehensiveReport(
            file_id=file_id,
            filename=file_info["original_filename"],
            total_errors=len(all_errors),
            check_results=check_results,
            priority_issues=priority_issues,
            overall_score=overall_score,
            recommendations=recommendations,
            processing_time=datetime.now().isoformat()
        )
        
        # 진행률 완료
        await complete_task_progress(task_id, f"문서 검사 완료: 총 {len(all_errors)}개 오류 발견")
        
        logger.info(f"문서 검사 완료: {file_id}, 총 {len(all_errors)}개 오류 발견")
        return comprehensive_report
        
    except HTTPException:
        raise
    except Exception as e:
        # 진행률 실패 처리
        task_id = f"check_{file_id}"
        await fail_task_progress(task_id, f"문서 검사 중 오류 발생: {str(e)}")
        
        logger.error(f"문서 검사 중 예상치 못한 오류: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail=f"Document check failed: {str(e)}"
        )


def _process_check_result(result: Dict[str, Any], check_type: str) -> CheckResult:
    """
    개별 검사 결과를 CheckResult 모델로 변환
    
    Args:
        result: 검사 결과
        check_type: 검사 유형
        
    Returns:
        CheckResult: 변환된 결과
    """
    try:
        errors = result.get("result", {}).get("errors", [])
        suggestions = []
        
        for error in errors:
            error_detail = _convert_to_error_detail(error, check_type)
            suggestions.append(error_detail)
        
        summary = result.get("summary", {})
        if isinstance(summary, dict):
            summary_text = summary.get("description", f"{check_type} 검사 완료")
        else:
            summary_text = str(summary)
        
        return CheckResult(
            check_type=check_type,
            errors_found=len(errors),
            suggestions=suggestions,
            summary=summary_text
        )
        
    except Exception as e:
        logger.error(f"검사 결과 처리 오류 ({check_type}): {str(e)}")
        return CheckResult(
            check_type=check_type,
            errors_found=0,
            suggestions=[],
            summary=f"결과 처리 오류: {str(e)}"
        )


def _convert_to_error_detail(error: Dict[str, Any], check_type: str) -> ErrorDetail:
    """
    오류 정보를 ErrorDetail 모델로 변환
    
    Args:
        error: 오류 정보
        check_type: 검사 유형
        
    Returns:
        ErrorDetail: 변환된 오류 상세 정보
    """
    return ErrorDetail(
        location=error.get("location", "위치 정보 없음"),
        error_type=f"{check_type}: {error.get('error_type', '알 수 없는 오류')}",
        current_text=error.get("current_text", ""),
        suggested_text=error.get("suggested_text", ""),
        confidence=error.get("confidence", 0.5),
        explanation=error.get("explanation", "설명 없음")
    )


def _get_priority_issues(all_errors: List[ErrorDetail], limit: int = 10) -> List[ErrorDetail]:
    """
    우선순위가 높은 이슈들을 선별
    
    Args:
        all_errors: 모든 오류 리스트
        limit: 반환할 최대 개수
        
    Returns:
        List[ErrorDetail]: 우선순위 이슈 리스트
    """
    # 신뢰도 기준으로 정렬하여 상위 이슈 선별
    sorted_errors = sorted(all_errors, key=lambda x: x.confidence, reverse=True)
    return sorted_errors[:limit]


def _calculate_overall_score(all_errors: List[ErrorDetail], word_count: int) -> float:
    """
    전체 문서 품질 점수 계산
    
    Args:
        all_errors: 모든 오류 리스트
        word_count: 총 단어 수
        
    Returns:
        float: 품질 점수 (0.0-1.0)
    """
    if word_count == 0:
        return 0.0
    
    # 오류 밀도 계산 (오류 수 / 단어 수)
    error_density = len(all_errors) / word_count
    
    # 가중 오류 점수 계산 (신뢰도 고려)
    weighted_errors = sum(error.confidence for error in all_errors)
    weighted_density = weighted_errors / word_count
    
    # 점수 계산 (오류가 적을수록 높은 점수)
    # 기본 점수에서 오류 밀도에 따라 차감
    base_score = 1.0
    penalty = min(weighted_density * 10, 0.9)  # 최대 90% 차감
    
    score = max(base_score - penalty, 0.1)  # 최소 10% 보장
    return round(score, 2)


def _generate_recommendations(check_results: List[CheckResult], all_errors: List[ErrorDetail]) -> List[str]:
    """
    검사 결과를 바탕으로 권장사항 생성
    
    Args:
        check_results: 검사 결과 리스트
        all_errors: 모든 오류 리스트
        
    Returns:
        List[str]: 권장사항 리스트
    """
    recommendations = []
    
    # 오류 유형별 분석
    error_types = {}
    for error in all_errors:
        error_type = error.error_type.split(":")[0] if ":" in error.error_type else error.error_type
        error_types[error_type] = error_types.get(error_type, 0) + 1
    
    # 가장 많은 오류 유형에 대한 권장사항
    if error_types:
        most_common_type = max(error_types, key=error_types.get)
        count = error_types[most_common_type]
        
        if most_common_type == "grammar":
            recommendations.append(f"문법 오류가 {count}개 발견되었습니다. 문장 구조와 조사 사용을 점검해보세요.")
        elif most_common_type == "korean_spell":
            recommendations.append(f"한국어 맞춤법 오류가 {count}개 발견되었습니다. 맞춤법 검사기를 활용해보세요.")
        elif most_common_type == "english_spell":
            recommendations.append(f"영어 맞춤법 오류가 {count}개 발견되었습니다. 영어 단어의 철자를 확인해보세요.")
        elif most_common_type == "consistency":
            recommendations.append(f"일관성 문제가 {count}개 발견되었습니다. 용어와 문체의 통일성을 확인해보세요.")
    
    # 전체 오류 수에 따른 권장사항
    total_errors = len(all_errors)
    if total_errors == 0:
        recommendations.append("문서 품질이 우수합니다. 추가적인 수정이 필요하지 않습니다.")
    elif total_errors <= 5:
        recommendations.append("전반적으로 양호한 문서입니다. 발견된 소수의 오류만 수정하면 됩니다.")
    elif total_errors <= 15:
        recommendations.append("일부 수정이 필요한 문서입니다. 우선순위가 높은 오류부터 수정해보세요.")
    else:
        recommendations.append("상당한 수정이 필요한 문서입니다. 체계적인 검토와 수정을 권장합니다.")
    
    # 고신뢰도 오류에 대한 권장사항
    high_confidence_errors = [e for e in all_errors if e.confidence >= 0.8]
    if high_confidence_errors:
        recommendations.append(f"신뢰도가 높은 {len(high_confidence_errors)}개의 오류는 우선적으로 수정하시기 바랍니다.")
    
    return recommendations[:5]  # 최대 5개 권장사항 반환


@router.get("/check/{file_id}/status")
async def get_check_status(file_id: str):
    """
    문서 검사 상태 확인 엔드포인트
    
    Args:
        file_id: 파일 고유 식별자
        
    Returns:
        Dict: 파일 상태 정보
    """
    try:
        file_info = file_manager.get_file_info(file_id)
        if not file_info:
            raise HTTPException(
                status_code=404,
                detail=f"File not found: {file_id}"
            )
        
        return {
            "file_id": file_id,
            "original_filename": file_info["original_filename"],
            "upload_time": file_info["upload_time"].isoformat(),
            "processed": file_info.get("processed", False),
            "processing_time": file_info.get("processing_time", {}).isoformat() if file_info.get("processing_time") else None,
            "ready_for_check": file_info.get("processed", False)
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"상태 확인 중 오류: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail=f"Status check failed: {str(e)}"
        )