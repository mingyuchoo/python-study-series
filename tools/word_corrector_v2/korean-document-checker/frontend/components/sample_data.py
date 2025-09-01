"""
샘플 데이터 생성기

백엔드가 준비되지 않은 상황에서 프론트엔드 테스트를 위한
샘플 검사 결과 데이터를 생성합니다.
"""

from typing import Dict, List, Any
from datetime import datetime
import random

def generate_sample_check_results(filename: str = "sample_document.docx") -> Dict[str, Any]:
    """
    샘플 검사 결과 데이터를 생성합니다.
    
    Args:
        filename: 샘플 파일명
        
    Returns:
        샘플 검사 결과 딕셔너리
    """
    
    # 샘플 수정 제안 생성
    def create_sample_suggestions(check_type: str, count: int) -> List[Dict[str, Any]]:
        suggestions = []
        
        sample_data = {
            'grammar': [
                {
                    'title': '주어와 서술어 불일치',
                    'location': '2페이지 3번째 문단',
                    'error_type': '문법 오류',
                    'current_text': '학생들이 공부를 열심히 한다.',
                    'suggested_text': '학생들이 공부를 열심히 한다.',
                    'explanation': '복수 주어에 맞는 서술어를 사용해야 합니다.',
                    'confidence': 95
                },
                {
                    'title': '조사 사용 오류',
                    'location': '1페이지 2번째 문단',
                    'error_type': '조사 오류',
                    'current_text': '책을 읽고 있습니다.',
                    'suggested_text': '책을 읽고 있습니다.',
                    'explanation': '목적격 조사 사용이 적절합니다.',
                    'confidence': 88
                }
            ],
            'korean_spell': [
                {
                    'title': '맞춤법 오류',
                    'location': '3페이지 1번째 문단',
                    'error_type': '맞춤법',
                    'current_text': '안녕하세요',
                    'suggested_text': '안녕하세요',
                    'explanation': '올바른 맞춤법입니다.',
                    'confidence': 99
                },
                {
                    'title': '띄어쓰기 오류',
                    'location': '2페이지 4번째 문단',
                    'error_type': '띄어쓰기',
                    'current_text': '그런데도',
                    'suggested_text': '그런데도',
                    'explanation': '부사는 붙여서 씁니다.',
                    'confidence': 92
                }
            ],
            'english_spell': [
                {
                    'title': '영어 철자 오류',
                    'location': '4페이지 2번째 문단',
                    'error_type': '영어 맞춤법',
                    'current_text': 'recieve',
                    'suggested_text': 'receive',
                    'explanation': 'i before e except after c 규칙을 적용합니다.',
                    'confidence': 100
                }
            ],
            'layout_consistency': [
                {
                    'title': '제목 스타일 불일치',
                    'location': '전체 문서',
                    'error_type': '레이아웃',
                    'current_text': '제목1: 굵게, 제목2: 기울임',
                    'suggested_text': '모든 제목을 굵게 통일',
                    'explanation': '제목 스타일을 일관되게 적용해야 합니다.',
                    'confidence': 85
                }
            ],
            'terminology_consistency': [
                {
                    'title': '용어 불일치',
                    'location': '5페이지 전체',
                    'error_type': '용어 통일',
                    'current_text': '인공지능, AI, 머신러닝',
                    'suggested_text': '인공지능(AI)로 통일',
                    'explanation': '동일한 개념에 대해 일관된 용어를 사용해야 합니다.',
                    'confidence': 78
                }
            ]
        }
        
        base_suggestions = sample_data.get(check_type, [])
        result = []
        
        for i in range(min(count, len(base_suggestions))):
            suggestion = base_suggestions[i].copy()
            result.append(suggestion)
        
        return result
    
    # 각 검사 유형별 결과 생성
    check_results = []
    total_errors = 0
    
    check_types = [
        ('grammar', '구문 검사', random.randint(0, 3)),
        ('korean_spell', '한국어 맞춤법', random.randint(0, 4)),
        ('english_spell', '영어 맞춤법', random.randint(0, 2)),
        ('layout_consistency', '레이아웃 일관성', random.randint(0, 2)),
        ('terminology_consistency', '용어 일관성', random.randint(0, 2))
    ]
    
    for check_type, check_name, error_count in check_types:
        suggestions = create_sample_suggestions(check_type, error_count)
        
        result = {
            'check_type': check_type,
            'errors_found': error_count,
            'suggestions': suggestions,
            'summary': f'{check_name} 검사에서 {error_count}개의 문제점을 발견했습니다.' if error_count > 0 else f'{check_name} 검사에서 문제점이 발견되지 않았습니다.'
        }
        
        check_results.append(result)
        total_errors += error_count
    
    # 우선순위 문제점 생성
    priority_issues = []
    
    if total_errors > 0:
        # 높은 우선순위 문제점
        if random.choice([True, False]):
            priority_issues.append({
                'title': '심각한 문법 오류',
                'location': '1페이지 첫 번째 문단',
                'check_type': 'grammar',
                'priority': 'high',
                'current_text': '잘못된 문법 구조',
                'suggested_text': '올바른 문법 구조',
                'explanation': '문장의 기본 구조가 잘못되어 의미 전달에 문제가 있습니다.',
                'confidence': 95
            })
        
        # 보통 우선순위 문제점
        for i in range(random.randint(1, 3)):
            priority_issues.append({
                'title': f'맞춤법 오류 {i+1}',
                'location': f'{i+2}페이지',
                'check_type': 'korean_spell',
                'priority': 'medium',
                'current_text': '틀린 맞춤법',
                'suggested_text': '올바른 맞춤법',
                'explanation': '일반적인 맞춤법 오류입니다.',
                'confidence': 88
            })
        
        # 낮은 우선순위 문제점
        if random.choice([True, False]):
            priority_issues.append({
                'title': '용어 통일 권장',
                'location': '전체 문서',
                'check_type': 'terminology_consistency',
                'priority': 'low',
                'current_text': '다양한 용어 사용',
                'suggested_text': '통일된 용어 사용',
                'explanation': '가독성 향상을 위해 용어를 통일하는 것이 좋습니다.',
                'confidence': 70
            })
    
    # 전체 품질 점수 계산 (문제점이 적을수록 높은 점수)
    max_possible_errors = 15  # 가정된 최대 오류 수
    error_penalty = (total_errors / max_possible_errors) * 40  # 최대 40점 감점
    overall_score = max(60, 100 - error_penalty)  # 최소 60점 보장
    
    # 권장사항 생성
    recommendations = []
    
    if total_errors == 0:
        recommendations.append("문서 품질이 매우 우수합니다. 추가 검토 없이 사용 가능합니다.")
    else:
        if any(issue['priority'] == 'high' for issue in priority_issues):
            recommendations.append("우선순위가 높은 문제점부터 수정하시기 바랍니다.")
        
        # 가장 문제가 많은 카테고리 찾기
        max_errors_category = max(check_results, key=lambda x: x['errors_found'])
        if max_errors_category['errors_found'] > 0:
            recommendations.append(f"{max_errors_category['check_type']} 영역에 특히 주의를 기울이시기 바랍니다.")
        
        recommendations.append("수정 후 재검사를 통해 개선 사항을 확인해보세요.")
        
        if overall_score < 70:
            recommendations.append("전반적인 문서 품질 향상을 위해 체계적인 검토가 필요합니다.")
    
    # 최종 결과 구성
    result = {
        'file_id': f"sample_{datetime.now().strftime('%Y%m%d_%H%M%S')}",
        'filename': filename,
        'total_errors': total_errors,
        'check_results': check_results,
        'priority_issues': priority_issues,
        'overall_score': round(overall_score, 1),
        'recommendations': recommendations,
        'check_time': datetime.now().isoformat(),
        'status': 'completed'
    }
    
    return result

def generate_perfect_document_result(filename: str = "perfect_document.docx") -> Dict[str, Any]:
    """
    완벽한 문서의 검사 결과를 생성합니다.
    
    Args:
        filename: 파일명
        
    Returns:
        완벽한 문서의 검사 결과
    """
    check_results = []
    
    check_types = ['grammar', 'korean_spell', 'english_spell', 'layout_consistency', 'terminology_consistency']
    
    for check_type in check_types:
        result = {
            'check_type': check_type,
            'errors_found': 0,
            'suggestions': [],
            'summary': f'{check_type} 검사에서 문제점이 발견되지 않았습니다.'
        }
        check_results.append(result)
    
    return {
        'file_id': f"perfect_{datetime.now().strftime('%Y%m%d_%H%M%S')}",
        'filename': filename,
        'total_errors': 0,
        'check_results': check_results,
        'priority_issues': [],
        'overall_score': 98.5,
        'recommendations': [
            "문서 품질이 매우 우수합니다.",
            "추가적인 수정 없이 사용 가능합니다.",
            "현재 수준을 유지하시기 바랍니다."
        ],
        'check_time': datetime.now().isoformat(),
        'status': 'completed'
    }

def generate_problematic_document_result(filename: str = "problematic_document.docx") -> Dict[str, Any]:
    """
    문제가 많은 문서의 검사 결과를 생성합니다.
    
    Args:
        filename: 파일명
        
    Returns:
        문제가 많은 문서의 검사 결과
    """
    # 각 카테고리에서 많은 문제점 생성
    check_results = []
    total_errors = 0
    
    check_configs = [
        ('grammar', 5),
        ('korean_spell', 8),
        ('english_spell', 3),
        ('layout_consistency', 4),
        ('terminology_consistency', 6)
    ]
    
    for check_type, error_count in check_configs:
        suggestions = []
        
        # 각 카테고리별로 여러 문제점 생성
        for i in range(error_count):
            suggestion = {
                'title': f'{check_type} 문제점 {i+1}',
                'location': f'{i+1}페이지',
                'error_type': check_type,
                'current_text': f'문제가 있는 텍스트 {i+1}',
                'suggested_text': f'수정된 텍스트 {i+1}',
                'explanation': f'{check_type} 관련 문제점입니다.',
                'confidence': random.randint(70, 95)
            }
            suggestions.append(suggestion)
        
        result = {
            'check_type': check_type,
            'errors_found': error_count,
            'suggestions': suggestions,
            'summary': f'{check_type} 검사에서 {error_count}개의 심각한 문제점을 발견했습니다.'
        }
        
        check_results.append(result)
        total_errors += error_count
    
    # 많은 우선순위 문제점 생성
    priority_issues = [
        {
            'title': '심각한 문법 구조 오류',
            'location': '1페이지 전체',
            'check_type': 'grammar',
            'priority': 'high',
            'current_text': '완전히 잘못된 문법',
            'suggested_text': '올바른 문법 구조',
            'explanation': '문장 구조가 완전히 잘못되어 있습니다.',
            'confidence': 98
        },
        {
            'title': '반복적인 맞춤법 오류',
            'location': '전체 문서',
            'check_type': 'korean_spell',
            'priority': 'high',
            'current_text': '여러 맞춤법 오류',
            'suggested_text': '올바른 맞춤법',
            'explanation': '동일한 맞춤법 오류가 반복됩니다.',
            'confidence': 95
        }
    ]
    
    # 낮은 품질 점수
    overall_score = 45.2
    
    recommendations = [
        "문서 전반에 걸쳐 체계적인 수정이 필요합니다.",
        "우선순위가 높은 문법 오류부터 수정하시기 바랍니다.",
        "맞춤법 검사기를 활용하여 기본적인 오류를 먼저 수정하세요.",
        "전문가의 검토를 받는 것을 권장합니다.",
        "수정 후 반드시 재검사를 실시하세요."
    ]
    
    return {
        'file_id': f"problematic_{datetime.now().strftime('%Y%m%d_%H%M%S')}",
        'filename': filename,
        'total_errors': total_errors,
        'check_results': check_results,
        'priority_issues': priority_issues,
        'overall_score': overall_score,
        'recommendations': recommendations,
        'check_time': datetime.now().isoformat(),
        'status': 'completed'
    }