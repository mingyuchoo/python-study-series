#!/usr/bin/env python3
"""
결과 표시 컴포넌트 테스트 스크립트

이 스크립트는 결과 표시 컴포넌트가 올바르게 작동하는지 확인합니다.
"""

import sys
import os

# 현재 디렉토리를 Python 경로에 추가
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

def test_imports():
    """필요한 모듈들이 올바르게 import되는지 테스트"""
    try:
        from components.result_display import ResultDisplayComponent, render_check_results
        from components.sample_data import generate_sample_check_results
        print("✅ 모든 모듈 import 성공")
        return True
    except ImportError as e:
        print(f"❌ Import 오류: {e}")
        return False

def test_sample_data_generation():
    """샘플 데이터 생성 테스트"""
    try:
        from components.sample_data import (
            generate_sample_check_results,
            generate_perfect_document_result,
            generate_problematic_document_result
        )
        
        # 일반 샘플 데이터
        sample_data = generate_sample_check_results("test.docx")
        assert 'filename' in sample_data
        assert 'check_results' in sample_data
        assert 'total_errors' in sample_data
        print("✅ 일반 샘플 데이터 생성 성공")
        
        # 완벽한 문서 데이터
        perfect_data = generate_perfect_document_result("perfect.docx")
        assert perfect_data['total_errors'] == 0
        assert perfect_data['overall_score'] > 90
        print("✅ 완벽한 문서 데이터 생성 성공")
        
        # 문제 많은 문서 데이터
        problematic_data = generate_problematic_document_result("problem.docx")
        assert problematic_data['total_errors'] > 10
        assert problematic_data['overall_score'] < 60
        print("✅ 문제 많은 문서 데이터 생성 성공")
        
        return True
    except Exception as e:
        print(f"❌ 샘플 데이터 생성 오류: {e}")
        return False

def test_component_initialization():
    """컴포넌트 초기화 테스트"""
    try:
        from components.result_display import ResultDisplayComponent
        
        component = ResultDisplayComponent()
        
        # 설정 확인
        assert hasattr(component, 'CHECK_TYPE_CONFIG')
        assert hasattr(component, 'PRIORITY_CONFIG')
        
        # 설정 내용 확인
        assert 'grammar' in component.CHECK_TYPE_CONFIG
        assert 'high' in component.PRIORITY_CONFIG
        
        print("✅ 컴포넌트 초기화 성공")
        return True
    except Exception as e:
        print(f"❌ 컴포넌트 초기화 오류: {e}")
        return False

def test_data_structure():
    """데이터 구조 검증 테스트"""
    try:
        from components.sample_data import generate_sample_check_results
        
        data = generate_sample_check_results("test.docx")
        
        # 필수 필드 확인
        required_fields = [
            'file_id', 'filename', 'total_errors', 'check_results',
            'priority_issues', 'overall_score', 'recommendations'
        ]
        
        for field in required_fields:
            assert field in data, f"필수 필드 '{field}'가 없습니다"
        
        # check_results 구조 확인
        for result in data['check_results']:
            assert 'check_type' in result
            assert 'errors_found' in result
            assert 'suggestions' in result
            assert 'summary' in result
        
        # priority_issues 구조 확인
        for issue in data['priority_issues']:
            assert 'title' in issue
            assert 'priority' in issue
            assert issue['priority'] in ['high', 'medium', 'low']
        
        print("✅ 데이터 구조 검증 성공")
        return True
    except Exception as e:
        print(f"❌ 데이터 구조 검증 오류: {e}")
        return False

def main():
    """메인 테스트 함수"""
    print("🧪 결과 표시 컴포넌트 테스트 시작")
    print("=" * 50)
    
    tests = [
        ("모듈 Import", test_imports),
        ("샘플 데이터 생성", test_sample_data_generation),
        ("컴포넌트 초기화", test_component_initialization),
        ("데이터 구조 검증", test_data_structure)
    ]
    
    passed = 0
    total = len(tests)
    
    for test_name, test_func in tests:
        print(f"\n🔍 {test_name} 테스트 중...")
        if test_func():
            passed += 1
        else:
            print(f"❌ {test_name} 테스트 실패")
    
    print("\n" + "=" * 50)
    print(f"📊 테스트 결과: {passed}/{total} 통과")
    
    if passed == total:
        print("🎉 모든 테스트 통과!")
        return 0
    else:
        print("❌ 일부 테스트 실패")
        return 1

if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)