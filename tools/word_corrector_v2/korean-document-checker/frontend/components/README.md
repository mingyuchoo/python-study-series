# Frontend Components

이 디렉토리는 한국어 문서 검사 AI 서비스의 프론트엔드 UI 컴포넌트들을 포함합니다.

## 컴포넌트 목록

### 1. file_upload.py
파일 업로드 UI 컴포넌트
- 파일 선택 및 검증
- 백엔드 서버로 파일 업로드
- 업로드 상태 표시

### 2. result_display.py
검사 결과 표시 UI 컴포넌트
- 카테고리별 검사 결과 표시
- 우선순위별 문제점 강조
- 종합 품질 대시보드
- 시각화 차트 (Plotly 사용)
- 결과 내보내기 기능

### 3. sample_data.py
테스트용 샘플 데이터 생성기
- 다양한 시나리오의 검사 결과 생성
- 프론트엔드 개발 및 테스트 지원

## 주요 기능

### 검사 결과 표시 (result_display.py)

#### 1. 종합 대시보드
- 📊 전체 통계 카드
- 📈 카테고리별 문제점 분포 차트
- 🚨 우선순위별 문제점 차트
- 📈 품질 점수 게이지
- 📋 상세 통계 테이블
- 📈 분석 인사이트

#### 2. 상세 결과 표시
- 📝 카테고리별 탭 구성
- 🔍 개별 문제점 상세 정보
- 💡 수정 제안 및 설명
- 🎯 신뢰도 표시

#### 3. 우선순위 시스템
- 🔴 높음: 즉시 수정 필요
- 🟡 보통: 권장 수정
- 🟢 낮음: 선택적 수정

#### 4. 내보내기 기능
- 📄 텍스트 파일 다운로드
- 📊 요약 보고서 생성

## 사용법

### 기본 사용
```python
from components.result_display import render_check_results

# 검사 결과 렌더링
render_check_results(check_results_data)
```

### 종합 보고서만 표시
```python
from components.result_display import render_comprehensive_report

# 종합 보고서만 렌더링
render_comprehensive_report(check_results_data)
```

### 테스트용 샘플 데이터
```python
from components.sample_data import generate_sample_check_results

# 샘플 데이터 생성
sample_data = generate_sample_check_results("test.docx")
render_check_results(sample_data)
```

## 데이터 구조

### 검사 결과 데이터 형식
```python
{
    "file_id": "unique_file_id",
    "filename": "document.docx",
    "total_errors": 5,
    "overall_score": 78.5,
    "check_results": [
        {
            "check_type": "grammar",
            "errors_found": 2,
            "suggestions": [...],
            "summary": "검사 요약"
        }
    ],
    "priority_issues": [
        {
            "title": "문제점 제목",
            "priority": "high|medium|low",
            "location": "위치 정보",
            "current_text": "현재 텍스트",
            "suggested_text": "제안 텍스트",
            "explanation": "설명",
            "confidence": 95
        }
    ],
    "recommendations": ["권장사항1", "권장사항2"]
}
```

## 의존성

### 필수 패키지
- `streamlit>=1.28.0`: 웹 UI 프레임워크
- `plotly>=5.17.0`: 데이터 시각화
- `pandas>=2.1.0`: 데이터 처리

### 설치
```bash
cd frontend
uv sync
```

## 테스트

컴포넌트 테스트 실행:
```bash
python test_result_display.py
```

## 스타일링

### 색상 테마
- 구문 검사: `#FF6B6B` (빨강)
- 한국어 맞춤법: `#4ECDC4` (청록)
- 영어 맞춤법: `#45B7D1` (파랑)
- 레이아웃 일관성: `#96CEB4` (연두)
- 용어 일관성: `#FFEAA7` (노랑)

### 우선순위 색상
- 높음: `#FF4757` (빨강)
- 보통: `#FFA502` (주황)
- 낮음: `#2ED573` (초록)

## 확장 가능성

### 향후 추가 예정 기능
- 📊 검사 이력 트렌드 분석
- 🔄 실시간 검사 진행률 표시
- 📱 모바일 반응형 디자인
- 🌐 다국어 지원
- 📈 상세 통계 및 분석
- 💾 검사 결과 데이터베이스 저장

### 커스터마이징
컴포넌트는 모듈화되어 있어 쉽게 확장하거나 수정할 수 있습니다:
- 새로운 검사 유형 추가
- 커스텀 시각화 차트
- 추가 내보내기 형식
- 사용자 정의 테마