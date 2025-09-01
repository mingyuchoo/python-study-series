# 마이크로소프트 워드 문서 검토 서비스

워드 문서(*.doc, *.docx)를 업로드하여 구문 오류, 맞춤법, 목차/제목 스타일 일관성을 자동으로 검토하는 웹 서비스입니다.

## 🚀 주요 기능

- **구문 오류 검사**: 괄호, 따옴표, 문장부호 불일치 검사
- **맞춤법 검사**: 한국어/영어 맞춤법 및 띄어쓰기 교정
- **스타일 일관성**: 제목 스타일, 폰트 사용 일관성 검사
- **문서 구조 분석**: 목차 구조 및 전체적인 문서 구성 분석
- **통계 정보**: 문자 수, 단어 수, 예상 읽기 시간 등

## 🛠️ 기술 스택

### Backend (FastAPI)
- **FastAPI**: REST API 서버
- **python-docx**: 워드 문서 파싱
- **mammoth**: 워드 문서 HTML 변환
- **pyspellchecker**: 영어 맞춤법 검사
- **hanspell**: 한국어 맞춤법 검사
- **PyKoSpacing**: 한국어 띄어쓰기 교정

### Frontend (Streamlit)
- **Streamlit**: 웹 인터페이스
- **requests**: API 통신
- **pandas**: 데이터 표시

## 📦 설치 및 실행

### 1. 백엔드 서버 실행

```bash
cd backend
pip install -r requirements.txt
python main.py
```

백엔드 서버가 `http://localhost:8000`에서 실행됩니다.

### 2. 프론트엔드 실행

새 터미널에서:

```bash
cd frontend
pip install -r requirements.txt
streamlit run app.py
```

웹 인터페이스가 `http://localhost:8501`에서 실행됩니다.

## 📋 사용 방법

1. 웹 브라우저에서 `http://localhost:8501` 접속
2. 워드 문서(.doc 또는 .docx) 파일 업로드
3. "문서 분석 시작" 버튼 클릭
4. 분석 결과 확인:
   - 구문 오류
   - 맞춤법 검사 결과
   - 스타일 일관성
   - 문서 구조
   - 통계 정보

## 🔧 API 엔드포인트

### POST /analyze-document/
워드 문서를 업로드하고 분석 결과를 반환합니다.

**Request:**
- `file`: 워드 문서 파일 (.doc, .docx)

**Response:**
```json
{
  "filename": "문서명.docx",
  "syntax_errors": [...],
  "spelling_errors": {
    "korean": [...],
    "english": [...],
    "spacing": [...]
  },
  "style_consistency": {...},
  "document_structure": {...},
  "statistics": {...},
  "recommendations": [...]
}
```

## 📝 검토 항목 상세

### 구문 오류
- 괄호 불일치 검사
- 따옴표 불일치 검사
- 문장 시작 대문자 사용 검사

### 맞춤법 검사
- 한국어 맞춤법 (hanspell 라이브러리 사용)
- 영어 맞춤법 (pyspellchecker 라이브러리 사용)
- 한국어 띄어쓰기 (PyKoSpacing 라이브러리 사용)

### 스타일 일관성
- 제목 스타일 사용 현황
- 폰트 일관성 검사
- 목차 구조 검증

### 문서 구조
- 문단 구성 분석
- 표 및 이미지 개수
- 전체적인 문서 구성

## ⚠️ 주의사항

- 한국어 맞춤법 검사는 네트워크 연결이 필요할 수 있습니다
- 대용량 문서의 경우 분석 시간이 오래 걸릴 수 있습니다
- 복잡한 서식이 적용된 문서의 경우 일부 기능이 제한될 수 있습니다

## 🤝 기여하기

버그 리포트나 기능 제안은 이슈로 등록해 주세요.