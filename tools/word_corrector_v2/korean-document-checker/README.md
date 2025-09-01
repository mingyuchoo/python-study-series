# Korean Document Checker

한국어 문서 맞춤법 및 품질 검사 AI 서비스

## 프로젝트 구조

```
korean-document-checker/
├── backend/                 # FastAPI 백엔드 서비스
│   ├── app/
│   │   ├── api/            # API 엔드포인트
│   │   ├── core/           # 핵심 설정 및 유틸리티
│   │   ├── services/       # 비즈니스 로직 서비스
│   │   └── models/         # 데이터 모델
│   └── pyproject.toml
├── frontend/               # Streamlit 프론트엔드
│   ├── components/         # UI 컴포넌트
│   ├── services/          # API 클라이언트
│   └── pyproject.toml
└── README.md
```

## 요구사항

- Python 3.12+
- uv (Python 패키지 관리자)

## 설치 및 실행

### 1. uv 설치

```bash
# Windows (PowerShell)
powershell -c "irm https://astral.sh/uv/install.ps1 | iex"

# macOS/Linux
curl -LsSf https://astral.sh/uv/install.sh | sh
```

### 2. 백엔드 설정

```bash
cd backend
uv sync
```

### 3. 프론트엔드 설정

```bash
cd frontend
uv sync
```

### 4. 환경 변수 설정

백엔드 디렉터리에 `.env` 파일을 생성하고 다음 변수들을 설정하세요:

```
AZURE_OPENAI_ENDPOINT=your_azure_openai_endpoint
AZURE_OPENAI_API_KEY=your_api_key
AZURE_OPENAI_API_VERSION=2024-12-01-preview
AZURE_OPENAI_DEPLOYMENT_NAME=gpt-4.1
```

## 기능

- 한국어 docx 파일 업로드
- 구문 검사 (문법 오류 검출)
- 맞춤법 검사 (한국어/영어)
- 문서 일관성 검사 (레이아웃, 용어)
- 종합 보고서 생성

## 기술 스택

- **백엔드**: FastAPI, Python 3.12
- **프론트엔드**: Streamlit
- **AI**: Azure OpenAI GPT-4.1
- **문서 처리**: python-docx
- **패키지 관리**: uv