# 한국어 문서 검사기 (Korean Document Checker)

Azure OpenAI GPT-4.1 모델을 활용한 한국어 문서 맞춤법 및 품질 검사 AI 서비스입니다.

## 🚀 주요 기능

- **구문 검사**: 한국어 문법 오류 검출 및 수정 제안
- **맞춤법 검사**: 한국어 및 영어 맞춤법 오류 검출
- **문서 일관성 검사**: 제목 스타일, 레이아웃, 전문용어 일관성 분석
- **종합 보고서**: 우선순위별 수정 권장사항 제공
- **웹 인터페이스**: Streamlit 기반 사용자 친화적 UI

## 📋 시스템 요구사항

- Python 3.12 이상
- uv (Python 패키지 관리자)
- Azure OpenAI 계정 및 API 키
- 최소 4GB RAM
- 인터넷 연결

## ⚡ 빠른 시작

### 자동 설치 및 실행

#### Windows
```cmd
# 1. 자동 설치
setup.bat

# 2. 백엔드 시작
start_backend.bat

# 3. 새 터미널에서 프론트엔드 시작
start_frontend.bat
```

#### macOS/Linux
```bash
# 1. 자동 설치
chmod +x setup.sh && ./setup.sh

# 2. 백엔드 시작
./start_backend.sh

# 3. 새 터미널에서 프론트엔드 시작
./start_frontend.sh
```

### 웹 브라우저에서 접속
- 애플리케이션: http://localhost:8501
- API 문서: http://localhost:8000/docs

## 🛠️ 수동 설치 방법

### 1. uv 설치

#### Windows (PowerShell)
```powershell
powershell -c "irm https://astral.sh/uv/install.ps1 | iex"
```

#### macOS/Linux
```bash
curl -LsSf https://astral.sh/uv/install.sh | sh
```

### 2. 프로젝트 클론 및 설정

```bash
git clone <repository-url>
cd korean-document-checker
```

### 3. 백엔드 설정

```bash
cd backend
uv sync
```

### 4. 프론트엔드 설정

```bash
cd ../frontend
uv sync
```

## ⚙️ 환경 설정

### Azure OpenAI 설정

1. **Azure Portal에서 OpenAI 리소스 생성**
   - Azure Portal (https://portal.azure.com) 접속
   - "리소스 만들기" → "Azure OpenAI" 검색
   - 새 Azure OpenAI 리소스 생성

2. **GPT-4.1 모델 배포**
   - Azure OpenAI Studio 접속
   - "배포" 탭에서 새 배포 생성
   - 모델: `gpt-4` 또는 `gpt-4-turbo` 선택
   - 배포 이름 설정 (예: `gpt-4-deployment`)

3. **API 키 및 엔드포인트 확인**
   - Azure OpenAI 리소스 → "키 및 엔드포인트" 메뉴
   - API 키와 엔드포인트 URL 복사

### 환경 변수 설정

#### Windows
```cmd
set AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/
set AZURE_OPENAI_API_KEY=your-api-key-here
set AZURE_OPENAI_DEPLOYMENT_NAME=gpt-4-deployment
```

#### macOS/Linux
```bash
export AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/
export AZURE_OPENAI_API_KEY=your-api-key-here
export AZURE_OPENAI_DEPLOYMENT_NAME=gpt-4-deployment
```

#### .env 파일 사용 (권장)

백엔드 디렉터리에 `.env` 파일 생성:

```env
AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/
AZURE_OPENAI_API_KEY=your-api-key-here
AZURE_OPENAI_API_VERSION=2024-12-01-preview
AZURE_OPENAI_DEPLOYMENT_NAME=gpt-4-deployment
MAX_FILE_SIZE=10485760
TEMP_FILE_RETENTION_HOURS=1
```

## 🚀 실행 방법

### 개발 환경에서 실행

#### 1. 백엔드 서버 시작

```bash
cd backend
uv run uvicorn app.main:app --reload --host 0.0.0.0 --port 8000
```

#### 2. 프론트엔드 애플리케이션 시작 (새 터미널)

```bash
cd frontend
uv run streamlit run app.py --server.port 8501
```

#### 3. 웹 브라우저에서 접속

- 프론트엔드: http://localhost:8501
- 백엔드 API 문서: http://localhost:8000/docs

### 프로덕션 환경에서 실행

#### 백엔드 (Gunicorn 사용)

```bash
cd backend
uv run gunicorn app.main:app -w 4 -k uvicorn.workers.UvicornWorker --bind 0.0.0.0:8000
```

#### 프론트엔드

```bash
cd frontend
uv run streamlit run app.py --server.port 8501 --server.address 0.0.0.0
```

## 📖 사용법

### 1. 파일 업로드
- 웹 인터페이스에서 "파일 선택" 버튼 클릭
- .docx 형식의 한국어 문서 선택
- 파일 크기는 10MB 이하로 제한

### 2. 문서 검사 실행
- 파일 업로드 완료 후 "검사 시작" 버튼 클릭
- 검사 진행 상황을 실시간으로 확인

### 3. 결과 확인
- **구문 검사 결과**: 문법 오류 및 수정 제안
- **맞춤법 검사 결과**: 한국어/영어 맞춤법 오류
- **일관성 검사 결과**: 문서 형식 및 용어 일관성
- **종합 보고서**: 우선순위별 수정 권장사항

### 4. 결과 활용
- 각 오류에 대한 상세 설명 및 수정 제안 확인
- 우선순위가 높은 문제부터 순차적으로 수정
- 수정 후 재검사를 통한 품질 향상

## 🔧 API 사용법

### 파일 업로드

```bash
curl -X POST "http://localhost:8000/api/upload" \
  -H "Content-Type: multipart/form-data" \
  -F "file=@document.docx"
```

### 문서 검사

```bash
curl -X POST "http://localhost:8000/api/check/{file_id}" \
  -H "Content-Type: application/json"
```

## 🐛 문제 해결

### 일반적인 문제

#### 1. Azure OpenAI 연결 오류
```
Error: Authentication failed
```
**해결방법**: 
- API 키와 엔드포인트 URL 확인
- Azure OpenAI 리소스 상태 확인
- 네트워크 연결 상태 확인

#### 2. 파일 업로드 실패
```
Error: File format not supported
```
**해결방법**:
- .docx 형식 파일인지 확인
- 파일 크기가 10MB 이하인지 확인
- 파일이 손상되지 않았는지 확인

#### 3. 메모리 부족 오류
```
Error: Out of memory
```
**해결방법**:
- 더 작은 크기의 문서로 테스트
- 시스템 메모리 확인 및 증설
- 다른 애플리케이션 종료

### 로그 확인

#### 백엔드 로그
```bash
cd backend
uv run uvicorn app.main:app --log-level debug
```

#### 프론트엔드 로그
Streamlit 콘솔에서 실시간 로그 확인 가능

## 📁 프로젝트 구조

```
korean-document-checker/
├── backend/                 # FastAPI 백엔드
│   ├── app/
│   │   ├── main.py         # 메인 애플리케이션
│   │   ├── api/            # API 엔드포인트
│   │   ├── core/           # 핵심 설정 및 클라이언트
│   │   ├── services/       # 비즈니스 로직
│   │   └── models/         # 데이터 모델
│   └── pyproject.toml      # 백엔드 의존성
├── frontend/               # Streamlit 프론트엔드
│   ├── app.py             # 메인 애플리케이션
│   ├── components/        # UI 컴포넌트
│   ├── services/          # API 클라이언트
│   └── pyproject.toml     # 프론트엔드 의존성
├── README.md              # 이 파일
└── ERROR_HANDLING_GUIDE.md # 에러 처리 가이드
```

## 🤝 기여하기

1. 이 저장소를 포크합니다
2. 새 기능 브랜치를 생성합니다 (`git checkout -b feature/new-feature`)
3. 변경사항을 커밋합니다 (`git commit -am 'Add new feature'`)
4. 브랜치에 푸시합니다 (`git push origin feature/new-feature`)
5. Pull Request를 생성합니다

## 📄 라이선스

이 프로젝트는 MIT 라이선스 하에 배포됩니다.

## 📞 지원

문제가 발생하거나 질문이 있으시면 다음을 참조하세요:

- **이슈 트래커**: GitHub Issues
- **설치 및 사용법**: 이 README 파일
- **배포 가이드**: [DEPLOYMENT_GUIDE.md](DEPLOYMENT_GUIDE.md)
- **문제 해결**: [TROUBLESHOOTING.md](TROUBLESHOOTING.md)
- **에러 처리**: [ERROR_HANDLING_GUIDE.md](ERROR_HANDLING_GUIDE.md)

## 🔄 업데이트

### 의존성 업데이트

```bash
# 백엔드
cd backend
uv sync --upgrade

# 프론트엔드  
cd frontend
uv sync --upgrade
```

### 애플리케이션 업데이트

```bash
git pull origin main
cd backend && uv sync
cd ../frontend && uv sync
```