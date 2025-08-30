# Travel AI Consultation API

해외여행 상담을 위한 AI 기반 FastAPI 서비스

## 설치 및 실행

### 1. 의존성 설치

```bash
uv pip install -r requirements.txt
```

### 2. 환경 변수 설정

`.env` 파일을 생성하고 필요한 설정을 입력하세요.

```bash
cp .env.example .env
```

프론트엔드는 기본적으로 `http://127.0.0.1:8000/api/v1`를 백엔드 주소로 사용합니다. 다른 주소/포트를 사용할 경우 다음 환경변수를 지정하세요.

```bash
# 예: 백엔드가 9000 포트에서 구동 중이라면
set BACKEND_BASE_URL=http://127.0.0.1:9000/api/v1
```

### 3. 데이터베이스 초기화

SQLite 데이터베이스를 생성하고 초기 데이터를 입력하세요.

### 4. 서버 실행

```bash
uv run uvicorn backend.main:backend --reload
```

### 5. 프론트엔드(Streamlit) 실행

```bash
# 프로젝트 루트에서 실행
uv run streamlit run frontend/app.py
```

사이드바에서 백엔드 URL을 확인/수정하고, 다음 흐름으로 이용하세요:

- 세션 생성 버튼으로 세션을 만든 후 세션 ID 확인
- 질문 가져오기 탭에서 질문 목록 조회
- 답변 제출 탭에서 질문 ID/답변 입력 후 저장
- 추천 받기 탭에서 추천 요청 (limit 조절 가능)
- 상품 검색 탭에서 키워드/필터로 검색
- 세션 답변 보기 탭에서 현재 세션의 답변 내역 확인

## API 문서

서버 실행 후 다음 URL에서 API 문서를 확인할 수 있습니다:

- Swagger UI: <http://localhost:8000/docs>
- ReDoc: <http://localhost:8000/redoc>

## 주요 엔드포인트

- `GET /api/v1/questions/` - 질문 목록 조회
- `POST /api/v1/sessions/create` - 세션 생성
- `POST /api/v1/sessions/answer` - 답변 제출
- `POST /api/v1/recommendations/` - 개인화 추천
- `POST /api/v1/recommendations/search` - 상품 검색
