# 수학 문제 해결 멀티 에이전트 시스템

이 프로젝트는 Azure OpenAI, Python, AutoGen, Streamlit, SQLite를 이용하여 수학 문제를 해결하는 멀티 에이전트 웹 애플리케이션입니다. 여러 AI 에이전트가 협업하여 수학 문제를 분석하고, 해결하며, 실생활 예시를 제공합니다.

## 주요 기능

- **수학 문제 해결**: 다양한 수학 문제를 여러 AI 에이전트가 협업하여 해결
- **실생활 예시 생성**: 수학 개념의 실제 응용 사례 제공
- **인터넷 검색 통합**: Tavily API를 활용한 관련 정보 검색
- **해결 기록 저장**: SQLite 데이터베이스를 통한 문제 및 해답 관리

## 에이전트 구성

### 수학 문제 해결 에이전트
- **Math_Analyzer**: 수학 문제 분석 및 해결 방법 제안
- **Equation_Formulator**: 문제를 수학적 방정식으로 변환
- **Solution_Calculator**: 방정식을 풀고 계산
- **Solution_Verifier**: 해답의 정확성 검증
- **Explanation_Generator**: 해답을 이해하기 쉽게 설명

### 보조 에이전트
- **Example_Creator**: 수학 개념의 실생활 예시 생성
- **Internet_Searcher**: 인터넷에서 관련 정보 검색
- **User_Proxy**: 사용자를 대신하여 코드 실행 및 대화 관리

## 기술 스택

- **프론트엔드**: Streamlit
- **백엔드**: Python
- **데이터베이스**: SQLite
- **AI**: Azure OpenAI (GPT-4o), AutoGen
- **검색**: Tavily API

## 설치 및 실행

1. 필요한 패키지 설치:
   ```bash
   uv pip install -r requirements.txt
   ```

2. 환경 변수 설정:
   `.env` 파일에 다음 API 키를 설정합니다:
   ```
   # Azure OpenAI API Configuration
   AZURE_API_KEY=your_azure_api_key
   AZURE_API_BASE=your_azure_endpoint
   AZURE_API_MODEL=your_azure_model
   AZURE_API_VERSION=your_azure_api_version

   # Tavily API Configuration
   TAVILY_API_KEY=your_tavily_api_key
   ```

3. 애플리케이션 실행:
   ```bash
   streamlit run app.py
   ```

## 사용 방법

1. 웹 브라우저에서 애플리케이션에 접속합니다 (기본 URL: http://localhost:8501).
2. '문제 해결' 탭에서 수학 문제를 입력합니다.
3. 필요한 경우 인터넷 검색 옵션을 활성화합니다.
4. '문제 해결하기' 버튼을 클릭하여 에이전트들이 문제를 해결하도록 합니다.
5. 해답, 설명, 실생활 예시 및 에이전트 대화 기록을 확인합니다.
6. '문제 기록' 탭에서 이전에 해결한 문제들을 확인할 수 있습니다.

## 프로젝트 구조

```
math-solver-app/
├── app.py                # 메인 Streamlit 애플리케이션
├── agents/               # 에이전트 정의
│   ├── __init__.py
│   ├── math_solver.py    # 수학 문제 해결 에이전트
│   ├── example_creator.py # 실생활 예시 생성 에이전트
│   ├── search_agent.py   # Tavily 검색 에이전트
├── database/             # SQLite 데이터베이스
│   ├── __init__.py
│   ├── db_manager.py     # 데이터베이스 작업
├── utils/                # 유틸리티 함수
│   ├── __init__.py
│   ├── openai_config.py  # Azure OpenAI 설정
├── requirements.txt      # 프로젝트 의존성
├── .env                  # 환경 변수
└── README.md             # 프로젝트 문서
```
