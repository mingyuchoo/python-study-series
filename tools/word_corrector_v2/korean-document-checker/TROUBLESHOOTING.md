# 문제 해결 가이드 (Troubleshooting Guide)

이 문서는 한국어 문서 검사기 사용 중 발생할 수 있는 일반적인 문제들과 해결 방법을 제공합니다.

## 📋 목차

1. [설치 관련 문제](#설치-관련-문제)
2. [Azure OpenAI 연결 문제](#azure-openai-연결-문제)
3. [파일 업로드 문제](#파일-업로드-문제)
4. [서버 실행 문제](#서버-실행-문제)
5. [성능 관련 문제](#성능-관련-문제)
6. [에러 메시지별 해결책](#에러-메시지별-해결책)

## 🛠️ 설치 관련 문제

### uv 설치 실패

**문제**: `uv` 명령어를 찾을 수 없음
```
'uv' is not recognized as an internal or external command
```

**해결책**:
1. **Windows**:
   ```powershell
   powershell -c "irm https://astral.sh/uv/install.ps1 | iex"
   ```
   
2. **macOS/Linux**:
   ```bash
   curl -LsSf https://astral.sh/uv/install.sh | sh
   source ~/.bashrc
   ```

3. **수동 설치**:
   - [uv 공식 설치 가이드](https://docs.astral.sh/uv/getting-started/installation/) 참조

### Python 버전 호환성 문제

**문제**: Python 3.12를 찾을 수 없음
```
Python 3.12 not found
```

**해결책**:
1. **Python 3.12 설치 확인**:
   ```bash
   python --version
   python3.12 --version
   ```

2. **Python 3.12 설치**:
   - **Windows**: [Python 공식 사이트](https://www.python.org/downloads/)에서 다운로드
   - **macOS**: `brew install python@3.12`
   - **Ubuntu**: `sudo apt install python3.12`

### 의존성 설치 실패

**문제**: 패키지 설치 중 오류 발생
```
Failed to install dependencies
```

**해결책**:
1. **캐시 정리**:
   ```bash
   uv cache clean
   ```

2. **가상환경 재생성**:
   ```bash
   rm -rf .venv
   uv sync
   ```

3. **네트워크 문제 확인**:
   ```bash
   ping pypi.org
   ```

## 🔗 Azure OpenAI 연결 문제

### API 키 인증 실패

**문제**: 
```
Error: Authentication failed
HTTP 401: Unauthorized
```

**해결책**:
1. **API 키 확인**:
   - Azure Portal → Azure OpenAI 리소스 → "키 및 엔드포인트"
   - 올바른 API 키 복사 및 `.env` 파일 업데이트

2. **환경 변수 확인**:
   ```bash
   # .env 파일 내용 확인
   cat backend/.env
   ```

3. **API 키 형식 확인**:
   - 32자리 16진수 문자열인지 확인
   - 앞뒤 공백 제거

### 엔드포인트 URL 오류

**문제**:
```
Error: Invalid endpoint URL
```

**해결책**:
1. **엔드포인트 형식 확인**:
   ```
   올바른 형식: https://your-resource-name.openai.azure.com/
   잘못된 형식: https://your-resource-name.openai.azure.com/openai/
   ```

2. **리소스 이름 확인**:
   - Azure Portal에서 정확한 리소스 이름 확인

### 모델 배포 문제

**문제**:
```
Error: Deployment not found
```

**해결책**:
1. **배포 상태 확인**:
   - Azure OpenAI Studio → "배포" 탭
   - 배포가 "성공" 상태인지 확인

2. **배포 이름 확인**:
   - `.env` 파일의 `AZURE_OPENAI_DEPLOYMENT_NAME` 값과 실제 배포 이름 일치 확인

3. **모델 버전 확인**:
   - GPT-4 또는 GPT-4 Turbo 모델이 배포되어 있는지 확인

### 요청 한도 초과

**문제**:
```
Error: Rate limit exceeded
HTTP 429: Too Many Requests
```

**해결책**:
1. **요청 빈도 조절**:
   - 잠시 대기 후 재시도
   - 동시 요청 수 제한

2. **할당량 확인**:
   - Azure Portal에서 사용량 및 할당량 확인
   - 필요시 할당량 증가 요청

## 📁 파일 업로드 문제

### 지원하지 않는 파일 형식

**문제**:
```
Error: File format not supported
```

**해결책**:
1. **파일 확장자 확인**:
   - `.docx` 파일만 지원
   - `.doc`, `.pdf`, `.txt` 파일은 지원하지 않음

2. **파일 변환**:
   - Microsoft Word에서 `.docx` 형식으로 저장
   - LibreOffice Writer 사용 가능

### 파일 크기 제한 초과

**문제**:
```
Error: File size exceeds limit
```

**해결책**:
1. **파일 크기 확인**:
   - 기본 제한: 10MB
   - 파일 크기 줄이기 (이미지 압축, 불필요한 내용 제거)

2. **제한 설정 변경**:
   ```env
   # .env 파일에서 수정 (바이트 단위)
   MAX_FILE_SIZE=20971520  # 20MB로 증가
   ```

### 파일 손상 오류

**문제**:
```
Error: Cannot read document
```

**해결책**:
1. **파일 무결성 확인**:
   - Microsoft Word에서 파일 열기 테스트
   - 파일 복구 시도

2. **새 파일로 저장**:
   - Word에서 "다른 이름으로 저장" → `.docx` 형식

## 🖥️ 서버 실행 문제

### 포트 충돌

**문제**:
```
Error: Port 8000 is already in use
```

**해결책**:
1. **포트 사용 프로세스 확인**:
   ```bash
   # Windows
   netstat -ano | findstr :8000
   
   # macOS/Linux
   lsof -i :8000
   ```

2. **프로세스 종료**:
   ```bash
   # Windows
   taskkill /PID <PID> /F
   
   # macOS/Linux
   kill -9 <PID>
   ```

3. **다른 포트 사용**:
   ```bash
   uv run uvicorn app.main:app --port 8001
   ```

### 메모리 부족

**문제**:
```
Error: Out of memory
```

**해결책**:
1. **시스템 메모리 확인**:
   ```bash
   # Windows
   wmic OS get TotalVisibleMemorySize,FreePhysicalMemory
   
   # macOS/Linux
   free -h
   ```

2. **다른 애플리케이션 종료**:
   - 불필요한 프로그램 종료
   - 브라우저 탭 정리

3. **스왑 파일 생성** (Linux):
   ```bash
   sudo fallocate -l 2G /swapfile
   sudo chmod 600 /swapfile
   sudo mkswap /swapfile
   sudo swapon /swapfile
   ```

### 모듈 import 오류

**문제**:
```
ModuleNotFoundError: No module named 'app'
```

**해결책**:
1. **작업 디렉터리 확인**:
   ```bash
   pwd  # 현재 디렉터리 확인
   cd backend  # 백엔드 디렉터리로 이동
   ```

2. **가상환경 활성화 확인**:
   ```bash
   uv run python -c "import sys; print(sys.path)"
   ```

3. **의존성 재설치**:
   ```bash
   uv sync --reinstall
   ```

## ⚡ 성능 관련 문제

### 응답 시간 지연

**문제**: API 응답이 너무 느림

**해결책**:
1. **네트워크 연결 확인**:
   ```bash
   ping your-resource.openai.azure.com
   ```

2. **Azure 리전 확인**:
   - 가까운 리전의 Azure OpenAI 리소스 사용

3. **문서 크기 최적화**:
   - 큰 문서를 작은 단위로 분할
   - 불필요한 내용 제거

### CPU 사용률 높음

**문제**: 서버 CPU 사용률이 100%에 근접

**해결책**:
1. **워커 프로세스 수 조정**:
   ```bash
   uv run uvicorn app.main:app --workers 2
   ```

2. **시스템 리소스 모니터링**:
   ```bash
   # Windows
   taskmgr
   
   # macOS/Linux
   top
   htop
   ```

## 🚨 에러 메시지별 해결책

### `ConnectionError: Failed to connect to Azure OpenAI`

**원인**: 네트워크 연결 문제 또는 잘못된 엔드포인트

**해결책**:
1. 인터넷 연결 확인
2. 방화벽 설정 확인
3. 엔드포인트 URL 재확인
4. DNS 설정 확인

### `ValidationError: Invalid file format`

**원인**: 지원하지 않는 파일 형식

**해결책**:
1. `.docx` 파일인지 확인
2. 파일 확장자 변경이 아닌 실제 형식 변환
3. Microsoft Word에서 다시 저장

### `TimeoutError: Request timeout`

**원인**: Azure OpenAI API 응답 지연

**해결책**:
1. 네트워크 상태 확인
2. 문서 크기 줄이기
3. 재시도 로직 활용
4. 타임아웃 설정 증가

### `PermissionError: Access denied`

**원인**: 파일 또는 디렉터리 접근 권한 부족

**해결책**:
1. **Windows**:
   ```cmd
   # 관리자 권한으로 실행
   ```

2. **macOS/Linux**:
   ```bash
   sudo chown -R $USER:$USER .
   chmod -R 755 .
   ```

### `JSONDecodeError: Invalid JSON response`

**원인**: Azure OpenAI API 응답 파싱 오류

**해결책**:
1. API 응답 로그 확인
2. 모델 배포 상태 확인
3. API 버전 호환성 확인
4. 요청 형식 검증

## 🔍 디버깅 방법

### 로그 레벨 증가

```bash
# 상세 로그 출력
uv run uvicorn app.main:app --log-level debug
```

### 환경 변수 확인

```python
# Python에서 환경 변수 확인
import os
print("AZURE_OPENAI_ENDPOINT:", os.getenv("AZURE_OPENAI_ENDPOINT"))
print("AZURE_OPENAI_API_KEY:", os.getenv("AZURE_OPENAI_API_KEY")[:10] + "...")
```

### API 직접 테스트

```bash
# curl을 사용한 직접 API 테스트
curl -X POST "http://localhost:8000/api/health" \
  -H "Content-Type: application/json"
```

### 네트워크 연결 테스트

```bash
# Azure OpenAI 엔드포인트 연결 테스트
curl -v https://your-resource.openai.azure.com/
```

## 📞 추가 지원

위의 해결책으로 문제가 해결되지 않는 경우:

1. **로그 파일 확인**: `backend/logs/app.log`
2. **GitHub Issues**: 프로젝트 저장소의 Issues 탭
3. **Azure 지원**: Azure Portal의 지원 요청
4. **커뮤니티**: Stack Overflow, Reddit 등

## 🔄 시스템 재설정

모든 해결책이 실패한 경우 완전 재설정:

```bash
# 1. 가상환경 삭제
rm -rf backend/.venv frontend/.venv

# 2. 캐시 정리
uv cache clean

# 3. 재설치
cd backend && uv sync
cd ../frontend && uv sync

# 4. 환경 변수 재설정
cp backend/.env.example backend/.env
# .env 파일 편집

# 5. 서비스 재시작
./start_backend.sh
./start_frontend.sh
```

이 가이드를 통해 대부분의 문제를 해결할 수 있습니다. 추가적인 도움이 필요한 경우 프로젝트 문서나 커뮤니티를 참조하세요.