# 에러 처리 및 사용자 피드백 시스템 가이드

Task 10에서 구현된 종합적인 에러 처리 및 사용자 피드백 시스템에 대한 상세 가이드입니다.

## 📋 구현 개요

### 구현된 기능
1. **백엔드 에러 처리 미들웨어** - 표준화된 에러 응답 및 로깅
2. **프론트엔드 에러 표시** - 사용자 친화적인 에러 메시지 및 해결 방법 안내
3. **실시간 진행률 추적** - 장시간 작업의 진행 상황 실시간 표시
4. **자동 에러 복구** - 다양한 에러 유형에 대한 자동 복구 시스템
5. **사용자 피드백** - 로딩 상태, 진행률, 알림 시스템

## 🔧 백엔드 에러 처리 시스템

### 1. 에러 처리 미들웨어 (`ErrorHandlingMiddleware`)

**위치**: `backend/app/middleware/error_handler.py`

**주요 기능**:
- 모든 예외를 캐치하고 표준화된 응답 생성
- 사용자 친화적 메시지로 자동 변환
- 에러 ID 생성 및 상세 로깅
- 응답 시간 측정 및 헤더 추가

**에러 유형별 처리**:
```python
# 커스텀 애플리케이션 예외
DocumentCheckerException -> 사용자 정의 에러 코드 및 메시지

# FastAPI HTTPException
HTTPException -> HTTP 상태 코드 기반 처리

# 예상치 못한 예외
Exception -> 내부 서버 오류로 처리, 민감 정보 숨김
```

### 2. 커스텀 예외 클래스

**위치**: `backend/app/core/exceptions.py`

**주요 예외 클래스**:
- `FileValidationError` - 파일 검증 오류 (400)
- `FileProcessingError` - 파일 처리 오류 (422)
- `FileNotFoundError` - 파일 없음 (404)
- `AzureOpenAIError` - AI 서비스 오류 (503)
- `RateLimitError` - 요청 한도 초과 (429)
- `ValidationError` - 입력 검증 오류 (400)

### 3. 진행률 추적 미들웨어 (`ProgressTrackingMiddleware`)

**위치**: `backend/app/middleware/progress_tracker.py`

**주요 기능**:
- 장시간 작업의 진행 상황 추적
- 실시간 진행률 업데이트
- 예상 남은 시간 계산
- 작업 완료/실패 상태 관리

**사용 예시**:
```python
# 진행률 추적 시작
await start_task_progress("task_123", 5, "문서 검사")

# 진행률 업데이트
await update_task_progress("task_123", 2, "맞춤법 검사", "한국어 맞춤법을 검사하고 있습니다...")

# 진행률 완료
await complete_task_progress("task_123", "검사가 완료되었습니다")
```

### 4. 진행률 조회 API

**위치**: `backend/app/api/endpoints/progress.py`

**엔드포인트**:
- `GET /api/progress/{task_id}` - 진행률 조회
- `GET /api/progress` - 활성 작업 목록
- `DELETE /api/progress/{task_id}` - 작업 취소

## 🎨 프론트엔드 에러 처리 시스템

### 1. 에러 표시 컴포넌트 (`ErrorDisplayComponent`)

**위치**: `frontend/components/error_display.py`

**주요 기능**:
- 에러 유형별 아이콘 및 색상 표시
- 해결 방법 자동 제안
- 세부 정보 표시 (선택적)
- 재시도 및 리셋 버튼 제공

**에러 유형별 스타일**:
```python
ERROR_STYLES = {
    "FILE_VALIDATION_ERROR": {"icon": "📁", "color": "orange"},
    "AZURE_OPENAI_ERROR": {"icon": "🤖", "color": "red"},
    "NETWORK_ERROR": {"icon": "🌐", "color": "red"},
    # ... 기타 에러 유형
}
```

### 2. 에러 복구 시스템 (`ErrorRecoveryManager`)

**위치**: `frontend/components/error_recovery.py`

**복구 전략**:
- `RETRY` - 자동 재시도 (지수 백오프)
- `FALLBACK` - 대체 방법 사용
- `USER_ACTION` - 사용자 개입 필요
- `RESET` - 세션 초기화
- `ABORT` - 작업 중단

**에러 심각도**:
- `LOW` - 자동 복구 가능
- `MEDIUM` - 사용자 개입 필요
- `HIGH` - 즉시 중단 필요
- `CRITICAL` - 시스템 오류

### 3. 실시간 진행률 표시 (`RealTimeProgressTracker`)

**위치**: `frontend/components/realtime_progress.py`

**주요 기능**:
- 백엔드 진행률 실시간 조회
- 시각적 진행률 바 및 상태 표시
- 예상 남은 시간 계산
- 취소 버튼 및 사용자 제어

### 4. 로딩 상태 표시 (`LoadingDisplayComponent`)

**위치**: `frontend/components/loading_display.py`

**제공 기능**:
- 단계별 진행 상황 표시
- 파일 업로드 진행률
- 문서 검사 진행률
- 인터랙티브 로딩 컴포넌트

## 🔄 API 클라이언트 에러 처리

### 향상된 API 클라이언트 (`APIClient`)

**위치**: `frontend/services/api_client.py`

**에러 처리 기능**:
- 자동 재시도 (지수 백오프)
- 네트워크 오류 분류
- 타임아웃 처리
- 상세 에러 정보 제공

**재시도 로직**:
```python
# 최대 3회 재시도, 지수 백오프
for attempt in range(self.max_retries + 1):
    try:
        return func(*args, **kwargs)
    except (ConnectionError, Timeout) as e:
        if attempt < self.max_retries:
            delay = self.retry_delay * (2 ** attempt)
            time.sleep(delay)
```

## 📊 사용자 피드백 시스템

### 1. 알림 관리자 (`ProgressNotificationManager`)

**기능**:
- 실시간 알림 표시
- 알림 히스토리 관리
- 알림 유형별 스타일링

### 2. 진행률 피드백

**제공 정보**:
- 현재 진행 단계
- 전체 진행률 (%)
- 경과 시간
- 예상 남은 시간
- 현재 작업 설명

### 3. 상태 표시

**상태 유형**:
- `시작됨` - 작업 시작
- `진행 중` - 작업 수행 중
- `완료됨` - 작업 성공 완료
- `실패됨` - 작업 실패
- `취소됨` - 사용자 취소

## 🚀 사용 방법

### 백엔드에서 에러 발생 시

```python
# 커스텀 예외 발생
raise FileValidationError(
    message="지원하지 않는 파일 형식입니다",
    details={"file_type": "pdf", "supported_types": [".docx"]}
)

# 진행률 추적과 함께 작업 수행
task_id = f"check_{file_id}"
await start_task_progress(task_id, 4, "문서 검사")

try:
    await update_task_progress(task_id, 1, "파일 처리", "텍스트를 추출하고 있습니다...")
    # 작업 수행
    await complete_task_progress(task_id, "검사가 완료되었습니다")
except Exception as e:
    await fail_task_progress(task_id, f"오류 발생: {str(e)}")
    raise
```

### 프론트엔드에서 에러 처리

```python
# API 에러를 복구 시스템으로 처리
try:
    result = api_client.upload_file(file_content, filename)
except APIClientError as e:
    recovery_success = handle_api_error_with_recovery(
        error=e,
        operation_callback=retry_upload,
        reset_callback=reset_session
    )

# 실시간 진행률 추적
progress_result = track_document_check_progress(
    api_client=api_client,
    file_id=file_id,
    on_complete=lambda data: handle_completion(data),
    on_error=lambda error: handle_error(error)
)
```

## 🧪 테스트

### 테스트 실행

```bash
python korean-document-checker/test_error_handling.py
```

### 테스트 항목

1. **백엔드 에러 처리** - 미들웨어, 예외 클래스, 에러 응답
2. **진행률 추적** - 시작, 업데이트, 완료 기능
3. **프론트엔드 에러 컴포넌트** - 표시, 복구, 실시간 추적
4. **API 클라이언트** - 에러 처리, 재시도 로직
5. **통합 테스트** - 백엔드-프론트엔드 호환성

## 📈 모니터링 및 로깅

### 로그 레벨

- `INFO` - 정상 작업 진행 상황
- `WARNING` - 복구 가능한 문제
- `ERROR` - 에러 발생 및 처리
- `CRITICAL` - 시스템 레벨 오류

### 에러 추적

- 고유 에러 ID 생성
- 상세 스택 트레이스 로깅
- 사용자 세션 정보 포함
- 에러 발생 빈도 추적

## 🔧 설정 및 커스터마이징

### 에러 메시지 커스터마이징

`error_handler.py`의 `get_user_friendly_message` 함수에서 에러 코드별 메시지 수정 가능

### 복구 전략 설정

`error_recovery.py`의 `RECOVERY_STRATEGIES`에서 에러 유형별 복구 전략 설정

### 진행률 업데이트 간격

`realtime_progress.py`의 `update_interval` 파라미터로 조정 (기본값: 2초)

## 🎯 향후 개선 사항

1. **메트릭 수집** - 에러 발생률, 복구 성공률 통계
2. **알림 시스템** - 이메일, 슬랙 등 외부 알림 연동
3. **에러 분석** - 에러 패턴 분석 및 예방 시스템
4. **사용자 피드백** - 에러 해결 만족도 조사
5. **자동 복구 개선** - 더 정교한 복구 전략 구현

## 📚 관련 파일

### 백엔드
- `app/middleware/error_handler.py` - 에러 처리 미들웨어
- `app/middleware/progress_tracker.py` - 진행률 추적
- `app/core/exceptions.py` - 커스텀 예외 클래스
- `app/api/endpoints/progress.py` - 진행률 API
- `app/models/response.py` - 응답 모델

### 프론트엔드
- `components/error_display.py` - 에러 표시 컴포넌트
- `components/error_recovery.py` - 에러 복구 시스템
- `components/realtime_progress.py` - 실시간 진행률
- `components/loading_display.py` - 로딩 상태 표시
- `services/api_client.py` - API 클라이언트

### 테스트
- `test_error_handling.py` - 종합 테스트 스크립트

---

이 시스템을 통해 사용자는 더 나은 경험을 얻을 수 있으며, 개발자는 에러를 효과적으로 추적하고 관리할 수 있습니다.