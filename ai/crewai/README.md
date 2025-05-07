# CrewAI 멀티에이전트 PPT 보고서 생성기

이 프로젝트는 CrewAI를 활용한 멀티에이전트 협업 시스템으로, 여러 AI 에이전트가 각자의 역할을 맡아 협력하며 PowerPoint 업무 보고서를 자동으로 생성합니다.

## 주요 기능

- **역할 기반 멀티에이전트 협업**: 데이터 분석가, 콘텐츠 작성자, 시각적 디자이너, 품질 검토자 등 여러 전문 에이전트가 협업
- **자동 데이터 분석**: 제공된 데이터를 분석하여 핵심 인사이트와 트렌드 추출
- **전문적인 콘텐츠 생성**: 비즈니스 보고서에 적합한 전문적이고 구조화된 콘텐츠 작성
- **시각적 요소 디자인**: 데이터 시각화 및 전문적인 프레젠테이션 디자인
- **품질 검토**: 최종 보고서의 정확성, 일관성 및 전문성 보장
- **PowerPoint 생성**: 분석 결과와 콘텐츠를 바탕으로 전문적인 PowerPoint 프레젠테이션 자동 생성

## 설치 방법

1. 필요한 패키지 설치:

```bash
pip install -r requirements.txt
```

2. OpenAI API 키 설정:

`.env` 파일을 생성하고 다음 내용을 추가하세요:

```
OPENAI_API_KEY=your_api_key_here
```

## 사용 방법

### 명령줄 인터페이스 (CLI)

가장 간단한 방법은 `cli.py` 스크립트를 사용하는 것입니다:

```bash
python cli.py --topic "분기별 영업 실적" --output "my_reports"
```

#### 옵션:

- `--topic`, `-t`: 보고서 주제 (필수)
- `--data`, `-d`: 분석할 데이터 소스 파일 경로 (선택 사항)
- `--output`, `-o`: 출력 파일을 저장할 디렉토리 (기본값: "output")

### 프로그래밍 방식 사용

```python
from enhanced_app import generate_business_report

# 보고서 생성
output_file = generate_business_report(
    report_topic="분기별 영업 실적",
    data_sources=["path/to/data1.csv", "path/to/data2.csv"],
    output_dir="my_reports"
)

print(f"보고서가 생성되었습니다: {output_file}")
```

## 샘플 데이터

데이터 소스를 제공하지 않으면, 시스템은 자동으로 다음과 같은 샘플 데이터를 생성합니다:

- 분기별 매출 데이터
- 시장 동향 분석
- 고객 피드백 요약

## 프로젝트 구조

- `app.py`: 기본 CrewAI 멀티에이전트 시스템
- `enhanced_app.py`: 향상된 기능을 갖춘 멀티에이전트 시스템
- `cli.py`: 명령줄 인터페이스
- `data_generator.py`: 샘플 데이터 생성 도구
- `ppt_generator.py`: PowerPoint 프레젠테이션 생성 도구
- `main.py`: 간단한 명령줄 인터페이스

## 에이전트 역할

1. **데이터 분석가**: 데이터를 분석하여 핵심 인사이트와 트렌드를 추출합니다.
2. **콘텐츠 작성자**: 분석 결과를 바탕으로 전문적인 보고서 내용을 작성합니다.
3. **시각적 디자이너**: 데이터 시각화와 프레젠테이션 디자인을 담당합니다.
4. **품질 검토자**: 최종 보고서의 정확성, 일관성 및 전문성을 검토합니다.

## 요구사항

- Python 3.10 이상, 3.13 미만
- OpenAI API 키
- 필요한 패키지 (requirements.txt 참조)

## 라이선스

MIT
