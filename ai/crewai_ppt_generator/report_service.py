import os
from dotenv import load_dotenv
from langchain_openai import ChatOpenAI
from typing import Optional, List, Any

from crewai import Agent, Crew, Process, Task
from data_generator import save_sample_data
from ppt_generator import PPTGenerator

# Load environment variables
load_dotenv()

# Set up the LLM
# Set environment variables for Azure OpenAI - LiteLLM will use these directly
os.environ["AZURE_API_KEY"] = os.environ["OPENAI_API_KEY"]
os.environ["AZURE_API_BASE"] = os.environ["OPENAI_API_URL"]
os.environ["AZURE_API_VERSION"] = "2023-05-15"

# Create the LLM instance with Azure OpenAI model
# Format: azure/<deployment_name>
model_name = f"azure/{os.environ['OPENAI_API_MODEL']}"

llm = ChatOpenAI(
    model=model_name,
    temperature=float(os.environ.get("OPENAI_API_TEMPERATURE", 1.0)),
)


# Define the agents with specific roles
def create_agents() -> tuple[Agent, Agent, Agent, Agent]:
    """
    Create and return the agents for the CrewAI system

    Returns:
        tuple: Tuple containing all agent objects
    """
    data_analyst = Agent(
        role="데이터 분석가",
        goal="비즈니스 보고서를 위한 데이터를 분석하고 핵심 인사이트를 추출합니다",
        backstory="""당신은 비즈니스 인텔리전스 분야에서 수년간의 경험을 가진 전문 데이터 분석가입니다.
        복잡한 데이터에서 의미 있는 인사이트를 추출하여 명확하고 실행 가능한 형식으로 제시하는 것이 전문 분야입니다.
        다른 사람들이 놓칠 수 있는 트렌드와 패턴을 식별하는 재능이 있습니다.""",
        verbose=True,
        llm=llm,
        allow_delegation=True,
    )

    content_writer = Agent(
        role="콘텐츠 작성자",
        goal="비즈니스 보고서를 위한 설득력 있고 명확하며 간결한 콘텐츠를 작성합니다",
        backstory="""당신은 임원 요약, 상세 분석 섹션 및 실행 가능한 권장 사항을 작성하는 전문 지식을 갖춘 숙련된 비즈니스 작가입니다.
        당신의 글은 명확하고 전문적이며 대상 청중에 맞게 조정된 것으로 알려져 있습니다.""",
        verbose=True,
        llm=llm,
        allow_delegation=True,
    )

    visual_designer = Agent(
        role="시각적 디자이너",
        goal="시각적으로 매력적이고 유익한 차트, 그래프 및 슬라이드 레이아웃을 만듭니다",
        backstory="""당신은 데이터 시각화 및 프레젠테이션 디자인을 전문으로 하는 재능 있는 시각적 디자이너입니다.
        복잡한 데이터를 이해와 참여를 향상시키는 명확하고 설득력 있는 시각적 요소로 변환하는 방법을 알고 있습니다.
        디자인 원칙에 대한 강한 감각을 가지고 있으며 전문적인 비즈니스 프레젠테이션을 만드는 방법을 알고 있습니다.""",
        verbose=True,
        llm=llm,
        allow_delegation=True,
    )

    quality_reviewer = Agent(
        role="품질 검토자",
        goal="최종 보고서의 정확성, 일관성 및 전문성을 보장합니다",
        backstory="""당신은 세부 사항에 주의를 기울이는 꼼꼼한 검토자입니다. 모든 비즈니스 보고서가 최고 수준의 품질, 정확성 및 전문성을 충족하도록 보장합니다.
        메시징, 포맷팅 및 데이터 프레젠테이션의 일관성을 확인하고 개선을 위한 건설적인 피드백을 제공합니다.""",
        verbose=True,
        llm=llm,
        allow_delegation=True,
    )

    return data_analyst, content_writer, visual_designer, quality_reviewer


# Define the tasks for each agent
def create_report_tasks(agents: tuple[Agent, Agent, Agent, Agent], report_topic: str, data_sources: Optional[list[str]] = None) -> list[Task]:
    """
    Create tasks for generating a business report on the specified topic

    Args:
        agents (tuple): Tuple containing all agent objects
        report_topic (str): The main topic of the business report
        data_sources (list, optional): List of data sources to analyze

    Returns:
        list: List of Task objects
    """
    data_analyst, content_writer, visual_designer, quality_reviewer = agents

    # Default data sources if none provided
    if data_sources is None:
        data_sources = [
            "sample_data/quarterly_sales_data.csv",
            "sample_data/market_trend_analysis.csv",
            "sample_data/customer_feedback_summary.csv",
        ]

    # Data analysis task
    data_analysis_task = Task(
        description=f"""다음 데이터 소스를 {report_topic} 보고서용으로 분석하세요: {', '.join(data_sources)}.
        비즈니스 보고서에 유용할 수 있는 주요 트렌드, 패턴 및 인사이트를 식별하세요.
        보고서에서 강조해야 할 특정 데이터 포인트와 함께 발견한 내용에 대한 요약을 준비하세요.
        차트나 그래프로 시각화해야 하는 3-5개의 주요 지표나 KPI를 제안하세요.""",
        expected_output="주요 인사이트, 트렌드 및 데이터 시각화 권장 사항이 포함된 상세 분석 문서",
        agent=data_analyst,
    )

    # Content creation task
    content_creation_task = Task(
        description=f"""제공된 데이터 분석을 기반으로 {report_topic}에 대한 비즈니스 보고서의 텍스트 내용을 작성하세요.
        보고서에는 다음이 포함되어야 합니다:
        1. 임원 요약 (1-2 단락)
        2. 주제 및 그 중요성에 대한 소개
        3. 상세 분석이 포함된 주요 발견 섹션
        4. 데이터에 기반한 전략적 권장 사항
        5. 주요 요점을 요약하는 결론
        
        내용이 전문적이고 간결하며 실행 가능한지 확인하세요. 비즈니스에 적합한 언어와 구조를 사용하세요.""",
        expected_output="비즈니스 보고서의 각 섹션에 대한 완전한 텍스트 내용",
        agent=content_writer,
        context=[data_analysis_task],
    )

    # Visual design task
    visual_design_task = Task(
        description=f"""제공된 데이터 분석 및 내용을 기반으로 {report_topic} 비즈니스 보고서의 시각적 요소를 만드세요.
        다음을 디자인하세요:
        1. 전문적인 비즈니스 프레젠테이션에 적합한 전체 슬라이드 템플릿 및 색상 구성표
        2. 데이터 분석에서 식별된 주요 지표에 대한 데이터 시각화 (차트, 그래프)
        3. 텍스트, 시각적 요소 및 기타 요소의 배치를 포함한 각 슬라이드에 대한 레이아웃 권장 사항
        4. 프레젠테이션을 향상시킬 수 있는 추가 시각적 요소
        
        각 시각적 요소에 대한 상세 설명과 최종 PowerPoint에서 어떻게 구현되어야 하는지 제공하세요.""",
        expected_output="차트 유형, 레이아웃 및 디자인 요소를 포함한 상세 시각적 디자인 사양",
        agent=visual_designer,
        context=[data_analysis_task, content_creation_task],
    )

    # Quality review task
    quality_review_task = Task(
        description=f"""품질, 일관성 및 전문성을 위해 {report_topic} 비즈니스 보고서의 모든 요소를 검토하세요.
        다음을 확인하세요:
        1. 데이터 및 인사이트의 정확성
        2. 작성된 내용의 명확성 및 전문성
        3. 시각적 요소의 효과성 및 적절성
        4. 프레젠테이션의 전체적인 흐름 및 구조
        5. 비즈니스 보고 모범 사례와의 일치
        
        개선을 위한 구체적인 피드백을 제공하고 최종 보고서가 높은 전문적 기준을 충족하는지 확인하세요.""",
        expected_output="비즈니스 보고서에 대한 구체적인 피드백 및 최종 승인이 포함된 종합적인 검토",
        agent=quality_reviewer,
        context=[data_analysis_task, content_creation_task, visual_design_task],
    )

    return [
        data_analysis_task,
        content_creation_task,
        visual_design_task,
        quality_review_task,
    ]


# Function to generate the PowerPoint presentation
def generate_presentation(report_topic: str, crew_results: Any, output_dir: str = "output") -> Optional[str]:
    """
    Generate a PowerPoint presentation based on the crew's results

    Args:
        report_topic (str): The main topic of the business report
        crew_results: Results from the crew's work (CrewAI output)
        output_dir (str): Directory to save output files

    Returns:
        str: Path to the generated PowerPoint file
    """
    try:
        # Create output directory if it doesn't exist
        os.makedirs(output_dir, exist_ok=True)

        # Debug the structure of crew_results
        print(f"CrewAI Results Type: {type(crew_results)}")

        # Extract content from crew results - handle different possible formats
        if isinstance(crew_results, list) and len(crew_results) >= 3:
            # Original expected format
            data_analysis = crew_results[0]
            content = crew_results[1]
            visual_design = crew_results[2]
        elif hasattr(crew_results, "values") and callable(
            getattr(crew_results, "values", None)
        ):
            # Dictionary-like format
            values = list(crew_results.values())
            if len(values) >= 3:
                data_analysis = values[0]
                content = values[1]
                visual_design = values[2]
            else:
                raise ValueError(
                    f"Not enough values in crew_results: {len(values)} values found, need at least 3"
                )
        elif hasattr(crew_results, "get_task_output"):
            # Newer CrewAI format with task outputs
            # Assuming tasks are in the same order as defined in create_report_tasks
            data_analysis = (
                crew_results.get_task_output(0) or "No data analysis available"
            )
            content = crew_results.get_task_output(1) or "No content available"
            visual_design = (
                crew_results.get_task_output(2) or "No visual design available"
            )
        else:
            # Last resort - try to convert to string and use as content
            print(f"Unknown crew_results format: {crew_results}")
            data_analysis = str(crew_results)
            content = str(crew_results)
            visual_design = str(crew_results)

        # Create PowerPoint generator
        ppt_gen = PPTGenerator(report_topic, output_dir)

        # Generate the presentation
        output_file = ppt_gen.generate_from_agent_results(
            data_analysis, content, visual_design
        )

        return output_file

    except Exception as e:  # 예외 발생 시
        print(f"프레젠테이션 생성 중 오류가 발생했습니다: {e}")
        import traceback
        traceback.print_exc()
        return None


# Main function to run the report generation process
def generate_business_report(report_topic: str, data_sources: Optional[list[str]] = None, output_dir: str = "output") -> Optional[str]:
    """
    Generate a complete business report on the specified topic

    Args:
        report_topic (str): The main topic of the business report
        data_sources (list, optional): List of data sources to analyze
        output_dir (str): Directory to save output files

    Returns:
        str: Path to the generated PowerPoint file
    """
    # Generate sample data if no data sources provided
    if data_sources is None:
        print("\nGenerating sample data for the report...\n")
        data_sources = save_sample_data("sample_data")

    # Create agents
    agents = create_agents()

    # Create tasks
    tasks = create_report_tasks(agents, report_topic, data_sources)

    # Create the crew with all agents
    crew = Crew(agents=agents, tasks=tasks, verbose=True, process=Process.sequential)

    # Start the crew's work
    print(f"\n비즈니스 보고서 생성 중: {report_topic}\n")
    results = crew.kickoff()

    # Generate the PowerPoint presentation
    print("\nPowerPoint 프레젠테이션 생성 중...\n")
    presentation_path = generate_presentation(report_topic, results, output_dir)

    if presentation_path:
        print(f"\n비즈니스 보고서가 성공적으로 생성되었습니다: {presentation_path}\n")
    else:
        print("\nPowerPoint 프레젠테이션 생성에 실패했습니다.\n")

    return presentation_path


# Example usage
if __name__ == "__main__":
    # Example topic and data sources
    topic = "분기별 영업 실적"

    # Generate the report
    generate_business_report(topic)
