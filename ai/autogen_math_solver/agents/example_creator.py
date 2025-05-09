from typing import Any, Dict, Optional

import autogen
from autogen import AssistantAgent, UserProxyAgent

from utils.agent_helpers import create_azure_openai_wrapper


def create_example_creator_agents(llm_config: Optional[dict] = None) -> Dict[str, Any]:
    """
    함수형 스타일로 예시 생성 에이전트 집합을 생성합니다.
    llm_config는 옵션이며, 없으면 create_azure_openai_wrapper()로 생성합니다.
    반환값: 에이전트 딕셔너리
    """
    if llm_config is None:
        from utils.agent_helpers import create_azure_openai_wrapper

        llm_config = create_azure_openai_wrapper()
    llm_config = dict(llm_config)  # 복사
    llm_config["temperature"] = 0.7

    creator = AssistantAgent(
        name="Example_Creator",
        system_message="""You are a real-world example creator for mathematical concepts. Your role is to:
        1. Create practical, engaging real-world examples that demonstrate mathematical concepts
        2. Explain how mathematical formulas and solutions apply to everyday situations
        3. Make abstract mathematical concepts concrete and relatable
        4. Provide diverse examples from various fields (science, engineering, finance, sports, etc.)
        5. Ensure examples are accurate, educational, and interesting
        
        Your examples should help people understand why the mathematical concept is useful and
        how it can be applied in real situations. Be creative but ensure the examples are realistic
        and the mathematics is applied correctly.
        """,
        llm_config=llm_config,
    )

    user_proxy = UserProxyAgent(
        name="User_Proxy",
        system_message="""You are a proxy for the user in this conversation. Your role is to:
        1. Provide feedback on the examples
        2. Decide when the conversation should terminate
        3. Summarize the final examples
        """,
        human_input_mode="NEVER",
        llm_config=llm_config,
    )

    return {"llm_config": llm_config, "creator": creator, "user_proxy": user_proxy}


def create_example(agents: Dict[str, Any], problem_text: str, solution_text: str) -> Optional[str]:
    """
    함수형 스타일로 수학 문제와 해답에 대한 실생활 예시를 생성합니다.
    agents: create_example_creator_agents()의 반환값
    """
    message = f"""Please create a practical, engaging real-world example that demonstrates the \
application of the following mathematical problem and its solution:\n\nProblem: {problem_text}\n\nSolution: {solution_text}\n\nCreate an example that shows how this mathematical concept can be applied in a real-world \
situation. Make it concrete, relatable, and educational. Explain how the mathematical \
formulas and solution process would be used in this situation.\n"""
    chat_result = agents["user_proxy"].initiate_chat(agents["creator"], message=message)
    example_message = None
    for msg in chat_result.chat_history:
        if msg["role"] == "assistant":
            example_message = msg["content"]
    return example_message
