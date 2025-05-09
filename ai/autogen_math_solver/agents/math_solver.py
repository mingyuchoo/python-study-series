from typing import Any, Dict, Optional

import autogen
import numpy as np
import sympy
from autogen import (Agent, AssistantAgent, GroupChat, GroupChatManager,
                     UserProxyAgent)

from utils.agent_helpers import create_azure_openai_wrapper


def create_math_solver_agents(llm_config: Optional[dict] = None) -> Dict[str, Any]:
    """
    함수형 스타일로 수학 문제 해결 에이전트 집합을 생성합니다.
    llm_config는 옵션이며, 없으면 create_azure_openai_wrapper()로 생성합니다.
    반환값: 에이전트 딕셔너리
    """
    if llm_config is None:
        from utils.agent_helpers import create_azure_openai_wrapper

        llm_config = create_azure_openai_wrapper()

    analyzer = AssistantAgent(
        name="Math_Analyzer",
        system_message="""You are a mathematical problem analyzer. Your role is to:
        1. Carefully analyze mathematical problems
        2. Break down complex problems into smaller, manageable parts
        3. Identify the mathematical concepts and techniques needed to solve the problem
        4. Suggest an approach to solve the problem
        5. Verify that all necessary information is available to solve the problem
        
        Be precise and thorough in your analysis. If there are multiple ways to approach the problem,
        mention them and explain the pros and cons of each approach.
        """,
        llm_config=llm_config,
    )

    equation_formulator = AssistantAgent(
        name="Equation_Formulator",
        system_message="""You are an equation formulation expert. Your role is to:
        1. Translate mathematical problems into precise mathematical equations and expressions
        2. Use appropriate mathematical notation and symbols
        3. Ensure the equations accurately represent the problem
        4. Simplify equations when possible without losing information
        5. Present the equations in a clear and organized manner
        
        Always show your work and explain your reasoning for formulating equations.
        Use LaTeX notation when appropriate for complex mathematical expressions.
        """,
        llm_config=llm_config,
    )

    calculator = AssistantAgent(
        name="Solution_Calculator",
        system_message="""You are a mathematical solution calculator. Your role is to:
        1. Solve mathematical equations and expressions step by step
        2. Apply appropriate mathematical techniques and algorithms
        3. Perform calculations accurately and efficiently
        4. Verify your solutions by checking your work
        5. Present your solutions clearly, showing all steps
        
        You can use symbolic math libraries like sympy when appropriate.
        Always double-check your calculations and provide the final answer in a clear format.
        """,
        llm_config=llm_config,
    )

    verifier = AssistantAgent(
        name="Solution_Verifier",
        system_message="""You are a mathematical solution verifier. Your role is to:
        1. Verify the correctness of mathematical solutions
        2. Check for calculation errors or logical flaws
        3. Ensure the solution addresses all parts of the original problem
        4. Validate that the solution makes sense in the context of the problem
        5. Suggest corrections or improvements if necessary
        
        Be thorough and critical in your verification. If you find any errors or issues,
        explain them clearly and suggest how they can be fixed.
        """,
        llm_config=llm_config,
    )

    explainer = AssistantAgent(
        name="Explanation_Generator",
        system_message="""You are a mathematical explanation generator. Your role is to:
        1. Explain mathematical solutions in clear, accessible language
        2. Break down complex concepts into understandable parts
        3. Provide intuitive explanations for mathematical techniques used
        4. Connect the solution to the original problem
        5. Highlight key insights and learning points
        
        Your explanations should be understandable to someone who is not an expert in mathematics.
        Use analogies, visualizations, and examples when appropriate to enhance understanding.
        """,
        llm_config=llm_config,
    )

    user_proxy = UserProxyAgent(
        name="User_Proxy",
        system_message="""You are a proxy for the user in this conversation. Your role is to:
        1. Execute code when requested by other agents
        2. Provide feedback on the solutions and explanations
        3. Decide when the conversation should terminate
        4. Summarize the final solution and explanation
        
        You can execute Python code that uses libraries like sympy and numpy for mathematical calculations.
        """,
        human_input_mode="NEVER",
        llm_config=llm_config,
        code_execution_config={"use_docker": False},
    )

    return {
        "llm_config": llm_config,
        "analyzer": analyzer,
        "equation_formulator": equation_formulator,
        "calculator": calculator,
        "verifier": verifier,
        "explainer": explainer,
        "user_proxy": user_proxy,
        "setup_group_chat": setup_group_chat,
        "solve_problem": solve_problem,
    }


def setup_group_chat(agents, llm_config):
    """
    함수형 스타일로 그룹챗 및 매니저를 생성합니다.
    agents: create_math_solver_agents()의 반환값
    """
    agent_list = [
        agents["analyzer"],
        agents["equation_formulator"],
        agents["calculator"],
        agents["verifier"],
        agents["explainer"],
        agents["user_proxy"],
    ]
    group_chat = GroupChat(agents=agent_list, messages=[], max_round=50)
    manager = GroupChatManager(groupchat=group_chat, llm_config=llm_config)
    return group_chat, manager


def solve_problem(agents, problem_text):
    """
    함수형 스타일로 수학 문제를 해결합니다.
    agents: create_math_solver_agents()의 반환값
    """
    group_chat, manager = setup_group_chat(agents, agents["llm_config"])
    chat_result = manager.initiate_chat(
        agents["user_proxy"], message=f"Please solve this mathematical problem collaboratively: {problem_text}"
    )
    conversation = [{"agent": message["name"], "content": message["content"]} for message in group_chat.messages]
    final_messages = [msg for msg in group_chat.messages if msg["name"] == "Explanation_Generator"]
    explanation = final_messages[-1]["content"] if final_messages else ""
    solution_messages = [msg for msg in group_chat.messages if msg["name"] == "Solution_Calculator"]
    solution = solution_messages[-1]["content"] if solution_messages else ""
    return {"solution": solution, "explanation": explanation, "conversation": conversation}
