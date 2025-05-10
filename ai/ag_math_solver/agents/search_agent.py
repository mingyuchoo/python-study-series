import os
from typing import Any, Callable, Dict, Optional

import autogen
from autogen import AssistantAgent, UserProxyAgent
from tavily import TavilyClient

from utils.agent_helpers import create_azure_openai_wrapper


def create_search_agents(llm_config: Optional[dict] = None, tavily_client: Any = None) -> Dict[str, Any]:
    """
    함수형 스타일로 인터넷 검색 에이전트 집합을 생성합니다.
    llm_config와 tavily_client는 옵션이며, 없으면 환경변수로부터 자동 생성합니다.
    반환값: 에이전트 딕셔너리
    """
    import os

    from tavily import TavilyClient

    if llm_config is None:
        from utils.agent_helpers import create_azure_openai_wrapper

        llm_config = create_azure_openai_wrapper()
    llm_config = dict(llm_config)
    llm_config["temperature"] = 0.3
    if tavily_client is None:
        tavily_api_key = os.getenv("TAVILY_API_KEY")
        tavily_client = TavilyClient(api_key=tavily_api_key) if tavily_api_key else None

    def search_internet(query):
        """Search the internet using Tavily API"""
        if tavily_client is None:
            return "Search functionality is not available. Tavily API key is missing."
        try:
            search_result = tavily_client.search(query=query, search_depth="advanced")
            formatted_results = "Search Results:\n\n"
            for i, result in enumerate(search_result.get("results", []), 1):
                formatted_results += f"Result {i}:\n"
                formatted_results += f"Title: {result.get('title', 'No title')}\n"
                formatted_results += f"URL: {result.get('url', 'No URL')}\n"
                formatted_results += f"Content: {result.get('content', 'No content')}\n\n"
            return formatted_results
        except Exception as e:
            return f"Error performing search: {str(e)}"

    searcher = AssistantAgent(
        name="Internet_Searcher",
        system_message="""You are an internet search specialist for mathematical concepts. Your role is to:
        1. Search for relevant information about mathematical concepts, formulas, and techniques
        2. Find examples, explanations, and alternative approaches to mathematical problems
        3. Verify mathematical facts and theorems
        4. Discover educational resources related to mathematical topics
        5. Summarize and present search results in a clear, organized manner
        
        Use the Tavily search API to find accurate, up-to-date information. Focus on reliable
        sources such as educational websites, academic papers, and reputable math resources.
        """,
        llm_config=llm_config,
    )
    user_proxy = UserProxyAgent(
        name="User_Proxy",
        system_message="""You are a proxy for the user in this conversation. Your role is to:
        1. Execute search queries when requested by the search agent
        2. Provide feedback on the search results
        3. Decide when the conversation should terminate
        4. Summarize the final search results
        """,
        human_input_mode="NEVER",
        llm_config=llm_config,
        function_map={"search_internet": search_internet},
    )
    return {
        "llm_config": llm_config,
        "searcher": searcher,
        "user_proxy": user_proxy,
        "search_internet": search_internet,
    }


def perform_search(agents: Dict[str, Any], query: str) -> Any:
    """
    함수형 스타일로 인터넷 검색 및 결과 분석을 수행합니다.
    agents: create_search_agents()의 반환값
    """
    message = f"""Please search for information related to the following mathematical query \n        and analyze the results to provide useful insights:\n\n{query}\n\nUse the search_internet function to perform the search, then analyze and summarize the results.\nFocus on finding relevant mathematical concepts, formulas, examples, and explanations.\n"""
    chat_result = agents["user_proxy"].initiate_chat(agents["searcher"], message=message)
    search_analysis = None
    for msg in chat_result.chat_history:
        if msg["role"] == "assistant":
            search_analysis = msg["content"]
    return search_analysis
