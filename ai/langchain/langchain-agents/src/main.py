from functools import partial
from typing import List, Callable, Dict, Any

from langchain.agents import Tool
from langgraph.prebuilt import create_react_agent
from langchain_aws import ChatBedrock
from langchain_core.messages import HumanMessage
from langchain_community.tools import DuckDuckGoSearchRun


def create_model(model_id: str) -> ChatBedrock:
    return ChatBedrock(model_id=model_id)


def create_search() -> DuckDuckGoSearchRun:
    return DuckDuckGoSearchRun()


def create_tool(name: str, func: Callable, description: str) -> Tool:
    return Tool(name=name, func=func, description=description)


def create_tools(search: DuckDuckGoSearchRun) -> List[Tool]:
    return [
        create_tool(
            "search",
            search.run,
            "useful for when you need to answer questions about current events.",
        )
    ]


def create_agent(model: ChatBedrock, tools: List[Tool]) -> Any:
    return create_react_agent(model, tools)


def invoke_agent(agent: Any, question: str) -> Dict[str, Any]:
    return agent.invoke({"messages": [HumanMessage(content=question)]})


def get_agent_response(response: Dict[str, Any]) -> str:
    return response["messages"][2].content


def main() -> None:
    model = create_model("anthropic.claude-3-5-sonnet-20240620-v1:0")
    search = create_search()
    tools = create_tools(search)
    agent = create_agent(model, tools)

    question = "What time is it in London?"
    response = invoke_agent(agent, question)

    print(get_agent_response(response))


if __name__ == "__main__":
    main()
