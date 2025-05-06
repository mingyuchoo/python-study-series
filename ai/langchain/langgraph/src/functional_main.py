from typing import Annotated, TypedDict, Callable, List
from dotenv import load_dotenv
from langchain_community.tools.tavily_search import TavilySearchResults
from langchain_aws import ChatBedrock
from langgraph.graph.message import add_messages
from langgraph.graph import StateGraph
from langgraph.prebuilt import ToolNode, tools_condition
from functools import partial

# 환경 변수 로드
load_dotenv()


# 타입 정의
class State(TypedDict):
    messages: Annotated[list, add_messages]


# 순수 함수: Tavily 검색 도구 생성
def create_tavily_search_tool(max_results: int = 2) -> TavilySearchResults:
    return TavilySearchResults(max_results=max_results)


# 순수 함수: ChatBedrock 모델 생성
def create_chat_bedrock(model: str) -> ChatBedrock:
    return ChatBedrock(model=model)


# 순수 함수: 도구와 모델 바인딩
def bind_tools_to_model(model: ChatBedrock, tools: List[Callable]) -> Callable:
    return model.bind_tools(tools)


# 순수 함수: 챗봇 응답 생성
def generate_chatbot_response(llm_with_tools: Callable, state: State) -> dict:
    return {"messages": [llm_with_tools.invoke(state["messages"])]}


# 순수 함수: 그래프 생성
def create_graph(tool_node: ToolNode, chatbot_func: Callable) -> StateGraph:
    graph_builder = StateGraph(State)
    graph_builder.add_node("chatbot", chatbot_func)
    graph_builder.add_node("tools", tool_node)
    graph_builder.add_conditional_edges("chatbot", tools_condition)
    graph_builder.add_edge("tools", "chatbot")
    graph_builder.set_entry_point("chatbot")
    return graph_builder.compile()


# 순수 함수: 그래프 업데이트 스트리밍
def stream_graph_updates(graph: StateGraph, user_input: str) -> None:
    for event in graph.stream({"messages": [("user", user_input)]}):
        for value in event.values():
            print("Assistant:", value["messages"][-1].content)


# 메인 함수
def main():
    # 도구 및 모델 초기화
    tavily_search_tool = create_tavily_search_tool()
    llm = create_chat_bedrock("anthropic.claude-3-5-sonnet-20240620-v1:0")
    llm_with_tools = bind_tools_to_model(llm, [tavily_search_tool])

    # 노드 및 그래프 생성
    tool_node = ToolNode(tools=[tavily_search_tool])
    chatbot_func = partial(generate_chatbot_response, llm_with_tools)
    graph = create_graph(tool_node, chatbot_func)

    # 대화 루프
    while True:
        try:
            user_input = input("User: ")
            if user_input.lower() in ["quit", "exit", "q"]:
                print("Goodbye!")
                break
            stream_graph_updates(graph, user_input)
        except:
            user_input = "What do you know about LangGraph?"
            print("User: " + user_input)
            stream_graph_updates(graph, user_input)
            break


if __name__ == "__main__":
    main()
