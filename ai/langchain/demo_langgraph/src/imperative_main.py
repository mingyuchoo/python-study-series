# ##############################################################################
# Module-0/basic.ipynb
#
# - http://localhost:8888/notebooks/module-0/basics.ipynb
#
# ##############################################################################

# ######################################
# Load Environment Variables
# ######################################

from typing import Annotated, TypedDict

from dotenv import load_dotenv

load_dotenv()


# ######################################
# A tool; Tavily Search
# ######################################

from langchain_community.tools.tavily_search import TavilySearchResults

tool_tavily_search = TavilySearchResults(max_results=2)
tool_tavily_search.invoke("What's a 'node' in LangGraph?")

tools = [tool_tavily_search]


# ######################################
# Bind model with tools
# ######################################

from langchain_openai import AzureChatOpenAI

llm = AzureChatOpenAI()  # Uses Azure config from environment variables
llm_with_tools = llm.bind_tools(tools)

# ######################################
# Chatbot
# ######################################

from typing import Annotated

from langgraph.graph.message import add_messages
from typing_extensions import TypedDict


class State(TypedDict):
    messages: Annotated[list, add_messages]


def chatbot(state: State):
    return {"messages": [llm_with_tools.invoke(state["messages"])]}


# ######################################
# Graph
# ######################################

from langgraph.graph import StateGraph
from langgraph.prebuilt import ToolNode, tools_condition

tool_node = ToolNode(tools=[tool_tavily_search])

graph_builder = StateGraph(State)
graph_builder.add_node("chatbot", chatbot)
graph_builder.add_node("tools", tool_node)
graph_builder.add_conditional_edges("chatbot", tools_condition)
graph_builder.add_edge("tools", "chatbot")
graph_builder.set_entry_point("chatbot")
graph = graph_builder.compile()


# ######################################
# Run
# ######################################


def stream_graph_updates(user_input: str):
    for event in graph.stream({"messages": [("user", user_input)]}):
        for value in event.values():
            print("Assistant:", value["messages"][-1].content)


while True:
    try:
        user_input = input("User: ")
        if user_input.lower() in ["quit", "exit", "q"]:
            print("Goodbye!")
            break

        stream_graph_updates(user_input)
    except:
        # fallback if input() is not available
        user_input = "What do you know about LangGraph?"
        print("User: " + user_input)
        stream_graph_updates(user_input)
        break
