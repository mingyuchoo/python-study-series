# ##############################################################################
# Module-0/basic.ipynb
#
# - http://localhost:8888/notebooks/module-0/basics.ipynb
#
# ##############################################################################

# ######################################
# Load Environment Variables
# ######################################

from dotenv import load_dotenv

load_dotenv()

# ######################################
# Chat with a model
# ######################################

from langchain_aws import ChatBedrock

llm = ChatBedrock(model="anthropic.claude-3-5-sonnet-20240620-v1:0")

from langchain_core.messages import HumanMessage

message = HumanMessage(content="Hello world", name="Lance")
messages = [message]

response = llm.invoke(messages)
print(response)

# ######################################
# Search with a Tool; Tavily
# ######################################

from langchain_community.tools.tavily_search import TavilySearchResults

tavily_search = TavilySearchResults(max_results=3)

search_docs = tavily_search.invoke("What is LangGraph?")
print(search_docs)
