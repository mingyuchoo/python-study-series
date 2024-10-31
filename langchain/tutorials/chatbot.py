from langchain_aws import ChatBedrock

model = ChatBedrock(model_id="anthropic.claude-3-5-sonnet-20240620-v1:0")

# #####################################
# 1.
# #####################################
from langchain_core.messages import HumanMessage

result = model.invoke([HumanMessage(content="Hi! I'm Bob")])

print(result.content)

result = model.invoke([HumanMessage(content="What's my name?")])

print(result.content)


# #####################################
# 2.
# #####################################

from langchain_core.messages import AIMessage

result = model.invoke(
    [
        HumanMessage(content="Hi! I'm Bob"),
        AIMessage(content="Hello Bob! How can I assist you today?"),
        HumanMessage(content="What's my name?"),
    ]
)

print(result.content)

# #####################################
# 3. Message persistence
# #####################################
from langgraph.checkpoint.memory import MemorySaver
from langgraph.graph import START, MessagesState, StateGraph

# Define a new graph
workflow = StateGraph(state_schema=MessagesState)

# Define the function that calls the model
def call_model(state: MessagesState):
    response = model.invoke(state["messages"])
    return {"messages": response}


# Define the (single) node in the graph
workflow.add_edge(START, "model")
workflow.add_node("model", call_model)

# Add memory
memory = MemorySaver()
app = workflow.compile(checkpointer=memory)

config = {"configurable": {"thread_id": "abc123"}}

query = "Hi! I'm Bob."
input_messages = [HumanMessage(query)]
output = app.invoke({"messages": input_messages}, config)
output["messages"][-1].pretty_print()  


query = "What's my name?"
input_messages = [HumanMessage(query)]
output = app.invoke({"messages": input_messages}, config)
output["messages"][-1].pretty_print()


# #####################################
# 4.Prompt templates
# #####################################

# #####################################
# 5.
# #####################################

# #####################################
# 6.
# #####################################

# #####################################
# 7.
# #####################################
 
