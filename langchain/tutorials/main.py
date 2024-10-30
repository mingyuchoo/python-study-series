import getpass
import os


from langchain_aws import ChatBedrock

model = ChatBedrock(model_id="anthropic.claude-3-5-sonnet-20240620-v1:0")

from langchain_core.messages import HumanMessage, SystemMessage

messages = [
    SystemMessage(content="Translate the following from English into Italian"),
    HumanMessage(content="Hi!"),
]


from langchain_core.output_parsers import StrOutputParser

parser = StrOutputParser()

chain = model | parser
chain.invoke(messages)

from langchain_core.prompt import ChatPromptTemplate

system_template = "Translate the following into {language}:"
prompt_template = ChatPromptTemplate.from_messages(
    [("system", system_template), ("user", "{text}")]
)

chain = prompt_template | model | parser

result = chain.invoke({"langauge": "italian", "text": "hi"})
print(result)
