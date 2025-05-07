import os
from typing import List, Optional

from crewai import Agent
from dotenv import load_dotenv
from langchain_openai import ChatOpenAI
from langchain.tools import Tool

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


def create_crew_agent(
    role: str,
    goal: str,
    backstory: str,
    verbose: bool = False,
    allow_delegation: bool = False,
    tools: Optional[List[Tool]] = None,
) -> Agent:
    """Creates a CrewAI agent with the specified configuration.

    Args:
        role: The role of the agent
        goal: The goal of the agent
        backstory: The backstory of the agent
        verbose: Whether to enable verbose output
        allow_delegation: Whether to allow delegation to other agents
        tools: List of tools available to the agent

    Returns:
        A configured CrewAI agent
    """
    if tools is None:
        tools = []

    return Agent(
        role=role,
        goal=goal,
        backstory=backstory,
        verbose=verbose,
        allow_delegation=allow_delegation,
        tools=tools,
        llm=llm,
    )
