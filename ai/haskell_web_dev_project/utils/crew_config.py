from crewai import Agent, Task, Crew, Process
from langchain_openai import AzureChatOpenAI, ChatOpenAI
import os
from utils.api_helpers import get_openai_config

def get_llm():
    """
    Get the appropriate LLM (Language Learning Model) based on configuration.
    Handles both standard OpenAI and Azure OpenAI.
    
    Returns:
        An instance of either ChatOpenAI or AzureChatOpenAI
    """
    config = get_openai_config()
    api_url = os.environ.get("OPENAI_API_URL", "")
    
    # Check if using Azure OpenAI
    if api_url and api_url.endswith("openai.azure.com"):
        # Azure OpenAI configuration
        return AzureChatOpenAI(
            openai_api_version=config.get("api_version"),
            azure_deployment=config.get("deployment_id"),
            openai_api_key=config.get("api_key"),
            azure_endpoint=config.get("api_base"),
            temperature=config.get("temperature"),
            max_tokens=config.get("max_tokens"),
            top_p=config.get("top_p"),
            streaming=config.get("stream")
        )
    else:
        # Standard OpenAI configuration
        return ChatOpenAI(
            api_key=config.get("api_key"),
            model=config.get("model"),
            temperature=config.get("temperature"),
            max_tokens=config.get("max_tokens"),
            top_p=config.get("top_p"),
            streaming=config.get("stream")
        )

def create_crew_agent(role, goal, backstory, verbose=True, allow_delegation=True, tools=None):
    """
    Create a CrewAI agent with the appropriate LLM configuration.
    
    Args:
        role (str): The role of the agent
        goal (str): The goal of the agent
        backstory (str): The backstory of the agent
        verbose (bool): Whether to enable verbose output
        allow_delegation (bool): Whether to allow delegation
        tools (list): List of tools available to the agent
        
    Returns:
        Agent: A CrewAI agent
    """
    if tools is None:
        tools = []
        
    llm = get_llm()
    
    return Agent(
        role=role,
        goal=goal,
        backstory=backstory,
        verbose=verbose,
        allow_delegation=allow_delegation,
        tools=tools,
        llm=llm
    )
