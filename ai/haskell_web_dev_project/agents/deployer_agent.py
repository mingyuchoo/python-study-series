from crewai import Agent
from langchain.tools import Tool
from typing import List
from utils.crew_config import create_crew_agent

def create_deployer_agent() -> Agent:
    """Creates an agent specialized in deploying Haskell web applications."""
    
    return create_crew_agent(
        role="DevOps Engineer",
        goal="Deploy Haskell web applications efficiently and securely with proper configuration",
        backstory="""You are a DevOps expert specialized in deploying Haskell applications to production environments. 
        You understand the nuances of deploying functional programming applications and know how to optimize 
        Haskell applications for performance in production. You're familiar with containerization, orchestration, 
        and CI/CD pipelines for Haskell projects. You ensure that deployments are secure, scalable, and maintainable.""",
        verbose=True,
        allow_delegation=True,
        tools=[]
    )
