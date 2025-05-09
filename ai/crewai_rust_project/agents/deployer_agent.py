from typing import List

from crewai import Agent
from langchain.tools import Tool

from utils.crew_config import create_crew_agent


def create_deployer_agent() -> Agent:
    """Creates an agent specialized in deploying Rust web applications."""

    return create_crew_agent(
        role="DevOps Engineer",
        goal="Deploy Rust web applications efficiently and securely with proper configuration",
        backstory="""You are a DevOps expert specialized in deploying Rust applications to production environments. 
        You understand the nuances of deploying functional programming applications and know how to optimize 
        Rust applications for performance in production. You're familiar with containerization, orchestration, 
        and CI/CD pipelines for Rust projects. You ensure that deployments are secure, scalable, and maintainable.""",
        verbose=True,
        allow_delegation=True,
        tools=[],
    )
