from typing import List

from crewai import Agent
from langchain.tools import Tool

from utils.crew_config import create_crew_agent


def create_analyzer_agent() -> Agent:
    """Creates an agent specialized in analyzing requirements for Haskell web applications."""

    return create_crew_agent(
        role="Requirements Analyzer",
        goal="Thoroughly analyze project requirements and provide detailed specifications for Haskell web applications",
        backstory="""You are an expert in requirements analysis with deep knowledge of Haskell and web development. 
        Your specialty is breaking down complex requirements into clear, actionable specifications. 
        You have years of experience in functional programming paradigms and understand how to leverage 
        Haskell's strengths for web development.""",
        verbose=True,
        allow_delegation=True,
        tools=[],
    )
