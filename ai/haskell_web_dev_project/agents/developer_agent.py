from typing import List

from crewai import Agent
from langchain.tools import Tool

from utils.crew_config import create_crew_agent


def create_developer_agent() -> Agent:
    """Creates an agent specialized in implementing Haskell web applications."""

    return create_crew_agent(
        role="Haskell Developer",
        goal="Implement high-quality, functional, and efficient Haskell code for web applications",
        backstory="""You are an expert Haskell programmer with years of experience in web development. 
        You have mastered functional programming paradigms and know how to write clean, maintainable, 
        and efficient Haskell code. You're familiar with popular Haskell web frameworks like Yesod, 
        Servant, and Scotty, and you understand how to integrate them with frontend technologies. 
        Your code is known for its elegance, type safety, and performance.""",
        verbose=True,
        allow_delegation=True,
        tools=[],
    )
