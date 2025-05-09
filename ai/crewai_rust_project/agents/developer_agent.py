from typing import List

from crewai import Agent
from langchain.tools import Tool

from utils.crew_config import create_crew_agent


def create_developer_agent() -> Agent:
    """Creates an agent specialized in implementing Rust web applications."""

    return create_crew_agent(
        role="Rust Developer",
        goal="Implement high-quality, functional, and efficient Rust code for web applications",
        backstory="""You are an expert Rust programmer with years of experience in web development. 
        You have mastered functional programming paradigms and know how to write clean, maintainable, 
        and efficient Rust code. You're familiar with popular Rust web frameworks like Yesod, 
        Servant, and Scotty, and you understand how to integrate them with frontend technologies. 
        Your code is known for its elegance, type safety, and performance.""",
        verbose=True,
        allow_delegation=True,
        tools=[],
    )
