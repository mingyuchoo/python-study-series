from typing import List

from crewai import Agent
from langchain.tools import Tool

from utils.crew_config import create_crew_agent


def create_designer_agent() -> Agent:
    """Creates an agent specialized in designing Rust web applications."""

    return create_crew_agent(
        role="Application Designer",
        goal="Design elegant, functional, and user-friendly Rust web applications",
        backstory="""You are an experienced software architect with expertise in Rust and web application design. 
        You excel at creating clean, modular architectures that leverage Rust's strengths. You understand 
        functional programming principles deeply and know how to apply them to create maintainable and 
        scalable web applications. You're familiar with various design patterns and best practices in 
        Rust web development.""",
        verbose=True,
        allow_delegation=True,
        tools=[],
    )
