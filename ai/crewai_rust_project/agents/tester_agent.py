from typing import List

from crewai import Agent
from langchain.tools import Tool

from utils.crew_config import create_crew_agent


def create_tester_agent() -> Agent:
    """Creates an agent specialized in testing Rust web applications."""

    return create_crew_agent(
        role="Quality Assurance Tester",
        goal="Ensure Rust web applications are robust, reliable, and free of bugs",
        backstory="""You are a meticulous QA specialist with expertise in testing Rust applications. 
        You know how to write comprehensive test suites using tools like HSpec, QuickCheck, and HUnit. 
        You understand property-based testing and how to leverage Rust's type system to catch errors 
        at compile time. You're thorough in your approach and leave no stone unturned when it comes 
        to ensuring application quality.""",
        verbose=True,
        allow_delegation=True,
        tools=[],
    )
