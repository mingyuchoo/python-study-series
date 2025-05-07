import os
from typing import List

from crewai import Task


def create_analyzer_tasks(
    agent, project_name: str, project_description: str, project_dir: str
) -> List[Task]:
    """Creates tasks for the requirements analyzer agent."""

    requirements_file = os.path.join(project_dir, "requirements_analysis.md")

    return [
        Task(
            description=f"""Analyze the following Haskell web application project requirements and create a detailed specification document:
            
            Project Name: {project_name}
            Project Description: {project_description}
            
            Your task is to:
            1. Identify the core functional requirements of the application
            2. Determine non-functional requirements (performance, security, scalability, etc.)
            3. Identify appropriate Haskell frameworks and libraries for this project
            4. Analyze technical constraints and dependencies
            5. Recommend a database solution that works well with Haskell
            6. Identify potential integration points with other systems
            7. Estimate the complexity and scope of the project
            
            Create a comprehensive requirements document that will serve as the foundation for the design phase.
            Be specific about Haskell-related requirements and considerations.
            
            Save your analysis to: {requirements_file}
            """,
            agent=agent,
            expected_output="A detailed requirements analysis document for a Haskell web application",
            output_file=requirements_file,
        )
    ]
