import os
from typing import List

from crewai import Task


def create_designer_tasks(
    agent, project_name: str, analysis_result: str, project_dir: str
) -> List[Task]:
    """Creates tasks for the system architect agent."""

    architecture_file = os.path.join(project_dir, "architecture_design.md")

    return [
        Task(
            description=f"""Design the architecture for the Haskell web application based on the requirements analysis:
            
            Project Name: {project_name}
            Requirements Analysis: {analysis_result}
            
            Your task is to:
            1. Create a high-level architecture diagram (describe it textually)
            2. Define the module structure following Haskell best practices
            3. Design the data models and type definitions
            4. Specify API endpoints and their types
            5. Design the database schema
            6. Plan the integration with frontend (if applicable)
            7. Identify design patterns appropriate for this Haskell application
            8. Consider error handling and type safety mechanisms
            
            Create a comprehensive architecture document that leverages Haskell's strengths in type safety, 
            immutability, and functional programming paradigms.
            
            Save your design to: {architecture_file}
            """,
            agent=agent,
            expected_output="A detailed architecture design document for a Haskell web application",
            output_file=architecture_file,
        )
    ]
