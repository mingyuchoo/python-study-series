import os
from typing import List

from crewai import Task


def create_developer_tasks(
    agent, project_name: str, design_result: str, project_dir: str
) -> List[Task]:
    """Creates tasks for the Haskell developer agent."""

    implementation_dir = os.path.join(project_dir, "implementation")
    os.makedirs(implementation_dir, exist_ok=True)

    implementation_report = os.path.join(project_dir, "implementation_report.md")

    return [
        Task(
            description=f"""Implement the Haskell web application based on the architecture design:
            
            Project Name: {project_name}
            Architecture Design: {design_result}
            
            Your task is to:
            1. Set up the project structure and build system (Stack/Cabal)
            2. Implement the core modules and data types
            3. Develop the API endpoints and handlers
            4. Implement database interactions
            5. Create necessary utility functions
            6. Implement business logic according to the requirements
            7. Set up configuration and environment handling
            8. Implement error handling and logging
            
            Write clean, idiomatic Haskell code that follows best practices for functional programming.
            Leverage Haskell's type system for safety and correctness.
            
            Create the following files in the implementation directory:
            - Setup instructions (setup.md)
            - Main application file (app.hs)
            - Core modules (describe in implementation_report.md)
            - Configuration files (describe in implementation_report.md)
            
            Provide a detailed implementation report explaining the code structure, key implementation decisions,
            and instructions for building and running the application.
            
            Save your implementation report to: {implementation_report}
            """,
            agent=agent,
            expected_output="A complete implementation of the Haskell web application with documentation",
            output_file=implementation_report,
        )
    ]
