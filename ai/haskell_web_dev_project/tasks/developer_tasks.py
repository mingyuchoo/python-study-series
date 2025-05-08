import os
from typing import List

from crewai import Task


def create_developer_tasks(
    agent, project_name: str, design_result: str, project_dir: str
) -> List[Task]:
    """Creates tasks for the Haskell developer agent."""

    implementation_dir = os.path.join(project_dir, "implementation")
    os.makedirs(implementation_dir, exist_ok=True)
    
    # Create docs directory for markdown files
    docs_dir = os.path.join(project_dir, "docs")
    os.makedirs(docs_dir, exist_ok=True)

    implementation_report = os.path.join(docs_dir, "implementation_report.md")

    return [
        Task(
            description=f"""Customize and extend the Haskell web application template based on the architecture design:
            
            Project Name: {project_name}
            Architecture Design: {design_result}
            
            The project has been initialized with a Haskell web application template in the project directory.
            Your task is to customize and extend this template to meet the specific requirements:
            
            1. Review the existing template files and understand their structure
            2. Modify the cabal file to reflect the project name and dependencies
            3. Customize the API endpoints and handlers based on the design
            4. Extend the database interactions as needed
            5. Implement additional business logic according to the requirements
            6. Update configuration files to match the project needs
            7. Enhance error handling and logging as necessary
            
            The template already includes:
            - Basic project structure with cabal configuration
            - Main application file (app.hs)
            - Database connectivity (Database.hs)
            - JSON handling (Json.hs)
            - Deployment configurations (in the deployment directory)
            - Setup scripts (setup.sh and install_deps.sh)
            
            Provide a detailed implementation report explaining the code structure, the changes you've made,
            key implementation decisions, and instructions for building and running the application.
            
            Save your implementation report to: {implementation_report}
            """,
            agent=agent,
            expected_output="A customized implementation of the Haskell web application with documentation",
            output_file=implementation_report,
        )
    ]
