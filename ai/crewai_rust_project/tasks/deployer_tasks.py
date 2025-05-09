import os
from typing import List

from crewai import Task


def create_deployer_tasks(
    agent, project_name: str, testing_result: str, project_dir: str
) -> List[Task]:
    """Creates tasks for the DevOps engineer agent."""

    deployment_dir = os.path.join(project_dir, "deployment")
    os.makedirs(deployment_dir, exist_ok=True)

    # Create docs directory for markdown files
    docs_dir = os.path.join(project_dir, "docs")
    os.makedirs(docs_dir, exist_ok=True)

    deployment_report = os.path.join(docs_dir, "deployment_report.md")

    return [
        Task(
            description=f"""Prepare deployment configuration for the Rust web application:
            
            Project Name: {project_name}
            Testing Report: {testing_result}
            
            Your task is to:
            1. Create a deployment strategy for the Rust application
            2. Set up containerization using Docker
            3. Configure CI/CD pipeline scripts
            4. Prepare production environment configuration
            5. Document scaling and performance considerations
            6. Set up monitoring and logging solutions
            7. Create backup and disaster recovery procedures
            8. Document security considerations for deployment
            
            Focus on best practices for deploying Rust applications in production environments.
            
            Create the following files in the deployment directory:
            - Deployment guide (guide.md)
            - Dockerfile
            - CI/CD configuration (ci_cd_config.yml)
            - Environment configuration examples (env_config.md)
            
            Provide a detailed deployment report that includes all necessary steps to deploy,
            maintain, and monitor the application in production.
            
            Save your deployment report to: {deployment_report}
            """,
            agent=agent,
            expected_output="A comprehensive deployment configuration and documentation",
            output_file=deployment_report,
        )
    ]
