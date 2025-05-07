import os
from typing import List

from crewai import Task


def create_tester_tasks(
    agent, project_name: str, implementation_result: str, project_dir: str
) -> List[Task]:
    """Creates tasks for the QA engineer agent."""

    testing_dir = os.path.join(project_dir, "tests")
    os.makedirs(testing_dir, exist_ok=True)

    testing_report = os.path.join(project_dir, "testing_report.md")

    return [
        Task(
            description=f"""Test the Haskell web application implementation:
            
            Project Name: {project_name}
            Implementation Report: {implementation_result}
            
            Your task is to:
            1. Design a comprehensive testing strategy for the Haskell application
            2. Write unit tests for core functions and modules
            3. Implement property-based tests using QuickCheck where appropriate
            4. Create integration tests for API endpoints
            5. Test database interactions
            6. Perform error handling and edge case testing
            7. Validate type safety across the application
            8. Document test coverage and results
            
            Leverage Haskell's testing frameworks (HUnit, HSpec, QuickCheck) to ensure the application
            is correct, robust, and maintainable.
            
            Create the following files in the tests directory:
            - Testing strategy document (strategy.md)
            - Example unit tests (unit_tests.hs)
            - Example property tests (property_tests.hs)
            - Example integration tests (integration_tests.hs)
            
            Provide a detailed testing report that includes test coverage, identified issues,
            and recommendations for improvements.
            
            Save your testing report to: {testing_report}
            """,
            agent=agent,
            expected_output="A comprehensive testing report with example test implementations",
            output_file=testing_report,
        )
    ]
