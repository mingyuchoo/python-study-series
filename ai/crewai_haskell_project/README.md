# Haskell Web Development Multi-Agent System

## Overview

This application uses CrewAI to implement a multi-agent collaboration system focused on developing Haskell web applications. Multiple agents with specialized roles work together to analyze requirements, design, implement, test, and deploy web applications written in Haskell.

## Features

- Web-based interface built with Streamlit
- Multiple AI agents with specialized roles:
  - Requirements Analyzer: Analyzes project requirements
  - System Architect: Designs the application architecture
  - Haskell Developer: Implements the application code
  - Quality Assurance Engineer: Tests the application
  - DevOps Engineer: Handles deployment configuration
- Modern, responsive UI
- Project-based workflow
- Comprehensive documentation generation
- Haskell-specific templates and best practices
- Ready-to-use example scenarios

## Installation

```bash
uv pip install -r requirements.txt
```

## Usage

```bash
streamlit run app.py
```

## Documentation

Detailed documentation is available in the `docs` directory:

- [Agent Collaboration Process](docs/agent_collaboration.md) - How the agents work together
- [Example Scenarios](docs/example_scenarios.md) - Sample Haskell web application scenarios
- [Haskell Best Practices](docs/haskell_best_practices.md) - Best practices for Haskell web development
- [Extending the System](docs/extending_the_system.md) - How to customize and extend the system

## Project Structure

```
haskell_web_dev_project/
├── agents/                # Agent definitions
│   ├── analyzer_agent.py  # Requirements analyzer agent
│   ├── designer_agent.py  # System architect agent
│   ├── developer_agent.py # Haskell developer agent
│   ├── tester_agent.py    # QA engineer agent
│   └── deployer_agent.py  # DevOps engineer agent
├── tasks/                 # Task definitions
│   ├── analyzer_tasks.py  # Requirements analysis tasks
│   ├── designer_tasks.py  # Architecture design tasks
│   ├── developer_tasks.py # Implementation tasks
│   ├── tester_tasks.py    # Testing tasks
│   └── deployer_tasks.py  # Deployment tasks
├── utils/                 # Utility functions
│   └── project_manager.py # Project directory management
├── app.py                 # Main Streamlit application
└── requirements.txt       # Python dependencies
```

## How It Works

1. The user provides a project name and description
2. The Requirements Analyzer agent analyzes the requirements and creates specifications
3. The System Architect agent designs the application architecture
4. The Haskell Developer agent implements the code
5. The QA Engineer agent tests the implementation
6. The DevOps Engineer agent prepares deployment configuration
7. All results are displayed in the Streamlit UI

## Configuration

The application uses environment variables for configuration. See the `.env` file for details.

## License

MIT
