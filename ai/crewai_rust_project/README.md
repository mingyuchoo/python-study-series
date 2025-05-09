# Rust Web Development Multi-Agent System

## Overview

This application uses CrewAI to implement a multi-agent collaboration system focused on developing Rust web applications. Multiple agents with specialized roles work together to analyze requirements, design, implement, test, and deploy web applications written in Rust.

## Features

- Web-based interface built with Streamlit
- Multiple AI agents with specialized roles:
  - Requirements Analyzer: Analyzes project requirements
  - System Architect: Designs the application architecture
  - Rust Developer: Implements the application code
  - Quality Assurance Engineer: Tests the application
  - DevOps Engineer: Handles deployment configuration
- Modern, responsive UI
- Project-based workflow
- Comprehensive documentation generation
- Rust-specific templates and best practices
- Ready-to-use example scenarios

## Installation

```bash
pip install -r requirements.txt
```

## Usage

```bash
streamlit run app.py
```

## Documentation

Detailed documentation is available in the `docs` directory:

- [Agent Collaboration Process](docs/agent_collaboration.md) - How the agents work together
- [Example Scenarios](docs/example_scenarios.md) - Sample Rust web application scenarios
- [Rust Best Practices](docs/rust_best_practices.md) - Best practices for Rust web development
- [Extending the System](docs/extending_the_system.md) - How to customize and extend the system

## Project Structure

```bash
rust_web_dev_project/
├── agents/                # Agent definitions
│   ├── analyzer_agent.py  # Requirements analyzer agent
│   ├── designer_agent.py  # System architect agent
│   ├── developer_agent.py # Rust developer agent
│   ├── tester_agent.py    # QA engineer agent
│   └── deployer_agent.py  # DevOps engineer agent
├── tasks/                 # Task definitions
│   ├── analyzer_tasks.py  # Requirements analysis tasks
│   ├── designer_tasks.py  # Architecture design tasks
│   ├── developer_tasks.py # Implementation tasks
│   ├── tester_tasks.py    # Testing tasks
│   └── deployer_tasks.py  # Deployment tasks
├── utils/                 # Utility functions
│   ├── crew_config.py     # Agent/Crew configuration helpers
│   └── project_manager.py # Project directory management
├── templates/             # Rust web app templates
├── docs/                  # Documentation (markdown)
├── app.py                 # Main Streamlit application
├── requirements.txt       # Python dependencies
├── .env                   # Environment variables
└── Makefile               # (optional) Build/utility commands
```

## How It Works

1. The user provides a project name and description via the Streamlit web UI
2. The Requirements Analyzer agent analyzes the requirements and creates specifications
3. The System Architect agent designs the application architecture
4. The Rust Developer agent implements the code using Rust templates
5. The QA Engineer agent tests the implementation
6. The DevOps Engineer agent prepares deployment configuration
7. All results and logs are displayed in the Streamlit UI, with real-time colored logs at the bottom of the page
8. Project files are generated and can be viewed/downloaded from the UI

## Configuration

The application uses environment variables for configuration. See the `.env` file for details.

## License

MIT
