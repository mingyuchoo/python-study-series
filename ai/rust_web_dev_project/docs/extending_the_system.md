# Extending the Rust Web Development Multi-Agent System

This document provides guidance on how to extend and customize our Rust web development multi-agent system for additional features and capabilities.

## Adding New Agent Types

The current system includes five specialized agents, but you can add new agent types for specific needs:

### 1. Create a New Agent Module

Create a new file in the `agents` directory, following the pattern of existing agent files:

```python
# agents/security_agent.py
from crewai import Agent
from langchain.tools import Tool
from typing import List

def create_security_agent() -> Agent:
    """Creates an agent specialized in security for Rust web applications."""
    
    return Agent(
        role="Security Engineer",
        goal="Ensure the security, privacy, and compliance of Rust web applications",
        backstory="""You are a cybersecurity expert specialized in web application security. 
        You have deep knowledge of common vulnerabilities, secure coding practices, and 
        compliance requirements. You excel at identifying security risks in Rust 
        applications and recommending appropriate mitigations.""",
        verbose=True,
        allow_delegation=True,
        tools=[],
        llm_config={
            "temperature": 0.7,
            "model": "gpt-4o"
        }
    )
```

### 2. Create Tasks for the New Agent

Create a new file in the `tasks` directory for the new agent's tasks:

```python
# tasks/security_tasks.py
from crewai import Task
from typing import List
import os

def create_security_tasks(agent, project_name: str, implementation_result: str, project_dir: str) -> List[Task]:
    """Creates tasks for the security engineer agent."""
    
    security_dir = os.path.join(project_dir, "security")
    os.makedirs(security_dir, exist_ok=True)
    
    security_report = os.path.join(project_dir, "security_report.md")
    
    return [
        Task(
            description=f"""Perform a security assessment of the Rust web application:
            
            Project Name: {project_name}
            Implementation Report: {implementation_result}
            
            Your task is to:
            1. Identify potential security vulnerabilities in the code
            2. Assess authentication and authorization mechanisms
            3. Review data handling and privacy practices
            4. Check for secure coding practices in Rust
            5. Recommend security improvements
            6. Create security policies and guidelines
            7. Develop security test cases
            8. Evaluate compliance with relevant regulations
            
            Focus on Rust-specific security considerations and best practices.
            
            Create the following files in the security directory:
            - Security assessment (assessment.md)
            - Security policies (policies.md)
            - Security test cases (tests.md)
            
            Provide a detailed security report that includes all findings and recommendations.
            
            Save your security report to: {security_report}
            """,
            agent=agent,
            expected_output="A comprehensive security assessment and recommendations",
            output_file=security_report
        )
    ]
```

### 3. Update the Main Application

Update `app.py` to include the new agent and tasks:

```python
# Add imports
from agents.security_agent import create_security_agent
from tasks.security_tasks import create_security_tasks

# In the processing section, add after testing phase:
status_text.text("Performing security assessment...")
security_agent = create_security_agent()
security_tasks = create_security_tasks(security_agent, st.session_state.project_name, implementation_result, project_dir)
security_crew = Crew(
    agents=[security_agent],
    tasks=security_tasks,
    verbose=True,
    process=Process.sequential
)
security_result = security_crew.kickoff()
st.session_state.results["security"] = security_result
progress_bar.progress(90)  # Adjust other progress percentages accordingly

# Add a new tab in the results section
tabs = st.tabs(["Analysis", "Design", "Implementation", "Testing", "Security", "Deployment"])
# Add content for the security tab
with tabs[4]:
    if "security" in st.session_state.results:
        st.markdown("### Security Assessment")
        st.markdown(st.session_state.results["security"])
```

## Adding Custom Tools

You can enhance agents with custom tools to perform specific actions:

### 1. Create a Tool Module

Create a new file in a `tools` directory:

```python
# tools/rust_tools.py
from langchain.tools import Tool
from typing import List
import subprocess
import os

def create_rust_lint_tool() -> Tool:
    """Creates a tool for linting Rust code."""
    
    def lint_rust_code(file_path: str) -> str:
        """Lint Rust code using HLint."""
        try:
            result = subprocess.run(
                ["hlint", file_path],
                capture_output=True,
                text=True,
                check=False
            )
            return result.stdout if result.stdout else "No lint issues found."
        except Exception as e:
            return f"Error running HLint: {str(e)}"
    
    return Tool(
        name="LintRustCode",
        func=lint_rust_code,
        description="Lint Rust code to identify potential issues and improvements."
    )

def create_rust_tools() -> List[Tool]:
    """Create all Rust-related tools."""
    return [
        create_rust_lint_tool(),
        # Add more tools here
    ]
```

### 2. Update Agent Creation

Update the agent creation to include the new tools:

```python
# agents/developer_agent.py
from tools.rust_tools import create_rust_tools

def create_developer_agent() -> Agent:
    """Creates an agent specialized in implementing Rust web applications."""
    
    return Agent(
        role="Rust Developer",
        goal="Implement high-quality, functional, and efficient Rust code for web applications",
        backstory="""You are an expert Rust programmer with years of experience in web development. 
        You have mastered functional programming paradigms and know how to write clean, maintainable, 
        and efficient Rust code.""",
        verbose=True,
        allow_delegation=True,
        tools=create_rust_tools(),  # Add the tools here
        llm_config={
            "temperature": 0.7,
            "model": "gpt-4o"
        }
    )
```

## Customizing Agent Behavior

You can customize agent behavior by adjusting their parameters:

### Temperature

Adjust the `temperature` parameter to control creativity vs. determinism:

```python
# More deterministic (good for technical tasks)
llm_config={
    "temperature": 0.3,
    "model": "gpt-4o"
}

# More creative (good for design tasks)
llm_config={
    "temperature": 0.8,
    "model": "gpt-4o"
}
```

### Agent Collaboration

Adjust the `allow_delegation` parameter to control whether agents can delegate tasks:

```python
# Allow delegation to other agents
allow_delegation=True

# Prevent delegation (agent must complete all tasks itself)
allow_delegation=False
```

## Adding New Rust Templates

You can add new Rust web application templates for different frameworks:

### 1. Create a New Template Directory

Create a new directory in the `templates` directory:

```
templates/
u251cu2500u2500 rust-web-app/        # Existing Scotty template
u2514u2500u2500 rust_yesod_app/      # New Yesod template
```

### 2. Add Template Files

Add the necessary files for the new template, following the same structure as existing templates.

### 3. Update the Developer Tasks

Update the developer tasks to use the appropriate template based on the architecture design:

```python
# tasks/developer_tasks.py
def create_developer_tasks(agent, project_name: str, design_result: str, project_dir: str) -> List[Task]:
    """Creates tasks for the Rust developer agent."""
    
    implementation_dir = os.path.join(project_dir, "implementation")
    os.makedirs(implementation_dir, exist_ok=True)
    
    implementation_report = os.path.join(project_dir, "implementation_report.md")
    
    # Determine which template to use based on the design result
    template_dir = "rust-web-app"  # Default template
    if "yesod" in design_result.lower():
        template_dir = "rust_yesod_app"
    elif "servant" in design_result.lower():
        template_dir = "rust_servant_app"
    
    return [
        Task(
            description=f"""Implement the Rust web application based on the architecture design:
            
            Project Name: {project_name}
            Architecture Design: {design_result}
            Template Directory: {template_dir}
            
            Your task is to:
            1. Set up the project structure and build system
            2. Implement the core modules and data types
            # ... rest of the task description
            """,
            agent=agent,
            expected_output="A complete implementation of the Rust web application with documentation",
            output_file=implementation_report
        )
    ]
```

## Extending the UI

You can extend the Streamlit UI to add more features:

### 1. Add New Sidebar Options

Update `app.py` to add new sidebar options:

```python
# In the sidebar section
with st.sidebar:
    st.header("Navigation")
    
    tab = st.radio(
        "View",
        ["Project Input", "Project List", "Settings"],
        index=0
    )
    
    if tab == "Project Input":
        st.session_state.current_step = "input"
    elif tab == "Project List":
        st.session_state.current_step = "project_list"
    elif tab == "Settings":
        st.session_state.current_step = "settings"
    
    st.markdown("---")
    # ... rest of the sidebar code
```

### 2. Add New Pages

Add new pages for the new sidebar options:

```python
# Add a new elif for the project list page
elif st.session_state.current_step == "project_list":
    st.header("Project List")
    
    # Get list of projects
    projects_base_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), "projects")
    if os.path.exists(projects_base_dir):
        project_dirs = [d for d in os.listdir(projects_base_dir) if os.path.isdir(os.path.join(projects_base_dir, d))]
        
        if project_dirs:
            for project_dir in project_dirs:
                with st.expander(project_dir):
                    st.write(f"Path: {os.path.join(projects_base_dir, project_dir)}")
                    if st.button("Load Project", key=f"load_{project_dir}"):
                        # Load project code here
                        st.session_state.project_name = project_dir
                        st.session_state.current_step = "results"
                        st.rerun()
        else:
            st.info("No projects found.")
    else:
        st.info("Projects directory not found.")

# Add a new elif for the settings page
elif st.session_state.current_step == "settings":
    st.header("Settings")
    
    with st.form("settings_form"):
        model = st.selectbox(
            "LLM Model",
            ["gpt-4o", "gpt-3.5-turbo"],
            index=0
        )
        
        temperature = st.slider(
            "Temperature",
            min_value=0.0,
            max_value=1.0,
            value=0.7,
            step=0.1
        )
        
        save_settings = st.form_submit_button("Save Settings")
        
        if save_settings:
            # Save settings to session state
            st.session_state.settings = {
                "model": model,
                "temperature": temperature
            }
            st.success("Settings saved!")
```

## Integration with External Services

You can integrate the system with external services for additional functionality:

### 1. GitHub Integration

Add GitHub integration to push generated code to a repository:

```python
# utils/github_integration.py
import os
import subprocess
from typing import Optional

class GitHubIntegration:
    def __init__(self, token: Optional[str] = None):
        self.token = token or os.environ.get("GITHUB_TOKEN")
    
    def initialize_repo(self, project_dir: str, repo_name: str) -> bool:
        """Initialize a git repository and set up remote."""
        try:
            # Initialize git repo
            subprocess.run(["git", "init"], cwd=project_dir, check=True)
            
            # Create .gitignore
            with open(os.path.join(project_dir, ".gitignore"), "w") as f:
                f.write("dist\ndist-*\ncabal-dev\n*.o\n*.hi\n*.hie\n*.chi\n*.chs.h\n*.dyn_o\n*.dyn_hi\n.hpc\n.hsenv\n.cabal-sandbox/\ncabal.sandbox.config\n*.prof\n*.aux\n*.hp\n*.eventlog\n.stack-work/\ncabal.project.local\ncabal.project.local~\n.HTF/\n.ghc.environment.*\n")
            
            # Add all files
            subprocess.run(["git", "add", "."], cwd=project_dir, check=True)
            
            # Initial commit
            subprocess.run(["git", "commit", "-m", "Initial commit"], cwd=project_dir, check=True)
            
            # Set up remote if token is available
            if self.token:
                remote_url = f"https://{self.token}@github.com/username/{repo_name}.git"
                subprocess.run(["git", "remote", "add", "origin", remote_url], cwd=project_dir, check=True)
                return True
            else:
                return False
        except Exception as e:
            print(f"Error initializing repo: {str(e)}")
            return False
    
    def push_to_github(self, project_dir: str) -> bool:
        """Push the repository to GitHub."""
        try:
            if self.token:
                subprocess.run(["git", "push", "-u", "origin", "main"], cwd=project_dir, check=True)
                return True
            else:
                return False
        except Exception as e:
            print(f"Error pushing to GitHub: {str(e)}")
            return False
```

### 2. Update the UI to Use GitHub Integration

Update the results page to include GitHub integration:

```python
# In the results section
st.markdown("---")
st.subheader("Export Options")

col1, col2 = st.columns(2)

with col1:
    if st.button("Download as ZIP"):
        # Code to create and download ZIP file
        pass

with col2:
    github_token = st.text_input("GitHub Token (optional)", type="password")
    repo_name = st.text_input("Repository Name", value=st.session_state.project_name)
    
    if st.button("Push to GitHub"):
        if github_token:
            from utils.github_integration import GitHubIntegration
            
            github = GitHubIntegration(token=github_token)
            project_dir = st.session_state.project_manager.get_project_directory(st.session_state.project_name)
            
            with st.spinner("Initializing repository..."):
                if github.initialize_repo(project_dir, repo_name):
                    if github.push_to_github(project_dir):
                        st.success(f"Successfully pushed to GitHub: username/{repo_name}")
                    else:
                        st.error("Failed to push to GitHub.")
                else:
                    st.error("Failed to initialize repository.")
        else:
            st.warning("GitHub token is required.")
```

## Conclusion

These extensions and customizations will enhance the Rust web development multi-agent system with additional capabilities. You can implement them incrementally based on your specific needs and priorities.

Remember to update the documentation and README.md file to reflect any changes you make to the system.
