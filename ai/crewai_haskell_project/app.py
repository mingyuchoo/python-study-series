import os
import time

import streamlit as st
from crewai import Agent, Crew, Process, Task
from dotenv import load_dotenv

from agents.analyzer_agent import create_analyzer_agent
from agents.deployer_agent import create_deployer_agent
from agents.designer_agent import create_designer_agent
from agents.developer_agent import create_developer_agent
from agents.tester_agent import create_tester_agent
from tasks.analyzer_tasks import create_analyzer_tasks
from tasks.deployer_tasks import create_deployer_tasks
from tasks.designer_tasks import create_designer_tasks
from tasks.developer_tasks import create_developer_tasks
from tasks.tester_tasks import create_tester_tasks
from utils.project_manager import ProjectManager

# Load environment variables from .env file
load_dotenv()


# Set default values for OpenAI API configuration if not in environment
if not os.environ.get("OPENAI_API_TEMPERATURE"):
    os.environ["OPENAI_API_TEMPERATURE"] = "1.0"

if not os.environ.get("OPENAI_API_MAX_TOKENS"):
    os.environ["OPENAI_API_MAX_TOKENS"] = "4096"

if not os.environ.get("OPENAI_API_TOP_P"):
    os.environ["OPENAI_API_TOP_P"] = "1.0"

if not os.environ.get("OPENAI_API_STREAM"):
    os.environ["OPENAI_API_STREAM"] = "True"


# Function to validate OpenAI API key
def is_valid_openai_key(api_key):
    if not api_key or len(api_key) < 20:  # Basic validation
        return False

    # Additional validation can be added here if needed
    return True


# Set page configuration
st.set_page_config(
    page_title="Haskell Web Development Team",
    page_icon="🧩",
    layout="wide",
    initial_sidebar_state="expanded",
)

# Custom CSS for modern UI
st.markdown(
    """
<style>
    .main .block-container {
        padding-top: 2rem;
        padding-bottom: 2rem;
    }
    .stApp {
        background-color: #f8f9fa;
    }
    h1, h2, h3 {
        color: #2c3e50;
    }
    .stButton>button {
        background-color: #3498db;
        color: white;
        border-radius: 5px;
        padding: 0.5rem 1rem;
        font-weight: bold;
    }
    .stButton>button:hover {
        background-color: #2980b9;
    }
    .stTextInput>div>div>input {
        border-radius: 5px;
    }
    .stSelectbox>div>div>select {
        border-radius: 5px;
    }
    .css-1d391kg, .css-12oz5g7 {
        padding: 2rem 1rem;
        border-radius: 10px;
        background-color: white;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        margin-bottom: 1rem;
    }
</style>
""",
    unsafe_allow_html=True,
)

# Application title
st.title("🧩 Haskell Web Development Multi-Agent System")
st.markdown(
    """This application uses CrewAI to implement a multi-agent collaboration system 
            focused on developing Haskell web applications. Multiple agents with specialized roles 
            work together to analyze requirements, design, implement, test, and deploy web applications 
            written in Haskell."""
)

# Initialize session state
if "project_manager" not in st.session_state:
    st.session_state.project_manager = ProjectManager()

if "current_step" not in st.session_state:
    st.session_state.current_step = "input"

if "project_name" not in st.session_state:
    st.session_state.project_name = ""

if "project_description" not in st.session_state:
    st.session_state.project_description = ""

if "results" not in st.session_state:
    st.session_state.results = {}

# All API configuration is now loaded from environment variables

if "error_message" not in st.session_state:
    st.session_state.error_message = None


# Function to reset the application state
def reset_app():
    st.session_state.current_step = "input"
    st.session_state.project_name = ""
    st.session_state.project_description = ""
    st.session_state.results = {}
    st.session_state.error_message = None


# Sidebar
with st.sidebar:
    st.header("Navigation")

    if st.button("Start New Project"):
        reset_app()

    st.markdown("---")
    st.markdown("### Project Steps")

    step_status = {
        "Requirements Analysis": (
            "✅" if "analysis" in st.session_state.results else "⏳"
        ),
        "Design": "✅" if "design" in st.session_state.results else "⏳",
        "Implementation": (
            "✅" if "implementation" in st.session_state.results else "⏳"
        ),
        "Testing": "✅" if "testing" in st.session_state.results else "⏳",
        "Deployment": "✅" if "deployment" in st.session_state.results else "⏳",
    }

    for step, status in step_status.items():
        st.markdown(f"{status} {step}")

# Main content area based on current step
if st.session_state.current_step == "input":
    st.header("Project Information")

    # Display any error message
    if st.session_state.error_message:
        st.error(st.session_state.error_message)
        st.session_state.error_message = None

    with st.form("project_form"):
        st.session_state.project_name = st.text_input(
            "Project Name", value=st.session_state.project_name
        )
        st.session_state.project_description = st.text_area(
            "Project Description",
            value=st.session_state.project_description,
            height=150,
            help="Describe the Haskell web application you want to build.",
        )

        submitted = st.form_submit_button("Start Development Process")

        if submitted:
            if (
                not st.session_state.project_name
                or not st.session_state.project_description
            ):
                st.error("Please fill in the project name and description.")
            elif not is_valid_openai_key(os.environ.get("OPENAI_API_KEY")):
                st.error("Please provide a valid OpenAI API key in your .env file.")
            else:
                # Environment variables are already loaded from .env file
                # Just proceed with the development process
                if "OPENAI_API_STREAM" in os.environ:
                    os.environ["OPENAI_API_STREAM"] = os.environ.get(
                        "OPENAI_API_STREAM"
                    )

                st.session_state.current_step = "processing"
                st.rerun()

elif st.session_state.current_step == "processing":
    st.header("Development in Progress")

    # Create project directory
    project_dir = st.session_state.project_manager.create_project_directory(
        st.session_state.project_name
    )

    # Display progress
    progress_bar = st.progress(0)
    status_text = st.empty()

    try:
        # Create agents with the configured API settings
        status_text.text("Creating agents...")

        analyzer_agent = create_analyzer_agent()
        designer_agent = create_designer_agent()
        developer_agent = create_developer_agent()
        tester_agent = create_tester_agent()
        deployer_agent = create_deployer_agent()

        # Create tasks
        status_text.text("Defining tasks...")
        progress_bar.progress(10)

        analyzer_tasks = create_analyzer_tasks(
            analyzer_agent,
            st.session_state.project_name,
            st.session_state.project_description,
            project_dir,
        )
        progress_bar.progress(20)

        # Run analysis
        status_text.text("Analyzing requirements...")
        analysis_crew = Crew(
            agents=[analyzer_agent],
            tasks=analyzer_tasks,
            verbose=True,
            process=Process.sequential,
        )

        try:
            analysis_result = analysis_crew.kickoff()
            st.session_state.results["analysis"] = analysis_result
            progress_bar.progress(30)

            # Design phase
            status_text.text("Designing application...")
            designer_tasks = create_designer_tasks(
                designer_agent,
                st.session_state.project_name,
                analysis_result,
                project_dir,
            )
            design_crew = Crew(
                agents=[designer_agent],
                tasks=designer_tasks,
                verbose=True,
                process=Process.sequential,
            )
            design_result = design_crew.kickoff()
            st.session_state.results["design"] = design_result
            progress_bar.progress(50)

            # Implementation phase
            status_text.text("Implementing application...")
            developer_tasks = create_developer_tasks(
                developer_agent,
                st.session_state.project_name,
                design_result,
                project_dir,
            )
            development_crew = Crew(
                agents=[developer_agent],
                tasks=developer_tasks,
                verbose=True,
                process=Process.sequential,
            )
            implementation_result = development_crew.kickoff()
            st.session_state.results["implementation"] = implementation_result
            progress_bar.progress(70)

            # Testing phase
            status_text.text("Testing application...")
            tester_tasks = create_tester_tasks(
                tester_agent,
                st.session_state.project_name,
                implementation_result,
                project_dir,
            )
            testing_crew = Crew(
                agents=[tester_agent],
                tasks=tester_tasks,
                verbose=True,
                process=Process.sequential,
            )
            testing_result = testing_crew.kickoff()
            st.session_state.results["testing"] = testing_result
            progress_bar.progress(85)

            # Deployment phase
            status_text.text("Preparing deployment...")
            deployer_tasks = create_deployer_tasks(
                deployer_agent,
                st.session_state.project_name,
                testing_result,
                project_dir,
            )
            deployment_crew = Crew(
                agents=[deployer_agent],
                tasks=deployer_tasks,
                verbose=True,
                process=Process.sequential,
            )
            deployment_result = deployment_crew.kickoff()
            st.session_state.results["deployment"] = deployment_result
            progress_bar.progress(100)

            # Complete
            status_text.text("Development completed!")
            st.session_state.current_step = "results"
            st.rerun()

        except Exception as e:
            # Handle API errors during agent execution
            error_message = str(e)

            if (
                "AuthenticationError" in error_message
                or "invalid_api_key" in error_message
            ):
                st.session_state.error_message = (
                    "Authentication error: Please check your OpenAI API key."
                )
            elif "RateLimitError" in error_message:
                st.session_state.error_message = "Rate limit exceeded: Your OpenAI API key has reached its rate limit. Please try again later."
            elif "timeout" in error_message.lower():
                st.session_state.error_message = "Request timed out: The OpenAI API request timed out. Please try again."
            else:
                st.session_state.error_message = f"An error occurred: {error_message}"

            # Go back to input step
            st.session_state.current_step = "input"
            st.rerun()

    except Exception as e:
        # Handle errors during setup
        st.error(f"Error setting up the development process: {str(e)}")
        st.session_state.error_message = (
            f"Error setting up the development process: {str(e)}"
        )
        st.session_state.current_step = "input"
        time.sleep(3)  # Give user time to see the error
        st.rerun()

elif st.session_state.current_step == "results":
    st.header("Development Results")

    # Display project information
    st.subheader("Project Information")
    st.write(f"**Project Name:** {st.session_state.project_name}")
    st.write(f"**Project Description:** {st.session_state.project_description}")

    # Display results for each phase
    st.markdown("---")

    tabs = st.tabs(["Analysis", "Design", "Implementation", "Testing", "Deployment"])

    with tabs[0]:
        if "analysis" in st.session_state.results:
            st.markdown("### Requirements Analysis")
            st.markdown(st.session_state.results["analysis"])

    with tabs[1]:
        if "design" in st.session_state.results:
            st.markdown("### Design")
            st.markdown(st.session_state.results["design"])

    with tabs[2]:
        if "implementation" in st.session_state.results:
            st.markdown("### Implementation")
            st.markdown(st.session_state.results["implementation"])

    with tabs[3]:
        if "testing" in st.session_state.results:
            st.markdown("### Testing")
            st.markdown(st.session_state.results["testing"])

    with tabs[4]:
        if "deployment" in st.session_state.results:
            st.markdown("### Deployment")
            st.markdown(st.session_state.results["deployment"])

    # Project files
    st.markdown("---")
    st.subheader("Project Files")

    project_dir = st.session_state.project_manager.get_project_directory(
        st.session_state.project_name
    )
    if os.path.exists(project_dir):
        files = os.listdir(project_dir)
        if files:
            for file in files:
                if os.path.isfile(os.path.join(project_dir, file)):
                    with st.expander(f"📄 {file}"):
                        with open(os.path.join(project_dir, file), "r") as f:
                            st.code(f.read())
        else:
            st.info("No files generated yet.")

    # Start new project button
    if st.button("Start New Project", key="new_project_btn"):
        reset_app()
        st.rerun()
