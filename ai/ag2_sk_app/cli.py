import json
import os
from autogen import AssistantAgent, UserProxyAgent
from semantic_kernel import Kernel
from semantic_kernel.connectors.ai.open_ai import AzureChatCompletion

# Load configuration from OAI_CONFIG_LIST file
def load_config():
    try:
        with open('OAI_CONFIG_LIST.json', 'r') as file:
            return json.load(file)
    except FileNotFoundError:
        raise FileNotFoundError("Configuration file 'OAI_CONFIG_LIST.json' not found. Please ensure it exists in the current directory.")
    except json.JSONDecodeError:
        raise ValueError("Invalid JSON format in 'OAI_CONFIG_LIST.json'. Please check the file content.")

# Semantic Kernel setup with Azure OpenAI
def setup_kernel():
    config = load_config()[0]  # Use the first configuration
    kernel = Kernel()
    # Add Azure OpenAI chat service
    try:
        kernel.add_service(
            AzureChatCompletion(
                service_id="azure_chat",
                deployment_name=config["model"],
                endpoint=config["base_url"],
                api_key=config["api_key"],
                api_version=config["api_version"]
            )
        )
    except Exception as e:
        raise ConnectionError(f"Failed to connect to Azure OpenAI: {str(e)}")
    # Create a simple custom plugin directly
    from semantic_kernel.functions import kernel_function
    
    class OfficePlugin:
        @kernel_function(
            description="Generate a formatted report based on user input with customizable sections",
            name="GenerateReport"
        )
        def generate_report(self, input_text: str, report_title: str = "Project Report", include_sections: str = "Executive Summary,Key Points,Conclusion") -> str:
            sections = include_sections.split(",")
            report_content = f"# {report_title}\n\n"
            if "Executive Summary" in sections:
                report_content += f"## Executive Summary\nThis report is based on: {input_text}\n\n"
            if "Key Points" in sections:
                report_content += f"## Key Points\n- Point 1\n- Point 2\n\n"
            if "Conclusion" in sections:
                report_content += f"## Conclusion\nSummary of the report."
            return report_content
    
    kernel.add_plugin(plugin=OfficePlugin(), plugin_name="OfficePlugin")
    return kernel

# AutoGen agent setup
def setup_agents():
    config = load_config()[0]
    llm_config = {
        "model": config["model"],
        "api_key": config["api_key"],
        "base_url": config["base_url"],
        "api_type": config["api_type"],
        "api_version": config["api_version"]
    }
    
    assistant = AssistantAgent(
        name="Assistant",
        llm_config=llm_config,
        system_message="""You are an AI assistant that can use tools and plugins to help the user.
        Use the OfficePlugin when the user asks for document creation or report generation.
        Ask the user for specific preferences such as report title or sections to include before generating the report."""
    )
    
    user_proxy = UserProxyAgent(
        name="User",
        code_execution_config={"work_dir": "coding", "use_docker": False},
        human_input_mode="TERMINATE"
    )
    
    return assistant, user_proxy

# Function to call Semantic Kernel plugin from AutoGen
def call_office_plugin(kernel, request, report_title="Project Report", include_sections="Executive Summary,Key Points,Conclusion"):
    plugin = kernel.plugins["OfficePlugin"]
    arguments = {
        "input_text": request,
        "report_title": report_title,
        "include_sections": include_sections
    }
    import asyncio
    result = asyncio.run(kernel.invoke(plugin["GenerateReport"], **arguments))
    # Log the request and result for debugging
    with open("report_generation_log.txt", "a") as log_file:
        log_file.write(f"Request: {request}\nTitle: {report_title}\nSections: {include_sections}\nResult: {result}\n---\n")
    return str(result)

# Main execution
def main():
    try:
        kernel = setup_kernel()
        assistant, user_proxy = setup_agents()
        
        # Register the Semantic Kernel plugin as a callable function for AutoGen
        assistant.register_function(
            function_map={
                "generate_report": lambda request, report_title="Project Report", include_sections="Executive Summary,Key Points,Conclusion": str(call_office_plugin(kernel, request, report_title, include_sections))
            }
        )
        
        print("Welcome to the AI Assistant Report Generator.")
        print("Type 'exit' or 'quit' to end the conversation.")
        print("You can ask for help in creating reports or documents.")
        
        # Start an interactive loop for real user input
        while True:
            try:
                user_input = input("\nUser: ")
                if user_input.lower() in ["exit", "quit"]:
                    print("Goodbye!")
                    break
                
                # Initiate chat with the user's input
                chat_result = user_proxy.initiate_chat(
                    assistant,
                    message=user_input,
                    max_turns=2  # Limit the number of turns if needed
                )
                
                # The assistant's response will be printed automatically by AutoGen
                # No need for additional printing here
                
            except KeyboardInterrupt:
                print("\nReceived interrupt. Goodbye!")
                break
            except Exception as inner_e:
                print(f"An error occurred during interaction: {str(inner_e)}")
                with open("error_log.txt", "a") as error_file:
                    error_file.write(f"Interaction Error: {str(inner_e)}\n")
    except Exception as e:
        print(f"An error occurred: {str(e)}")
        with open("error_log.txt", "a") as error_file:
            error_file.write(f"Error: {str(e)}\n")

if __name__ == "__main__":
    main()
