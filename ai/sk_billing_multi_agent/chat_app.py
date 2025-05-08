import asyncio
import os

from dotenv import load_dotenv
from semantic_kernel.agents import ChatCompletionAgent
from semantic_kernel.connectors.ai.open_ai import AzureChatCompletion

# Load environment variables
load_dotenv()

# Azure OpenAI Configuration
azure_endpoint = os.getenv("AZURE_OPENAI_ENDPOINT")
azure_api_key = os.getenv("AZURE_OPENAI_API_KEY")
azure_deployment = os.getenv("AZURE_OPENAI_CHAT_DEPLOYMENT_NAME")

# Create service configurations
azure_service = AzureChatCompletion(deployment_name=azure_deployment, endpoint=azure_endpoint, api_key=azure_api_key)

# Create agent instances
billing_agent = ChatCompletionAgent(
    service=azure_service,
    name="BillingAgent",
    instructions="You handle billing issues like charges, payment methods, cycles, fees, discrepancies, and payment failures.",
)

refund_agent = ChatCompletionAgent(
    service=azure_service,
    name="RefundAgent",
    instructions="Assist users with refund inquiries, including eligibility, policies, processing, and status updates.",
)

# Create triage agent with billing and refund agents as plugins
triage_agent = ChatCompletionAgent(
    service=azure_service,
    name="TriageAgent",
    instructions="Evaluate user requests and forward them to BillingAgent or RefundAgent for targeted assistance."
    " Provide the full answer to the user containing any information from the agents",
    plugins=[billing_agent, refund_agent],
)

# Initialize conversation state
thread = None


async def main() -> None:
    print("Welcome to the chat bot!\n  Type 'exit' to exit.\n  Try to get some billing or refund help.")

    while True:
        user_input = input("User:> ")

        if user_input.lower().strip() == "exit":
            print("\n\nExiting chat...")
            return False

        # Get response from the triage agent - pass user message directly
        response = await triage_agent.get_response(messages=user_input, thread=thread)

        # Display the response
        if response:
            print(f"Agent :> {response}")


if __name__ == "__main__":
    asyncio.run(main())
