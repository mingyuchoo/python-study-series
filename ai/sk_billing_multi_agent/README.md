# Chat App

This project is a command-line chatbot application that leverages Azure OpenAI and the Semantic Kernel framework to provide automated assistance for billing and refund inquiries. It uses a triage agent to route user questions to specialized agents for billing or refund support.

## Features
- **Conversational AI**: Interact with a chatbot for billing and refund help.
- **Agent Routing**: A triage agent forwards your queries to either a BillingAgent or RefundAgent for targeted support.
- **Azure OpenAI Integration**: Utilizes Azure OpenAI for generating responses.
- **Environment-Based Configuration**: Sensitive credentials are loaded from a `.env` file.

## Requirements
- Python 3.11+
- Azure OpenAI account and credentials

### Python Dependencies
- `semantic-kernel==1.29.0`
- `python-dotenv==1.0.0`
- `black==25.1.0` (development)
- `isort==5.13.0` (development)

Install dependencies with:
```bash
uv pip install -r requirements.txt
```

## Setup
1. Clone this repository.
2. Create a `.env` file in the project root with the following variables:
   ```env
   AZURE_OPENAI_ENDPOINT=your-endpoint-url
   AZURE_OPENAI_API_KEY=your-api-key
   AZURE_OPENAI_CHAT_DEPLOYMENT_NAME=your-deployment-name
   ```
3. Install dependencies as above.

## Usage
Run the chatbot from your terminal:
```bash
python chat_app.py
```
Type your questions related to billing or refunds. Type `exit` to quit.

## Use Cases
Here are some practical scenarios where this chatbot app can be used:

- **Billing Inquiries**: Users can ask about recent charges, payment methods, billing cycles, fees, or discrepancies in their account.
  - Example: "Why was I charged twice this month?"
  - Example: "How can I update my payment method?"
- **Refund Requests**: Users can inquire about refund policies, eligibility, or the status of a refund.
  - Example: "Am I eligible for a refund on my last purchase?"
  - Example: "How long will it take to process my refund?"
- **Automated Customer Support**: Integrate this chatbot into support workflows to automate responses for common billing and refund questions, reducing manual workload for support teams.
- **Internal Tools**: Use the chatbot internally for finance or support teams to quickly triage and resolve billing/refund-related queries.

## Project Structure
- `chat_app.py`: Main application code for the chatbot.
- `requirements.txt`: Python dependencies.
- `pyproject.toml`: Project configuration and code style settings.
- `.env`: Environment variables (not included in version control).

## License
Specify your license here.

---
*Generated on 2025-05-09.*
