# AutoGen with Semantic Kernel Integration

This application demonstrates how to integrate AutoGen agents with Semantic Kernel plugins using Azure OpenAI services.

## Setup

1. Install dependencies:

   ```bash
   pip install -r requirements.txt
   ```

2. Ensure your Azure OpenAI credentials are correctly configured in `OAI_CONFIG_LIST.json`.

## Running the Application

```bash
python main.py
```

## What This Demo Does

- Sets up a Semantic Kernel with an OfficePlugin for report generation
- Configures AutoGen agents to use Azure OpenAI
- Connects the Semantic Kernel plugin to AutoGen agents as callable functions
- Initiates a chat where the assistant can generate formatted reports using the plugin

Interact with the assistant by asking it to create reports or documents!
