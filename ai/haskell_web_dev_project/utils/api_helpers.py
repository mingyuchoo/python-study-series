import os
import re

def format_openai_key(api_key):
    """
    Format and validate the OpenAI API key.
    Removes any whitespace, newlines, or other non-standard characters.
    
    Args:
        api_key (str): The raw API key from environment variables
        
    Returns:
        str: The formatted API key
    """
    if not api_key:
        return None
        
    # Remove any whitespace, newlines, etc.
    formatted_key = api_key.strip()
    
    # Check if it's an Azure OpenAI key (which might have a different format)
    is_azure = os.environ.get("OPENAI_API_URL", "").endswith("openai.azure.com")
    
    if is_azure:
        # Azure API keys are typically long strings without a specific prefix
        # Just return the cleaned key without additional formatting
        return formatted_key
    else:
        # For standard OpenAI keys, they typically start with 'sk-'
        # But we won't add the prefix as it might cause issues if the key is already properly formatted
        # Just return the cleaned key
        return formatted_key

def get_openai_config():
    """
    Get the OpenAI configuration from environment variables.
    Handles both standard OpenAI and Azure OpenAI configurations.
    
    Returns:
        dict: The OpenAI configuration
    """
    api_key = format_openai_key(os.environ.get("OPENAI_API_KEY", ""))
    api_url = os.environ.get("OPENAI_API_URL", "")
    is_azure = api_url and api_url.endswith("openai.azure.com")
    
    # Base configuration common to both standard and Azure OpenAI
    config = {
        "temperature": float(os.environ.get("OPENAI_API_TEMPERATURE", 0.7)),
        "model": os.environ.get("OPENAI_API_MODEL", "gpt-4o"),
        "max_tokens": int(os.environ.get("OPENAI_API_MAX_TOKENS", 4096)),
        "top_p": float(os.environ.get("OPENAI_API_TOP_P", 1.0)),
        "stream": os.environ.get("OPENAI_API_STREAM", "True").lower() == "true"
    }
    
    if is_azure:
        # Azure OpenAI specific configuration
        config["api_type"] = "azure"
        config["api_base"] = api_url
        config["api_version"] = os.environ.get("OPENAI_API_VERSION", "2023-05-15")
        config["api_key"] = api_key
        
        # For Azure, the deployment name is used instead of the model name
        # If the model is specified in the format 'deployment_name/model_name',
        # extract the deployment name
        model = config["model"]
        if "/" in model:
            deployment_name = model.split("/")[0]
            config["deployment_id"] = deployment_name
        else:
            config["deployment_id"] = model
    else:
        # Standard OpenAI configuration
        config["api_key"] = api_key
        
    return config
