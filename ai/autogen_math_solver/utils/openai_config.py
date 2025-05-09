import os

import openai
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

# Azure OpenAI API Configuration


# Tavily API Configuration
TAVILY_API_KEY = os.getenv("TAVILY_API_KEY")

from typing import Any, Dict


def get_azure_openai_client() -> Any:
    """Create and return an Azure OpenAI client (함수형, 환경변수 직접 참조)"""
    openai.api_type = "azure"
    openai.api_key = os.getenv("AZURE_API_KEY")
    openai.api_base = os.getenv("AZURE_API_BASE")
    openai.api_version = os.getenv("AZURE_API_VERSION")
    return openai


def get_azure_openai_config() -> Dict[str, Any]:
    """Return Azure OpenAI configuration as a dictionary compatible with OpenAI 0.28.1 and AutoGen 0.1.14 (함수형, 환경변수 직접 참조)"""
    model = os.getenv("AZURE_API_MODEL")
    api_base = os.getenv("AZURE_API_BASE")
    api_key = os.getenv("AZURE_API_KEY")
    api_version = os.getenv("AZURE_API_VERSION")
    return {
        "model": model,
        "engine": model,  # For Azure OpenAI with older SDK
        "api_type": "azure",
        "api_base": api_base,
        "api_key": api_key,
        "api_version": api_version,
    }
