import os
from typing import Any, Dict


def get_minimal_azure_config() -> Dict[str, Any]:
    return {
        "model": os.getenv("AZURE_API_MODEL"),
        "api_key": os.getenv("AZURE_API_KEY"),
        "api_type": "azure",
    }


# Create a client configuration that works with AutoGen
def get_autogen_config() -> Dict[str, Any]:
    return {
        "config_list": [
            {
                "model": AZURE_API_MODEL,
                "api_key": AZURE_API_KEY,
                "api_type": "azure",
            }
        ],
        "temperature": 0.1,
    }
