import os
import random
import time
# Create a rate limiter for Azure OpenAI API calls
# Adjust these values based on your Azure OpenAI tier limits
from typing import Any, Callable

import autogen
import openai

from utils.openai_config import get_azure_openai_config


def rate_limiter_factory(max_requests: int = 2, time_period: int = 10) -> Callable[[], None]:
    """함수형 rate limiter 상태를 생성해 반환합니다."""
    state = {"request_times": []}

    def wait_if_needed():
        current_time = time.time()
        state["request_times"] = [t for t in state["request_times"] if current_time - t <= time_period]
        if len(state["request_times"]) >= max_requests:
            oldest_request = min(state["request_times"])
            wait_time = time_period - (current_time - oldest_request)
            if wait_time > 0:
                print(f"Rate limit reached. Waiting for {wait_time:.2f} seconds...")
                time.sleep(wait_time)
        state["request_times"].append(time.time())

    return wait_if_needed


def retry_with_exponential_backoff(
    max_retries: int = 5, initial_backoff: int = 8, max_backoff: int = 60
) -> Callable[[Callable[..., Any]], Callable[..., Any]]:
    def decorator(func):
        def wrapper(*args, **kwargs):
            backoff = initial_backoff
            for _ in range(max_retries):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    print(f"Error: {e}. Retrying in {backoff:.2f} seconds...")
                    time.sleep(backoff)
                    backoff = min(backoff * 2, max_backoff)
            raise Exception("Max retries exceeded")

        return wrapper

    return decorator


# Patch the OpenAI API to use our rate limiter and retry logic
original_create = openai.ChatCompletion.create

rate_limiter = rate_limiter_factory()


@retry_with_exponential_backoff(max_retries=5, initial_backoff=8, max_backoff=60)
def rate_limited_create(*args: Any, **kwargs: Any) -> Any:
    rate_limiter()
    return original_create(*args, **kwargs)


# Apply the patch
openai.ChatCompletion.create = rate_limited_create


def create_azure_openai_wrapper() -> dict:
    """
    Create a configuration for AutoGen that works with OpenAI 0.28.1 and Azure OpenAI.
    Includes rate limiting and retry logic for handling Azure OpenAI rate limits.
    """
    try:
        # Get configuration for Azure OpenAI compatible with OpenAI 0.28.1
        config = get_azure_openai_config()

        # Configure OpenAI for Azure
        openai.api_type = "azure"
        openai.api_key = os.getenv("AZURE_API_KEY")
        openai.api_base = os.getenv("AZURE_API_BASE")
        openai.api_version = os.getenv("AZURE_API_VERSION")

        # Return a configuration that works with AutoGen 0.1.14
        return {
            "config_list": [config],
            "temperature": 0.1,
            # cache_seed is not supported in OpenAI 0.28.1
        }
    except Exception as e:
        print(f"Error creating Azure OpenAI configuration: {e}")
        # Fallback to a simple configuration
        return {
            "config_list": [
                {
                    "model": os.getenv("AZURE_API_MODEL", "gpt-4o"),
                    "engine": os.getenv("AZURE_API_MODEL", "gpt-4o"),
                    "api_type": "azure",
                    "api_base": os.getenv("AZURE_API_BASE"),
                    "api_key": os.getenv("AZURE_API_KEY"),
                    "api_version": os.getenv("AZURE_API_VERSION"),
                }
            ],
            "temperature": 0.1,
        }
