import random
import time
from functools import wraps
from typing import Callable


def rate_limiter_factory(max_requests: int = 3, time_period: int = 60) -> Callable[[], None]:
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


def retry_with_exponential_backoff(max_retries: int = 5, initial_backoff: int = 1, max_backoff: int = 60) -> Callable:
    """Decorator to retry a function with exponential backoff

    Args:
        max_retries: Maximum number of retries
        initial_backoff: Initial backoff time in seconds
        max_backoff: Maximum backoff time in seconds
    """

    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            backoff = initial_backoff
            retries = 0

            while True:
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    retries += 1
                    if retries > max_retries:
                        raise

                    # Check if it's a rate limit error
                    if "rate limit" in str(e).lower() or "too many requests" in str(e).lower():
                        # Add some jitter to the backoff
                        jitter = random.uniform(0, 0.1 * backoff)
                        sleep_time = backoff + jitter
                        print(
                            f"Rate limit error. Retrying in {sleep_time:.2f} seconds... (Retry {retries}/{max_retries})"
                        )
                        time.sleep(sleep_time)

                        # Increase backoff for next retry
                        backoff = min(backoff * 2, max_backoff)
                    else:
                        # If it's not a rate limit error, just raise it
                        raise

        return wrapper

    return decorator
