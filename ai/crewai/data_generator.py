import os
import random
from datetime import datetime, timedelta

import numpy as np
import pandas as pd


def generate_sales_data(periods=12, start_date=None):
    """
    Generate sample quarterly sales data for demonstration purposes

    Args:
        periods (int): Number of time periods (months) to generate
        start_date (datetime, optional): Starting date for the data

    Returns:
        pd.DataFrame: DataFrame containing the generated sales data
    """
    if start_date is None:
        start_date = datetime.now() - timedelta(days=periods * 30)

    # Create date range
    date_range = pd.date_range(start=start_date, periods=periods, freq="ME")

    # Product categories
    categories = ["Electronics", "Clothing", "Home Goods", "Food & Beverage", "Beauty"]

    # Regions
    regions = ["North", "South", "East", "West", "Central"]

    # Generate data
    data = []

    for date in date_range:
        for category in categories:
            for region in regions:
                # Base sales with some randomness
                base_sales = random.randint(10000, 50000)

                # Add seasonal trends
                month = date.month
                if month in [11, 12]:  # Holiday season
                    seasonal_factor = 1.5
                elif month in [6, 7, 8]:  # Summer
                    seasonal_factor = (
                        1.2 if category in ["Clothing", "Food & Beverage"] else 1.0
                    )
                else:
                    seasonal_factor = 1.0

                # Category-specific trends
                if category == "Electronics":
                    category_factor = 1.3
                elif category == "Home Goods":
                    category_factor = 0.9
                else:
                    category_factor = 1.0

                # Region-specific trends
                if region == "North":
                    region_factor = 1.1
                elif region == "South":
                    region_factor = 0.95
                else:
                    region_factor = 1.0

                # Calculate final sales
                sales = int(
                    base_sales * seasonal_factor * category_factor * region_factor
                )

                # Calculate profit (random percentage of sales)
                profit_margin = random.uniform(0.15, 0.35)
                profit = int(sales * profit_margin)

                # Calculate units sold
                avg_price = random.randint(20, 200)
                units = int(sales / avg_price)

                # Add some random marketing spend
                marketing_spend = int(sales * random.uniform(0.05, 0.15))

                data.append(
                    {
                        "Date": date,
                        "Year": date.year,
                        "Month": date.month,
                        "Quarter": (date.month - 1) // 3 + 1,
                        "Category": category,
                        "Region": region,
                        "Sales": sales,
                        "Profit": profit,
                        "Units": units,
                        "Marketing_Spend": marketing_spend,
                        "Profit_Margin": profit_margin,
                    }
                )

    # Create DataFrame
    df = pd.DataFrame(data)

    return df


def generate_market_trends(periods=12, start_date=None):
    """
    Generate sample market trend data

    Args:
        periods (int): Number of time periods (months) to generate
        start_date (datetime, optional): Starting date for the data

    Returns:
        pd.DataFrame: DataFrame containing the generated market trend data
    """
    if start_date is None:
        start_date = datetime.now() - timedelta(days=periods * 30)

    # Create date range
    date_range = pd.date_range(start=start_date, periods=periods, freq="ME")

    # Industries
    industries = ["Retail", "Technology", "Healthcare", "Finance", "Manufacturing"]

    # Generate data
    data = []

    # Base market growth rates
    base_growth_rates = {
        "Retail": 0.02,
        "Technology": 0.05,
        "Healthcare": 0.03,
        "Finance": 0.02,
        "Manufacturing": 0.01,
    }

    for date in date_range:
        for industry in industries:
            # Base growth rate with some randomness
            base_rate = base_growth_rates[industry]
            growth_rate = base_rate + random.uniform(-0.01, 0.01)

            # Add seasonal trends
            month = date.month
            if month in [11, 12]:  # Holiday season
                seasonal_factor = 0.01 if industry == "Retail" else 0.0
            elif month in [1, 2]:  # Post-holiday slump
                seasonal_factor = -0.01 if industry == "Retail" else 0.0
            else:
                seasonal_factor = 0.0

            # Final growth rate
            final_growth_rate = growth_rate + seasonal_factor

            # Market size (billions)
            market_size = random.uniform(50, 500)

            # Consumer confidence (0-100)
            consumer_confidence = random.uniform(60, 90)

            # Competitive intensity (1-10)
            competitive_intensity = random.uniform(3, 9)

            data.append(
                {
                    "Date": date,
                    "Year": date.year,
                    "Month": date.month,
                    "Quarter": (date.month - 1) // 3 + 1,
                    "Industry": industry,
                    "Growth_Rate": final_growth_rate,
                    "Market_Size_Billions": market_size,
                    "Consumer_Confidence": consumer_confidence,
                    "Competitive_Intensity": competitive_intensity,
                }
            )

    # Create DataFrame
    df = pd.DataFrame(data)

    return df


def generate_customer_feedback(n_samples=500):
    """
    Generate sample customer feedback data

    Args:
        n_samples (int): Number of feedback samples to generate

    Returns:
        pd.DataFrame: DataFrame containing the generated customer feedback data
    """
    # Product categories
    categories = ["Electronics", "Clothing", "Home Goods", "Food & Beverage", "Beauty"]

    # Customer segments
    segments = ["New", "Returning", "Loyal", "VIP"]

    # Feedback types
    feedback_types = [
        "Product Quality",
        "Customer Service",
        "Price",
        "Delivery",
        "Website Experience",
    ]

    # Generate data
    data = []

    for _ in range(n_samples):
        category = random.choice(categories)
        segment = random.choice(segments)
        feedback_type = random.choice(feedback_types)

        # Base satisfaction score
        if segment == "Loyal" or segment == "VIP":
            base_score = random.uniform(3.5, 5.0)
        elif segment == "Returning":
            base_score = random.uniform(3.0, 4.5)
        else:  # New customers
            base_score = random.uniform(2.5, 5.0)

        # Adjust based on feedback type
        if feedback_type == "Price":
            adjustment = random.uniform(-1.0, 0.0)  # Price tends to get lower scores
        elif feedback_type == "Customer Service":
            adjustment = random.uniform(-0.5, 0.5)  # Variable
        else:
            adjustment = random.uniform(-0.3, 0.3)

        # Final satisfaction score (1-5)
        satisfaction = max(1.0, min(5.0, base_score + adjustment))

        # Net Promoter Score (0-10)
        if satisfaction >= 4.0:
            nps = random.randint(9, 10)  # Promoter
        elif satisfaction >= 3.0:
            nps = random.randint(7, 8)  # Passive
        else:
            nps = random.randint(0, 6)  # Detractor

        # Would recommend (binary)
        would_recommend = 1 if nps >= 9 else 0

        data.append(
            {
                "Category": category,
                "Customer_Segment": segment,
                "Feedback_Type": feedback_type,
                "Satisfaction_Score": round(satisfaction, 1),
                "NPS": nps,
                "Would_Recommend": would_recommend,
            }
        )

    # Create DataFrame
    df = pd.DataFrame(data)

    return df


def save_sample_data(output_dir="sample_data"):
    """
    Generate and save sample data files

    Args:
        output_dir (str): Directory to save the data files

    Returns:
        list: Paths to the generated data files
    """
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)

    # Generate data
    sales_data = generate_sales_data(periods=24)
    market_trends = generate_market_trends(periods=24)
    customer_feedback = generate_customer_feedback(n_samples=500)

    # Save data to CSV files
    sales_path = os.path.join(output_dir, "quarterly_sales_data.csv")
    market_path = os.path.join(output_dir, "market_trend_analysis.csv")
    feedback_path = os.path.join(output_dir, "customer_feedback_summary.csv")

    sales_data.to_csv(sales_path, index=False)
    market_trends.to_csv(market_path, index=False)
    customer_feedback.to_csv(feedback_path, index=False)

    return [sales_path, market_path, feedback_path]


if __name__ == "__main__":
    # Generate and save sample data
    file_paths = save_sample_data()
    print(f"Sample data files generated:")
    for path in file_paths:
        print(f"- {path}")
