import argparse
import sys

from app import generate_business_report


def main():
    # Set up argument parser
    parser = argparse.ArgumentParser(
        description="Generate business reports using CrewAI multi-agent system"
    )

    # Add arguments
    parser.add_argument(
        "--topic",
        "-t",
        type=str,
        required=True,
        help="The main topic of the business report",
    )
    parser.add_argument(
        "--sources",
        "-s",
        type=str,
        nargs="+",
        help="List of data sources to analyze (optional)",
    )

    # Parse arguments
    if len(sys.argv) == 1:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args()

    # Generate the report
    generate_business_report(args.topic, args.sources)


if __name__ == "__main__":
    main()
