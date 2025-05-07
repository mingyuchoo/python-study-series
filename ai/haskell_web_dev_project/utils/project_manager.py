import os
import re
import shutil
from datetime import datetime


class ProjectManager:
    """Utility class for managing project directories and files."""

    def __init__(self, base_dir: str = None):
        """Initialize the project manager with a base directory."""
        if base_dir is None:
            # Use the parent directory of the current file as the base directory
            current_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
            self.base_dir = os.path.join(current_dir, "projects")
        else:
            self.base_dir = base_dir

        # Create the base directory if it doesn't exist
        os.makedirs(self.base_dir, exist_ok=True)

    def create_project_directory(self, project_name: str) -> str:
        """Create a new project directory with a timestamp."""
        # Clean the project name for use as a directory name
        clean_name = self._clean_name(project_name)

        # Add timestamp to make the directory name unique
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        project_dir_name = f"{clean_name}_{timestamp}"

        # Create the full path
        project_dir = os.path.join(self.base_dir, project_dir_name)

        # Create the directory
        os.makedirs(project_dir, exist_ok=True)

        # Create subdirectories
        os.makedirs(os.path.join(project_dir, "implementation"), exist_ok=True)
        os.makedirs(os.path.join(project_dir, "tests"), exist_ok=True)
        os.makedirs(os.path.join(project_dir, "deployment"), exist_ok=True)

        return project_dir

    def get_project_directory(self, project_name: str) -> str:
        """Get the most recent project directory for a given project name."""
        clean_name = self._clean_name(project_name)

        # List all directories in the base directory
        if not os.path.exists(self.base_dir):
            return None

        all_dirs = [
            d
            for d in os.listdir(self.base_dir)
            if os.path.isdir(os.path.join(self.base_dir, d))
            and d.startswith(clean_name)
        ]

        if not all_dirs:
            return None

        # Sort by creation time (newest first)
        all_dirs.sort(
            key=lambda x: os.path.getctime(os.path.join(self.base_dir, x)), reverse=True
        )

        # Return the most recent directory
        return os.path.join(self.base_dir, all_dirs[0])

    def _clean_name(self, name: str) -> str:
        """Clean a name for use as a directory name."""
        # Replace spaces with underscores and remove special characters
        clean = re.sub(r"[^\w\s-]", "", name).strip().lower()
        clean = re.sub(r"[\s]+", "_", clean)
        return clean

    def save_file(self, project_dir: str, file_name: str, content: str) -> str:
        """Save content to a file in the project directory."""
        file_path = os.path.join(project_dir, file_name)

        # Create parent directories if they don't exist
        os.makedirs(os.path.dirname(file_path), exist_ok=True)

        with open(file_path, "w") as f:
            f.write(content)

        return file_path

    def read_file(self, project_dir: str, file_name: str) -> str:
        """Read content from a file in the project directory."""
        file_path = os.path.join(project_dir, file_name)

        if not os.path.exists(file_path):
            return None

        with open(file_path, "r") as f:
            return f.read()

    def delete_project(self, project_dir: str) -> bool:
        """Delete a project directory."""
        if not os.path.exists(project_dir):
            return False

        shutil.rmtree(project_dir)
        return True
