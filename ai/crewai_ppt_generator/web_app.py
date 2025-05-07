import os
import uuid
from pathlib import Path
from typing import List, Optional, Any

import uvicorn
from fastapi import BackgroundTasks, FastAPI, File, Form, Request, UploadFile
from fastapi.responses import FileResponse, RedirectResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates

# Import the existing functionality
from report_service import generate_business_report

app = FastAPI(title="CrewAI Business Report Generator")

# Set up static files and templates
app.mount("/static", StaticFiles(directory="static"), name="static")
templates = Jinja2Templates(directory="templates")

# Store running jobs
jobs = {}


@app.get("/")
async def home(request: Request) -> any:
    """Render the home page"""
    return templates.TemplateResponse(
        "index.html", {"request": request, "title": "CrewAI Business Report Generator"}
    )


@app.post("/generate")
async def generate_report(
    request: Request,
    background_tasks: BackgroundTasks,
    topic: str = Form(...),
    files: List[UploadFile] = File(None),
) -> RedirectResponse:
    """Generate a business report based on the provided topic and optional data files"""
    # Generate a unique job ID
    job_id = str(uuid.uuid4())

    # Create a directory for uploaded files if any
    data_sources = None
    if files and files[0].filename:
        upload_dir = Path(f"uploads/{job_id}")
        upload_dir.mkdir(parents=True, exist_ok=True)

        data_sources = []
        for file in files:
            if file.filename:
                file_path = upload_dir / file.filename
                with open(file_path, "wb") as f:
                    f.write(await file.read())
                data_sources.append(str(file_path))

    # Create output directory
    output_dir = f"my_reports/{job_id}"
    os.makedirs(output_dir, exist_ok=True)

    # Store job information
    jobs[job_id] = {
        "topic": topic,
        "status": "running",
        "output_file": None,
        "error": None,
    }

    # Run the report generation in the background
    background_tasks.add_task(
        run_report_generation, job_id, topic, data_sources, output_dir
    )

    # Redirect to the status page
    return RedirectResponse(url=f"/status/{job_id}", status_code=303)


@app.get("/status/{job_id}")
async def job_status(request: Request, job_id: str) -> any:
    """Check the status of a report generation job"""
    if job_id not in jobs:
        return templates.TemplateResponse(
            "error.html",
            {"request": request, "error": "작업을 찾을 수 없습니다."},
            status_code=404,
        )

    return templates.TemplateResponse(
        "status.html",
        {
            "request": request,
            "job_id": job_id,
            "job": jobs[job_id],
            "title": "Job Status",
        },
    )


@app.get("/api/status/{job_id}")
async def api_job_status(job_id: str) -> dict[str, any]:
    """API endpoint to check job status"""
    if job_id not in jobs:
        return {"error": "작업을 찾을 수 없습니다."}

    return jobs[job_id]


@app.get("/download/{job_id}")
async def download_report(job_id: str) -> any:
    """Download the generated report"""
    if job_id not in jobs or not jobs[job_id]["output_file"]:
        return {"error": "보고서를 찾을 수 없거나 아직 준비되지 않았습니다."}

    return FileResponse(
        path=jobs[job_id]["output_file"],
        filename=os.path.basename(jobs[job_id]["output_file"]),
        media_type="application/vnd.openxmlformats-officedocument.presentationml.presentation",
    )


async def run_report_generation(
    job_id: str, topic: str, data_sources: Optional[List[str]], output_dir: str
) -> None:
    """Run the report generation process in the background"""
    try:
        output_file = generate_business_report(topic, data_sources, output_dir)
        jobs[job_id]["status"] = "completed"
        jobs[job_id]["output_file"] = output_file
    except Exception as e:
        jobs[job_id]["status"] = "failed"
        jobs[job_id]["error"] = str(e)


if __name__ == "__main__":
    # Create necessary directories
    os.makedirs("static", exist_ok=True)
    os.makedirs("templates", exist_ok=True)
    os.makedirs("uploads", exist_ok=True)
    os.makedirs("my_reports", exist_ok=True)

    # Run the FastAPI app
    uvicorn.run("web_app:app", host="0.0.0.0", port=8000, reload=True)
