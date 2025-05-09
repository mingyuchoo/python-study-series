import json
import os
import sqlite3
from datetime import datetime
from typing import Any, Dict, List, Optional, Tuple


def ensure_db_dir(db_path: str) -> None:
    dirname = os.path.dirname(db_path)
    if dirname:
        os.makedirs(dirname, exist_ok=True)


def connect(db_path: str) -> Tuple[Any, Any]:
    conn = sqlite3.connect(db_path)
    conn.row_factory = sqlite3.Row
    cursor = conn.cursor()
    return conn, cursor


def disconnect(conn: Any) -> None:
    if conn:
        conn.close()


def initialize_db(db_path: str = "database/math_problems.db") -> None:
    ensure_db_dir(db_path)
    conn, cursor = connect(db_path)
    cursor.execute(
        """
    CREATE TABLE IF NOT EXISTS problems (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        problem_text TEXT NOT NULL,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
    """
    )
    cursor.execute(
        """
    CREATE TABLE IF NOT EXISTS solutions (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        problem_id INTEGER NOT NULL,
        solution_text TEXT NOT NULL,
        explanation TEXT,
        real_world_example TEXT,
        agent_conversation JSON,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (problem_id) REFERENCES problems (id)
    )
    """
    )
    conn.commit()
    disconnect(conn)


def add_problem(db_path: str, problem_text: str) -> int:
    conn, cursor = connect(db_path)
    cursor.execute("SELECT id FROM problems WHERE problem_text = ?", (problem_text,))
    existing = cursor.fetchone()
    if existing:
        problem_id = existing["id"]
    else:
        cursor.execute("INSERT INTO problems (problem_text) VALUES (?)", (problem_text,))
        problem_id = cursor.lastrowid
    conn.commit()
    disconnect(conn)
    return problem_id


def add_solution(
    db_path: str,
    problem_id: int,
    solution_text: str,
    explanation: str,
    real_world_example: str,
    agent_conversation: Any,
) -> int:
    conn, cursor = connect(db_path)
    cursor.execute(
        """INSERT INTO solutions 
           (problem_id, solution_text, explanation, real_world_example, agent_conversation) 
           VALUES (?, ?, ?, ?, ?)""",
        (problem_id, solution_text, explanation, real_world_example, json.dumps(agent_conversation)),
    )
    solution_id = cursor.lastrowid
    conn.commit()
    disconnect(conn)
    return solution_id


def get_problem(db_path: str, problem_id: int) -> Dict[str, Any]:
    conn, cursor = connect(db_path)
    cursor.execute("SELECT * FROM problems WHERE id = ?", (problem_id,))
    problem = dict(cursor.fetchone() or {})
    disconnect(conn)
    return problem


def get_solution(db_path: str, solution_id: int) -> Dict[str, Any]:
    conn, cursor = connect(db_path)
    cursor.execute("SELECT * FROM solutions WHERE id = ?", (solution_id,))
    solution = dict(cursor.fetchone() or {})
    if solution and "agent_conversation" in solution:
        solution["agent_conversation"] = json.loads(solution["agent_conversation"])
    disconnect(conn)
    return solution


def get_recent_problems(db_path: str, limit: int = 10) -> List[Dict[str, Any]]:
    conn, cursor = connect(db_path)
    cursor.execute("SELECT * FROM problems ORDER BY created_at DESC LIMIT ?", (limit,))
    problems = [dict(row) for row in cursor.fetchall()]
    disconnect(conn)
    return problems


def get_solutions_for_problem(db_path: str, problem_id: int) -> List[Dict[str, Any]]:
    conn, cursor = connect(db_path)
    cursor.execute("SELECT * FROM solutions WHERE problem_id = ? ORDER BY created_at DESC", (problem_id,))
    solutions = [dict(row) for row in cursor.fetchall()]
    for solution in solutions:
        if "agent_conversation" in solution:
            solution["agent_conversation"] = json.loads(solution["agent_conversation"])
    disconnect(conn)
    return solutions
