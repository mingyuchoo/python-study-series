import sqlite3
from contextlib import contextmanager
from typing import List

from backend.config import settings


class DatabaseManager:
    """SQLite 데이터베이스 관리"""

    def __init__(self, db_path: str = None):
        self.db_path = db_path or settings.sqlite_db_path

    @contextmanager
    def get_connection(self):
        """데이터베이스 연결 컨텍스트 매니저"""
        conn = sqlite3.connect(self.db_path)
        conn.row_factory = sqlite3.Row  # 딕셔너리 형태로 결과 반환
        try:
            yield conn
        finally:
            conn.close()

    def execute_query(self, query: str, params: tuple = None) -> List[dict]:
        """쿼리 실행"""
        with self.get_connection() as conn:
            cursor = conn.cursor()
            if params:
                cursor.execute(query, params)
            else:
                cursor.execute(query)
            return [dict(row) for row in cursor.fetchall()]

    def execute_insert(self, query: str, params: tuple = None) -> int:
        """INSERT 쿼리 실행"""
        with self.get_connection() as conn:
            cursor = conn.cursor()
            if params:
                cursor.execute(query, params)
            else:
                cursor.execute(query)
            conn.commit()
            return cursor.lastrowid


db_manager = DatabaseManager()
