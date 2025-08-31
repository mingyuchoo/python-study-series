import difflib
import itertools
import os
import re
import sqlite3
from typing import Any, Dict, List, Optional

import chromadb
from chromadb.utils import embedding_functions


class TravelVectorStore:
    """
    최소 동작 스텁 구현체.
    - 애플리케이션이 기동되도록 인터페이스만 충족합니다.
    - 벡터 검색/유사도 계산은 추후 실제 구현으로 교체하세요.
    """

    def __init__(self, sqlite_db_path: str, vector_store_path: str):
        self.sqlite_db_path = sqlite_db_path
        self.vector_store_path = vector_store_path

        # Chroma Persistent Client 및 컬렉션 초기화
        os.makedirs(self.vector_store_path, exist_ok=True)
        try:
            self.client = chromadb.PersistentClient(path=self.vector_store_path)
            # 한국어 친화 임베딩 모델
            self.embedding_fn = (
                embedding_functions.SentenceTransformerEmbeddingFunction(
                    model_name="jhgan/ko-sroberta-multitask"
                )
            )
            # 컬렉션들 준비
            self.questions_collection = self.client.get_or_create_collection(
                name="travel_questions",
                embedding_function=self.embedding_fn,
                metadata={"hnsw:space": "cosine"},
            )
            self.packages_collection = self.client.get_or_create_collection(
                name="travel_packages",
                embedding_function=self.embedding_fn,
                metadata={"hnsw:space": "cosine"},
            )
            self.destinations_collection = self.client.get_or_create_collection(
                name="destinations",
                embedding_function=self.embedding_fn,
                metadata={"hnsw:space": "cosine"},
            )
        except Exception as e:
            print(f"[VectorStore] Chroma 초기화 실패: {e}")
            self.client = None
            self.questions_collection = None
            self.packages_collection = None
            self.destinations_collection = None

    def load_sqlite_data_to_vector_store(self) -> None:
        """
        SQLite의 질문/목적지/여행상품 데이터를 로드하여 각 컬렉션에 업서트합니다.
        테이블이 없을 경우 개별 로딩 단계에서 안전하게 스킵합니다.
        """
        try:
            if not os.path.exists(self.sqlite_db_path):
                print(
                    f"[VectorStore] SQLite 파일이 존재하지 않습니다: {self.sqlite_db_path}"
                )
                return
            if not all(
                [
                    self.questions_collection,
                    self.packages_collection,
                    self.destinations_collection,
                ]
            ):
                print(
                    "[VectorStore] Chroma 컬렉션이 초기화되지 않아 적재를 생략합니다."
                )
                return

            self._load_questions_to_vector()
            self._load_destinations_to_vector()
            self._load_packages_to_vector()
            print("[VectorStore] SQLite → Chroma 데이터 로딩 완료")
        except Exception as e:
            print(f"[VectorStore] 적재 중 경고: {e}")

    def _load_questions_to_vector(self) -> None:
        """
        SQLite의 질문 테이블을 Chroma에 업서트합니다.
        테이블이 없을 경우 안전하게 스킵합니다.
        """
        try:
            with sqlite3.connect(self.sqlite_db_path) as conn:
                cur = conn.cursor()
                cur.execute(
                    """
                    SELECT 
                        q.question_id,
                        q.question_text,
                        q.question_type,
                        q.is_required,
                        qc.category_name,
                        GROUP_CONCAT(qo.option_text, ' | ') as options
                    FROM
                        questions q
                    JOIN
                        question_categories qc ON q.category_id = qc.category_id
                    LEFT JOIN
                        question_options qo ON q.question_id = qo.question_id
                    GROUP BY
                        q.question_id
                    ORDER BY
                        q.question_order
                    """
                )
                rows = cur.fetchall()
                if not rows:
                    print("[VectorStore] 질문 데이터 없음")
                    return

                ids, documents, metadatas = [], [], []
                for qid, qtext, qtype, required, category, options in rows:
                    doc = f"{category} {qtext}"
                    if options:
                        doc += f" 선택지: {options}"
                    documents.append(doc)
                    metadatas.append(
                        {
                            "question_id": qid,
                            "question_text": qtext,
                            "question_type": qtype,
                            "is_required": bool(required),
                            "category": category,
                            "options": options or "",
                        }
                    )
                    ids.append(f"q_{qid}")

                self.questions_collection.upsert(
                    ids=ids, documents=documents, metadatas=metadatas
                )
                print(f"[VectorStore] 질문 {len(ids)}개 업서트")
        except Exception as e:
            print(f"[VectorStore] 질문 적재 오류: {e}")

    def _load_destinations_to_vector(self) -> None:
        """
        SQLite의 목적지 테이블을 Chroma에 업서트합니다.
        테이블이 없을 경우 안전하게 스킵합니다.
        """
        try:
            with sqlite3.connect(self.sqlite_db_path) as conn:
                cur = conn.cursor()
                cur.execute(
                    """
                    SELECT 
                        destination_id,
                        country_name,
                        region_name,
                        city_name,
                        is_popular,
                        visa_required,
                        description
                    FROM
                        destinations
                    """
                )
                rows = cur.fetchall()
                if not rows:
                    print("[VectorStore] 목적지 데이터 없음")
                    return

                ids, documents, metadatas = [], [], []
                for did, country, region, city, popular, visa, desc in rows:
                    pieces = [str(country or ""), str(region or ""), str(city or "")]
                    if desc:
                        pieces.append(str(desc))
                    doc = " ".join([p for p in pieces if p])
                    documents.append(doc)
                    metadatas.append(
                        {
                            "destination_id": did,
                            "country_name": country,
                            "region_name": region or "",
                            "city_name": city or "",
                            "is_popular": bool(popular),
                            "visa_required": bool(visa),
                            "description": desc or "",
                        }
                    )
                    ids.append(f"d_{did}")

                self.destinations_collection.upsert(
                    ids=ids, documents=documents, metadatas=metadatas
                )
                print(f"[VectorStore] 목적지 {len(ids)}개 업서트")
        except Exception as e:
            print(f"[VectorStore] 목적지 적재 오류: {e}")

    def _load_packages_to_vector(self) -> None:
        """
        SQLite의 여행상품 테이블을 Chroma에 업서트합니다.
        테이블이 없을 경우 안전하게 스킵합니다.
        """
        try:
            with sqlite3.connect(self.sqlite_db_path) as conn:
                cur = conn.cursor()
                cur.execute(
                    """
                    SELECT
                        tp.package_id,
                        tp.package_name,
                        tp.duration_days,
                        tp.min_price,
                        tp.max_price,
                        tp.package_type,
                        tp.season,
                        tp.description,
                        tp.highlights,
                        tp.inclusions,
                        d.country_name,
                        d.city_name,
                        GROUP_CONCAT(pi.day_title || ': ' || pi.activities, ' | ') as itinerary
                    FROM
                        travel_packages tp
                    JOIN
                        destinations d ON tp.destination_id = d.destination_id
                    LEFT JOIN
                        package_itinerary pi ON tp.package_id = pi.package_id
                    WHERE
                        tp.is_active = 1
                    GROUP BY
                        tp.package_id
                    """
                )
                rows = cur.fetchall()
                if not rows:
                    print("[VectorStore] 여행 상품 데이터 없음")
                    return

                ids, documents, metadatas = [], [], []
                for (
                    pid,
                    name,
                    duration,
                    min_price,
                    max_price,
                    ptype,
                    season,
                    desc,
                    highlights,
                    inclusions,
                    country,
                    city,
                    itinerary,
                ) in rows:
                    text_parts = [name, country or "", city or "", desc or ""]
                    if highlights:
                        text_parts.append(f"특징: {highlights}")
                    if itinerary:
                        text_parts.append(f"일정: {itinerary}")
                    doc = " ".join([p for p in text_parts if p])
                    documents.append(doc)
                    metadatas.append(
                        {
                            "package_id": pid,
                            "package_name": name,
                            "duration_days": int(duration or 0),
                            "min_price": int(min_price or 0),
                            "max_price": int(max_price or 0),
                            "package_type": ptype or "",
                            "season": season or "",
                            "country": country or "",
                            "city": city or "",
                            "description": desc or "",
                            "highlights": highlights or "",
                            "inclusions": inclusions or "",
                        }
                    )
                    ids.append(f"p_{pid}")

                self.packages_collection.upsert(
                    ids=ids, documents=documents, metadatas=metadatas
                )
                print(f"[VectorStore] 여행 상품 {len(ids)}개 업서트")
        except Exception as e:
            print(f"[VectorStore] 여행 상품 적재 오류: {e}")

    def get_personalized_recommendations(
        self, user_answers: Dict[int, str]
    ) -> List[Dict[str, Any]]:
        """
        사용자 답변을 하나의 쿼리 텍스트로 합성하여 컬렉션에서 유사도 검색을 수행.
        RecommendationService가 기대하는 키를 포함하는 딕셔너리 리스트를 반환.
        """
        if not user_answers:
            return []
        if self.packages_collection is None:
            print("[VectorStore] 컬렉션 없음: 빈 추천 반환")
            return []

        # 1) 쿼리 합성 + 사용자 선호 추출
        query_text = " \n ".join(
            [f"Q{qid}: {ans}" for qid, ans in sorted(user_answers.items())]
        )
        prefs = self._convert_answers_to_preferences(user_answers)
        dest_keyword = prefs.get("destination")

        try:
            # 2) 넉넉히 검색 후 필터링 적용
            result = self.packages_collection.query(
                query_texts=[query_text], n_results=50
            )
            ids = result.get("ids", [[]])[0]
            metas = result.get("metadatas", [[]])[0]
            distances = result.get("distances", [[]])
            distances = distances[0] if distances else [None] * len(ids)

            candidates: List[Dict[str, Any]] = []
            for pid, meta, dist in itertools.zip_longest(
                ids, metas, distances, fillvalue=None
            ):
                if not meta:
                    continue
                # cosine distance -> similarity (1 - distance)
                sim = (1.0 - float(dist)) if isinstance(dist, (int, float)) else None

                item = {
                    "package_id": meta.get("package_id"),
                    "package_name": meta.get("package_name", ""),
                    "country": meta.get("country", ""),
                    "city": meta.get("city", ""),
                    "duration_days": meta.get("duration_days", 0),
                    "min_price": meta.get("min_price", 0),
                    "max_price": meta.get("max_price", meta.get("min_price", 0)),
                    "metadata": meta,
                    "similarity_score": sim,
                    "final_score": sim,
                }
                # 7) 규칙 기반 가산점 포함 최종 점수 계산
                item["final_score"] = self._calculate_recommendation_score(
                    item, user_answers
                )
                candidates.append(item)

            # 8) 최종 점수 기준 내림차순 정렬
            candidates.sort(key=lambda x: (x.get("final_score") or 0.0), reverse=True)
            return candidates
        except Exception as e:
            print(f"[VectorStore] 추천 질의 중 오류: {e}")
            return []

    def search_travel_packages(
        self, user_preferences: Dict[str, Any], limit: int = 10
    ) -> List[Dict[str, Any]]:
        """
        사용자의 선호도(목적지, 관심사 등)를 텍스트로 합성해 컬렉션에서 검색합니다.
        라우터 `recommendations.search_packages()`가 기대하는 딕셔너리 리스트 포맷을 유지합니다.
        """
        if self.packages_collection is None:
            print("[VectorStore] 컬렉션 없음: 빈 검색 결과 반환")
            return []

        destination = user_preferences.get("destination")
        interests = user_preferences.get("interests")
        budget = user_preferences.get("budget")
        days = user_preferences.get("duration_days") or user_preferences.get("days")

        parts = []
        if destination:
            parts.append(f"destination: {destination}")
        if interests:
            parts.append(f"interests: {interests}")
        if budget:
            parts.append(f"budget: {budget}")
        if days:
            parts.append(f"duration_days: {days}")
        query_text = " \n ".join(parts) if parts else "travel package recommendations"

        try:
            result = self.packages_collection.query(
                query_texts=[query_text], n_results=limit
            )
            ids = result.get("ids", [[]])[0]
            metas = result.get("metadatas", [[]])[0]
            distances = result.get("distances", [[]])
            distances = distances[0] if distances else [None] * len(ids)

            items: List[Dict[str, Any]] = []
            for pid, meta, dist in itertools.zip_longest(
                ids, metas, distances, fillvalue=None
            ):
                if not meta:
                    continue
                sim = (1.0 - float(dist)) if isinstance(dist, (int, float)) else None
                candidate = {
                    "package_id": meta.get("package_id"),
                    "package_name": meta.get("package_name", ""),
                    "country": meta.get("country", ""),
                    "city": meta.get("city", ""),
                    "duration_days": meta.get("duration_days", 0),
                    "min_price": meta.get("min_price", 0),
                    "max_price": meta.get("max_price", meta.get("min_price", 0)),
                    "metadata": meta,
                    "similarity_score": sim,
                }
                # 벡터 유사도 하한선 적용 (너무 낮은 결과 제거)
                if sim is not None and sim < 0.40:
                    continue

                # 키워드 기반 연관성 필터 (오타 허용 퍼지 매칭 포함)
                keyword = destination or (
                    interests[0] if isinstance(interests, list) and interests else None
                )
                if keyword and not self._is_relevant(candidate["metadata"], keyword):
                    continue

                # 후처리 필터 (예산/기간)
                if self._matches_budget(
                    candidate["metadata"], user_preferences.get("budget")
                ) and self._matches_duration(
                    candidate["metadata"],
                    user_preferences.get("duration")
                    or user_preferences.get("duration_days"),
                ):
                    items.append(candidate)

            return items
        except Exception as e:
            print(f"[VectorStore] 검색 질의 중 오류: {e}")
            return []

    def search_relevant_questions(self, user_query: str, limit: int = 5) -> List[Dict[str, Any]]:
        """
        사용자 자연어 쿼리와 유사한 질문 검색
        """
        if self.questions_collection is None:
            return []
        try:
            result = self.questions_collection.query(
                query_texts=[user_query], n_results=limit
            )
            docs = result.get("documents", [[]])[0]
            metas = result.get("metadatas", [[]])[0]
            dists = result.get("distances", [[]])
            dists = dists[0] if dists else [0.0] * len(docs)
            out: List[Dict[str, Any]] = []
            for i, doc in enumerate(docs):
                meta = metas[i]
                dist = dists[i]
                out.append(
                    {
                        "question_id": meta.get("question_id"),
                        "question_text": meta.get("question_text"),
                        "category": meta.get("category"),
                        "similarity_score": (1 - float(dist) if isinstance(dist, (int, float)) else None),
                        "metadata": meta,
                    }
                )
            return out
        except Exception as e:
            print(f"[VectorStore] 질문 검색 오류: {e}")
            return []

    def _matches_budget(self, package_metadata: Dict[str, Any], budget_range: Optional[str]) -> bool:
        """
        예산 범위에 맞는지 확인
        """
        if not budget_range:
            return True
        min_price = int(package_metadata.get("min_price", 0))
        max_price = int(package_metadata.get("max_price", 0))
        ranges = {
            "under_1m": (0, 1_000_000),
            "1m_2m": (1_000_000, 2_000_000),
            "2m_3m": (2_000_000, 3_000_000),
            "3m_5m": (3_000_000, 5_000_000),
            "over_5m": (5_000_000, float("inf")),
        }
        if budget_range in ranges:
            bmin, bmax = ranges[budget_range]
            return not (max_price < bmin or min_price > bmax)
        return True

    def _matches_duration(self, package_metadata: Dict[str, Any], duration_range: Optional[str]) -> bool:
        """
        여행 일수 범위에 맞는지 확인
        """
        if not duration_range:
            return True
        pdays = int(package_metadata.get("duration_days", 0))
        ranges = {
            "2-3": (2, 3),
            "4-5": (4, 5),
            "6-7": (6, 7),
            "8-10": (8, 10),
            "11+": (11, 365),
        }
        if duration_range in ranges:
            dmin, dmax = ranges[duration_range]
            return dmin <= pdays <= dmax
        return True

    def _normalize(self, s: Optional[str]) -> str:
        """
        문자열을 정규화합니다.
        """
        return str(s or "").strip().lower()

    def _fuzzy_contains(self, text: str, keyword: str, threshold: float = 0.80) -> bool:
        """
        토큰 단위로 퍼지 매칭 수행. 한국어/영문/숫자 기준으로 토큰화.
        """
        t = self._normalize(text)
        k = self._normalize(keyword)
        if not t or not k:
            return False

        # 우선 직접 서브스트링 매칭
        if k in t:
            return True
        # 토큰 단위 퍼지 유사도 검사
        tokens = [tok for tok in re.split(r"[^0-9A-Za-z가-힣]+", t) if tok]
        for tok in tokens:
            if difflib.SequenceMatcher(None, tok, k).ratio() >= threshold:
                return True
        return False

    def _is_relevant(self, meta: Dict[str, Any], keyword: str) -> bool:
        """
        국가/도시/상품명/설명/하이라이트 필드에 대해 서브스트링 또는 퍼지 매칭으로 연관성 판단
        """
        fields = [
            meta.get("country", ""),
            meta.get("city", ""),
            meta.get("package_name", ""),
            meta.get("description", ""),
            meta.get("highlights", ""),
        ]
        blob = " ".join(str(f or "") for f in fields)
        return self._fuzzy_contains(blob, keyword, threshold=0.5)

    def _convert_answers_to_preferences(self, user_answers: Dict[int, str]) -> Dict[str, Any]:
        """
        사용자의 답변을 선호도로 변환
        """
        prefs: Dict[str, Any] = {}
        mapping = {
            1: ("destination", {"japan": "일본", "thailand": "태국", "china": "중국"}),
            3: ("duration", None),
            6: ("budget", None),
            9: ("interests", None),
            11: (
                "travel_style",
                {
                    "semi_free": "반자유여행",
                    "free": "자유여행",
                    "package": "패키지여행",
                },
            ),
        }
        for qid, (key, m) in mapping.items():
            if qid in user_answers:
                val = user_answers[qid]
                prefs[key] = m.get(val, val) if m else val
        return prefs

    def _calculate_recommendation_score(self, package: Dict[str, Any], user_answers: Dict[int, str]) -> float:
        """
        추천 점수 계산
        """
        score = float(package.get("similarity_score", 0.5)) * 100.0
        # 목적지 가산점
        if user_answers.get(1) == "japan" and package.get("country") == "일본":
            score += 20
        # 기간/예산 매칭
        if 3 in user_answers and self._matches_duration(
            package["metadata"], user_answers[3]
        ):
            score += 15
        if 6 in user_answers and self._matches_budget(
            package["metadata"], user_answers[6]
        ):
            score += 15
        # 여행 스타일 매칭
        if 11 in user_answers:
            if package["metadata"].get("package_type") == user_answers[11]:
                score += 10
        return min(score, 100.0)

    def save_user_session(self, session_id: str, user_answers: Dict[int, str], recommendations: List[Dict[str, Any]]) -> None:
        """
        세션 완료 처리 및 추천 결과를 DB에 저장
        """
        try:
            with sqlite3.connect(self.sqlite_db_path) as conn:
                cur = conn.cursor()
                # 세션 완료 처리
                cur.execute(
                    """
                    UPDATE
                        user_sessions 
                    SET
                        completed = 1 
                    WHERE
                        session_id = ?
                    """,
                    (session_id,),
                )
                # 추천 결과 저장
                for rec in recommendations:
                    cur.execute(
                        """
                        INSERT INTO
                            recommendations (session_id, package_id, score, reason)
                        VALUES
                            (?, ?, ?, ?)
                        """,
                        (
                            session_id,
                            rec.get("package_id"),
                            float(rec.get("final_score") or 0.0),
                            f"벡터 유사도: {float(rec.get('similarity_score') or 0.0):.2f}, 규칙 기반 점수 추가",
                        ),
                    )
                conn.commit()
            print(
                f"[VectorStore] 세션 저장 완료 (session_id={session_id}, answers={len(user_answers)}, recs={len(recommendations)})"
            )
        except Exception as e:
            print(f"[VectorStore] 세션 저장 중 오류: {e}")
