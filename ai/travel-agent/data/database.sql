-- =====================================================
-- 해외여행 상담 AI 서비스 데이터베이스 설계 (SQLite)
-- =====================================================

-- 1. 질문 카테고리 테이블
CREATE TABLE question_categories (
    category_id INTEGER PRIMARY KEY AUTOINCREMENT,
    category_name VARCHAR(100) NOT NULL,
    category_order INTEGER NOT NULL,
    description TEXT
);

-- 2. 질문 테이블
CREATE TABLE questions (
    question_id INTEGER PRIMARY KEY AUTOINCREMENT,
    category_id INTEGER NOT NULL,
    question_text TEXT NOT NULL,
    question_type VARCHAR(50) NOT NULL, -- single_choice, multiple_choice, text, number, date
    is_required BOOLEAN DEFAULT 1,
    question_order INTEGER NOT NULL,
    FOREIGN KEY (category_id) REFERENCES question_categories(category_id)
);

-- 3. 질문 선택지 테이블 (객관식 질문용)
CREATE TABLE question_options (
    option_id INTEGER PRIMARY KEY AUTOINCREMENT,
    question_id INTEGER NOT NULL,
    option_text VARCHAR(500) NOT NULL,
    option_value VARCHAR(100),
    option_order INTEGER NOT NULL,
    FOREIGN KEY (question_id) REFERENCES questions(question_id)
);

-- 4. 국가/지역 테이블
CREATE TABLE destinations (
    destination_id INTEGER PRIMARY KEY AUTOINCREMENT,
    country_name VARCHAR(100) NOT NULL,
    region_name VARCHAR(100),
    city_name VARCHAR(100),
    is_popular BOOLEAN DEFAULT 0,
    visa_required BOOLEAN DEFAULT 0,
    description TEXT
);

-- 5. 여행 상품 테이블
CREATE TABLE travel_packages (
    package_id INTEGER PRIMARY KEY AUTOINCREMENT,
    package_name VARCHAR(200) NOT NULL,
    destination_id INTEGER NOT NULL,
    duration_days INTEGER NOT NULL,
    min_price INTEGER NOT NULL,
    max_price INTEGER NOT NULL,
    package_type VARCHAR(50), -- free, semi_free, package_tour
    season VARCHAR(50), -- spring, summer, autumn, winter, all
    min_group_size INTEGER DEFAULT 1,
    max_group_size INTEGER DEFAULT 20,
    description TEXT,
    highlights TEXT,
    inclusions TEXT,
    exclusions TEXT,
    is_active BOOLEAN DEFAULT 1,
    created_date DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (destination_id) REFERENCES destinations(destination_id)
);

-- 6. 여행 상품 일정 테이블
CREATE TABLE package_itinerary (
    itinerary_id INTEGER PRIMARY KEY AUTOINCREMENT,
    package_id INTEGER NOT NULL,
    day_number INTEGER NOT NULL,
    day_title VARCHAR(200),
    activities TEXT NOT NULL,
    meals_included VARCHAR(100), -- breakfast, lunch, dinner
    accommodation_info TEXT,
    FOREIGN KEY (package_id) REFERENCES travel_packages(package_id)
);

-- 7. 사용자 답변 세션 테이블
CREATE TABLE user_sessions (
    session_id VARCHAR(100) PRIMARY KEY,
    created_date DATETIME DEFAULT CURRENT_TIMESTAMP,
    completed BOOLEAN DEFAULT 0,
    user_ip VARCHAR(45),
    user_agent TEXT
);

-- 8. 사용자 답변 테이블
CREATE TABLE user_answers (
    answer_id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_id VARCHAR(100) NOT NULL,
    question_id INTEGER NOT NULL,
    answer_text TEXT,
    answer_value VARCHAR(100),
    answered_date DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (session_id) REFERENCES user_sessions(session_id),
    FOREIGN KEY (question_id) REFERENCES questions(question_id)
);

-- 9. 추천 결과 테이블
CREATE TABLE recommendations (
    recommendation_id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_id VARCHAR(100) NOT NULL,
    package_id INTEGER NOT NULL,
    score DECIMAL(5,2) NOT NULL,
    reason TEXT,
    recommended_date DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (session_id) REFERENCES user_sessions(session_id),
    FOREIGN KEY (package_id) REFERENCES travel_packages(package_id)
);

-- =====================================================
-- 데이터 입력 (DML)
-- =====================================================

-- 질문 카테고리 입력
INSERT INTO question_categories (category_name, category_order, description) VALUES
('기본정보', 1, '여행 목적지, 기간, 동행자 등 기본적인 여행 정보'),
('예산선호도', 2, '예산 범위 및 숙박, 교통 선호도'),
('여행스타일', 3, '여행 목적 및 활동, 여행 패턴'),
('실용정보', 4, '언어, 건강, 비자, 보험 관련 정보'),
('특별요구', 5, '식단, 기념일, 과거 경험 등 특별한 요구사항');

-- 질문 입력
INSERT INTO questions (category_id, question_text, question_type, is_required, question_order) VALUES
-- 기본정보 (category_id = 1)
(1, '어느 나라나 지역을 방문하고 싶으신가요?', 'single_choice', 1, 1),
(1, '여행 출발 예정일은 언제인가요?', 'date', 1, 2),
(1, '몇 일간 여행하실 예정인가요?', 'single_choice', 1, 3),
(1, '함께 여행하실 분은 몇 명인가요? (본인 포함)', 'single_choice', 1, 4),
(1, '동행자는 누구신가요?', 'multiple_choice', 0, 5),

-- 예산선호도 (category_id = 2)
(2, '전체 여행 예산은 어느 정도로 생각하고 계신가요? (1인 기준)', 'single_choice', 1, 6),
(2, '선호하시는 숙박시설 유형은?', 'single_choice', 1, 7),
(2, '항공편 좌석 등급 선호도는?', 'single_choice', 1, 8),

-- 여행스타일 (category_id = 3)
(3, '주된 여행 목적은 무엇인가요?', 'multiple_choice', 1, 9),
(3, '관심 있는 여행 활동은? (복수 선택 가능)', 'multiple_choice', 1, 10),
(3, '선호하시는 여행 형태는?', 'single_choice', 1, 11),

-- 실용정보 (category_id = 4)
(4, '현지 언어 구사 능력은 어느 정도인가요?', 'single_choice', 1, 12),
(4, '특별한 건강상 고려사항이 있나요?', 'text', 0, 13),
(4, '여행자 보험 가입을 원하시나요?', 'single_choice', 1, 14),

-- 특별요구 (category_id = 5)
(5, '식단 제한이나 알레르기가 있나요?', 'text', 0, 15),
(5, '특별한 기념일과 관련된 여행인가요?', 'text', 0, 16),
(5, '해당 지역 여행 경험이 있으신가요?', 'single_choice', 0, 17);

-- 질문 선택지 입력
INSERT INTO question_options (question_id, option_text, option_value, option_order) VALUES
-- 질문 1: 여행 목적지
(1, '일본', 'japan', 1),
(1, '중국', 'china', 2),
(1, '태국', 'thailand', 3),
(1, '베트남', 'vietnam', 4),
(1, '유럽', 'europe', 5),
(1, '미국', 'usa', 6),
(1, '기타', 'other', 7),

-- 질문 3: 여행 기간
(3, '2-3일', '2-3', 1),
(3, '4-5일', '4-5', 2),
(3, '6-7일', '6-7', 3),
(3, '8-10일', '8-10', 4),
(3, '11일 이상', '11+', 5),

-- 질문 4: 인원수
(4, '1명 (혼자)', '1', 1),
(4, '2명', '2', 2),
(4, '3명', '3', 3),
(4, '4명', '4', 4),
(4, '5명 이상', '5+', 5),

-- 질문 5: 동행자
(5, '배우자/연인', 'spouse', 1),
(5, '가족 (부모님)', 'parents', 2),
(5, '가족 (자녀)', 'children', 3),
(5, '친구', 'friends', 4),
(5, '동료', 'colleagues', 5),

-- 질문 6: 예산
(6, '100만원 미만', 'under_1m', 1),
(6, '100-200만원', '1m_2m', 2),
(6, '200-300만원', '2m_3m', 3),
(6, '300-500만원', '3m_5m', 4),
(6, '500만원 이상', 'over_5m', 5),

-- 질문 7: 숙박시설
(7, '게스트하우스/호스텔', 'hostel', 1),
(7, '3성급 호텔', '3star', 2),
(7, '4성급 호텔', '4star', 3),
(7, '5성급 호텔/리조트', '5star', 4),
(7, '펜션/에어비앤비', 'pension', 5),

-- 질문 8: 항공편
(8, '이코노미', 'economy', 1),
(8, '프리미엄 이코노미', 'premium_economy', 2),
(8, '비즈니스', 'business', 3),

-- 질문 9: 여행 목적
(9, '휴양/휴식', 'relaxation', 1),
(9, '관광/명소 탐방', 'sightseeing', 2),
(9, '문화체험', 'culture', 3),
(9, '모험/액티비티', 'adventure', 4),
(9, '쇼핑', 'shopping', 5),
(9, '음식 체험', 'food', 6),

-- 질문 10: 관심 활동
(10, '역사/문화 탐방', 'history', 1),
(10, '자연 관광', 'nature', 2),
(10, '쇼핑', 'shopping', 3),
(10, '음식 체험', 'food', 4),
(10, '액티비티 스포츠', 'sports', 5),
(10, '야경/사진 촬영', 'photography', 6),

-- 질문 11: 여행 형태
(11, '완전 자유여행', 'free', 1),
(11, '반자유 패키지', 'semi_free', 2),
(11, '패키지 투어', 'package', 3),

-- 질문 12: 언어 능력
(12, '전혀 못함', 'none', 1),
(12, '기초 회화 가능', 'basic', 2),
(12, '일상 회화 가능', 'intermediate', 3),
(12, '유창함', 'fluent', 4),

-- 질문 14: 여행자 보험
(14, '네, 가입하겠습니다', 'yes', 1),
(14, '아니오, 가입하지 않겠습니다', 'no', 2),
(14, '추천해주시면 검토하겠습니다', 'recommend', 3),

-- 질문 17: 여행 경험
(17, '처음입니다', 'first', 1),
(17, '1-2번 있습니다', 'few', 2),
(17, '여러 번 있습니다', 'many', 3);

-- 국가/지역 정보 입력
INSERT INTO destinations (country_name, region_name, city_name, is_popular, visa_required, description) VALUES
('일본', '칸토', '도쿄', 1, 0, '일본의 수도, 현대와 전통이 공존하는 도시'),
('일본', '칸사이', '오사카', 1, 0, '일본의 부엌, 음식과 문화의 중심지'),
('일본', '칸사이', '교토', 1, 0, '일본의 고도, 전통 문화와 사찰의 도시'),
('태국', '중부', '방콕', 1, 0, '태국의 수도, 왕궁과 사원이 유명'),
('태국', '남부', '푸켓', 1, 0, '아름다운 해변과 리조트로 유명'),
('중국', '화동', '상하이', 1, 1, '중국의 경제중심지, 현대적인 스카이라인'),
('베트남', '북부', '하노이', 1, 1, '베트남의 수도, 프랑스 식민지 건축물'),
('베트남', '남부', '호치민시', 1, 1, '베트남 최대 도시, 역동적인 상업도시');

-- 여행 상품 입력
INSERT INTO travel_packages (package_name, destination_id, duration_days, min_price, max_price, package_type, season, min_group_size, max_group_size, description, highlights, inclusions, exclusions) VALUES
(
    '벚꽃 시즌 일본 문화체험 6박7일',
    1, -- 도쿄
    7,
    2000000,
    2500000,
    'semi_free',
    'spring',
    2,
    15,
    '벚꽃 시즌에 맞춰 도쿄와 오사카를 둘러보는 문화체험 여행',
    '벚꽃 명소 투어,전통 문화 체험,애니메이션 성지순례,결혼기념일 특별서비스',
    '항공료,숙박비,조식,JR패스,한국어가이드,여행자보험,벚꽃명소입장료',
    '점심/저녁식사,개인경비,선택관광'
),
(
    '오사카 미식 여행 4박5일',
    2, -- 오사카
    5,
    1200000,
    1800000,
    'semi_free',
    'all',
    2,
    12,
    '오사카의 대표 음식들을 체험하는 미식 중심 여행',
    '도톤보리 미식투어,오사카성 관람,쿠로시오시장,교토 당일치기',
    '항공료,숙박비,조식,시내교통,한국어가이드,미식체험비용',
    '점심/저녁식사 일부,개인경비,선택관광'
),
(
    '교토 전통문화 체험 3박4일',
    3, -- 교토
    4,
    900000,
    1400000,
    'package',
    'all',
    1,
    20,
    '교토의 전통 사찰과 문화를 체험하는 패키지',
    '기요미즈데라,후시미이나리,전통차도체험,기모노렌탈',
    '항공료,숙박비,전식사,입장료,문화체험비,한국어가이드',
    '개인경비,쇼핑비용'
),
(
    '방콕 자유여행 4박5일',
    4, -- 방콕
    5,
    800000,
    1200000,
    'free',
    'all',
    1,
    10,
    '방콕에서의 완전 자유여행, 개인 일정 구성',
    '왕궁투어,수상시장,마사지스파,야시장쇼핑',
    '항공료,숙박비,조식,공항픽업,여행자보험',
    '식사,교통비,입장료,개인경비'
);

-- 여행 상품 일정 입력 (벚꽃 시즌 일본 여행 예시)
INSERT INTO package_itinerary (package_id, day_number, day_title, activities, meals_included, accommodation_info) VALUES
(1, 1, '도쿄 도착 및 아사쿠사 탐방', '인천국제공항 출발 → 하네다공항 도착 → 호텔 체크인 → 아사쿠사 센소지 관람 → 환영 디너', 'dinner', '신주쿠 프린스 호텔'),
(1, 2, '벚꽃 명소 투어 및 시부야', '우에노 공원 벚꽃 관람 → 치도리가후치 → 메이지 신궁 → 시부야 스크램블 교차로 → 20주년 기념 디너', 'breakfast,dinner', '신주쿠 프린스 호텔'),
(1, 3, '애니메이션 성지순례 및 쇼핑', '아키하바라 → 하라주쿠 → 캐릭터 카페 점심 → 긴자 쇼핑 → 도쿄역 기념사진', 'breakfast,lunch', '신주쿠 프린스 호텔'),
(1, 4, '오사카 이동 및 오사카성', '신칸센으로 오사카 이동 → 호텔 체크인 → 오사카성 관람 → 도톤보리 탐방 → 타코야키 체험', 'breakfast,dinner', '난바 오리엔탈 호텔'),
(1, 5, '자유의 날', '완전 자유시간 (교토, 나라 당일치기 또는 오사카 자유관광)', 'breakfast', '난바 오리엔탈 호텔'),
(1, 6, '전통문화 체험', '구마노 고도 체험 → 고베규 점심 → 구로시오 시장 쇼핑 → 환송 가이세키 디너', 'breakfast,lunch,dinner', '난바 오리엔탈 호텔'),
(1, 7, '귀국', '호텔 체크아웃 → 간사이공항 → 인천국제공항 도착', 'breakfast', NULL);

-- 예시 사용자 세션 및 답변 입력
INSERT INTO user_sessions (session_id, user_ip, user_agent) VALUES
('session_20240410_001', '192.168.1.100', 'Mozilla/5.0 Chrome/91.0');

INSERT INTO user_answers (session_id, question_id, answer_text, answer_value) VALUES
('session_20240410_001', 1, '일본', 'japan'),
('session_20240410_001', 2, '2024-04-10', '2024-04-10'),
('session_20240410_001', 3, '6-7일', '6-7'),
('session_20240410_001', 4, '3명', '3'),
('session_20240410_001', 5, '배우자/연인,가족 (자녀)', 'spouse,children'),
('session_20240410_001', 6, '200-300만원', '2m_3m'),
('session_20240410_001', 7, '3성급 호텔', '3star'),
('session_20240410_001', 8, '이코노미', 'economy'),
('session_20240410_001', 9, '관광/명소 탐방,문화체험', 'sightseeing,culture'),
('session_20240410_001', 10, '역사/문화 탐방,음식 체험,쇼핑', 'history,food,shopping'),
('session_20240410_001', 11, '반자유 패키지', 'semi_free'),
('session_20240410_001', 12, '전혀 못함', 'none'),
('session_20240410_001', 13, '남편이 당뇨가 있어서 식사 시간을 지켜야 하고, 너무 많이 걷는 것은 부담스러워요.', NULL),
('session_20240410_001', 14, '네, 가입하겠습니다', 'yes'),
('session_20240410_001', 15, '딸이 해산물 알레르기가 있어서 생선이나 새우가 들어간 음식은 피해야 해요.', NULL),
('session_20240410_001', 16, '결혼 20주년 기념 여행이에요. 특별한 디너나 기념사진 촬영도 고려해주시면 좋겠어요.', NULL),
('session_20240410_001', 17, '처음입니다', 'first');

-- 추천 결과 입력
INSERT INTO recommendations (session_id, package_id, score, reason) VALUES
('session_20240410_001', 1, 95.5, '벚꽃 시즌, 일본 문화체험, 3명 가족여행, 예산 적합, 반자유 패키지, 특별 기념일 서비스 포함'),
('session_20240410_001', 2, 78.0, '오사카 미식 체험 가능하나 벚꽃 시즌과 문화체험 요소 부족'),
('session_20240410_001', 3, 72.5, '전통문화 체험 부분은 적합하나 기간이 짧고 벚꽃 시즌 고려 부족');

-- =====================================================
-- 인덱스 생성 (성능 향상용)
-- =====================================================
CREATE INDEX idx_questions_category ON questions(category_id);
CREATE INDEX idx_question_options_question ON question_options(question_id);
CREATE INDEX idx_user_answers_session ON user_answers(session_id);
CREATE INDEX idx_user_answers_question ON user_answers(question_id);
CREATE INDEX idx_recommendations_session ON recommendations(session_id);
CREATE INDEX idx_travel_packages_destination ON travel_packages(destination_id);
CREATE INDEX idx_package_itinerary_package ON package_itinerary(package_id);

-- =====================================================
-- 조회 쿼리 예시
-- =====================================================

-- 1. 특정 세션의 모든 답변 조회
/*
SELECT 
    qc.category_name,
    q.question_text,
    ua.answer_text,
    ua.answer_value
FROM user_answers ua
JOIN questions q ON ua.question_id = q.question_id
JOIN question_categories qc ON q.category_id = qc.category_id
WHERE ua.session_id = 'session_20240410_001'
ORDER BY q.question_order;
*/

-- 2. 특정 세션의 추천 상품 목록
/*
SELECT 
    tp.package_name,
    d.country_name,
    d.city_name,
    tp.duration_days,
    tp.min_price,
    tp.max_price,
    r.score,
    r.reason
FROM recommendations r
JOIN travel_packages tp ON r.package_id = tp.package_id
JOIN destinations d ON tp.destination_id = d.destination_id
WHERE r.session_id = 'session_20240410_001'
ORDER BY r.score DESC;
*/

-- 3. 인기 여행지별 상품 수 통계
/*
SELECT 
    d.country_name,
    d.city_name,
    COUNT(tp.package_id) as package_count,
    AVG(tp.min_price) as avg_min_price,
    AVG(tp.max_price) as avg_max_price
FROM destinations d
LEFT JOIN travel_packages tp ON d.destination_id = tp.destination_id
WHERE d.is_popular = 1
GROUP BY d.destination_id
ORDER BY package_count DESC;
*/