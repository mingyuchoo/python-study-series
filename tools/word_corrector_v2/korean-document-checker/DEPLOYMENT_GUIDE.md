# 배포 가이드 (Deployment Guide)

이 문서는 한국어 문서 검사기를 다양한 환경에 배포하는 방법을 설명합니다.

## 📋 목차

1. [개발 환경 배포](#개발-환경-배포)
2. [프로덕션 환경 배포](#프로덕션-환경-배포)
3. [Docker 배포](#docker-배포)
4. [클라우드 배포](#클라우드-배포)
5. [모니터링 및 로깅](#모니터링-및-로깅)
6. [보안 설정](#보안-설정)

## 🛠️ 개발 환경 배포

### 빠른 시작

#### Windows
```cmd
# 백엔드 시작
start_backend.bat

# 새 터미널에서 프론트엔드 시작
start_frontend.bat
```

#### macOS/Linux
```bash
# 백엔드 시작
./start_backend.sh

# 새 터미널에서 프론트엔드 시작
./start_frontend.sh
```

### 수동 실행

#### 1. 환경 변수 설정

`.env` 파일을 `backend/` 디렉터리에 생성:

```env
AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/
AZURE_OPENAI_API_KEY=your-api-key-here
AZURE_OPENAI_API_VERSION=2024-12-01-preview
AZURE_OPENAI_DEPLOYMENT_NAME=gpt-4-deployment
MAX_FILE_SIZE=10485760
TEMP_FILE_RETENTION_HOURS=1
BACKEND_URL=http://localhost:8000
```

#### 2. 백엔드 실행

```bash
cd backend
uv sync
uv run uvicorn app.main:app --reload --host 0.0.0.0 --port 8000
```

#### 3. 프론트엔드 실행

```bash
cd frontend
uv sync
uv run streamlit run app.py --server.port 8501
```

## 🚀 프로덕션 환경 배포

### 시스템 요구사항

- **CPU**: 최소 2 코어, 권장 4 코어
- **메모리**: 최소 4GB, 권장 8GB
- **디스크**: 최소 10GB 여유 공간
- **네트워크**: 안정적인 인터넷 연결
- **OS**: Ubuntu 20.04+, CentOS 8+, Windows Server 2019+

### 1. 시스템 준비

#### Ubuntu/Debian
```bash
# 시스템 업데이트
sudo apt update && sudo apt upgrade -y

# 필수 패키지 설치
sudo apt install -y python3.12 python3.12-venv curl git

# uv 설치
curl -LsSf https://astral.sh/uv/install.sh | sh
source ~/.bashrc
```

#### CentOS/RHEL
```bash
# 시스템 업데이트
sudo yum update -y

# Python 3.12 설치 (EPEL 저장소 필요)
sudo yum install -y epel-release
sudo yum install -y python312 python312-pip curl git

# uv 설치
curl -LsSf https://astral.sh/uv/install.sh | sh
source ~/.bashrc
```

### 2. 애플리케이션 배포

```bash
# 애플리케이션 디렉터리 생성
sudo mkdir -p /opt/korean-document-checker
sudo chown $USER:$USER /opt/korean-document-checker

# 소스 코드 배포
cd /opt/korean-document-checker
git clone <repository-url> .

# 의존성 설치
cd backend && uv sync --frozen
cd ../frontend && uv sync --frozen
```

### 3. 환경 설정

```bash
# 프로덕션 환경 변수 설정
sudo tee /opt/korean-document-checker/backend/.env << EOF
AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/
AZURE_OPENAI_API_KEY=your-production-api-key
AZURE_OPENAI_API_VERSION=2024-12-01-preview
AZURE_OPENAI_DEPLOYMENT_NAME=gpt-4-deployment
MAX_FILE_SIZE=10485760
TEMP_FILE_RETENTION_HOURS=1
BACKEND_URL=http://localhost:8000
EOF

# 파일 권한 설정
sudo chmod 600 /opt/korean-document-checker/backend/.env
```

### 4. 시스템 서비스 설정

#### 백엔드 서비스 (systemd)

```bash
sudo tee /etc/systemd/system/korean-doc-checker-backend.service << EOF
[Unit]
Description=Korean Document Checker Backend
After=network.target

[Service]
Type=exec
User=www-data
Group=www-data
WorkingDirectory=/opt/korean-document-checker/backend
Environment=PATH=/opt/korean-document-checker/backend/.venv/bin
ExecStart=/opt/korean-document-checker/backend/.venv/bin/uvicorn app.main:app --host 0.0.0.0 --port 8000 --workers 4
Restart=always
RestartSec=3

[Install]
WantedBy=multi-user.target
EOF

# 서비스 활성화 및 시작
sudo systemctl daemon-reload
sudo systemctl enable korean-doc-checker-backend
sudo systemctl start korean-doc-checker-backend
```

#### 프론트엔드 서비스 (systemd)

```bash
sudo tee /etc/systemd/system/korean-doc-checker-frontend.service << EOF
[Unit]
Description=Korean Document Checker Frontend
After=network.target korean-doc-checker-backend.service

[Service]
Type=exec
User=www-data
Group=www-data
WorkingDirectory=/opt/korean-document-checker/frontend
Environment=PATH=/opt/korean-document-checker/frontend/.venv/bin
ExecStart=/opt/korean-document-checker/frontend/.venv/bin/streamlit run app.py --server.port 8501 --server.address 0.0.0.0
Restart=always
RestartSec=3

[Install]
WantedBy=multi-user.target
EOF

# 서비스 활성화 및 시작
sudo systemctl daemon-reload
sudo systemctl enable korean-doc-checker-frontend
sudo systemctl start korean-doc-checker-frontend
```

### 5. 리버스 프록시 설정 (Nginx)

```bash
# Nginx 설치
sudo apt install -y nginx

# 설정 파일 생성
sudo tee /etc/nginx/sites-available/korean-doc-checker << EOF
server {
    listen 80;
    server_name your-domain.com;

    # 프론트엔드 (Streamlit)
    location / {
        proxy_pass http://localhost:8501;
        proxy_http_version 1.1;
        proxy_set_header Upgrade \$http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host \$host;
        proxy_set_header X-Real-IP \$remote_addr;
        proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto \$scheme;
        proxy_cache_bypass \$http_upgrade;
    }

    # 백엔드 API
    location /api/ {
        proxy_pass http://localhost:8000;
        proxy_set_header Host \$host;
        proxy_set_header X-Real-IP \$remote_addr;
        proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto \$scheme;
    }

    # 정적 파일 캐싱
    location ~* \.(js|css|png|jpg|jpeg|gif|ico|svg)$ {
        expires 1y;
        add_header Cache-Control "public, immutable";
    }
}
EOF

# 사이트 활성화
sudo ln -s /etc/nginx/sites-available/korean-doc-checker /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx
```

## 🐳 Docker 배포

### 1. Dockerfile 생성

#### 백엔드 Dockerfile

```dockerfile
# backend/Dockerfile
FROM python:3.12-slim

WORKDIR /app

# uv 설치
COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv

# 의존성 파일 복사
COPY pyproject.toml uv.lock ./

# 의존성 설치
RUN uv sync --frozen --no-cache

# 애플리케이션 코드 복사
COPY . .

# 포트 노출
EXPOSE 8000

# 애플리케이션 실행
CMD ["uv", "run", "uvicorn", "app.main:app", "--host", "0.0.0.0", "--port", "8000"]
```

#### 프론트엔드 Dockerfile

```dockerfile
# frontend/Dockerfile
FROM python:3.12-slim

WORKDIR /app

# uv 설치
COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv

# 의존성 파일 복사
COPY pyproject.toml uv.lock ./

# 의존성 설치
RUN uv sync --frozen --no-cache

# 애플리케이션 코드 복사
COPY . .

# 포트 노출
EXPOSE 8501

# 애플리케이션 실행
CMD ["uv", "run", "streamlit", "run", "app.py", "--server.port", "8501", "--server.address", "0.0.0.0"]
```

### 2. Docker Compose 설정

```yaml
# docker-compose.yml
version: '3.8'

services:
  backend:
    build: ./backend
    ports:
      - "8000:8000"
    environment:
      - AZURE_OPENAI_ENDPOINT=${AZURE_OPENAI_ENDPOINT}
      - AZURE_OPENAI_API_KEY=${AZURE_OPENAI_API_KEY}
      - AZURE_OPENAI_API_VERSION=${AZURE_OPENAI_API_VERSION}
      - AZURE_OPENAI_DEPLOYMENT_NAME=${AZURE_OPENAI_DEPLOYMENT_NAME}
    volumes:
      - ./backend/temp:/app/temp
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8000/api/health"]
      interval: 30s
      timeout: 10s
      retries: 3

  frontend:
    build: ./frontend
    ports:
      - "8501:8501"
    environment:
      - BACKEND_URL=http://backend:8000
    depends_on:
      - backend
    restart: unless-stopped

  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf
      - ./ssl:/etc/nginx/ssl
    depends_on:
      - frontend
      - backend
    restart: unless-stopped
```

### 3. Docker 실행

```bash
# 환경 변수 파일 생성
cp .env.example .env
# .env 파일 편집

# 컨테이너 빌드 및 실행
docker-compose up -d

# 로그 확인
docker-compose logs -f

# 서비스 상태 확인
docker-compose ps
```

## ☁️ 클라우드 배포

### Azure Container Instances

```bash
# 리소스 그룹 생성
az group create --name korean-doc-checker-rg --location koreacentral

# 컨테이너 인스턴스 생성
az container create \
  --resource-group korean-doc-checker-rg \
  --name korean-doc-checker \
  --image your-registry/korean-doc-checker:latest \
  --cpu 2 \
  --memory 4 \
  --ports 80 \
  --environment-variables \
    AZURE_OPENAI_ENDPOINT=$AZURE_OPENAI_ENDPOINT \
    AZURE_OPENAI_API_KEY=$AZURE_OPENAI_API_KEY
```

### AWS ECS

```json
{
  "family": "korean-doc-checker",
  "networkMode": "awsvpc",
  "requiresCompatibilities": ["FARGATE"],
  "cpu": "1024",
  "memory": "2048",
  "executionRoleArn": "arn:aws:iam::account:role/ecsTaskExecutionRole",
  "containerDefinitions": [
    {
      "name": "korean-doc-checker",
      "image": "your-registry/korean-doc-checker:latest",
      "portMappings": [
        {
          "containerPort": 80,
          "protocol": "tcp"
        }
      ],
      "environment": [
        {
          "name": "AZURE_OPENAI_ENDPOINT",
          "value": "your-endpoint"
        }
      ],
      "secrets": [
        {
          "name": "AZURE_OPENAI_API_KEY",
          "valueFrom": "arn:aws:secretsmanager:region:account:secret:name"
        }
      ]
    }
  ]
}
```

## 📊 모니터링 및 로깅

### 1. 로그 설정

#### 백엔드 로깅 설정

```python
# backend/app/core/logging.py
import logging
import sys
from pathlib import Path

def setup_logging():
    log_format = "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
    
    # 콘솔 핸들러
    console_handler = logging.StreamHandler(sys.stdout)
    console_handler.setFormatter(logging.Formatter(log_format))
    
    # 파일 핸들러
    log_file = Path("logs/app.log")
    log_file.parent.mkdir(exist_ok=True)
    file_handler = logging.FileHandler(log_file)
    file_handler.setFormatter(logging.Formatter(log_format))
    
    # 루트 로거 설정
    root_logger = logging.getLogger()
    root_logger.setLevel(logging.INFO)
    root_logger.addHandler(console_handler)
    root_logger.addHandler(file_handler)
```

### 2. 헬스체크 엔드포인트

```python
# backend/app/api/endpoints/health.py
from fastapi import APIRouter, HTTPException
from app.core.azure_client import AzureOpenAIClient

router = APIRouter()

@router.get("/health")
async def health_check():
    try:
        # Azure OpenAI 연결 테스트
        client = AzureOpenAIClient()
        await client.test_connection()
        
        return {
            "status": "healthy",
            "timestamp": datetime.utcnow(),
            "services": {
                "azure_openai": "connected",
                "file_system": "accessible"
            }
        }
    except Exception as e:
        raise HTTPException(status_code=503, detail=f"Service unhealthy: {str(e)}")
```

### 3. 메트릭 수집

```bash
# Prometheus 설정 예시
# prometheus.yml
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'korean-doc-checker'
    static_configs:
      - targets: ['localhost:8000']
    metrics_path: '/metrics'
```

## 🔒 보안 설정

### 1. HTTPS 설정 (Let's Encrypt)

```bash
# Certbot 설치
sudo apt install -y certbot python3-certbot-nginx

# SSL 인증서 발급
sudo certbot --nginx -d your-domain.com

# 자동 갱신 설정
sudo crontab -e
# 다음 라인 추가:
# 0 12 * * * /usr/bin/certbot renew --quiet
```

### 2. 방화벽 설정

```bash
# UFW 방화벽 설정
sudo ufw enable
sudo ufw allow ssh
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp
sudo ufw deny 8000/tcp  # 백엔드 직접 접근 차단
sudo ufw deny 8501/tcp  # 프론트엔드 직접 접근 차단
```

### 3. API 키 보안

```bash
# 환경 변수를 시스템 서비스로 관리
sudo mkdir -p /etc/korean-doc-checker
sudo tee /etc/korean-doc-checker/environment << EOF
AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/
AZURE_OPENAI_API_KEY=your-secure-api-key
EOF

sudo chmod 600 /etc/korean-doc-checker/environment
sudo chown root:root /etc/korean-doc-checker/environment
```

## 🔄 업데이트 및 롤백

### 1. 무중단 배포

```bash
#!/bin/bash
# deploy.sh

# 새 버전 배포
git pull origin main
cd backend && uv sync
cd ../frontend && uv sync

# 서비스 재시작 (무중단)
sudo systemctl reload korean-doc-checker-backend
sudo systemctl reload korean-doc-checker-frontend

# 헬스체크
curl -f http://localhost:8000/api/health || {
    echo "Health check failed, rolling back..."
    git checkout HEAD~1
    sudo systemctl restart korean-doc-checker-backend
    sudo systemctl restart korean-doc-checker-frontend
    exit 1
}

echo "Deployment successful!"
```

### 2. 백업 및 복구

```bash
# 설정 백업
sudo tar -czf /backup/korean-doc-checker-$(date +%Y%m%d).tar.gz \
  /opt/korean-document-checker \
  /etc/systemd/system/korean-doc-checker-*.service \
  /etc/nginx/sites-available/korean-doc-checker

# 복구
sudo tar -xzf /backup/korean-doc-checker-20240101.tar.gz -C /
sudo systemctl daemon-reload
sudo systemctl restart korean-doc-checker-backend korean-doc-checker-frontend nginx
```

## 📈 성능 최적화

### 1. 백엔드 최적화

```python
# Gunicorn 설정
# gunicorn.conf.py
bind = "0.0.0.0:8000"
workers = 4
worker_class = "uvicorn.workers.UvicornWorker"
worker_connections = 1000
max_requests = 1000
max_requests_jitter = 100
timeout = 300
keepalive = 2
```

### 2. 캐싱 설정

```python
# Redis 캐싱 (선택사항)
from redis import Redis
import json

redis_client = Redis(host='localhost', port=6379, db=0)

async def cached_analysis(content_hash: str, analysis_func):
    cached_result = redis_client.get(f"analysis:{content_hash}")
    if cached_result:
        return json.loads(cached_result)
    
    result = await analysis_func()
    redis_client.setex(f"analysis:{content_hash}", 3600, json.dumps(result))
    return result
```

## 🚨 문제 해결

### 일반적인 배포 문제

1. **포트 충돌**
   ```bash
   sudo netstat -tlnp | grep :8000
   sudo kill -9 <PID>
   ```

2. **권한 문제**
   ```bash
   sudo chown -R www-data:www-data /opt/korean-document-checker
   sudo chmod -R 755 /opt/korean-document-checker
   ```

3. **메모리 부족**
   ```bash
   # 스왑 파일 생성
   sudo fallocate -l 2G /swapfile
   sudo chmod 600 /swapfile
   sudo mkswap /swapfile
   sudo swapon /swapfile
   ```

4. **Azure OpenAI 연결 실패**
   ```bash
   # 네트워크 연결 테스트
   curl -v https://your-resource.openai.azure.com/
   
   # DNS 확인
   nslookup your-resource.openai.azure.com
   ```

이 가이드를 따라 배포하면 안정적이고 확장 가능한 한국어 문서 검사 서비스를 운영할 수 있습니다.