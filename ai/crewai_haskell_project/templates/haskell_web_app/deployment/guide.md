# Deployment Guide for Haskell Web Application

This guide provides instructions for deploying the Haskell web application to various environments.

## Prerequisites

- Docker and Docker Compose installed
- Access to a server or cloud provider (AWS, GCP, Azure, etc.)
- Domain name (optional)

## Local Deployment with Docker

1. Build the Docker image:
   ```bash
   docker build -t haskell-web-app .
   ```

2. Run the container:
   ```bash
   docker run -p 8080:8080 haskell-web-app
   ```

3. Access the application at http://localhost:8080

## Deployment with Docker Compose

1. Create a `docker-compose.yml` file:
   ```yaml
   version: '3'
   services:
     app:
       build: .
       ports:
         - "8080:8080"
       environment:
         - PORT=8080
       restart: always
   ```

2. Start the services:
   ```bash
   docker-compose up -d
   ```

## Cloud Deployment

### AWS Elastic Beanstalk

1. Install the EB CLI:
   ```bash
   uv pip install awsebcli
   ```

2. Initialize EB application:
   ```bash
   eb init
   ```

3. Create an environment and deploy:
   ```bash
   eb create haskell-web-app-env
   ```

4. For subsequent deployments:
   ```bash
   eb deploy
   ```

### Heroku

1. Create a `heroku.yml` file:
   ```yaml
   build:
     docker:
       web: Dockerfile
   ```

2. Create a Heroku app:
   ```bash
   heroku create
   ```

3. Set the stack to container:
   ```bash
   heroku stack:set container
   ```

4. Deploy to Heroku:
   ```bash
   git push heroku main
   ```

## Production Considerations

### Environment Variables

The application uses the following environment variables:

- `PORT`: The port number to listen on (default: 8080)

### Scaling

For horizontal scaling, consider using:

- Kubernetes for container orchestration
- AWS Auto Scaling Groups
- Docker Swarm

### Monitoring

Implement monitoring using:

- Prometheus for metrics collection
- Grafana for visualization
- ELK stack for log management

### Security

- Use HTTPS in production
- Implement rate limiting
- Set up a firewall
- Use secure headers

## Continuous Deployment

The repository includes GitHub Actions workflows for CI/CD. To enable automatic deployments:

1. Set up the required secrets in your GitHub repository:
   - `DOCKERHUB_USERNAME`
   - `DOCKERHUB_TOKEN`

2. For cloud provider deployments, add additional secrets as needed.

## Backup and Disaster Recovery

1. Regularly backup any persistent data
2. Implement a disaster recovery plan
3. Test recovery procedures periodically

## Troubleshooting

### Common Issues

1. **Port already in use**:
   ```bash
   docker run -p 8081:8080 haskell-web-app
   ```

2. **Container not starting**:
   Check logs with:
   ```bash
   docker logs <container_id>
   ```

3. **Performance issues**:
   - Increase container resources
   - Optimize database queries
   - Implement caching
