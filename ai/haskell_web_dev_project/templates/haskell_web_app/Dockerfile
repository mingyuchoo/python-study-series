# Use the official Haskell image as the base image
FROM haskell:9.2.5 as build

# Set working directory
WORKDIR /app

# Copy cabal file and install dependencies
COPY haskell-web-app.cabal ./
RUN cabal update && \
    cabal build --only-dependencies -j4

# Copy the rest of the application
COPY . .

# Build the application
RUN cabal build

# Create a new stage with a minimal image
FROM debian:bullseye-slim

# Install runtime dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libgmp10 \
    ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy the built executable from the build stage
COPY --from=build /app/dist-newstyle/build/x86_64-linux/ghc-9.2.5/haskell-web-app-0.1.0.0/x/haskell-web-app/build/haskell-web-app/haskell-web-app .

# Copy static files
COPY --from=build /app/static ./static

# Expose the port the app will run on
EXPOSE 8080

# Set environment variables
ENV PORT=8080

# Command to run the application
CMD ["./haskell-web-app"]
