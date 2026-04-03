# syntax=docker/dockerfile:1

# =============================================================================
# Stage 1: Chef - Install cargo-chef for dependency caching
# =============================================================================
FROM rust:bookworm AS chef
RUN cargo install cargo-chef
WORKDIR /app

# =============================================================================
# Stage 2: Planner - Generate dependency recipe
# =============================================================================
FROM chef AS planner
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

# =============================================================================
# Stage 3: Builder - Build all workspace binaries
# =============================================================================
FROM chef AS builder

# libclang-dev: required by bindgen (build-dep of process crate)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libclang-dev \
    && rm -rf /var/lib/apt/lists/*

# Build dependencies first (cached layer)
COPY --from=planner /app/recipe.json recipe.json
RUN cargo chef cook --release --recipe-path recipe.json

# Build all workspace binaries
COPY . .
RUN cargo build --release

# Collect binaries into staging directory
RUN mkdir -p /app/staging/bin && \
    find target/release -maxdepth 1 -type f -executable \
        ! -name '*.d' ! -name 'build-script-*' \
        -exec cp {} /app/staging/bin/ \;

# =============================================================================
# Stage 4: Runtime - Minimal image with binaries
# =============================================================================
FROM ubuntu:24.04 AS runtime

LABEL org.opencontainers.image.source="https://github.com/rustcoreutils/posixutils-rs"
LABEL org.opencontainers.image.licenses="MIT"
LABEL org.opencontainers.image.description="Rust-native POSIX utilities (130+ commands)"
LABEL org.opencontainers.image.title="posixutils-rs"

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN useradd --create-home --shell /bin/bash posixutils

COPY --from=builder /app/staging/bin/ /usr/local/bin/

# argv[0]-based symlinks (vi->ex, compress->uncompress/zcat)
RUN ln -sf vi /usr/local/bin/ex && \
    ln -sf compress /usr/local/bin/uncompress && \
    ln -sf compress /usr/local/bin/zcat

USER posixutils
WORKDIR /home/posixutils

CMD ["sh"]
