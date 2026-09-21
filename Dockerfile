# syntax=docker/dockerfile:1

# posixutils-rs container image.
#
# The result is a toolbox: every workspace binary installed into
# /usr/local/bin, which precedes /usr/bin in the default PATH, so `ls`, `sort`,
# `tar` and friends resolve to our implementations and not the distro's.

# One place to bump the toolchain. Pinned rather than floating: `rust:bookworm`
# silently followed the toolchain from 1.84 to 1.98 over the life of this file.
ARG RUST_VERSION=1.98.1
ARG CHEF_VERSION=0.1.78

# =============================================================================
# Stage 1: Chef - Install cargo-chef for dependency caching
# =============================================================================
# The official image, so the toolchain has one upstream rather than two. The
# install is its own layer and re-runs only when one of the two ARGs above
# changes.
FROM rust:${RUST_VERSION}-bookworm AS chef
ARG CHEF_VERSION
RUN cargo install --locked cargo-chef@${CHEF_VERSION}
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

# Build dependencies first (cached layer)
COPY --from=planner /app/recipe.json recipe.json
RUN --mount=type=cache,target=/usr/local/cargo/registry,sharing=locked \
    --mount=type=cache,target=/usr/local/cargo/git,sharing=locked \
    cargo chef cook --release --locked --recipe-path recipe.json

# Build all workspace binaries
COPY . .
RUN --mount=type=cache,target=/usr/local/cargo/registry,sharing=locked \
    --mount=type=cache,target=/usr/local/cargo/git,sharing=locked \
    cargo build --release --locked

# Collect binaries into a staging directory.
#
# The symlinks matter as much as the files: `tar`, `cpio`, `ex`, `uncompress`,
# `zcat` and `[` are argv[0] front-ends that the crates' build scripts drop
# next to their binary, and a `-type f` sweep silently leaves them behind.
# Copying whatever the build produced keeps this list from drifting again --
# the hand-written `ln -sf` roster this replaced had already lost `tar` and
# `cpio`, so the image served the distro's GNU tar instead.
RUN set -eu; \
    mkdir -p /app/staging/bin; \
    find target/release -maxdepth 1 \
        \( -type f -executable -o -type l \) \
        ! -name '*.d' ! -name '*.so' \
        -exec cp -a -t /app/staging/bin/ {} +; \
    find /app/staging/bin -maxdepth 1 -type f -exec strip --strip-all {} +; \
    ls /app/staging/bin | wc -l

# =============================================================================
# Stage 4: Runtime - Minimal image with binaries
# =============================================================================
# bookworm-slim, not ubuntu: the same glibc the builder linked against, so a
# binary cannot depend on a symbol version the runtime lacks.
FROM debian:bookworm-slim AS runtime

# Populated by CI (docker/metadata-action). Declared here so an unlabelled
# local build does not inherit the base image's own version label -- the
# ubuntu-based image used to advertise itself as version "24.04".
ARG VERSION=0.9.0
ARG REVISION=
ARG CREATED=

LABEL org.opencontainers.image.source="https://github.com/rustcoreutils/posixutils-rs"
LABEL org.opencontainers.image.licenses="MIT"
LABEL org.opencontainers.image.description="Rust-native POSIX utilities (130+ commands)"
LABEL org.opencontainers.image.title="posixutils-rs"
LABEL org.opencontainers.image.version="${VERSION}"
LABEL org.opencontainers.image.revision="${REVISION}"
LABEL org.opencontainers.image.created="${CREATED}"

# ncurses-base: the terminfo database, without which vi, more, screen and tput
#   have no terminal to drive.
# tzdata: without /usr/share/zoneinfo, `TZ=America/New_York date` printed the
#   zone name as its own abbreviation.
# ca-certificates is deliberately absent: nothing links a TLS stack (ipp pulls
#   ureq with default-features off), so it was 3.9 MB of unused trust store.
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt/lists,sharing=locked \
    apt-get update && apt-get install -y --no-install-recommends \
    ncurses-base \
    tzdata

# A fixed uid/gid, so a bind-mounted host directory has predictable ownership.
# Letting useradd choose gave 1001 on ubuntu (which ships its own uid 1000) and
# would give 1000 here -- the same image, two different owners.
RUN groupadd --gid 1000 posixutils && \
    useradd --uid 1000 --gid 1000 --create-home --shell /usr/local/bin/sh posixutils

COPY --from=builder /app/staging/bin/ /usr/local/bin/

# crond and at read their jobs from the spool; neither directory exists in a
# base image.
RUN mkdir -p /var/spool/cron /var/spool/atjobs && \
    chmod 1733 /var/spool/atjobs

ENV LANG=C.UTF-8

USER 1000:1000
WORKDIR /home/posixutils

CMD ["sh"]
