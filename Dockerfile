# syntax=docker/dockerfile:1.4
FROM rocker/tidyverse:4.6

ENV DEBIAN_FRONTEND=noninteractive

# Let the R arrow package download a prebuilt libarrow instead of
# compiling the whole Arrow C++ stack (saves ~20-30 min on its own).
ENV LIBARROW_BINARY=true
ENV ARROW_R_DEV=false

# =========================
# System dependencies
# =========================
RUN apt-get update && apt-get install -y \
    openssh-server \
    sudo \
    git \
    curl \
    wget \
    vim \
    nano \
    htop \
    tmux \
    rsync \
    unzip \
    zip \
    jq \
    build-essential \
    cmake \
    pkg-config \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff-dev \
    libjpeg-dev \
    libicu-dev \
    libsqlite3-dev \
 && rm -rf /var/lib/apt/lists/*

# =========================
# SSH setup
# =========================
RUN mkdir -p /var/run/sshd && \
    ssh-keygen -A

# =========================
# Create user (SAFE: no UID/GID conflicts)
# =========================
ARG USERNAME=developer

RUN useradd -m -s /bin/bash ${USERNAME} && \
    echo "${USERNAME} ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/${USERNAME} && \
    chmod 0440 /etc/sudoers.d/${USERNAME}

# =========================
# SSH directory setup
# =========================
RUN mkdir -p /home/${USERNAME}/.ssh && \
    chown -R ${USERNAME}:${USERNAME} /home/${USERNAME}/.ssh && \
    chmod 700 /home/${USERNAME}/.ssh

# =========================
# SSH configuration
# =========================
RUN printf '%s\n' \
'Port 22' \
'PermitRootLogin no' \
'PubkeyAuthentication yes' \
'PasswordAuthentication no' \
'KbdInteractiveAuthentication no' \
'ChallengeResponseAuthentication no' \
'UsePAM yes' \
'AllowTcpForwarding yes' \
'AllowAgentForwarding yes' \
'X11Forwarding no' \
'AuthorizedKeysFile .ssh/authorized_keys' \
'ClientAliveInterval 60' \
'ClientAliveCountMax 3' \
> /etc/ssh/sshd_config

# =========================
# R packages
#  - NO repos= override: use rocker's preconfigured P3M binary repo
#  - Ncpus: compile any source fallbacks in parallel
#  - pak: parallel downloads + better binary resolution
#  - BuildKit cache mount: don't repay download/compile on rebuilds
# =========================
RUN --mount=type=cache,target=/root/.cache/R/pkgcache \
    R -q -e "options(Ncpus=parallel::detectCores()); \
      install.packages('pak'); \
      pak::pkg_install(c( \
        'arrow', \
        'duckdb', \
        'DBI', \
        'data.table', \
        'fst', \
        'fixest', \
        'modelsummary', \
        'gt', \
        'janitor', \
        'targets', \
        'future', \
        'furrr', \
        'here', \
        'glue', \
        'cli', \
        'renv', \
        'quarto' \
      ))"

# =========================
# Workspace
# =========================
WORKDIR /workspace

EXPOSE 22

CMD ["/usr/sbin/sshd", "-D", "-e"]
