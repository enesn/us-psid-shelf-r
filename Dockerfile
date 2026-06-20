
FROM rocker/tidyverse:4.5

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
        'writexl', \
        'yaml', \
        'digest', \
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
# SSH server (login as root, for Positron Remote SSH)
# =========================
ARG SSH_PUBKEY=""

RUN mkdir -p /run/sshd /root/.ssh && \
    sed -i 's/^#*PermitRootLogin.*/PermitRootLogin prohibit-password/' /etc/ssh/sshd_config && \
    sed -i 's/^#*PasswordAuthentication.*/PasswordAuthentication no/'   /etc/ssh/sshd_config && \
    if [ -n "${SSH_PUBKEY}" ]; then \
      echo "${SSH_PUBKEY}" > /root/.ssh/authorized_keys && \
      chmod 700 /root/.ssh && chmod 600 /root/.ssh/authorized_keys ; \
    fi

# run sshd as an s6 service so it starts automatically with /init
RUN mkdir -p /etc/services.d/sshd && \
    printf '#!/bin/sh\nmkdir -p /run/sshd\nexec /usr/sbin/sshd -D -e\n' \
      > /etc/services.d/sshd/run && \
    chmod +x /etc/services.d/sshd/run

EXPOSE 22 8787