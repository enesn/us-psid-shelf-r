FROM ghcr.io/enesn/r-econops:052026

ENV DEBIAN_FRONTEND=noninteractive

# Install SSH server and useful tools
RUN apt-get update && apt-get install -y \
    openssh-server \
    sudo \
    git \
    curl \
    wget \
    vim \
    nano \
    procps \
    htop \
    && rm -rf /var/lib/apt/lists/*

# Create SSH runtime directory
RUN mkdir /var/run/sshd

# Create developer user
ARG USERNAME=developer
ARG UID=1000
ARG GID=1000

RUN groupadd -g ${GID} ${USERNAME} && \
    useradd -m -u ${UID} -g ${GID} -s /bin/bash ${USERNAME}

# Passwordless sudo
RUN echo "${USERNAME} ALL=(ALL) NOPASSWD:ALL" > \
    /etc/sudoers.d/${USERNAME}

# Prepare SSH directory
RUN mkdir -p /home/${USERNAME}/.ssh && \
    chown -R ${USERNAME}:${USERNAME} /home/${USERNAME}/.ssh && \
    chmod 700 /home/${USERNAME}/.ssh

# SSH configuration
RUN sed -i 's/#PubkeyAuthentication yes/PubkeyAuthentication yes/' /etc/ssh/sshd_config && \
    sed -i 's/#PasswordAuthentication yes/PasswordAuthentication no/' /etc/ssh/sshd_config && \
    echo "PermitRootLogin no" >> /etc/ssh/sshd_config && \
    echo "AllowUsers ${USERNAME}" >> /etc/ssh/sshd_config

WORKDIR /workspace

EXPOSE 22

CMD ["/usr/sbin/sshd", "-D"]