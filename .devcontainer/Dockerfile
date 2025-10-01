FROM mcr.microsoft.com/devcontainers/cpp:1-debian

RUN <<EOT
apt-get update
# Install required build tools and external libraries
apt-get --yes install --no-install-recommends \
    build-essential \
    cmake \
    curl \
    gfortran \
    git \
    libatomic1 \
    m4 \
    perl \
    pkg-config \
    python3 \
    wget
# Install additional packages
apt-get --yes install --no-install-recommends \
    ccache \
    clang \
    clang-format
rm -rf /var/lib/apt/lists/*
EOT
