FROM mcr.microsoft.com/devcontainers/cpp:1-debian

RUN <<EOT
apt-get update
apt-get --yes install --no-install-recommends \
    ccache \
    clang \
    clang-format \
    gfortran \
    m4
rm -rf /var/lib/apt/lists/*
EOT
