FROM mcr.microsoft.com/devcontainers/cpp:1-debian

RUN <<EOT
apt-get update
apt-get --yes install --no-install-recommends \
    gfortran \
    m4
rm -rf /var/lib/apt/lists/*
EOT
