FROM gitpod/workspace-full

USER gitpod

RUN sudo apt-get update \
    && sudo apt-get install -y \
        build-essential \
        libatomic1 \
        python \
        gfortran \
        perl \
        wget \
        m4 \
        cmake \
        pkg-config \
    && sudo rm -rf /var/lib/apt/lists/*