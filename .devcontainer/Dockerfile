FROM julia:latest

RUN apt-get update && apt-get install -y build-essential libatomic1 python gfortran perl wget m4 cmake pkg-config git
RUN mkdir -p /root/.vscode-server/data/Machine/
COPY settings.json /root/.vscode-server/data/Machine/