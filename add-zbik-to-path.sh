#!/bin/bash

mkdir -p bin
ln -sf $(pwd)/julia ./bin/zbik
echo "export PATH=\"$(pwd)/bin:\$PATH\"" >> ~/.bashrc
source ~/.bashrc
