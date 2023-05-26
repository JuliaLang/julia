#!/bin/bash

mkdir -p bin
ln -sf $(pwd)/julia ./bin/zbik
ln -sf $(pwd)/julia ./bin/żbik
echo "export PATH=\"$(pwd)/bin:\$PATH\"" >> ~/.profile
source ~/.profile
