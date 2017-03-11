#!/bin/sh
# This file is a part of Julia. License is MIT: http://julialang.org/license
# Requires maven and java 8

./mvnw clean install
java -jar target/benchmarks.jar -bm AverageTime -tu ns

# Fast invocation of benchmarks:
# java -jar target/benchmarks.jar -wf 1 -wi 1 -i 1 -bm AverageTime -tu ms
