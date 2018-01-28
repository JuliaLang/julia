#!/bin/sh

set -xe

export JULIA='../../julia'
export BIN='../../tmp'

mkdir -vp tmp
gmake -C examples/embedding check
