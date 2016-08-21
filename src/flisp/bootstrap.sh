#!/bin/sh

cp flisp.boot flisp.boot.bak

printf "Creating stage 0 boot file...\n"
#../../branches/interpreter/femtolisp/flisp mkboot0.lsp system.lsp compiler.lsp > flisp.boot.new
./flisp mkboot0.lsp system.lsp compiler.lsp > flisp.boot.new
mv flisp.boot.new flisp.boot

printf "Creating stage 1 boot file...\n"
./flisp mkboot1.lsp

printf "Testing...\n"
make test
