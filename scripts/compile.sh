#!/bin/bash 
filename=$1
program=$2

mkdir bin
pasc $filename 
PascBackend pkg/codegen/out/$program.bin
llc -filetype=obj bin/$program.ll -o bin/$program.o
clang bin/$program.o -o bin/$program -L/usr/local/lib