#!/bin/bash
clang -c maintester.c
stack run -- test1.wat
llc-9 --filetype=obj test1.ll
clang maintester.o test1.o -o exe
./exe > output.txt
cat output.txt