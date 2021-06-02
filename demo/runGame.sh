#!/bin/bash
rm *.o exe
clang -c conway.c
stack run -- test.wat
llc-9 --filetype=obj test.ll
clang conway.o test.o -lm -o life
./life 