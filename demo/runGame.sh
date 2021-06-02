#!/bin/bash
rm *.o life
clang -c conway.c
# stack run -- test.wat
llc-9 --filetype=obj test.ll
clang conway.o test.o -lm -lncurses -o life
./life
# ./life > gameoutput.txt
# cat gameoutput.txt