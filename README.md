# wasmc
A WASM compiler using LLVM. To run, `stack run -- test.wat` and then `llc --filetype=obj test.ll` and `clang test.o -o test`.


# https://bcain-llvm.readthedocs.io/projects/clang/en/latest/Toolchain/
clang-cl -fuse-ld=lld-link test.ll libstdc.lib

or

clang-cl -fuse-ld=lld-link test.ll -stdlib

or

using lld directly from the command line. 

write a header file with matching wasm function type sigs
write a c file some_program and include the header file
clang -c some_program.c
llc --filetype=obj lib.wat
clang some_program.o lib.o -o exe
