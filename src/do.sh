llc -relocation-model=pic -march=x86-64 a.ll -o a.s
clang -march=x86-64  a.s ./lib/edsgerlib.a -lm -o a.out 