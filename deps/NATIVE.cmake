# native toolchain file to fix llvm cross-compilation finickiness
# ref http://lists.llvm.org/pipermail/llvm-dev/2016-February/095366.html
set(CMAKE_C_COMPILER cc)
set(CMAKE_CXX_COMPILER c++)
