# these aren't julia environment variables #
BASE="/home/alexfanqi/vm_share"
JULIA_SRC="${BASE}/src/julia"
# ======================================== #
cd ${JULIA_SRC}

cat <<-EOF > Make.user
	bindir=/usr/bin
	prefix=/usr
	sysconfdir=/etc
	BUNDLE_DEBUG_LIBS:=0
	USE_BINARYBUILDER:=0
	USE_SYSTEM_CSL:=1
	USE_SYSTEM_LLVM:=1
	USE_SYSTEM_LIBUNWIND:=1
	USE_SYSTEM_PCRE:=1
	USE_SYSTEM_LIBM:=0
	USE_SYSTEM_OPENLIBM:=1
	USE_SYSTEM_DSFMT:=1
	USE_SYSTEM_BLAS:=1
	USE_SYSTEM_LAPACK:=1
	USE_SYSTEM_LIBBLASTRAMPOLINE:=0
	USE_SYSTEM_GMP:=1
	USE_SYSTEM_MPFR:=1
	USE_SYSTEM_LIBSUITESPARSE:=1
	USE_SYSTEM_LIBUV:=0
	USE_SYSTEM_UTF8PROC:=1
	USE_SYSTEM_MBEDTLS:=1
	USE_SYSTEM_LIBSSH2:=1
	USE_SYSTEM_NGHTTP2:=1
	USE_SYSTEM_CURL:=1
	USE_SYSTEM_LIBGIT2:=1
	USE_SYSTEM_PATCHELF:=1
	USE_SYSTEM_ZLIB:=1
	USE_SYSTEM_P7ZIP:=1
	USE_PERF_JITEVENTS:=0
	FORCE_ASSERTIONS:=0
	LLVM_ASSERTIONS=0
	JULIA_PRECOMPILE=1
	VERBOSE:=1
EOF

export CCACHE_DIR="${BASE}/ccache/julia"
#export PATH="/usr/lib/llvm/14/bin/:${PATH}"
export PATH="${BASE}/rootfs/llvm-release-14.0.0/bin:${PATH}"
export PATH="/usr/lib/ccache/bin/:${PATH}"
export LDFLAGS="-rtlib=compiler-rt -unwindlib=libgcc -Wl,--as-needed"
# otherwise open_libllvm in binaryplatforms.jl "Unable to open libLLVM!"
#export LD_LIBRARY_PATH=/usr/lib/llvm/15/lib64/
export LD_LIBRARY_PATH="${BASE}/rootfs/llvm-release-14.0.0/lib/"
#export LBT_DEFAULT_LIBS=/usr/lib64/libopenblas64.so

NPROC=$(nproc)
#make CC=clang CXX=clang++ LLVM_CONFIG="/usr/lib/llvm/15/bin/llvm-config" -j $((NPROC/2)) release
make CC=clang CXX=clang++ LLVM_CONFIG="${BASE}/rootfs/llvm-release-14.0.0/bin/llvm-config" -j $((NPROC/2)) release
