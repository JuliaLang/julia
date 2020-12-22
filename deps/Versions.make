## Dependencies and where to find them, listed in alphabetical order

# To define a new dependency, you need to know the following pieces of information:
#
#  * The Makefile variable stem; for LibCURL this is just "CURL".
#  * The JLL name; for GMP this is "GMP", while for LLVM it could be "LLVM_full" or "LLVM_full_assert"
#  * The upstream source version; for dSFMT this is currently "2.2.3"
#
# Everything else will be auto-generated.  In particular, the version listed here
# represents the upstream source version; the JLL binary version that gets downloaded is
# controlled by the `Project.toml` files in `stdlib/XXX_jll/`.

# Compiler Support Libraries
CSL_JLL_NAME := CompilerSupportLibraries

# Clang (paired with LLVM, only here as a JLL download)
CLANG_JLL_NAME := Clang
CLANG_JLL_VER  := 11.0.0+5

# DSFMT
DSFMT_VER := 2.2.4
DSFMT_JLL_NAME := dSFMT

# GMP
GMP_VER := 6.2.0
GMP_JLL_NAME := GMP

# LibCURL
CURL_VER := 7.73.0
CURL_JLL_NAME := LibCURL

# LAPACK, source-only
LAPACK_VER := 3.9.0

# LibGit2
LIBGIT2_VER := 1.1.0
LIBGIT2_JLL_NAME := LibGit2

# LibSSH2
LIBSSH2_VER := 1.9.0
LIBSSH2_JLL_NAME := LibSSH2

# LibUV
LIBUV_VER := 2
LIBUV_JLL_NAME := LibUV

# LLVM
LLVM_VER := 11.0.0
LLVM_JLL_NAME := libLLVM
# We provide a way to subversively swap out which LLVM JLL we pull artifacts from
ifeq ($(BINARYBUILDER_LLVM_ASSERTS), 1)
LLVM_JLL_VER := 11.0.0+4
LLVM_JLL_DOWNLOAD_NAME := libLLVM_assert
endif

# LLVM_tools (downloads LLVM_jll to get things like `lit` and `opt`)
LLVM_TOOLS_JLL_NAME := LLVM
LLVM_TOOLS_JLL_VER := 11.0.0+6

# MbedTLS
MBEDTLS_VER := 2.24.0
MBEDTLS_JLL_NAME := MbedTLS

# MPFR
MPFR_VER := 4.1.0
MPFR_JLL_NAME := MPFR

# nghttp2
NGHTTP2_VER := 1.41.0
NGHTTP2_JLL_NAME := nghttp2

# Objconv (we don't ship this, so no need for a fake JLL; therefore we specify the JLL_VER here)
OBJCONV_VER := 2.49.1
OBJCONV_JLL_NAME := Objconv
OBJCONV_JLL_VER  := 2.49.1+0

# OpenBLAS
OPENBLAS_VER := 0.3.10
OPENBLAS_JLL_NAME := OpenBLAS

# OpenLibm
OPENLIBM_VER := 0.7.3
OPENLIBM_JLL_NAME := OpenLibm

# OSXUnwind (we statically link this, so no need for a fake JLL; therefore we specify the JLL_VER here)
OSXUNWIND_VER := 0.0.6
OSXUNWIND_JLL_NAME := LibOSXUnwind
OSXUNWIND_JLL_VER  := 0.0.6+1

# Patchelf (we don't ship this or even use a JLL, we just always build it)
PATCHELF_VER := 0.9

# p7zip
P7ZIP_VER := 16.2.0
P7ZIP_JLL_NAME := p7zip

# PCRE
PCRE_VER := 10.35
PCRE_JLL_NAME := PCRE2

# SuiteSparse
SUITESPARSE_VER := 5.4.0
SUITESPARSE_JLL_NAME := SuiteSparse

# unwind
UNWIND_VER := 1.3.2
UNWIND_JLL_NAME := LibUnwind

# zlib
ZLIB_VER := 1.2.11
ZLIB_JLL_NAME := Zlib

# Specify the version of the Mozilla CA Certificate Store to obtain.
# The versions of cacert.pem are identified by the date (YYYY-MM-DD) of their changes.
# See https://curl.haxx.se/docs/caextract.html for more details.
MOZILLA_CACERT_VERSION := 2020-10-14
