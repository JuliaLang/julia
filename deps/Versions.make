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
CLANG_JLL_VER  := 13.0.1+0

# DSFMT
DSFMT_VER := 2.2.4
DSFMT_JLL_NAME := dSFMT

# GMP
GMP_VER := 6.2.1
GMP_JLL_NAME := GMP

# LibCURL
CURL_VER := 7.83.1
CURL_JLL_NAME := LibCURL

# LAPACK, source-only
LAPACK_VER := 3.9.0

# LibGit2
LIBGIT2_JLL_NAME := LibGit2
LIBGIT2_BRANCH = v1.3.0
LIBGIT2_SHA1 = b7bad55e4bb0a285b073ba5e02b01d3f522fc95d

# LibSSH2
LIBSSH2_JLL_NAME := LibSSH2
LIBSSH2_VER := 1.10.2
LIBSSH2_BRANCH = libssh2-1.10.0
LIBSSH2_SHA1 = 635caa90787220ac3773c1d5ba11f1236c22eae8

# LibUV
LIBUV_JLL_NAME := LibUV
LIBUV_VER := 2
LIBUV_BRANCH = julia-uv2-1.44.1
LIBUV_SHA1 = 1b2d16477fe1142adea952168d828a066e03ee4c

# LibWhich
LIBWHICH_BRANCH = master
LIBWHICH_SHA1 = 81e9723c0273d78493dc8c8ed570f68d9ce7e89e

# LLVM
LLVM_JLL_NAME := libLLVM
LLVM_VER := 13.0.1
LLVM_ASSERT_JLL_VER := 13.0.1+0
LLVM_BRANCH = julia-13.0.1-0
LLVM_SHA1 = julia-13.0.1-0

# LLVM_tools (downloads LLVM_jll to get things like `lit` and `opt`)
LLVM_TOOLS_JLL_NAME := LLVM
LLVM_TOOLS_JLL_VER := 13.0.1+0
LLVM_TOOLS_ASSERT_JLL_VER := 13.0.1+0

# LLVM libunwind
LLVMUNWIND_VER := 12.0.1
LLVMUNWIND_JLL_NAME := LLVMLibUnwind

# MbedTLS
MBEDTLS_VER := 2.28.0
MBEDTLS_JLL_NAME := MbedTLS

# MPFR
MPFR_VER := 4.1.0
MPFR_JLL_NAME := MPFR

# nghttp2
NGHTTP2_VER := 1.47.0
NGHTTP2_JLL_NAME := nghttp2

# Objconv (we don't ship this, so no need for a fake JLL; therefore we specify the JLL_VER here)
OBJCONV_VER := 2.49.1
OBJCONV_JLL_NAME := Objconv
OBJCONV_JLL_VER  := 2.49.1+0

# blastrampoline
BLASTRAMPOLINE_JLL_NAME := libblastrampoline
BLASTRAMPOLINE_VER := 5.1.0
BLASTRAMPOLINE_BRANCH = v5.0.1
BLASTRAMPOLINE_SHA1 = d32042273719672c6669f6442a0be5605d434b70

# OpenBLAS
OPENBLAS_JLL_NAME := OpenBLAS
OPENBLAS_VER := 0.3.17
OPENBLAS_BRANCH = v0.3.20
OPENBLAS_SHA1 = 0b678b19dc03f2a999d6e038814c4c50b9640a4e

# OpenLibm
OPENLIBM_JLL_NAME := OpenLibm
OPENLIBM_VER := 0.8.1
OPENLIBM_BRANCH = v0.8.1
OPENLIBM_SHA1 = ae2d91698508701c83cab83714d42a1146dccf85

# Patchelf (we don't ship this or even use a JLL, we just always build it)
PATCHELF_VER := 0.13

# p7zip
P7ZIP_VER := 17.04
P7ZIP_JLL_NAME := p7zip

# PCRE
PCRE_VER := 10.40
PCRE_JLL_NAME := PCRE2

# SuiteSparse
LIBSUITESPARSE_VER := 5.10.1
LIBSUITESPARSE_JLL_NAME := SuiteSparse

# unwind
UNWIND_VER := 1.5.0
UNWIND_VER_TAG := 1.5
UNWIND_JLL_NAME := LibUnwind
UNWIND_JLL_VER  := 1.5.0+1

# UTF8proc
UTF8PROC_BRANCH = v2.7.0
UTF8PROC_SHA1 = 8ca6144c85c165987cb1c5d8395c7314e13d4cd7

# zlib
ZLIB_JLL_NAME := Zlib
ZLIB_VER := 1.2.12
ZLIB_BRANCH = v1.2.12
ZLIB_SHA1 = 21767c654d31d2dccdde4330529775c6c5fd5389

# Specify the version of the Mozilla CA Certificate Store to obtain.
# The versions of cacert.pem are identified by the date (YYYY-MM-DD) of their changes.
# See https://curl.haxx.se/docs/caextract.html for more details.
MOZILLA_CACERT_VERSION := 2022-02-01
