The Julia language is licensed under the MIT License (see [LICENSE.md](./LICENSE.md) ). The "language" consists
of the compiler (the contents of `src/`), most of the standard library (`base/` and `stdlib/`),
and some utilities (most of the rest of the files in this repository). See below
for exceptions.

- [crc32c.c](https://stackoverflow.com/questions/17645167/implementing-sse-4-2s-crc32c-in-software) (CRC-32c checksum code by Mark Adler) [[ZLib](https://opensource.org/licenses/Zlib)].
- [LDC](https://github.com/ldc-developers/ldc/blob/master/LICENSE) (for ccall/cfunction ABI definitions) [BSD-3]. The portion of code that Julia uses from LDC is [BSD-3] licensed.
- [LLVM](https://releases.llvm.org/3.9.0/LICENSE.TXT) (for parts of src/disasm.cpp) [UIUC]
- [NetBSD](https://www.netbsd.org/about/redistribution.html) (for setjmp, longjmp, and strptime implementations on Windows) [BSD-3]
- [Python](https://docs.python.org/3/license.html) (for strtod implementation on Windows) [PSF]
- [FEMTOLISP](https://github.com/JeffBezanson/femtolisp) [BSD-3]

The following components included in Julia `Base` have their own separate licenses:

- base/ryu/* [Boost] (see [ryu](https://github.com/ulfjack/ryu/blob/master/LICENSE-Boost))
- base/special/{rem_pio2,hyperbolic}.jl [Freely distributable with preserved copyright notice] (see [FDLIBM](https://www.netlib.org/fdlibm))

The Julia language links to the following external libraries, which have their
own licenses:

- [LIBUNWIND](https://github.com/libunwind/libunwind/blob/master/LICENSE) [MIT]
- [LIBUV](https://github.com/JuliaLang/libuv/blob/julia-uv2-1.39.0/LICENSE) [MIT]
- [LLVM](https://releases.llvm.org/12.0.1/LICENSE.TXT) [APACHE 2.0 with LLVM Exception]
- [UTF8PROC](https://github.com/JuliaStrings/utf8proc) [MIT]

and optionally:

- [LibTracyClient](https://github.com/wolfpld/tracy/blob/master/LICENSE) [BSD-3]
- [ITTAPI](https://github.com/intel/ittapi/tree/master/LICENSES) [BSD-3 AND GPL2]

Julia's `stdlib` uses the following external libraries, which have their own licenses:

- [DSFMT](https://github.com/MersenneTwister-Lab/dSFMT/blob/master/LICENSE.txt) [BSD-3]
- [OPENLIBM](https://github.com/JuliaMath/openlibm/blob/master/LICENSE.md) [MIT, BSD-2, ISC]
- [GMP](https://gmplib.org/manual/Copying.html#Copying) [LGPL3+ or GPL2+]
- [LIBGIT2](https://github.com/libgit2/libgit2/blob/development/COPYING) [GPL2+ with unlimited linking exception]
- [CURL](https://curl.haxx.se/docs/copyright.html) [MIT/X derivative]
- [LIBSSH2](https://github.com/libssh2/libssh2/blob/master/COPYING) [BSD-3]
- [OPENSSL](https://www.openssl.org/source/license.html) [Apache 2.0]
- [MPFR](https://www.mpfr.org/mpfr-current/mpfr.html#Copying) [LGPL3+]
- [OPENBLAS](https://raw.github.com/xianyi/OpenBLAS/master/LICENSE) [BSD-3]
- [LAPACK](https://netlib.org/lapack/LICENSE.txt) [BSD-3]
- [PCRE](https://www.pcre.org/licence.txt) [BSD-3]
- [SUITESPARSE](https://github.com/DrTimothyAldenDavis/SuiteSparse/blob/master/LICENSE.txt) [mix of BSD-3-Clause, LGPL2.1+ and GPL2+; see individual module licenses]
  - [`libamd`](https://github.com/DrTimothyAldenDavis/SuiteSparse/blob/dev/AMD/Doc/License.txt) [BSD-3-Clause]
  - [`libcamd`](https://github.com/DrTimothyAldenDavis/SuiteSparse/blob/dev/CAMD/Doc/License.txt) [BSD-3-Clause]
  - [`libccolamd`](https://github.com/DrTimothyAldenDavis/SuiteSparse/blob/dev/CCOLAMD/Doc/License.txt) [BSD-3-Clause]
  - [`libcolamd`](https://github.com/DrTimothyAldenDavis/SuiteSparse/blob/dev/COLAMD/Doc/License.txt) [BSD-3-Clause]
  - [`libsuitesparseconfig`](https://github.com/DrTimothyAldenDavis/SuiteSparse/blob/dev/SuiteSparse_config/README.txt) [BSD-3-Clause]
  - [`libbtf`](https://github.com/DrTimothyAldenDavis/SuiteSparse/blob/dev/BTF/Doc/License.txt) [LGPL-2.1+]
  - [`libklu`](https://github.com/DrTimothyAldenDavis/SuiteSparse/blob/dev/KLU/Doc/License.txt) [LGPL-2.1+]
  - [`libldl`](https://github.com/DrTimothyAldenDavis/SuiteSparse/blob/dev/LDL/Doc/License.txt) [LGPL-2.1+]
  - [`libcholmod`](https://github.com/DrTimothyAldenDavis/SuiteSparse/blob/dev/CHOLMOD/Doc/License.txt) [LGPL-2.1+ and GPL-2.0+]
  - [`librbio`](https://github.com/DrTimothyAldenDavis/SuiteSparse/blob/dev/RBio/Doc/License.txt) [GPL-2.0+]
  - [`libspqr`](https://github.com/DrTimothyAldenDavis/SuiteSparse/blob/dev/SPQR/Doc/License.txt) [GPL-2.0+]
  - [`libumfpack`](https://github.com/DrTimothyAldenDavis/SuiteSparse/blob/dev/UMFPACK/Doc/License.txt) [GPL-2.0+]
- [LIBBLASTRAMPOLINE](https://github.com/staticfloat/libblastrampoline/blob/main/LICENSE) [MIT]
- [NGHTTP2](https://github.com/nghttp2/nghttp2/blob/master/COPYING) [MIT]

Julia's build process uses the following external tools:

- [PATCHELF](https://github.com/NixOS/patchelf/blob/master/COPYING) [GPL3]
- [OBJCONV](https://www.agner.org/optimize/#objconv) [GPL3]
- [LIBWHICH](https://github.com/vtjnash/libwhich/blob/master/LICENSE) [MIT]

Julia bundles the following external programs and libraries:

- [7-Zip](https://www.7-zip.org/license.txt)
- [ZLIB](https://zlib.net/zlib_license.html)
- [ZSTD](https://github.com/facebook/zstd/blob/v1.5.7/LICENSE)

On some platforms, distributions of Julia contain SSL certificate authority certificates,
released under the [Mozilla Public License](https://en.wikipedia.org/wiki/Mozilla_Public_License).
