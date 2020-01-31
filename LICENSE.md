The Julia language is licensed under the MIT License. The "language" consists
of the compiler (the contents of src/), most of the standard library (base/),
and some utilities (most of the rest of the files in this repository). See below
for exceptions.

> Copyright (c) 2009-2019: Jeff Bezanson, Stefan Karpinski, Viral B. Shah,
> and other contributors:
>
> https://github.com/JuliaLang/julia/contributors
>
> Permission is hereby granted, free of charge, to any person obtaining
> a copy of this software and associated documentation files (the
> "Software"), to deal in the Software without restriction, including
> without limitation the rights to use, copy, modify, merge, publish,
> distribute, sublicense, and/or sell copies of the Software, and to
> permit persons to whom the Software is furnished to do so, subject to
> the following conditions:
>
> The above copyright notice and this permission notice shall be
> included in all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
> EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
> MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
> NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
> LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
> OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
> WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Julia includes code from the following projects, which have their own licenses:

- [crc32c.c](https://stackoverflow.com/questions/17645167/implementing-sse-4-2s-crc32c-in-software) (CRC-32c checksum code by Mark Adler) [[ZLib](https://opensource.org/licenses/Zlib)].
- [LDC](https://github.com/ldc-developers/ldc/blob/master/LICENSE) (for ccall/cfunction ABI definitions) [BSD-3]. The portion of code that Julia uses from LDC is [BSD-3] licensed.
- [LLVM](https://releases.llvm.org/3.9.0/LICENSE.TXT) (for parts of src/jitlayers.cpp and src/disasm.cpp) [BSD-3, effectively]
- [MUSL](https://git.musl-libc.org/cgit/musl/tree/COPYRIGHT) (for getopt implementation on Windows) [MIT]
- [MINGW](https://sourceforge.net/p/mingw/mingw-org-wsl/ci/legacy/tree/mingwrt/mingwex/dirname.c) (for dirname implementation on Windows) [MIT]
- [NetBSD](https://www.netbsd.org/about/redistribution.html) (for setjmp, longjmp, and strptime implementations on Windows) [BSD-3]
- [Python](https://docs.python.org/2/license.html) (for strtod and joinpath implementation on Windows) [BSD-3, effectively]
- [Google Benchmark](https://github.com/google/benchmark) (for cyclecount implementation) [Apache 2.0]

The following components included in Julia `Base` have their own separate licenses:

- base/ryu/* [Boost] (see [ryu](https://github.com/ulfjack/ryu/blob/master/LICENSE-Boost))
- base/grisu/* [BSD-3] (see [double-conversion](https://github.com/google/double-conversion/blob/master/LICENSE))
- base/special/{exp,rem_pio2,hyperbolic}.jl [Freely distributable with preserved copyright notice] (see [FDLIBM](https://www.netlib.org/fdlibm))

The Julia language links to the following external libraries, which have their
own licenses:

- [FEMTOLISP](https://github.com/JeffBezanson/femtolisp) [BSD-3]
- [LIBUNWIND](https://git.savannah.gnu.org/gitweb/?p=libunwind.git;a=blob_plain;f=LICENSE;hb=master) [MIT]
- [LIBUV](https://github.com/joyent/libuv/blob/master/LICENSE) [MIT]
- [LLVM](https://releases.llvm.org/6.0.0/LICENSE.TXT) [BSD-3, effectively]
- [UTF8PROC](https://github.com/JuliaStrings/utf8proc) [MIT]

Julia's `stdlib` uses the following external libraries, which have their own licenses:

- [DSFMT](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/LICENSE.txt) [BSD-3]
- [OPENLIBM](https://github.com/JuliaMath/openlibm/blob/master/LICENSE.md) [MIT, BSD-2, ISC]
- [GMP](https://gmplib.org/manual/Copying.html#Copying) [LGPL3+ or GPL2+]
- [LIBGIT2](https://github.com/libgit2/libgit2/blob/development/COPYING) [GPL2+ with unlimited linking exception]
- [CURL](https://curl.haxx.se/docs/copyright.html) [MIT/X derivative]
- [LIBSSH2](https://github.com/libssh2/libssh2/blob/master/COPYING) [BSD-3]
- [MBEDTLS](https://tls.mbed.org/how-to-get) [either GPLv2 or Apache 2.0]
- [MPFR](https://www.mpfr.org/mpfr-current/mpfr.html#Copying) [LGPL3+]
- [OPENBLAS](https://raw.github.com/xianyi/OpenBLAS/master/LICENSE) [BSD-3]
- [LAPACK](https://netlib.org/lapack/LICENSE.txt) [BSD-3]
- [PCRE](https://www.pcre.org/licence.txt) [BSD-3]
- [SUITESPARSE](http://suitesparse.com) [mix of LGPL2+ and GPL2+; see individual module licenses]

Julia's build process uses the following external tools:

- [PATCHELF](https://nixos.org/patchelf.html)
- [OBJCONV](https://www.agner.org/optimize/#objconv)

Julia bundles the following external programs and libraries:

- [7-Zip](https://www.7-zip.org/license.txt)
- [ZLIB](https://zlib.net/zlib_license.html)

On some platforms, distributions of Julia contain SSL certificate authority certificates,
released under the [Mozilla Public License](https://en.wikipedia.org/wiki/Mozilla_Public_License).
