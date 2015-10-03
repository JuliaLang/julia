The Julia language is licensed under the MIT License. The "language" consists
of the compiler (the contents of src/), most of the standard library (base/),
and some utilities (most of the rest of the files in this repository). See below
for exceptions.

> Copyright (c) 2009-2015: Jeff Bezanson, Stefan Karpinski, Viral B. Shah,
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
- [LDC](https://github.com/ldc-developers/ldc/blob/master/LICENSE) (for ccall/cfunction ABI definitions) [BSD-3]. The portion of code that Julia uses from LDC is [BSD-3] licensed.
- [MUSL](http://git.musl-libc.org/cgit/musl/tree/COPYRIGHT) (for getopt implementations on Windows) [MIT]
- [NetBSD](http://www.netbsd.org/about/redistribution.html) (for setjmp/longjmp implementations on Windows) [BSD-3]

The Julia language links to the following external libraries, which have their
own licenses:

- [FEMTOLISP](https://github.com/JeffBezanson/femtolisp) [BSD-3]
- [LIBUNWIND](http://git.savannah.gnu.org/gitweb/?p=libunwind.git;a=blob_plain;f=LICENSE;hb=master) [MIT]
- [LIBUV](https://github.com/joyent/libuv/blob/master/LICENSE) [MIT]
- [LLVM](http://llvm.org/releases/3.3/LICENSE.TXT) [BSD-3, effectively]
- [UTF8PROC](https://github.com/JuliaLang/libmojibake) [MIT]


Julia's standard library uses the following external libraries, which have
their own licenses:

- [ARPACK](http://www.caam.rice.edu/software/ARPACK/RiceBSD.txt#LICENSE) [BSD-3]
- [ATLAS](http://math-atlas.sourceforge.net/faq.html#license) [BSD-3]
- [DSFMT](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/LICENSE.txt) [BSD-3]
- [OPENLIBM](https://github.com/JuliaLang/openlibm/blob/master/LICENSE.md) [MIT, BSD-2, ISC]
- [OPENSPECFUN](https://github.com/JuliaLang/openspecfun) [MIT, public domain]
- [FADDEEVA](http://ab-initio.mit.edu/Faddeeva) [MIT]
- [FFTW](http://fftw.org/doc/License-and-Copyright.html) [GPL2+]
- [GMP](http://gmplib.org/manual/Copying.html#Copying) [LGPL3+ or GPL2+]
- [LIBGIT2](https://github.com/libgit2/libgit2/blob/development/COPYING) [GPL2+ with unlimited linking exception]
- [MPFR](http://www.mpfr.org/mpfr-current/mpfr.html#Copying) [LGPL3+]
- [OPENBLAS](https://raw.github.com/xianyi/OpenBLAS/master/LICENSE) [BSD-3]
- [LAPACK](http://netlib.org/lapack/LICENSE.txt) [BSD-3]
- [PCRE](http://www.pcre.org/licence.txt) [BSD-3]
- [SUITESPARSE](http://faculty.cse.tamu.edu/davis/suitesparse.html) [mix of LGPL2+ and GPL2+; see individual module licenses]


The following components of Julia's standard library have separate licenses:

- base/fftw.jl (see [FFTW](http://fftw.org/doc/License-and-Copyright.html))
- base/sparse/csparse.jl (LGPL-2.1+)
- base/linalg/umfpack.jl (see [SUITESPARSE](http://faculty.cse.tamu.edu/davis/suitesparse.html))
- base/linalg/cholmod.jl (see [SUITESPARSE](http://faculty.cse.tamu.edu/davis/suitesparse.html))


Julia builds the following libraries by default, but does not use them itself:

- [RMATH](http://www.r-project.org/Licenses/)


Julia's build process uses the following external tools:

- [PATCHELF](http://hydra.nixos.org/build/1524660/download/1/README)
- [OBJCONV](http://www.agner.org/optimize/#objconv)


Julia bundles the following external programs and libraries on some platforms:

- [7-Zip](http://www.7-zip.org/license.txt)
- [BUSYBOX](https://github.com/rmyorston/busybox-w32/blob/master/LICENSE)
- [GIT](http://git-scm.com/about/free-and-open-source)
- [ZLIB](http://zlib.net/zlib_license.html)
- [LIBEXPAT](http://expat.cvs.sourceforge.net/viewvc/expat/expat/README)
- [OPENSSL](https://github.com/openssl/openssl/blob/master/LICENSE)
