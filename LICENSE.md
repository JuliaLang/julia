The Julia language is licensed under the MIT License. The "language" consists
of the compiler (the contents of src/), most of the standard library (base/),
and some utilities (most of the rest of the files in this repository). See below
for exceptions.

> Copyright (c) 2009-2014: Jeff Bezanson, Stefan Karpinski, Viral B. Shah,
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


The Julia language links to the following external libraries, which have their
own licenses:

- [FEMTOLISP](https://github.com/JeffBezanson/femtolisp)
- [LIBUNWIND](http://git.savannah.gnu.org/gitweb/?p=libunwind.git;a=blob_plain;f=LICENSE;hb=master)
- [LIBUV](https://github.com/joyent/libuv/blob/master/LICENSE)
- [LLVM](http://llvm.org/releases/3.3/LICENSE.TXT)
- [LIBMOJIBAKE](https://github.com/JuliaLang/libmojibake)


Julia's standard library uses the following external libraries, which have
their own licenses:

- [AMOS](http://www.netlib.org/slatec/guide)
- [ARPACK](http://www.caam.rice.edu/software/ARPACK/RiceBSD.txt#LICENSE)
- [ATLAS](http://math-atlas.sourceforge.net/faq.html#license)
- [DSFMT](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/LICENSE.txt)
- [OPENLIBM](https://github.com/JuliaLang/openlibm/blob/master/LICENSE.md)
- [OPENSPECFUN](https://github.com/JuliaLang/openspecfun)
- [FADDEEVA](http://ab-initio.mit.edu/Faddeeva)
- [FFTW](http://fftw.org/doc/License-and-Copyright.html)
- [GMP](http://gmplib.org/manual/Copying.html#Copying)
- [LIBGIT2](https://github.com/libgit2/libgit2/blob/development/COPYING)
- [MPFR](http://www.mpfr.org/mpfr-current/mpfr.html#Copying)
- [MUSL](http://git.musl-libc.org/cgit/musl/tree/COPYRIGHT)
- [OPENBLAS](https://raw.github.com/xianyi/OpenBLAS/master/LICENSE)
- [LAPACK](http://netlib.org/lapack/LICENSE.txt)
- [PCRE](http://www.pcre.org/licence.txt)
- [SUITESPARSE](http://faculty.cse.tamu.edu/davis/suitesparse.html)


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
