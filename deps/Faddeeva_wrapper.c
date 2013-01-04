/* Copyright (c) 2012 Massachusetts Institute of Technology
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
 */

/* C89-compatible wrappers for complex Faddeeva functions, that 
   pass and return complex values via a double* which points to
   the real and imaginary parts consecutively.  (Note that this
   is binary-compatible with both C99 complex numbers and with
   C++ std::complex<double>.) */

#include "Faddeeva.h"

#define WRAP(func)							\
void wrap ## Faddeeva_ ## func(double *func, const double *z, double relerr) \
{									\
     const double complex *zc = (const double complex *) z;		\
     double complex c = Faddeeva_ ## func(*zc, relerr);			\
     func[0] = creal(c);						\
     func[1] = cimag(c);						\
}

WRAP(w)
WRAP(erfcx)
WRAP(erf)
WRAP(erfi)
WRAP(erfc)
WRAP(Dawson)
