/**
 * Copyright (C) 2013 JuliaLang Project / Jameson Nash
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
 *
 *
 * Inspired by blaswrap.c by Jarno Rajahalme 2011 (written for Octave)
 *
 * Wrapper for Apple libBLAS.dylib for the gfortran calling convention
 *
 * At least on the versions of OSX 10.6 so far (up and including 10.6.6)
 * these libraries are incompatible with 64 bit builds, as some functions
 * in libBLAS.dylib are not conforming to F2C calling conventions, as
 * they should.  This breaks them in 64-bit builds on the x86_64
 * architecture.
 *
 * Newer gfortran compoilers no longer default to the F2C calling
 * convention.  These wrappers map the F2C conformant functions in
 * libBLAS and libLAPACK to the native gfortran calling convention, so
 * that the libraries can be used with software built for x86_64
 * architecture.
 *
 * These wrappers are as efficient as the Apple functions of the same name.
**/

#include <Accelerate/Accelerate.h>
#include <complex.h>

float sasum_(int *N, float *SX, int *INCX) {
    return cblas_sasum(*N, SX, *INCX);
}
float scasum_(int *N, void *SX, int *INCX) {
    return cblas_scasum(*N, SX, *INCX);
}
float scnrm2_(int *N, void *X, int *INCX) {
    return cblas_scnrm2(*N, X, *INCX);
}
float sdot_(int *N, float *SX, int *INCX, float *SY, int *INCY) {
    return cblas_sdot(*N, SX, *INCX, SY, *INCY);
}
float sdsdot_(int *N, float *SB, float *SX, int *INCX, float *SY, int *INCY) {
    return cblas_sdsdot(*N, *SB, SX, *INCX, SY, *INCY);
}
float snrm2_(int *N, float *X, int *INCX) {
    return cblas_snrm2(*N, X, *INCX);
}
/*
float scabs1_(void *Z) {
    this function is part of BLAS spec, but is not provided by Apple nor required by LAPACK
}
*/

complex float cdotc_(int *N, void *CX, int *INCX, void *CY, int *INCY) {
    complex float dotc;
    cblas_cdotc_sub(*N, CX, *INCX, CY, *INCY, &dotc);
    return dotc;
}
complex float cdotu_(int *N, void *CX, int *INCX, void *CY, int *INCY) {
    complex float dotu;
    cblas_cdotu_sub(*N, CX, *INCX, CY, *INCY, &dotu);
    return dotu;
}
complex double zdotc_(int *N, void *CX, int *INCX, void *CY, int *INCY) {
    complex double dotc;
    cblas_zdotc_sub(*N, CX, *INCX, CY, *INCY, &dotc);
    return dotc;
}
complex double zdotu_(int *N, void *CX, int *INCX, void *CY, int *INCY) {
    complex double dotu;
    cblas_zdotu_sub(*N, CX, *INCX, CY, *INCY, &dotu);
    return dotu;
}

//#include <string.h>
//#include <ctype.h>
//extern void xerbl2_(const char*, const int*) __attribute__((weak));
void BLASParamErrorProcNULL(
   const char *funcName,
   const char *paramName,
   const int *paramPos,
   const int *paramValue) {
//    //cblas_xerbla(*paramPos, (char*)funcName, " param %s\n", paramName);
//    if (funcName && !strncmp(funcName, "cblas_", 6)) funcName += 6;
//    size_t i, len = strlen(funcName);
//    char funcName2[7];
//    for (i = 0; i < len; i++)
//        funcName2[i] = toupper(funcName[i]);
//    for (i = len; i < 6; i++)
//        funcName2[i] = ' ';
//    funcName2[i] = 0;
//    int paramPos2 = *paramPos-1;
//    if (xerbl2_) xerbl2_(funcName2, &paramPos2);
    /* default BLAS error handler calls abort() on invalid parameters, but we want to handle these failures in Julia */
}

__attribute__((constructor))
static void init(void) {
    SetBLASParamErrorProc(BLASParamErrorProcNULL);
}

__attribute__((destructor))
static void fini(void) {
    SetBLASParamErrorProc(NULL); /* restore default handler */
}
