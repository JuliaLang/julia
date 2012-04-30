/*
   A C-program for MT19937, with initialization improved 2002/2/10.
   Coded by Takuji Nishimura and Makoto Matsumoto.
   This is a faster version by taking Shawn Cokus's optimization,
   Matthe Bellew's simplification, Isaku Wada's real version.
   David Bateman added normal and exponential distributions following
   Marsaglia and Tang's Ziggurat algorithm.

   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
   Copyright (C) 2004, David Bateman
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote
        products derived from this software without specific prior written
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER
   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


   Any feedback is very welcome.
   http://www.math.keio.ac.jp/matumoto/emt.html
   email: matumoto@math.keio.ac.jp

*/

#include <math.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>

typedef int randmtzig_idx_type;
typedef signed char randmtzig_int8_t;
typedef unsigned char randmtzig_uint8_t;
typedef short randmtzig_int16_t;
typedef unsigned short randmtzig_uint16_t;
typedef int randmtzig_int32_t;
typedef unsigned int randmtzig_uint32_t;
typedef long long randmtzig_int64_t;
typedef unsigned long long randmtzig_uint64_t;

/* Declarations */

void randmtzig_create_ziggurat_tables (void);
double randmtzig_randn (void);
void randmtzig_fill_randn (double *p, randmtzig_idx_type n);
double randmtzig_exprnd (void);
void randmtzig_fill_exprnd (double *p, randmtzig_idx_type n);

/* ===== Uniform generators ===== */

inline static randmtzig_uint64_t randi53 (void)
{
    const randmtzig_uint32_t lo = dsfmt_gv_genrand_uint32();
    const randmtzig_uint32_t hi = dsfmt_gv_genrand_uint32()&0x1FFFFF;
//#ifndef __LP64__
#if 0
    randmtzig_uint64_t u;
    randmtzig_uint32_t *p = (randmtzig_uint32_t *)&u;
    p[0] = lo;
    p[1] = hi;
    return u;
#else
    return (((randmtzig_uint64_t)hi<<32)|lo);
#endif
}

inline static randmtzig_uint64_t randi54 (void)
{
  const randmtzig_uint32_t lo = dsfmt_gv_genrand_uint32();
  const randmtzig_uint32_t hi = dsfmt_gv_genrand_uint32()&0x3FFFFF;
//#ifndef __LP64__
#if 0
  randmtzig_uint64_t u;
  randmtzig_uint32_t *p = (randmtzig_uint32_t *)&u;
  p[0] = lo;
  p[1] = hi;
  return u;
#else
  return (((randmtzig_uint64_t)hi<<32)|lo);
#endif
}

/* generates a random number on (0,1) with 53-bit resolution */
inline static double randu53 (void)
{
    return dsfmt_gv_genrand_open_open();
}

/* ===== Ziggurat normal and exponential generators ===== */
# define ZIGINT randmtzig_uint64_t
# define EMANTISSA 9007199254740992.0  /* 53 bit mantissa */
# define ERANDI randi53() /* 53 bits for mantissa */
# define NMANTISSA EMANTISSA
# define NRANDI randi54() /* 53 bits for mantissa + 1 bit sign */
# define RANDU randu53()


#define ZIGGURAT_TABLE_SIZE 256

#define ZIGGURAT_NOR_R 3.6541528853610088
#define ZIGGURAT_NOR_INV_R 0.27366123732975828
#define NOR_SECTION_AREA 0.00492867323399

#define ZIGGURAT_EXP_R 7.69711747013104972
#define ZIGGURAT_EXP_INV_R 0.129918765548341586
#define EXP_SECTION_AREA 0.0039496598225815571993

/*
This code is based on the paper Marsaglia and Tsang, "The ziggurat method
for generating random variables", Journ. Statistical Software. Code was
presented in this paper for a Ziggurat of 127 levels and using a 32 bit
integer random number generator. This version of the code, uses the
Mersenne Twister as the integer generator and uses 256 levels in the
Ziggurat. This has several advantages.

  1) As Marsaglia and Tsang themselves states, the more levels the few
     times the expensive tail algorithm must be called
  2) The cycle time of the generator is determined by the integer
     generator, thus the use of a Mersenne Twister for the core random
     generator makes this cycle extremely long.
  3) The license on the original code was unclear, thus rewriting the code
     from the article means we are free of copyright issues.
  4) Compile flag for full 53-bit random mantissa.

It should be stated that the authors made my life easier, by the fact that
the algorithm developed in the text of the article is for a 256 level
ziggurat, even if the code itself isn't...

One modification to the algorithm developed in the article, is that it is
assumed that 0 <= x < Inf, and "unsigned long"s are used, thus resulting in
terms like 2^32 in the code. As the normal distribution is defined between
-Inf < x < Inf, we effectively only have 31 bit integers plus a sign. Thus
in Marsaglia and Tsang, terms like 2^32 become 2^31. We use NMANTISSA for
this term.  The exponential distribution is one sided so we use the
full 32 bits.  We use EMANTISSA for this term.

It appears that I'm slightly slower than the code in the article, this
is partially due to a better generator of random integers than they
use. But might also be that the case of rapid return was optimized by
inlining the relevant code with a #define. As the basic Mersenne
Twister is only 25% faster than this code I suspect that the main
reason is just the use of the Mersenne Twister and not the inlining,
so I'm not going to try and optimize further.
*/

static ZIGINT ki[ZIGGURAT_TABLE_SIZE];
static ZIGINT ke[ZIGGURAT_TABLE_SIZE];
static double wi[ZIGGURAT_TABLE_SIZE], fi[ZIGGURAT_TABLE_SIZE];
static double we[ZIGGURAT_TABLE_SIZE], fe[ZIGGURAT_TABLE_SIZE];

void randmtzig_create_ziggurat_tables (void)
{
  int i;
  double x, x1;

  /* Ziggurat tables for the normal distribution */
  x1 = ZIGGURAT_NOR_R;
  wi[255] = x1 / NMANTISSA;
  fi[255] = exp (-0.5 * x1 * x1);

  /* Index zero is special for tail strip, where Marsaglia and Tsang
   * defines this as
   * k_0 = 2^31 * r * f(r) / v, w_0 = 0.5^31 * v / f(r), f_0 = 1,
   * where v is the area of each strip of the ziggurat.
   */
  ki[0] = (ZIGINT) (x1 * fi[255] / NOR_SECTION_AREA * NMANTISSA);
  wi[0] = NOR_SECTION_AREA / fi[255] / NMANTISSA;
  fi[0] = 1.;

  for (i = 254; i > 0; i--)
    {
      /* New x is given by x = f^{-1}(v/x_{i+1} + f(x_{i+1})), thus
       * need inverse operator of y = exp(-0.5*x*x) -> x = sqrt(-2*ln(y))
       */
      x = sqrt(-2. * log(NOR_SECTION_AREA / x1 + fi[i+1]));
      ki[i+1] = (ZIGINT)(x / x1 * NMANTISSA);
      wi[i] = x / NMANTISSA;
      fi[i] = exp (-0.5 * x * x);
      x1 = x;
    }

  ki[1] = 0;

  /* Zigurrat tables for the exponential distribution */
  x1 = ZIGGURAT_EXP_R;
  we[255] = x1 / EMANTISSA;
  fe[255] = exp (-x1);

  /* Index zero is special for tail strip, where Marsaglia and Tsang
   * defines this as
   * k_0 = 2^32 * r * f(r) / v, w_0 = 0.5^32 * v / f(r), f_0 = 1,
   * where v is the area of each strip of the ziggurat.
   */
  ke[0] = (ZIGINT) (x1 * fe[255] / EXP_SECTION_AREA * EMANTISSA);
  we[0] = EXP_SECTION_AREA / fe[255] / EMANTISSA;
  fe[0] = 1.;

  for (i = 254; i > 0; i--)
    {
      /* New x is given by x = f^{-1}(v/x_{i+1} + f(x_{i+1})), thus
       * need inverse operator of y = exp(-x) -> x = -ln(y)
       */
      x = - log(EXP_SECTION_AREA / x1 + fe[i+1]);
      ke[i+1] = (ZIGINT)(x / x1 * EMANTISSA);
      we[i] = x / EMANTISSA;
      fe[i] = exp (-x);
      x1 = x;
    }
  ke[1] = 0;
}

/*
 * Here is the guts of the algorithm. As Marsaglia and Tsang state the
 * algorithm in their paper
 *
 * 1) Calculate a random signed integer j and let i be the index
 *     provided by the rightmost 8-bits of j
 * 2) Set x = j * w_i. If j < k_i return x
 * 3) If i = 0, then return x from the tail
 * 4) If [f(x_{i-1}) - f(x_i)] * U < f(x) - f(x_i), return x
 * 5) goto step 1
 *
 * Where f is the functional form of the distribution, which for a normal
 * distribution is exp(-0.5*x*x)
 */

double randmtzig_randn (void)
{
  while (1)
    {
//#ifdef __LP64__
#if 1
      /* arbitrary mantissa (selected by NRANDI, with 1 bit for sign) */
      const randmtzig_uint64_t r = NRANDI;
      const randmtzig_int64_t rabs=r>>1;
      const int idx = (int)(rabs&0xFF);
      const double x = ( r&1 ? -rabs : rabs) * wi[idx];
#else
      double x;
      int si,idx;
      register randmtzig_uint32_t lo, hi;
      randmtzig_int64_t rabs;
      randmtzig_uint32_t *p = (randmtzig_uint32_t *)&rabs;
      lo = dsfmt_gv_genrand_uint32();
      idx = lo&0xFF;
      hi = dsfmt_gv_genrand_uint32();
      si = hi&UMASK;
      p[0] = lo;
      p[1] = hi&0x1FFFFF;
      x = ( si ? -rabs : rabs ) * wi[idx];
# endif

      if (rabs < (randmtzig_int64_t)ki[idx])
        return x;        /* 99.3% of the time we return here 1st try */
      else if (idx == 0)
        {
          /* As stated in Marsaglia and Tsang
           *
           * For the normal tail, the method of Marsaglia[5] provides:
           * generate x = -ln(U_1)/r, y = -ln(U_2), until y+y > x*x,
           * then return r+x. Except that r+x is always in the positive
           * tail!!!! Any thing random might be used to determine the
           * sign, but as we already have r we might as well use it
           *
           * [PAK] but not the bottom 8 bits, since they are all 0 here!
           */
          double xx, yy;
          do
            {
              xx = - ZIGGURAT_NOR_INV_R * log (RANDU);
              yy = - log (RANDU);
            }
          while ( yy+yy <= xx*xx);
          return (rabs&0x100 ? -ZIGGURAT_NOR_R-xx : ZIGGURAT_NOR_R+xx);
        }
      else if ((fi[idx-1] - fi[idx]) * RANDU + fi[idx] < exp(-0.5*x*x))
        return x;
    }
}

double randmtzig_exprnd (void)
{
     while (1)
     {
	  ZIGINT ri = ERANDI;
	  const int idx = (int)(ri & 0xFF);
	  const double x = ri * we[idx];
	  if (ri < ke[idx])
	       return x;		// 98.9% of the time we return here 1st try
	  else if (idx == 0)
	  {
	       /* As stated in Marsaglia and Tsang
		* 
		* For the exponential tail, the method of Marsaglia[5] provides:
		* x = r - ln(U);
		*/
	       return ZIGGURAT_EXP_R - log(RANDU);
	  }
	  else if ((fe[idx-1] - fe[idx]) * RANDU + fe[idx] < exp(-x))
	       return x;
     }
}

void randmtzig_fill_randn (double *p, randmtzig_idx_type n)
{
     randmtzig_idx_type i;
     for (i = 0; i < n; i++)
          p[i] = randmtzig_randn();
}

void randmtzig_fill_exprnd (double *p, randmtzig_idx_type n)
{
     randmtzig_idx_type i;
     for (i = 0; i < n; i++)
          p[i] = randmtzig_exprnd();
}

