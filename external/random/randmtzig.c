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


   2007-09-22  Ivan Raikov  <iraikov@ece.gatech.edu>
   * Created single-precision versions of the array generator
     procedures.
   * Removed static state and table variables and created reentrant
     variants of each routine.

   2007-09-20  Ivan Raikov  <ivan.g.raikov@gmail.com>
   Adapted for use in Chicken Scheme.

   * 2006-04-01 David Bateman
   * * convert for use in octave, declaring static functions only used
   *   here and adding oct_ to functions visible externally
   * * inverse sense of ALLBITS
   * 2004-01-19 Paul Kienzle
   * * comment out main
   * add init_by_entropy, get_state, set_state
   * * converted to allow compiling by C++ compiler
   *
   * 2004-01-25 David Bateman
   * * Add Marsaglia and Tsang Ziggurat code
   *
   * 2004-07-13 Paul Kienzle
   * * make into an independent library with some docs.
   * * introduce new main and test code.
   *
   * 2004-07-28 Paul Kienzle & David Bateman
   * * add -DALLBITS flag for 32 vs. 53 bits of randomness in mantissa
   * * make the naming scheme more uniform
   * * add -DHAVE_X86 for faster support of 53 bit mantissa on x86 arch.
   *
   * 2005-02-23 Paul Kienzle
   * * fix -DHAVE_X86_32 flag and add -DUSE_X86_32=0|1 for explicit control
   */

/*
   === Build instructions ===

   Compile with -DHAVE_GETTIMEOFDAY if the gettimeofday function is 
   available.  This is not necessary if your architecture has
   /dev/urandom defined.

   Compile with -DALLBITS to disable 53-bit random numbers. This is about
   50% slower than using 32-bit random numbers.

   Uses implicit -Di386 or explicit -DHAVE_X86_32 to determine if CPU=x86.  
   You can force X86 behaviour with -DUSE_X86_32=1, or suppress it with 
   -DUSE_X86_32=0. You should also consider -march=i686 or similar for 
   extra performance. Check whether -DUSE_X86_32=0 is faster on 64-bit
   x86 architectures.

   If you want to replace the Mersenne Twister with another
   generator then redefine randi32 appropriately.

   === Usage instructions ===
   Before using any of the generators, initialize the state with one of
   randmtzig_init_by_int, randmtzig_init_by_array or randmtzig_init_by_entropy.

   All generators share the same state vector.

   === Mersenne Twister ===
   void randmtzig_init_by_int(uint32_t s)           32-bit initial state
   void randmtzig_init_by_array(uint32_t k[],int m) m*32-bit initial state
   void randmtzig_init_by_entropy(void)             random initial state
   void randmtzig_get_state(uint32_t save[MT_N+1])  saves state in array
   void randmtzig_set_state(uint32_t save[MT_N+1])  restores state from array
   static uint32_t randmt(void)               returns 32-bit unsigned int

   === inline generators ===
   static uint32_t randi32(void)   returns 32-bit unsigned int
   static uint64_t randi53(void)   returns 53-bit unsigned int
   static uint64_t randi54(void)   returns 54-bit unsigned int
   static uint64_t randi64(void)   returns 64-bit unsigned int
   static double randu32(void)     returns 32-bit uniform in (0,1)
   static double randu53(void)     returns 53-bit uniform in (0,1)

   double randmtzig_randu(void)       returns M-bit uniform in (0,1)
   double randmtzig_randn(void)       returns M-bit standard normal
   double randmtzig_rande(void)       returns N-bit standard exponential

   === Array generators ===
   void randmtzig_fill_randi32(randmtzig_idx_type, uint32_t [])
   void randmtzig_fill_randi64(randmtzig_idx_type, uint64_t [])
   void randmtzig_fill_randu(randmtzig_idx_type, double [])
   void randmtzig_fill_randn(randmtzig_idx_type, double [])
   void randmtzig_fill_rande(randmtzig_idx_type, double [])

*/

#include <math.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>

#include "jl_random.h"

typedef int randmtzig_idx_type;
typedef signed char randmtzig_int8_t;
typedef unsigned char randmtzig_uint8_t;
typedef short randmtzig_int16_t;
typedef unsigned short randmtzig_uint16_t;
typedef int randmtzig_int32_t;
typedef unsigned int randmtzig_uint32_t;
typedef long long randmtzig_int64_t;
typedef unsigned long long randmtzig_uint64_t;

#define PI 3.14159265358979323846

#define MT_N 624
#define ZIGGURAT_TABLE_SIZE 256
   
/* FIXME may want to suppress X86 if sizeof(long)>4 */
#if !defined(USE_X86_32)
# if defined(i386) || defined(HAVE_X86_32)
#  define USE_X86_32 1
# else
#  define USE_X86_32 0
# endif
#endif

/* ===== Mersenne Twister 32-bit generator ===== */  

#define MT_M 397
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UMASK 0x80000000UL /* most significant w-r bits */
#define LMASK 0x7fffffffUL /* least significant r bits */
#define MIXBITS(u,v) ( ((u) & UMASK) | ((v) & LMASK) )
#define TWIST(u,v) ((MIXBITS(u,v) >> 1) ^ ((v)&1UL ? MATRIX_A : 0UL))

//static uint32_t *next;

/* static uint32_t state[MT_N]; /\* the array for the state vector  *\/ */
/* unsigned int next; */
/* static int left = 1; */
/* static int initf = 0; */
/* static int initt = 1; */


#define STATE_NEXT  0 
#define STATE_LEFT  1 
#define STATE_INITF 2 
#define STATE_INITT 3
#define STATE_SEED  4

/* initializes state[MT_N] with a seed */
void 
randmtzig_init_by_int (randmtzig_uint32_t s, randmtzig_uint32_t *state)
{
    int j;
    state[STATE_SEED+0] = s & 0xffffffffUL;
    for (j = 1; j < MT_N; j++) {
        state[STATE_SEED+j] = (1812433253UL * (state[STATE_SEED+j-1] ^ (state[STATE_SEED+j-1] >> 30)) + j); 
        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
        /* In the previous versions, MSBs of the seed affect   */
        /* only MSBs of the array state[].                        */
        /* 2002/01/09 modified by Makoto Matsumoto             */
        state[STATE_SEED+j] &= 0xffffffffUL;  /* for >32 bit machines */
    }
    state[STATE_LEFT] = 1; 
    state[STATE_INITF] = 1;
    state[STATE_INITT] = 1;
}

/* initialize by an array with array-length */
/* init_key is the array for initializing keys */
/* key_length is its length */
void 
randmtzig_init_by_array (randmtzig_uint32_t init_key[], int key_length, randmtzig_uint32_t *state)
{
  int i, j, k;
  randmtzig_init_by_int (19650218UL,state);
  i = 1;
  j = 0;
  k = (MT_N > key_length ? MT_N : key_length);
  for (; k; k--)
    {
      state[STATE_SEED+i] = (state[STATE_SEED+i] ^ ((state[STATE_SEED+i-1] ^ (state[STATE_SEED+i-1] >> 30)) * 1664525UL))
	+ init_key[j] + j; /* non linear */
      state[STATE_SEED+i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
      i++;
      j++;
      if (i >= MT_N)
	{
	  state[STATE_SEED+0] = state[STATE_SEED+MT_N-1];
	  i = 1;
	}
      if (j >= key_length)
	j = 0;
    }
  for (k = MT_N - 1; k; k--)
    {
      state[STATE_SEED+i] = 
	   (state[STATE_SEED+i] ^ ((state[STATE_SEED+i-1] ^ (state[STATE_SEED+i-1] >> 30)) * 1566083941UL))
	   - i; /* non linear */
      state[STATE_SEED+i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
      i++;
      if (i >= MT_N)
	{
	  state[STATE_SEED+0] = state[STATE_SEED+MT_N-1];
	  i = 1;
	}
    }

  state[STATE_SEED+0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */
  state[STATE_LEFT] = 1; 
  state[STATE_INITF] = 1;
}

void 
randmtzig_init_by_entropy (randmtzig_uint32_t *state)
{
    randmtzig_uint32_t entropy[MT_N];
    int n = 0;

    /* Look for entropy in /dev/urandom */
    FILE* urandom =fopen("/dev/urandom", "rb");
    if (urandom) 
      {
	while (n < MT_N) 
	  {
	    unsigned char word[4];
	    if (fread(word, 4, 1, urandom) != 1) 
	      break;
	    entropy[n++] = word[0]+(word[1]<<8)+(word[2]<<16)+(word[3]<<24);
	  }
	fclose(urandom);
      }

    /* If there isn't enough entropy, gather some from various sources */
    if (n < MT_N) 
      entropy[n++] = time(NULL); /* Current time in seconds */
    if (n < MT_N) 
      entropy[n++] = clock();    /* CPU time used (usec) */
#ifdef HAVE_GETTIMEOFDAY
    if (n < MT_N) 
      {
	struct timeval tv;
	if (gettimeofday(&tv, NULL) != -1)
	  entropy[n++] = tv.tv_usec;   /* Fractional part of current time */
      }
#endif
    /* Send all the entropy into the initial state vector */
    randmtzig_init_by_array(entropy,n,state);
}


static void 
next_state (randmtzig_uint32_t *state)
{
  randmtzig_uint32_t *p = state+STATE_SEED;
  int j;

  /* if init_by_int() has not been called, */
  /* a default initial seed is used         */
  /* if (initf==0) init_by_int(5489UL); */
  /* Or better yet, a random seed! */
  if (state[STATE_INITF] != 1) 
       randmtzig_init_by_entropy(state);

  state[STATE_LEFT] = MT_N;
  state[STATE_NEXT] = 0;
    
  for (j = MT_N - MT_M + 1; --j; p++) 
    *p = p[MT_M] ^ TWIST(p[0], p[1]);

  for (j = MT_M; --j; p++) 
    *p = p[MT_M-MT_N] ^ TWIST(p[0], p[1]);

  *p = p[MT_M-MT_N] ^ TWIST(p[0], state[STATE_SEED+0]);
}

/* generates a random number on [0,0xffffffff]-interval */
static randmtzig_uint32_t randmt (randmtzig_uint32_t *state)
{
     register randmtzig_uint32_t y;
     
     (state[STATE_LEFT])--;
     if (state[STATE_LEFT] == 0)  next_state(state);
     y = state[STATE_SEED+(state[STATE_NEXT])];
     (state[STATE_NEXT])++;
     
     /* Tempering */
     y ^= (y >> 11);
     y ^= (y << 7) & 0x9d2c5680UL;
     y ^= (y << 15) & 0xefc60000UL;

     return (y ^ (y >> 18));
}

/* ===== Uniform generators ===== */

/* Select which 32 bit generator to use */
//#define randi32 randmt

static randmtzig_uint64_t randi53 (randmtzig_uint32_t *state)
{
  const randmtzig_uint32_t lo = dsfmt_gv_genrand_uint32();
  const randmtzig_uint32_t hi = dsfmt_gv_genrand_uint32()&0x1FFFFF;
#if HAVE_X86_32
  randmtzig_uint64_t u;
  randmtzig_uint32_t *p = (randmtzig_uint32_t *)&u;
  p[0] = lo;
  p[1] = hi;
  return u;
#else
  return (((randmtzig_uint64_t)hi<<32)|lo);
#endif
}

static randmtzig_uint64_t randi54 (randmtzig_uint32_t *state)
{
  const randmtzig_uint32_t lo = dsfmt_gv_genrand_uint32();
  const randmtzig_uint32_t hi = dsfmt_gv_genrand_uint32()&0x3FFFFF;
#if HAVE_X86_32
  randmtzig_uint64_t u;
  randmtzig_uint32_t *p = (randmtzig_uint32_t *)&u;
  p[0] = lo;
  p[1] = hi;
  return u;
#else
  return (((randmtzig_uint64_t)hi<<32)|lo);
#endif
}

static randmtzig_uint64_t randi64 (randmtzig_uint32_t *state)
{
  const randmtzig_uint32_t lo = dsfmt_gv_genrand_uint32();
  const randmtzig_uint32_t hi = dsfmt_gv_genrand_uint32();
#if HAVE_X86_32
  randmtzig_uint64_t u;
  randmtzig_uint32_t *p = (randmtzig_uint32_t *)&u;
  p[0] = lo;
  p[1] = hi;
  return u;
#else
  return (((randmtzig_uint64_t)hi<<32)|lo);
#endif
}

/* generates a random number on (0,1)-real-interval */
static double randu32 (randmtzig_uint32_t *state)
{
  return ((double)dsfmt_gv_genrand_uint32() + 0.5) * (1.0/4294967296.0); 
  /* divided by 2^32 */
}

/* generates a random number on (0,1) with 53-bit resolution */
static double randu53 (randmtzig_uint32_t *state) 
{ 
  const randmtzig_uint32_t a=dsfmt_gv_genrand_uint32()>>5;
  const randmtzig_uint32_t b=dsfmt_gv_genrand_uint32()>>6; 
  return(a*67108864.0+b+0.4) * (1.0/9007199254740992.0);
} 

/* Determine mantissa for uniform doubles */
double randmtzig_randu (randmtzig_uint32_t *state)
{
  return randu53(state);
}

randmtzig_uint32_t randmtzig_randi32 (randmtzig_uint32_t *state)
{
     return dsfmt_gv_genrand_uint32();
}

/* ===== Ziggurat normal and exponential generators ===== */
# define ZIGINT randmtzig_uint64_t
# define EMANTISSA 9007199254740992.0  /* 53 bit mantissa */
# define ERANDI randi53(state) /* 53 bits for mantissa */
# define NMANTISSA EMANTISSA  
# define NRANDI randi54(state) /* 53 bits for mantissa + 1 bit sign */
# define RANDU randu53(state)


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


/* static ZIGINT ki[ZIGGURAT_TABLE_SIZE]; */
/* static ZIGINT ke[ZIGGURAT_TABLE_SIZE]; */
/* static double wi[ZIGGURAT_TABLE_SIZE], fi[ZIGGURAT_TABLE_SIZE]; */
/* static double we[ZIGGURAT_TABLE_SIZE], fe[ZIGGURAT_TABLE_SIZE]; */


static void 
create_ziggurat_tables (randmtzig_uint32_t *state, ZIGINT *ki, ZIGINT *ke, double *wi, double *fi,
			double *we, double *fe)
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

  state[STATE_INITT] = 0;
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

double
randmtzig_randn (randmtzig_uint32_t *state, ZIGINT *ki, ZIGINT *ke, double *wi, double *fi,
		 double *we, double *fe)
{
  if (state[STATE_INITT] == 1) 
       create_ziggurat_tables(state, ki, ke, wi, fi, we, fe);

  while (1)
    {
      /* The following code is specialized for 32-bit mantissa.
       * Compared to the arbitrary mantissa code, there is a performance
       * gain for 32-bits:  PPC: 2%, MIPS: 8%, x86: 40%
       * There is a bigger performance gain compared to using a full
       * 53-bit mantissa:  PPC: 60%, MIPS: 65%, x86: 240%
       * Of course, different compilers and operating systems may
       * have something to do with this.
       */
#if !defined(ALLBITS)
# if HAVE_X86_32
      /* 53-bit mantissa, 1-bit sign, x86 32-bit architecture */
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
# else /* !HAVE_X86_32 */
      /* arbitrary mantissa (selected by NRANDI, with 1 bit for sign) */
      const randmtzig_uint64_t r = NRANDI;
      const randmtzig_int64_t rabs=r>>1;
      const int idx = (int)(rabs&0xFF);
      const double x = ( r&1 ? -rabs : rabs) * wi[idx];
# endif /* !HAVE_X86_32 */
      if (rabs < (randmtzig_int64_t)ki[idx])
#else /* ALLBITS */
      /* 32-bit mantissa */
      const randmtzig_uint32_t r = dsfmt_gv_genrand_uint32();
      const randmtzig_uint32_t rabs = r&LMASK;
      const int idx = (int)(r&0xFF);
      const double x = ((randmtzig_int32_t)r) * wi[idx];
      if (rabs < ki[idx])
#endif /* ALLBITS */
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

double
randmtzig_rande (randmtzig_uint32_t *state, ZIGINT *ki, ZIGINT *ke, double *wi, double *fi,
		 double *we, double *fe)
{
     if (state[STATE_INITT] == 1) 
	  create_ziggurat_tables(state,ki,ke,wi,fi,we,fe);
     
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


/* ---------------------------------------------------------------- */
/* gammln - compute natural log of gamma function of xx */
static double gammln(double xx)
{
    double x,tmp,ser;
    static double cof[6]={76.18009173,-86.50532033,24.01409822,
			  -1.231739516,0.120858003e-2,-0.536382e-5};
    int j;
    x=xx-1.0;
    tmp=x+5.5;
    tmp -= (x+0.5)*log(tmp);
    ser=1.0;
    for (j=0;j<=5;j++) 
    {
	 x += 1.0;
	 ser += cof[j]/x;
    }
    return -tmp+log(2.50662827465*ser);
}


/* 
   Choose a random number from a binomial distribution. 
   Based on the bnldev function from Schaefer, Larkum, Sakmann, Roth
   "Coincidence detection in pyramidal neurons is tuned by their
   dendritic branching pattern" J. Neurophys. 

   Miki London & Peter N. Steinmetz, Kamran Diba
*/
double randmtzig_randb(int nnr, double ppr, randmtzig_uint32_t *state) 
{
     int j;
     static int nold=(-1);
     double am,em,g,angle,p,bnl,sq,bt,y;
     static double pold=(-1.0),pc,plog,pclog,en,oldg;
    
     /* prepare to always ignore errors within this routine */
     p=(ppr <= 0.5 ? ppr : 1.0-ppr);
     am=nnr*p;
     if (nnr < 25) 
     {
	  bnl=0.0;
	  for (j=1;j<=nnr;j++)
	       if (randmtzig_randu(state) < p) bnl += 1.0;
     }
     else if (am < 1.0) 
     {
	  g=exp(-am);
	  bt=1.0;
	  for (j=0;j<=nnr;j++) 
	  {
	       bt *= randmtzig_randu (state);
            if (bt < g) break;
	  }
	  bnl=(j <= nnr ? j : nnr);
     }
     else 
     {
	  if (nnr != nold) 
	  {
	       en=nnr;
	       oldg=gammln(en+1.0);
	       nold=nnr;
	  }
	  if (p != pold) 
	  {
	       pc=1.0-p;
	       plog=log(p);
	       pclog=log(pc);
	       pold=p;
	  }
	  sq=sqrt(2.0*am*pc);
	  do {
	       do {
		    angle=PI*randmtzig_randu (state);
                    angle=PI*randmtzig_randu (state);
		    y=tan(angle);
		    em=sq*y+am;
	       } while (em < 0.0 || em >= (en+1.0));
	       em=floor(em);
	       bt=1.2*sq*(1.0+y*y)*exp(oldg-gammln(em+1.0) - 
				       gammln(en-em+1.0)+em*plog+(en-em)*pclog);
	  } while (randmtzig_randu (state) > bt);
	  bnl=em;
     }

     if (p != ppr) bnl=nnr-bnl;
    
    return bnl;
}


#define RUNI(s) randmtzig_randu(s)
#define RNOR(s,ki,ke,wi,fi,we,fe) randmtzig_randn(s,ki,ke,wi,fi,we,fe)
#define LGAMMA lgamma

/* ---- pprsc.c from Stadloeber's winrand --- */

/* flogfak(k) = ln(k!) */
static double 
flogfak (double k)
{
#define       C0      9.18938533204672742e-01
#define       C1      8.33333333333333333e-02
#define       C3     -2.77777777777777778e-03
#define       C5      7.93650793650793651e-04
#define       C7     -5.95238095238095238e-04

  static double logfak[30L] = {
    0.00000000000000000,   0.00000000000000000,   0.69314718055994531,
    1.79175946922805500,   3.17805383034794562,   4.78749174278204599,
    6.57925121201010100,   8.52516136106541430,  10.60460290274525023,
    12.80182748008146961,  15.10441257307551530,  17.50230784587388584,
    19.98721449566188615,  22.55216385312342289,  25.19122118273868150,
    27.89927138384089157,  30.67186010608067280,  33.50507345013688888,
    36.39544520803305358,  39.33988418719949404,  42.33561646075348503,
    45.38013889847690803,  48.47118135183522388,  51.60667556776437357,
    54.78472939811231919,  58.00360522298051994,  61.26170176100200198,
    64.55753862700633106,  67.88974313718153498,  71.25703896716800901
  };
  
  double  r, rr;
  
  if (k >= 30.0) 
    {
      r  = 1.0 / k;
      rr = r * r;
      return ((k + 0.5)*log(k) - k + C0 + r*(C1 + rr*(C3 + rr*(C5 + rr*C7))));
    }
  else
    return (logfak[(int)k]);
}


/******************************************************************
 *                                                                *
 * Poisson Distribution - Patchwork Rejection/Inversion  *
 *                                                                *
 ******************************************************************
 *                                                                *
 * For parameter  my < 10  Tabulated Inversion is applied.        *
 * For my >= 10  Patchwork Rejection is employed:                 *
 * The area below the histogram function f(x) is rearranged in    *
 * its body by certain point reflections. Within a large center   *
 * interval variates are sampled efficiently by rejection from    *
 * uniform hats. Rectangular immediate acceptance regions speed   *
 * up the generation. The remaining tails are covered by          *
 * exponential functions.                                         *
 *                                                                *
 ******************************************************************
 *                                                                *
 * FUNCTION :   - pprsc samples a random number from the Poisson  *
 *                distribution with parameter my > 0.             *
 * REFERENCE :  - H. Zechner (1994): Efficient sampling from      *
 *                continuous and discrete unimodal distributions, *
 *                Doctoral Dissertation, 156 pp., Technical       *
 *                University Graz, Austria.                       *
 * SUBPROGRAM : - drand(seed) ... (0,1)-Uniform generator with    *
 *                unsigned long integer *seed.                    *
 *                                                                *
 * Implemented by H. Zechner, January 1994                        *
 * Revised by F. Niederl, July 1994                               *
 *                                                                *
 ******************************************************************/

static double 
f (double k, double l_nu, double c_pm)
{
  return exp(k * l_nu - flogfak(k) - c_pm);
}

static double 
pprsc (double my, randmtzig_uint32_t *state)
{
  static double        my_last = -1.0;
  static double        m,  k2, k4, k1, k5;
  static double        dl, dr, r1, r2, r4, r5, ll, lr, l_my, c_pm,
    f1, f2, f4, f5, p1, p2, p3, p4, p5, p6;
  double               Dk, X, Y;
  double               Ds, U, V, W;
  
  if (my != my_last)
    {                               /* set-up           */
      my_last = my;
      /* approximate deviation of reflection points k2, k4 from my - 1/2 */
      Ds = sqrt(my + 0.25);
      
      /* mode m, reflection points k2 and k4, and points k1 and k5,      */
      /* which delimit the centre region of h(x)                         */
      m  = floor(my);
      k2 = ceil(my - 0.5 - Ds);
      k4 = floor(my - 0.5 + Ds);
      k1 = k2 + k2 - m + 1L;
      k5 = k4 + k4 - m;
      
      /* range width of the critical left and right centre region        */
      dl = (k2 - k1);
      dr = (k5 - k4);
      
      /* recurrence constants r(k)=p(k)/p(k-1) at k = k1, k2, k4+1, k5+1 */
      r1 = my / k1;
      r2 = my / k2;
      r4 = my / (k4 + 1.0);
      r5 = my / (k5 + 1.0);

      /* reciprocal values of the scale parameters of exp. tail envelope */
      ll =  log(r1);                                 /* expon. tail left */
      lr = -log(r5);                                 /* expon. tail right*/
      
      /* Poisson constants, necessary for computing function values f(k) */
      l_my = log(my);
      c_pm = m * l_my - flogfak(m);
      
      /* function values f(k) = p(k)/p(m) at k = k2, k4, k1, k5          */
      f2 = f(k2, l_my, c_pm);
      f4 = f(k4, l_my, c_pm);
      f1 = f(k1, l_my, c_pm);
      f5 = f(k5, l_my, c_pm);
      
      /* area of the two centre and the two exponential tail regions     */
      /* area of the two immediate acceptance regions between k2, k4     */
      p1 = f2 * (dl + 1.0);                            /* immed. left    */
      p2 = f2 * dl         + p1;                       /* centre left    */
      p3 = f4 * (dr + 1.0) + p2;                       /* immed. right   */
      p4 = f4 * dr         + p3;                       /* centre right   */
      p5 = f1 / ll         + p4;                       /* exp. tail left */
      p6 = f5 / lr         + p5;                       /* exp. tail right*/
    }
  
  for (;;)
    {
      /* generate uniform number U -- U(0, p6)                           */
      /* case distinction corresponding to U                             */
	 if ((U = RUNI(state) * p6) < p2)
	{                                            /* centre left      */
	  
	  /* immediate acceptance region 
	     R2 = [k2, m) *[0, f2),  X = k2, ... m -1 */
	  if ((V = U - p1) < 0.0)  return(k2 + floor(U/f2));
	  /* immediate acceptance region 
	     R1 = [k1, k2)*[0, f1),  X = k1, ... k2-1 */
	  if ((W = V / dl) < f1 )  return(k1 + floor(V/f1));
	  
	  /* computation of candidate X < k2, and its counterpart Y > k2 */
	  /* either squeeze-acceptance of X or acceptance-rejection of Y */
	  Dk = floor(dl * RUNI(state)) + 1.0;
	  if (W <= f2 - Dk * (f2 - f2/r2))
	    {                                        /* quick accept of  */
	      return(k2 - Dk);                       /* X = k2 - Dk      */
	    }
	  if ((V = f2 + f2 - W) < 1.0)
	    {                                        /* quick reject of Y*/
	      Y = k2 + Dk;
	      if (V <= f2 + Dk * (1.0 - f2)/(dl + 1.0))
		{                                    /* quick accept of  */
		  return(Y);                         /* Y = k2 + Dk      */
		}
	      if (V <= f(Y, l_my, c_pm))  return(Y); /* final accept of Y*/
	    }
	  X = k2 - Dk;
	}
      else if (U < p4)
	{                                            /* centre right     */
	  /*  immediate acceptance region 
	      R3 = [m, k4+1)*[0, f4), X = m, ... k4    */
	  if ((V = U - p3) < 0.0)  return(k4 - floor((U - p2)/f4));
	  /* immediate acceptance region 
	     R4 = [k4+1, k5+1)*[0, f5)                */
	  if ((W = V / dr) < f5 )  return(k5 - floor(V/f5));
	  
	  /* computation of candidate X > k4, and its counterpart Y < k4 */
	  /* either squeeze-acceptance of X or acceptance-rejection of Y */
	  Dk = floor(dr * RUNI(state)) + 1.0;
	  if (W <= f4 - Dk * (f4 - f4*r4))
	    {                                        /* quick accept of  */
	      return(k4 + Dk);                       /* X = k4 + Dk      */
	    }
	  if ((V = f4 + f4 - W) < 1.0)
	    {                                        /* quick reject of Y*/
	      Y = k4 - Dk;
	      if (V <= f4 + Dk * (1.0 - f4)/ dr)
		{                                    /* quick accept of  */
		  return(Y);                         /* Y = k4 - Dk      */
		}
	      if (V <= f(Y, l_my, c_pm))  return(Y); /* final accept of Y*/
	    }
	  X = k4 + Dk;
	}
      else
	{
	     W = RUNI(state);
	  if (U < p5)
	    {                                        /* expon. tail left */
	      Dk = floor(1.0 - log(W)/ll);
	      if ((X = k1 - Dk) < 0L)  continue;     /* 0 <= X <= k1 - 1 */
	      W *= (U - p4) * ll;                    /* W -- U(0, h(x))  */
	      if (W <= f1 - Dk * (f1 - f1/r1))  
		return(X);                           /* quick accept of X*/
	    }
	  else
	    {                                        /* expon. tail right*/
	      Dk = floor(1.0 - log(W)/lr);
	      X  = k5 + Dk;                          /* X >= k5 + 1      */
	      W *= (U - p5) * lr;                    /* W -- U(0, h(x))  */
	      if (W <= f5 - Dk * (f5 - f5*r5))  
		return(X);                           /* quick accept of X*/
	    }
	}
      
      /* acceptance-rejection test of candidate X from the original area */
      /* test, whether  W <= f(k),    with  W = U*h(x)  and  U -- U(0, 1)*/
      /* log f(X) = (X - m)*log(my) - log X! + log m!                    */
      if (log(W) <= X * l_my - flogfak(X) - c_pm)  return(X);
    }
}
/* ---- pprsc.c end ------ */


/* The remainder of the file is by Paul Kienzle */

/* Given uniform u, find x such that CDF(L,x)==u.  Return x. */
static void 
poisson_cdf_lookup(double lambda, double *p, size_t n, randmtzig_uint32_t *state)
{
  /* Table size is predicated on the maximum value of lambda
   * we want to store in the table, and the maximum value of
   * returned by the uniform random number generator on [0,1).
   * With lambda==10 and u_max = 1 - 1/(2^32+1), we
   * have poisson_pdf(lambda,36) < 1-u_max.  If instead our
   * generator uses more bits of mantissa or returns a value
   * in the range [0,1], then for lambda==10 we need a table 
   * size of 46 instead.  For long doubles, the table size 
   * will need to be longer still.  */
#define TABLESIZE 46
  double t[TABLESIZE];
  
  /* Precompute the table for the u up to and including 0.458.
   * We will almost certainly need it. */
  int intlambda = (int)floor(lambda);
  double P;
  int tableidx;
  size_t i = n;
  
  t[0] = P = exp(-lambda);
  for (tableidx = 1; tableidx <= intlambda; tableidx++) {
    P = P*lambda/(double)tableidx;
    t[tableidx] = t[tableidx-1] + P;
  }

  while (i-- > 0) {
       double u = RUNI(state);
    
    /* If u > 0.458 we know we can jump to floor(lambda) before
     * comparing (this observation is based on Stadlober's winrand
     * code). For lambda >= 1, this will be a win.  Lambda < 1
     * is already fast, so adding an extra comparison is not a
     * problem. */
    int k = (u > 0.458 ? intlambda : 0);

    /* We aren't using a for loop here because when we find the
     * right k we want to jump to the next iteration of the
     * outer loop, and the continue statement will only work for 
     * the inner loop. */
  nextk:
    if ( u <= t[k] ) {
      p[i] = (double) k;
      continue;
    }
    if (++k < tableidx) goto nextk;
    
    /* We only need high values of the table very rarely so we 
     * don't automatically compute the entire table. */
    while (tableidx < TABLESIZE) {
      P = P*lambda/(double)tableidx;
      t[tableidx] = t[tableidx-1] + P;
      /* Make sure we converge to 1.0 just in case u is uniform
       * on [0,1] rather than [0,1). */
      if (t[tableidx] == t[tableidx-1]) t[tableidx] = 1.0;
      tableidx++;
      if (u <= t[tableidx-1]) break;
    }
    
    /* We are assuming that the table size is big enough here.
     * This should be true even if RUNI is returning values in
     * the range [0,1] rather than [0,1).
     */
    p[i] = (double)(tableidx-1);
  }
}

/* From Press, et al., Numerical Recipes */
static void
poisson_rejection (double lambda, double *p, size_t n, randmtzig_uint32_t *state)
{
  double sq = sqrt(2.0*lambda);
  double alxm = log(lambda);
  double g = lambda*alxm - LGAMMA(lambda+1.0);
  size_t i;
  
  for (i = 0; i < n; i++) 
    {
      double y, em, t;
      do {
	do {
	     y = tan(M_PI*RUNI(state));
	  em = sq * y + lambda;
	} while (em < 0.0);
	em = floor(em);
	t = 0.9*(1.0+y*y)*exp(em*alxm-flogfak(em)-g);
      } while (RUNI(state) > t);
      p[i] = em;
    }
}


/* Generate one poisson variate */
double randmtzig_randp (double L, randmtzig_uint32_t *state, 
			ZIGINT *ki, ZIGINT *ke, double *wi, double *fi, double *we, double *fe)
{
  double ret;
  if (L < 0.0) ret = NAN;
  else if (L <= 12.0) {
    /* From Press, et al. Numerical recipes */
    double g = exp(-L);
    int em = -1;
    double t = 1.0;
    do {
      ++em;
      t *= RUNI(state);
    } while (t > g);
    ret = em;
  } else if (L <= 1e8) {
    /* numerical recipes */
       poisson_rejection(L, &ret, 1,state);
  } else if (isinf(L)) {
    /* FIXME R uses NaN, but the normal approx. suggests that as
     * limit should be inf. Which is correct? */
    ret = NAN;
  } else {
    /* normal approximation: from Phys. Rev. D (1994) v50 p1284 */
       ret = floor(RNOR(state,ki,ke,wi,fi,we,fe)*sqrt(L) + L + 0.5);
    if (ret < 0.0) ret = 0.0; /* will probably never happen */
  }
  return ret;
}




/* Array generators */
void 
randmtzig_fill_drandu (randmtzig_idx_type n, double *p, randmtzig_uint32_t *state)
{
     randmtzig_idx_type i;
     for (i = 0; i < n; i++) 
	  p[i] = randmtzig_randu(state);
}

void 
randmtzig_fill_drandn (randmtzig_idx_type n, double *p, randmtzig_uint32_t *state,
		       ZIGINT *ki, ZIGINT *ke, double *wi, double *fi,
		       double *we, double *fe)
{
     randmtzig_idx_type i;
     for (i = 0; i < n; i++) 
	  p[i] = randmtzig_randn(state,ki,ke,wi,fi,we,fe);
}

void 
randmtzig_fill_drande (randmtzig_idx_type n, double *p, randmtzig_uint32_t *state,
		       ZIGINT *ki, ZIGINT *ke, double *wi, double *fi,
		       double *we, double *fe)
		       
{
     randmtzig_idx_type i;
     for (i = 0; i < n; i++) 
	  p[i] = randmtzig_rande(state,ki,ke,wi,fi,we,fe);
}

void 
randmtzig_fill_drandb (int nnr, double ppr, 
		       randmtzig_idx_type n, double *v, randmtzig_uint32_t *state)
{
     randmtzig_idx_type i;
     for (i = 0; i < n; i++) 
	  v[i] = randmtzig_randb(nnr, ppr, state);
}


void 
randmtzig_fill_srandu (randmtzig_idx_type n, float *p, randmtzig_uint32_t *state)
{
     randmtzig_idx_type i;
     for (i = 0; i < n; i++)  
	  p[i] = randmtzig_randu(state);
}

void 
randmtzig_fill_srandn (randmtzig_idx_type n, float *p, randmtzig_uint32_t *state,
		       ZIGINT *ki, ZIGINT *ke, double *wi, double *fi,
		       double *we, double *fe)
{
     randmtzig_idx_type i;
     for (i = 0; i < n; i++) 
	  p[i] = randmtzig_randn(state,ki,ke,wi,fi,we,fe);
}

void 
randmtzig_fill_srande (randmtzig_idx_type n, float *p, randmtzig_uint32_t *state,
		       ZIGINT *ki, ZIGINT *ke, double *wi, double *fi,
		       double *we, double *fe)
{
  randmtzig_idx_type i;
  for (i = 0; i < n; i++) 
    p[i] = randmtzig_rande(state,ki,ke,wi,fi,we,fe);
}


void 
randmtzig_fill_srandb (int nnr, float ppr, 
		       randmtzig_idx_type n, float *v, randmtzig_uint32_t *state)
{
     randmtzig_idx_type i;
     for (i = 0; i < n; i++) 
	  v[i] = randmtzig_randb(nnr, ppr, state);
}

/* The cutoff of L <= 1e8 in the following two functions before using 
 * the normal approximation is based on:
 *   > L=1e8; x=floor(linspace(0,2*L,1000));
 *   > max(abs(normal_pdf(x,L,L)-poisson_pdf(x,L)))
 *   ans =  1.1376e-28
 * For L=1e7, the max is around 1e-9, which is within the step size of RUNI.
 * For L>1e10 the pprsc function breaks down, as I saw from the histogram
 * of a large sample, so 1e8 is both small enough and large enough. */

/* Generate a set of poisson numbers with the same distribution */
void 
randmtzig_fill_drandp (double L, randmtzig_idx_type n, double *p, randmtzig_uint32_t *state, 
		       ZIGINT *ki, ZIGINT *ke, double *wi, double *fi, double *we, double *fe)
{
  randmtzig_idx_type i;
  if (L < 0.0 || isinf(L)) 
    {
      for (i=0; i<n; i++) 
	p[i] = NAN;
    } 
  else if (L <= 10.0) 
    {
	 poisson_cdf_lookup(L, p, n, state);
    } 
  else if (L <= 1e8) 
    {
      for (i=0; i<n; i++) 
	   p[i] = pprsc(L,state);
    } 
  else 
    {
      /* normal approximation: from Phys. Rev. D (1994) v50 p1284 */
      const double sqrtL = sqrt(L);
      for (i = 0; i < n; i++) 
	{
	     p[i] = floor(RNOR(state,ki,ke,wi,fi,we,fe)*sqrtL + L + 0.5);
	  if (p[i] < 0.0) 
	    p[i] = 0.0; /* will probably never happen */
	}
    }
}



/* Given uniform u, find x such that CDF(L,x)==u.  Return x. */
static void 
poisson_cdf_slookup(double lambda, float *p, size_t n, randmtzig_uint32_t *state)
{
  /* Table size is predicated on the maximum value of lambda
   * we want to store in the table, and the maximum value of
   * returned by the uniform random number generator on [0,1).
   * With lambda==10 and u_max = 1 - 1/(2^32+1), we
   * have poisson_pdf(lambda,36) < 1-u_max.  If instead our
   * generator uses more bits of mantissa or returns a value
   * in the range [0,1], then for lambda==10 we need a table 
   * size of 46 instead.  For long doubles, the table size 
   * will need to be longer still.  */
#define TABLESIZE 46
  float t[TABLESIZE];
  
  /* Precompute the table for the u up to and including 0.458.
   * We will almost certainly need it. */
  int intlambda = (int)floor(lambda);
  double P;
  int tableidx;
  size_t i = n;
  
  t[0] = P = exp(-lambda);
  for (tableidx = 1; tableidx <= intlambda; tableidx++) {
    P = P*lambda/(float)tableidx;
    t[tableidx] = t[tableidx-1] + P;
  }

  while (i-- > 0) {
       float u = RUNI(state);
    
    /* If u > 0.458 we know we can jump to floor(lambda) before
     * comparing (this observation is based on Stadlober's winrand
     * code). For lambda >= 1, this will be a win.  Lambda < 1
     * is already fast, so adding an extra comparison is not a
     * problem. */
    int k = (u > 0.458 ? intlambda : 0);

    /* We aren't using a for loop here because when we find the
     * right k we want to jump to the next iteration of the
     * outer loop, and the continue statement will only work for 
     * the inner loop. */
  nextk:
    if ( u <= t[k] ) {
      p[i] = (float) k;
      continue;
    }
    if (++k < tableidx) goto nextk;
    
    /* We only need high values of the table very rarely so we 
     * don't automatically compute the entire table. */
    while (tableidx < TABLESIZE) {
      P = P*lambda/(float)tableidx;
      t[tableidx] = t[tableidx-1] + P;
      /* Make sure we converge to 1.0 just in case u is uniform
       * on [0,1] rather than [0,1). */
      if (t[tableidx] == t[tableidx-1]) t[tableidx] = 1.0;
      tableidx++;
      if (u <= t[tableidx-1]) break;
    }
    
    /* We are assuming that the table size is big enough here.
     * This should be true even if RUNI is returning values in
     * the range [0,1] rather than [0,1).
     */
    p[i] = (float)(tableidx-1);
  }
}


void 
randmtzig_fill_srandp (double L, randmtzig_idx_type n, float *p, randmtzig_uint32_t *state,
                       ZIGINT *ki, ZIGINT *ke, double *wi, double *fi, double *we, double *fe)
{
  randmtzig_idx_type i;
  if (L < 0.0 || isinf(L)) 
    {
      for (i=0; i<n; i++) 
	p[i] = NAN;
    } 
  else if (L <= 10.0) 
    {
	 poisson_cdf_slookup(L, p, n, state);
    } 
  else if (L <= 1e8) 
    {
      for (i=0; i<n; i++) 
	   p[i] = pprsc(L,state);
    } 
  else 
    {
      /* normal approximation: from Phys. Rev. D (1994) v50 p1284 */
      const float sqrtL = sqrt(L);
      for (i = 0; i < n; i++) 
	{
	     p[i] = floor(RNOR(state,ki,ke,wi,fi,we,fe)*sqrtL + L + 0.5);
	  if (p[i] < 0.0) 
	    p[i] = 0.0; /* will probably never happen */
	}
    }
}

