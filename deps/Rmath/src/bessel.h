/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001  R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/* Constants und Documentation that apply to several of the
 * ./bessel_[ijky].c  files */

/* *******************************************************************

 Explanation of machine-dependent constants

   beta	  = Radix for the floating-point system
   minexp = Smallest representable power of beta
   maxexp = Smallest power of beta that overflows
   it = p = Number of bits (base-beta digits) in the mantissa
	    (significand) of a working precision (floating-point) variable
   NSIG	  = Decimal significance desired.  Should be set to
	    INT(LOG10(2)*it+1).	 Setting NSIG lower will result
	    in decreased accuracy while setting NSIG higher will
	    increase CPU time without increasing accuracy.  The
	    truncation error is limited to a relative error of
	    T=.5*10^(-NSIG).
   ENTEN  = 10 ^ K, where K is the largest long such that
	    ENTEN is machine-representable in working precision
   ENSIG  = 10 ^ NSIG
   RTNSIG = 10 ^ (-K) for the smallest long K such that
	    K >= NSIG/4
   ENMTEN = Smallest ABS(X) such that X/4 does not underflow
   XINF	  = Largest positive machine number; approximately beta ^ maxexp
	    == DBL_MAX (defined in  #include <float.h>)
   SQXMIN = Square root of beta ^ minexp = sqrt(DBL_MIN)

   EPS	  = The smallest positive floating-point number such that 1.0+EPS > 1.0
	  = beta ^ (-p)	 == DBL_EPSILON


  For I :

   EXPARG = Largest working precision argument that the library
	    EXP routine can handle and upper limit on the
	    magnitude of X when IZE=1; approximately LOG(beta ^ maxexp)

  For I and J :

   xlrg_IJ = xlrg_BESS_IJ (was = XLARGE). Upper limit on the magnitude of X
	    (when IZE=2 for I()).  Bear in mind that if floor(abs(x)) =: N, then
	    at least N iterations of the backward recursion will be executed.
	    The value of 10 ^ 4 was used till Feb.2009, when it was increased
	    to 10 ^ 5 (= 1e5).

  For j :
   XMIN_J  = Smallest acceptable argument for RBESY; approximately
	    max(2*beta ^ minexp, 2/XINF), rounded up

  For Y :

   xlrg_Y =  (was = XLARGE). Upper bound on X;
	    approximately 1/DEL, because the sine and cosine functions
	    have lost about half of their precision at that point.

   EPS_SINC = Machine number below which sin(x)/x = 1; approximately SQRT(EPS).
   THRESH = Lower bound for use of the asymptotic form;
	    approximately AINT(-LOG10(EPS/2.0))+1.0


  For K :

   xmax_k =  (was = XMAX). Upper limit on the magnitude of X when ize = 1;
	    i.e. maximal x for UNscaled answer.

	    Solution to equation:
	       W(X) * (1 -1/8 X + 9/128 X^2) = beta ^ minexp
	    where  W(X) = EXP(-X)*SQRT(PI/2X)

 --------------------------------------------------------------------

     Approximate values for some important machines are:

		  beta minexp maxexp it NSIG ENTEN ENSIG RTNSIG ENMTEN	 EXPARG
 IEEE (IBM/XT,
   SUN, etc.) (S.P.)  2	  -126	128  24	  8  1e38   1e8	  1e-2	4.70e-38     88
 IEEE	(...) (D.P.)  2	 -1022 1024  53	 16  1e308  1e16  1e-4	8.90e-308   709
 CRAY-1	      (S.P.)  2	 -8193 8191  48	 15  1e2465 1e15  1e-4	1.84e-2466 5677
 Cyber 180/855
   under NOS  (S.P.)  2	  -975 1070  48	 15  1e322  1e15  1e-4	1.25e-293   741
 IBM 3033     (D.P.) 16	   -65	 63  14	  5  1e75   1e5	  1e-2	2.16e-78    174
 VAX	      (S.P.)  2	  -128	127  24	  8  1e38   1e8	  1e-2	1.17e-38     88
 VAX D-Format (D.P.)  2	  -128	127  56	 17  1e38   1e17  1e-5	1.17e-38     88
 VAX G-Format (D.P.)  2	 -1024 1023  53	 16  1e307  1e16  1e-4	2.22e-308   709


And routine specific :

		    xlrg_IJ xlrg_Y xmax_k EPS_SINC XMIN_J    XINF   THRESH
 IEEE (IBM/XT,
   SUN, etc.) (S.P.)	1e4  1e4   85.337  1e-4	 2.36e-38   3.40e38	8.
 IEEE	(...) (D.P.)	1e4  1e8  705.342  1e-8	 4.46e-308  1.79e308   16.
 CRAY-1	      (S.P.)	1e4  2e7 5674.858  5e-8	 3.67e-2466 5.45e2465  15.
 Cyber 180/855
   under NOS  (S.P.)	1e4  2e7  672.788  5e-8	 6.28e-294  1.26e322   15.
 IBM 3033     (D.P.)	1e4  1e8  177.852  1e-8	 2.77e-76   7.23e75    17.
 VAX	      (S.P.)	1e4  1e4   86.715  1e-4	 1.18e-38   1.70e38	8.
 VAX e-Format (D.P.)	1e4  1e9   86.715  1e-9	 1.18e-38   1.70e38    17.
 VAX G-Format (D.P.)	1e4  1e8  706.728  1e-8	 2.23e-308  8.98e307   16.

*/
#define nsig_BESS	16
#define ensig_BESS	1e16
#define rtnsig_BESS	1e-4
#define enmten_BESS	8.9e-308
#define enten_BESS	1e308

#define exparg_BESS	709.
#define xlrg_BESS_IJ	1e5
#define xlrg_BESS_Y	1e8
#define thresh_BESS_Y	16.

#define xmax_BESS_K	705.342/* maximal x for UNscaled answer */


/* sqrt(DBL_MIN) =	1.491668e-154 */
#define sqxmin_BESS_K	1.49e-154

/* x < eps_sinc	 <==>  sin(x)/x == 1 (particularly "==>");
  Linux (around 2001-02) gives 2.14946906753213e-08
  Solaris 2.5.1		 gives 2.14911933289084e-08
*/
#define M_eps_sinc	2.149e-8
