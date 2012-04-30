
	  *********************************
 	  * Announcing FDLIBM Version 5.3 *
	  *********************************
============================================================
			FDLIBM
============================================================
	developed at Sun Microsystems, Inc. 

What's new in FDLIBM 5.3?

CONFIGURE
	To build FDLIBM, edit the supplied Makefile or create
	a local Makefile by running "sh configure" 
	using the supplied configure script contributed by Nelson Beebe

BUGS FIXED

    1. e_pow.c incorrect results when 
	x is very close to -1.0 and y is very large, e.g.
  	pow(-1.0000000000000002e+00,4.5035996273704970e+15) = 0
  	pow(-9.9999999999999978e-01,4.5035996273704970e+15) = 0
	Correct results are close to -e and -1/e.

    2. k_tan.c error was > 1 ulp target for FDLIBM
	5.2: Worst error at least 1.45 ulp at
	tan(1.7765241907548024E+269) = 1.7733884462610958E+16
	5.3: Worst error 0.96 ulp

NOT FIXED YET

    3. Compiler failure on non-standard code
	Statements like
	            *(1+(int*)&t1) = 0;
	are not standard C and cause some optimizing compilers (e.g. GCC)
	to generate bad code under optimization.    These cases
	are to be addressed in the next release.
	
FDLIBM (Freely Distributable LIBM) is a C math library 
for machines that support IEEE 754 floating-point arithmetic. 
In this release, only double precision is supported.

FDLIBM is intended to provide a reasonably portable (see 
assumptions below), reference quality (below one ulp for
major functions like sin,cos,exp,log) math library 
(libm.a).  For a copy of FDLIBM, please see
	http://www.netlib.org/fdlibm/
or
	http://www.validlab.com/software/

--------------
1. ASSUMPTIONS
--------------
FDLIBM (double precision version) assumes:
 a.  IEEE 754 style (if not precise compliance) arithmetic;
 b.  32 bit 2's complement integer arithmetic;
 c.  Each double precision floating-point number must be in IEEE 754 
     double format, and that each number can be retrieved as two 32-bit 
     integers through the using of pointer bashing as in the example 
     below:

     Example: let y = 2.0
	double fp number y: 	2.0
	IEEE double format:	0x4000000000000000

	Referencing y as two integers:
	*(int*)&y,*(1+(int*)&y) =	{0x40000000,0x0} (on sparc)
					{0x0,0x40000000} (on 386)

	Note: Four macros are defined in fdlibm.h to handle this kind of
	retrieving:

	__HI(x)		the high part of a double x 
			(sign,exponent,the first 21 significant bits)
	__LO(x)		the least 32 significant bits of x
	__HIp(x)	same as __HI except that the argument is a pointer
			to a double
	__LOp(x)	same as __LO except that the argument is a pointer
			to a double
	
	To ensure obtaining correct ordering, one must define  __LITTLE_ENDIAN
	during compilation for little endian machine (like 386,486). The 
	default is big endian.

	If the behavior of pointer bashing is undefined, one may hack on the 
	macro in fdlibm.h.
	
  d. IEEE exceptions may trigger "signals" as is common in Unix
     implementations. 

-------------------
2. EXCEPTION CASES
-------------------
All exception cases in the FDLIBM functions will be mapped
to one of the following four exceptions:

   +-huge*huge, +-tiny*tiny,    +-1.0/0.0,	+-0.0/0.0
    (overflow)	(underflow)  (divided-by-zero) 	(invalid)

For example, log(0) is a singularity and is thus mapped to 
	-1.0/0.0 = -infinity.
That is, FDLIBM's log will compute -one/zero and return the
computed value.  On an IEEE machine, this will trigger the 
divided-by-zero exception and a negative infinity is returned by 
default.

Similarly, exp(-huge) will be mapped to tiny*tiny to generate
an underflow signal. 


--------------------------------
3. STANDARD CONFORMANCE WRAPPER 
--------------------------------
The default FDLIBM functions (compiled with -D_IEEE_LIBM flag)  
are in "IEEE spirit" (i.e., return the most reasonable result in 
floating-point arithmetic). If one wants FDLIBM to comply with
standards like SVID, X/OPEN, or POSIX/ANSI, then one can 
create a multi-standard compliant FDLIBM. In this case, each
function in FDLIBM is actually a standard compliant wrapper
function.  

File organization:
    1. For FDLIBM's kernel (internal) function,
		File name	Entry point
		---------------------------
		k_sin.c		__kernel_sin
		k_tan.c		__kernel_tan
		---------------------------
    2. For functions that have no standards conflict 
		File name	Entry point
		---------------------------
		s_sin.c		sin
		s_erf.c		erf
		---------------------------
    3. Ieee754 core functions
		File name	Entry point
		---------------------------
		e_exp.c		__ieee754_exp
		e_sinh.c	__ieee754_sinh
		---------------------------
    4. Wrapper functions
		File name	Entry point
		---------------------------
		w_exp.c		exp
		w_sinh.c	sinh
		---------------------------

Wrapper functions will twist the result of the ieee754 
function to comply to the standard specified by the value 
of _LIB_VERSION 
    if _LIB_VERSION = _IEEE_, return the ieee754 result;
    if _LIB_VERSION = _SVID_, return SVID result;
    if _LIB_VERSION = _XOPEN_, return XOPEN result;
    if _LIB_VERSION = _POSIX_, return POSIX/ANSI result.
(These are macros, see fdlibm.h for their definition.)


--------------------------------
4. HOW TO CREATE FDLIBM's libm.a
--------------------------------
There are two types of libm.a. One is IEEE only, and the other is
multi-standard compliant (supports IEEE,XOPEN,POSIX/ANSI,SVID).

To create the IEEE only libm.a, use 
	    make "CFLAGS = -D_IEEE_LIBM"	 
This will create an IEEE libm.a, which is smaller in size, and 
somewhat faster.

To create a multi-standard compliant libm, use
    make "CFLAGS = -D_IEEE_MODE"   --- multi-standard fdlibm: default
					 to IEEE
    make "CFLAGS = -D_XOPEN_MODE"  --- multi-standard fdlibm: default
					 to X/OPEN
    make "CFLAGS = -D_POSIX_MODE"  --- multi-standard fdlibm: default
					 to POSIX/ANSI
    make "CFLAGS = -D_SVID3_MODE"  --- multi-standard fdlibm: default
					 to SVID


Here is how one makes a SVID compliant libm.
    Make the library by
		make "CFLAGS = -D_SVID3_MODE".
    The libm.a of FDLIBM will be multi-standard compliant and 
    _LIB_VERSION is initialized to the value _SVID_ . 

    example1:
    ---------
	    main()
	    {
		double y0();
		printf("y0(1e300) = %1.20e\n",y0(1e300));
		exit(0);
	    }

    % cc example1.c libm.a
    % a.out
    y0: TLOSS error
    y0(1e300) = 0.00000000000000000000e+00


It is possible to change the default standard in multi-standard 
fdlibm. Here is an example of how to do it:
    example2:
    ---------
	#include "fdlibm.h"	/* must include FDLIBM's fdlibm.h */
	main()
	{
		double y0();
		_LIB_VERSION =  _IEEE_;
		printf("IEEE: y0(1e300) = %1.20e\n",y0(1e300));
		_LIB_VERSION = _XOPEN_;
		printf("XOPEN y0(1e300) = %1.20e\n",y0(1e300));
		_LIB_VERSION = _POSIX_;
		printf("POSIX y0(1e300) = %1.20e\n",y0(1e300));
		_LIB_VERSION = _SVID_;
		printf("SVID  y0(1e300) = %1.20e\n",y0(1e300));
		exit(0);
	}

    % cc example2.c libm.a
    % a.out
      IEEE: y0(1e300) = -1.36813604503424810557e-151
      XOPEN y0(1e300) = 0.00000000000000000000e+00
      POSIX y0(1e300) = 0.00000000000000000000e+00
      y0: TLOSS error
      SVID  y0(1e300) = 0.00000000000000000000e+00

Note:	Here _LIB_VERSION is a global variable. If global variables 
	are forbidden, then one should modify fdlibm.h to change
	_LIB_VERSION to be a global constant. In this case, one
	may not change the value of _LIB_VERSION as in example2.

---------------------------
5. NOTES ON PORTING FDLIBM
---------------------------
	Care must be taken when installing FDLIBM over existing
	libm.a.
	All co-existing function prototypes must agree, otherwise
	users will encounter mysterious failures.

	So far, the only known likely conflict is the declaration 
	of the IEEE recommended function scalb:

		double scalb(double,double)	(1)	SVID3 defined
		double scalb(double,int)	(2)	IBM,DEC,...

	FDLIBM follows Sun definition and use (1) as default. 
	If one's existing libm.a uses (2), then one may raise
	the flags _SCALB_INT during the compilation of FDLIBM
	to get the correct function prototype.
	(E.g., make "CFLAGS = -D_IEEE_LIBM -D_SCALB_INT".)
	NOTE that if -D_SCALB_INT is raised, it won't be SVID3
	conformant.

--------------
6. PROBLEMS ?
--------------
Please send comments and bug reports to the electronic mail address
suggested by: 
		fdlibm-comments AT sun.com

