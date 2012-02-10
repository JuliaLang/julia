*DECK D1MACH
      DOUBLE PRECISION FUNCTION D1MACH(I)
C***BEGIN PROLOGUE  D1MACH
C***DATE WRITTEN   750101   (YYMMDD)
C***REVISION DATE  890213   (YYMMDD)
C***CATEGORY NO.  R1
C***KEYWORDS  LIBRARY=SLATEC,TYPE=DOUBLE PRECISION(R1MACH-S D1MACH-D),
C             MACHINE CONSTANTS
C***AUTHOR  FOX, P. A., (BELL LABS)
C           HALL, A. D., (BELL LABS)
C           SCHRYER, N. L., (BELL LABS)
C***PURPOSE  Returns double precision machine dependent constants
C***DESCRIPTION
C
C   D1MACH can be used to obtain machine-dependent parameters
C   for the local machine environment.  It is a function
C   subprogram with one (input) argument, and can be called
C   as follows, for example
C
C        D = D1MACH(I)
C
C   where I=1,...,5.  The (output) value of D above is
C   determined by the (input) value of I.  The results for
C   various values of I are discussed below.
C
C   D1MACH( 1) = B**(EMIN-1), the smallest positive magnitude.
C   D1MACH( 2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
C   D1MACH( 3) = B**(-T), the smallest relative spacing.
C   D1MACH( 4) = B**(1-T), the largest relative spacing.
C   D1MACH( 5) = LOG10(B)
C
C   Assume double precision numbers are represented in the T-digit,
C   base-B form
C
C              sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C   where 0 .LE. X(I) .LT. B for I=1,...,T, 0 .LT. X(1), and
C   EMIN .LE. E .LE. EMAX.
C
C   The values of B, T, EMIN and EMAX are provided in I1MACH as
C   follows:
C   I1MACH(10) = B, the base.
C   I1MACH(14) = T, the number of base-B digits.
C   I1MACH(15) = EMIN, the smallest exponent E.
C   I1MACH(16) = EMAX, the largest exponent E.
C
C   To alter this function for a particular environment,
C   the desired set of DATA statements should be activated by
C   removing the C from column 1.  Also, the values of
C   D1MACH(1) - D1MACH(4) should be checked for consistency
C   with the local operating system.
C
C***REFERENCES  FOX P.A., HALL A.D., SCHRYER N.L.,*FRAMEWORK FOR A
C                 PORTABLE LIBRARY*, ACM TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOL. 4, NO. 2, JUNE 1978, PP. 177-188.
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  D1MACH
C
      INTEGER SMALL(4)
      INTEGER LARGE(4)
      INTEGER RIGHT(4)
      INTEGER DIVER(4)
      INTEGER LOG10(4)
C
      DOUBLE PRECISION DMACH(5)
      SAVE DMACH
C
C      EQUIVALENCE (DMACH(1),SMALL(1))
C      EQUIVALENCE (DMACH(2),LARGE(1))
C      EQUIVALENCE (DMACH(3),RIGHT(1))
C      EQUIVALENCE (DMACH(4),DIVER(1))
C      EQUIVALENCE (DMACH(5),LOG10(1))
C
C     MACHINE CONSTANTS FOR THE IBM PC
C     ASSUMES THAT ALL ARITHMETIC IS DONE IN DOUBLE PRECISION
C     ON 8088, I.E., NOT IN 80 BIT FORM FOR THE 8087.
C
      DATA DMACH(1) / 2.23D-308 /
C      DATA SMALL(1),SMALL(2) /  2002288515,    1050897 /
      DATA DMACH(2) / 1.79D-308 /
C      DATA LARGE(1),LARGE(2) /  1487780761, 2146426097 /
      DATA DMACH(3) / 1.11D-16 /
C      DATA RIGHT(1),RIGHT(2) / -1209488034, 1017118298 /
      DATA DMACH(4) / 2.22D-16 /
C      DATA DIVER(1),DIVER(2) / -1209488034, 1018166874 /
      DATA DMACH(5) / 0.3010299956639812 /
C      DATA LOG10(1),LOG10(2) /  1352628735, 1070810131 /
C
C
C***FIRST EXECUTABLE STATEMENT  D1MACH
      IF (I .LT. 1  .OR.  I .GT. 5)
     1   CALL XERROR ('D1MACH -- I OUT OF BOUNDS', 25, 1, 2)
C
      D1MACH = DMACH(I)
      RETURN
C
      END
