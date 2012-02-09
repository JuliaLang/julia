      SUBROUTINE ZBUNI(ZR, ZI, FNU, KODE, N, YR, YI, NZ, NUI, NLAST,
     * FNUL, TOL, ELIM, ALIM)
C***BEGIN PROLOGUE  ZBUNI
C***REFER TO  ZBESI,ZBESK
C
C     ZBUNI COMPUTES THE I BESSEL FUNCTION FOR LARGE CABS(Z).GT.
C     FNUL AND FNU+N-1.LT.FNUL. THE ORDER IS INCREASED FROM
C     FNU+N-1 GREATER THAN FNUL BY ADDING NUI AND COMPUTING
C     ACCORDING TO THE UNIFORM ASYMPTOTIC EXPANSION FOR I(FNU,Z)
C     ON IFORM=1 AND THE EXPANSION FOR J(FNU,Z) ON IFORM=2
C
C***ROUTINES CALLED  ZUNI1,ZUNI2,ZABS,D1MACH
C***END PROLOGUE  ZBUNI
C     COMPLEX CSCL,CSCR,CY,RZ,ST,S1,S2,Y,Z
      DOUBLE PRECISION ALIM, AX, AY, CSCLR, CSCRR, CYI, CYR, DFNU,
     * ELIM, FNU, FNUI, FNUL, GNU, RAZ, RZI, RZR, STI, STR, S1I, S1R,
     * S2I, S2R, TOL, YI, YR, ZI, ZR, ZABS, ASCLE, BRY, C1R, C1I, C1M,
     * D1MACH
      INTEGER I, IFLAG, IFORM, K, KODE, N, NL, NLAST, NUI, NW, NZ
      DIMENSION YR(N), YI(N), CYR(2), CYI(2), BRY(3)
      NZ = 0
      AX = DABS(ZR)*1.7321D0
      AY = DABS(ZI)
      IFORM = 1
      IF (AY.GT.AX) IFORM = 2
      IF (NUI.EQ.0) GO TO 60
      FNUI = DBLE(FLOAT(NUI))
      DFNU = FNU + DBLE(FLOAT(N-1))
      GNU = DFNU + FNUI
      IF (IFORM.EQ.2) GO TO 10
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR I(FNU,Z) FOR LARGE FNU APPLIED IN
C     -PI/3.LE.ARG(Z).LE.PI/3
C-----------------------------------------------------------------------
      CALL ZUNI1(ZR, ZI, GNU, KODE, 2, CYR, CYI, NW, NLAST, FNUL, TOL,
     * ELIM, ALIM)
      GO TO 20
   10 CONTINUE
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR J(FNU,Z*EXP(M*HPI)) FOR LARGE FNU
C     APPLIED IN PI/3.LT.ABS(ARG(Z)).LE.PI/2 WHERE M=+I OR -I
C     AND HPI=PI/2
C-----------------------------------------------------------------------
      CALL ZUNI2(ZR, ZI, GNU, KODE, 2, CYR, CYI, NW, NLAST, FNUL, TOL,
     * ELIM, ALIM)
   20 CONTINUE
      IF (NW.LT.0) GO TO 50
      IF (NW.NE.0) GO TO 90
      STR = ZABS(COMPLEX(CYR(1),CYI(1)))
C----------------------------------------------------------------------
C     SCALE BACKWARD RECURRENCE, BRY(3) IS DEFINED BUT NEVER USED
C----------------------------------------------------------------------
      BRY(1)=1.0D+3*D1MACH(1)/TOL
      BRY(2) = 1.0D0/BRY(1)
      BRY(3) = BRY(2)
      IFLAG = 2
      ASCLE = BRY(2)
      CSCLR = 1.0D0
      IF (STR.GT.BRY(1)) GO TO 21
      IFLAG = 1
      ASCLE = BRY(1)
      CSCLR = 1.0D0/TOL
      GO TO 25
   21 CONTINUE
      IF (STR.LT.BRY(2)) GO TO 25
      IFLAG = 3
      ASCLE=BRY(3)
      CSCLR = TOL
   25 CONTINUE
      CSCRR = 1.0D0/CSCLR
      S1R = CYR(2)*CSCLR
      S1I = CYI(2)*CSCLR
      S2R = CYR(1)*CSCLR
      S2I = CYI(1)*CSCLR
      RAZ = 1.0D0/ZABS(COMPLEX(ZR,ZI))
      STR = ZR*RAZ
      STI = -ZI*RAZ
      RZR = (STR+STR)*RAZ
      RZI = (STI+STI)*RAZ
      DO 30 I=1,NUI
        STR = S2R
        STI = S2I
        S2R = (DFNU+FNUI)*(RZR*STR-RZI*STI) + S1R
        S2I = (DFNU+FNUI)*(RZR*STI+RZI*STR) + S1I
        S1R = STR
        S1I = STI
        FNUI = FNUI - 1.0D0
        IF (IFLAG.GE.3) GO TO 30
        STR = S2R*CSCRR
        STI = S2I*CSCRR
        C1R = DABS(STR)
        C1I = DABS(STI)
        C1M = DMAX1(C1R,C1I)
        IF (C1M.LE.ASCLE) GO TO 30
        IFLAG = IFLAG+1
        ASCLE = BRY(IFLAG)
        S1R = S1R*CSCRR
        S1I = S1I*CSCRR
        S2R = STR
        S2I = STI
        CSCLR = CSCLR*TOL
        CSCRR = 1.0D0/CSCLR
        S1R = S1R*CSCLR
        S1I = S1I*CSCLR
        S2R = S2R*CSCLR
        S2I = S2I*CSCLR
   30 CONTINUE
      YR(N) = S2R*CSCRR
      YI(N) = S2I*CSCRR
      IF (N.EQ.1) RETURN
      NL = N - 1
      FNUI = DBLE(FLOAT(NL))
      K = NL
      DO 40 I=1,NL
        STR = S2R
        STI = S2I
        S2R = (FNU+FNUI)*(RZR*STR-RZI*STI) + S1R
        S2I = (FNU+FNUI)*(RZR*STI+RZI*STR) + S1I
        S1R = STR
        S1I = STI
        STR = S2R*CSCRR
        STI = S2I*CSCRR
        YR(K) = STR
        YI(K) = STI
        FNUI = FNUI - 1.0D0
        K = K - 1
        IF (IFLAG.GE.3) GO TO 40
        C1R = DABS(STR)
        C1I = DABS(STI)
        C1M = DMAX1(C1R,C1I)
        IF (C1M.LE.ASCLE) GO TO 40
        IFLAG = IFLAG+1
        ASCLE = BRY(IFLAG)
        S1R = S1R*CSCRR
        S1I = S1I*CSCRR
        S2R = STR
        S2I = STI
        CSCLR = CSCLR*TOL
        CSCRR = 1.0D0/CSCLR
        S1R = S1R*CSCLR
        S1I = S1I*CSCLR
        S2R = S2R*CSCLR
        S2I = S2I*CSCLR
   40 CONTINUE
      RETURN
   50 CONTINUE
      NZ = -1
      IF(NW.EQ.(-2)) NZ=-2
      RETURN
   60 CONTINUE
      IF (IFORM.EQ.2) GO TO 70
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR I(FNU,Z) FOR LARGE FNU APPLIED IN
C     -PI/3.LE.ARG(Z).LE.PI/3
C-----------------------------------------------------------------------
      CALL ZUNI1(ZR, ZI, FNU, KODE, N, YR, YI, NW, NLAST, FNUL, TOL,
     * ELIM, ALIM)
      GO TO 80
   70 CONTINUE
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR J(FNU,Z*EXP(M*HPI)) FOR LARGE FNU
C     APPLIED IN PI/3.LT.ABS(ARG(Z)).LE.PI/2 WHERE M=+I OR -I
C     AND HPI=PI/2
C-----------------------------------------------------------------------
      CALL ZUNI2(ZR, ZI, FNU, KODE, N, YR, YI, NW, NLAST, FNUL, TOL,
     * ELIM, ALIM)
   80 CONTINUE
      IF (NW.LT.0) GO TO 50
      NZ = NW
      RETURN
   90 CONTINUE
      NLAST = N
      RETURN
      END
