      SUBROUTINE ZUNI1(ZR, ZI, FNU, KODE, N, YR, YI, NZ, NLAST, FNUL,
     * TOL, ELIM, ALIM)
C***BEGIN PROLOGUE  ZUNI1
C***REFER TO  ZBESI,ZBESK
C
C     ZUNI1 COMPUTES I(FNU,Z)  BY MEANS OF THE UNIFORM ASYMPTOTIC
C     EXPANSION FOR I(FNU,Z) IN -PI/3.LE.ARG Z.LE.PI/3.
C
C     FNUL IS THE SMALLEST ORDER PERMITTED FOR THE ASYMPTOTIC
C     EXPANSION. NLAST=0 MEANS ALL OF THE Y VALUES WERE SET.
C     NLAST.NE.0 IS THE NUMBER LEFT TO BE COMPUTED BY ANOTHER
C     FORMULA FOR ORDERS FNU TO FNU+NLAST-1 BECAUSE FNU+NLAST-1.LT.FNUL.
C     Y(I)=CZERO FOR I=NLAST+1,N
C
C***ROUTINES CALLED  ZUCHK,ZUNIK,ZUOIK,D1MACH,ZABS
C***END PROLOGUE  ZUNI1
C     COMPLEX CFN,CONE,CRSC,CSCL,CSR,CSS,CWRK,CZERO,C1,C2,PHI,RZ,SUM,S1,
C    *S2,Y,Z,ZETA1,ZETA2
      DOUBLE PRECISION ALIM, APHI, ASCLE, BRY, CONER, CRSC,
     * CSCL, CSRR, CSSR, CWRKI, CWRKR, C1R, C2I, C2M, C2R, ELIM, FN,
     * FNU, FNUL, PHII, PHIR, RAST, RS1, RZI, RZR, STI, STR, SUMI,
     * SUMR, S1I, S1R, S2I, S2R, TOL, YI, YR, ZEROI, ZEROR, ZETA1I,
     * ZETA1R, ZETA2I, ZETA2R, ZI, ZR, CYR, CYI, D1MACH, ZABS
      INTEGER I, IFLAG, INIT, K, KODE, M, N, ND, NLAST, NN, NUF, NW, NZ
      DIMENSION BRY(3), YR(N), YI(N), CWRKR(16), CWRKI(16), CSSR(3),
     * CSRR(3), CYR(2), CYI(2)
      DATA ZEROR,ZEROI,CONER / 0.0D0, 0.0D0, 1.0D0 /
C
      NZ = 0
      ND = N
      NLAST = 0
C-----------------------------------------------------------------------
C     COMPUTED VALUES WITH EXPONENTS BETWEEN ALIM AND ELIM IN MAG-
C     NITUDE ARE SCALED TO KEEP INTERMEDIATE ARITHMETIC ON SCALE,
C     EXP(ALIM)=EXP(ELIM)*TOL
C-----------------------------------------------------------------------
      CSCL = 1.0D0/TOL
      CRSC = TOL
      CSSR(1) = CSCL
      CSSR(2) = CONER
      CSSR(3) = CRSC
      CSRR(1) = CRSC
      CSRR(2) = CONER
      CSRR(3) = CSCL
      BRY(1) = 1.0D+3*D1MACH(1)/TOL
C-----------------------------------------------------------------------
C     CHECK FOR UNDERFLOW AND OVERFLOW ON FIRST MEMBER
C-----------------------------------------------------------------------
      FN = DMAX1(FNU,1.0D0)
      INIT = 0
      CALL ZUNIK(ZR, ZI, FN, 1, 1, TOL, INIT, PHIR, PHII, ZETA1R,
     * ZETA1I, ZETA2R, ZETA2I, SUMR, SUMI, CWRKR, CWRKI)
      IF (KODE.EQ.1) GO TO 10
      STR = ZR + ZETA2R
      STI = ZI + ZETA2I
      RAST = FN/ZABS(COMPLEX(STR,STI))
      STR = STR*RAST*RAST
      STI = -STI*RAST*RAST
      S1R = -ZETA1R + STR
      S1I = -ZETA1I + STI
      GO TO 20
   10 CONTINUE
      S1R = -ZETA1R + ZETA2R
      S1I = -ZETA1I + ZETA2I
   20 CONTINUE
      RS1 = S1R
      IF (DABS(RS1).GT.ELIM) GO TO 130
   30 CONTINUE
      NN = MIN0(2,ND)
      DO 80 I=1,NN
        FN = FNU + DBLE(FLOAT(ND-I))
        INIT = 0
        CALL ZUNIK(ZR, ZI, FN, 1, 0, TOL, INIT, PHIR, PHII, ZETA1R,
     *   ZETA1I, ZETA2R, ZETA2I, SUMR, SUMI, CWRKR, CWRKI)
        IF (KODE.EQ.1) GO TO 40
        STR = ZR + ZETA2R
        STI = ZI + ZETA2I
        RAST = FN/ZABS(COMPLEX(STR,STI))
        STR = STR*RAST*RAST
        STI = -STI*RAST*RAST
        S1R = -ZETA1R + STR
        S1I = -ZETA1I + STI + ZI
        GO TO 50
   40   CONTINUE
        S1R = -ZETA1R + ZETA2R
        S1I = -ZETA1I + ZETA2I
   50   CONTINUE
C-----------------------------------------------------------------------
C     TEST FOR UNDERFLOW AND OVERFLOW
C-----------------------------------------------------------------------
        RS1 = S1R
        IF (DABS(RS1).GT.ELIM) GO TO 110
        IF (I.EQ.1) IFLAG = 2
        IF (DABS(RS1).LT.ALIM) GO TO 60
C-----------------------------------------------------------------------
C     REFINE  TEST AND SCALE
C-----------------------------------------------------------------------
        APHI = ZABS(COMPLEX(PHIR,PHII))
        RS1 = RS1 + DLOG(APHI)
        IF (DABS(RS1).GT.ELIM) GO TO 110
        IF (I.EQ.1) IFLAG = 1
        IF (RS1.LT.0.0D0) GO TO 60
        IF (I.EQ.1) IFLAG = 3
   60   CONTINUE
C-----------------------------------------------------------------------
C     SCALE S1 IF CABS(S1).LT.ASCLE
C-----------------------------------------------------------------------
        S2R = PHIR*SUMR - PHII*SUMI
        S2I = PHIR*SUMI + PHII*SUMR
        STR = DEXP(S1R)*CSSR(IFLAG)
        S1R = STR*DCOS(S1I)
        S1I = STR*DSIN(S1I)
        STR = S2R*S1R - S2I*S1I
        S2I = S2R*S1I + S2I*S1R
        S2R = STR
        IF (IFLAG.NE.1) GO TO 70
        CALL ZUCHK(S2R, S2I, NW, BRY(1), TOL)
        IF (NW.NE.0) GO TO 110
   70   CONTINUE
        CYR(I) = S2R
        CYI(I) = S2I
        M = ND - I + 1
        YR(M) = S2R*CSRR(IFLAG)
        YI(M) = S2I*CSRR(IFLAG)
   80 CONTINUE
      IF (ND.LE.2) GO TO 100
      RAST = 1.0D0/ZABS(COMPLEX(ZR,ZI))
      STR = ZR*RAST
      STI = -ZI*RAST
      RZR = (STR+STR)*RAST
      RZI = (STI+STI)*RAST
      BRY(2) = 1.0D0/BRY(1)
      BRY(3) = D1MACH(2)
      S1R = CYR(1)
      S1I = CYI(1)
      S2R = CYR(2)
      S2I = CYI(2)
      C1R = CSRR(IFLAG)
      ASCLE = BRY(IFLAG)
      K = ND - 2
      FN = DBLE(FLOAT(K))
      DO 90 I=3,ND
        C2R = S2R
        C2I = S2I
        S2R = S1R + (FNU+FN)*(RZR*C2R-RZI*C2I)
        S2I = S1I + (FNU+FN)*(RZR*C2I+RZI*C2R)
        S1R = C2R
        S1I = C2I
        C2R = S2R*C1R
        C2I = S2I*C1R
        YR(K) = C2R
        YI(K) = C2I
        K = K - 1
        FN = FN - 1.0D0
        IF (IFLAG.GE.3) GO TO 90
        STR = DABS(C2R)
        STI = DABS(C2I)
        C2M = DMAX1(STR,STI)
        IF (C2M.LE.ASCLE) GO TO 90
        IFLAG = IFLAG + 1
        ASCLE = BRY(IFLAG)
        S1R = S1R*C1R
        S1I = S1I*C1R
        S2R = C2R
        S2I = C2I
        S1R = S1R*CSSR(IFLAG)
        S1I = S1I*CSSR(IFLAG)
        S2R = S2R*CSSR(IFLAG)
        S2I = S2I*CSSR(IFLAG)
        C1R = CSRR(IFLAG)
   90 CONTINUE
  100 CONTINUE
      RETURN
C-----------------------------------------------------------------------
C     SET UNDERFLOW AND UPDATE PARAMETERS
C-----------------------------------------------------------------------
  110 CONTINUE
      IF (RS1.GT.0.0D0) GO TO 120
      YR(ND) = ZEROR
      YI(ND) = ZEROI
      NZ = NZ + 1
      ND = ND - 1
      IF (ND.EQ.0) GO TO 100
      CALL ZUOIK(ZR, ZI, FNU, KODE, 1, ND, YR, YI, NUF, TOL, ELIM, ALIM)
      IF (NUF.LT.0) GO TO 120
      ND = ND - NUF
      NZ = NZ + NUF
      IF (ND.EQ.0) GO TO 100
      FN = FNU + DBLE(FLOAT(ND-1))
      IF (FN.GE.FNUL) GO TO 30
      NLAST = ND
      RETURN
  120 CONTINUE
      NZ = -1
      RETURN
  130 CONTINUE
      IF (RS1.GT.0.0D0) GO TO 120
      NZ = N
      DO 140 I=1,N
        YR(I) = ZEROR
        YI(I) = ZEROI
  140 CONTINUE
      RETURN
      END
