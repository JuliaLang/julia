      SUBROUTINE ZSERI(ZR, ZI, FNU, KODE, N, YR, YI, NZ, TOL, ELIM,
     * ALIM)
C***BEGIN PROLOGUE  ZSERI
C***REFER TO  ZBESI,ZBESK
C
C     ZSERI COMPUTES THE I BESSEL FUNCTION FOR REAL(Z).GE.0.0 BY
C     MEANS OF THE POWER SERIES FOR LARGE CABS(Z) IN THE
C     REGION CABS(Z).LE.2*SQRT(FNU+1). NZ=0 IS A NORMAL RETURN.
C     NZ.GT.0 MEANS THAT THE LAST NZ COMPONENTS WERE SET TO ZERO
C     DUE TO UNDERFLOW. NZ.LT.0 MEANS UNDERFLOW OCCURRED, BUT THE
C     CONDITION CABS(Z).LE.2*SQRT(FNU+1) WAS VIOLATED AND THE
C     COMPUTATION MUST BE COMPLETED IN ANOTHER ROUTINE WITH N=N-ABS(NZ).
C
C***ROUTINES CALLED  DGAMLN,D1MACH,ZUCHK,ZABS,ZDIV,ZLOG,ZMLT
C***END PROLOGUE  ZSERI
C     COMPLEX AK1,CK,COEF,CONE,CRSC,CSCL,CZ,CZERO,HZ,RZ,S1,S2,Y,Z
      DOUBLE PRECISION AA, ACZ, AK, AK1I, AK1R, ALIM, ARM, ASCLE, ATOL,
     * AZ, CKI, CKR, COEFI, COEFR, CONEI, CONER, CRSCR, CZI, CZR, DFNU,
     * ELIM, FNU, FNUP, HZI, HZR, RAZ, RS, RTR1, RZI, RZR, S, SS, STI,
     * STR, S1I, S1R, S2I, S2R, TOL, YI, YR, WI, WR, ZEROI, ZEROR, ZI,
     * ZR, DGAMLN, D1MACH, ZABS
      INTEGER I, IB, IDUM, IFLAG, IL, K, KODE, L, M, N, NN, NZ, NW
      DIMENSION YR(N), YI(N), WR(2), WI(2)
      DATA ZEROR,ZEROI,CONER,CONEI / 0.0D0, 0.0D0, 1.0D0, 0.0D0 /
C
      NZ = 0
      AZ = ZABS(COMPLEX(ZR,ZI))
      IF (AZ.EQ.0.0D0) GO TO 160
      ARM = 1.0D+3*D1MACH(1)
      RTR1 = DSQRT(ARM)
      CRSCR = 1.0D0
      IFLAG = 0
      IF (AZ.LT.ARM) GO TO 150
      HZR = 0.5D0*ZR
      HZI = 0.5D0*ZI
      CZR = ZEROR
      CZI = ZEROI
      IF (AZ.LE.RTR1) GO TO 10
      CALL ZMLT(HZR, HZI, HZR, HZI, CZR, CZI)
   10 CONTINUE
      ACZ = ZABS(COMPLEX(CZR,CZI))
      NN = N
      CALL ZLOG(HZR, HZI, CKR, CKI, IDUM)
   20 CONTINUE
      DFNU = FNU + DBLE(FLOAT(NN-1))
      FNUP = DFNU + 1.0D0
C-----------------------------------------------------------------------
C     UNDERFLOW TEST
C-----------------------------------------------------------------------
      AK1R = CKR*DFNU
      AK1I = CKI*DFNU
      AK = DGAMLN(FNUP,IDUM)
      AK1R = AK1R - AK
      IF (KODE.EQ.2) AK1R = AK1R - ZR
      IF (AK1R.GT.(-ELIM)) GO TO 40
   30 CONTINUE
      NZ = NZ + 1
      YR(NN) = ZEROR
      YI(NN) = ZEROI
      IF (ACZ.GT.DFNU) GO TO 190
      NN = NN - 1
      IF (NN.EQ.0) RETURN
      GO TO 20
   40 CONTINUE
      IF (AK1R.GT.(-ALIM)) GO TO 50
      IFLAG = 1
      SS = 1.0D0/TOL
      CRSCR = TOL
      ASCLE = ARM*SS
   50 CONTINUE
      AA = DEXP(AK1R)
      IF (IFLAG.EQ.1) AA = AA*SS
      COEFR = AA*DCOS(AK1I)
      COEFI = AA*DSIN(AK1I)
      ATOL = TOL*ACZ/FNUP
      IL = MIN0(2,NN)
      DO 90 I=1,IL
        DFNU = FNU + DBLE(FLOAT(NN-I))
        FNUP = DFNU + 1.0D0
        S1R = CONER
        S1I = CONEI
        IF (ACZ.LT.TOL*FNUP) GO TO 70
        AK1R = CONER
        AK1I = CONEI
        AK = FNUP + 2.0D0
        S = FNUP
        AA = 2.0D0
   60   CONTINUE
        RS = 1.0D0/S
        STR = AK1R*CZR - AK1I*CZI
        STI = AK1R*CZI + AK1I*CZR
        AK1R = STR*RS
        AK1I = STI*RS
        S1R = S1R + AK1R
        S1I = S1I + AK1I
        S = S + AK
        AK = AK + 2.0D0
        AA = AA*ACZ*RS
        IF (AA.GT.ATOL) GO TO 60
   70   CONTINUE
        S2R = S1R*COEFR - S1I*COEFI
        S2I = S1R*COEFI + S1I*COEFR
        WR(I) = S2R
        WI(I) = S2I
        IF (IFLAG.EQ.0) GO TO 80
        CALL ZUCHK(S2R, S2I, NW, ASCLE, TOL)
        IF (NW.NE.0) GO TO 30
   80   CONTINUE
        M = NN - I + 1
        YR(M) = S2R*CRSCR
        YI(M) = S2I*CRSCR
        IF (I.EQ.IL) GO TO 90
        CALL ZDIV(COEFR, COEFI, HZR, HZI, STR, STI)
        COEFR = STR*DFNU
        COEFI = STI*DFNU
   90 CONTINUE
      IF (NN.LE.2) RETURN
      K = NN - 2
      AK = DBLE(FLOAT(K))
      RAZ = 1.0D0/AZ
      STR = ZR*RAZ
      STI = -ZI*RAZ
      RZR = (STR+STR)*RAZ
      RZI = (STI+STI)*RAZ
      IF (IFLAG.EQ.1) GO TO 120
      IB = 3
  100 CONTINUE
      DO 110 I=IB,NN
        YR(K) = (AK+FNU)*(RZR*YR(K+1)-RZI*YI(K+1)) + YR(K+2)
        YI(K) = (AK+FNU)*(RZR*YI(K+1)+RZI*YR(K+1)) + YI(K+2)
        AK = AK - 1.0D0
        K = K - 1
  110 CONTINUE
      RETURN
C-----------------------------------------------------------------------
C     RECUR BACKWARD WITH SCALED VALUES
C-----------------------------------------------------------------------
  120 CONTINUE
C-----------------------------------------------------------------------
C     EXP(-ALIM)=EXP(-ELIM)/TOL=APPROX. ONE PRECISION ABOVE THE
C     UNDERFLOW LIMIT = ASCLE = D1MACH(1)*SS*1.0D+3
C-----------------------------------------------------------------------
      S1R = WR(1)
      S1I = WI(1)
      S2R = WR(2)
      S2I = WI(2)
      DO 130 L=3,NN
        CKR = S2R
        CKI = S2I
        S2R = S1R + (AK+FNU)*(RZR*CKR-RZI*CKI)
        S2I = S1I + (AK+FNU)*(RZR*CKI+RZI*CKR)
        S1R = CKR
        S1I = CKI
        CKR = S2R*CRSCR
        CKI = S2I*CRSCR
        YR(K) = CKR
        YI(K) = CKI
        AK = AK - 1.0D0
        K = K - 1
        IF (ZABS(COMPLEX(CKR,CKI)).GT.ASCLE) GO TO 140
  130 CONTINUE
      RETURN
  140 CONTINUE
      IB = L + 1
      IF (IB.GT.NN) RETURN
      GO TO 100
  150 CONTINUE
      NZ = N
      IF (FNU.EQ.0.0D0) NZ = NZ - 1
  160 CONTINUE
      YR(1) = ZEROR
      YI(1) = ZEROI
      IF (FNU.NE.0.0D0) GO TO 170
      YR(1) = CONER
      YI(1) = CONEI
  170 CONTINUE
      IF (N.EQ.1) RETURN
      DO 180 I=2,N
        YR(I) = ZEROR
        YI(I) = ZEROI
  180 CONTINUE
      RETURN
C-----------------------------------------------------------------------
C     RETURN WITH NZ.LT.0 IF CABS(Z*Z/4).GT.FNU+N-NZ-1 COMPLETE
C     THE CALCULATION IN CBINU WITH N=N-IABS(NZ)
C-----------------------------------------------------------------------
  190 CONTINUE
      NZ = -NZ
      RETURN
      END
