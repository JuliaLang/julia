      SUBROUTINE ZASYI(ZR, ZI, FNU, KODE, N, YR, YI, NZ, RL, TOL, ELIM,
     * ALIM)
C***BEGIN PROLOGUE  ZASYI
C***REFER TO  ZBESI,ZBESK
C
C     ZASYI COMPUTES THE I BESSEL FUNCTION FOR REAL(Z).GE.0.0 BY
C     MEANS OF THE ASYMPTOTIC EXPANSION FOR LARGE CABS(Z) IN THE
C     REGION CABS(Z).GT.MAX(RL,FNU*FNU/2). NZ=0 IS A NORMAL RETURN.
C     NZ.LT.0 INDICATES AN OVERFLOW ON KODE=1.
C
C***ROUTINES CALLED  D1MACH,ZABS,ZDIV,ZEXP,ZMLT,ZSQRT
C***END PROLOGUE  ZASYI
C     COMPLEX AK1,CK,CONE,CS1,CS2,CZ,CZERO,DK,EZ,P1,RZ,S2,Y,Z
      DOUBLE PRECISION AA, AEZ, AK, AK1I, AK1R, ALIM, ARG, ARM, ATOL,
     * AZ, BB, BK, CKI, CKR, CONEI, CONER, CS1I, CS1R, CS2I, CS2R, CZI,
     * CZR, DFNU, DKI, DKR, DNU2, ELIM, EZI, EZR, FDN, FNU, PI, P1I,
     * P1R, RAZ, RL, RTPI, RTR1, RZI, RZR, S, SGN, SQK, STI, STR, S2I,
     * S2R, TOL, TZI, TZR, YI, YR, ZEROI, ZEROR, ZI, ZR, D1MACH, ZABS
      INTEGER I, IB, IL, INU, J, JL, K, KODE, KODED, M, N, NN, NZ
      DIMENSION YR(N), YI(N)
      DATA PI, RTPI  /3.14159265358979324D0 , 0.159154943091895336D0 /
      DATA ZEROR,ZEROI,CONER,CONEI / 0.0D0, 0.0D0, 1.0D0, 0.0D0 /
C
      NZ = 0
      AZ = ZABS(COMPLEX(ZR,ZI))
      ARM = 1.0D+3*D1MACH(1)
      RTR1 = DSQRT(ARM)
      IL = MIN0(2,N)
      DFNU = FNU + DBLE(FLOAT(N-IL))
C-----------------------------------------------------------------------
C     OVERFLOW TEST
C-----------------------------------------------------------------------
      RAZ = 1.0D0/AZ
      STR = ZR*RAZ
      STI = -ZI*RAZ
      AK1R = RTPI*STR*RAZ
      AK1I = RTPI*STI*RAZ
      CALL ZSQRT(AK1R, AK1I, AK1R, AK1I)
      CZR = ZR
      CZI = ZI
      IF (KODE.NE.2) GO TO 10
      CZR = ZEROR
      CZI = ZI
   10 CONTINUE
      IF (DABS(CZR).GT.ELIM) GO TO 100
      DNU2 = DFNU + DFNU
      KODED = 1
      IF ((DABS(CZR).GT.ALIM) .AND. (N.GT.2)) GO TO 20
      KODED = 0
      CALL ZEXP(CZR, CZI, STR, STI)
      CALL ZMLT(AK1R, AK1I, STR, STI, AK1R, AK1I)
   20 CONTINUE
      FDN = 0.0D0
      IF (DNU2.GT.RTR1) FDN = DNU2*DNU2
      EZR = ZR*8.0D0
      EZI = ZI*8.0D0
C-----------------------------------------------------------------------
C     WHEN Z IS IMAGINARY, THE ERROR TEST MUST BE MADE RELATIVE TO THE
C     FIRST RECIPROCAL POWER SINCE THIS IS THE LEADING TERM OF THE
C     EXPANSION FOR THE IMAGINARY PART.
C-----------------------------------------------------------------------
      AEZ = 8.0D0*AZ
      S = TOL/AEZ
      JL = INT(SNGL(RL+RL)) + 2
      P1R = ZEROR
      P1I = ZEROI
      IF (ZI.EQ.0.0D0) GO TO 30
C-----------------------------------------------------------------------
C     CALCULATE EXP(PI*(0.5+FNU+N-IL)*I) TO MINIMIZE LOSSES OF
C     SIGNIFICANCE WHEN FNU OR N IS LARGE
C-----------------------------------------------------------------------
      INU = INT(SNGL(FNU))
      ARG = (FNU-DBLE(FLOAT(INU)))*PI
      INU = INU + N - IL
      AK = -DSIN(ARG)
      BK = DCOS(ARG)
      IF (ZI.LT.0.0D0) BK = -BK
      P1R = AK
      P1I = BK
      IF (MOD(INU,2).EQ.0) GO TO 30
      P1R = -P1R
      P1I = -P1I
   30 CONTINUE
      DO 70 K=1,IL
        SQK = FDN - 1.0D0
        ATOL = S*DABS(SQK)
        SGN = 1.0D0
        CS1R = CONER
        CS1I = CONEI
        CS2R = CONER
        CS2I = CONEI
        CKR = CONER
        CKI = CONEI
        AK = 0.0D0
        AA = 1.0D0
        BB = AEZ
        DKR = EZR
        DKI = EZI
        DO 40 J=1,JL
          CALL ZDIV(CKR, CKI, DKR, DKI, STR, STI)
          CKR = STR*SQK
          CKI = STI*SQK
          CS2R = CS2R + CKR
          CS2I = CS2I + CKI
          SGN = -SGN
          CS1R = CS1R + CKR*SGN
          CS1I = CS1I + CKI*SGN
          DKR = DKR + EZR
          DKI = DKI + EZI
          AA = AA*DABS(SQK)/BB
          BB = BB + AEZ
          AK = AK + 8.0D0
          SQK = SQK - AK
          IF (AA.LE.ATOL) GO TO 50
   40   CONTINUE
        GO TO 110
   50   CONTINUE
        S2R = CS1R
        S2I = CS1I
        IF (ZR+ZR.GE.ELIM) GO TO 60
        TZR = ZR + ZR
        TZI = ZI + ZI
        CALL ZEXP(-TZR, -TZI, STR, STI)
        CALL ZMLT(STR, STI, P1R, P1I, STR, STI)
        CALL ZMLT(STR, STI, CS2R, CS2I, STR, STI)
        S2R = S2R + STR
        S2I = S2I + STI
   60   CONTINUE
        FDN = FDN + 8.0D0*DFNU + 4.0D0
        P1R = -P1R
        P1I = -P1I
        M = N - IL + K
        YR(M) = S2R*AK1R - S2I*AK1I
        YI(M) = S2R*AK1I + S2I*AK1R
   70 CONTINUE
      IF (N.LE.2) RETURN
      NN = N
      K = NN - 2
      AK = DBLE(FLOAT(K))
      STR = ZR*RAZ
      STI = -ZI*RAZ
      RZR = (STR+STR)*RAZ
      RZI = (STI+STI)*RAZ
      IB = 3
      DO 80 I=IB,NN
        YR(K) = (AK+FNU)*(RZR*YR(K+1)-RZI*YI(K+1)) + YR(K+2)
        YI(K) = (AK+FNU)*(RZR*YI(K+1)+RZI*YR(K+1)) + YI(K+2)
        AK = AK - 1.0D0
        K = K - 1
   80 CONTINUE
      IF (KODED.EQ.0) RETURN
      CALL ZEXP(CZR, CZI, CKR, CKI)
      DO 90 I=1,NN
        STR = YR(I)*CKR - YI(I)*CKI
        YI(I) = YR(I)*CKI + YI(I)*CKR
        YR(I) = STR
   90 CONTINUE
      RETURN
  100 CONTINUE
      NZ = -1
      RETURN
  110 CONTINUE
      NZ=-2
      RETURN
      END
