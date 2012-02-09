      SUBROUTINE ZACAI(ZR, ZI, FNU, KODE, MR, N, YR, YI, NZ, RL, TOL,
     * ELIM, ALIM)
C***BEGIN PROLOGUE  ZACAI
C***REFER TO  ZAIRY
C
C     ZACAI APPLIES THE ANALYTIC CONTINUATION FORMULA
C
C         K(FNU,ZN*EXP(MP))=K(FNU,ZN)*EXP(-MP*FNU) - MP*I(FNU,ZN)
C                 MP=PI*MR*CMPLX(0.0,1.0)
C
C     TO CONTINUE THE K FUNCTION FROM THE RIGHT HALF TO THE LEFT
C     HALF Z PLANE FOR USE WITH ZAIRY WHERE FNU=1/3 OR 2/3 AND N=1.
C     ZACAI IS THE SAME AS ZACON WITH THE PARTS FOR LARGER ORDERS AND
C     RECURRENCE REMOVED. A RECURSIVE CALL TO ZACON CAN RESULT IF ZACON
C     IS CALLED FROM ZAIRY.
C
C***ROUTINES CALLED  ZASYI,ZBKNU,ZMLRI,ZSERI,ZS1S2,D1MACH,ZABS
C***END PROLOGUE  ZACAI
C     COMPLEX CSGN,CSPN,C1,C2,Y,Z,ZN,CY
      DOUBLE PRECISION ALIM, ARG, ASCLE, AZ, CSGNR, CSGNI, CSPNR,
     * CSPNI, C1R, C1I, C2R, C2I, CYR, CYI, DFNU, ELIM, FMR, FNU, PI,
     * RL, SGN, TOL, YY, YR, YI, ZR, ZI, ZNR, ZNI, D1MACH, ZABS
      INTEGER INU, IUF, KODE, MR, N, NN, NW, NZ
      DIMENSION YR(N), YI(N), CYR(2), CYI(2)
      DATA PI / 3.14159265358979324D0 /
      NZ = 0
      ZNR = -ZR
      ZNI = -ZI
      AZ = ZABS(COMPLEX(ZR,ZI))
      NN = N
      DFNU = FNU + DBLE(FLOAT(N-1))
      IF (AZ.LE.2.0D0) GO TO 10
      IF (AZ*AZ*0.25D0.GT.DFNU+1.0D0) GO TO 20
   10 CONTINUE
C-----------------------------------------------------------------------
C     POWER SERIES FOR THE I FUNCTION
C-----------------------------------------------------------------------
      CALL ZSERI(ZNR, ZNI, FNU, KODE, NN, YR, YI, NW, TOL, ELIM, ALIM)
      GO TO 40
   20 CONTINUE
      IF (AZ.LT.RL) GO TO 30
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR LARGE Z FOR THE I FUNCTION
C-----------------------------------------------------------------------
      CALL ZASYI(ZNR, ZNI, FNU, KODE, NN, YR, YI, NW, RL, TOL, ELIM,
     * ALIM)
      IF (NW.LT.0) GO TO 80
      GO TO 40
   30 CONTINUE
C-----------------------------------------------------------------------
C     MILLER ALGORITHM NORMALIZED BY THE SERIES FOR THE I FUNCTION
C-----------------------------------------------------------------------
      CALL ZMLRI(ZNR, ZNI, FNU, KODE, NN, YR, YI, NW, TOL)
      IF(NW.LT.0) GO TO 80
   40 CONTINUE
C-----------------------------------------------------------------------
C     ANALYTIC CONTINUATION TO THE LEFT HALF PLANE FOR THE K FUNCTION
C-----------------------------------------------------------------------
      CALL ZBKNU(ZNR, ZNI, FNU, KODE, 1, CYR, CYI, NW, TOL, ELIM, ALIM)
      IF (NW.NE.0) GO TO 80
      FMR = DBLE(FLOAT(MR))
      SGN = -DSIGN(PI,FMR)
      CSGNR = 0.0D0
      CSGNI = SGN
      IF (KODE.EQ.1) GO TO 50
      YY = -ZNI
      CSGNR = -CSGNI*DSIN(YY)
      CSGNI = CSGNI*DCOS(YY)
   50 CONTINUE
C-----------------------------------------------------------------------
C     CALCULATE CSPN=EXP(FNU*PI*I) TO MINIMIZE LOSSES OF SIGNIFICANCE
C     WHEN FNU IS LARGE
C-----------------------------------------------------------------------
      INU = INT(SNGL(FNU))
      ARG = (FNU-DBLE(FLOAT(INU)))*SGN
      CSPNR = DCOS(ARG)
      CSPNI = DSIN(ARG)
      IF (MOD(INU,2).EQ.0) GO TO 60
      CSPNR = -CSPNR
      CSPNI = -CSPNI
   60 CONTINUE
      C1R = CYR(1)
      C1I = CYI(1)
      C2R = YR(1)
      C2I = YI(1)
      IF (KODE.EQ.1) GO TO 70
      IUF = 0
      ASCLE = 1.0D+3*D1MACH(1)/TOL
      CALL ZS1S2(ZNR, ZNI, C1R, C1I, C2R, C2I, NW, ASCLE, ALIM, IUF)
      NZ = NZ + NW
   70 CONTINUE
      YR(1) = CSPNR*C1R - CSPNI*C1I + CSGNR*C2R - CSGNI*C2I
      YI(1) = CSPNR*C1I + CSPNI*C1R + CSGNR*C2I + CSGNI*C2R
      RETURN
   80 CONTINUE
      NZ = -1
      IF(NW.EQ.(-2)) NZ=-2
      RETURN
      END
