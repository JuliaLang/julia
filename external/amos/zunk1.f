      SUBROUTINE ZUNK1(ZR, ZI, FNU, KODE, MR, N, YR, YI, NZ, TOL, ELIM,
     * ALIM)
C***BEGIN PROLOGUE  ZUNK1
C***REFER TO  ZBESK
C
C     ZUNK1 COMPUTES K(FNU,Z) AND ITS ANALYTIC CONTINUATION FROM THE
C     RIGHT HALF PLANE TO THE LEFT HALF PLANE BY MEANS OF THE
C     UNIFORM ASYMPTOTIC EXPANSION.
C     MR INDICATES THE DIRECTION OF ROTATION FOR ANALYTIC CONTINUATION.
C     NZ=-1 MEANS AN OVERFLOW WILL OCCUR
C
C***ROUTINES CALLED  ZKSCL,ZS1S2,ZUCHK,ZUNIK,D1MACH,ZABS
C***END PROLOGUE  ZUNK1
C     COMPLEX CFN,CK,CONE,CRSC,CS,CSCL,CSGN,CSPN,CSR,CSS,CWRK,CY,CZERO,
C    *C1,C2,PHI,PHID,RZ,SUM,SUMD,S1,S2,Y,Z,ZETA1,ZETA1D,ZETA2,ZETA2D,ZR
      DOUBLE PRECISION ALIM, ANG, APHI, ASC, ASCLE, BRY, CKI, CKR,
     * CONER, CRSC, CSCL, CSGNI, CSPNI, CSPNR, CSR, CSRR, CSSR,
     * CWRKI, CWRKR, CYI, CYR, C1I, C1R, C2I, C2M, C2R, ELIM, FMR, FN,
     * FNF, FNU, PHIDI, PHIDR, PHII, PHIR, PI, RAST, RAZR, RS1, RZI,
     * RZR, SGN, STI, STR, SUMDI, SUMDR, SUMI, SUMR, S1I, S1R, S2I,
     * S2R, TOL, YI, YR, ZEROI, ZEROR, ZETA1I, ZETA1R, ZETA2I, ZETA2R,
     * ZET1DI, ZET1DR, ZET2DI, ZET2DR, ZI, ZR, ZRI, ZRR, D1MACH, ZABS
      INTEGER I, IB, IFLAG, IFN, IL, INIT, INU, IUF, K, KDFLG, KFLAG,
     * KK, KODE, MR, N, NW, NZ, INITD, IC, IPARD, J
      DIMENSION BRY(3), INIT(2), YR(N), YI(N), SUMR(2), SUMI(2),
     * ZETA1R(2), ZETA1I(2), ZETA2R(2), ZETA2I(2), CYR(2), CYI(2),
     * CWRKR(16,3), CWRKI(16,3), CSSR(3), CSRR(3), PHIR(2), PHII(2)
      DATA ZEROR,ZEROI,CONER / 0.0D0, 0.0D0, 1.0D0 /
      DATA PI / 3.14159265358979324D0 /
C
      KDFLG = 1
      NZ = 0
C-----------------------------------------------------------------------
C     EXP(-ALIM)=EXP(-ELIM)/TOL=APPROX. ONE PRECISION GREATER THAN
C     THE UNDERFLOW LIMIT
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
      BRY(2) = 1.0D0/BRY(1)
      BRY(3) = D1MACH(2)
      ZRR = ZR
      ZRI = ZI
      IF (ZR.GE.0.0D0) GO TO 10
      ZRR = -ZR
      ZRI = -ZI
   10 CONTINUE
      J = 2
      DO 70 I=1,N
C-----------------------------------------------------------------------
C     J FLIP FLOPS BETWEEN 1 AND 2 IN J = 3 - J
C-----------------------------------------------------------------------
        J = 3 - J
        FN = FNU + DBLE(FLOAT(I-1))
        INIT(J) = 0
        CALL ZUNIK(ZRR, ZRI, FN, 2, 0, TOL, INIT(J), PHIR(J), PHII(J),
     *   ZETA1R(J), ZETA1I(J), ZETA2R(J), ZETA2I(J), SUMR(J), SUMI(J),
     *   CWRKR(1,J), CWRKI(1,J))
        IF (KODE.EQ.1) GO TO 20
        STR = ZRR + ZETA2R(J)
        STI = ZRI + ZETA2I(J)
        RAST = FN/ZABS(COMPLEX(STR,STI))
        STR = STR*RAST*RAST
        STI = -STI*RAST*RAST
        S1R = ZETA1R(J) - STR
        S1I = ZETA1I(J) - STI
        GO TO 30
   20   CONTINUE
        S1R = ZETA1R(J) - ZETA2R(J)
        S1I = ZETA1I(J) - ZETA2I(J)
   30   CONTINUE
        RS1 = S1R
C-----------------------------------------------------------------------
C     TEST FOR UNDERFLOW AND OVERFLOW
C-----------------------------------------------------------------------
        IF (DABS(RS1).GT.ELIM) GO TO 60
        IF (KDFLG.EQ.1) KFLAG = 2
        IF (DABS(RS1).LT.ALIM) GO TO 40
C-----------------------------------------------------------------------
C     REFINE  TEST AND SCALE
C-----------------------------------------------------------------------
        APHI = ZABS(COMPLEX(PHIR(J),PHII(J)))
        RS1 = RS1 + DLOG(APHI)
        IF (DABS(RS1).GT.ELIM) GO TO 60
        IF (KDFLG.EQ.1) KFLAG = 1
        IF (RS1.LT.0.0D0) GO TO 40
        IF (KDFLG.EQ.1) KFLAG = 3
   40   CONTINUE
C-----------------------------------------------------------------------
C     SCALE S1 TO KEEP INTERMEDIATE ARITHMETIC ON SCALE NEAR
C     EXPONENT EXTREMES
C-----------------------------------------------------------------------
        S2R = PHIR(J)*SUMR(J) - PHII(J)*SUMI(J)
        S2I = PHIR(J)*SUMI(J) + PHII(J)*SUMR(J)
        STR = DEXP(S1R)*CSSR(KFLAG)
        S1R = STR*DCOS(S1I)
        S1I = STR*DSIN(S1I)
        STR = S2R*S1R - S2I*S1I
        S2I = S1R*S2I + S2R*S1I
        S2R = STR
        IF (KFLAG.NE.1) GO TO 50
        CALL ZUCHK(S2R, S2I, NW, BRY(1), TOL)
        IF (NW.NE.0) GO TO 60
   50   CONTINUE
        CYR(KDFLG) = S2R
        CYI(KDFLG) = S2I
        YR(I) = S2R*CSRR(KFLAG)
        YI(I) = S2I*CSRR(KFLAG)
        IF (KDFLG.EQ.2) GO TO 75
        KDFLG = 2
        GO TO 70
   60   CONTINUE
        IF (RS1.GT.0.0D0) GO TO 300
C-----------------------------------------------------------------------
C     FOR ZR.LT.0.0, THE I FUNCTION TO BE ADDED WILL OVERFLOW
C-----------------------------------------------------------------------
        IF (ZR.LT.0.0D0) GO TO 300
        KDFLG = 1
        YR(I)=ZEROR
        YI(I)=ZEROI
        NZ=NZ+1
        IF (I.EQ.1) GO TO 70
        IF ((YR(I-1).EQ.ZEROR).AND.(YI(I-1).EQ.ZEROI)) GO TO 70
        YR(I-1)=ZEROR
        YI(I-1)=ZEROI
        NZ=NZ+1
   70 CONTINUE
      I = N
   75 CONTINUE
      RAZR = 1.0D0/ZABS(COMPLEX(ZRR,ZRI))
      STR = ZRR*RAZR
      STI = -ZRI*RAZR
      RZR = (STR+STR)*RAZR
      RZI = (STI+STI)*RAZR
      CKR = FN*RZR
      CKI = FN*RZI
      IB = I + 1
      IF (N.LT.IB) GO TO 160
C-----------------------------------------------------------------------
C     TEST LAST MEMBER FOR UNDERFLOW AND OVERFLOW. SET SEQUENCE TO ZERO
C     ON UNDERFLOW.
C-----------------------------------------------------------------------
      FN = FNU + DBLE(FLOAT(N-1))
      IPARD = 1
      IF (MR.NE.0) IPARD = 0
      INITD = 0
      CALL ZUNIK(ZRR, ZRI, FN, 2, IPARD, TOL, INITD, PHIDR, PHIDI,
     * ZET1DR, ZET1DI, ZET2DR, ZET2DI, SUMDR, SUMDI, CWRKR(1,3),
     * CWRKI(1,3))
      IF (KODE.EQ.1) GO TO 80
      STR = ZRR + ZET2DR
      STI = ZRI + ZET2DI
      RAST = FN/ZABS(COMPLEX(STR,STI))
      STR = STR*RAST*RAST
      STI = -STI*RAST*RAST
      S1R = ZET1DR - STR
      S1I = ZET1DI - STI
      GO TO 90
   80 CONTINUE
      S1R = ZET1DR - ZET2DR
      S1I = ZET1DI - ZET2DI
   90 CONTINUE
      RS1 = S1R
      IF (DABS(RS1).GT.ELIM) GO TO 95
      IF (DABS(RS1).LT.ALIM) GO TO 100
C----------------------------------------------------------------------------
C     REFINE ESTIMATE AND TEST
C-------------------------------------------------------------------------
      APHI = ZABS(COMPLEX(PHIDR,PHIDI))
      RS1 = RS1+DLOG(APHI)
      IF (DABS(RS1).LT.ELIM) GO TO 100
   95 CONTINUE
      IF (DABS(RS1).GT.0.0D0) GO TO 300
C-----------------------------------------------------------------------
C     FOR ZR.LT.0.0, THE I FUNCTION TO BE ADDED WILL OVERFLOW
C-----------------------------------------------------------------------
      IF (ZR.LT.0.0D0) GO TO 300
      NZ = N
      DO 96 I=1,N
        YR(I) = ZEROR
        YI(I) = ZEROI
   96 CONTINUE
      RETURN
C---------------------------------------------------------------------------
C     FORWARD RECUR FOR REMAINDER OF THE SEQUENCE
C----------------------------------------------------------------------------
  100 CONTINUE
      S1R = CYR(1)
      S1I = CYI(1)
      S2R = CYR(2)
      S2I = CYI(2)
      C1R = CSRR(KFLAG)
      ASCLE = BRY(KFLAG)
      DO 120 I=IB,N
        C2R = S2R
        C2I = S2I
        S2R = CKR*C2R - CKI*C2I + S1R
        S2I = CKR*C2I + CKI*C2R + S1I
        S1R = C2R
        S1I = C2I
        CKR = CKR + RZR
        CKI = CKI + RZI
        C2R = S2R*C1R
        C2I = S2I*C1R
        YR(I) = C2R
        YI(I) = C2I
        IF (KFLAG.GE.3) GO TO 120
        STR = DABS(C2R)
        STI = DABS(C2I)
        C2M = DMAX1(STR,STI)
        IF (C2M.LE.ASCLE) GO TO 120
        KFLAG = KFLAG + 1
        ASCLE = BRY(KFLAG)
        S1R = S1R*C1R
        S1I = S1I*C1R
        S2R = C2R
        S2I = C2I
        S1R = S1R*CSSR(KFLAG)
        S1I = S1I*CSSR(KFLAG)
        S2R = S2R*CSSR(KFLAG)
        S2I = S2I*CSSR(KFLAG)
        C1R = CSRR(KFLAG)
  120 CONTINUE
  160 CONTINUE
      IF (MR.EQ.0) RETURN
C-----------------------------------------------------------------------
C     ANALYTIC CONTINUATION FOR RE(Z).LT.0.0D0
C-----------------------------------------------------------------------
      NZ = 0
      FMR = DBLE(FLOAT(MR))
      SGN = -DSIGN(PI,FMR)
C-----------------------------------------------------------------------
C     CSPN AND CSGN ARE COEFF OF K AND I FUNCTIONS RESP.
C-----------------------------------------------------------------------
      CSGNI = SGN
      INU = INT(SNGL(FNU))
      FNF = FNU - DBLE(FLOAT(INU))
      IFN = INU + N - 1
      ANG = FNF*SGN
      CSPNR = DCOS(ANG)
      CSPNI = DSIN(ANG)
      IF (MOD(IFN,2).EQ.0) GO TO 170
      CSPNR = -CSPNR
      CSPNI = -CSPNI
  170 CONTINUE
      ASC = BRY(1)
      IUF = 0
      KK = N
      KDFLG = 1
      IB = IB - 1
      IC = IB - 1
      DO 270 K=1,N
        FN = FNU + DBLE(FLOAT(KK-1))
C-----------------------------------------------------------------------
C     LOGIC TO SORT OUT CASES WHOSE PARAMETERS WERE SET FOR THE K
C     FUNCTION ABOVE
C-----------------------------------------------------------------------
        M=3
        IF (N.GT.2) GO TO 175
  172   CONTINUE
        INITD = INIT(J)
        PHIDR = PHIR(J)
        PHIDI = PHII(J)
        ZET1DR = ZETA1R(J)
        ZET1DI = ZETA1I(J)
        ZET2DR = ZETA2R(J)
        ZET2DI = ZETA2I(J)
        SUMDR = SUMR(J)
        SUMDI = SUMI(J)
        M = J
        J = 3 - J
        GO TO 180
  175   CONTINUE
        IF ((KK.EQ.N).AND.(IB.LT.N)) GO TO 180
        IF ((KK.EQ.IB).OR.(KK.EQ.IC)) GO TO 172
        INITD = 0
  180   CONTINUE
        CALL ZUNIK(ZRR, ZRI, FN, 1, 0, TOL, INITD, PHIDR, PHIDI,
     *   ZET1DR, ZET1DI, ZET2DR, ZET2DI, SUMDR, SUMDI,
     *   CWRKR(1,M), CWRKI(1,M))
        IF (KODE.EQ.1) GO TO 200
        STR = ZRR + ZET2DR
        STI = ZRI + ZET2DI
        RAST = FN/ZABS(COMPLEX(STR,STI))
        STR = STR*RAST*RAST
        STI = -STI*RAST*RAST
        S1R = -ZET1DR + STR
        S1I = -ZET1DI + STI
        GO TO 210
  200   CONTINUE
        S1R = -ZET1DR + ZET2DR
        S1I = -ZET1DI + ZET2DI
  210   CONTINUE
C-----------------------------------------------------------------------
C     TEST FOR UNDERFLOW AND OVERFLOW
C-----------------------------------------------------------------------
        RS1 = S1R
        IF (DABS(RS1).GT.ELIM) GO TO 260
        IF (KDFLG.EQ.1) IFLAG = 2
        IF (DABS(RS1).LT.ALIM) GO TO 220
C-----------------------------------------------------------------------
C     REFINE  TEST AND SCALE
C-----------------------------------------------------------------------
        APHI = ZABS(COMPLEX(PHIDR,PHIDI))
        RS1 = RS1 + DLOG(APHI)
        IF (DABS(RS1).GT.ELIM) GO TO 260
        IF (KDFLG.EQ.1) IFLAG = 1
        IF (RS1.LT.0.0D0) GO TO 220
        IF (KDFLG.EQ.1) IFLAG = 3
  220   CONTINUE
        STR = PHIDR*SUMDR - PHIDI*SUMDI
        STI = PHIDR*SUMDI + PHIDI*SUMDR
        S2R = -CSGNI*STI
        S2I = CSGNI*STR
        STR = DEXP(S1R)*CSSR(IFLAG)
        S1R = STR*DCOS(S1I)
        S1I = STR*DSIN(S1I)
        STR = S2R*S1R - S2I*S1I
        S2I = S2R*S1I + S2I*S1R
        S2R = STR
        IF (IFLAG.NE.1) GO TO 230
        CALL ZUCHK(S2R, S2I, NW, BRY(1), TOL)
        IF (NW.EQ.0) GO TO 230
        S2R = ZEROR
        S2I = ZEROI
  230   CONTINUE
        CYR(KDFLG) = S2R
        CYI(KDFLG) = S2I
        C2R = S2R
        C2I = S2I
        S2R = S2R*CSRR(IFLAG)
        S2I = S2I*CSRR(IFLAG)
C-----------------------------------------------------------------------
C     ADD I AND K FUNCTIONS, K SEQUENCE IN Y(I), I=1,N
C-----------------------------------------------------------------------
        S1R = YR(KK)
        S1I = YI(KK)
        IF (KODE.EQ.1) GO TO 250
        CALL ZS1S2(ZRR, ZRI, S1R, S1I, S2R, S2I, NW, ASC, ALIM, IUF)
        NZ = NZ + NW
  250   CONTINUE
        YR(KK) = S1R*CSPNR - S1I*CSPNI + S2R
        YI(KK) = CSPNR*S1I + CSPNI*S1R + S2I
        KK = KK - 1
        CSPNR = -CSPNR
        CSPNI = -CSPNI
        IF (C2R.NE.0.0D0 .OR. C2I.NE.0.0D0) GO TO 255
        KDFLG = 1
        GO TO 270
  255   CONTINUE
        IF (KDFLG.EQ.2) GO TO 275
        KDFLG = 2
        GO TO 270
  260   CONTINUE
        IF (RS1.GT.0.0D0) GO TO 300
        S2R = ZEROR
        S2I = ZEROI
        GO TO 230
  270 CONTINUE
      K = N
  275 CONTINUE
      IL = N - K
      IF (IL.EQ.0) RETURN
C-----------------------------------------------------------------------
C     RECUR BACKWARD FOR REMAINDER OF I SEQUENCE AND ADD IN THE
C     K FUNCTIONS, SCALING THE I SEQUENCE DURING RECURRENCE TO KEEP
C     INTERMEDIATE ARITHMETIC ON SCALE NEAR EXPONENT EXTREMES.
C-----------------------------------------------------------------------
      S1R = CYR(1)
      S1I = CYI(1)
      S2R = CYR(2)
      S2I = CYI(2)
      CSR = CSRR(IFLAG)
      ASCLE = BRY(IFLAG)
      FN = DBLE(FLOAT(INU+IL))
      DO 290 I=1,IL
        C2R = S2R
        C2I = S2I
        S2R = S1R + (FN+FNF)*(RZR*C2R-RZI*C2I)
        S2I = S1I + (FN+FNF)*(RZR*C2I+RZI*C2R)
        S1R = C2R
        S1I = C2I
        FN = FN - 1.0D0
        C2R = S2R*CSR
        C2I = S2I*CSR
        CKR = C2R
        CKI = C2I
        C1R = YR(KK)
        C1I = YI(KK)
        IF (KODE.EQ.1) GO TO 280
        CALL ZS1S2(ZRR, ZRI, C1R, C1I, C2R, C2I, NW, ASC, ALIM, IUF)
        NZ = NZ + NW
  280   CONTINUE
        YR(KK) = C1R*CSPNR - C1I*CSPNI + C2R
        YI(KK) = C1R*CSPNI + C1I*CSPNR + C2I
        KK = KK - 1
        CSPNR = -CSPNR
        CSPNI = -CSPNI
        IF (IFLAG.GE.3) GO TO 290
        C2R = DABS(CKR)
        C2I = DABS(CKI)
        C2M = DMAX1(C2R,C2I)
        IF (C2M.LE.ASCLE) GO TO 290
        IFLAG = IFLAG + 1
        ASCLE = BRY(IFLAG)
        S1R = S1R*CSR
        S1I = S1I*CSR
        S2R = CKR
        S2I = CKI
        S1R = S1R*CSSR(IFLAG)
        S1I = S1I*CSSR(IFLAG)
        S2R = S2R*CSSR(IFLAG)
        S2I = S2I*CSSR(IFLAG)
        CSR = CSRR(IFLAG)
  290 CONTINUE
      RETURN
  300 CONTINUE
      NZ = -1
      RETURN
      END
