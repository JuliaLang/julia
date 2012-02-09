      SUBROUTINE ZUNK2(ZR, ZI, FNU, KODE, MR, N, YR, YI, NZ, TOL, ELIM,
     * ALIM)
C***BEGIN PROLOGUE  ZUNK2
C***REFER TO  ZBESK
C
C     ZUNK2 COMPUTES K(FNU,Z) AND ITS ANALYTIC CONTINUATION FROM THE
C     RIGHT HALF PLANE TO THE LEFT HALF PLANE BY MEANS OF THE
C     UNIFORM ASYMPTOTIC EXPANSIONS FOR H(KIND,FNU,ZN) AND J(FNU,ZN)
C     WHERE ZN IS IN THE RIGHT HALF PLANE, KIND=(3-MR)/2, MR=+1 OR
C     -1. HERE ZN=ZR*I OR -ZR*I WHERE ZR=Z IF Z IS IN THE RIGHT
C     HALF PLANE OR ZR=-Z IF Z IS IN THE LEFT HALF PLANE. MR INDIC-
C     ATES THE DIRECTION OF ROTATION FOR ANALYTIC CONTINUATION.
C     NZ=-1 MEANS AN OVERFLOW WILL OCCUR
C
C***ROUTINES CALLED  ZAIRY,ZKSCL,ZS1S2,ZUCHK,ZUNHJ,D1MACH,ZABS
C***END PROLOGUE  ZUNK2
C     COMPLEX AI,ARG,ARGD,ASUM,ASUMD,BSUM,BSUMD,CFN,CI,CIP,CK,CONE,CRSC,
C    *CR1,CR2,CS,CSCL,CSGN,CSPN,CSR,CSS,CY,CZERO,C1,C2,DAI,PHI,PHID,RZ,
C    *S1,S2,Y,Z,ZB,ZETA1,ZETA1D,ZETA2,ZETA2D,ZN,ZR
      DOUBLE PRECISION AARG, AIC, AII, AIR, ALIM, ANG, APHI, ARGDI,
     * ARGDR, ARGI, ARGR, ASC, ASCLE, ASUMDI, ASUMDR, ASUMI, ASUMR,
     * BRY, BSUMDI, BSUMDR, BSUMI, BSUMR, CAR, CIPI, CIPR, CKI, CKR,
     * CONER, CRSC, CR1I, CR1R, CR2I, CR2R, CSCL, CSGNI, CSI,
     * CSPNI, CSPNR, CSR, CSRR, CSSR, CYI, CYR, C1I, C1R, C2I, C2M,
     * C2R, DAII, DAIR, ELIM, FMR, FN, FNF, FNU, HPI, PHIDI, PHIDR,
     * PHII, PHIR, PI, PTI, PTR, RAST, RAZR, RS1, RZI, RZR, SAR, SGN,
     * STI, STR, S1I, S1R, S2I, S2R, TOL, YI, YR, YY, ZBI, ZBR, ZEROI,
     * ZEROR, ZETA1I, ZETA1R, ZETA2I, ZETA2R, ZET1DI, ZET1DR, ZET2DI,
     * ZET2DR, ZI, ZNI, ZNR, ZR, ZRI, ZRR, D1MACH, ZABS
      INTEGER I, IB, IFLAG, IFN, IL, IN, INU, IUF, K, KDFLG, KFLAG, KK,
     * KODE, MR, N, NAI, NDAI, NW, NZ, IDUM, J, IPARD, IC
      DIMENSION BRY(3), YR(N), YI(N), ASUMR(2), ASUMI(2), BSUMR(2),
     * BSUMI(2), PHIR(2), PHII(2), ARGR(2), ARGI(2), ZETA1R(2),
     * ZETA1I(2), ZETA2R(2), ZETA2I(2), CYR(2), CYI(2), CIPR(4),
     * CIPI(4), CSSR(3), CSRR(3)
      DATA ZEROR,ZEROI,CONER,CR1R,CR1I,CR2R,CR2I /
     1         0.0D0, 0.0D0, 1.0D0,
     1 1.0D0,1.73205080756887729D0 , -0.5D0,-8.66025403784438647D-01 /
      DATA HPI, PI, AIC /
     1     1.57079632679489662D+00,     3.14159265358979324D+00,
     1     1.26551212348464539D+00/
      DATA CIPR(1),CIPI(1),CIPR(2),CIPI(2),CIPR(3),CIPI(3),CIPR(4),
     * CIPI(4) /
     1  1.0D0,0.0D0 ,  0.0D0,-1.0D0 ,  -1.0D0,0.0D0 ,  0.0D0,1.0D0 /
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
      YY = ZRI
      ZNR = ZRI
      ZNI = -ZRR
      ZBR = ZRR
      ZBI = ZRI
      INU = INT(SNGL(FNU))
      FNF = FNU - DBLE(FLOAT(INU))
      ANG = -HPI*FNF
      CAR = DCOS(ANG)
      SAR = DSIN(ANG)
      C2R = HPI*SAR
      C2I = -HPI*CAR
      KK = MOD(INU,4) + 1
      STR = C2R*CIPR(KK) - C2I*CIPI(KK)
      STI = C2R*CIPI(KK) + C2I*CIPR(KK)
      CSR = CR1R*STR - CR1I*STI
      CSI = CR1R*STI + CR1I*STR
      IF (YY.GT.0.0D0) GO TO 20
      ZNR = -ZNR
      ZBI = -ZBI
   20 CONTINUE
C-----------------------------------------------------------------------
C     K(FNU,Z) IS COMPUTED FROM H(2,FNU,-I*Z) WHERE Z IS IN THE FIRST
C     QUADRANT. FOURTH QUADRANT VALUES (YY.LE.0.0E0) ARE COMPUTED BY
C     CONJUGATION SINCE THE K FUNCTION IS REAL ON THE POSITIVE REAL AXIS
C-----------------------------------------------------------------------
      J = 2
      DO 80 I=1,N
C-----------------------------------------------------------------------
C     J FLIP FLOPS BETWEEN 1 AND 2 IN J = 3 - J
C-----------------------------------------------------------------------
        J = 3 - J
        FN = FNU + DBLE(FLOAT(I-1))
        CALL ZUNHJ(ZNR, ZNI, FN, 0, TOL, PHIR(J), PHII(J), ARGR(J),
     *   ARGI(J), ZETA1R(J), ZETA1I(J), ZETA2R(J), ZETA2I(J), ASUMR(J),
     *   ASUMI(J), BSUMR(J), BSUMI(J))
        IF (KODE.EQ.1) GO TO 30
        STR = ZBR + ZETA2R(J)
        STI = ZBI + ZETA2I(J)
        RAST = FN/ZABS(COMPLEX(STR,STI))
        STR = STR*RAST*RAST
        STI = -STI*RAST*RAST
        S1R = ZETA1R(J) - STR
        S1I = ZETA1I(J) - STI
        GO TO 40
   30   CONTINUE
        S1R = ZETA1R(J) - ZETA2R(J)
        S1I = ZETA1I(J) - ZETA2I(J)
   40   CONTINUE
C-----------------------------------------------------------------------
C     TEST FOR UNDERFLOW AND OVERFLOW
C-----------------------------------------------------------------------
        RS1 = S1R
        IF (DABS(RS1).GT.ELIM) GO TO 70
        IF (KDFLG.EQ.1) KFLAG = 2
        IF (DABS(RS1).LT.ALIM) GO TO 50
C-----------------------------------------------------------------------
C     REFINE  TEST AND SCALE
C-----------------------------------------------------------------------
        APHI = ZABS(COMPLEX(PHIR(J),PHII(J)))
        AARG = ZABS(COMPLEX(ARGR(J),ARGI(J)))
        RS1 = RS1 + DLOG(APHI) - 0.25D0*DLOG(AARG) - AIC
        IF (DABS(RS1).GT.ELIM) GO TO 70
        IF (KDFLG.EQ.1) KFLAG = 1
        IF (RS1.LT.0.0D0) GO TO 50
        IF (KDFLG.EQ.1) KFLAG = 3
   50   CONTINUE
C-----------------------------------------------------------------------
C     SCALE S1 TO KEEP INTERMEDIATE ARITHMETIC ON SCALE NEAR
C     EXPONENT EXTREMES
C-----------------------------------------------------------------------
        C2R = ARGR(J)*CR2R - ARGI(J)*CR2I
        C2I = ARGR(J)*CR2I + ARGI(J)*CR2R
        CALL ZAIRY(C2R, C2I, 0, 2, AIR, AII, NAI, IDUM)
        CALL ZAIRY(C2R, C2I, 1, 2, DAIR, DAII, NDAI, IDUM)
        STR = DAIR*BSUMR(J) - DAII*BSUMI(J)
        STI = DAIR*BSUMI(J) + DAII*BSUMR(J)
        PTR = STR*CR2R - STI*CR2I
        PTI = STR*CR2I + STI*CR2R
        STR = PTR + (AIR*ASUMR(J)-AII*ASUMI(J))
        STI = PTI + (AIR*ASUMI(J)+AII*ASUMR(J))
        PTR = STR*PHIR(J) - STI*PHII(J)
        PTI = STR*PHII(J) + STI*PHIR(J)
        S2R = PTR*CSR - PTI*CSI
        S2I = PTR*CSI + PTI*CSR
        STR = DEXP(S1R)*CSSR(KFLAG)
        S1R = STR*DCOS(S1I)
        S1I = STR*DSIN(S1I)
        STR = S2R*S1R - S2I*S1I
        S2I = S1R*S2I + S2R*S1I
        S2R = STR
        IF (KFLAG.NE.1) GO TO 60
        CALL ZUCHK(S2R, S2I, NW, BRY(1), TOL)
        IF (NW.NE.0) GO TO 70
   60   CONTINUE
        IF (YY.LE.0.0D0) S2I = -S2I
        CYR(KDFLG) = S2R
        CYI(KDFLG) = S2I
        YR(I) = S2R*CSRR(KFLAG)
        YI(I) = S2I*CSRR(KFLAG)
        STR = CSI
        CSI = -CSR
        CSR = STR
        IF (KDFLG.EQ.2) GO TO 85
        KDFLG = 2
        GO TO 80
   70   CONTINUE
        IF (RS1.GT.0.0D0) GO TO 320
C-----------------------------------------------------------------------
C     FOR ZR.LT.0.0, THE I FUNCTION TO BE ADDED WILL OVERFLOW
C-----------------------------------------------------------------------
        IF (ZR.LT.0.0D0) GO TO 320
        KDFLG = 1
        YR(I)=ZEROR
        YI(I)=ZEROI
        NZ=NZ+1
        STR = CSI
        CSI =-CSR
        CSR = STR
        IF (I.EQ.1) GO TO 80
        IF ((YR(I-1).EQ.ZEROR).AND.(YI(I-1).EQ.ZEROI)) GO TO 80
        YR(I-1)=ZEROR
        YI(I-1)=ZEROI
        NZ=NZ+1
   80 CONTINUE
      I = N
   85 CONTINUE
      RAZR = 1.0D0/ZABS(COMPLEX(ZRR,ZRI))
      STR = ZRR*RAZR
      STI = -ZRI*RAZR
      RZR = (STR+STR)*RAZR
      RZI = (STI+STI)*RAZR
      CKR = FN*RZR
      CKI = FN*RZI
      IB = I + 1
      IF (N.LT.IB) GO TO 180
C-----------------------------------------------------------------------
C     TEST LAST MEMBER FOR UNDERFLOW AND OVERFLOW. SET SEQUENCE TO ZERO
C     ON UNDERFLOW.
C-----------------------------------------------------------------------
      FN = FNU + DBLE(FLOAT(N-1))
      IPARD = 1
      IF (MR.NE.0) IPARD = 0
      CALL ZUNHJ(ZNR, ZNI, FN, IPARD, TOL, PHIDR, PHIDI, ARGDR, ARGDI,
     * ZET1DR, ZET1DI, ZET2DR, ZET2DI, ASUMDR, ASUMDI, BSUMDR, BSUMDI)
      IF (KODE.EQ.1) GO TO 90
      STR = ZBR + ZET2DR
      STI = ZBI + ZET2DI
      RAST = FN/ZABS(COMPLEX(STR,STI))
      STR = STR*RAST*RAST
      STI = -STI*RAST*RAST
      S1R = ZET1DR - STR
      S1I = ZET1DI - STI
      GO TO 100
   90 CONTINUE
      S1R = ZET1DR - ZET2DR
      S1I = ZET1DI - ZET2DI
  100 CONTINUE
      RS1 = S1R
      IF (DABS(RS1).GT.ELIM) GO TO 105
      IF (DABS(RS1).LT.ALIM) GO TO 120
C----------------------------------------------------------------------------
C     REFINE ESTIMATE AND TEST
C-------------------------------------------------------------------------
      APHI = ZABS(COMPLEX(PHIDR,PHIDI))
      RS1 = RS1+DLOG(APHI)
      IF (DABS(RS1).LT.ELIM) GO TO 120
  105 CONTINUE
      IF (RS1.GT.0.0D0) GO TO 320
C-----------------------------------------------------------------------
C     FOR ZR.LT.0.0, THE I FUNCTION TO BE ADDED WILL OVERFLOW
C-----------------------------------------------------------------------
      IF (ZR.LT.0.0D0) GO TO 320
      NZ = N
      DO 106 I=1,N
        YR(I) = ZEROR
        YI(I) = ZEROI
  106 CONTINUE
      RETURN
  120 CONTINUE
      S1R = CYR(1)
      S1I = CYI(1)
      S2R = CYR(2)
      S2I = CYI(2)
      C1R = CSRR(KFLAG)
      ASCLE = BRY(KFLAG)
      DO 130 I=IB,N
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
        IF (KFLAG.GE.3) GO TO 130
        STR = DABS(C2R)
        STI = DABS(C2I)
        C2M = DMAX1(STR,STI)
        IF (C2M.LE.ASCLE) GO TO 130
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
  130 CONTINUE
  180 CONTINUE
      IF (MR.EQ.0) RETURN
C-----------------------------------------------------------------------
C     ANALYTIC CONTINUATION FOR RE(Z).LT.0.0D0
C-----------------------------------------------------------------------
      NZ = 0
      FMR = DBLE(FLOAT(MR))
      SGN = -DSIGN(PI,FMR)
C-----------------------------------------------------------------------
C     CSPN AND CSGN ARE COEFF OF K AND I FUNCIONS RESP.
C-----------------------------------------------------------------------
      CSGNI = SGN
      IF (YY.LE.0.0D0) CSGNI = -CSGNI
      IFN = INU + N - 1
      ANG = FNF*SGN
      CSPNR = DCOS(ANG)
      CSPNI = DSIN(ANG)
      IF (MOD(IFN,2).EQ.0) GO TO 190
      CSPNR = -CSPNR
      CSPNI = -CSPNI
  190 CONTINUE
C-----------------------------------------------------------------------
C     CS=COEFF OF THE J FUNCTION TO GET THE I FUNCTION. I(FNU,Z) IS
C     COMPUTED FROM EXP(I*FNU*HPI)*J(FNU,-I*Z) WHERE Z IS IN THE FIRST
C     QUADRANT. FOURTH QUADRANT VALUES (YY.LE.0.0E0) ARE COMPUTED BY
C     CONJUGATION SINCE THE I FUNCTION IS REAL ON THE POSITIVE REAL AXIS
C-----------------------------------------------------------------------
      CSR = SAR*CSGNI
      CSI = CAR*CSGNI
      IN = MOD(IFN,4) + 1
      C2R = CIPR(IN)
      C2I = CIPI(IN)
      STR = CSR*C2R + CSI*C2I
      CSI = -CSR*C2I + CSI*C2R
      CSR = STR
      ASC = BRY(1)
      IUF = 0
      KK = N
      KDFLG = 1
      IB = IB - 1
      IC = IB - 1
      DO 290 K=1,N
        FN = FNU + DBLE(FLOAT(KK-1))
C-----------------------------------------------------------------------
C     LOGIC TO SORT OUT CASES WHOSE PARAMETERS WERE SET FOR THE K
C     FUNCTION ABOVE
C-----------------------------------------------------------------------
        IF (N.GT.2) GO TO 175
  172   CONTINUE
        PHIDR = PHIR(J)
        PHIDI = PHII(J)
        ARGDR = ARGR(J)
        ARGDI = ARGI(J)
        ZET1DR = ZETA1R(J)
        ZET1DI = ZETA1I(J)
        ZET2DR = ZETA2R(J)
        ZET2DI = ZETA2I(J)
        ASUMDR = ASUMR(J)
        ASUMDI = ASUMI(J)
        BSUMDR = BSUMR(J)
        BSUMDI = BSUMI(J)
        J = 3 - J
        GO TO 210
  175   CONTINUE
        IF ((KK.EQ.N).AND.(IB.LT.N)) GO TO 210
        IF ((KK.EQ.IB).OR.(KK.EQ.IC)) GO TO 172
        CALL ZUNHJ(ZNR, ZNI, FN, 0, TOL, PHIDR, PHIDI, ARGDR,
     *   ARGDI, ZET1DR, ZET1DI, ZET2DR, ZET2DI, ASUMDR,
     *   ASUMDI, BSUMDR, BSUMDI)
  210   CONTINUE
        IF (KODE.EQ.1) GO TO 220
        STR = ZBR + ZET2DR
        STI = ZBI + ZET2DI
        RAST = FN/ZABS(COMPLEX(STR,STI))
        STR = STR*RAST*RAST
        STI = -STI*RAST*RAST
        S1R = -ZET1DR + STR
        S1I = -ZET1DI + STI
        GO TO 230
  220   CONTINUE
        S1R = -ZET1DR + ZET2DR
        S1I = -ZET1DI + ZET2DI
  230   CONTINUE
C-----------------------------------------------------------------------
C     TEST FOR UNDERFLOW AND OVERFLOW
C-----------------------------------------------------------------------
        RS1 = S1R
        IF (DABS(RS1).GT.ELIM) GO TO 280
        IF (KDFLG.EQ.1) IFLAG = 2
        IF (DABS(RS1).LT.ALIM) GO TO 240
C-----------------------------------------------------------------------
C     REFINE  TEST AND SCALE
C-----------------------------------------------------------------------
        APHI = ZABS(COMPLEX(PHIDR,PHIDI))
        AARG = ZABS(COMPLEX(ARGDR,ARGDI))
        RS1 = RS1 + DLOG(APHI) - 0.25D0*DLOG(AARG) - AIC
        IF (DABS(RS1).GT.ELIM) GO TO 280
        IF (KDFLG.EQ.1) IFLAG = 1
        IF (RS1.LT.0.0D0) GO TO 240
        IF (KDFLG.EQ.1) IFLAG = 3
  240   CONTINUE
        CALL ZAIRY(ARGDR, ARGDI, 0, 2, AIR, AII, NAI, IDUM)
        CALL ZAIRY(ARGDR, ARGDI, 1, 2, DAIR, DAII, NDAI, IDUM)
        STR = DAIR*BSUMDR - DAII*BSUMDI
        STI = DAIR*BSUMDI + DAII*BSUMDR
        STR = STR + (AIR*ASUMDR-AII*ASUMDI)
        STI = STI + (AIR*ASUMDI+AII*ASUMDR)
        PTR = STR*PHIDR - STI*PHIDI
        PTI = STR*PHIDI + STI*PHIDR
        S2R = PTR*CSR - PTI*CSI
        S2I = PTR*CSI + PTI*CSR
        STR = DEXP(S1R)*CSSR(IFLAG)
        S1R = STR*DCOS(S1I)
        S1I = STR*DSIN(S1I)
        STR = S2R*S1R - S2I*S1I
        S2I = S2R*S1I + S2I*S1R
        S2R = STR
        IF (IFLAG.NE.1) GO TO 250
        CALL ZUCHK(S2R, S2I, NW, BRY(1), TOL)
        IF (NW.EQ.0) GO TO 250
        S2R = ZEROR
        S2I = ZEROI
  250   CONTINUE
        IF (YY.LE.0.0D0) S2I = -S2I
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
        IF (KODE.EQ.1) GO TO 270
        CALL ZS1S2(ZRR, ZRI, S1R, S1I, S2R, S2I, NW, ASC, ALIM, IUF)
        NZ = NZ + NW
  270   CONTINUE
        YR(KK) = S1R*CSPNR - S1I*CSPNI + S2R
        YI(KK) = S1R*CSPNI + S1I*CSPNR + S2I
        KK = KK - 1
        CSPNR = -CSPNR
        CSPNI = -CSPNI
        STR = CSI
        CSI = -CSR
        CSR = STR
        IF (C2R.NE.0.0D0 .OR. C2I.NE.0.0D0) GO TO 255
        KDFLG = 1
        GO TO 290
  255   CONTINUE
        IF (KDFLG.EQ.2) GO TO 295
        KDFLG = 2
        GO TO 290
  280   CONTINUE
        IF (RS1.GT.0.0D0) GO TO 320
        S2R = ZEROR
        S2I = ZEROI
        GO TO 250
  290 CONTINUE
      K = N
  295 CONTINUE
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
      DO 310 I=1,IL
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
        IF (KODE.EQ.1) GO TO 300
        CALL ZS1S2(ZRR, ZRI, C1R, C1I, C2R, C2I, NW, ASC, ALIM, IUF)
        NZ = NZ + NW
  300   CONTINUE
        YR(KK) = C1R*CSPNR - C1I*CSPNI + C2R
        YI(KK) = C1R*CSPNI + C1I*CSPNR + C2I
        KK = KK - 1
        CSPNR = -CSPNR
        CSPNI = -CSPNI
        IF (IFLAG.GE.3) GO TO 310
        C2R = DABS(CKR)
        C2I = DABS(CKI)
        C2M = DMAX1(C2R,C2I)
        IF (C2M.LE.ASCLE) GO TO 310
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
  310 CONTINUE
      RETURN
  320 CONTINUE
      NZ = -1
      RETURN
      END
