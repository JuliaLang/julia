module SLATEC

export DRD, DRF

const D1MACH1 = realmin(Float64)
const D1MACH2 = realmax(Float64)
const D1MACH3 = eps(Float64)/2
const D1MACH4 = eps(Float64)
const D1MACH5 = log10(2.)

#***BEGIN PROLOGUE  DRF
#***PURPOSE  Compute the incomplete or complete elliptic integral of the
#            1st kind.  For X, Y, and Z non-negative and at most one of
#            them zero, RF(X,Y,Z) = Integral from zero to infinity of
#                                -1/2     -1/2     -1/2
#                      (1/2)(t+X)    (t+Y)    (t+Z)    dt.
#            If X, Y or Z is zero, the integral is complete.
#***LIBRARY   SLATEC
#***CATEGORY  C14
#***TYPE      DOUBLE PRECISION (RF-S, DRF-D)
#***KEYWORDS  COMPLETE ELLIPTIC INTEGRAL, DUPLICATION THEOREM,
#             INCOMPLETE ELLIPTIC INTEGRAL, INTEGRAL OF THE FIRST KIND,
#             TAYLOR SERIES
#***AUTHOR  Carlson, B. C.
#             Ames Laboratory-DOE
#             Iowa State University
#             Ames, IA  50011
#           Notis, E. M.
#             Ames Laboratory-DOE
#             Iowa State University
#             Ames, IA  50011
#           Pexton, R. L.
#             Lawrence Livermore National Laboratory
#             Livermore, CA  94550

function DRF(X::Float64, Y::Float64, Z::Float64)

    const ERRTOL = (4.0*D1MACH3)^(1.0/6.0)
    const LOLIM  = 5.0 * D1MACH1
    const UPLIM  = D1MACH2/5.0
    const C1 = 1.0/24.0
    const C2 = 3.0/44.0
    const C3 = 1.0/14.0

    ans = 0.0
    if min(X,Y,Z) < 0.0
        return ans, 1
    end

    if max(X,Y,Z) > UPLIM
        return ans, 3
    end

    if min(X+Y,X+Z,Y+Z) < LOLIM
        return ans, 2
    end

    XN = X
    YN = Y
    ZN = Z
    MU = 0.
    XNDEV = 0.
    YNDEV = 0.
    ZNDEV = 0.

    while true
        MU = (XN+YN+ZN)/3.0
        XNDEV = 2.0 - (MU+XN)/MU
        YNDEV = 2.0 - (MU+YN)/MU
        ZNDEV = 2.0 - (MU+ZN)/MU
        EPSLON = max(abs(XNDEV),abs(YNDEV),abs(ZNDEV))
        if (EPSLON < ERRTOL) break end
        XNROOT = sqrt(XN)
        YNROOT = sqrt(YN)
        ZNROOT = sqrt(ZN)
        LAMDA = XNROOT*(YNROOT+ZNROOT) + YNROOT*ZNROOT
        XN = (XN+LAMDA)*0.250
        YN = (YN+LAMDA)*0.250
        ZN = (ZN+LAMDA)*0.250
    end

    E2 = XNDEV*YNDEV - ZNDEV*ZNDEV
    E3 = XNDEV*YNDEV*ZNDEV
    S  = 1.0 + (C1*E2-0.10-C2*E3)*E2 + C3*E3
    ans = S/sqrt(MU)

    return ans, 0
end

#***BEGIN PROLOGUE  DRD
#***PURPOSE  Compute the incomplete or complete elliptic integral of
#            the 2nd kind. For X and Y nonnegative, X+Y and Z positive,
#            DRD(X,Y,Z) = Integral from zero to infinity of
#                                -1/2     -1/2     -3/2
#                      (3/2)(t+X)    (t+Y)    (t+Z)    dt.
#            If X or Y is zero, the integral is complete.
#***LIBRARY   SLATEC
#***CATEGORY  C14
#***TYPE      DOUBLE PRECISION (RD-S, DRD-D)
#***KEYWORDS  COMPLETE ELLIPTIC INTEGRAL, DUPLICATION THEOREM,
#             INCOMPLETE ELLIPTIC INTEGRAL, INTEGRAL OF THE SECOND KIND,
#             TAYLOR SERIES
#***AUTHOR  Carlson, B. C.
#             Ames Laboratory-DOE
#             Iowa State University
#             Ames, IA  50011
#           Notis, E. M.
#             Ames Laboratory-DOE
#             Iowa State University
#             Ames, IA  50011
#           Pexton, R. L.
#             Lawrence Livermore National Laboratory
#             Livermore, CA  94550

function DRD(X::Float64, Y::Float64, Z::Float64)

    const ERRTOL = (D1MACH3/3.0)^(1.0/6.0)
    const LOLIM  = 2.0/(D1MACH2)^(2.0/3.0)
    const TUPLIM = D1MACH1^(1.0E0/3.0E0)
    const TUPLIM = (0.10*ERRTOL)^(1.0E0/3.0E0)/TUPLIM
    const UPLIM  = TUPLIM^2.0
    const C1 = 3.0/14.0
    const C2 = 1.0/6.0
    const C3 = 9.0/22.0
    const C4 = 3.0/26.0

    ans = 0.0
    if min(X,Y) < 0.0
        return ans, 1
    end

    if max(X,Y,Z) > UPLIM
        return ans, 3
    end

    if min(X+Y,Z) < LOLIM
        return ans, 2
    end

    XN = X
    YN = Y
    ZN = Z
    SIGMA = 0.0
    POWER4 = 1.0
    MU = 0.
    XNDEV = 0.
    YNDEV = 0.
    ZNDEV = 0.

    while true
        MU = (XN+YN+3.0*ZN)*0.20
        XNDEV = (MU-XN)/MU
        YNDEV = (MU-YN)/MU
        ZNDEV = (MU-ZN)/MU
        EPSLON = max(abs(XNDEV), abs(YNDEV), abs(ZNDEV))
        if (EPSLON < ERRTOL) break end
        XNROOT = sqrt(XN)
        YNROOT = sqrt(YN)
        ZNROOT = sqrt(ZN)
        LAMDA = XNROOT*(YNROOT+ZNROOT) + YNROOT*ZNROOT
        SIGMA = SIGMA + POWER4/(ZNROOT*(ZN+LAMDA))
        POWER4 = POWER4*0.250
        XN = (XN+LAMDA)*0.250
        YN = (YN+LAMDA)*0.250
        ZN = (ZN+LAMDA)*0.250
    end

    EA = XNDEV*YNDEV
    EB = ZNDEV*ZNDEV
    EC = EA - EB
    ED = EA - 6.0*EB
    EF = ED + EC + EC
    S1 = ED*(-C1+0.250*C3*ED-1.50*C4*ZNDEV*EF)
    S2 = ZNDEV*(C2*EF+ZNDEV*(-C3*EC+ZNDEV*C4*EA))
    ans = 3.0*SIGMA + POWER4*(1.0+S1+S2)/(MU*sqrt(MU))

    return ans, 0
end

end # module
