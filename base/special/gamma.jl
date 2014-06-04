gamma(x::Float64) = nan_dom_err(ccall((:tgamma,libm),  Float64, (Float64,), x), x)
gamma(x::Float32) = nan_dom_err(ccall((:tgammaf,libm),  Float32, (Float32,), x), x)
gamma(x::Real) = gamma(float(x))
@vectorize_1arg Number gamma

function lgamma_r(x::Float64)
    signp = Array(Int32, 1)
    y = ccall((:lgamma_r,libm),  Float64, (Float64, Ptr{Int32}), x, signp)
    return y, signp[1]
end
function lgamma_r(x::Float32)
    signp = Array(Int32, 1)
    y = ccall((:lgammaf_r,libm),  Float32, (Float32, Ptr{Int32}), x, signp)
    return y, signp[1]
end
lgamma_r(x::Real) = lgamma_r(float(x))

lfact(x::Real) = (x<=1 ? zero(float(x)) : lgamma(x+one(x)))
@vectorize_1arg Number lfact

const clg_coeff = [76.18009172947146,
                   -86.50532032941677,
                   24.01409824083091,
                   -1.231739572450155,
                   0.1208650973866179e-2,
                   -0.5395239384953e-5]

function clgamma_lanczos(z)
    const sqrt2pi = 2.5066282746310005
    
    y = x = z
    temp = x + 5.5
    zz = log(temp)
    zz = zz * (x+0.5)
    temp -= zz
    ser = complex(1.000000000190015, 0)
    for j=1:6
        y += 1.0
        zz = clg_coeff[j]/y
        ser += zz
    end
    zz = sqrt2pi*ser / x
    return log(zz) - temp
end

function lgamma(z::Complex)
    if real(z) <= 0.5
        a = clgamma_lanczos(1-z)
        b = log(sinpi(z))
        const logpi = 1.14472988584940017
        z = logpi - b - a
    else
        z = clgamma_lanczos(z)
    end
    complex(real(z), angle_restrict_symm(imag(z)))
end

gamma(z::Complex) = exp(lgamma(z))

# Derivatives of the digamma function
function psifn(x::Float64, n::Int, kode::Int, m::Int)
# Translated from http://www.netlib.org/slatec/src/dpsifn.f
# Note: Underflow handling at 380 in original is skipped
    const nmax = 100
    ans = Array(Float64, m)
#-----------------------------------------------------------------------
#             bernoulli numbers
#-----------------------------------------------------------------------
    const b =                         [1.00000000000000000e+00,
              -5.00000000000000000e-01,1.66666666666666667e-01,
              -3.33333333333333333e-02,2.38095238095238095e-02,
              -3.33333333333333333e-02,7.57575757575757576e-02,
              -2.53113553113553114e-01,1.16666666666666667e+00,
              -7.09215686274509804e+00,5.49711779448621554e+01,
              -5.29124242424242424e+02,6.19212318840579710e+03,
              -8.65802531135531136e+04,1.42551716666666667e+06,
              -2.72982310678160920e+07,6.01580873900642368e+08,
              -1.51163157670921569e+10,4.29614643061166667e+11,
              -1.37116552050883328e+13,4.88332318973593167e+14,
              -1.92965793419400681e+16]
    trm = Array(Float64, 22)
    trmr = Array(Float64, 100)
#***first executable statement  dpsifn
    if x <= 0.0 throw(DomainError()) end
    if n < 0 error("n must be non-negative") end
    if kode < 1 | kode > 2 error("kode must be one or two") end
    if m < 1 error("m must be larger than one") end
    mm = m
    const nx = min(-exponent(realmin(Float64)) + 1, exponent(realmax(Float64)))
    const r1m5 = log10(2)
    const r1m4 = Base.eps(Float64) * 0.5
    const wdtol = max(r1m4, 0.5e-18)
#-----------------------------------------------------------------------
#     elim = approximate exponential over and underflow limit
#-----------------------------------------------------------------------
    const elim = 2.302*(nx*r1m5 - 3.0)
    xln = log(x)
    nn = n + mm - 1
    fn = nn
    t = (fn + 1)*xln
#-----------------------------------------------------------------------
#     overflow and underflow test for small and large x
#-----------------------------------------------------------------------
    if abs(t) > elim
        if t <= 0.0 error("n too large") end
        error("overflow; x too small or n+m-1 too large or both")
    end
    if x < wdtol
        ans[1] = x^(-n - 1)
        if mm != 1
            k = 1
            for i = 2:mm
                ans[k + 1] = ans[k]/x
                k += 1
            end
        end
        if n != 0 return ans end
        if kode == 2 ans[1] = ans[1] + xln end
        return ans
    end
#-----------------------------------------------------------------------
#     compute xmin and the number of terms of the series, fln+1
#-----------------------------------------------------------------------
    rln = r1m5 * precision(x)
    rln = min(rln, 18.06)
    fln = max(rln, 3.0) - 3.0
    yint = 3.50 + 0.40*fln
    slope = 0.21 + fln*(0.0006038*fln + 0.008677)
    xm = yint + slope*fn
    mx = itrunc(xm) + 1
    xmin = mx
    if n != 0
        xm = -2.302*rln - min(0.0,xln)
        arg = xm/n
        arg = min(0.0,arg)
        eps = exp(arg)
        xm = 1.0 - eps
        if abs(arg) < 1.0e-3 xm = -arg end
        fln = x*xm/eps
        xm = xmin - x
        if (xm > 7.0) & (fln < 15.0)
            nn = itrunc(fln) + 1
            np = n + 1
            t1 = (n + 1)*xln
            t = exp(-t1)
            s = t
            den = x
            for i = 1:nn
                den += 1.0
                trm[i] = den^(-np)
                s += trm[i]
            end
            ans[1] = s
            if n == 0
                if kode == 2 ans[1] = s + xln end
            end
            if mm == 1 return ans end
#-----------------------------------------------------------------------
#     generate higher derivatives, j.gt.n
#-----------------------------------------------------------------------
            tol = wdtol/5.0
            for j = 2:mm
                t = t/x
                s = t
                tols = t*tol
                den = x
                for i = 1:nn
                    den += 1.0
                    trm[i] = trm[i]/den
                    s += trm[i]
                    if trm[i] < tols break end
                end
                ans[j] = s
            end
            return ans
        end
    end
    
    xdmy = x
    xdmln = xln
    xinc = 0.0
    if x < xmin
        nx = itrunc(x)
        xinc = xmin - nx
        xdmy = x + xinc
        xdmln = log(xdmy)
    end
#-----------------------------------------------------------------------
#     generate w(n+mm-1,x) by the asymptotic expansion
#-----------------------------------------------------------------------
    t = fn*xdmln
    t1 = xdmln + xdmln
    t2 = t + xdmln
    tk = max(abs(t), abs(t1), abs(t2))
    if tk > elim error("underflow") end
    tss = exp(-t)
    tt = 0.5/xdmy
    t1 = tt
    tst = wdtol*tt
    if nn != 0 t1 = tt + 1.0/fn end
    rxsq = 1.0/(xdmy*xdmy)
    ta = 0.5*rxsq
    t = (fn + 1)*ta
    s = t*b[3]
    if abs(s) >= tst
        tk = 2.0
        for k = 4:22
            t = t*((tk + fn + 1)/(tk + 1.0))*((tk + fn)/(tk + 2.0))*rxsq
            trm[k] = t*b[k]
            if abs(trm[k]) < tst break end
            s += trm[k]
            tk += 2.0
        end
    end
    s = (s + t1)*tss
    while true
        if xinc != 0.0
#-----------------------------------------------------------------------
#     backward recur from xdmy to x
#-----------------------------------------------------------------------
            nx = itrunc(xinc)
            np = nn + 1
            if nx > nmax error("n too large") end
            if nn == 0 break end
            xm = xinc - 1.0
            fx = x + xm
#-----------------------------------------------------------------------
#     this loop should not be changed. fx is accurate when x is small
#-----------------------------------------------------------------------
            for i = 1:nx
                trmr[i] = fx^(-np)
                s += trmr[i]
                xm -= 1.0
                fx = x + xm
            end
        end
        ans[mm] = s
        if fn == 0
            if kode != 2
                ans[1] = s - xdmln
                return ans
            end
            if xdmy == x return ans end
            xq = xdmy/x
            ans[1] = s - log(xq)
            return ans
        end
#-----------------------------------------------------------------------
#     generate lower derivatives, j.lt.n+mm-1
#-----------------------------------------------------------------------
        if mm == 1 return ans end
        for j = 2:mm
            fn -= 1
            tss *= xdmy
            t1 = tt
            if fn != 0 t1 = tt + 1.0/fn end
            t = (fn + 1)*ta
            s = t*b[3]
            if abs(s) >= tst
                tk = 4 + fn
                for k = 4:22 #110
                    trm[k] = trm[k]*(fn + 1)/tk
                    if abs(trm[k]) < tst break end
                    s += trm[k]
                    tk += 2.0
                end
            end
            s = (s + t1)*tss
            if xinc != 0.0
                if fn == 0 break end
                xm = xinc - 1.0
                fx = x + xm
                for i = 1:nx
                    trmr[i] = trmr[i]*fx
                    s += trmr[i]
                    xm -= 1.0
                    fx = x + xm
                end
            end
            mx = mm - j + 1
            ans[mx] = s
            if fn == 0
                if kode != 2
                    ans[1] = s - xdmln
                    return ans
                end
                if xdmy == x return ans end
                xq = xdmy/x
                ans[1] = s - log(xq)
                return ans
            end
        end
        if fn == 0 break end
        return ans
    end
#-----------------------------------------------------------------------
#     recursion for n = 0
#-----------------------------------------------------------------------
    for i = 1:nx
        s += 1.0/(x + nx - i)
    end
    if kode != 2
        ans[1] = s - xdmln
        return ans
    end
    if xdmy == x return ans end
    xq = xdmy/x
    ans[1] = s - log(xq)
    return ans
end
polygamma(k::Int, x::Float64) = (2rem(k,2) - 1)*psifn(x, k, 1, 1)[1]*gamma(k + 1)
polygamma(k::Int, x::Float32) = float32(polygamma(k, float64(x)))
polygamma(k::Int, x::Real) = polygamma(k, float64(x))

# Translation of psi.c from cephes
function digamma(x::Float64)  
    negative = false
    nz = 0.0

    if x <= 0.0
        negative = true
        q = x
        p = floor(q)
        if p == q
            return NaN
        end

        nz = q - p
        if nz != 0.5
            if nz > 0.5
                p += 1.0
                nz = q - p
            end
            nz = pi / tan(pi * nz)
        else
            nz = 0.0
        end
        x = 1.0 - x
    end

    if x <= 10.0 && x == floor(x)
        y = 0.0
        for i = 1:x-1
            y += 1.0 / i
        end
        y -= γ  # γ == -digamma(1) == 0.5772156649015328606065121;

        if negative
            y -= nz
        end
        return y
    end

    w = 0.0
    while x < 10.0
        w += 1.0 / x
        x += 1.0
    end

    if x < 1.0e17
        z = 1.0 / (x*x)
        y = @horner(z, 8.33333333333333333333e-2, -8.33333333333333333333e-3, 3.96825396825396825397e-3,
                       -4.16666666666666666667e-3, 7.57575757575757575758e-3,-2.10927960927960927961e-2,
                       8.33333333333333333333e-2)
        y *= z
    else
        y = 0.0
    end

    y = log(x) - 0.5/x - y - w

    if negative
        y -= nz
    end

    return y
end
digamma(x::Float32) = float32(digamma(float64(x)))
digamma(x::Real) = digamma(float64(x))
@vectorize_1arg Real digamma

trigamma(x::Real) = polygamma(1, x)
@vectorize_1arg Real trigamma

# Inverse digamma function
#
# Implementation of fixed point algorithm described in
#  "Estimating a Dirichlet distribution" by Thomas P. Minka, 2000
function invdigamma(y::Float64)
    # Closed form initial estimates
    if y >= -2.22
        x_old = exp(y) + 0.5
        x_new = x_old
    else
        x_old = -1.0 / (y - digamma(1.0))
        x_new = x_old
    end

    # Fixed point algorithm
    delta = Inf
    iteration = 0
    while delta > 1e-12 && iteration < 25
        iteration += 1
        x_new = x_old - (digamma(x_old) - y) / trigamma(x_old)
        delta = abs(x_new - x_old)
        x_old = x_new
    end

    return x_new
end
invdigamma(x::Float32) = float32(invdigamma(float64(x)))
invdigamma(x::Real) = invdigamma(float64(x))
@vectorize_1arg Real invdigamma

function beta(x::Number, w::Number)
    yx, sx = lgamma_r(x)
    yw, sw = lgamma_r(w)
    yxw, sxw = lgamma_r(x+w)
    return copysign(exp(yx + yw - yxw), sx*sw*sxw)
end
lbeta(x::Number, w::Number) = lgamma(x)+lgamma(w)-lgamma(x+w)
@vectorize_2arg Number beta
@vectorize_2arg Number lbeta

const eta_coeffs =
    [.99999999999999999997,
     -.99999999999999999821,
     .99999999999999994183,
     -.99999999999999875788,
     .99999999999998040668,
     -.99999999999975652196,
     .99999999999751767484,
     -.99999999997864739190,
     .99999999984183784058,
     -.99999999897537734890,
     .99999999412319859549,
     -.99999996986230482845,
     .99999986068828287678,
     -.99999941559419338151,
     .99999776238757525623,
     -.99999214148507363026,
     .99997457616475604912,
     -.99992394671207596228,
     .99978893483826239739,
     -.99945495809777621055,
     .99868681159465798081,
     -.99704078337369034566,
     .99374872693175507536,
     -.98759401271422391785,
     .97682326283354439220,
     -.95915923302922997013,
     .93198380256105393618,
     -.89273040299591077603,
     .83945793215750220154,
     -.77148960729470505477,
     .68992761745934847866,
     -.59784149990330073143,
     .50000000000000000000,
     -.40215850009669926857,
     .31007238254065152134,
     -.22851039270529494523,
     .16054206784249779846,
     -.10726959700408922397,
     .68016197438946063823e-1,
     -.40840766970770029873e-1,
     .23176737166455607805e-1,
     -.12405987285776082154e-1,
     .62512730682449246388e-2,
     -.29592166263096543401e-2,
     .13131884053420191908e-2,
     -.54504190222378945440e-3,
     .21106516173760261250e-3,
     -.76053287924037718971e-4,
     .25423835243950883896e-4,
     -.78585149263697370338e-5,
     .22376124247437700378e-5,
     -.58440580661848562719e-6,
     .13931171712321674741e-6,
     -.30137695171547022183e-7,
     .58768014045093054654e-8,
     -.10246226511017621219e-8,
     .15816215942184366772e-9,
     -.21352608103961806529e-10,
     .24823251635643084345e-11,
     -.24347803504257137241e-12,
     .19593322190397666205e-13,
     -.12421162189080181548e-14,
     .58167446553847312884e-16,
     -.17889335846010823161e-17,
     .27105054312137610850e-19]

function eta(z::Union(Float64,Complex128))
    if z == 0
        return oftype(z, 0.5)
    end
    re, im = reim(z)
    if im==0 && re < 0 && re==round(re/2)*2
        return zero(z)
    end
    reflect = false
    if re < 0.5
        z = 1-z
        reflect = true
    end
    s = zero(z)
    for n = length(eta_coeffs):-1:1
        c = eta_coeffs[n]
        p = n^-z
        s += c * p
    end
    if reflect
        z2 = 2.0^z
        b = 2.0 - (2.0*z2)
        f = z2 - 2
        piz = pi^z
        
        b = b/f/piz
        
        return s * gamma(z) * b * cospi(z/2)
    end
    return s
end

eta(x::Integer) = eta(float64(x))
eta(x::Real)    = oftype(float(x),eta(float64(x)))
eta(z::Complex) = oftype(float(z),eta(complex128(z)))
@vectorize_1arg Number eta

function zeta(z::Number)
    zz = 2^z
    eta(z) * zz/(zz-2)
end
@vectorize_1arg Number zeta
