# e_jnf.c -- float version of e_jn.c.
# Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.

# ====================================================
# Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
#
# Developed at SunPro, a Sun Microsystems, Inc. business.
# Permission to use, copy, modify, and distribute this
# software is freely granted, provided that this notice
# is preserved.
# ====================================================

module JNF

import Base.Math: libm, J0F, J1F

log_unchecked(x::Float32) = ccall((:logf, libm), Float32, (Float32,), x)

function ieee754_jnf(n::Int32, x::Float32)
    # J(-n,x) = (-1)^n * J(n, x), J(n, -x) = (-1)^n * J(n, x)
    # Thus, J(-n,x) = J(n,-x)
    hx = reinterpret(UInt32, x)
    ix = 0x7fffffff & hx
    # if J(n, NaN) is NaN
    ix > 0x7f800000 && return x + x
    if n < 0
        n = -n
        x = -x
        hx = hx $ 0x80000000
    end
    n == 0 && return J0F.ieee754_j0f(x)
    n == 1 && return J1F.ieee754_j1f(x)
    sgn = (n & 1) & (hx >> 31) # even n -- 0, odd n -- sign(x)
    x = abs(x)
    if ix == 0 || ix >= 0x7f800000 # if x is 0 or inf
        b = 0f0
    elseif n <= x
        # Safe to use J(n + 1, x) = 2n / x * J(n, x) - J(n - 1, x)
        a = J0F.ieee754_j0f(x)
        b = J1F.ieee754_j1f(x)
        for i in Int32(1):(n - Int32(1))
            a, b = b, b * ((i + i) / x) - a # avoid underflow
        end
    else
        if ix < 0x30800000 # x < 2^-29
            # x is tiny, return the first Taylor expansion of J(n, x)
            # J(n, x) = 1 / n! * (x / 2)^n  - ...
            if (n > 33) # underflow
                b = 0f0
            else
                temp = x * 0.5f0
                b = temp
                a = 1f0
                for i in Int32(2):n
                    a *= i # a = n!
                    b *= temp # b = (x / 2)^n
                end
                b = b / a
            end
        else
            # use backward recurrence */
            #                      x      x^2      x^2
            #  J(n,x)/J(n-1,x) =  ----   ------   ------   .....
            #                      2n  - 2(n+1) - 2(n+2)
            #
            #                      1      1        1
            #  (for large x)   =  ----  ------   ------   .....
            #                      2n   2(n+1)   2(n+2)
            #                      -- - ------ - ------ -
            #                       x     x         x
            #
            # Let w = 2n/x and h=2/x, then the above quotient
            # is equal to the continued fraction:
            #                  1
            #      = -----------------------
            #                     1
            #         w - -----------------
            #                        1
            #              w+h - ---------
            #                     w+2h - ...
            #
            # To determine how many terms needed, let
            # Q(0) = w, Q(1) = w(w + h) - 1,
            # Q(k) = (w + k * h) * Q(k - 1) - Q(k - 2),
            # When Q(k) > 1e4        good for single
            # When Q(k) > 1e9        good for double
            # When Q(k) > 1e17       good for quadruple
            # determine k
            w = (n + n) / x
            h = 2f0 / x
            q0 = w
            z = w + h
            q1 = w * z - 1f0
            k::Int32 = 1
            while q1 < 1f9
                k += Int32(1)
                z += h
                q1, q0 = z * q1 - q0, q1
            end
            m = n + n
            t = 0f0
            for i in (Int32(2) * (n + k)):-Int32(2):m
                 t = 1f0 / (i / x - t)
            end
            a = t
            b = 1f0
            # estimate log((2 / x)^n * n!) = n * log(2 / x) + n * ln(n)
            # Hence, if n * (log(2n / x)) > ...
            # single 8.8722839355e+01
            # double 7.09782712893383973096e+02
            # long double 1.1356523406294143949491931077970765006170e+04
            # then recurrent value may overflow and the result is
            # likely underflow to zero
            v = 2f0 / x
            tmp = n * log_unchecked(abs(v * n))
            di = Float32(2n - 2)
            if tmp < 8.8721679688f+01
                for i in (n - Int32(1)):-Int32(1):Int32(1)
                    temp = b
                    b *= di
                    b = b / x - a
                    a = temp
                    di -= 2f0
                end
            else
                for i in (n - Int32(1)):-Int32(1):Int32(1)
                    temp = b
                    b *= di
                    b = b/x - a
                    a = temp
                    di -= 2f0
                    # scale b to avoid spurious overflow
                    if b > 1f10
                        a /= b
                        t /= b
                        b = 1f0
                    end
                end
            end
            z = J0F.ieee754_j0f(x)
            w = J1F.ieee754_j1f(x)
            if abs(z) >= abs(w)
                b = t * z / b
            else
                b = t * w / a
            end
        end
    end
    if sgn == 1
        return -b
    else
        return b
    end
end

function ieee754_ynf(n::Int32, x::Float32)
    hx = reinterpret(UInt32, x)
    ix = 0x7fffffff & hx
    ix > 0x7f800000 && return x + x
    ix == 0 && return -Inf32
    (hx & 0x80000000 != 0) && return NaN32
    sign::Int32 = 1
    if n < 0
        n = -n
        sign = 1 - ((n & 1) << 1)
    end
    n == 0 && return J0F.ieee754_y0f(x)
    n == 1 && return sign * J1F.ieee754_y1f(x)
    ix == 0x7f800000 && return 0f0
    a = J0F.ieee754_y0f(x)
    b = J1F.ieee754_y1f(x)
    # quit if b is -Inf
    for i in Int32(1):(n - Int32(1))
        b === -Inf32 && break
        temp = b
        b = ((i + i) / x) * b - a
        a = temp
    end
    sign > 0 ? b : -b
end
end
