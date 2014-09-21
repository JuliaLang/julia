
local ffi = require 'ffi'
local bit = require 'bit'
local gsl = require 'gsl'

local min, max, abs, sqrt, random, floor = math.min, math.max, math.abs, math.sqrt, math.random, math.floor
local cabs = complex.abs
local rshift = bit.rshift
local format = string.format

local gettime
do
    ffi.cdef[[
         struct timeval {
            long tv_sec;
            long tv_usec;
         };

         int gettimeofday(struct timeval * tp, void *tzp);
       ]]

    local tv = ffi.new('struct timeval[1]')

    gettime = function()
      ffi.C.gettimeofday(tv, nil)
      return tv[0].tv_sec, tv[0].tv_usec
    end
end

-- return the elapsed time in ms
local function elapsed(f)
    local s0, us0 = gettime()
    f()
    local s1, us1 = gettime()
    return tonumber(s1 - s0) * 1000 + tonumber(us1 - us0) / 1000
end

local function timeit(f, name)
    local t = nil
    for k = 1, 5 do
        local tx = elapsed(f)
        t = t and min(t, tx) or tx
    end
    print(format("lua,%s,%g", name, t))
end

local function fib(n)
    if n < 2 then
        return n
    else
        return fib(n-1) + fib(n-2)
    end
end

assert(fib(20) == 6765)
timeit(|| fib(20), "fib")

local function parseint()
    local r = rng.new('rand')
    local lmt = 2^32 - 1
    local n, m
    for i = 1, 1000 do
        n = r:getint(lmt)
        local s = format('0x%x', tonumber(n))
        m = tonumber(s)
    end
    assert(m == n)
    return n
end

timeit(parseint, "parse_int")

local function mandel(z)
    local c = z
    local maxiter = 80
    for n = 1, maxiter do
        if cabs(z) > 2 then
            return n-1
        end
        z = z*z + c
    end
    return maxiter
end
function mandelperf()
    local a, re, im, z
    a = ffi.new("double[?]", 546)
    r = 0
    for r = -20, 5 do
        re = r*0.1
        for i=-10, 10 do
            im = i*0.1
            a[r*21+i+430] = mandel(re + 1i * im)
        end
    end
    return a
end

do
    local a = mandelperf()
    local sum = 0
    for i = 0, 545 do sum = sum + a[i] end
    assert(sum == 14791)
end

timeit(mandelperf, "mandel")

local function qsort(a, lo, hi)
    local i, j = lo, hi
    while i < hi do
        local pivot = a[rshift(lo+hi, 1)]
        while i <= j do
            while a[i] < pivot do i = i+1 end
            while a[j] > pivot do j = j-1 end
            if i <= j then
                a[i], a[j] = a[j], a[i]
                i, j = i+1, j-1
            end
        end
        if lo < j then qsort(a, lo, j) end
        lo, j = i, hi
    end
    return a
end

local function sortperf()
    local n = 5000
    local r = rng.new('rand')
    local v = iter.ilist(|| r:get(), n)
    qsort(v, 1, n)
end



local function pisum()
    local sum
    for j = 1, 500 do
        sum = 0
        for k = 1, 10000 do
            sum = sum + 1 / (k*k)
        end
    end
    return sum
end

local function stat(v)
    local p, q = 0, 0
    local n = #v
    for k = 1, n do
        local x = v[k]
        p = p + x
        q = q + x*x
    end
    return sqrt((n*(n*q-p*p))/((n-1)*p*p))
end

local function randmatstat(t)
    local n = 5
    local A = iter.ilist(|| matrix.alloc(n, n), 4)

    local P = matrix.alloc(n, 4*n)
    local Q = matrix.alloc(2*n, 2*n)

    local PtP1 = matrix.alloc(4*n, 4*n)
    local PtP2 = matrix.alloc(4*n, 4*n)
    local QtQ1 = matrix.alloc(2*n, 2*n)
    local QtQ2 = matrix.alloc(2*n, 2*n)

    local get, set = A[1].get, A[1].set

    local r = rng.new('rand')
    local randn = || rnd.gaussian(r, 1)

    local function hstackf(i, j)
        local k, r = math.divmod(j - 1, n)
        return get(A[k + 1], i, r + 1)
    end

    local function vstackf(i, j)
        local ik, ir = math.divmod(i - 1, n)
        local jk, jr = math.divmod(j - 1, n)
        return get(A[2*ik + jk + 1], ir + 1, jr + 1)
    end

    local Tr, NT = gsl.CblasTrans, gsl.CblasNoTrans

    local v, w = {}, {}

    for i = 1, t do
        matrix.fset(A[1], randn)
        matrix.fset(A[2], randn)
        matrix.fset(A[3], randn)
        matrix.fset(A[4], randn)

        matrix.fset(P, hstackf)
        matrix.fset(Q, vstackf)

        gsl.gsl_blas_dgemm(Tr, NT, 1.0, P, P, 0.0, PtP1)
        gsl.gsl_blas_dgemm(NT, NT, 1.0, PtP1, PtP1, 0.0, PtP2)
        gsl.gsl_blas_dgemm(NT, NT, 1.0, PtP2, PtP2, 0.0, PtP1)

        local vi = 0
        for j = 1, n do vi = vi + get(PtP1, j, j) end
        v[i] = vi

        gsl.gsl_blas_dgemm(Tr, NT, 1.0, Q, Q, 0.0, QtQ1)
        gsl.gsl_blas_dgemm(NT, NT, 1.0, QtQ1, QtQ1, 0.0, QtQ2)
        gsl.gsl_blas_dgemm(NT, NT, 1.0, QtQ2, QtQ2, 0.0, QtQ1)

        local wi = 0
        for j = 1, 2*n do wi = wi + get(QtQ1, j, j) end
        w[i] = wi
    end

    return stat(v), stat(w)
end

do
    local s1, s2 = randmatstat(1000)
    assert( 0.5 < s1 and s1 < 1.0
        and 0.5 < s2 and s2 < 1.0 )
end

local function randmatmult(n)
    local r = rng.new('rand')
    --local rand = || r:get()
    local rand = random
    local a = matrix.new(n, n, rand)
    local b = matrix.new(n, n, rand)
    return a*b
end

local function printfd(n)
    local f = io.open("/dev/null","w")
    for i = 1, n do
        f:write(format("%d %d\n", i, i+1))
    end
    f:close()
end


timeit(sortperf, "quicksort")
timeit(pisum, "pi_sum")
timeit(|| randmatstat(1000), "rand_mat_stat")
timeit(|| randmatmult(1000), "rand_mat_mul")
-- timeit(|| printfd(100000), "printfd")
