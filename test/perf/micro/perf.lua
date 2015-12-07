
if jit.arch ~= 'x64' then
    print('WARNING: please use BIT=64 for optimal OpenBLAS performance')
end

local ffi     = require 'ffi'
local bit     = require 'bit'
local time    = require 'time'
local alg     = require 'sci.alg'
local prng    = require 'sci.prng'
local stat    = require 'sci.stat'
local dist    = require 'sci.dist'
local complex = require 'sci.complex'

local min, sqrt, random, abs = math.min, math.sqrt, math.random, math.abs
local cabs = complex.abs
local rshift = bit.rshift
local format = string.format
local nowutc = time.nowutc
local rng = prng.std()
local vec, mat, join = alg.vec, alg.mat, alg.join
local sum, trace = alg.sum, alg.trace
local var, mean = stat.var, stat.mean

--------------------------------------------------------------------------------
local function elapsed(f)
    local t0 = nowutc()
    local val1, val2 = f()
    local t1 = nowutc()
    return (t1 - t0):tomilliseconds(), val1, val2
end

local function timeit(f, name, check)
    local t, k, s = 1/0, 0, nowutc()
    while true do
        k = k + 1
        local tx, val1, val2 = elapsed(f)
        t = min(t, tx)
        if check then 
            check(val1, val2)
        end
        if k > 5 and (nowutc() - s):toseconds() >= 2 then break end
    end
    io.write(format('lua,%s,%g\n', name, t))
end

--------------------------------------------------------------------------------
local function fib(n)
    if n < 2 then
        return n
    else
        return fib(n-1) + fib(n-2)
    end
end

timeit(function() return fib(20) end, 'fib', function(x) assert(x == 6765) end)

local function parseint()
    local lmt = 2^32 - 1
    local n, m
    for i = 1, 1000 do
        n = random(lmt) -- Between 0 and 2^32 - 1, i.e. uint32_t.
        local s = format('0x%x', tonumber(n))
        m = tonumber(s)
    end
    assert(n == m) -- Done here to be even with Julia benchmark.
    return n, m
end

timeit(parseint, 'parse_int')

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
local function mandelperf()
    local a = mat(26, 21)
    for r=1,26 do -- Lua's for i=l,u,c doesn't match Julia's for i=l:c:u.
        for c=1,21 do
            local re, im = (r - 21)*0.1, (c - 11)*0.1
            a[{r, c}] = mandel(re + im*1i)
        end
    end
    return a
end

timeit(mandelperf, 'mandel', function(a) assert(sum(a) == 14791) end)

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
    local v = ffi.new('double[?]', n+1)
    for i=1,n do
        v[i] = rng:sample()
    end
    return qsort(v, 1, n)
end

timeit(sortperf, 'quicksort', function(x)
    for i=2,5000 do
        assert(x[i-1] <= x[i])
    end
end
)

local function pisum()
    local s
    for j = 1, 500 do
        s = 0
        for k = 1, 10000 do
            s = s + 1 / (k*k)
        end
    end
    return s
end

timeit(pisum, 'pi_sum', function(x) 
    assert(abs(x - 1.644834071848065) < 1e-12)
end)

local function rand(r, c)
    local x = mat(r, c)
    for i=1,#x do 
        x[i] = rng:sample()
    end
    return x
end

local function randn(r, c)
    local x = mat(r, c)
    for i=1,#x do 
        x[i] = dist.normal(0, 1):sample(rng) 
    end
    return x
end

local function randmatstat(t)
    local n = 5
    local v, w = vec(t), vec(t)
    for i=1,t do
        local a, b, c, d = randn(n, n), randn(n, n), randn(n, n), randn(n, n)
        local P = join(a..b..c..d)
        local Q = join(a..b, c..d)
        v[i] = trace((P[]`**P[])^^4)
        w[i] = trace((Q[]`**Q[])^^4)
    end
    return sqrt(var(v))/mean(v), sqrt(var(w))/mean(w)
end

timeit(function() return randmatstat(1000) end, 'rand_mat_stat', 
    function(s1, s2)
        assert( 0.5 < s1 and s1 < 1.0 and 0.5 < s2 and s2 < 1.0 )
    end)

local function randmatmult(n)
    local a, b = rand(n, n), rand(n, n)
    return a[]**b[]
end

timeit(function() return randmatmult(1000) end, 'rand_mat_mul')

if jit.os ~= 'Windows' then
    local function printfd(n)
        local f = io.open('/dev/null','w')
        for i = 1, n do
            f:write(format('%d %d\n', i, i+1))
        end
        f:close()
    end

    timeit(function() return printfd(100000) end, 'printfd')
end
