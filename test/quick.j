# quick tests to do on every build as a sanity check

# basic booleans
@assert true
@assert !false

# the bool operator
@assert bool(false) == false
@assert bool(true) == true

load("core.j")

# basic arithmetic and indexing
@assert 2+3 == 5
@assert 2.+3. == 5.
@assert 2*3 == 6
@assert 2.*3 == 6
@assert 2. * 3. == 6.
@assert min(1.0,1) == 1

a = ones(4)
b = a+a
@assert b[1]==2. && b[2]==2. && b[3]==2. && b[4]==2.

@assert length((1,)) == 1
@assert length((1,2)) == 2

@assert 1+[1,2,3] == [2,3,4]
@assert [1,2,3]*5 == [5,10,15]

a = [1 2; 3 4]
@assert a' == [1 3; 2 4]

a = rand()
b = rand()
@assert a != b

@assert sign(-1) == -1
@assert sign(0) == 0
@assert isequal(sign(-NaN), NaN)
@assert signbit(-NaN) == -1

@assert isnan(NaN)   == true
@assert isnan(1//2)  == false

@assert isinf(-1.0)  == false
@assert isinf(Inf)   == true
@assert isinf(-Inf)  == true
@assert isinf(NaN)   == false

@assert isfinite(Inf)   == false
@assert isfinite(-Inf)  == false
@assert isfinite(NaN)   == false
@assert isfinite(1//2)  == true

@assert sqrt(2) == 1.4142135623730951

@assert 1+1.5 == 2.5
@assert is(typeof(convert(ComplexPair{Int16},1)),ComplexPair{Int16})
@assert ComplexPair(1,2)+1 == ComplexPair(2,2)
@assert 0.7 < real(sqrt(ComplexPair(0,1))) < 0.707107

@assert parse_int(Int32,"z",36) == 35
@assert parse_bin("0") == 0
@assert parse_oct("7") == 7
@assert parse_int(Int64,"3830974272",10) == 3830974272
@assert parse_hex("0BADF00D") == 195948557

# ranges
@assert size(10:1:0) == (0,)
@assert length(1:.2:2) == 6
@assert length(Range(2.,.2,1.)) == 0

# Arrays 
@assert [ones(2,2)  2*ones(2,1)] == [1 1 2; 1 1 2]

# blas, lapack
n = 10
a = rand(n,n)
(l,u,p) = lu(a)
@assert sum(l[p,:]*u - a) < 1e-8

# arpack
if WORD_SIZE==64
    # TODO: hangs on 32-bit
    (d,v) = eigs(a, 3)
    @assert abs(sum(a*v[:,1]-d[1]*v[:,1])) < 1e-8
end

# hash table
h = HashTable()
for i=1:100
    h[i] = i+1
end
for i=1:100
    @assert (h[i] == i+1)
end
h[77] = 100
@assert h[77]==100

# fft
a = rand(8) + im*rand(8)
@assert norm((1/length(a))*ifft(fft(a)) - a) < 1e-8

# sparse
@assert speye(10) * speye(10) == speye(10)
@assert speye(10) \ ones(10) == ones(10)
