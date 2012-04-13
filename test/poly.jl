# assert file to test polynomial
load("extras/poly.jl")


pNULL = Polynomial(Float32[])
p0 = Polynomial([0])
p1 = Polynomial([0,0,0,0,0,0,0,0,0,0,0,0,0,1])
p2 = Polynomial([0,0,1,1])
p3 = Polynomial([0,0,0,0,1,2,1])
p4 = Polynomial([0,1,3,3,1])
p5 = Polynomial([0,0,0,0,0,0,0,0,0,0,0,0,0,1,4,6,4,1])
pN = Polynomial([0,24,15,87,3,276])
p1000 = Polynomial(randn(1000))

@assert length(pNULL) == 0
@assert length(p1000) == 1000
print_to_string(show, p1000)
print_to_string(show, pNULL)

@assert p3 == Polynomial([1,2,1])
@assert pN*10 == Polynomial([240, 150, 870, 30, 2760])
@assert pN/10 == Polynomial([2.4, 1.5, 8.7, 0.3, 27.6])
@assert 10*pNULL + pN == pN
@assert 10*p0 + pN == pN
@assert p5 + 2*p1 == Polynomial([1,4,6,4,3])
@assert 10*pNULL - pN == -pN
@assert p0 - pN == -pN
@assert p5 - 2*p1 == Polynomial([1,4,6,4,-1])
@assert p2*p2*p2 == p4
@assert p2^4 == p5
@assert pNULL^3 == pNULL
@assert pNULL*pNULL == pNULL

@assert polyval(pN, -.125) == 276.9609375
@assert polyval(pNULL, 10) == 0
@assert polyval(p0, -10) == 0
@assert polydir(polyint(pN)) == pN
@assert polyint(pNULL,1) == p1
@assert polydir(p3) == Polynomial([2,2])
@assert polydir(p1) == polydir(p0) == polydir(pNULL) == pNULL

@assert poly([-1,-1]) == p3
@assert length(roots(p0) == roots(p1) == roots(pNULL)) == 0
@assert all(roots(p2) == [-1])
a_roots = copy(pN.a)
@assert all(abs(sort(roots(poly(a_roots))) - sort(a_roots)) < 1e6)
@assert length(roots(p5)) == 4
@assert all(roots(pNULL) == [])

