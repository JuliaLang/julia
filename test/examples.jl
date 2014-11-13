dir = joinpath(JULIA_HOME, Base.DOCDIR, "examples")

include(joinpath(dir, "bubblesort.jl"))
a = rand(1:100,100)
@test issorted(sort!(a;alg=BubbleSort))

include(joinpath(dir, "enum.jl"))
@enum TestEnum TestEnum1 TestEnum2 TestEnum3
@test [TestEnum1.n,TestEnum2.n,TestEnum3.n] == [0,1,2]

include(joinpath(dir, "lru.jl"))
include(joinpath(dir, "lru_test.jl"))

include(joinpath(dir, "modint.jl"))
b = ModInts.ModInt{10}(2)
c = ModInts.ModInt{10}(4)
@test b + c == ModInts.ModInt{10}(6)
@test c - b == ModInts.ModInt{10}(2)
x = ModInts.ModInt{256}(13)
y = inv(x)
@test y == ModInts.ModInt{256}(197)
@test x*y == ModInts.ModInt{256}(1)
@test_throws ErrorException inv(ModInts.ModInt{8}(4))

include(joinpath(dir, "ndgrid.jl"))
r = repmat(1:10,1,10)
r1, r2 = ndgrid(1:10, 1:10)
@test r1 == r
@test r2 == r'
r3, r4 = meshgrid(1:10,1:10)
@test r3 == r'
@test r4 == r

include(joinpath(dir, "queens.jl"))
@test solve(8, 8, 1) == Array{Int,1}[[1,1]]
@test solve(8, 8, 7) == Array{Int,1}[[1,1],[2,3],[3,5],[4,2],[5,8],[7,4],[8,7]]

# At least make sure code loads
include(joinpath(dir, "plife.jl"))

include(joinpath(dir, "preduce.jl"))

include(joinpath(dir, "wordcount.jl"))
