test_mod = Module()

@test JuliaLowering.include_string(test_mod, """
collect(x^2 for x in 1:3)
""") == [1,4,9]

@test JuliaLowering.include_string(test_mod, """
collect(x for x in 1:5 if isodd(x))
""") == [1,3,5]

@test JuliaLowering.include_string(test_mod, """
collect((y,x) for (x,y) in zip(1:3, 2:4) if y != 3)
""") == [(2,1), (4,3)]

# product iterator
@test JuliaLowering.include_string(test_mod, """
collect((x,y) for x in 1:3, y in 1:2)
""") == [(1,1)  (1,2)
         (2,1)  (2,2)
         (3,1)  (3,2)]

# flattened iterator
@test JuliaLowering.include_string(test_mod, """
collect((x,y,z) for x in 1:3, y in 4:5 for z in 6:7)
""") == [
    (1,4,6)
    (1,4,7)
    (2,4,6)
    (2,4,7)
    (3,4,6)
    (3,4,7)
    (1,5,6)
    (1,5,7)
    (2,5,6)
    (2,5,7)
    (3,5,6)
    (3,5,7)
]

@testset "duplicate vars" for jeval_s in [
    s->JuliaLowering.include_string(test_mod, "[ $s ]"),
    s->JuliaLowering.include_string(test_mod, "collect( $s )")]

    # Duplicate iteration variables - body sees only innermost
    @test jeval_s("x for x in 1:3 for x in 10:14") ==
        [10,11,12,13,14,10,11,12,13,14,10,11,12,13,14,]
    # Duplicate in same "for"
    @test jeval_s("x for x = 1:3, x = 10:14") ==
        [10 11 12 13 14 ; 10 11 12 13 14 ; 10 11 12 13 14 ; ]

    # Same as above, but with filter
    @test jeval_s("x for x in 1:3 for x in 10:14 if iseven(x)") ==
        [10,12,14,10,12,14,10,12,14,]
    @test jeval_s("x for x = 1:3, x = 10:14 if iseven(x)") ==
        [10,10,10,12,12,12,14,14,14]

    @test jeval_s("x for x in 1:3 if iseven(x) for x in 10:14") ==
        [10,11,12,13,14,]
end

# Outer iteration variables are protected from mutation
@test JuliaLowering.include_string(test_mod, """
collect((z=y; y=100; z) for y in 1:3 for x in 1:2)
""") == [1, 1, 2, 2, 3, 3]

# Simple typed comprehension lowered to for loops
@test JuliaLowering.include_string(test_mod, """
Tuple{Int,Int}[(x,y) for x in 1:2, y in 1:3]
""") == [(1,1) (1,2) (1,3)
         (2,1) (2,2) (2,3)]

# Triply nested comprehension
@test JuliaLowering.include_string(test_mod, """
[(x,y,z) for x in 1:3 for y in 4:5 for z in 6:7]
""") == [
    (1, 4, 6)
    (1, 4, 7)
    (1, 5, 6)
    (1, 5, 7)
    (2, 4, 6)
    (2, 4, 7)
    (2, 5, 6)
    (2, 5, 7)
    (3, 4, 6)
    (3, 4, 7)
    (3, 5, 6)
    (3, 5, 7)
]

# splat in lhs
@test JuliaLowering.include_string(test_mod, """
[(h, i, j) for (h, i..., j) in ((1,2,3,4),(5,6,7,8))]
""") == [(1,(2,3),4) ; (5,(6,7),8)]
@test JuliaLowering.include_string(test_mod, """
collect((h, i, j) for (h, i..., j) in ((1,2,3,4),(5,6,7,8)))
""") == [(1,(2,3),4) ; (5,(6,7),8)]

# bad splat in iterable expression (pkgeval Lerche, MultidimensionalTools): a
# splat happens to work because of the way `xs...` is placed directly into
# `(call Generator ...)` in desugaring, which throws a MethodError if `xs` has
# more than one element.
@test JuliaLowering.include_string(test_mod, """
    func_generator_splat(xs...) = [2i for i in xs...]
    func_generator_splat([1, 2])
""") == [2,4]
@test JuliaLowering.include_string(test_mod, """
    func_comprehension_splat(xs...) = collect(2i for i in xs...)
    func_comprehension_splat([1, 2])
""") == [2,4]

@testset "generators: issue 18621" begin
    @test JuliaLowering.include_string(test_mod, """
    function g18621()
        f = function(i)
            f = (i)->2
            1
        end
        [f(i) for i in 1:3]
    end
    g18621()
    """) == [1,2,2]
    @test JuliaLowering.include_string(test_mod, """
    global f18621 = function(i)
        global f18621 = (i) -> 2
        1
    end
    [f18621(i) for i in 1:3]
    """) == [1,2,2]
    @test JuliaLowering.include_string(test_mod, """
    function h18621()
        g = (k(i) for i in 1:5)
        k = identity
        return collect(g)
    end
    h18621()
    """) == 1:5
end

@testset "generator iteration variables are local" begin
    @test JuliaLowering.include_string(test_mod, """
    let x = 0
        ys = [x for x::Int in 1:3]
        (x, ys)
    end
    """) == (0, [1,2,3])
    @test JuliaLowering.include_string(test_mod, """
    let x = collect(1:3)
        ys = [x for (i, x) in enumerate(x)]
        (x, ys)
    end
    """) == (collect(1:3), [1,2,3])
end

@testset "compat: comprehension with non-generator arg" begin
    let ex = Expr(:comprehension, :i, Expr(:(=), :i, Expr(:call, :(:), 1, 3)))
        @test jl_eval(test_mod, ex) == fl_eval(test_mod, ex)
    end
end

@testset "(AI) correct types" begin
    local jeval(str) = jl_eval(test_mod, parsestmt(SyntaxTree, str))
    local feval(str) = fl_eval(test_mod, parsestmt(Expr, str))
    local same_type(str) = jeval(str) == feval(str)
    let s = "[(a,b) for (a,b) in [[1,2],[3,4]]]"
        @test same_type(s)
        @test jeval(s) == feval(s)
    end
    let s = "[(k,v) for (k,v) in Dict(1=>10)]"
        @test same_type(s)
        @test jeval(s) == feval(s)
    end
    let s = "collect((k,v) for (k,v) in pairs([10,20]))"
        @test same_type(s)
        @test jeval(s) == feval(s)
    end
    let s = "[(a,) for (a,) in 1:3]"
        @test same_type(s)
        @test jeval(s) == feval(s)
    end
    let s = "[(a,) for (a,) in \"ab\"]"
        @test same_type(s)
        @test jeval(s) == feval(s)
    end
    let s = "collect(a+1 for a::Int in \"ab\")"
        @test same_type(s)
        @test jeval(s) == feval(s)
    end
    let s = "[x for x::Float64 in [1,2,3]]"
        @test same_type(s)
        @test jeval(s) == feval(s)
    end
    let s = "collect(x+1 for x::Int in Real[1.0, 2.0])"
        @test same_type(s)
        @test jeval(s) == feval(s)
    end
    let s = "(r=Int[]; for a::Int in \"ab\"; push!(r, a+1); end; r)"
        @test same_type(s)
        @test jeval(s) == feval(s)
    end
    let s = "Int[a+1 for a::Int in \"ab\"]"
        @test same_type(s)
        @test jeval(s) == feval(s)
    end
    let s = "collect(b for (a,b,c) in [(1,2,3)], c::Float64 in 10:11)"
        @test same_type(s)
        @test jeval(s) == feval(s)
    end
end

@testset "placeholders" begin
    local jeval(str) = jl_eval(test_mod, parsestmt(SyntaxTree, str))
    local feval(str) = fl_eval(test_mod, parsestmt(Expr, str))

    @test jeval("""
    let arr = [1,2,3]
        [1 for _ in push!(arr, 4)]
    end
    """) == [1,1,1,1]
    @test jeval("""
    let arr = [1,2,3]
        [1 for _::Float64 in push!(arr, 4)]
    end
    """) == [1,1,1,1]
    @test jeval("""
    let arr = [1,2,3]
        [1 for _ in push!(arr, 4), _ in push!(arr, 5)]
    end
    """) == ones(Int, 5, 5)
    # Pushes forever in both lowering implementations
    # @test jeval("""
    # let arr = [1,2,3]
    #     [1 for _ in push!(arr, 4) for _ in push!(arr, 5)]
    # end
    # """)
    @test jeval("""
    let arr = [(1,1),(2,2),(3,3)]
        [1 for (_,_) in push!(arr, (4,4)), (_,_) in push!(arr, (5,5))]
    end
    """) == ones(Int, 5, 5)
    @test jeval("""
    let arr = [(1,1),(2,2),(3,3,3,3)]
        [1 for (_,_...) in push!(arr, (4,4)), (_...,_) in push!(arr, (5,5))]
    end
    """) == ones(Int, 5, 5)
end
