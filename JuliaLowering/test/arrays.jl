using Test, JuliaLowering

@testset "Array syntax" begin

test_mod = Module()

# Test that two array element types are equal and that they are also equal
# elementwise
function ≅(a, b)
    eltype(a) == eltype(b) && a == b
end

# vect
@test JuliaLowering.include_string(test_mod, """
[1,2,3]
""") ≅ [1,2,3]

# hcat
@test JuliaLowering.include_string(test_mod, """
[1 2 3]
""") ≅ [1 2 3]

# typed_hcat
@test JuliaLowering.include_string(test_mod, """
Int[1.0 2.0 3.0]
""") ≅ [1 2 3]

# splat with vect/hcat/typed_hcat
@test JuliaLowering.include_string(test_mod, """
let xs = [1,2,3]
    [0, xs...]
end
""") ≅ [0,1,2,3]
@test JuliaLowering.include_string(test_mod, """
let xs = [1,2,3]
    [0 xs...]
end
""") ≅ [0 1 2 3]
@test JuliaLowering.include_string(test_mod, """
let xs = [1,2,3]
    Int[0 xs...]
end
""") ≅ Int[0 1 2 3]

# vcat
@test JuliaLowering.include_string(test_mod, """
[1;2;3]
""") ≅ [1; 2; 3]

@test JuliaLowering.include_string(test_mod, """
let
    xs = (1,2)
    [xs...; xs...]
end
""") ≅ [1,2,1,2]

# hvcat
@test JuliaLowering.include_string(test_mod, """
[1 2 3; 4 5 6]
""") ≅ [1 2 3;
        4 5 6]

# hvcat_rows
@test JuliaLowering.include_string(test_mod, """
let
    xs = (1,2)
    [xs... 3; 4 xs...]
end
""") ≅ [1 2 3;
        4 1 2]

# typed_vcat
@test JuliaLowering.include_string(test_mod, """
Int[1.0; 2.0; 3.0]
""") ≅ [1; 2; 3]

# typed_hvcat
@test JuliaLowering.include_string(test_mod, """
Int[1.0 2.0 3.0; 4.0 5.0 6.0]
""") ≅ [1 2 3;
        4 5 6]

# typed_hvcat_rows
@test JuliaLowering.include_string(test_mod, """
let
    xs = (1.0,2.0)
    Int[xs... 3; 4 xs...]
end
""") ≅ [1 2 3;
        4 1 2]

# ncat with a single dimension
@test JuliaLowering.include_string(test_mod, """
[1 ;;; 2 ;;; 3]
""") ≅ [1 ;;; 2 ;;; 3]

@test JuliaLowering.include_string(test_mod, """
Int[1.0 ;;; 2.0 ;;; 3.0]
""") ≅ [1 ;;; 2 ;;; 3]

# Lowering of ref to setindex
@test JuliaLowering.include_string(test_mod, """
let
    as = [0,0,0,0]
    as[begin] = 1
    as[2] = 2
    as[end] = 4
    as
end
""") == [1, 2, 0, 4]

@test JuliaLowering.include_string(test_mod, """
let
    as = zeros(Int, 2,3)
    as[begin, end] = 1
    as[end, begin] = 2
    js = (2,)
    as[js..., end] = 3
    as
end
""") == [0 0 1;
         2 0 3]

# getindex
@test JuliaLowering.include_string(test_mod, """
let
    x = [1 2;
         3 4]
    (x[end,begin], x[begin,end])
end
""") == (3, 2)

# getindex with splats
@test JuliaLowering.include_string(test_mod, """
let
    x = [1 2;
         3 4
         ;;;
         5 6;
         7 8]
    inds = (2,1)
    ind1 = (1,)
    (x[inds..., begin], x[inds..., end], x[1, inds...],
     x[ind1..., ind1..., end])
end
""") == (3, 7, 2, 5)

# begin/end not replaced in some cases
JuliaLowering.include_string(test_mod, "f(args...;kws...) = 2")
@test JuliaLowering.include_string(test_mod, """
    [7,8,9][f(;var"end"=123, var"begin"=456)]
""") === 8
@test JuliaLowering.include_string(test_mod, """
    [7,8,9][f(quote var"end" end)]
""") === 8
@test JuliaLowering.include_string(test_mod, """
let var"end" = [1,2,3], y = [7,8,9]
    y[var"end"[var"end"]]
end
""") === 9

end # @testset "Array syntax" begin
