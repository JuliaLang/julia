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


end
