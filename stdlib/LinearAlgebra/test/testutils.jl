# This file is a part of Julia. License is MIT: https://julialang.org/license

# Test approximate equality of vectors or columns of matrices modulo floating
# point roundoff and phase (sign) differences.
#
# This function is designed to test for equality between vectors of floating point
# numbers when the vectors are defined only up to a global phase or sign, such as
# normalized eigenvectors or singular vectors. The global phase is usually
# defined consistently, but may occasionally change due to small differences in
# floating point rounding noise or rounding modes, or through the use of
# different conventions in different algorithms. As a result, most tests checking
# such vectors have to detect and discard such overall phase differences.
#
# Inputs:
#     a, b:: StridedVecOrMat to be compared
#     err :: Default: m^3*(eps(S)+eps(T)), where m is the number of rows
#
# Raises an error if any columnwise vector norm exceeds err. Otherwise, returns
# nothing.
function test_approx_eq_modphase(a::StridedVecOrMat{S}, b::StridedVecOrMat{T},
                                 err = length(axes(a,1))^3*(eps(S)+eps(T))) where {S<:Real,T<:Real}
    @test axes(a,1) == axes(b,1) && axes(a,2) == axes(b,2)
    for i in axes(a,2)
        v1, v2 = a[:, i], b[:, i]
        @test min(abs(norm(v1-v2)),abs(norm(v1+v2))) ≈ 0.0 atol=err
    end
end
