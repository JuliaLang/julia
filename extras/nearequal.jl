# numerical near equality
isclose(x, y, rtol, atol) = abs(x-y) <= atol+rtol.*max(abs(x), abs(y))
function isclose{T1<:Float, T2<:Float}(x::T1, y::T2)
    tol = max(eps(T1), eps(T2))
    isclose(x, y, tol^(1/3), sqrt(tol))
end
isclose{T1<:Integer, T2<:Float}(x::T1, y::T2) = isclose(float(x), y)
isclose{T1<:Float, T2<:Integer}(x::T1, y::T2) = isclose(x, float(y))

isclose(X::AbstractArray, y::Number) = map(x -> isclose(x, y), X)
isclose(x::Number, Y::AbstractArray) = map(y -> isclose(y, x), Y)

function isclose(X::AbstractArray, Y::AbstractArray)
    if size(X) != size(Y)
        error("Arrays must have the same sizes (first is $(size(X)), second is $(size(Y))).")
    end
    Z = similar(X, Bool)
    for i in 1:numel(X)
        Z[i] = isclose(X[i], Y[i])
    end
    Z
end

isclose{T1<:Integer, T2<:Integer}(x::T1, y::T2) = isequal(x, y)
