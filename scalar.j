function sign(x::Scalar)
    if x < 0
        return -1
    elseif x > 0
        return 1
    end
    return 0
end

conj(x::Scalar) = x
transpose(x::Scalar) = x
ctranspose(x::Scalar) = conj(x)
