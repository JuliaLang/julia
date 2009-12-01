function sign(x::Scalar)
    if x < 0
        return -1
    elseif x > 0
        return 1
    end
    return 0
end

function signbit(x::Scalar)
    if x < 0
        return -1
    elseif x > 0
        return 1
    end
    return 1
end

function signbit(x::Float)
    if x < 0
        return -1
    elseif x > 0
        return 1
    elseif 1.0/x < 0
        return -1
    end
    return 1
end

conj(x::Scalar) = x
transpose(x::Scalar) = x
ctranspose(x::Scalar) = conj(x)
