function signbit(x::Real)
    if x < 0
        return -1
    elseif x > 0
        return 1
    elseif 1.0/x < 0
        return -1
    end
    return 1
end
