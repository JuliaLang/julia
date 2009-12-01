function print(x)
    _print(x)
    return ()
end

!(x) = false
!=(x, y) = !(x == y)

function signbit(x)
    if x < 0
        return -1
    elseif x > 0
        return 1
    end
    return 1
end
