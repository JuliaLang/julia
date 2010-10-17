function error(s::Union(Latin1String,UTF8String))
    throw(ErrorException(s))
end

function assert(c)
    if !c
        error("Assertion failed.")
    end
    true
end
