# This file is a part of Julia. License is MIT: http://julialang.org/license

function hex2bytes(s::ASCIIString)
    len = length(s)
    iseven(len) || throw(ArgumentError("string length must be even: length($(repr(s))) == $len"))
    arr = zeros(UInt8, div(len,2))
    i = j = 0
    while i < len
        n = 0
        c = s[i+=1]
        n = '0' <= c <= '9' ? c - '0' :
            'a' <= c <= 'f' ? c - 'a' + 10 :
            'A' <= c <= 'F' ? c - 'A' + 10 :
                throw(ArgumentError("not a hexadecimal string: $(repr(s))"))
        c = s[i+=1]
        n = '0' <= c <= '9' ? n << 4 + c - '0' :
            'a' <= c <= 'f' ? n << 4 + c - 'a' + 10 :
            'A' <= c <= 'F' ? n << 4 + c - 'A' + 10 :
                throw(ArgumentError("not a hexadecimal string: $(repr(s))"))
        arr[j+=1] = n
    end
    return arr
end

bytes2hex{T<:UInt8}(arr::Vector{T}) = join([hex(i,2) for i in arr])
