module Rot13

function rot13(c::Char)
    shft = islowercase(c) ? 'a' : 'A'
    isletter(c) ? c = shft + (c - shft + 13) % 26 : c
end

rot13(str::AbstractString) = map(rot13, str)

function (@main)(args)
    foreach(arg -> print(rot13(arg), " "), args)
    return 0
end

end # module Rot13
