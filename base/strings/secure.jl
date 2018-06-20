"""
    SecureString(string::AbstractString)

A string where the contents will be securely wiped when garbage collected. However, it is
considered best practise to wipe the string using `shred!(::SecureString)` as soon as the
secure data is no longer required. Note that when the parameter is of type `Vector{UInt8}`
then the memory of the passed in parameter will also be securely wiped.

# Examples
```jldoctest
julia> str = "abc"::String
"abc"

julia> s = SecureString(str)
"abc"

julia> shred!(s)
"\0\0\0"

julia> str
"abc"
```
"""
mutable struct SecureString <: AbstractString
    data::Vector{UInt8}

    function SecureString(data::Vector{UInt8})
        s = new(data)
        finalizer(final_shred!, s)
        return s
    end
end

SecureString(str::String) = SecureString(copy(unsafe_wrap(Vector{UInt8}, str)))
SecureString(str::Union{AbstractString,CodeUnits}) = SecureString(String(str))

show(io::IO, s::SecureString) = print(io, "SecureString(\"*****\")")
write(io::IO, s::SecureString) = write(io, s.data)

function print(io::IO, s::SecureString)
    write(io, s.data)
    nothing
end

String(s::SecureString) = String(copy(s.data))

iterate(s::SecureString, i::Integer=firstindex(s)) = iterate(String(s), i)
ncodeunits(s::SecureString) = Core.sizeof(s.data)
codeunit(s::SecureString) = UInt8
codeunit(s::SecureString, i::Integer) = s.data[i]

isvalid(s::SecureString, i::Int) = isvalid(String(s), i)

==(a::SecureString, b::SecureString) = a.data == b.data
==(a::String, b::SecureString) = unsafe_wrap(Vector{UInt8}, a) == b.data
==(a::AbstractString, b::SecureString) = String(a) == b
==(a::SecureString, b::AbstractString) = b == a

function final_shred!(s::SecureString)
    if !isshredded(s)
        # TODO: Printing SecureString data is temporary while I locate issues in tests
        # Note: `@warn` won't work here
        println("SecureString \"$s\" not explicitly shredded")
        shred!(s)
    end
end

function shred!(s::SecureString)
    securezero!(s.data)
    return s
end

isshredded(s::SecureString) = sum(s.data) == 0

function shred!(f::Function, x)
    try
        f(x)
    finally
        shred!(x)
    end
    x
end
