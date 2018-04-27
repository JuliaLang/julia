# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type IPAddr end

Base.isless(a::T, b::T) where {T<:IPAddr} = isless(a.host, b.host)
(dt::Type{<:Integer})(ip::IPAddr) = dt(ip.host)::dt

struct IPv4 <: IPAddr
    host::UInt32
    IPv4(host::UInt32) = new(host)
    IPv4(a::UInt8,b::UInt8,c::UInt8,d::UInt8) = new(UInt32(a)<<24|
                                                    UInt32(b)<<16|
                                                    UInt32(c)<<8|
                                                    d)
    function IPv4(a::Integer,b::Integer,c::Integer,d::Integer)
        if !(0<=a<=255 && 0<=b<=255 && 0<=c<=255 && 0<=d<=255)
            throw(ArgumentError("IPv4 field out of range (must be 0-255)"))
        end
        IPv4(UInt8(a),UInt8(b),UInt8(c),UInt8(d))
    end
end

"""
    IPv4(host::Integer) -> IPv4

Returns an IPv4 object from ip address `host` formatted as an [`Integer`](@ref).

```jldoctest
julia> IPv4(3223256218)
ip"192.30.252.154"
```
"""
function IPv4(host::Integer)
    if host < 0
        throw(ArgumentError("IPv4 address must be positive"))
    elseif typemax(typeof(host)) > typemax(UInt32) && host > typemax(UInt32)
        throw(ArgumentError("IPv4 address must fit within 32 bits"))
    else
        return IPv4(UInt32(host))
    end
end

# constructor: ("1.2.3.4")
IPv4(str::AbstractString) = parse(IPv4, str)

show(io::IO,ip::IPv4) = print(io,"ip\"",ip,"\"")
print(io::IO,ip::IPv4) = print(io,string((ip.host&(0xFF000000))>>24),".",
                                  string((ip.host&(0xFF0000))>>16),".",
                                  string((ip.host&(0xFF00))>>8),".",
                                  string(ip.host&0xFF))

struct IPv6 <: IPAddr
    host::UInt128
    IPv6(host::UInt128) = new(host)
    IPv6(a::UInt16,b::UInt16,c::UInt16,d::UInt16,
     e::UInt16,f::UInt16,g::UInt16,h::UInt16) = new(UInt128(a)<<(7*16)|
                            UInt128(b)<<(6*16)|
                            UInt128(c)<<(5*16)|
                            UInt128(d)<<(4*16)|
                            UInt128(e)<<(3*16)|
                            UInt128(f)<<(2*16)|
                            UInt128(g)<<(1*16)|
                            h)
    function IPv6(a::Integer,b::Integer,c::Integer,d::Integer,
          e::Integer,f::Integer,g::Integer,h::Integer)
        if !(0<=a<=0xFFFF && 0<=b<=0xFFFF && 0<=c<=0xFFFF && 0<=d<=0xFFFF &&
             0<=e<=0xFFFF && 0<=f<=0xFFFF && 0<=g<=0xFFFF && 0<=h<=0xFFFF)
            throw(ArgumentError("IPv6 field out of range (must be 0-65535)"))
        end
        IPv6(UInt16(a),UInt16(b),UInt16(c),UInt16(d),
             UInt16(e),UInt16(f),UInt16(g),UInt16(h))
    end
end

"""
    IPv6(host::Integer) -> IPv6

Returns an IPv6 object from ip address `host` formatted as an [`Integer`](@ref).

```jldoctest
julia> IPv6(3223256218)
ip"::c01e:fc9a"
```
"""
function IPv6(host::Integer)
    if host < 0
        throw(ArgumentError("IPv6 address must be positive"))
        # We allow passing bigger integer types, but need to be careful to avoid overflow
        # Let's hope promotion rules are sensible
    elseif typemax(typeof(host)) > typemax(UInt128) && host > typemax(UInt128)
        throw(ArgumentError("IPv6 address must fit within 128 bits"))
    else
        return IPv6(UInt128(host))
    end
end

IPv6(str::AbstractString) = parse(IPv6, str)

# Suppress leading '0's and "0x"
print_ipv6_field(io,field::UInt16) = print(io,string(field, base = 16))

print_ipv6_field(io,ip,i) = print_ipv6_field(io,ipv6_field(ip,i))
function ipv6_field(ip::IPv6,i)
    if i < 0 || i > 7
        throw(BoundsError())
    end
    UInt16(ip.host&(UInt128(0xFFFF)<<(i*16))>>(i*16))
end

show(io::IO, ip::IPv6) = print(io,"ip\"",ip,"\"")
# RFC 5952 compliant show function
# http://tools.ietf.org/html/rfc5952
function print(io::IO,ip::IPv6)
    i = 8
    m = 0
    longest_sub_i = -1
    while i!=0
        i-=1
        field = ipv6_field(ip,i)
        if field == 0 && longest_sub_i == -1
            # Find longest subsequence of 0
            longest_sub_i,j,m,c = i,i,1,1
            while j != 0
                j-=1
                if ipv6_field(ip,j) == 0
                    c += 1
                else
                    c = 0
                end
                if c > m
                    if j+c != longest_sub_i+1
                        longest_sub_i = j+c-1
                    end
                    m = c
                end
            end
            # Prevent single 0 from contracting to :: as required
            if m == 1
                longest_sub_i = 9
            end
        end
        if i == longest_sub_i
            print(io,":")
            i -= m-1
            if i == 0
                print(io,":")
                break
            end
        else
            if i != 7
                print(io,":")
            end
            print_ipv6_field(io,field)
        end
    end
end

# Parsing

const ipv4_leading_zero_error = """
Leading zeros in IPv4 addresses are disallowed due to ambiguity.
If the address is in octal or hexadecimal, convert it to decimal, otherwise remove the leading zero.
"""

function parse(::Type{IPv4}, str::AbstractString)
    fields = split(str,'.')
    i = 1
    ret = 0
    for f in fields
        if isempty(f)
            throw(ArgumentError("empty field in IPv4 address"))
        end
        if length(f) > 1 && f[1] == '0'
            throw(ArgumentError(ipv4_leading_zero_error))
        else
            r = parse(Int, f, base = 10)
        end
        if i != length(fields)
            if r < 0 || r > 255
                throw(ArgumentError("IPv4 field out of range (must be 0-255)"))
            end
            ret |= UInt32(r) << ((4-i)*8)
        else
            if r > ((UInt64(1)<<((5-length(fields))*8))-1)
                throw(ArgumentError("IPv4 field too large"))
            end
            ret |= r
        end
        i+=1
    end
    IPv4(ret)
end

function parseipv6fields(fields,num_fields)
    if length(fields) > num_fields
        throw(ArgumentError("too many fields in IPv6 address"))
    end
    cf = 7
    ret = UInt128(0)
    for f in fields
        if isempty(f)
            # ::abc:... and ..:abc::
            if cf != 7 && cf != 0
                cf -= num_fields-length(fields)
            end
            cf -= 1
            continue
        end
        ret |= UInt128(parse(Int, f, base = 16))<<(cf*16)
        cf -= 1
    end
    ret
end
parseipv6fields(fields) = parseipv6fields(fields,8)

function parse(::Type{IPv6}, str::AbstractString)
    fields = split(str,':')
    if length(fields) > 8
        throw(ArgumentError("too many fields in IPv6 address"))
    elseif length(fields) == 8
        return IPv6(parseipv6fields(fields))
    elseif in('.',fields[end])
        return IPv6((parseipv6fields(fields[1:(end-1)],6))
            | parse(IPv4, fields[end]).host )
    else
        return IPv6(parseipv6fields(fields))
    end
end

#
# This support IPv4 addresses in the common dot (IPv4) or colon (IPv6)
# separated formats. Most other common formats use a standard integer encoding
# of the appropriate size and should use the appropriate constructor
#

function parse(::Type{IPAddr}, str::AbstractString)
    if ':' in str
        return parse(IPv6, str)
    else
        return parse(IPv4, str)
    end
end

macro ip_str(str)
    return parse(IPAddr, str)
end

struct InetAddr{T<:IPAddr}
    host::T
    port::UInt16
end

InetAddr(ip::IPAddr, port) = InetAddr{typeof(ip)}(ip, port)
