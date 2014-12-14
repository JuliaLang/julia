## IP ADDRESS HANDLING ##
abstract IPAddr

IPv4broadcast = reinterpret(UInt32, int32(-1))
IPv6broadcast = reinterpret(UInt128, int128(-1))

immutable IPv4 <: IPAddr
    host::UInt32
    IPv4(host::UInt32) = new(host)
    IPv4(a::UInt8,b::UInt8,c::UInt8,d::UInt8) = new(uint32(a)<<24|
                                                    uint32(b)<<16|
                                                    uint32(c)<<8|
                                                    d)
    function IPv4(a::Integer,b::Integer,c::Integer,d::Integer)
        if !(0<=a<=255 && 0<=b<=255 && 0<=c<=255 && 0<=d<=255)
            throw(DomainError())
        end
        IPv4(uint8(a),uint8(b),uint8(c),uint8(d))
    end
end

function IPv4(host::Integer)
    if host < 0
        error("IP address must not be negative")
    elseif typemax(typeof(host)) > typemax(UInt32) && host > typemax(UInt32)
        error("IPv4 address must fit within 32 bits")
    else
        return IPv4(uint32(host))
    end
end

# constructor: ("1.2.3.4")
IPv4(ipstr::AbstractString) = parseipv4(ipstr)

show(io::IO,ip::IPv4) = print(io,"ip\"",ip,"\"")
print(io::IO,ip::IPv4) = print(io,dec((ip.host&(0xFF000000))>>24),".",
                                  dec((ip.host&(0xFF0000))>>16),".",
                                  dec((ip.host&(0xFF00))>>8),".",
                                  dec(ip.host&0xFF))

immutable IPv6 <: IPAddr
    host::UInt128
    IPv6(host::UInt128) = new(host)
    IPv6(a::UInt16,b::UInt16,c::UInt16,d::UInt16,
     e::UInt16,f::UInt16,g::UInt16,h::UInt16) = new(uint128(a)<<(7*16)|
                            uint128(b)<<(6*16)|
                            uint128(c)<<(5*16)|
                            uint128(d)<<(4*16)|
                            uint128(e)<<(3*16)|
                            uint128(f)<<(2*16)|
                            uint128(g)<<(1*16)|
                            h)
    function IPv6(a::Integer,b::Integer,c::Integer,d::Integer,
          e::Integer,f::Integer,g::Integer,h::Integer)
    if !(0<=a<=0xFFFF && 0<=b<=0xFFFF && 0<=c<=0xFFFF && 0<=d<=0xFFFF &&
         0<=e<=0xFFFF && 0<=f<=0xFFFF && 0<=g<=0xFFFF && 0<=h<=0xFFFF)
        throw(DomainError())
    end
    IPv6(uint16(a),uint16(b),uint16(c),uint16(d),
         uint16(e),uint16(f),uint16(g),uint16(h))
    end
end

function IPv6(host::Integer)
    if host < 0
        error("IP address must not be negative")
        # We allow passing bigger integer types, but need to be careful to avoid overflow
        # Let's hope promotion rules are sensible
    elseif typemax(typeof(host)) > typemax(UInt128) && host > typemax(UInt128)
        error("IPv6 address must fit within 128 bits")
    else
        return IPv6(uint128(host))
    end
end

IPv6(ipstr::AbstractString) = parseipv6(ipstr)



# RFC 5952 compliant show function
# http://tools.ietf.org/html/rfc5952
show(io::IO, ip::IPv6) = print(io,"ip\"",ip,"\"")


# Suppress leading '0's and "0x"
repr_ipv6_field(field::UInt16) = return(hex(field))
repr_ipv6_field(ip,i) = repr_ipv6_field(ipv6_field(ip,i))

function ipv6_field(ip::IPv6,i)
    if i < 0 || i > 7
        throw(BoundsError())
    end
    uint16(ip.host&(uint128(0xFFFF)<<(i*16))>>(i*16))
end

function repr(ip::IPv6)
    i = 8
    m = 0
    str = ""
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
            str = string(str,":")
            i -= m-1
            if i == 0
                str = string(str,":")
                break
            end
        else
            if i != 7
                str = string(str,":")
            end
            str = string(str, repr_ipv6_field(field))
        end
    end
    return str
end
string(ip::IPv6) = repr(ip::IPv6)

print(io::IO,ip::IPv6) = print(io,repr(ip))

# Parsing

function parseipv4(str)
    fields = split(str,'.')
    i = 1
    ret = 0
    for f in fields
        if length(f) == 0
            error("empty field in IPv4 address")
        end
        if f[1] == '0'
            if length(f) >= 2 && f[2] == 'x'
                if length(f) > 8 # 2+(3*2) - prevent parseint from overflowing on 32bit
                    error("IPv4 field too large")
                end
                r = parseint(f[3:end],16)
            else
                if length(f) > 9 # 1+8 - prevent parseint from overflowing on 32bit
                    error("IPv4 field too large")
                end
                r = parseint(f,8)
            end
        else
            r = parseint(f,10)
        end
        if i != length(fields)
            if r < 0 || r > 255
                error("IPv4 field out of range (must be 0-255)")
            end
            ret |= uint32(r) << ((4-i)*8)
        else
            if r > ((uint64(1)<<((5-length(fields))*8))-1)
                error("IPv4 field too large")
            end
            ret |= r
        end
        i+=1
    end
    IPv4(ret)
end

function parseipv6fields(fields,num_fields)
    if length(fields) > num_fields
        error("too many fields in IPv6 address")
    end
    cf = 7
    ret = uint128(0)
    for f in fields
        if f == ""
            # ::abc:... and ..:abc::
            if cf != 7 && cf != 0
                cf -= num_fields-length(fields)
            end
            cf -= 1
            continue
        end
        ret |= uint128(parseint(f,16))<<(cf*16)
        cf -= 1
    end
    ret
end
parseipv6fields(fields) = parseipv6fields(fields,8)

function parseipv6(str)
    fields = split(str,':')
    if length(fields) > 8
        error("too many fields in IPv6 address")
    elseif length(fields) == 8
        return IPv6(parseipv6fields(fields))
    elseif in('.',fields[end])
        return IPv6((parseipv6fields(fields[1:(end-1)],6))
            | parseipv4(fields[end]).host )
    else
        return IPv6(parseipv6fields(fields))
    end
end

#
# This support IPv4 addresses in the common dot (IPv4) or colon (IPv6)
# separated formats. Most other common formats use a standard integer encoding
# of the appropriate size and should use the appropriate constructor
#

function parseip(str)
    if in(':',str)
        # IPv6 Address
        return parseipv6(str)
    else
        # IPv4 Address
        return parseipv4(str)
    end
end

macro ip_str(str)
    return parseip(str)
end

type InetAddr
    host::IPAddr
    port::UInt16
    function InetAddr(host,port)
        if !(0 <= port <= typemax(UInt16))
            throw(DomainError())
        end
        new(host,uint16(port))
    end
end

# Network representations
abstract IPNet


immutable IPv4Net <: IPNet
    netaddr::IPv4
    netmask::IPv4
end

IPv4Net{T}(tuple::(T,T)) = IPv4Net(tuple[1],tuple[2])

function IPv4Net(ipmask::AbstractString)
    if search(ipmask,'/') > 0
        addrstr, netmaskbits = split(ipmask,"/")
        netmaskbitsint = int(netmaskbits)
        if !(0 <= netmaskbitsint <= 32)
            error("Invalid netmask")
        else
            netmaskint = ~(IPv4broadcast >> int(netmaskbits))
        end
    else
        addrstr = ipmask
        netmaskint = IPv4broadcast
    end
    netaddr = IPv4(addrstr)
    netmask = IPv4(netmaskint)
    return IPv4Net(netaddr,netmask)
end

function IPv4Net(netaddr, netmask=IPv4broadcast)
    netaddr = IPv4(netaddr)
    netmask = IPv4(netmask)
    return IPv4Net(netaddr, netmask)
end

IPv4Net(netarr::AbstractArray) = [IPv4Net(x) for x in netarr]


immutable IPv6Net <: IPNet
    # we treat the netmask as a potentially noncontiguous bitmask
    # for speed of calculation and consistency, but RFC2373, section
    # 2 provides for contiguous bitmasks only. We validate this
    # in the internal constructor.
    netaddr::IPv6
    netmask::IPv6

    function IPv6Net(netaddr::IPv6, netmask::IPv6)
        invbitfield = ~netmask.host
        if !(isinteger(log(2,invbitfield+1)))
            throw(DomainError())
        end
        return new(netaddr, netmask)
    end
end

IPv6Net{T}(tuple::(T,T)) = IPv6Net(tuple[1],tuple[2])

function IPv6Net(ipmask::AbstractString)
    if search(ipmask,'/') > 0
        addrstr, netmaskbits = split(ipmask,"/")
        netmaskbitsint = int(netmaskbits)
        if !(0 <= netmaskbitsint <= 128)
            error("Invalid netmask")
        else
            netmaskint = ~(IPv6broadcast >> int(netmaskbits))
        end
    else
        addrstr = ipmask
        netmaskint = IPv6broadcast
    end
    netaddr = IPv6(addrstr)
    netmask = IPv6(netmaskint)
    return IPv6Net(netaddr,netmask)
end

function IPv6Net(netaddr, netmask=IPv6broadcast)
    netaddr = IPv6(netaddr)
    netmask = IPv6(netmask)
    return IPv6Net(netaddr, netmask)
end

IPv6Net(netarr::AbstractArray) = [IPv6Net(x) for x in netarr]



function string(net::IPv4Net)
    ns = Array(AbstractString,4)
    for i = 4:-1:1
        ns[5-i] = Base.string((net.netaddr.host >> (8(i-1))) & 0xff)
    end
    s = string("IPv4Net(\"", join(ns,"."), "\", ")
    for i = 4:-1:1
        ns[5-i] = Base.string((net.netmask.host >> (8(i-1))) & 0xff)
    end
    s = string(s, "\"", join(ns,"."), "\")")
    return s
end

function string(net::IPv6Net)
    netmaskint = 128-int(log(2,~net.netmask.host+1))
    return string("IPv6Net(\"", string(net.netaddr), "/", netmaskint, "\")")
end

# Add method for in -> 1.2.3.4 IN 1.2.3.0/24
function in(ipaddr::IPAddr, net::IPNet)
    maskedaddr = ipaddr.host & net.netmask.host
    maskednet = net.netaddr.host & net.netmask.host

    return (maskedaddr == maskednet)
end

function contains(net::IPNet, ipaddr::IPAddr)
    return in(ipaddr, net)
end

function show(io::IO, ipnet::IPNet)
    print(io, string(ipnet))
end
