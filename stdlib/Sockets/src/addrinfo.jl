# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    DNSError

The type of exception thrown when an error occurs in DNS lookup.
The `host` field indicates the host URL string.
The `code` field indicates the error code based on libuv.
"""
struct DNSError <: Exception
    host::String
    code::Int32
end

function show(io::IO, err::DNSError)
    print(io, "DNSError: ", err.host, ", ", Base.struverror(err.code),
                                      " (", Base.uverrorname(err.code), ")")
end

function uv_getaddrinfocb(req::Ptr{Cvoid}, status::Cint, addrinfo::Ptr{Cvoid})
    t = Base._claim_uvreq_waiter(req)
    if t === nothing
        # nobody to deliver to: also release the system-owned result
        addrinfo == C_NULL || ccall(:uv_freeaddrinfo, Cvoid, (Ptr{Cvoid},), addrinfo)
        return nothing
    end
    if status != 0 || addrinfo == C_NULL
        schedule(t, _UVError("getaddrinfo", status))
    else
        freeaddrinfo = addrinfo
        addrs = IPAddr[]
        while addrinfo != C_NULL
            sockaddr = ccall(:jl_sockaddr_from_addrinfo, Ptr{Cvoid}, (Ptr{Cvoid},), addrinfo)
            if ccall(:jl_sockaddr_is_ip4, Int32, (Ptr{Cvoid},), sockaddr) == 1
                ip4addr = ccall(:jl_sockaddr_host4, UInt32, (Ptr{Cvoid},), sockaddr)
                push!(addrs, IPv4(ntoh(ip4addr)))
            elseif ccall(:jl_sockaddr_is_ip6, Int32, (Ptr{Cvoid},), sockaddr) == 1
                ip6addr = Ref{UInt128}()
                scope_id = ccall(:jl_sockaddr_host6, UInt32, (Ptr{Cvoid}, Ptr{UInt128}), sockaddr, ip6addr)
                push!(addrs, IPv6(ntoh(ip6addr[])))
            end
            addrinfo = ccall(:jl_next_from_addrinfo, Ptr{Cvoid}, (Ptr{Cvoid},), addrinfo)
        end
        ccall(:uv_freeaddrinfo, Cvoid, (Ptr{Cvoid},), freeaddrinfo)
        schedule(t, addrs)
    end
    nothing
end

"""
    getalladdrinfo(host::AbstractString) -> Vector{IPAddr}

Gets all of the IP addresses of the `host`.
Uses the operating system's underlying `getaddrinfo` implementation, which may do a DNS lookup.

# Examples
```julia-repl
julia> getalladdrinfo("google.com")
2-element Vector{IPAddr}:
 ip"172.217.6.174"
 ip"2607:f8b0:4000:804::200e"
```
"""
function getalladdrinfo(host::String; cancel::Base.CancelTokenArg=Base.DEFAULT_CANCEL)
    tok = Base.check_cancel_arg(cancel)
    req = Libc.malloc(Base._sizeof_uv_getaddrinfo)
    uv_req_set_data(req, C_NULL) # in case we get interrupted before arriving at the wait call
    iolock_begin()
    status = ccall(:jl_getaddrinfo, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Ptr{Cvoid}, Ptr{Cvoid}),
                   eventloop(), req, host, #=service=#C_NULL,
                   @cfunction(uv_getaddrinfocb, Cvoid, (Ptr{Cvoid}, Cint, Ptr{Cvoid})))
    if status < 0
        Libc.free(req)
        if status == UV_EINVAL
            throw(ArgumentError("Invalid getaddrinfo argument"))
        elseif status == UV_ENOMEM || status == UV_ENOBUFS
            throw(OutOfMemoryError())
        end
        uv_error("getaddrinfo", status)
    end
    r = _wait_dns_req(req, tok)
    if isa(r, IOError)
        code = r.code
        if code in (UV_EAI_ADDRFAMILY, UV_EAI_AGAIN, UV_EAI_BADFLAGS,
                    UV_EAI_BADHINTS, UV_EAI_CANCELED, UV_EAI_FAIL,
                    UV_EAI_FAMILY, UV_EAI_NODATA, UV_EAI_NONAME,
                    UV_EAI_OVERFLOW, UV_EAI_PROTOCOL, UV_EAI_SERVICE,
                    UV_EAI_SOCKTYPE)
            throw(DNSError(host, code))
        elseif code == UV_EAI_MEMORY
            throw(OutOfMemoryError())
        else
            throw(r)
        end
    end
    return r::Vector{IPAddr}
end
getalladdrinfo(host::AbstractString; cancel::Base.CancelTokenArg=Base.DEFAULT_CANCEL) =
    getalladdrinfo(String(host); cancel)

# Park on an in-flight uv getaddrinfo/getnameinfo request (already issued,
# with its data field still C_NULL). There is no waitee object for a DNS
# lookup, so the request itself doubles as the wait witness; a wait that
# ends without the completion callback having run `uv_cancel`s the lookup
# and detaches the request. Caller must hold the iolock.
_wait_dns_req(req::Ptr{Cvoid}, tok::Base.MaybeToken) =
    Base._wait_uvreq(Base.cancel_source(tok), req, req, true, nothing)

"""
    getaddrinfo(host::AbstractString, IPAddr) -> IPAddr

Gets the first IP address of the `host` of the specified `IPAddr` type.
Uses the operating system's underlying getaddrinfo implementation, which may do
a DNS lookup.

# Examples
```jldoctest; filter = r"(ip\\"::1\\"|ERROR: DNSError:.*|Stacktrace:(\\n \\[0-9]+\\].*)*)"
julia> getaddrinfo("localhost", IPv6)
ip"::1"

julia> getaddrinfo("localhost", IPv4)
ip"127.0.0.1"
```
"""
function getaddrinfo(host::String, T::Type{<:IPAddr}; cancel::Base.CancelTokenArg=Base.DEFAULT_CANCEL)
    addrs = getalladdrinfo(host; cancel)
    for addr in addrs
        if addr isa T
            return addr
        end
    end
    throw(DNSError(host, UV_EAI_NONAME))
end
getaddrinfo(host::AbstractString, T::Type{<:IPAddr}; cancel::Base.CancelTokenArg=Base.DEFAULT_CANCEL) =
    getaddrinfo(String(host), T; cancel)

"""
    getaddrinfo(host::AbstractString) -> IPAddr

Gets the first available IP address of `host`, which may be either an `IPv4` or
`IPv6` address. Uses the operating system's underlying getaddrinfo
implementation, which may do a DNS lookup.
"""
function getaddrinfo(host::AbstractString; cancel::Base.CancelTokenArg=Base.DEFAULT_CANCEL)
    addrs = getalladdrinfo(String(host); cancel)
    if !isempty(addrs)
        return addrs[begin]::Union{IPv4,IPv6}
    end
    throw(DNSError(host, UV_EAI_NONAME))
end

function uv_getnameinfocb(req::Ptr{Cvoid}, status::Cint, hostname::Cstring, service::Cstring)
    t = Base._claim_uvreq_waiter(req)
    if t !== nothing
        if status != 0
            schedule(t, _UVError("getnameinfo", status))
        else
            schedule(t, unsafe_string(hostname))
        end
    end
    nothing
end

"""
    getnameinfo(host::IPAddr) -> String

Performs a reverse-lookup for IP address to return a hostname and service
using the operating system's underlying `getnameinfo` implementation.

# Examples
```julia-repl
julia> getnameinfo(IPv4("8.8.8.8"))
"google-public-dns-a.google.com"
```
"""
function getnameinfo(address::Union{IPv4, IPv6}; cancel::Base.CancelTokenArg=Base.DEFAULT_CANCEL)
    tok = Base.check_cancel_arg(cancel)
    req = Libc.malloc(Base._sizeof_uv_getnameinfo)
    uv_req_set_data(req, C_NULL) # in case we get interrupted before arriving at the wait call
    port = hton(UInt16(0))
    flags = 0
    uvcb = @cfunction(uv_getnameinfocb, Cvoid, (Ptr{Cvoid}, Cint, Cstring, Cstring))
    status = UV_EINVAL
    host_in = Ref(hton(address.host))
    iolock_begin()
    status = ccall(:jl_getnameinfo, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}, UInt16, Cint, Ptr{Cvoid}, Cint),
                   eventloop(), req, host_in, port, flags, uvcb, address isa IPv6)
    if status < 0
        Libc.free(req)
        if status == UV_EINVAL
            throw(ArgumentError("Invalid getnameinfo argument"))
        elseif status == UV_ENOMEM || status == UV_ENOBUFS
            throw(OutOfMemoryError())
        end
        uv_error("getnameinfo", status)
    end
    r = _wait_dns_req(req, tok)
    if isa(r, IOError)
        code = r.code
        if code in (UV_EAI_ADDRFAMILY, UV_EAI_AGAIN, UV_EAI_BADFLAGS,
                    UV_EAI_BADHINTS, UV_EAI_CANCELED, UV_EAI_FAIL,
                    UV_EAI_FAMILY, UV_EAI_NODATA, UV_EAI_NONAME,
                    UV_EAI_OVERFLOW, UV_EAI_PROTOCOL, UV_EAI_SERVICE,
                    UV_EAI_SOCKTYPE)
            throw(DNSError(repr(address), code))
        elseif code == UV_EAI_MEMORY
            throw(OutOfMemoryError())
        else
            throw(r)
        end
    end
    return r::String
end

const _sizeof_uv_interface_address = ccall(:jl_uv_sizeof_interface_address,Int32,())

"""
    getipaddr() -> IPAddr

Get an IP address of the local machine, preferring IPv4 over IPv6. Throws if no
addresses are available.

    getipaddr(addr_type::Type{T}) where T<:IPAddr -> T

Get an IP address of the local machine of the specified type. Throws if no
addresses of the specified type are available.

This function is a backwards-compatibility wrapper around [`getipaddrs`](@ref).
New applications should use [`getipaddrs`](@ref) instead.

# Examples
```julia-repl
julia> getipaddr()
ip"192.168.1.28"

julia> getipaddr(IPv6)
ip"fe80::9731:35af:e1c5:6e49"
```

See also [`getipaddrs`](@ref).
"""
function getipaddr(addr_type::Type{T}) where T<:IPAddr
    addrs = getipaddrs(addr_type)
    isempty(addrs) && error("No networking interface available")

    # When `addr_type` is `IPAddr`, `addrs` contain IP addresses of all types
    # In that case, we prefer to return the first IPv4
    i = something(findfirst(ip -> ip isa IPv4, addrs), 1)
    return addrs[i]
end
getipaddr() = getipaddr(IPAddr)


"""
    getipaddrs(addr_type::Type{T}=IPAddr; loopback::Bool=false) where T<:IPAddr -> Vector{T}

Get the IP addresses of the local machine.

Setting the optional `addr_type` parameter to `IPv4` or `IPv6` causes only addresses of that type to be returned.

The `loopback` keyword argument dictates whether loopback addresses (e.g. `ip"127.0.0.1"`, `ip"::1"`) are included.

!!! compat "Julia 1.2"
    This function is available as of Julia 1.2.

# Examples
```julia-repl
julia> getipaddrs()
5-element Vector{IPAddr}:
 ip"198.51.100.17"
 ip"203.0.113.2"
 ip"2001:db8:8:4:445e:5fff:fe5d:5500"
 ip"2001:db8:8:4:c164:402e:7e3c:3668"
 ip"fe80::445e:5fff:fe5d:5500"

julia> getipaddrs(IPv6)
3-element Vector{IPv6}:
 ip"2001:db8:8:4:445e:5fff:fe5d:5500"
 ip"2001:db8:8:4:c164:402e:7e3c:3668"
 ip"fe80::445e:5fff:fe5d:5500"
```

See also [`islinklocaladdr`](@ref).
"""
function getipaddrs(addr_type::Type{T}=IPAddr; loopback::Bool=false) where T<:IPAddr
    addresses = T[]
    addr_ref = Ref{Ptr{UInt8}}(C_NULL)
    count_ref = Ref{Int32}(1)
    lo_present = false
    err = ccall(:jl_uv_interface_addresses, Int32, (Ref{Ptr{UInt8}}, Ref{Int32}), addr_ref, count_ref)
    uv_error("getlocalip", err)
    addr, count = addr_ref[], count_ref[]
    for i = 0:(count-1)
        current_addr = addr + i*_sizeof_uv_interface_address
        if 1 == ccall(:jl_uv_interface_address_is_internal, Int32, (Ptr{UInt8},), current_addr)
            lo_present = true
            if !loopback
                continue
            end
        end
        sockaddr = ccall(:jl_uv_interface_address_sockaddr, Ptr{Cvoid}, (Ptr{UInt8},), current_addr)
        if IPv4 <: T && ccall(:jl_sockaddr_is_ip4, Int32, (Ptr{Cvoid},), sockaddr) == 1
            push!(addresses, IPv4(ntoh(ccall(:jl_sockaddr_host4, UInt32, (Ptr{Cvoid},), sockaddr))))
        elseif IPv6 <: T && ccall(:jl_sockaddr_is_ip6, Int32, (Ptr{Cvoid},), sockaddr) == 1
            addr6 = Ref{UInt128}()
            scope_id = ccall(:jl_sockaddr_host6, UInt32, (Ptr{Cvoid}, Ref{UInt128},), sockaddr, addr6)
            push!(addresses, IPv6(ntoh(addr6[])))
        end
    end
    ccall(:uv_free_interface_addresses, Cvoid, (Ptr{UInt8}, Int32), addr, count)
    return addresses
end

"""
    islinklocaladdr(addr::IPAddr)

Tests if an IP address is a link-local address. Link-local addresses
are not guaranteed to be unique beyond their network segment,
therefore routers do not forward them. Link-local addresses are from
the address blocks `169.254.0.0/16` or `fe80::/10`.

# Examples
```julia
filter(!islinklocaladdr, getipaddrs())
```
"""
function islinklocaladdr(addr::IPv4)
    # RFC 3927
    return (addr.host &
            0xFFFF0000) ==
            0xA9FE0000
end
function islinklocaladdr(addr::IPv6)
    # RFC 4291
    return (addr.host &
            0xFFC00000000000000000000000000000) ==
            0xFE800000000000000000000000000000
end
