# This file is a part of Julia. License is MIT: https://julialang.org/license

struct DNSError <: Exception
    host::String
    code::Int32
end

function show(io::IO, err::DNSError)
    print(io, "DNSError: ", err.host, ", ", Base.struverror(err.code),
                                      " (", Base.uverrorname(err.code), ")")
end

function uv_getaddrinfocb(req::Ptr{Cvoid}, status::Cint, addrinfo::Ptr{Cvoid})
    data = uv_req_data(req)
    if data != C_NULL
        t = unsafe_pointer_to_objref(data)::Task
        uv_req_set_data(req, C_NULL)
        if status != 0 || addrinfo == C_NULL
            schedule(t, UVError("getaddrinfocb", status))
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
    else
        # no owner for this req, safe to just free it
        Libc.free(req)
    end
    nothing
end

"""
    getalladdrinfo(host::AbstractString) -> Vector{IPAddr}

Gets all of the IP addresses of the `host`.
Uses the operating system's underlying getaddrinfo implementation, which may do a DNS lookup.
"""
function getalladdrinfo(host::String)
    isascii(host) || error("non-ASCII hostname: $host")
    req = Libc.malloc(Base._sizeof_uv_getaddrinfo)
    uv_req_set_data(req, C_NULL) # in case we get interrupted before arriving at the wait call
    status = ccall(:jl_getaddrinfo, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Ptr{Cvoid}, Ptr{Cvoid}),
                   eventloop(), req, host, #=service=#C_NULL, uv_jl_getaddrinfocb::Ptr{Cvoid})
    if status < 0
        Libc.free(req)
        if status == UV_EINVAL
            throw(ArgumentError("Invalid getaddrinfo argument"))
        elseif status == UV_ENOMEM || status == UV_ENOBUFS
            throw(OutOfMemoryError())
        end
        uv_error("getaddrinfo", status)
    end
    ct = current_task()
    preserve_handle(ct)
    r = try
        uv_req_set_data(req, ct)
        wait()
    finally
        if uv_req_data(req) != C_NULL
            # req is still alive,
            # so make sure we don't get spurious notifications later
            uv_req_set_data(req, C_NULL)
            ccall(:uv_cancel, Int32, (Ptr{Cvoid},), req) # try to let libuv know we don't care anymore
        else
            # done with req
            Libc.free(req)
        end
        unpreserve_handle(ct)
    end
    if isa(r, UVError)
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
            throw(UVError("getaddrinfo", code))
        end
    end
    return r::Vector{IPAddr}
end
getalladdrinfo(host::AbstractString) = getalladdrinfo(String(host))

"""
    getalladdrinfo(host::AbstractString, IPAddr=IPv4) -> IPAddr

Gets the first IP address of the `host` of the specified `IPAddr` type.
Uses the operating system's underlying getaddrinfo implementation, which may do a DNS lookup.
"""
function getaddrinfo(host::String, T::Type{<:IPAddr})
    addrs = getalladdrinfo(host)
    for addr in addrs
        if addr isa T
            return addr
        end
    end
    throw(DNSError(host, UV_EAI_NONAME))
end
getaddrinfo(host::AbstractString, T::Type{<:IPAddr}) = getaddrinfo(String(host), T)
getaddrinfo(host::AbstractString) = getaddrinfo(String(host), IPv4)

function uv_getnameinfocb(req::Ptr{Cvoid}, status::Cint, hostname::Cstring, service::Cstring)
    data = uv_req_data(req)
    if data != C_NULL
        t = unsafe_pointer_to_objref(data)::Task
        uv_req_set_data(req, C_NULL)
        if status != 0
            schedule(t, UVError("getnameinfocb", status))
        else
            schedule(t, unsafe_string(hostname))
        end
    else
        # no owner for this req, safe to just free it
        Libc.free(req)
    end
    nothing
end

"""
    getnameinfo(host::IPAddr) -> String

Performs a reverse-lookup for IP address to return a hostname and service
using the operating system's underlying getnameinfo implementation.
"""
function getnameinfo(address::Union{IPv4, IPv6})
    req = Libc.malloc(Base._sizeof_uv_getnameinfo)
    uv_req_set_data(req, C_NULL) # in case we get interrupted before arriving at the wait call
    ev = eventloop()
    port = hton(UInt16(0))
    flags = 0
    uvcb = uv_jl_getnameinfocb::Ptr{Cvoid}
    status = UV_EINVAL
    if address isa IPv4
        status = ccall(:jl_getnameinfo, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, UInt32, UInt16, Cint, Ptr{Cvoid}),
                       ev, req, hton(address.host), port, flags, uvcb)
    elseif address isa IPv6
        status = ccall(:jl_getnameinfo6, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, Ref{UInt128}, UInt16, Cint, Ptr{Cvoid}),
                       ev, req, hton(address.host), port, flags, uvcb)
    end
    if status < 0
        Libc.free(req)
        if status == UV_EINVAL
            throw(ArgumentError("Invalid getnameinfo argument"))
        elseif status == UV_ENOMEM || status == UV_ENOBUFS
            throw(OutOfMemoryError())
        end
        uv_error("getnameinfo", status)
    end
    ct = current_task()
    preserve_handle(ct)
    r = try
        uv_req_set_data(req, ct)
        wait()
    finally
        if uv_req_data(req) != C_NULL
            # req is still alive,
            # so make sure we don't get spurious notifications later
            uv_req_set_data(req, C_NULL)
            ccall(:uv_cancel, Int32, (Ptr{Cvoid},), req) # try to let libuv know we don't care anymore
        else
            # done with req
            Libc.free(req)
        end
        unpreserve_handle(ct)
    end
    if isa(r, UVError)
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
            throw(UVError("getnameinfo", code))
        end
    end
    return r::String
end

const _sizeof_uv_interface_address = ccall(:jl_uv_sizeof_interface_address,Int32,())

"""
    getipaddr() -> IPAddr

Get the IP address of the local machine.
"""
function getipaddr()
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
            continue
        end
        sockaddr = ccall(:jl_uv_interface_address_sockaddr, Ptr{Cvoid}, (Ptr{UInt8},), current_addr)
        if ccall(:jl_sockaddr_in_is_ip4, Int32, (Ptr{Cvoid},), sockaddr) == 1
            rv = IPv4(ntoh(ccall(:jl_sockaddr_host4, UInt32, (Ptr{Cvoid},), sockaddr)))
            ccall(:uv_free_interface_addresses, Cvoid, (Ptr{UInt8}, Int32), addr, count)
            return rv
        # Uncomment to enbable IPv6
        #elseif ccall(:jl_sockaddr_in_is_ip6, Int32, (Ptr{Cvoid},), sockaddr) == 1
        #   host = Vector{UInt128}(undef, 1)
        #   ccall(:jl_sockaddr_host6, UInt32, (Ptr{Cvoid}, Ptr{UInt128}), sockaddrr, host)
        #   return IPv6(ntoh(host[1]))
        end
    end
    ccall(:uv_free_interface_addresses, Cvoid, (Ptr{UInt8}, Int32), addr, count)
    return lo_present ? localhost : error("No networking interface available")
end
