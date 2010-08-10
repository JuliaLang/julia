struct IOStream
    ios::Ptr{Void}

    global fdio, open, memio
    fdio(fd::Int) =
        new(ccall(dlsym(JuliaDLHandle,"jl_new_fdio"), Ptr{Void}, (Int32,),
                  int32(fd)))

    open(fname::String, rd::Bool, wr::Bool, cr::Bool, tr::Bool) =
        new(ccall(dlsym(JuliaDLHandle,"jl_new_fileio"), Ptr{Void},
                  (Ptr{Uint8}, Int32, Int32, Int32, Int32),
                  fname, int32(rd), int32(wr), int32(cr), int32(tr)))
    open(fname::String) = open(fname, true, true, true, false)

    memio() = memio(0)
    memio(x::Int) =
        new(ccall(dlsym(JuliaDLHandle,"jl_new_memio"), Ptr{Void}, (Uint32,),
                  uint32(x)))
end

nthbyte(x::Int, n::Int) = (n>sizeof(x) ? uint8(0) : uint8((x>>((n-1)<<3))))

write(s, x::Uint8) = error(strcat(string(typeof(s)),
                                  " does not support byte I/O"))

function write(s, x::Int)
    for n=1:sizeof(x)
        write(s, nthbyte(x, n))
    end
end

write(s, x::Bool)    = write(s, uint8(x))
write(s, x::Float32) = write(s, boxui32(unbox32(x)))
write(s, x::Float64) = write(s, boxui64(unbox64(x)))

function write(s, a::Array)
    for i=1:numel(a)
        write(s, a[i])
    end
end

read(s, x::Type{Uint8}) = error(strcat(string(typeof(s)),
                                       " does not support byte I/O"))

function read{T <: Int}(s, ::Type{T})
    x = zero(T)
    for n=1:sizeof(x)
        x |= (convert(T,read(s,Uint8))<<((n-1)<<3))
    end
    x
end

read(s, ::Type{Bool})    = (read(s,Uint8)!=0)
read(s, ::Type{Float32}) = boxf32(unbox32(read(s,Int32)))
read(s, ::Type{Float64}) = boxf64(unbox64(read(s,Int64)))

read{T}(s, t::Type{T}, dims::Tuple) = read(s, t, dims...)

function read{T}(s, ::Type{T}, dims::Size...)
    a = Array(T, dims...)
    for i=1:numel(a)
        a[i] = read(s, T)
    end
    a
end

function write(s::IOStream, b::Uint8)
    ccall(dlsym(JuliaDLHandle,"ios_putc"), Int32, (Int32, Ptr{Void}),
          int32(b), s.ios)
end

function read(s::IOStream, ::Type{Uint8})
    b = ccall(dlsym(JuliaDLHandle,"ios_getc"), Int32, (Ptr{Void},), s.ios)
    if b == -1
        error("read: end of file")
    end
    uint8(b)
end

function close(s::IOStream)
    ccall(dlsym(JuliaDLHandle,"ios_close"), Void, (Ptr{Void},), s.ios)
end

function flush(s::IOStream)
    ccall(dlsym(JuliaDLHandle,"ios_flush"), Void, (Ptr{Void},), s.ios)
end

struct IOTally
    nbytes::Size
    IOTally() = new(zero(Size))
end

write(s::IOTally, x::Uint8) = (s.nbytes += 1; ())
flush(s::IOTally) = ()

## serializing values ##

ser_tag = idtable()
(()->begin
 global ser_tag
 i = 1
 for t = {Any, Symbol, Bool, Int8, Uint8, Int16, Uint16, Int32, Uint32,
          Int64, Uint64, Float32, Float64,
          TagKind, UnionKind, StructKind, Tuple, Array}
     ser_tag[t] = i
     i += 1
 end
end)()

writetag(s, x) = write(s, uint8(ser_tag[x]))

serialize(s, x::Bool) = (writetag(s,Bool); write(s, uint8(x)))

function serialize(s, t::Tuple)
    writetag(s, Tuple)
    write(s, int32(length(t)))
    for i=1:length(t)
        serialize(s, t[i])
    end
end

function serialize(s, x::Symbol)
    writetag(s, Symbol)
    name = string(x)
    write(s, int32(length(name)))
    write(s, name)
end

function serialize(s, a::Array)
    writetag(s, Array)
    elty = typeof(a).parameters[1]
    serialize(s, elty)
    serialize(s, a.dims)
    if isa(elty,BitsKind)
        write(s, a)
    else
        for i=1:numel(a)
            serialize(s, a[i])
        end
    end
end

function serialize(s, t::TagKind)
    if has(ser_tag,t)
        write(s, uint8(0))
        writetag(s, t)
    else
        writetag(s, TagKind)
        serialize(s, t.name.name)
        serialize(s, t.parameters)
    end
end

function serialize(s, x)
    if has(ser_tag,x)
        write(s, uint8(0))  # tag 0 indicates just a tag
        writetag(s, x)
        return ()
    end
    t = typeof(x)
    if isa(t,BitsKind)
        writetag(s, t)
        write(s, x)
    elseif isa(t,StructKind)
        writetag(s, StructKind)
        serialize(s, t.name.name)
        serialize(s, t.parameters)
        for n = t.names
            serialize(s, getfield(x, n))
        end
    end
end

## deserializing values ##

deser_tag = idtable()
for (k, v) = ser_tag
    deser_tag[v] = k
end

function deserialize(s)
    b = int32(read(s, Uint8))
    if b == 0
        return deser_tag[int32(read(s, Uint8))]
    end
    tag = deser_tag[b]
    if is(tag,Tuple)
        len = read(s, Int32)
        return deserialize_tuple(s, len)
    end
    return deserialize(s, tag)
end

deserialize(s, t::BitsKind) = read(s, t)

function deserialize_tuple(s, len, elts...)
    if len == 0
        elts
    else
        deserialize_tuple(s, len-1, elts..., deserialize(s))
    end
end

deserialize(s, ::Type{Symbol}) = symbol(read(s, Uint8, read(s, Int32)))

function deserialize(s, ::Type{Array})
    elty = deserialize(s)
    dims = deserialize(s)
    if isa(elty,BitsKind)
        return read(s, elty, dims...)
    end
    A = Array(elty, dims...)
    for i = 1:numel(A)
        A[i] = deserialize(s)
    end
    return A
end

function deserialize(s, ::Type{TagKind})
    name = deserialize(s)::Symbol
    params = deserialize(s)
    instantiate_type(eval(name), params...)
end

function deserialize(s, ::Type{StructKind})
    name = deserialize(s)::Symbol
    params = deserialize(s)
    t = eval(name)
    for n = t.names
        # TODO: need a way to construct object by reflection
        deserialize(s)
    end
end
