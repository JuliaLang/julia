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
