# OS-specific code

@unix_only

type SharedMemory
    stream :: IOStream
end

function open_shm(name::String, size::Integer, create::Bool, readonly::Bool)
    oflags = (create ? JL_O_CREAT : 0) | (readonly ? O_RDONLY : O_RDWR)
    fd = ccall(:shm_open, Cint, (Ptr{Uint8}, Cint, Cint), 
                 name, oflags, S_IRUSR | S_IWUSR)
    if create && ccall(:ftruncate, Cint, (Cint, Cint), fd, size) != 0
        error("unable to ftruncate shared memory segment " * name)
    end
    SharedMemory(fdio(name, fd, true))
end

function unlink_shm(name::String)
    if ccall(:shm_unlink, Cint, (Ptr{Uint8},), name) != 0
        error("unable to unlink shared memory segment " * name)
    end
end

function mmap_array_shm{T,N}(::Type{T}, dims::NTuple{N,Integer}, shm::SharedMemory, offset::FileOffset)
    mmap_array(T, dims, shm.stream, offset, grow=false)
end

end # @unix_only

@windows_only

const SHM_GRANULARITY::Int = ccall(:jl_getallocationgranularity, Clong, ())

type SharedMemory
    handle :: Ptr{Void}
    readonly :: Bool
end

function open_shm(name::String, size::Integer, create::Bool, readonly::Bool)
    
    if size < 0
        error("requested size is negative")
    end
    if size > typemax(Int)-SHM_GRANULARITY
        error("size is too large to memory-map on this platform")
    end
    
    if create
        ro = readonly ? 0x02 : 0x04
        sz = convert(Csize_t, size)
        szhi = sz>>32
        szlo = sz&typemax(Uint32)
        hdl = ccall(:CreateFileMappingA, stdcall, Ptr{Void}, 
                    (Ptr{Void}, Ptr{Void}, Cint, Cint, Cint, Ptr{Uint8}),
                    -1, C_NULL, ro, szhi, szlo, name)
    else
        ro = readonly ? 4 : 2
        hdl = ccall(:OpenFileMappingA, stdcall, Ptr{Void},
                    (Cint, Cint, Ptr{Uint8}),
                    ro, true, name)
    end
    if hdl == C_NULL
        error("could not create shared memory object: $(FormatMessage())")
    end
    shm = SharedMemory(hdl)

    function finalizer_closure()
        if !bool(ccall(:CloseHandle, stdcall, Cint, (Ptr{Void},), hdl))
            error("could not close shared memory object: $(FormatMessage())")
        end
    end
    finalizer(shm,x->finalizer_closure())

    shm
end

function unlink_shm(name::String)
    # no-op in windows
end

function mmap_array_shm{T,N}(::Type{T}, dims::NTuple{N,Integer}, shm::SharedMemory, offset::FileOffset)
    len = prod(dims)*sizeof(T)
    pgoff::FileOffset = div(offset, SHM_GRANULARITY)*SHM_GRANULARITY
    szf = convert(Csize_t, len+offset)
    sza = szf - convert(Csize_t, pgoff)
    ro = shm.readonly ? 4 : 2
    pgoffhi = pgoff>>32
    pgofflo = pgoff&typemax(Uint32)
    hdl = ccall(:MapViewOfFile, stdcall, Ptr{Void},
                (Ptr{Void}, Cint, Cint, Csize_t),
                shm.handle, ro, pgoffhi, pgofflo, sza)
    if hdl == C_NULL
        error("could not create mapping view: $(FormatMessage())")
    end
    
    A = pointer_to_array(convert(Ptr{T}, hdl+offset-pgoff), dims)

    function finalizer_closure()
        if !bool(ccall(:UnmapViewOfFile, stdcall, Cint, (Ptr{Void},), hdl))
            error("could not unmap view: $(FormatMessage())")
        end
    end
    finalizer(A,x->finalizer_closure())

    A
end

end # @windows_only



