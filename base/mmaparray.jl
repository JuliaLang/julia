function munmap(p,len)
    ret = ccall(:munmap,Int,(Ptr{Void},Int),p,len)
    if ret != 0
        error(strerror())
    end
end

type MmapArray{T,N} <: AbstractArray{T,N}
    a::Array{T,N}
end
function MmapArray{T,N}(::Type{T},dims::Dims,s::IOStream,opts::Options)
    # Are all IOstreams mappable??
    # Is there a way to get the read/write mode from s?
    const PROT_READ::Int = 1
    const PROT_WRITE::Int = 2
    const MAP_SHARED::Int = 1
    const MAP_PRIVATE::Int = 2
    const MAP_ANONYMOUS::Int = 32
    @defaults opts rwmode="r" sharemode="p" offset=-1
    @check_used opts
    if offset == -1
        offset = position(s)
    end
    if rwmode == "r"
        prot = PROT_READ
    elseif rwmode == "w"
        prot = PROT_WRITE
    elseif rwmode == "rw" || rwmode == "wr"
        prot = PROT_READ | PROT_WRITE
    else
        error("Read/write mode not recognized")
    end
    sharemode = sharemode[1]
    if sharemode == 'p'
        flags = MAP_PRIVATE
    elseif sharemode == 's'
        flags = MAP_SHARED
    elseif sharemode = 'a'
        flags = MAP_ANONYMOUS
    else
        error("Sharing mode not recognized")
    end
    #Do we need pagesize = int(ccall(:jl_get_pagesize, Int32, ()))?
    #  (which just calls sysconf(_SC_PAGE_SIZE))
    pagesize = 4096
    offset_page = ifloor(offset/pagesize)*pagesize
    len::Int = prod(dims)*sizeof(T) + (offset-offset_page)
    p = ccall(:mmap,Ptr{Void},(Ptr{Void},Int,Int,Int,Int,FileOffset),C_NULL,len,prot,flags,fd(s),offset_page)
    println(p)
    if convert(Int,p) < 1
        println("Memory mapping failed")
        error(strerror())
    end
    # Note: need new array declaration syntax. Naively, this would
    # just create a new jl_array_t object, but instead of allocating
    # memory for "data", it would just use the supplied pointer. My
    # main concern is, what happens upon garbage collection? Do we
    # need a new "allocated" flag in jl_array_t? I don't understand
    # JL_GC_PUSH and this makes me a bit reluctant to dive into
    # something so fundamental...
    ret = MmapArray(Array(T,p+(offset-offset_page),dims...))
    finalizer(ret,x->munmap(p,len))
end

ref(MA::MmapArray,ind...) = ref(MA.a,ind...)
assign(MA::MmapArray,X,ind...) = assign(MA.a,X,ind...)
length(MA::MmapArray) = length(MA.a)
size(MA::MmapArray) = size(MA.a)
numel(MA::MmapArray) = numel(MA.a)
eltype(MA::MmapArray) = eltype(MA.a)
