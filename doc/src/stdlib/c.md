# C Interface

```@docs
ccall
Core.Intrinsics.cglobal
Base.cfunction
Base.unsafe_convert
Base.cconvert
Base.unsafe_load
Base.unsafe_store!
Base.unsafe_copy!{T}(::Ptr{T}, ::Ptr{T}, ::Any)
Base.unsafe_copy!(::Array, ::Any, ::Array, ::Any, ::Any)
Base.copy!(::Any, ::Any)
Base.copy!(::Any, ::Any, ::Any, ::Any, ::Any)
Base.pointer
Base.unsafe_wrap{T,N}(::Union{Type{Array},Type{Array{T}},Type{Array{T,N}}}, ::Ptr{T}, ::NTuple{N,Int})
Base.pointer_from_objref
Base.unsafe_pointer_to_objref
Base.disable_sigint
Base.reenable_sigint
Base.systemerror
Core.Ptr
Core.Ref
Base.Cchar
Base.Cuchar
Base.Cshort
Base.Cushort
Base.Cint
Base.Cuint
Base.Clong
Base.Culong
Base.Clonglong
Base.Culonglong
Base.Cintmax_t
Base.Cuintmax_t
Base.Csize_t
Base.Cssize_t
Base.Cptrdiff_t
Base.Cwchar_t
Base.Cfloat
Base.Cdouble
```

# LLVM Interface

```@docs
Core.Intrinsics.llvmcall
```
