# C Interface

```@docs
ccall
Core.Intrinsics.cglobal
Base.@cfunction
Base.CFunction
Base.unsafe_convert
Base.cconvert
Base.unsafe_load
Base.unsafe_store!
Base.unsafe_copyto!{T}(::Ptr{T}, ::Ptr{T}, ::Any)
Base.unsafe_copyto!{T}(::Array{T}, ::Any, ::Array{T}, ::Any, ::Any)
Base.copyto!
Base.pointer
Base.unsafe_wrap{T,N}(::Union{Type{Array},Type{Array{T}},Type{Array{T,N}}}, ::Ptr{T}, ::NTuple{N,Int})
Base.pointer_from_objref
Base.unsafe_pointer_to_objref
Base.disable_sigint
Base.reenable_sigint
Base.exit_on_sigint
Base.systemerror
Base.windowserror
Core.Ptr
Core.Ref
Base.Cchar
Base.Cuchar
Base.Cshort
Base.Cstring
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
Base.Cwstring
Base.Cfloat
Base.Cdouble
```

# LLVM Interface

```@docs
Core.Intrinsics.llvmcall
```
