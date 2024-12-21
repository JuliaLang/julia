# This file is a part of Julia. License is MIT: https://julialang.org/license

baremodule Base

using Core.Intrinsics, Core.IR

# to start, we're going to use a very simple definition of `include`
# that doesn't require any function (except what we can get from the `Core` top-module)
# start this big so that we don't have to resize before we have defined how to grow an array
const _included_files = Array{Tuple{Module,String},1}(Core.undef, 400)
setfield!(_included_files, :size, (1,))
function include(mod::Module, path::String)
    len = getfield(_included_files.size, 1)
    memlen = _included_files.ref.mem.length
    lenp1 = Core.add_int(len, 1)
    if len === memlen # by the time this is true we hopefully will have defined _growend!
        _growend!(_included_files, UInt(1))
    else
        setfield!(_included_files, :size, (lenp1,))
    end
    Core.memoryrefset!(Core.memoryref(_included_files.ref, lenp1), (mod, ccall(:jl_prepend_cwd, Any, (Any,), path)), :not_atomic, true)
    Core.println(path)
    ccall(:jl_uv_flush, Nothing, (Ptr{Nothing},), Core.io_pointer(Core.stdout))
    Core.include(mod, path)
end
include(path::String) = include(Base, path)

struct IncludeInto <: Function
    m::Module
end
(this::IncludeInto)(fname::AbstractString) = include(this.m, fname)

# from now on, this is now a top-module for resolving syntax
const is_primary_base_module = ccall(:jl_module_parent, Ref{Module}, (Any,), Base) === Core.Main
ccall(:jl_set_istopmod, Cvoid, (Any, Bool), Base, is_primary_base_module)

# The @inline/@noinline macros that can be applied to a function declaration are not available
# until after array.jl, and so we will mark them within a function body instead.
macro inline()   Expr(:meta, :inline)   end
macro noinline() Expr(:meta, :noinline) end

macro _boundscheck() Expr(:boundscheck) end

# Try to help prevent users from shooting them-selves in the foot
# with ambiguities by defining a few common and critical operations
# (and these don't need the extra convert code)
getproperty(x::Module, f::Symbol) = (@inline; getglobal(x, f))
getproperty(x::Type, f::Symbol) = (@inline; getfield(x, f))
setproperty!(x::Type, f::Symbol, v) = error("setfield! fields of Types should not be changed")
setproperty!(x::Array, f::Symbol, v) = error("setfield! fields of Array should not be changed")
getproperty(x::Tuple, f::Int) = (@inline; getfield(x, f))
setproperty!(x::Tuple, f::Int, v) = setfield!(x, f, v) # to get a decent error

getproperty(x, f::Symbol) = (@inline; getfield(x, f))
function setproperty!(x, f::Symbol, v)
    ty = fieldtype(typeof(x), f)
    val = v isa ty ? v : convert(ty, v)
    return setfield!(x, f, val)
end

typeof(function getproperty end).name.constprop_heuristic = Core.FORCE_CONST_PROP
typeof(function setproperty! end).name.constprop_heuristic = Core.FORCE_CONST_PROP

dotgetproperty(x, f) = getproperty(x, f)

getproperty(x::Module, f::Symbol, order::Symbol) = (@inline; getglobal(x, f, order))
function setproperty!(x::Module, f::Symbol, v, order::Symbol=:monotonic)
    @inline
    ty = Core.get_binding_type(x, f)
    val = v isa ty ? v : convert(ty, v)
    return setglobal!(x, f, val, order)
end
getproperty(x::Type, f::Symbol, order::Symbol) = (@inline; getfield(x, f, order))
setproperty!(x::Type, f::Symbol, v, order::Symbol) = error("setfield! fields of Types should not be changed")
getproperty(x::Tuple, f::Int, order::Symbol) = (@inline; getfield(x, f, order))
setproperty!(x::Tuple, f::Int, v, order::Symbol) = setfield!(x, f, v, order) # to get a decent error

getproperty(x, f::Symbol, order::Symbol) = (@inline; getfield(x, f, order))
function setproperty!(x, f::Symbol, v, order::Symbol)
    @inline
    ty = fieldtype(typeof(x), f)
    val = v isa ty ? v : convert(ty, v)
    return setfield!(x, f, val, order)
end

function swapproperty!(x, f::Symbol, v, order::Symbol=:not_atomic)
    @inline
    ty = fieldtype(typeof(x), f)
    val = v isa ty ? v : convert(ty, v)
    return Core.swapfield!(x, f, val, order)
end
function modifyproperty!(x, f::Symbol, op, v, order::Symbol=:not_atomic)
    @inline
    return Core.modifyfield!(x, f, op, v, order)
end
function replaceproperty!(x, f::Symbol, expected, desired, success_order::Symbol=:not_atomic, fail_order::Symbol=success_order)
    @inline
    ty = fieldtype(typeof(x), f)
    val = desired isa ty ? desired : convert(ty, desired)
    return Core.replacefield!(x, f, expected, val, success_order, fail_order)
end
function setpropertyonce!(x, f::Symbol, desired, success_order::Symbol=:not_atomic, fail_order::Symbol=success_order)
    @inline
    ty = fieldtype(typeof(x), f)
    val = desired isa ty ? desired : convert(ty, desired)
    return Core.setfieldonce!(x, f, val, success_order, fail_order)
end

function swapproperty!(x::Module, f::Symbol, v, order::Symbol=:not_atomic)
    @inline
    ty = Core.get_binding_type(x, f)
    val = v isa ty ? v : convert(ty, v)
    return Core.swapglobal!(x, f, val, order)
end
function modifyproperty!(x::Module, f::Symbol, op, v, order::Symbol=:not_atomic)
    @inline
    return Core.modifyglobal!(x, f, op, v, order)
end
function replaceproperty!(x::Module, f::Symbol, expected, desired, success_order::Symbol=:not_atomic, fail_order::Symbol=success_order)
    @inline
    ty = Core.get_binding_type(x, f)
    val = desired isa ty ? desired : convert(ty, desired)
    return Core.replaceglobal!(x, f, expected, val, success_order, fail_order)
end
function setpropertyonce!(x::Module, f::Symbol, desired, success_order::Symbol=:not_atomic, fail_order::Symbol=success_order)
    @inline
    ty = Core.get_binding_type(x, f)
    val = desired isa ty ? desired : convert(ty, desired)
    return Core.setglobalonce!(x, f, val, success_order, fail_order)
end

convert(::Type{Any}, Core.@nospecialize x) = x
convert(::Type{T}, x::T) where {T} = x
include("coreio.jl")

import Core: @doc, @__doc__, WrappedException, @int128_str, @uint128_str, @big_str, @cmd

# core docsystem
include("docs/core.jl")
Core.atdoc!(CoreDocs.docm)

eval(x) = Core.eval(Base, x)
eval(m::Module, x) = Core.eval(m, x)

include("exports.jl")
include("public.jl")

if false
    # simple print definitions for debugging. enable these if something
    # goes wrong during bootstrap before printing code is available.
    # otherwise, they just just eventually get (noisily) overwritten later
    global show, print, println
    show(io::IO, x) = Core.show(io, x)
    print(io::IO, a...) = Core.print(io, a...)
    println(io::IO, x...) = Core.println(io, x...)
end

"""
    time_ns() -> UInt64

Get the time in nanoseconds relative to some arbitrary time in the past. The primary use is for measuring the elapsed time
between two moments in time.
"""
time_ns() = ccall(:jl_hrtime, UInt64, ())

# A warning to be interpolated in the docstring of every dangerous mutating function in Base, see PR #50824
const _DOCS_ALIASING_WARNING = """
!!! warning
    Behavior can be unexpected when any mutated argument shares memory with any other argument.
"""

## Load essential files and libraries
include("essentials.jl")
include("ctypes.jl")
include("gcutils.jl")
include("generator.jl")
include("runtime_internals.jl")
include("options.jl")

# define invoke(f, T, args...; kwargs...), without kwargs wrapping
# to forward to invoke
function Core.kwcall(kwargs::NamedTuple, ::typeof(invoke), f, T, args...)
    @inline
    # prepend kwargs and f to the invoked from the user
    T = rewrap_unionall(Tuple{Core.Typeof(kwargs), Core.Typeof(f), (unwrap_unionall(T)::DataType).parameters...}, T)
    return invoke(Core.kwcall, T, kwargs, f, args...)
end
# invoke does not have its own call cache, but kwcall for invoke does
setfield!(typeof(invoke).name.mt, :max_args, 3, :monotonic) # invoke, f, T, args...

# define applicable(f, T, args...; kwargs...), without kwargs wrapping
# to forward to applicable
function Core.kwcall(kwargs::NamedTuple, ::typeof(applicable), @nospecialize(args...))
    @inline
    return applicable(Core.kwcall, kwargs, args...)
end
function Core._hasmethod(@nospecialize(f), @nospecialize(t)) # this function has a special tfunc (TODO: make this a Builtin instead like applicable)
    tt = rewrap_unionall(Tuple{Core.Typeof(f), (unwrap_unionall(t)::DataType).parameters...}, t)
    return Core._hasmethod(tt)
end

# core operations & types
include("promotion.jl")
include("tuple.jl")
include("expr.jl")
include("pair.jl")
include("traits.jl")
include("range.jl")
include("error.jl")

# core numeric operations & types
==(x, y) = x === y
include("bool.jl")
include("number.jl")
include("int.jl")
include("operators.jl")
include("pointer.jl")
include("refvalue.jl")
include("cmem.jl")

include("checked.jl")
using .Checked
function cld end
function fld end

# Lazy strings
include("strings/lazy.jl")

# array structures
include("indices.jl")
include("genericmemory.jl")
include("array.jl")
include("abstractarray.jl")
include("subarray.jl")
include("views.jl")
include("baseext.jl")

include("c.jl")
include("ntuple.jl")
include("abstractset.jl")
include("bitarray.jl")
include("bitset.jl")
include("abstractdict.jl")
include("iddict.jl")
include("idset.jl")
include("iterators.jl")
using .Iterators: zip, enumerate, only
using .Iterators: Flatten, Filter, product  # for generators
using .Iterators: Stateful    # compat (was formerly used in reinterpretarray.jl)
include("namedtuple.jl")

include("anyall.jl")

include("ordering.jl")
using .Order

include("coreir.jl")

# For OS specific stuff
# We need to strcat things here, before strings are really defined
function strcat(x::String, y::String)
    out = ccall(:jl_alloc_string, Ref{String}, (Csize_t,), Core.sizeof(x) + Core.sizeof(y))
    GC.@preserve x y out begin
        out_ptr = unsafe_convert(Ptr{UInt8}, out)
        unsafe_copyto!(out_ptr, unsafe_convert(Ptr{UInt8}, x), Core.sizeof(x))
        unsafe_copyto!(out_ptr + Core.sizeof(x), unsafe_convert(Ptr{UInt8}, y), Core.sizeof(y))
    end
    return out
end

BUILDROOT::String = ""
DATAROOT::String = ""

baremodule BuildSettings end

function process_sysimg_args!()
    let i = 2 # skip file name
        while i <= length(Core.ARGS)
            Core.println(Core.ARGS[i])
            if Core.ARGS[i] == "--buildsettings"
                include(BuildSettings, ARGS[i+1])
            elseif Core.ARGS[i] == "--buildroot"
                global BUILDROOT = Core.ARGS[i+1]
            elseif Core.ARGS[i] == "--dataroot"
                global DATAROOT = Core.ARGS[i+1]
            else
                error(strcat("invalid sysimage argument: ", Core.ARGS[i]))
            end
            i += 2
        end
    end
end
process_sysimg_args!()

function isready end

include(strcat(DATAROOT, "julia/Compiler/src/Compiler.jl"))

const _return_type = Compiler.return_type

# Enable compiler
Compiler.bootstrap!()

include("flparse.jl")
Core._setparser!(fl_parse)

# Further definition of Base will happen in Base.jl if loaded.

end # baremodule Base
