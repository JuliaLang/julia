# This file is a part of Julia. License is MIT: https://julialang.org/license

if isdefined(Base, :end_base_include) && !isdefined(Base, :Compiler)

# Define a dummy `Compiler` module to make it installable even on Julia versions where
# Compiler.jl is not available as a standard library.
@eval module Compiler
    function __init__()
        println("""
        The `Compiler` standard library is not available for this version of Julia.
        Use Julia version `v"1.12.0-DEV.1581"` or later.
        """)
    end
end

# When generating an incremental precompile file, we first check whether we
# already have a copy of this *exact* code in the system image. If so, we
# simply generates a pkgimage that has the dependency edges we recorded in
# the system image and simply returns that copy of the compiler. If not,
# we proceed to load/precompile this as an ordinary package.
elseif (isdefined(Base, :generating_output) && Base.generating_output(true) &&
        Base.samefile(joinpath(Sys.BINDIR, Base.DATAROOTDIR, Base._compiler_require_dependencies[1][2]), @eval @__FILE__) &&
        !Base.any_includes_stale(
            map(Base.compiler_chi, Base._compiler_require_dependencies),
            "sysimg", nothing))

    Base.prepare_compiler_stub_image!()
    append!(Base._require_dependencies, map(Base.expand_compiler_path, Base._compiler_require_dependencies))
    # There isn't much point in precompiling native code - downstream users will
    # specialize their own versions of the compiler code and we don't activate
    # the compiler by default anyway, so let's save ourselves some disk space.
    ccall(:jl_suppress_precompile, Cvoid, (Cint,), 1)

else

@eval baremodule Compiler

# Needs to match UUID defined in Project.toml
ccall(:jl_set_module_uuid, Cvoid, (Any, NTuple{2, UInt64}), Compiler,
    (0x807dbc54_b67e_4c79, 0x8afb_eafe4df6f2e1))

using Core.Intrinsics, Core.IR

using Core: ABIOverride, Builtin, CodeInstance, IntrinsicFunction, MethodInstance, MethodMatch,
    MethodTable, PartialOpaque, SimpleVector, TypeofVararg,
    _apply_iterate, apply_type, compilerbarrier, donotdelete, memoryref_isassigned,
    memoryrefget, memoryrefnew, memoryrefoffset, memoryrefset!, print, println, show, svec,
    typename, unsafe_write, write

using Base
using Base: @_foldable_meta, @_gc_preserve_begin, @_gc_preserve_end, @nospecializeinfer,
    PARTITION_KIND_GLOBAL, PARTITION_KIND_UNDEF_CONST, PARTITION_KIND_BACKDATED_CONST, PARTITION_KIND_DECLARED,
    PARTITION_FLAG_DEPWARN,
    Base, BitVector, Bottom, Callable, DataTypeFieldDesc,
    EffectsOverride, Filter, Generator, IteratorSize, JLOptions, NUM_EFFECTS_OVERRIDES,
    OneTo, Ordering, RefValue, SizeUnknown, _NAMEDTUPLE_NAME,
    _array_for, _bits_findnext, _methods_by_ftype, _uniontypes, all, allocatedinline, any,
    argument_datatype, binding_kind, cconvert, copy_exprargs, datatype_arrayelem,
    datatype_fieldcount, datatype_fieldtypes, datatype_layoutsize, datatype_nfields,
    datatype_pointerfree, decode_effects_override, diff_names, fieldindex,
    generating_output, get_nospecializeinfer_sig, get_world_counter, has_free_typevars,
    hasgenerator, hasintersect, indexed_iterate, isType, is_file_tracked, is_function_def,
    is_meta_expr, is_meta_expr_head, is_nospecialized, is_nospecializeinfer, is_defined_const_binding,
    is_some_const_binding, is_some_guard, is_some_imported, is_valid_intrinsic_elptr,
    isbitsunion, isconcretedispatch, isdispatchelem, isexpr, isfieldatomic, isidentityfree,
    iskindtype, ismutabletypename, ismutationfree, issingletontype, isvarargtype, isvatuple,
    kwerr, lookup_binding_partition, may_invoke_generator, methods, midpoint, moduleroot,
    partition_restriction, quoted, rename_unionall, rewrap_unionall, specialize_method,
    structdiff, tls_world_age, unconstrain_vararg_length, unionlen, uniontype_layout,
    uniontypes, unsafe_convert, unwrap_unionall, unwrapva, vect, widen_diagonal,
    _uncompressed_ir, maybe_add_binding_backedge!, datatype_min_ninitialized,
    partialstruct_init_undefs, fieldcount_noerror
using Base.Order

import Base: ==, _topmod, append!, convert, copy, copy!, findall, first, get, get!,
    getindex, haskey, in, isempty, isready, iterate, iterate, last, length, max_world,
    min_world, popfirst!, push!, resize!, setindex!, size, intersect

const getproperty = Core.getfield
const setproperty! = Core.setfield!
const swapproperty! = Core.swapfield!
const modifyproperty! = Core.modifyfield!
const replaceproperty! = Core.replacefield!
const _DOCS_ALIASING_WARNING = ""

ccall(:jl_set_istopmod, Cvoid, (Any, Bool), Compiler, false)

eval(x) = Core.eval(Compiler, x)
eval(m, x) = Core.eval(m, x)

function include(x::String)
    if !isdefined(Base, :end_base_include)
        # During bootstrap, all includes are relative to `base/`
        x = Base.strcat(Base.strcat(Base.DATAROOT, "julia/Compiler/src/"), x)
    end
    Base.include(Compiler, x)
end

function include(mod::Module, x::String)
    if !isdefined(Base, :end_base_include)
        x = Base.strcat(Base.strcat(Base.DATAROOT, "julia/Compiler/src/"), x)
    end
    Base.include(mod, x)
end

macro _boundscheck() Expr(:boundscheck) end

function return_type end
function is_return_type(Core.@nospecialize(f))
    f === return_type && return true
    if isdefined(Base, :Compiler) && Compiler !== Base.Compiler
        # Also model the return_type function of the builtin Compiler the same.
        # This isn't completely sound. We don't actually have any idea what the
        # base compiler will do at runtime. In the fullness of time, we should
        # re-work the semantics to make the cache primary and thus avoid having
        # to reason about what the compiler may do at runtime, but we're not
        # fully there yet.
        return f === Base.Compiler.return_type
    end
    return false
end

include("sort.jl")

# We don't include some.jl, but this definition is still useful.
something(x::Nothing, y...) = something(y...)
something(x::Any, y...) = x

############
# compiler #
############

baremodule BuildSettings
using Core: ARGS, include
using ..Compiler: >, getindex, length

global MAX_METHODS::Int = 3

if length(ARGS) > 2 && ARGS[2] === "--buildsettings"
    include(BuildSettings, ARGS[3])
end
end

if !isdefined(Base, :end_base_include)
    macro show(ex...)
        blk = Expr(:block)
        for s in ex
            push!(blk.args, :(println(stdout, $(QuoteNode(s)), " = ",
                                              begin local value = $(esc(s)) end)))
        end
        isempty(ex) || push!(blk.args, :value)
        blk
    end
else
    using Base: @show
end

include("cicache.jl")
include("methodtable.jl")
include("effects.jl")
include("types.jl")
include("utilities.jl")
include("validation.jl")

include("ssair/basicblock.jl")
include("ssair/domtree.jl")
include("ssair/ir.jl")
include("ssair/tarjan.jl")

include("abstractlattice.jl")
include("stmtinfo.jl")
include("inferenceresult.jl")
include("inferencestate.jl")

include("typeutils.jl")
include("typelimits.jl")
include("typelattice.jl")
include("tfuncs.jl")

include("abstractinterpretation.jl")
include("typeinfer.jl")
include("optimize.jl")

include("bootstrap.jl")
include("reflection_interface.jl")
include("opaque_closure.jl")

macro __SOURCE_FILE__()
    __source__.file === nothing && return nothing
    return QuoteNode(__source__.file::Symbol)
end

module IRShow end # relies on string and IO operations defined in Base
baremodule TrimVerifier end # relies on IRShow, so define this afterwards

if isdefined(Base, :end_base_include)
    # When this module is loaded as the standard library, include these files as usual
    include(IRShow, "ssair/show.jl")
    include(TrimVerifier, "verifytrim.jl")
else
    function load_irshow!()
        Base.delete_method(Base.which(verify_typeinf_trim, (IO, Vector{Any}, Bool)),)
        include(IRShow, "ssair/show.jl")
        include(TrimVerifier, "verifytrim.jl")
    end
    function verify_typeinf_trim(io::IO, codeinfos::Vector{Any}, onlywarn::Bool)
        # stub implementation
        msg = "--trim verifier not defined"
        onlywarn ? println(io, msg) : error(msg)
    end
    # During bootstrap, skip including these files and defer to base/show.jl to include it later
end

end # baremodule Compiler

end # if isdefined(Base, :generating_output) && ...
