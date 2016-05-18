# This file is a part of Julia. License is MIT: http://julialang.org/license

# name and module reflection
module_name(m::Module) = ccall(:jl_module_name, Ref{Symbol}, (Any,), m)
module_parent(m::Module) = ccall(:jl_module_parent, Ref{Module}, (Any,), m)
current_module() = ccall(:jl_get_current_module, Ref{Module}, ())

function fullname(m::Module)
    m === Main && return ()
    m === Base && return (:Base,)  # issue #10653
    mn = module_name(m)
    mp = module_parent(m)
    if mp === m
        # not Main, but is its own parent, means a prior Main module
        n = ()
        this = Main
        while this !== m
            if isdefined(this, :LastMain)
                n = tuple(n..., :LastMain)
                this = this.LastMain
            else
                error("no reference to module ", mn)
            end
        end
        return n
    end
    return tuple(fullname(mp)..., mn)
end

names(m::Module, all::Bool, imported::Bool) = sort!(ccall(:jl_module_names, Array{Symbol,1}, (Any,Int32,Int32), m, all, imported))
names(m::Module, all::Bool) = names(m, all, false)
names(m::Module) = names(m, false, false)

isexported(m::Module, s::Symbol) = ccall(:jl_module_exports_p, Cint, (Any, Any), m, s)!=0

function isbindingresolved(m::Module, var::Symbol)
    ccall(:jl_binding_resolved_p, Cint, (Any, Any), m, var) != 0
end

binding_module(s::Symbol) = binding_module(current_module(), s)
function binding_module(m::Module, s::Symbol)
    p = ccall(:jl_get_module_of_binding, Ptr{Void}, (Any, Any), m, s)
    p == C_NULL && return m
    return unsafe_pointer_to_objref(p)::Module
end

function resolve(g::GlobalRef; force::Bool=false)
    if force || isbindingresolved(g.mod, g.name)
        return GlobalRef(binding_module(g.mod, g.name), g.name)
    end
    return g
end

"""
    fieldname(x::DataType, i)

Get the name of field `i` of a `DataType`.
"""
fieldname(t::DataType, i::Integer) = t.name.names[i]::Symbol
fieldname{T<:Tuple}(t::Type{T}, i::Integer) = i < 1 || i > nfields(t) ? throw(BoundsError(t, i)) : Int(i)

"""
    fieldnames(x::DataType)

Get an array of the fields of a `DataType`.
"""
function fieldnames(v)
    t = typeof(v)
    if !isa(t,DataType)
        throw(ArgumentError("cannot call fieldnames() on a non-composite type"))
    end
    return fieldnames(t)
end
fieldnames(t::DataType) = Symbol[fieldname(t, n) for n in 1:nfields(t)]
fieldnames{T<:Tuple}(t::Type{T}) = Int[n for n in 1:nfields(t)]

isconst(s::Symbol) = ccall(:jl_is_const, Int32, (Ptr{Void}, Any), C_NULL, s) != 0

isconst(m::Module, s::Symbol) =
    ccall(:jl_is_const, Int32, (Any, Any), m, s) != 0

# return an integer such that object_id(x)==object_id(y) if is(x,y)
object_id(x::ANY) = ccall(:jl_object_id, UInt, (Any,), x)

immutable DataTypeLayout
    nfields::UInt32
    alignment::UInt32
    # alignment : 28;
    # haspadding : 1;
    # pointerfree : 1;
    # fielddesc_type : 2;
end


# type predicates
datatype_alignment(dt::DataType) = dt.layout == C_NULL ? throw(UndefRefError()) :
    Int(unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).alignment & 0x0FFFFFFF)

datatype_haspadding(dt::DataType) = dt.layout == C_NULL ? throw(UndefRefError()) :
    (unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).alignment >> 28) & 1 == 1

datatype_pointerfree(dt::DataType) = dt.layout == C_NULL ? throw(UndefRefError()) :
    (unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).alignment >> 29) & 1 == 1

datatype_fielddesc_type(dt::DataType) = dt.layout == C_NULL ? throw(UndefRefError()) :
    (unsafe_load(convert(Ptr{DataTypeLayout}, dt.layout)).alignment >> 30) & 3

isimmutable(x::ANY) = (@_pure_meta; (isa(x,Tuple) || !typeof(x).mutable))
isstructtype(t::DataType) = (@_pure_meta; nfields(t) != 0 || (t.size==0 && !t.abstract))
isstructtype(x) = (@_pure_meta; false)
isbits(t::DataType) = (@_pure_meta; !t.mutable & (t.layout != C_NULL) && datatype_pointerfree(t))
isbits(t::Type) = (@_pure_meta; false)
isbits(x) = (@_pure_meta; isbits(typeof(x)))
isleaftype(t::ANY) = (@_pure_meta; isa(t, DataType) && t.isleaftype)

typeintersect(a::ANY,b::ANY) = (@_pure_meta; ccall(:jl_type_intersection, Any, (Any,Any), a, b))
typeseq(a::ANY,b::ANY) = (@_pure_meta; a<:b && b<:a)

"""
    fieldoffset(type, i)

The byte offset of field `i` of a type relative to the data start. For example, we could
use it in the following manner to summarize information about a struct type:

```jldoctest
julia> structinfo(T) = [(fieldoffset(T,i), fieldname(T,i), fieldtype(T,i)) for i = 1:nfields(T)];

julia> structinfo(Base.Filesystem.StatStruct)
12-element Array{Tuple{UInt64,Symbol,Type{_}},1}:
 (0x0000000000000000,:device,UInt64)
 (0x0000000000000008,:inode,UInt64)
 (0x0000000000000010,:mode,UInt64)
 (0x0000000000000018,:nlink,Int64)
 (0x0000000000000020,:uid,UInt64)
 (0x0000000000000028,:gid,UInt64)
 (0x0000000000000030,:rdev,UInt64)
 (0x0000000000000038,:size,Int64)
 (0x0000000000000040,:blksize,Int64)
 (0x0000000000000048,:blocks,Int64)
 (0x0000000000000050,:mtime,Float64)
 (0x0000000000000058,:ctime,Float64)
```
"""
fieldoffset(x::DataType, idx::Integer) = (@_pure_meta; ccall(:jl_get_field_offset, Csize_t, (Any, Cint), x, idx))

"""
    fieldtype(T, name::Symbol | index::Int)

Determine the declared type of a field (specified by name or index) in a composite DataType `T`.
"""
fieldtype

type_alignment(x::DataType) = (@_pure_meta; ccall(:jl_get_alignment, Csize_t, (Any,), x))

# return all instances, for types that can be enumerated
function instances end

# subtypes
function _subtypes(m::Module, x::DataType, sts=Set(), visited=Set())
    push!(visited, m)
    for s in names(m,true)
        if isdefined(m,s)
            t = eval(m,s)
            if isa(t, DataType) && t.name.name == s && supertype(t).name == x.name
                ti = typeintersect(t, x)
                ti != Bottom && push!(sts, ti)
            elseif isa(t, Module) && !in(t, visited)
                _subtypes(t, x, sts, visited)
            end
        end
    end
    sts
end
subtypes(m::Module, x::DataType) = sort(collect(_subtypes(m, x)), by=string)
subtypes(x::DataType) = subtypes(Main, x)

# function reflection
function_name(f::Function) = typeof(f).name.mt.name

function to_tuple_type(t::ANY)
    @_pure_meta
    if isa(t,Tuple) || isa(t,AbstractArray) || isa(t,SimpleVector)
        t = Tuple{t...}
    end
    if isa(t,Type) && t<:Tuple
        if !all(p->(isa(p,Type)||isa(p,TypeVar)), t.parameters)
            error("argument tuple type must contain only types")
        end
    else
        error("expected tuple type")
    end
    t
end

tt_cons(t::ANY, tup::ANY) = (@_pure_meta; Tuple{t, (isa(tup, Type) ? tup.parameters : tup)...})

code_lowered(f, t::ANY=Tuple) = map(m -> (m::Method).lambda_template, methods(f, t))

# low-level method lookup functions used by the compiler

function _methods(f::ANY,t::ANY,lim)
    ft = isa(f,Type) ? Type{f} : typeof(f)
    tt = isa(t,Type) ? Tuple{ft, t.parameters...} : Tuple{ft, t...}
    return _methods_by_ftype(tt, lim)
end
function _methods_by_ftype(t::ANY, lim)
    tp = t.parameters::SimpleVector
    nu = 1
    for ti in tp
        if isa(ti, Union)
            nu *= length((ti::Union).types)
        end
    end
    if 1 < nu <= 64
        return _methods(Any[tp...], length(tp), lim, [])
    end
    # XXX: the following can return incorrect answers that the above branch would have corrected
    return ccall(:jl_matching_methods, Any, (Any,Cint,Cint), t, lim, 0)
end
function _methods(t::Array,i,lim::Integer,matching::Array{Any,1})
    if i == 0
        new = ccall(:jl_matching_methods, Any, (Any,Cint,Cint), Tuple{t...}, lim, 0)
        new === false && return false
        append!(matching, new::Array{Any,1})
    else
        ti = t[i]
        if isa(ti, Union)
            for ty in (ti::Union).types
                t[i] = ty
                if _methods(t,i-1,lim,matching) === false
                    t[i] = ti
                    return false
                end
            end
            t[i] = ti
        else
            return _methods(t,i-1,lim,matching)
        end
    end
    return matching
end

# high-level, more convenient method lookup functions

# type for reflecting and pretty-printing a subset of methods
type MethodList
    ms::Array{Method,1}
    mt::MethodTable
end

length(m::MethodList) = length(m.ms)
isempty(m::MethodList) = isempty(m.ms)
start(m::MethodList) = start(m.ms)
done(m::MethodList, s) = done(m.ms, s)
next(m::MethodList, s) = next(m.ms, s)

function MethodList(mt::MethodTable)
    ms = Method[]
    visit(mt) do m
        push!(ms, m)
    end
    MethodList(ms, mt)
end

function methods(f::ANY, t::ANY)
    if isa(f, Core.Builtin)
        throw(ArgumentError("argument is not a generic function"))
    end
    t = to_tuple_type(t)
    return MethodList(Method[m[3] for m in _methods(f,t,-1)], typeof(f).name.mt)
end

methods(f::Core.Builtin) = MethodList(Method[], typeof(f).name.mt)

function methods_including_ambiguous(f::ANY, t::ANY)
    ft = isa(f,Type) ? Type{f} : typeof(f)
    tt = isa(t,Type) ? Tuple{ft, t.parameters...} : Tuple{ft, t...}
    ms = ccall(:jl_matching_methods, Any, (Any,Cint,Cint), tt, -1, 1)::Array{Any,1}
    return MethodList(Method[m[3] for m in ms], typeof(f).name.mt)
end

function methods(f::ANY)
    # return all matches
    return methods(f, Tuple{Vararg{Any}})
end

function visit(f, mt::MethodTable)
    mt.defs !== nothing && visit(f, mt.defs)
    nothing
end
function visit(f, mc::TypeMapLevel)
    if mc.targ !== nothing
        e = mc.targ::Vector{Any}
        for i in 1:length(e)
            isdefined(e, i) && visit(f, e[i])
        end
    end
    if mc.arg1 !== nothing
        e = mc.arg1::Vector{Any}
        for i in 1:length(e)
            isdefined(e, i) && visit(f, e[i])
        end
    end
    mc.linear_leaf !== nothing && visit(f, mc.linear_leaf)
    if mc.tname !== nothing
        e = mc.tname::Vector{Any}
        for i in 1:length(e)
            isdefined(e, i) && visit(f, e[i])
        end
    end
    if mc.name1 !== nothing
        e = mc.name1::Vector{Any}
        for i in 1:length(e)
            isdefined(e, i) && visit(f, e[i])
        end
    end
    mc.list !== nothing && visit(f, mc.list)
    mc.any !== nothing && visit(f, mc.any)
    nothing
end
function visit(f, d::TypeMapEntry)
    while !is(d, nothing)
        f(d.func)
        d = d.next
    end
    nothing
end

function length(mt::MethodTable)
    n = 0
    visit(mt) do m
        n += 1
    end
    return n::Int
end
isempty(mt::MethodTable) = (mt.defs === nothing)

uncompressed_ast(l::Method) = uncompressed_ast(l.lambda_template)
uncompressed_ast(l::LambdaInfo) =
    isa(l.code,Array{UInt8,1}) ? ccall(:jl_uncompress_ast, Array{Any,1}, (Any,Any), l, l.code) : l.code

# Printing code representations in IR and assembly
function _dump_function(f, t::ANY, native, wrapper, strip_ir_metadata, dump_module)
    ccall(:jl_is_in_pure_context, Bool, ()) && error("native reflection cannot be used from generated functions")
    t = tt_cons(Core.Typeof(f), to_tuple_type(t))
    llvmf = ccall(:jl_get_llvmf, Ptr{Void}, (Any, Bool, Bool), t, wrapper, native)

    if llvmf == C_NULL
        error("did not find a unique method for the specified argument types")
    end

    if native
        str = ccall(:jl_dump_function_asm, Ref{String}, (Ptr{Void},Cint), llvmf, 0)
    else
        str = ccall(:jl_dump_function_ir, Ref{String},
                    (Ptr{Void}, Bool, Bool), llvmf, strip_ir_metadata, dump_module)
    end

    isleaftype(t) || (str = "# WARNING: This code may not match what actually runs.\n" * str)
    return str
end

code_llvm(io::IO, f::ANY, types::ANY=Tuple, strip_ir_metadata=true, dump_module=false) =
    print(io, _dump_function(f, types, false, false, strip_ir_metadata, dump_module))
code_llvm(f::ANY, types::ANY=Tuple) = code_llvm(STDOUT, f, types)
code_llvm_raw(f::ANY, types::ANY=Tuple) = code_llvm(STDOUT, f, types, false)

code_native(io::IO, f::ANY, types::ANY=Tuple) =
    print(io, _dump_function(f, types, true, false, false, false))
code_native(f::ANY, types::ANY=Tuple) = code_native(STDOUT, f, types)

# give a decent error message if we try to instantiate a staged function on non-leaf types
function func_for_method_checked(m::Method, types)
    if m.isstaged && !isleaftype(types)
        error("cannot call @generated function `", m, "` ",
              "with abstract argument types: ", types)
    end
    return m
end

function code_typed(f::ANY, types::ANY=Tuple; optimize=true)
    ccall(:jl_is_in_pure_context, Bool, ()) && error("code reflection cannot be used from generated functions")
    types = to_tuple_type(types)
    asts = []
    for x in _methods(f,types,-1)
        linfo = func_for_method_checked(x[3], types)
        if optimize
            (li, ty, inf) = Core.Inference.typeinf(linfo, x[1], x[2], true)
        else
            (li, ty, inf) = Core.Inference.typeinf_uncached(linfo, x[1], x[2], optimize=false)
        end
        inf || error("inference not successful") # Inference disabled
        push!(asts, li)
    end
    asts
end

function return_types(f::ANY, types::ANY=Tuple)
    ccall(:jl_is_in_pure_context, Bool, ()) && error("code reflection cannot be used from generated functions")
    types = to_tuple_type(types)
    rt = []
    for x in _methods(f,types,-1)
        linfo = func_for_method_checked(x[3], types)
        (_li, ty, inf) = Core.Inference.typeinf(linfo, x[1], x[2])
        inf || error("inference not successful") # Inference disabled
        push!(rt, ty)
    end
    rt
end

function which(f::ANY, t::ANY)
    if isa(f, Core.Builtin)
        throw(ArgumentError("argument is not a generic function"))
    end
    t = to_tuple_type(t)
    if isleaftype(t)
        ms = methods(f, t)
        isempty(ms) && error("no method found for the specified argument types")
        length(ms)!=1 && error("no unique matching method for the specified argument types")
        return first(ms)
    else
        ft = isa(f,Type) ? Type{f} : typeof(f)
        m = ccall(:jl_gf_invoke_lookup, Any, (Any,), Tuple{ft, t.parameters...})
        if m === nothing
            error("no method found for the specified argument types")
        end
        return m.func::Method
    end
end

which(s::Symbol) = which_module(current_module(), s)
# TODO: making this a method of which() causes a strange error
function which_module(m::Module, s::Symbol)
    if !isdefined(m, s)
        error("\"$s\" is not defined in module $m")
    end
    binding_module(m, s)
end

functionloc(m::LambdaInfo) = functionloc(m.def)
function functionloc(m::Method)
    ln = m.line
    if ln <= 0
        error("could not determine location of method definition")
    end
    (find_source_file(string(m.file)), ln)
end

functionloc(f::ANY, types::ANY) = functionloc(which(f,types))

function functionloc(f)
    mt = methods(f)
    if isempty(mt)
        if isa(f,Function)
            error("function has no definitions")
        else
            error("object is not callable")
        end
    end
    if length(mt) > 1
        error("function has multiple methods; please specify a type signature")
    end
    functionloc(first(mt))
end

function function_module(f, types::ANY)
    m = methods(f, types)
    if isempty(m)
        error("no matching methods")
    end
    first(m).module
end

function method_exists(f::ANY, t::ANY)
    t = to_tuple_type(t)
    t = Tuple{isa(f,Type) ? Type{f} : typeof(f), t.parameters...}
    return ccall(:jl_method_exists, Cint, (Any, Any), typeof(f).name.mt, t) != 0
end

function isambiguous(m1::Method, m2::Method)
    ti = typeintersect(m1.sig, m2.sig)
    ti === Bottom && return false
    ml = _methods_by_ftype(ti, -1)
    isempty(ml) && return true
    for m in ml
        if ti <: m[3].sig
            return false
        end
    end
    return true
end
