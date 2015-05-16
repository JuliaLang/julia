# This file is a part of Julia. License is MIT: http://julialang.org/license

# name and module reflection
module_name(m::Module) = ccall(:jl_module_name, Any, (Any,), m)::Symbol
module_parent(m::Module) = ccall(:jl_module_parent, Any, (Any,), m)::Module
current_module() = ccall(:jl_get_current_module, Any, ())::Module

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

binding_module(var::Symbol) = binding_module(current_module(), var)
function binding_module(m::Module, var::Symbol)
    if isdefined(m, var) # this returns true for 'used' bindings
        mod = ccall(:jl_get_module_of_binding, Any, (Any, Any), m, var)
    else
        error("\"$var\" is not bound in module $m")
    end
    mod
end

fieldnames(t::DataType) = Symbol[n for n in t.name.names ]
function fieldnames(v)
    t = typeof(v)
    if !isa(t,DataType)
        throw(ArgumentError("cannot call fieldnames() on a non-composite type"))
    end
    return fieldnames(t)
end

fieldname(t::DataType, i::Integer) = t.name.names[i]::Symbol

isconst(s::Symbol) = ccall(:jl_is_const, Int32, (Ptr{Void}, Any), C_NULL, s) != 0

isconst(m::Module, s::Symbol) =
    ccall(:jl_is_const, Int32, (Any, Any), m, s) != 0

# return an integer such that object_id(x)==object_id(y) if is(x,y)
object_id(x::ANY) = ccall(:jl_object_id, UInt, (Any,), x)

# type predicates
isimmutable(x::ANY) = (isa(x,Tuple) || !typeof(x).mutable)
isstructtype(t::DataType) = nfields(t) != 0 || (t.size==0 && !t.abstract)
isstructtype(x) = false
isbits(t::DataType) = !t.mutable & t.pointerfree & isleaftype(t)
isbits(t::Type) = false
isbits(x) = isbits(typeof(x))
isleaftype(t::ANY) = ccall(:jl_is_leaf_type, Int32, (Any,), t) != 0

typeintersect(a::ANY,b::ANY) = ccall(:jl_type_intersection, Any, (Any,Any), a, b)
typeseq(a::ANY,b::ANY) = a<:b && b<:a

function fieldoffsets(x::DataType)
    offsets = Array(Int, nfields(x))
    ccall(:jl_field_offsets, Void, (Any, Ptr{Int}), x, offsets)
    offsets
end

type_alignment(x::DataType) = ccall(:jl_get_alignment,Csize_t,(Any,),x)
field_offset(x::DataType,idx) = ccall(:jl_get_field_offset,Csize_t,(Any,Int32),x,idx)

# subtypes
function _subtypes(m::Module, x::DataType, sts=Set(), visited=Set())
    push!(visited, m)
    for s in names(m,true)
        if isdefined(m,s)
            t = eval(m,s)
            if isa(t, DataType) && t.name.name == s && super(t).name == x.name
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
isgeneric(f::ANY) = (isa(f,Function) && isa(f.env,MethodTable))

function_name(f::Function) = isgeneric(f) ? f.env.name : (:anonymous)

tt_cons(t::ANY, tup::ANY) = Tuple{t, (isa(tup, Type) ? tup.parameters : tup)...}

code_lowered(f, t::ANY) = map(m->uncompressed_ast(m.func.code), methods(f, t))
methods(f::Function,t::ANY) = Any[m[3] for m in _methods(f,t,-1)]
methods(f::ANY,t::ANY) = methods(call, tt_cons(isa(f,Type) ? Type{f} : typeof(f), t))
function _methods(f::ANY,t::ANY,lim)
    if isa(t,Type)
        _methods(f, Any[t.parameters...], length(t.parameters), lim, [])
    else
        _methods(f, Any[t...], length(t), lim, [])
    end
end
function _methods(f::ANY,t::Array,i,lim::Integer,matching::Array{Any,1})
    if i == 0
        new = ccall(:jl_matching_methods, Any, (Any,Any,Int32), f, Tuple{t...}, lim)
        if new === false
            return false
        end
        append!(matching, new::Array{Any,1})
    else
        ti = t[i]
        if isa(ti, UnionType)
            for ty in (ti::UnionType).types
                t[i] = ty
                if _methods(f,t,i-1,lim,matching) === false
                    t[i] = ty
                    return false
                end
            end
            t[i] = ti
        else
            return _methods(f,t,i-1,lim,matching)
        end
    end
    matching
end

function methods(f::Function)
    if !isgeneric(f)
        throw(ArgumentError("argument is not a generic function"))
    end
    f.env
end

methods(x::ANY) = methods(call, Tuple{isa(x,Type) ? Type{x} : typeof(x), Vararg{Any}})

function length(mt::MethodTable)
    n = 0
    d = mt.defs
    while !is(d,nothing)
        n += 1
        d = d.next
    end
    n
end

start(mt::MethodTable) = mt.defs
next(mt::MethodTable, m::Method) = (m,m.next)
done(mt::MethodTable, m::Method) = false
done(mt::MethodTable, i::Void) = true

uncompressed_ast(l::LambdaStaticData) =
    isa(l.ast,Expr) ? l.ast : ccall(:jl_uncompress_ast, Any, (Any,Any), l, l.ast)

# Printing code representations in IR and assembly
_dump_function(f, t::Tuple{Vararg{Type}}, native, wrapper, strip_ir_metadata) =
    _dump_function(f, Tuple{t...}, native, wrapper, strip_ir_metadata)
function _dump_function(f, t::ANY, native, wrapper, strip_ir_metadata)
    llvmf = ccall(:jl_get_llvmf, Ptr{Void}, (Any, Any, Bool), f, t, wrapper)

    if llvmf == C_NULL
        error("no method found for the specified argument types")
    end

    if (native)
        str = ccall(:jl_dump_function_asm, Any, (Ptr{Void},), llvmf)::ByteString
    else
        str = ccall(:jl_dump_function_ir, Any,
                        (Ptr{Void}, Bool), llvmf, strip_ir_metadata)::ByteString
    end

    return str
end

code_llvm(io::IO, f::Function, types::ANY, strip_ir_metadata=true) =
    print(io, _dump_function(f, types, false, false, strip_ir_metadata))
code_llvm(f::ANY, types::ANY) = code_llvm(STDOUT, f, types)
code_llvm_raw(f::ANY, types::ANY) = code_llvm(STDOUT, f, types, false)
code_llvm(io::IO, f::ANY, t::ANY, args...) =
    code_llvm(io, call,
              tt_cons(isa(f, Type) ? Type{f} : typeof(f), t), args...)

code_native(io::IO, f::Function, types::ANY) =
    print(io, _dump_function(f, types, true, false, false))
code_native(f::ANY, types::ANY) = code_native(STDOUT, f, types)
code_native(io::IO, f::ANY, t::ANY) =
    code_native(io, call, tt_cons(isa(f, Type) ? Type{f} : typeof(f), t))

if isdefined(Core, :Inference) && not_int(is(current_module(), Core.Inference))
    code_typed(args...;kwargs...) = Core.Inference.code_typed(args...;kwargs...)
    return_types(args...;kwargs...) = Core.Inference.return_types(args...;kwargs...)
end

which(f::ANY, t::Tuple{Vararg{Type}}) = which(f, Tuple{t...})
function which(f::ANY, t::ANY)
    if isleaftype(t)
        ms = methods(f, t)
        isempty(ms) && error("no method found for the specified argument types")
        length(ms)!=1 && error("no unique matching method for the specified argument types")
        ms[1]
    else
        if !isa(f,Function)
            t = Tuple{isa(f,Type) ? Type{f} : typeof(f), t.parameters...}
            f = call
        elseif !isgeneric(f)
            throw(ArgumentError("argument is not a generic function"))
        end
        m = ccall(:jl_gf_invoke_lookup, Any, (Any, Any), f, t)
        if m === nothing
            error("no method found for the specified argument types")
        end
        m
    end
end

which(s::Symbol) = binding_module(current_module(), s)

function functionloc(m::Method)
    lsd = m.func.code::LambdaStaticData
    ln = lsd.line
    if ln <= 0
        error("could not determine location of method definition")
    end
    (find_source_file(string(lsd.file)), ln)
end

functionloc(f::ANY, types::ANY) = functionloc(which(f,types))

function functionloc(f)
    m = methods(f)
    if length(m) > 1
        error("function has multiple methods; please specify a type signature")
    end
    functionloc(m.defs)
end

function function_module(f, types::ANY)
    m = methods(f, types)
    if isempty(m)
        error("no matching methods")
    end
    m[1].func.code.module
end
