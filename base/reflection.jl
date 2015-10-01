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

# return all instances, for types that can be enumerated
function instances end

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

function to_tuple_type(t::ANY)
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

tt_cons(t::ANY, tup::ANY) = Tuple{t, (isa(tup, Type) ? tup.parameters : tup)...}

code_lowered(f, t::ANY) = map(m->uncompressed_ast(m.func.code), methods(f, t))
function methods(f::Function,t::ANY)
    if !isgeneric(f)
        throw(ArgumentError("argument is not a generic function"))
    end
    t = to_tuple_type(t)
    Any[m[3] for m in _methods(f,t,-1)]
end
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
        if isa(ti, Union)
            for ty in (ti::Union).types
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
function _dump_function(f, t::ANY, native, wrapper, strip_ir_metadata, dump_module)
    t = to_tuple_type(t)
    llvmf = ccall(:jl_get_llvmf, Ptr{Void}, (Any, Any, Bool), f, t, wrapper)

    if llvmf == C_NULL
        error("no method found for the specified argument types")
    end

    if (native)
        str = ccall(:jl_dump_function_asm, Any, (Ptr{Void},Cint), llvmf, 0)::ByteString
    else
        str = ccall(:jl_dump_function_ir, Any,
                    (Ptr{Void}, Bool, Bool), llvmf, strip_ir_metadata, dump_module)::ByteString
    end

    return str
end

code_llvm(io::IO, f::Function, types::ANY, strip_ir_metadata=true, dump_module=false) =
    print(io, _dump_function(f, types, false, false, strip_ir_metadata, dump_module))
code_llvm(f::ANY, types::ANY) = code_llvm(STDOUT, f, types)
code_llvm_raw(f::ANY, types::ANY) = code_llvm(STDOUT, f, types, false)
code_llvm(io::IO, f::ANY, t::ANY, args...) =
    code_llvm(io, call,
              tt_cons(isa(f, Type) ? Type{f} : typeof(f), t), args...)

code_native(io::IO, f::Function, types::ANY) =
    print(io, _dump_function(f, types, true, false, false, false))
code_native(f::ANY, types::ANY) = code_native(STDOUT, f, types)
code_native(io::IO, f::ANY, t::ANY) =
    code_native(io, call, tt_cons(isa(f, Type) ? Type{f} : typeof(f), t))

# give a decent error message if we try to instantiate a staged function on non-leaf types
function func_for_method_checked(m, types)
    linfo = Core.Inference.func_for_method(m[3],types,m[2])
    if linfo === Core.Inference.NF
        error("cannot call @generated function `", m[3], "` ",
              "with abstract argument types: ", types)
    end
    linfo::LambdaStaticData
end

function code_typed(f::Function, types::ANY; optimize=true)
    types = to_tuple_type(types)
    asts = []
    for x in _methods(f,types,-1)
        linfo = func_for_method_checked(x, types)
        if optimize
            (tree, ty) = Core.Inference.typeinf(linfo, x[1], x[2], linfo,
                                                true, true)
        else
            (tree, ty) = Core.Inference.typeinf_uncached(linfo, x[1], x[2],
                                                         optimize=false)
        end
        if !isa(tree, Expr)
            push!(asts, ccall(:jl_uncompress_ast, Any, (Any,Any), linfo, tree))
        else
            push!(asts, tree)
        end
    end
    asts
end

function code_typed(f, t::ANY; optimize=true)
    code_typed(call, tt_cons(isa(f, Type) ? Type{f} : typeof(f), t),
               optimize=optimize)
end

function return_types(f::Function, types::ANY)
    types = to_tuple_type(types)
    rt = []
    for x in _methods(f,types,-1)
        linfo = func_for_method_checked(x,types)
        (tree, ty) = Core.Inference.typeinf(linfo, x[1], x[2])
        push!(rt, ty)
    end
    rt
end

function return_types(f, t::ANY)
    return_types(call, tt_cons(isa(f, Type) ? Type{f} : typeof(f), t))
end

function which(f::ANY, t::ANY)
    t = to_tuple_type(t)
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

which(s::Symbol) = which_module(current_module(), s)
# TODO: making this a method of which() causes a strange error
function which_module(m::Module, s::Symbol)
    if !isdefined(m, s)
        error("\"$s\" is not defined in module $m")
    end
    binding_module(m, s)
end

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
