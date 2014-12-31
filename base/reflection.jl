# name and module reflection
module_name(m::Module) = ccall(:jl_module_name, Any, (Any,), m)::Symbol
module_parent(m::Module) = ccall(:jl_module_parent, Any, (Any,), m)::Module
current_module() = ccall(:jl_get_current_module, Any, ())::Module

function fullname(m::Module)
    if m === Main
        ()
    elseif module_parent(m) === m
        # not Main, but is its own parent, means a prior Main module
        n = ()
        this = Main
        while this !== m
            if isdefined(this, :LastMain)
                n = tuple(n..., :LastMain)
                this = this.LastMain
            else
                error("no reference to module ", module_name(m))
            end
        end
        return n
    else
        tuple(fullname(module_parent(m))..., module_name(m))
    end
end

names(m::Module, all::Bool, imported::Bool) = ccall(:jl_module_names, Array{Symbol,1}, (Any,Int32,Int32), m, all, imported)
names(m::Module, all::Bool) = names(m, all, false)
names(m::Module) = names(m, false, false)
names(t::DataType) = collect(t.names)

function names(v)
    t = typeof(v)
    if isa(t,DataType)
        return names(t)
    else
        error("cannot call names() on a non-composite type")
    end
end

isconst(s::Symbol) =
    ccall(:jl_is_const, Int32, (Ptr{Void}, Any), C_NULL, s) != 0

isconst(m::Module, s::Symbol) =
    ccall(:jl_is_const, Int32, (Any, Any), m, s) != 0

# return an integer such that object_id(x)==object_id(y) if is(x,y)
object_id(x::ANY) = ccall(:jl_object_id, UInt, (Any,), x)

# type predicates
isimmutable(x::ANY) = (isa(x,Tuple) || !typeof(x).mutable)
isstructtype(t::DataType) = t.names!=() || (t.size==0 && !t.abstract)
isstructtype(x) = false
isbits(t::DataType) = !t.mutable & t.pointerfree & isleaftype(t)
isbits(t::Type) = false
isbits(x) = isbits(typeof(x))
isleaftype(t::ANY) = ccall(:jl_is_leaf_type, Int32, (Any,), t) != 0

typeintersect(a::ANY,b::ANY) = ccall(:jl_type_intersection, Any, (Any,Any), a, b)
typeseq(a::ANY,b::ANY) = a<:b && b<:a

function fieldoffsets(x::DataType)
    offsets = Array(Int, length(x.names))
    ccall(:jl_field_offsets, Void, (Any, Ptr{Int}), x, offsets)
    offsets
end

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

subtypetree(x::DataType, level=-1) = (level == 0 ? (x, []) : (x, Any[subtypetree(y, level-1) for y in subtypes(x)]))

# function reflection
isgeneric(f::ANY) = (isa(f,Function) && isa(f.env,MethodTable))

function_name(f::Function) = isgeneric(f) ? f.env.name : (:anonymous)

code_lowered(f::Function,t::(Type...)) = map(m->uncompressed_ast(m.func.code), methods(f,t))
methods(f::Function,t::ANY) = Any[m[3] for m in _methods(f,t,-1)]
methods(f::ANY,t::ANY) = methods(call, tuple(isa(f,Type) ? Type{f} : typeof(f), t...))
_methods(f::ANY,t::ANY,lim) = _methods(f, Any[(t::Tuple)...], length(t::Tuple), lim, [])
function _methods(f::ANY,t::Array,i,lim::Integer,matching::Array{Any,1})
    if i == 0
        new = ccall(:jl_matching_methods, Any, (Any,Any,Int32), f, tuple(t...), lim)
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
        error("not a generic function")
    end
    f.env
end

methods(x::ANY) = methods(call, (isa(x,Type) ? Type{x} : typeof(x), Any...))

function length(mt::MethodTable)
    n = 0
    d = mt.defs
    while !is(d,())
        n += 1
        d = d.next
    end
    n
end

start(mt::MethodTable) = mt.defs
next(mt::MethodTable, m::Method) = (m,m.next)
done(mt::MethodTable, m::Method) = false
done(mt::MethodTable, i::()) = true

uncompressed_ast(l::LambdaStaticData) =
    isa(l.ast,Expr) ? l.ast : ccall(:jl_uncompress_ast, Any, (Any,Any), l, l.ast)

function _dump_function(f, t::ANY, native, wrapper)
    str = ccall(:jl_dump_function, Any, (Any,Any,Bool,Bool), f, t, native, wrapper)::ByteString
    if str == ""
        error("no method found for the specified argument types")
    end
    str
end

code_llvm(io::IO, f::Function, types::(Type...)) = print(io, _dump_function(f, types, false, false))
code_llvm(f::Function, types::(Type...)) = code_llvm(STDOUT, f, types)
code_native(io::IO, f::Function, types::(Type...)) = print(io, _dump_function(f, types, true, false))
code_native(f::Function, types::(Type...)) = code_native(STDOUT, f, types)

function functionlocs(f::ANY, types=(Type...))
    locs = Any[]
    for m in methods(f, types)
        lsd = m.func.code::LambdaStaticData
        ln = lsd.line
        if ln > 0
            push!(locs, (find_source_file(string(lsd.file)), ln))
        end
    end
    if length(locs) == 0
       error("could not find function definition")
    end
    locs
end

functionloc(f::ANY, types=(Any...)) = functionlocs(f, types)[1]

function function_module(f, types=(Any...))
    m = methods(f, types)
    if isempty(m)
        error("no matching methods")
    end
    m[1].func.code.module
end
