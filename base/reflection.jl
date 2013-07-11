# name and module reflection
module_name(m::Module) = ccall(:jl_module_name, Any, (Any,), m)::Symbol
module_parent(m::Module) = ccall(:jl_module_parent, Any, (Any,), m)::Module
current_module() = ccall(:jl_get_current_module, Any, ())::Module

fullname(m::Module) = m===Main ? () : tuple(fullname(module_parent(m))...,
                                            module_name(m))

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
object_id(x::ANY) = ccall(:jl_object_id, Uint, (Any,), x)

# type predicates
const isimmutable = x->(isa(x,Tuple) || !typeof(x).mutable)
isstructtype(t::DataType) = t.names!=() || (t.size==0 && !t.abstract)
isstructtype(x) = false
isbits(t::DataType) = !t.mutable && t.pointerfree
isbits(t::Type) = false
isbits(x) = isbits(typeof(x))
isleaftype(t::ANY) = ccall(:jl_is_leaf_type, Int32, (Any,), t) != 0

typeintersect(a::ANY,b::ANY) = ccall(:jl_type_intersection, Any, (Any,Any), a, b)
typeseq(a::ANY,b::ANY) = subtype(a,b)&&subtype(b,a)

function fieldoffsets(x::DataType)
    offsets = Array(Int, length(x.names))
    ccall(:jl_field_offsets, Void, (Any, Ptr{Int}), x, offsets)
    offsets
end

# subtypes
function _subtypes(m::Module, x::DataType, sts=Set(), visited=Set())
    add!(visited, m)
    for s in names(m,true)
        if isdefined(m,s)
            t = eval(m,s)
            if isa(t, DataType) && super(t).name == x.name
                add!(sts, t)
            elseif isa(t, Module) && !contains(visited, t)
                _subtypes(t, x, sts, visited)
            end
        end
    end
    sts
end
subtypes(m::Module, x::DataType) = sort(collect(_subtypes(m, x)), by=string)
subtypes(x::DataType) = subtypes(Main, x)

subtypetree(x::DataType, level=-1) = (level == 0 ? (x, {}) : (x, {subtypetree(y, level-1) for y in subtypes(x)}))

# function reflection
isgeneric(f::ANY) = (isa(f,Function)||isa(f,DataType)) && isa(f.env,MethodTable)

function_name(f::Function) = isgeneric(f) ? f.env.name : (:anonymous)

methods(f::ANY,t::ANY) = _methods(f,t,-1)::Array{Any,1}
_methods(f::ANY,t::ANY,lim) = ccall(:jl_matching_methods, Any, (Any,Any,Int32), f, t, lim)

function methods(f::Function)
    if !isgeneric(f)
        error("methods: not a generic function")
    end
    f.env
end

methods(t::DataType) = (_methods(t,Tuple,0);  # force constructor creation
                        t.env)

uncompressed_ast(l::LambdaStaticData) =
    isa(l.ast,Expr) ? l.ast : ccall(:jl_uncompress_ast, Any, (Any,Any), l, l.ast)

disassemble(f::Function, types::Tuple, asm::Bool = false) =
    print(ccall(:jl_dump_function, Any, (Any,Any,Bool), f, types, asm)::ByteString)

function functionlocs(f::Function, types=(Any...))
    locs = Any[]
    for m in methods(f, types)
        if isa(m[3],LambdaStaticData)
            lsd = m[3]::LambdaStaticData
            ln = lsd.line
            if ln > 0
                push!(locs, (find_source_file(string(lsd.file)), ln))
            end
        end
    end
    if length(locs) == 0
       error("could not find function definition")
    end
    locs
end

functionloc(f::Function, types=(Any...)) = functionlocs(f, types)[1]

function function_module(f::Function, types=(Any...))
    m = methods(f, types)
    if isempty(m)
        error("no matching methods")
    end
    m[1][3].module
end
