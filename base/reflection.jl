# name and module reflection
module_name(m::Module) = ccall(:jl_module_name, Any, (Any,), m)::Symbol
module_parent(m::Module) = ccall(:jl_module_parent, Any, (Any,), m)::Module
current_module() = ccall(:jl_get_current_module, Any, ())::Module

names(m::Module, all::Bool, imported::Bool) = ccall(:jl_module_names, Array{Symbol,1}, (Any,Int32,Int32), m, all, imported)
names(m::Module) = names(m,false,false)
names(t::DataType) = t.names

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
subtypes(m::Module, x::DataType) = sortby(string, collect(_subtypes(m, x)))
subtypes(x::DataType) = subtypes(Main, x)

subtypetree(x::DataType, level=-1) = (level == 0 ? (x, {}) : (x, {subtypetree(y, level-1) for y in subtypes(x)}))

# function reflection
isgeneric(f::ANY) = (isa(f,Function)||isa(f,DataType)) && isa(f.env,MethodTable)

methods(f::ANY,t::ANY) = _methods(f,t,-1)::Array{Any,1}
_methods(f::ANY,t::ANY,lim) = ccall(:jl_matching_methods, Any, (Any,Any,Int32), f, t, lim)

function methods(f::Function)
    if !isgeneric(f)
        error("methods: not a generic function")
    end
    f.env
end

methods(t::DataType) = (methods(t,Tuple);  # force constructor creation
                        t.env)

disassemble(f::Function, types::Tuple) =
    print(ccall(:jl_dump_function, Any, (Any,Any), f, types)::ByteString)

function functionlocs(f::Function, types)
    locs = Array(Tuple, 0)
    for m = methods(f, types)
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
functionlocs(f::Function) = functionlocs(f, (Any...))

functionloc(f::Function, types) = functionlocs(f, types)[1]
functionloc(f::Function) = functionloc(f, (Any...))


