# This file is a part of Julia. License is MIT: http://julialang.org/license

module TypeTrees
##
# Generate a text graphic of Julia modules type tree
##

immutable Binding
    mod::Module
    sym::Symbol
end
Binding(tn::TypeName) = Binding(tn.module, tn.name)
Base.isless(a::Binding, b::Binding) = isless(a.sym, b.sym)

# The node type holds the type of the current node and a dict of subtypes
immutable TTNode
    typ # ::Type
    subtypes::Dict{Binding, TTNode}

    TTNode(t::ANY) = new(t, Dict{Binding, TTNode}())
end

# Add a node to a dict if not added
function add_ttnode(subtypes::Dict{Binding, TTNode}, sname::Binding, tnode::TTNode)
    return get!(subtypes, sname, tnode)
end

function add_ttnode(subtypes::Dict{Binding, TTNode}, sname::Binding, t::Type)
    return get!(subtypes, sname, TTNode(t))
end

# Store a type and its type hierarchy chain
# Recurse till we reach the top level type
function store_type(sname::Binding, t::Union)
    suptype = Union
    tnode = TTNode(t)

    # store unions under Union type
    subtypes = store_type(Binding(suptype.name), suptype)
    add_ttnode(subtypes, sname, tnode)
    store_union(sname, tnode, t)

    # unions are also in a sense related to the types of their components

    return tnode.subtypes
end
function store_union(sname::Binding, tnode::TTNode, t::ANY)
    t = Base.unwrap_unionall(t)
    if isa(t, Union)
        store_union(sname, tnode, t.a)
        store_union(sname, tnode, t.b)
    elseif isa(t, DataType)
        binding = Binding(t.name)
        subtypes = store_type(binding, t)
        add_ttnode(subtypes, sname, tnode)
    end
    nothing
end

function store_type(sname::Binding, t::UnionAll)
    suptype = Base.unwrap_unionall(t)
    binding = isa(suptype, DataType) ? Binding(suptype.name) : Binding(Main, string(suptype::Union))
    subtypes = store_type(binding, suptype)
    tnode = add_ttnode(subtypes, sname, t)
    return tnode.subtypes
end

function store_type(sname::Binding, t::DataType)
    suptype = supertype(t)
    subtypes = (suptype != t) ? store_type(Binding(suptype.name), suptype) : types_tree
    tnode = add_ttnode(subtypes, sname, t)
    return tnode.subtypes
end

# examine all symbols in module and store those that are types
function store_all_from(m::Module)
    for s in names(m, true)
        if isdefined(m, s) && !Base.isdeprecated(m, s)
            t = getfield(m, s)
            if isa(t, Type) && t !== Union{}
                store_type(Binding(m, s), t)
            elseif isa(t, Module) && module_name(t) === s && module_parent(t) === m && t !== m
                store_all_from(t)
            end
        end
    end
end

type_props(typ) = ""
type_props(typ::DataType) = string("<<",
                                 typ.abstract    ? " abstract"    : " concrete",
                                 typ.mutable     ? " mutable"     : " immutable",
                                 typ.layout != C_NULL ? string(
                                     Base.datatype_pointerfree(typ) ? " pointerfree" : "",
                                     Base.datatype_haspadding(typ) ? " haspadding" : "",
                                     " nfields:", Core.nfields(typ),
                                     " size:", typ.size,
                                     ", align:", Base.datatype_alignment(typ)) : "",
                                 " >>")

function print_tree(subtypes::Dict{Binding, TTNode}, pfx::String="")
    for b in sort!(collect(keys(subtypes)))
        v = subtypes[b]
        ishidden = unsafe_load(Base.unsafe_convert(Ptr{UInt8}, b.sym)) == UInt8('#')
        if ishidden && supertype(v.typ) === Function
            continue
        end
        if b.mod === Main
            n = string(b.sym)
        elseif !isa(v.typ, DataType) || v.typ.name.module != b.mod || v.typ.name.name != b.sym
            n_io = IOBuffer()
            print(n_io, b.mod, '.', b.sym)
            ua = v.typ
            if isa(ua, UnionAll)
                print(n_io, "{")
                while true
                    print(n_io, ua.var)
                    ua = ua.body
                    if isa(ua, UnionAll)
                        print(n_io, ", ")
                    else
                        break
                    end
                end
                print(n_io, "}")
            end
            n = String(take!(n_io))
        else
            n = string(v.typ)
        end
        if n == string(v.typ)
            println(pfx, "+- ", n, " ", type_props(v.typ))
        else
            println(pfx, "+- ", n, " = ", v.typ, " ", type_props(v.typ))
        end
        v.typ === Function && println(pfx, ".     ## hiding implicit Function subtypes ##")
        print_tree(v.subtypes, pfx * ".  ")
    end
end


# TODO: optionally take module names in command line
# TODO: option to list subtrees of type tree, or other symbol types
const types_tree = Dict{Binding, TTNode}()

store_all_from(Main)

# print_tree(types_tree)

end # module

if !isinteractive()
    TypeTrees.print_tree(TypeTrees.types_tree)
end
