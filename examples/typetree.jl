##
#
# (spaghetti) Code to create a text graphic of the Julia type tree
# Used to generate https://github.com/JuliaLang/julia/wiki/Types-Hierarchy
#
##

function list_all(T::Type, modules::Array{Module}, f::Function)
    a = Dict{String, T}()
    for m = modules
        sm = string(m)
        for s = names(m,true)
            try
                t = eval(m,s)
                if f(t)
                    a[string(s)] = t
                end
            end
         end
    end
    a
end

function to_array{T}(s::Set{T})
    a = Array(T, length(s))
    i = 1
    for ele = s
        a[i] = ele
        i += 1
    end
    a
end

function mk_tree() 
    for (x,t) in all_types
        add_type(x,t,true)
    end
end
extra = Dict{String, String}()
function insert_type(m,s,x,ex)
    try
        ms = m[s]
        add!(ms, x)
    catch
        m[s] = Set{String}(x)
    end
    extra[x] = ex
end

function add_type(x,t,sup)
    if isa(t, DataType)
        s = sup ? super(t) : t
        s_param = ""
        if x != string(t.name)
            s_param *= " = " * string(t.name)
        elseif length(s.parameters) != 0
            s_param *= " (=" * string(s.name) * "{" * join(s.parameters, ", ") * "})"
        end
        insert_type(children_map, string(s.name), x, s_param)
    elseif isa(t, UnionType)
        for c in t.types
            add_type(x, c, false)
        end
        insert_type(children_map, "UnionType", x, " = " * string(t))
    elseif isa(t, TypeConstructor)
        add_type(x, t.body, false)
        #println(typeof(t.body))
        insert_type(children_map, "TypeConstructor", x, " = " * string(t))
    else
        try
            print("unknown -> ")
            print(x)
            print(" ")
            print(typeof(t))
            print(" ")
            print(t.name)
        end
        println(t)
    end
end
   
function show_tree(root, prefix, norm_ex)
    r = root
    t = all_types[root]
    try
        if length(t.parameters) != 0
            root *= "{" * join(t.parameters, ", ") * "}"
        end
    end
    ex = extra[r]
    if ex != norm_ex
        root *= ex
    end
    println(prefix[1:end-1] * "+- " * root)
    norm_ex = ""
    try
        norm_ex = " (=" * r * "{" * join(t.parameters, ", ") * "})"
    end
    children = get(children_map, r, None)
    if children != None
        ch = to_array(children)
        sort!(ch)
        del(children_map, r)
        iter = start(ch)
        cpre = prefix * "   +"
        dpre = prefix * "    "
        while !done(ch, iter)
            c,iter = next(ch, iter)
            if c != root
                show_tree(c, done(ch, iter) ? dpre : cpre, norm_ex)
            end
        end
    end
end

show_tree(root) = show_tree(root, "", "")

## main ##
all_types = list_all(Type, [Core, Base, Main], (x)->isa(x, Type))
children_map = Dict{String, Set{String}}()
mk_tree()
println("\n\nType Tree:")
show_tree("Any") #todo: generalize this to be every item without a parent in all_types


