module Defaults

function findtypevars(e::Expr, vars, found = falses(length(vars)))
    for a in e.args
        all(found) && break # handles empty case
        findtypevars(a, vars, found)
    end
    found
end

function findtypevars(e, vars, found)
    i = findfirst(vars, e)
    i > 0 && (found[i] = true)
    found
end

function getargname(arg::Expr)
    if arg.head == :(::)
        if length(arg.args) == 2
            arg.args[1]
        else # length == 1
            arg1 = arg.args[1]
            arg1 isa Expr && arg1.head == :curly && arg1.args[1] == :Type ||
                throw(ArgumentError("unsupported argument"))
            arg1.args[2]
        end
    elseif arg.head == :kw
        getargname(arg.args[1])
    elseif arg.head == :...
        Expr(:..., getargname(arg.args[1]))
    else
        throw(ArgumentError("malformed definition"))
    end
end

getargname(arg::Symbol) = arg
getargname(arg) = throw(ArgumentError("malformed definition"))

function replacedefault!(args, sym, defval)
    for i in eachindex(args)
        if args[i] isa Expr
            replacedefault!(args[i].args, sym, defval)
        elseif args[i] === sym
            args[i] = defval
        end
    end
end

function gendefinitions(master, defaults, fname, argnames, linefile, wherevars)
    definitions = []
    for n = 1:(2^length(defaults)-1)
        sgn = copy(master)
        an = similar(argnames, Any) # Any to handle x...
        copy!(an, argnames)
        for b = 31:-1:1
            if n & (1<<(b-1)) != 0
                idx, defval = defaults[b]
                deleteat!(sgn, idx)
                # handle cases with references to previous args,
                # like f([x=1], y=typeof(x))
                replacedefault!(view(sgn, idx:length(sgn)), # not idx+1, as idx was just deleted
                                an[idx], defval)
                an[idx] = defval
            end
        end
        newdef = :($fname($(sgn...)) = $fname($(an...)))
        newvars = wherevars[findtypevars(newdef, wherevars)]
        # @assert newdef.args[2].args[1] isa LineNumberNode
        newdef.args[2].args[1] = linefile
        push!(definitions, newdef=>newvars)
    end
    definitions
end


macro defaults(e)
    e.head in [:(=), :function] ||
        throw(ArgumentError("expected function definition"))
    if e.args[1].head == :where
        wherevars = e.args[1].args[2:end]
        e.args[1] = e.args[1].args[1] # delete where
    else
        wherevars = []
    end
    e.args[1].head == :call ||
        throw(ArgumentError("expected function definition 2"))
    args = e.args[1].args
    defs = []
    fname = args[1]
    argnames = []
    for i = 2:length(args) # omit function name
        if args[i] isa Expr && args[i].head == :vect
            length(args[i].args) == 1 && args[i].args[1].head == :(=) ||
                throw(ArgumentError("malformed default expression"))
            push!(defs, i-1 => args[i].args[1].args[2])
            args[i] = args[i].args[1].args[1]
        end
        push!(argnames, getargname(args[i]))
    end
    length(defs) > 31 &&
        throw(ArgumentError(
            "number of defaults ($(length(defs))) exceeded maximum (31)"))
    linefile = e.args[2].args[1]
    # @assert linefile isa LineNumberNode # not within macro
    definitions = (e => wherevars, gendefinitions(args[2:end], defs, fname, argnames, linefile, wherevars)...)
    for (d, w) in definitions
        if !isempty(w)
            d.args[1] = Expr(:where, d.args[1], w...)
        end
    end
    esc(Expr(:block, [d.first for d in definitions]...))
end

export @defaults

end # module
