

type Options # <: Associative doesn't gain any functionality -- omit
    keys::Array{Symbol,1}
    vals::Array{Any,1}
    
    Options(k, v) = length(k)==length(v) ? new(k,v) : error("non-matching vectors of keys and vals")
end

function options(args...)
    # figure out how long the keys/vals should be and allocate them
    if (length(args) % 2 != 0)
        error("options must have an even number of arguments")
    end
    numargs = div(length(args), 2)
    k = Array(Symbol, numargs)
    v = Array(Any, numargs)
    
    # loop through pairs of args and add keys/vals
    for a = 1:numargs
        k[a] = args[(a-1)*2 + 1]
        v[a] = args[(a-1)*2 + 2]
    end
    
    return Options(k, v)
end

function add_defaults!(o::Options, args...)
    # find stuff in o that's not in args, save it to return or error
    bUnmatched = [!contains(args, x)::Bool | x = o.keys]
    unmatchedKeys = o.keys[bUnmatched]
    unmatchedVals = o.vals[bUnmatched]
    # in theory we could remove these from o, but it doesn't matter
    
    if (args[length(args)] == :etc) # last of a tuple doesn't work!
        # remove it, don't check
        args = args[1:(length(args)-1)] # and neither does pop!
    elseif any(bUnmatched) # throw an error if o has a key not in args
        error("options without defaults or :etc : $(unmatchedKeys)")
    end
    
    # iterate over the arguments
    # if the key exists in o, do nothing
    # if the key doesn't exist, add it
    
    if isodd(length(args))
        error("add_defaults must have an even number of arguments")
    end
    numargs = div(length(args), 2)
    
    # loop through pairs of args, adding defaults
    for a = 1:numargs
        iArgs = (a-1)*2 + 1
        if o[args[iArgs]] == nothing
            push(o.keys, args[iArgs])
            push(o.vals, args[iArgs+1])
        end
    end
    
    return Options(unmatchedKeys, unmatchedVals)
end

function ref(o::Options, x::Symbol)
    for a = 1:length(o.keys)
        if o.keys[a] == x
            return o.vals[a]
        end
    end
    return nothing
end

function show(o::Options)
    for a = 1:length(o.keys)
        print("$(o.keys[a]) = $(o.vals[a])")
        if a != length(o.keys)
            print(", ")
        end
    end
end
