#### options.jl ####
# A framework for providing optional arguments to functions

# Harlan Harris & Timothy E. Holy, with contributions from Stefan
# Karpinski, Patrick O'Leary, and Jeff Bezanson

#### Options type ####
abstract OptionsChecking
type CheckNone <: OptionsChecking
end
type CheckWarn <: OptionsChecking
end
type CheckError <: OptionsChecking
end
type Options{T<:OptionsChecking}
    key2index::Dict{Symbol,Int}
    vals::Vector
    used::Vector{Bool}
    check_lock::Vector{Bool}
end
# Constructor: supply type followed by parameter/value pairs,
#   o = Options(CheckWarn,:a,5,:b,rand(3),...)
# Note, the macro @options makes construction easier
function Options{T<:OptionsChecking}(::Type{T},args...)
    if isodd(length(args))
        error("Parameter/value lists must come in pairs")
    end
    n = div(length(args),2)
    keys, index, vals = if n > 0
        (args[1:2:end], ntuple(n, identity), Any[args[2:2:end]...])
    else
        ((), (), Array(Any, 0))
    end
    ht = Dict{Symbol,Int}(keys,index)
    used = falses(n)
    check_lock = falses(n)
    Options{T}(ht,vals,used,check_lock)
end
# Constructor: supply type followed by list of assignment expressions, e.g.,
#   o = Options(CheckNone,:(a=5),:(b=rand(3)),...)
function Options{T<:OptionsChecking}(::Type{T},ex::Expr...)
    ht = Dict{Symbol,Int}()
    vals = Array(Any,0)
    n = length(ex)
    for i = 1:n
        if ex[i].head != :(=)
            error("All expressions must be assignments")
        end
        ht[ex[i].args[1]] = i
        push(vals,ex[i].args[2])
    end
    used = falses(n)
    check_lock = falses(n)
    Options{T}(ht,vals,used,check_lock)
end
# If no type specified, CheckError is the default
Options(p1::Symbol,v1,args...) = Options(CheckError,p1,v1,args...)
Options(ex::Expr...) = Options(CheckError,ex...)

function copy{T<:OptionsChecking}(o::Options{T})
    Options{T}(copy(o.key2index),
            copy(o.vals),
            copy(o.used),
            copy(o.check_lock))
end
function convert{Tnew<:OptionsChecking,Told<:OptionsChecking}(::Type{Tnew},o::Options{Told})
    Options{Tnew}(o.key2index,o.vals,o.used,o.check_lock)
end
function show{T<:OptionsChecking}(o::Options{T})
    # Put them in the same order specified by the user
    key = Array(ASCIIString,length(o.vals))
    for (k, v) = o.key2index
        key[v] = string(k)
    end
    for i = 1:length(key)
        print("$(key[i]) = $(o.vals[i])")
        if i < length(key)
            print(", ")
        end
    end
    print(" ($T)")
end


#### Functions ####
# Return an options setting
function ref(o::Options,s::Symbol)
    index = ht_keyindex(o.key2index,s)
    if index > 0
        index = o.key2index.vals[index]
        return o.vals[index]
    else
        return nothing
    end
end
# Re-set an options setting, or add a new one
function assign(o::Options,v,s::Symbol)
    index = ht_keyindex(o.key2index,s)
    if index > 0
        index = o.key2index.vals[index]
        o.vals[index] = v
        o.used[index] = false
        o.check_lock[index] = false
    else
        o.key2index[s] = length(o.vals)+1
        push(o.vals,v)
        push(o.used,false)
        push(o.check_lock,false)
    end
end
# Functions for "claiming" and checking usage of individual options
function ischeck(o::Options)
    ret = !o.check_lock   # items marked true are to be checked by caller
    o.check_lock[ret] = true
    return ret
end
function ischeck(o::Options{CheckNone})
    return falses(length(o.vals))
end
function docheck_common(o::Options,checkflag::Vector{Bool})
    unused = checkflag & !o.used[1:length(checkflag)]
    msg = ""
    if any(unused)
        s = Array(ASCIIString,0)
        for (k, v) = o.key2index
            if unused[v]
                push(s,string(k))
            end
        end
        msg = strcat("The following options were not used: ",s)
    end
    return unused, msg
end
function docheck(o::Options{CheckNone},checkflag::Vector{Bool})
    clearcheck(o,checkflag)
end
function docheck(o::Options{CheckWarn},checkflag::Vector{Bool})
    unused, msg = docheck_common(o,checkflag)
    if any(unused)
        println("Warning: ",msg)
    end
    clearcheck(o,checkflag)
end
function docheck(o::Options{CheckError},checkflag::Vector{Bool})
    unused, msg = docheck_common(o,checkflag)
    clearcheck(o,checkflag)  # in case it's in a try/catch block...
    if any(unused)
        error(msg)
    end
end
# Reset status on handled options (in case o is reused later)
function clearcheck(o::Options,checkflag::Vector{Bool})
    o.used[checkflag] = false
    o.check_lock[checkflag] = false
end



#### Macros ####
# Macro to set the defaults. Usage:
#     @defaults opts a=3 b=a+2 c=f(a,b)
# After executing this macro, you can use a, b, and c as ordinary variables
macro defaults(opts,ex...)
    # Create a new variable storing the checkflag
    varname = strcat("_",string(opts),"_checkflag")
    exret = :($symbol(varname) = ischeck($opts))
    # Check each argument in the assignment list
    htindex = gensym()
    for i = 1:length(ex)
        thisex = ex[i]
        if !isa(thisex,Expr) || thisex.head != :(=)
            error("@defaults: following the options variable, all statements must be assignments")
        end
        thissym = thisex.args[1]
        exret = quote
            $exret
            $htindex = ht_keyindex(($opts).key2index,$expr(:quote,thissym))
            if $htindex > 0
                $htindex = ($opts).key2index.vals[$htindex]
                $thissym = ($opts).vals[$htindex]
                ($opts).used[$htindex] = true
            else
                $thisex
            end
        end
    end
    exret
end

# Macro to check whether options were used (optional, use only if you
# want to protect the user from typos or other nonsensical settings)
# Usage:
#    @check_used opts
macro check_used(opts)
    varname = gensym()
    varname = strcat("_",string(opts),"_checkflag")
    :(docheck($opts,$symbol(varname)))
end

# Macro for setting options. Usage:
#    opts = @options a=5 b=7
#    opts = @options CheckWarn a=5 b=7
macro options(ex...)
    callargs = Any[:Options]
    istart = 1
    if isa(ex[1], Symbol)
        push(callargs, ex[1])
        istart = 2
    end
    for indx = istart:length(ex)
        if !isa(ex[indx], Expr)
            error("Arguments to @options must be assignments, e.g., a=5")
        end
        push(callargs, expr(:quote, ex[indx].args[1]))
        push(callargs, ex[indx].args[2])
    end
    expr(:call, callargs)
end

# Macro to reset or append additional options. Usage:
#    @set_options opts c=25 d=4
macro set_options(opts,ex...)
    exret = quote
    end
    for indx = 1:length(ex)
        thisex = ex[indx]
        if !isa(thisex,Expr) || thisex.head != :(=)
            error("Arguments to add_options must be a list of assignments, e.g.,a=5")
        end
        thissym = thisex.args[1]
        thisval = thisex.args[2]
        exret = quote
            $exret
            ($opts)[$expr(:quote,thissym)] = $thisval
        end
    end
    exret
end
