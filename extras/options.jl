#### options.jl ####
# A framework for providing optional arguments to functions

# Harlan Harris & Timothy E. Holy, with contributions from Stefan
# Karpinski, Patrick O'Leary, and Jeff Bezanson
module OptionsMod
import Base.*
# can't get Base.ht_keyindex from dict.jl -- will pull it manually

export Options,
	CheckNone, CheckWarn, CheckError,
	add_defaults!, show, ref, assign,
	ischeck, docheck, clearcheck,
	@options, @defaults, @check_used, @set_options
	
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
function show{T<:OptionsChecking}(io, o::Options{T})
    # Put them in the same order specified by the user
    key = Array(ASCIIString,length(o.vals))
    for (k, v) = o.key2index
        key[v] = string(k)
    end
    for i = 1:length(key)
        print(io, "$(key[i]) = $(o.vals[i])")
        if i < length(key)
            print(io, ", ")
        end
    end
    print(io, " ($T)")
end


#### Functions ####
# Return an options setting
function ref(o::Options,s::Symbol)
    index = Base.ht_keyindex(o.key2index,s)
    if index > 0
        index = o.key2index.vals[index]
        return o.vals[index]
    else
        return nothing
    end
end
# Re-set an options setting, or add a new one
function assign(o::Options,v,s::Symbol)
    index = Base.ht_keyindex(o.key2index,s)
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
function ischeck(a)
    error("First argument must be an options type")
end
function docheck_common(o::Options,checkflag::Vector{Bool})
    unused = checkflag & !o.used[1:length(checkflag)]
    msg = ""
    if any(unused)
        s = Array(ASCIIString,0)
        for (k, v) = o.key2index
            if v <= length(unused) && unused[v]
                push(s,string(k))
            end
        end
        msg = strcat("The following option(s) were unused: ",s)
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
    for i = 1:length(checkflag)
        if checkflag[i]
            o.used[i] = false
            o.check_lock[i] = false
        end
    end
end



#### Macros ####
# Macro to set the defaults. Usage:
#     @defaults opts a=3 b=a+2 c=f(a,b)
# After executing this macro, you can use a, b, and c as ordinary variables
macro defaults(opts,ex...)
    # Create a new variable storing the checkflag
    varname = strcat("_",string(opts),"_checkflag")
    exret = :($(esc(symbol(varname))) = ischeck($(esc(opts))))
    # Transform the tuple into a vector, so that
    # we can manipulate it
    ex = {ex...}
    # Check each argument in the assignment list
    i = 1
    while i <= length(ex)
        y = ex[i]
        if isa(y, Expr) && y.head == :block
            # Found a begin..end block: expand its contents in-place
            # and restart from the same position
            del(ex, i)
            i0 = i
            for z in y.args
                insert(ex, i, z)
                i += 1
            end
            i = i0
            continue
        elseif isa(y, LineNumberNode)
            # A line number node, ignore
            i += 1
            continue
        elseif !isa(y,Expr) || !(y.head == :(=) || y.head == :(=>) || y.head == :(:=))
            error("Arguments to @defaults following the options variable must be assignments, e.g., a=5 or a=>5")
        end
        y.head = :(=)

        sym = y.args[1]
        exret = quote
            $exret
            htindex = Base.ht_keyindex($(esc(opts)).key2index,$(expr(:quote,sym)))
            if htindex > 0
                htindex = $(esc(opts)).key2index.vals[htindex]
                $(esc(sym)) = $(esc(opts)).vals[htindex]
                $(esc(opts)).used[htindex] = true
            else
                $(esc(y))
            end
        end
        i += 1
    end
    exret
end

# Macro to check whether options were used (optional, use only if you
# want to protect the user from typos or other nonsensical settings)
# Usage:
#    @check_used opts
macro check_used(opts)
    varname = strcat("_",string(opts),"_checkflag")
    :(docheck($(esc(opts)),$(esc(symbol(varname)))))
end

# Macro for setting options. Usage:
#    opts = @options a=5 b=7
#    opts = @options CheckWarn a=5 b=7
macro options(ex...)
    callargs = Any[:Options]
    # Transform the tuple into a vector, so that
    # we can manipulate it
    ex = {ex...}
    i = 1
    if length(ex) >= 1 && isa(ex[1], Symbol)
        push(callargs, esc(ex[1]))
        i += 1
    end
    while i <= length(ex)
        y = ex[i]
        if isa(y, Expr) && y.head == :block
            # Found a begin..end block: expand its contents in-place
            # and restart from the same position
            del(ex, i)
            i0 = i
            for z in y.args
                insert(ex, i, z)
                i += 1
            end
            i = i0
            continue
        elseif isa(y, LineNumberNode)
            # A line number node, ignore
            i += 1
            continue
        elseif !isa(y,Expr) || !(y.head == :(=) || y.head == :(=>) || y.head == :(:=))
            error("Arguments to @options must be assignments, e.g., a=5 or a=>5")
        end
        push(callargs, expr(:quote, y.args[1]))
        push(callargs, esc(y.args[2]))
        i += 1
    end
    expr(:call, callargs)
end

# Macro to reset or append additional options. Usage:
#    @set_options opts c=25 d=4
macro set_options(opts,ex...)
    exret = quote
    end
    # Transform the tuple into a vector, so that
    # we can manipulate it
    ex = {ex...}
    # Check each argument in the assignment list
    i = 1
    while i <= length(ex)
        y = ex[i]
        if isa(y, Expr) && y.head == :block
            # Found a begin..end block: expand its contents in-place
            # and restart from the same position
            del(ex, i)
            i0 = i
            for z in y.args
                insert(ex, i, z)
                i += 1
            end
            i = i0
            continue
        elseif isa(y, LineNumberNode)
            # A line number node, ignore
            i += 1
            continue
        elseif !isa(y,Expr) || !(y.head == :(=) || y.head == :(=>) || y.head == :(:=))
            error("Arguments to @set_options following the options variable must be assignments, e.g., a=5 or a=>5")
        end
        y.head = :(=)

        sym = y.args[1]
        val = y.args[2]
        exret = quote
            $exret
            $(esc(opts))[$(expr(:quote,sym))] = $(esc(val))
        end
        i += 1
    end
    exret
end

end # module
