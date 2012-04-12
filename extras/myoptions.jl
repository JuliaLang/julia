#### options.jl ####
# A framework for providing optional arguments to functions

# Harlan Harris, Timothy E. Holy, and Stefan Karpinski

#### Options type ####
const OPTIONS_NONE = 0
const OPTIONS_WARN = 1
const OPTIONS_ERROR = 2
type Options
    keyindex::HashTable{Symbol,Int}
    vals::Vector
    checked::Vector{Bool}
    check_lock::Vector{Bool}
    check_behavior::Int   # one of OPTIONS_NONE, OPTIONS_WARN, OPTIONS_ERROR
end
# Constructor: supply check_behavior and parameter/value pairs,
# where the parameter is expressed as a symbol, e.g.
#   Options(OPTIONS_WARN,:a,5,:b,rand(3),...)
# Note, the macro @options makes construction easier
function Options(check_behavior::Int,args...)
    if length(args) % 2 != 0
        error("Parameter/value lists must come in pairs")
    end
    n = div(length(args),2)
    if n > 0
        keys = args[1:2:end]
        indx = ntuple(n,i->i)
        vals = [args[2:2:end]...]
    else
        keys = ()
        indx = ()
        vals = Array(Any,0)
    end
    ht = HashTable{Symbol,Int}(keys,indx)
    checked = falses(n)
    check_lock = falses(n)
    Options(ht,vals,checked,check_lock,check_behavior)
end
# Constructor given a list of assignment expressions,
#   Options(OPTIONS_NONE,:(a=5),:(b=rand(3)),...)
function Options(check_behavior::Int,ex::Expr...)
    pv = Array(Any,2*length(ex))
    for i = 1:length(ex)
        if ex[i].head != :(=)
            error("All expressions must be assignments")
        end
        pv[2*i-1] = ex[i].args[1]
        pv[2*i] = ex[i].args[2]
    end
    Options(check_behavior,pv...)
end
Options(ex::Expr...) = Options(OPTIONS_ERROR,ex...)
function copy(o::Options)
    Options(copy(o.keyindex),
            copy(o.vals),
            copy(o.checked),
            copy(o.check_lock),
            copy(o.check_behavior))
end



#### Functions ####
# Add a new options setting
function assign(o::Options,v,s::Symbol)
    o.keyindex[s] = length(o.vals)+1
    push(o.vals,v)
    push(o.checked,false)
    push(o.check_lock,false)
end
# Functions for "claiming" and checking usage of individual options
function ischeck(o::Options)
    if o.check_behavior == OPTIONS_NONE
        return falses(length(o.vals))
    end
    ret = !o.check_lock   # items marked true are to be checked by caller
    o.check_lock[ret] = true
    return ret
end
function docheck(o::Options,checkflag::Vector{Bool})
    if o.check_behavior != OPTIONS_NONE
        println("Checked: ",o.checked)
        println("checkflag: ",checkflag)
        unchecked = checkflag & !o.checked[1:length(checkflag)]
        println("unchecked: ",unchecked)
        if any(unchecked)
            s = ""
            for (k, v) = o.keyindex
                if unchecked[v]
                    s = [s,string(k)]
                end
            end
            s = s[2:end]
            msg = "The following options were not used: "
            if o.check_behavior == OPTIONS_WARN
                println("Warning: ",msg,s)
            else
                clearcheck(o,checkflag)  # in case inside try/catch block
                error(msg,s)
            end
        end
    end
    clearcheck(o,checkflag)
end
# Reset status on handled options (in case o is reused later)
function clearcheck(o::Options,checkflag::Vector{Bool})
    # Note checkflag may be shorter than o.checked and o.check_lock,
    # so can't just say o.checked[checkflag] = false
    for i = 1:length(checkflag)
        if checkflag[i]
            o.check_lock[i] = false
            o.checked[i] = false
        end
    end
end


# Given a tuple of assignment expressions, check to see whether the
# left-hand-side appears as a symbol in the options table. If so,
# replace the rhs in the assignment with the value in options.
function assign_replace(o::Options,ex::(Expr...))
    exout = copy(ex)
    for i = 1:length(ex)
        if ex[i].head == :(=)
            try  # try/catch requires only 1 table traversal, "has" needs 2?
                indx = o.keyindex[ex[i].args[1]]
                exout[i].args[2] = o.vals[indx]
                o.checked[indx] = true
            catch
            end
        end
    end
    return exout
end
# Create a variable using the following syntax:
#    eval(assign_str("fred",5))
function assign_str(name::String,val)
    expr(:(=),Any[symbol(name),val])
end

#### Convenience macros ####
# Macro to set the defaults. Usage:
#     @defaults opts a=3 b=a+2 c=f(a,b)
# After executing this macro, you can use a, b, and c as ordinary variables
macro defaults(opts,ex...)
    indx, extmp, thisex, varname = gensym(4)
    quote
        $extmp = assign_replace($opts,$ex)
        for $indx = 1:length($extmp)
            eval(($extmp)[$indx])
        end
        $varname = strcat("_",$string(opts),"_checkflag")
        eval(assign_str($varname,ischeck($opts)))
    end
    # The last line executes local _nameofopts_checkflag = ischeck(nameofopts)
end

# Macro to check whether options were used (optional, use only if you
# want to protect the user from typos or other nonsensical settings)
# Usage:
#    @check_options_used opts
macro check_options_used(opts)
    varname = gensym()
    quote
        $varname = strcat("_",$string(opts),"_checkflag")
        eval(expr(:call,Any[:docheck,$opts,symbol($varname)]))
    end
    # The last line executes docheck(namedopts,_namedopts_checkflag)
end

# Macro for setting options. Usage:
#    opts = @options a=5 b=7
#    opts = @options OPTIONS_WARN a=5 b=7
macro options(ex...)
    quote
        if isa(($ex)[1],Expr)
            Options(($ex)...)
        else
            Options($ex[1],(($ex)[2:end])...)
        end
    end
end

# Macro to append additional options. Usage:
#    @add_options opts c=25 d=4
macro add_options(opts,ex...)
    indx = gensym()
    quote
        for $indx = 1:length($ex)
            if ($ex)[$indx].head != :(=)
                error("Arguments to add_options must be a list of assignments, e.g., a=5")
            end
            ($opts)[($ex)[$indx].args[1]] = ($ex)[$indx].args[2]
        end
    end
end
