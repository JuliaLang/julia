const OPTIONS_NONE = 0
const OPTIONS_WARN = 1
const OPTIONS_ERROR = 2
type Options
    keys::Vector{Symbol}
    vals::Vector{Any}
    checked::Vector{Bool}
    check_lock::Vector{Bool}
    check_behavior::Int   # one of OPTIONS_NONE, OPTIONS_WARN, OPTIONS_ERROR

    function Options(args...)
        check_behavior = OPTIONS_ERROR
        if length(args) % 2 != 0
            if isa(args[1],Int)
                check_behavior = args[1]
                args = args[2:end]
            else
                error("Parameter/value lists must come in pairs")
            end
        end
        n = div(length(args),2)
        if n > 0
            keys = [args[1:2:end]...]
            vals = [args[2:2:end]...]
        else
            keys = Array(Symbol,0)
            vals = Array(Any,0)
        end
        checked = falses(n)
        check_lock = falses(n)
        new(keys,vals,checked,check_lock,check_behavior)
    end
end
function ischeck(o::Options)
    ret = !o.check_lock
    o.check_lock[:] = true
    ret
end
function docheck(o::Options,checkflag::Vector{Bool})
    unchecked = find(checkflag & !o.checked[1:length(checkflag)])
    if !isempty(unchecked) && o.check_behavior != OPTIONS_NONE
        s = strcat("The following options were not used: ",string(o.keys[unchecked[1]]))
        for i = 2:length(unchecked)
            s = strcat(s,", ",string(o.keys[unchecked[i]]))
        end
        if o.check_behavior == OPTIONS_WARN
            println(s)
        else
            error(s)
        end
    end
end
function index(o::Options,s::Symbol)
    if has(o.symtable,s)
        ret = o.symtable[s]
    else
        ret = 0
    end
    ret
end
function push(o::Options,s::Symbol,v)
    push(o.keys,s)
    push(o.vals,v)
    push(o.checked,false)
    push(o.check_lock,false)
end
function fill_defaults(o::Options,pv...)
    if length(pv) % 2 != 0
        error("Parameter/value lists must come in pairs")
    end
    ht = HashTable{Symbol,Any}()
    for i = 1:2:length(pv)
        ht[pv[i]] = pv[i+1]
    end
    for i = 1:length(o.keys)
        if has(ht,o.keys[i])
            ht[o.keys[i]] = o.vals[i]
            o.checked[i] = true
        end
    end
    return ht
end

