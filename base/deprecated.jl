# This file is a part of Julia. License is MIT: http://julialang.org/license

# Deprecated functions and objects
#
# Please add new deprecations at the bottom of the file.
# A function deprecated in a release will be removed in the next one.
# Please also add a reference to the pull request which introduced the
# deprecation.
#
# For simple cases where a direct replacement is available, use @deprecate:
# the first argument is the signature of the deprecated method, the second one
# is the call which replaces it. Remove the definition of the deprecated method
# and unexport it, as @deprecate takes care of calling the replacement
# and of exporting the function.
#
# For more complex cases, move the body of the deprecated method in this file,
# and call depwarn() directly from inside it. The symbol depwarn() expects is
# the name of the function, which is used to ensure that the deprecation warning
# is only printed the first time for each call place.

macro deprecate(old,new)
    meta = Expr(:meta, :noinline)
    if isa(old,Symbol)
        oldname = Expr(:quote,old)
        newname = Expr(:quote,new)
        Expr(:toplevel,
            Expr(:export,esc(old)),
            :(function $(esc(old))(args...)
                  $meta
                  depwarn(string($oldname," is deprecated, use ",$newname," instead."),
                          $oldname)
                  $(esc(new))(args...)
              end))
    elseif isa(old,Expr) && old.head == :call
        remove_linenums!(new)
        oldcall = sprint(io->show_unquoted(io,old))
        newcall = sprint(io->show_unquoted(io,new))
        oldsym = if isa(old.args[1],Symbol)
            old.args[1]
        elseif isa(old.args[1],Expr) && old.args[1].head == :curly
            old.args[1].args[1]
        else
            error("invalid usage of @deprecate")
        end
        oldname = Expr(:quote, oldsym)
        Expr(:toplevel,
            Expr(:export,esc(oldsym)),
            :($(esc(old)) = begin
                  $meta
                  depwarn(string($oldcall," is deprecated, use ",$newcall," instead."),
                          $oldname)
                  $(esc(new))
              end))
    else
        error("invalid usage of @deprecate")
    end
end

remove_linenums!(ex) = ex
function remove_linenums!(ex::Expr)
    filter!(x->!((isa(x,Expr) && is(x.head,:line)) || isa(x,LineNumberNode)), ex.args)
    for subex in ex.args
        remove_linenums!(subex)
    end
    ex
end

function depwarn(msg, funcsym)
    opts = JLOptions()
    if opts.depwarn > 0
        ln = unsafe_load(cglobal(:jl_lineno, Int))
        fn = bytestring(unsafe_load(cglobal(:jl_filename, Ptr{Cchar})))
        bt = backtrace()
        caller = firstcaller(bt, funcsym)
        if opts.depwarn == 1 # raise a warning
            warn(msg, once=(caller != C_NULL), key=caller, bt=bt,
                 filename=fn, lineno=ln)
        elseif opts.depwarn == 2 # raise an error
            throw(ErrorException(msg))
        end
    end
end

function firstcaller(bt::Array{Ptr{Void},1}, funcsym::Symbol)
    # Identify the calling line
    i = 1
    while i <= length(bt)
        lkup = ccall(:jl_lookup_code_address, Any, (Ptr{Void},Cint), bt[i], true)
        i += 1
        if lkup === ()
            continue
        end
        fname, file, line, inlinedat_file, inlinedat_line, fromC = lkup
        if fname == funcsym
            break
        end
    end
    if i <= length(bt)
        return bt[i]
    end
    return C_NULL
end

# 0.5 deprecations
