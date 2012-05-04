# Allow calling of functions in C++ shared libraries.
# Usage: a = @cpp ccall(dlsym(mylib,:mysymbol),...)
macro cpp(ex)
    # If you get "undefined symbol" errors, use nm or readelf to view
    # the names of the symbols in the library. Then use the resulting
    # information to help improve this macro!
    # Note: for global objects without class or namespace qualifiers
    # (e.g., global consts), you do not need to use this macro (it
    # will work with a plain ccall, even though it is a C++ library)
    msg = "@cpp requires a ccall(dlsym(mylib,:mysymbol),...)  expression"
    if !isa(ex,Expr) || ex.head != :ccall
        error(msg)
    end
    # Parse the library symbol's name
    extmp = ex.args[1]
    if isa(extmp,Expr) && extmp.head == :call && extmp.args[1] == :dlsym
        sym = extmp.args[3]
    else
        error(msg)
    end
    fstr = string(sym)
    fstr = fstr[2:end]   # strip the :
    #GNU3-4 ABI
    fstr = string("_Z",strlen(fstr),fstr)
    # Parse the arguments to ccall and construct the parameter type string
    extmp = ex.args[3]
    if extmp.head != :tuple
        error(msg)
    end
    exargs = extmp.args
    pstr = ""
    symtable = (:Void,:Bool,:Cchar,:Char,:ASCIIString,:Int,:Int8,:Uint8,:Int16,:Uint16,:Int32,:Uint32,:Int64,:Uint64,:Float32,:Float64)
    # GNU3-4 ABI v.3 and v.4
    ptable = ('v','b','c','w',"Pc",'i','a','h','s','t','i','j','l','m','f','d')
    for iarg = 1:length(exargs)
        thisarg = exargs[iarg]
        if isa(thisarg,Expr) && thisarg.head == :curly && thisarg.args[1] == :Ptr
            pstr = string(pstr,'P')
            thisarg = thisarg.args[2]
        end
        matched = false
        for isym = 1:length(symtable)
            if thisarg == symtable[isym]
                matched = true
                pstr = string(pstr,ptable[isym])
                # Cchar is a special notation just for name mangling,
                # convert back to :Int8
                if thisarg == :Cchar
                    ex.args[3].args[iarg] = :Int8
                end
                break
            end
        end
        if !matched
            println(thisarg)
            error("Argument not recognized")
        end
    end
    ex.args[1].args[3] = strcat(fstr,pstr)
    ex
end
