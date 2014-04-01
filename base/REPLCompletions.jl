module REPLCompletions

export completions, shell_completions

using Base.Meta

function completes_global(x, name)
    return beginswith(x, name) && !('#' in x)
end

# REPL Symbol Completions
function complete_symbol(sym, ffunc)
    # Find module
    strs = split(sym, ".")
    # Maybe be smarter in the future
    context_module = Main

    mod = context_module
    lookup_module = true
    t = None
    for name in strs[1:(end-1)]
        s = symbol(name)
        if lookup_module
            if isdefined(mod, s)
                b = mod.(s)
                if isa(b, Module)
                    mod = b
                elseif Base.isstructtype(typeof(b))
                    lookup_module = false
                    t = typeof(b)
                else
                    # A.B.C where B is neither a type nor a
                    # module. Will have to be revisited if
                    # overloading is allowed
                    return ASCIIString[]
                end
            else
                # A.B.C where B doesn't exist in A. Give up
                return ASCIIString[]
            end
        else
            # We're now looking for a type
            fields = t.names
            found = false
            for i in 1:length(fields)
                if s == fields[i]
                    t = t.types[i]
                    if !Base.isstructtype(t)
                        return ASCIIString[]
                    end
                    found = true
                    break
                end
            end
            if !found
                #Same issue as above, but with types instead of modules
                return ASCIIString[]
            end
        end
    end

    name = strs[end]

    suggestions = String[]
    if lookup_module
        # We will exlcude the results that the user does not want, as well
        # as excluding Main.Main.Main, etc., because that's most likely not what
        # the user wants
        p = s->(ffunc(mod, s) && s != module_name(mod))
        # Looking for a binding in a module
        if mod == context_module
            # Also look in modules we got through `using`
            mods = ccall(:jl_module_usings, Any, (Any,), Main)
            for m in mods
                ssyms = names(m)
                filter!(p, ssyms)
                syms = map!(string, Array(UTF8String, length(ssyms)), ssyms)
                append!(suggestions, syms[map((x)->completes_global(x, name), syms)])
            end
            ssyms = names(mod, true, true)
            filter!(p, ssyms)
            syms = map!(string, Array(UTF8String, length(ssyms)), ssyms)
        else
            ssyms = names(mod, true, false)
            filter!(p, ssyms)
            syms = map!(string, Array(UTF8String, length(ssyms)), ssyms)
        end
        append!(suggestions, syms[map((x)->completes_global(x, name), syms)])
    else
        # Looking for a member of a type
        fields = t.names
        for field in fields
            s = string(field)
            if beginswith(s, name)
                push!(suggestions, s)
            end
        end
    end
    suggestions
end

function complete_keyword(s::ByteString)
    const sorted_keywords = [
        "abstract", "baremodule", "begin", "bitstype", "break", "catch", "ccall",
        "const", "continue", "do", "else", "elseif", "end", "export", "finally",
        "for", "function", "global", "if", "immutable", "import", "importall",
        "let", "local", "macro", "module", "quote", "return", "try", "type",
        "typealias", "using", "while"]
    r = searchsorted(sorted_keywords, s)
    i = first(r)
    n = length(sorted_keywords)
    while i <= n && beginswith(sorted_keywords[i],s)
        r = first(r):i
        i += 1
    end
    sorted_keywords[r]
end

function complete_path(path::ByteString)
    matches = ByteString[]
    dir, prefix = splitdir(path)
    if length(dir) == 0
        files = readdir()
    elseif isdir(dir)
        files = readdir(dir)
    else
        return matches
    end
    for file in files
        if beginswith(file, prefix)
            p = joinpath(dir, file)
            push!(matches, isdir(p) ? joinpath(p,"") : p)
        end
    end
    matches
end

function complete_methods(input::String)
    tokens = split(input, '.')
    fn = Main
    for token in tokens
        sym = symbol(token)
        isdefined(fn, sym) || return UTF8String[]
        fn = fn.(sym)
    end
    isgeneric(fn) || return UTF8String[]
    UTF8String[string(m) for m in methods(fn)]
end

const non_word_chars = " \t\n\"\\'`@\$><=:;|&{}()[].,+-*/?%^~"

function completions(string,pos)
    startpos = min(pos, 1)
    dotpos = 0
    instring = false
    incmd = false
    infunc = false
    escaped = false
    nearquote = false
    i = start(string)
    while i <= pos
        c,j = next(string, i)
        if c == '\\'
            instring && (escaped $= true)
            nearquote = false
        elseif c == '\''
            !instring && (nearquote = true)
        elseif c == '"'
            (!escaped && !nearquote && !incmd) && (instring $= true)
            escaped = nearquote = false
        elseif c == '`'
            (!escaped && !nearquote && !instring) && (incmd $= true)
            escaped = nearquote = false
        else
            escaped = nearquote = false
        end
        if c < 0x80
            if instring || incmd
                c in " \t\n\"\\'`@\$><=;|&{(" && (startpos = j)
            elseif c in non_word_chars
                if c == '.'
                    dotpos = i
                elseif i == pos && c == '('
                    infunc = true
                else
                    startpos = j
                end
            end
        end
        i = j
    end

    if instring || incmd
        r = startpos:pos
        paths = complete_path(string[r])
        if instring && length(paths) == 1
            paths[1] *= "\""
        end
        return sort(paths), r, true
    end

    ffunc = (mod,x)->true
    suggestions = UTF8String[]
    r = rsearch(string, "using", startpos)
    if infunc
        # We're right after the start of a function call
        return (complete_methods(string[startpos:pos-1]), startpos:pos,false)
    elseif !isempty(r) && all(isspace, string[nextind(string, last(r)):prevind(string, startpos)])
        # We're right after using. Let's look only for packages
        # and modules we can reach from here

        # If there's no dot, we're in toplevel, so we should
        # also search for packages
        s = string[startpos:pos]
        if dotpos <= startpos
            append!(suggestions, filter(pname->begin
                pname[1] != '.' &&
                pname != "METADATA" &&
                beginswith(pname, s)
            end,readdir(Pkg.dir())))
        end
        ffunc = (mod,x)->(isdefined(mod, x) && isa(mod.(x), Module))
    end
    startpos == 0 && (pos = -1)
    dotpos <= startpos && (dotpos = startpos - 1)
    s = string[startpos:pos]
    append!(suggestions, complete_keyword(s))
    append!(suggestions, complete_symbol(s, ffunc))
    return sort(unique(suggestions)), (dotpos+1):pos, true
end

function shell_completions(string,pos)
    # First parse everything up to the current position
    scs = string[1:pos]
    local args, last_parse
    try
        args, last_parse = Base.shell_parse(scs, true)
    catch
        return UTF8String[], 0:-1, false
    end
    # Now look at the last this we parsed
    isempty(args.args[end].args) && return UTF8String[], 0:-1, false
    arg = args.args[end].args[end]
    if isa(arg,String)
        # Treat this as a path (perhaps give a list of comands in the future as well?)
        dir,name = splitdir(arg)
        if isempty(dir)
            files = readdir()
        else
            isdir(dir) || return UTF8String[], 0:-1, false
            files = readdir(dir)
        end
        # Filter out files and directories that do not begin with the partial name we were
        # completing and append "/" to directories to simplify further completion
        ret = map(filter(x->beginswith(x, name), files)) do x
            if !isdir(joinpath(dir, x))
                return x
            else
                return x*"/"
            end
        end
        r = (nextind(string, pos-sizeof(name))):pos
        return ret, r, true
    elseif isexpr(arg, :escape) && (isexpr(arg.args[1], :incomplete) || isexpr(arg.args[1], :error))
        r = first(last_parse):prevind(last_parse, last(last_parse))
        partial = scs[r]
        ret, range = completions(partial, endof(partial))
        range += first(r) - 1
        return ret, range, true
    end
end

end # module
