module REPLCompletions

export completions, shell_completions

using Base.Meta

function completes_global(x, name)
    return beginswith(x, name) && !('#' in x)
end

function filtered_mod_names(ffunc::Function, mod::Module, name::String, all::Bool=false, imported::Bool=false)
    ssyms = names(mod, all, imported)
    filter!(ffunc, ssyms)
    syms = UTF8String[string(s) for s in ssyms]
    filter!(x->completes_global(x, name), syms)
end

# REPL Symbol Completions
function complete_symbol(sym, ffunc)
    # Find module
    strs = split(sym, '.')
    # Maybe be smarter in the future
    context_module = Main

    mod = context_module
    lookup_module = true
    t = None
    for name in strs[1:(end-1)]
        s = symbol(name)
        if lookup_module
            # If we're considering A.B.C where B doesn't exist in A, give up
            isdefined(mod, s) || return UTF8String[]
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
                return UTF8String[]
            end
        else
            # We're now looking for a type
            fields = t.names
            found = false
            for i in 1:length(fields)
                s == fields[i] || continue
                t = t.types[i]
                Base.isstructtype(t) || return UTF8String[]
                found = true
                break
            end
            #Same issue as above, but with types instead of modules
            found || return UTF8String[]
        end
    end

    name = strs[end]

    suggestions = UTF8String[]
    if lookup_module
        # We will exclude the results that the user does not want, as well
        # as excluding Main.Main.Main, etc., because that's most likely not what
        # the user wants
        p = s->(ffunc(mod, s) && s != module_name(mod))
        # Looking for a binding in a module
        if mod == context_module
            # Also look in modules we got through `using`
            mods = ccall(:jl_module_usings, Any, (Any,), Main)
            for m in mods
                append!(suggestions, filtered_mod_names(p, m, name))
            end
            append!(suggestions, filtered_mod_names(p, mod, name, true, true))
        else
            append!(suggestions, filtered_mod_names(p, mod, name, true, false))
        end
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

const non_word_chars = [" \t\n\"\\'`@\$><=:;|&{}()[],+-*/?%^~"...]
const non_filename_chars = [" \t\n\"\\'`@\$><=;|&{("...]

# Aux function to detect whether we're right after a
# using or import keyword
function afterusing(string::ByteString, startpos::Int)
    (isempty(string) || startpos == 0) && return false
    str = string[1:prevind(string,startpos)]
    isempty(str) && return false
    rstr = reverse(str)
    r = search(rstr, r"\s(gnisu|tropmi)\b")
    isempty(r) && return false
    fr = chr2ind(str, length(str)-ind2chr(rstr, last(r))+1)
    return ismatch(r"^\b(using|import)\s*(\w+\s*,\s*)*\w*$", str[fr:end])
end

function completions(string, pos)
    inc_tag = Base.incomplete_tag(parse(string[1:pos], raise=false))
    if inc_tag in [:cmd, :string]
        startpos = nextind(string, rsearch(string, non_filename_chars, pos))
        r = startpos:pos
        paths = complete_path(string[r])
        if inc_tag == :string && length(paths) == 1
            paths[1] *= "\""
        end
        return sort(paths), r, true
    elseif inc_tag == :other && string[pos] == '('
        endpos = prevind(string, pos)
        startpos = nextind(string, rsearch(string, non_word_chars, endpos))
        return complete_methods(string[startpos:endpos]), startpos:endpos, false
    elseif inc_tag == :comment
        return UTF8String[], 0:-1, false
    end

    dotpos = rsearch(string, '.', pos)
    startpos = nextind(string, rsearch(string, non_word_chars, pos))

    ffunc = (mod,x)->true
    suggestions = UTF8String[]
    comp_keywords = true
    if afterusing(string, startpos)
        # We're right after using or import. Let's look only for packages
        # and modules we can reach from here

        # If there's no dot, we're in toplevel, so we should
        # also search for packages
        s = string[startpos:pos]
        if dotpos <= startpos
            append!(suggestions, filter(readdir(Pkg.dir())) do pname
                pname[1] != '.' &&
                pname != "METADATA" &&
                pname != "REQUIRE" &&
                beginswith(pname, s)
            end)
        end
        ffunc = (mod,x)->(isdefined(mod, x) && isa(mod.(x), Module))
        comp_keywords = false
    end
    startpos == 0 && (pos = -1)
    dotpos <= startpos && (dotpos = startpos - 1)
    s = string[startpos:pos]
    comp_keywords && append!(suggestions, complete_keyword(s))
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
