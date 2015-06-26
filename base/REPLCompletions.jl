# This file is a part of Julia. License is MIT: http://julialang.org/license

module REPLCompletions

export completions, shell_completions, bslash_completions

using Base.Meta

function completes_global(x, name)
    return startswith(x, name) && !('#' in x)
end

function filtered_mod_names(ffunc::Function, mod::Module, name::AbstractString, all::Bool=false, imported::Bool=false)
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
    t = Union{}
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
            fields = fieldnames(t)
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
        fields = fieldnames(t)
        for field in fields
            s = string(field)
            if startswith(s, name)
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
    while i <= n && startswith(sorted_keywords[i],s)
        r = first(r):i
        i += 1
    end
    sorted_keywords[r]
end

function complete_path(path::AbstractString, pos)
    if Base.is_unix(OS_NAME) && ismatch(r"^~(?:/|$)", path)
        # if the path is just "~", don't consider the expanded username as a prefix
        if path == "~"
            dir, prefix = homedir(), ""
        else
            dir, prefix = splitdir(homedir() * path[2:end])
        end
    else
        dir, prefix = splitdir(path)
    end
    local files
    try
        if length(dir) == 0
            files = readdir()
        elseif isdir(dir)
            files = readdir(dir)
        else
            return UTF8String[], 0:-1, false
        end
    catch
        return UTF8String[], 0:-1, false
    end

    matches = UTF8String[]
    for file in files
        if startswith(file, prefix)
            id = try isdir(joinpath(dir, file)) catch; false end
            # joinpath is not used because windows needs to complete with double-backslash
            push!(matches, id ? file * (@windows? "\\\\" : "/") : file)
        end
    end
    matches = UTF8String[replace(s, r"\s", "\\ ") for s in matches]
    startpos = pos - endof(prefix) + 1 - length(matchall(r" ", prefix))
    # The pos - endof(prefix) + 1 is correct due to `endof(prefix)-endof(prefix)==0`,
    # hence we need to add one to get the first index. This is also correct when considering
    # pos, because pos is the `endof` a larger string which `endswith(path)==true`.
    return matches, startpos:pos, length(matches) > 0
end

# Determines whether method_complete should be tried. It should only be done if
# the string endswiths ',' or '(' when disregarding whitespace_chars
function should_method_complete(s::AbstractString)
    for c in reverse(s)
        if c in [',', '(']
            return true
        elseif !(c in whitespace_chars)
            return false
        end
    end
    false
end

# Returns a range that includes the method name in front of the first non
# closed start brace from the end of the string.
function find_start_brace(s::AbstractString)
    braces = 0
    r = RevString(s)
    i = start(r)
    in_single_quotes = false
    in_double_quotes = false
    in_back_ticks = false
    while !done(r, i)
        c, i = next(r, i)
        if !in_single_quotes && !in_double_quotes && !in_back_ticks
            if c == '('
                braces += 1
            elseif c == ')'
                braces -= 1
            elseif c == '\''
                in_single_quotes = true
            elseif c == '"'
                in_double_quotes = true
            elseif c == '`'
                in_back_ticks = true
            end
        else
            if !in_back_ticks && !in_double_quotes && c == '\''
                in_single_quotes = !in_single_quotes
            elseif !in_back_ticks && !in_single_quotes && c == '"'
                in_double_quotes = !in_double_quotes
            elseif !in_single_quotes && !in_double_quotes && c == '`'
                in_back_ticks = !in_back_ticks
            end
        end
        braces == 1 && break
    end
    braces != 1 && return 0:-1, -1
    method_name_end = reverseind(r, i)
    startind = nextind(s, rsearch(s, non_identifier_chars, method_name_end))
    return startind:endof(s), method_name_end
end

# Returns the value in a expression if sym is defined in current namespace fn.
# This method is used to iterate to the value of a expression like:
# :(Base.REPLCompletions.whitespace_chars) a `dump` of this expression
# will show it consist of Expr, QuoteNode's and Symbol's which all needs to
# be handled differently to iterate down to get the value of whitespace_chars.
function get_value(sym::Expr, fn)
    sym.head != :. && return (nothing, false)
    for ex in sym.args
        fn, found = get_value(ex, fn)
        !found && return (nothing, false)
    end
    fn, true
end
get_value(sym::Symbol, fn) = isdefined(fn, sym) ? (fn.(sym), true) : (nothing, false)
get_value(sym::QuoteNode, fn) = isdefined(fn, sym.value) ? (fn.(sym.value), true) : (nothing, false)
get_value(sym, fn) = sym, true

# Takes the argument of a function call and determine the type of signature of the method.
# If the function gets called with a val::DataType then it returns Type{val} else typeof(val)
method_type_of_arg(val::DataType) = Type{val}
method_type_of_arg(val) = typeof(val)

# Method completion on function call expression that look like :(max(1))
function complete_methods(ex_org::Expr)
    args_ex = DataType[]
    func, found = get_value(ex_org.args[1], Main)
    (!found || (found && !isgeneric(func))) && return UTF8String[]
    for ex in ex_org.args[2:end]
        val, found = get_value(ex, Main)
        found ? push!(args_ex, method_type_of_arg(val)) : push!(args_ex, Any)
    end
    out = UTF8String[]
    t_in = Tuple{args_ex...} # Input types
    for method in methods(func)
        # Check if the method's type signature intersects the input types
        typeintersect(Tuple{method.sig.parameters[1 : min(length(args_ex), end)]...}, t_in) != Union{} &&
            push!(out,string(method))
    end
    return out
end

include("latex_symbols.jl")
include("emoji_symbols.jl")

const non_identifier_chars = [" \t\n\r\"\\'`\$><=:;|&{}()[],+-*/?%^~"...]
const whitespace_chars = [" \t\n\r"...]

# Aux function to detect whether we're right after a
# using or import keyword
function afterusing(string::ByteString, startpos::Int)
    (isempty(string) || startpos == 0) && return false
    str = string[1:prevind(string,startpos)]
    isempty(str) && return false
    rstr = reverse(str)
    r = search(rstr, r"\s(gnisu|tropmi)\b")
    isempty(r) && return false
    fr = reverseind(str, last(r))
    return ismatch(r"^\b(using|import)\s*(\w+\s*,\s*)*\w*$", str[fr:end])
end

function bslash_completions(string, pos)
    slashpos = rsearch(string, '\\', pos)
    if (rsearch(string, whitespace_chars, pos) < slashpos &&
        !(1 < slashpos && (string[prevind(string, slashpos)]=='\\')))
        # latex / emoji symbol substitution
        s = string[slashpos:pos]
        latex = get(latex_symbols, s, "")
        if !isempty(latex) # complete an exact match
            return (true, ([latex], slashpos:pos, true))
        end
        emoji = get(emoji_symbols, s, "")
        if !isempty(emoji)
            return (true, ([emoji], slashpos:pos, true))
        end
        # return possible matches; these cannot be mixed with regular
        # Julian completions as only latex / emoji symbols contain the leading \
        if startswith(s, "\\:") # emoji
            emoji_names = filter(k -> startswith(k, s), keys(emoji_symbols))
            return (true, (sort!(collect(emoji_names)), slashpos:pos, true))
        else # latex
            latex_names = filter(k -> startswith(k, s), keys(latex_symbols))
            return (true, (sort!(collect(latex_names)), slashpos:pos, true))
        end
    end
    return (false, (UTF8String[], 0:-1, false))
end

function completions(string, pos)
    # First parse everything up to the current position
    partial = string[1:pos]
    inc_tag = Base.syntax_deprecation_warnings(false) do
        Base.incomplete_tag(parse(partial, raise=false))
    end
    if inc_tag in [:cmd, :string]
        m = match(r"[\t\n\r\"'`@\$><=;|&\{]| (?!\\)", reverse(partial))
        startpos = nextind(partial, reverseind(partial, m.offset))
        r = startpos:pos
        paths, r, success = complete_path(replace(string[r], r"\\ ", " "), pos)
        if inc_tag == :string &&
           length(paths) == 1 &&                              # Only close if there's a single choice,
           !isdir(expanduser(replace(string[startpos:start(r)-1] * paths[1], r"\\ ", " "))) &&  # except if it's a directory
           (length(string) <= pos || string[pos+1] != '"')    # or there's already a " at the cursor.
            paths[1] *= "\""
        end
        #Latex symbols can be completed for strings
        (success || inc_tag==:cmd) && return sort(paths), r, success
    end

    ok, ret = bslash_completions(string, pos)
    ok && return ret

    # Make sure that only bslash_completions is working on strings
    inc_tag==:string && return UTF8String[], 0:-1, false

     if inc_tag == :other && should_method_complete(partial)
        frange, method_name_end = find_start_brace(partial)
        ex = Base.syntax_deprecation_warnings(false) do
            parse(partial[frange] * ")", raise=false)
        end
        if isa(ex, Expr) && ex.head==:call
            return complete_methods(ex), start(frange):method_name_end, false
        end
    elseif inc_tag == :comment
        return UTF8String[], 0:-1, false
    end

    dotpos = rsearch(string, '.', pos)
    startpos = nextind(string, rsearch(string, non_identifier_chars, pos))

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
            for dir in [Pkg.dir(); LOAD_PATH; pwd()]
                isdir(dir) || continue
                for pname in readdir(dir)
                    if pname[1] != '.' && pname != "METADATA" &&
                        pname != "REQUIRE" && startswith(pname, s)
                        # Valid file paths are
                        #   <Mod>.jl
                        #   <Mod>/src/<Mod>.jl
                        #   <Mod>.jl/src/<Mod>.jl
                        if isfile(joinpath(dir, pname))
                            endswith(pname, ".jl") && push!(suggestions, pname[1:end-3])
                        else
                            mod_name = if endswith(pname, ".jl")
                                pname[1:end - 3]
                            else
                                pname
                            end
                            if isfile(joinpath(dir, pname, "src",
                                               "$mod_name.jl"))
                                push!(suggestions, mod_name)
                            end
                        end
                    end
                end
            end
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

function shell_completions(string, pos)
    # First parse everything up to the current position
    scs = string[1:pos]
    local args, last_parse
    try
        args, last_parse = Base.shell_parse(scs, true)
    catch
        return UTF8String[], 0:-1, false
    end
    # Now look at the last thing we parsed
    isempty(args.args[end].args) && return UTF8String[], 0:-1, false
    arg = args.args[end].args[end]
    if all(s -> isa(s, AbstractString), args.args[end].args)
        # Treat this as a path (perhaps give a list of commands in the future as well?)
        return complete_path(join(args.args[end].args), pos)
    elseif isexpr(arg, :escape) && (isexpr(arg.args[1], :incomplete) || isexpr(arg.args[1], :error))
        r = first(last_parse):prevind(last_parse, last(last_parse))
        partial = scs[r]
        ret, range = completions(partial, endof(partial))
        range += first(r) - 1
        return ret, range, true
    end
    return UTF8String[], 0:-1, false
end

end # module
