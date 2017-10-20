# This file is a part of Julia. License is MIT: https://julialang.org/license

module REPLCompletions

export completions, shell_completions, bslash_completions

using Base.Meta

function completes_global(x, name)
    return startswith(x, name) && !('#' in x)
end

function appendmacro!(syms, macros, needle, endchar)
    for s in macros
        if endswith(s, needle)
            from = nextind(s, start(s))
            to = prevind(s, sizeof(s)-sizeof(needle)+1)
            push!(syms, s[from:to]*endchar)
        end
    end
end

function filtered_mod_names(ffunc::Function, mod::Module, name::AbstractString, all::Bool=false, imported::Bool=false)
    ssyms = names(mod, all, imported)
    filter!(ffunc, ssyms)
    syms = String[string(s) for s in ssyms]
    macros =  filter(x -> startswith(x, "@" * name), syms)
    appendmacro!(syms, macros, "_str", "\"")
    appendmacro!(syms, macros, "_cmd", "`")
    filter!(x->completes_global(x, name), syms)
    return syms
end

# REPL Symbol Completions
function complete_symbol(sym, ffunc)
    # Maybe be smarter in the future
    context_module = Main
    mod = context_module
    name = sym

    lookup_module = true
    t = Union{}
    if rsearch(sym, non_identifier_chars) < rsearch(sym, '.')
        # Find module
        lookup_name, name = rsplit(sym, ".", limit=2)

        ex = Base.syntax_deprecation_warnings(false) do
            parse(lookup_name, raise=false)
        end

        b, found = get_value(ex, context_module)
        if found
            if isa(b, Module)
                mod = b
                lookup_module = true
            elseif Base.isstructtype(typeof(b))
                lookup_module = false
                t = typeof(b)
            end
        else # If the value is not found using get_value, the expression contain an advanced expression
            lookup_module = false
            t, found = get_type(ex, context_module)
        end
        found || return String[]
        # Ensure REPLCompletion do not crash when asked to complete a tuple, #15329
        !lookup_module && t <: Tuple && return String[]
    end

    suggestions = String[]
    if lookup_module
        # We will exclude the results that the user does not want, as well
        # as excluding Main.Main.Main, etc., because that's most likely not what
        # the user wants
        p = s->(!Base.isdeprecated(mod, s) && s != module_name(mod) && ffunc(mod, s))
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
        if t isa DataType && t != Any
            fields = fieldnames(t)
            for field in fields
                s = string(field)
                if startswith(s, name)
                    push!(suggestions, s)
                end
            end
        end
    end
    suggestions
end

const sorted_keywords = [
    "abstract type", "baremodule", "begin", "break", "catch", "ccall",
    "const", "continue", "do", "else", "elseif", "end", "export", "false",
    "finally", "for", "function", "global", "if", "import",
    "let", "local", "macro", "module", "mutable struct",
    "primitive type", "quote", "return", "struct",
    "true", "try", "using", "while"]

function complete_keyword(s::String)
    r = searchsorted(sorted_keywords, s)
    i = first(r)
    n = length(sorted_keywords)
    while i <= n && startswith(sorted_keywords[i],s)
        r = first(r):i
        i += 1
    end
    sorted_keywords[r]
end

function complete_path(path::AbstractString, pos; use_envpath=false)
    if Base.Sys.isunix() && ismatch(r"^~(?:/|$)", path)
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
        if isempty(dir)
            files = readdir()
        elseif isdir(dir)
            files = readdir(dir)
        else
            return String[], 0:-1, false
        end
    catch
        return String[], 0:-1, false
    end

    matches = Set{String}()
    for file in files
        if startswith(file, prefix)
            id = try isdir(joinpath(dir, file)) catch; false end
            # joinpath is not used because windows needs to complete with double-backslash
            push!(matches, id ? file * (@static Sys.iswindows() ? "\\\\" : "/") : file)
        end
    end

    if use_envpath && length(dir) == 0
        # Look for files in PATH as well
        local pathdirs = split(ENV["PATH"], @static Sys.iswindows() ? ";" : ":")

        for pathdir in pathdirs
            local actualpath
            try
                actualpath = realpath(pathdir)
            catch
                # Bash doesn't expect every folder in PATH to exist, so neither shall we
                continue
            end

            if actualpath != pathdir && in(actualpath,pathdirs)
                # Remove paths which (after resolving links) are in the env path twice.
                # Many distros eg. point /bin to /usr/bin but have both in the env path.
                continue
            end

            local filesinpath
            try
                filesinpath = readdir(pathdir)
            catch e
                # Bash allows dirs in PATH that can't be read, so we should as well.
                if isa(e, SystemError)
                    continue
                else
                    # We only handle SystemErrors here
                    rethrow(e)
                end
            end

            for file in filesinpath
                # In a perfect world, we would filter on whether the file is executable
                # here, or even on whether the current user can execute the file in question.
                if startswith(file, prefix) && isfile(joinpath(pathdir, file))
                    push!(matches, file)
                end
            end
        end
    end

    matchList = String[replace(s, r"\s", "\\ ") for s in matches]
    startpos = pos - endof(prefix) + 1 - length(matchall(r" ", prefix))
    # The pos - endof(prefix) + 1 is correct due to `endof(prefix)-endof(prefix)==0`,
    # hence we need to add one to get the first index. This is also correct when considering
    # pos, because pos is the `endof` a larger string which `endswith(path)==true`.
    return matchList, startpos:pos, !isempty(matchList)
end

function complete_expanduser(path::AbstractString, r)
    expanded = expanduser(path)
    return String[expanded], r, path != expanded
end

# Determines whether method_complete should be tried. It should only be done if
# the string endswiths ',' or '(' when disregarding whitespace_chars
function should_method_complete(s::AbstractString)
    method_complete = false
    for c in reverse(s)
        if c in [',', '(']
            method_complete = true
            break
        elseif !(c in whitespace_chars)
            method_complete = false
            break
        end
    end
    method_complete
end

# Returns a range that includes the method name in front of the first non
# closed start brace from the end of the string.
function find_start_brace(s::AbstractString; c_start='(', c_end=')')
    braces = 0
    r = RevString(s)
    i = start(r)
    in_single_quotes = false
    in_double_quotes = false
    in_back_ticks = false
    while !done(r, i)
        c, i = next(r, i)
        if !in_single_quotes && !in_double_quotes && !in_back_ticks
            if c == c_start
                braces += 1
            elseif c == c_end
                braces -= 1
            elseif c == '\''
                in_single_quotes = true
            elseif c == '"'
                in_double_quotes = true
            elseif c == '`'
                in_back_ticks = true
            end
        else
            if !in_back_ticks && !in_double_quotes && c == '\'' && !done(r, i) && next(r, i)[1]!='\\'
                in_single_quotes = !in_single_quotes
            elseif !in_back_ticks && !in_single_quotes && c == '"' && !done(r, i) && next(r, i)[1]!='\\'
                in_double_quotes = !in_double_quotes
            elseif !in_single_quotes && !in_double_quotes && c == '`' && !done(r, i) && next(r, i)[1]!='\\'
                in_back_ticks = !in_back_ticks
            end
        end
        braces == 1 && break
    end
    braces != 1 && return 0:-1, -1
    method_name_end = reverseind(r, i)
    startind = nextind(s, rsearch(s, non_identifier_chars, method_name_end))
    return (startind:endof(s), method_name_end)
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
    return (fn, true)
end
get_value(sym::Symbol, fn) = isdefined(fn, sym) ? (getfield(fn, sym), true) : (nothing, false)
get_value(sym::QuoteNode, fn) = isdefined(fn, sym.value) ? (getfield(fn, sym.value), true) : (nothing, false)
get_value(sym, fn) = (sym, true)

# Return the value of a getfield call expression
function get_value_getfield(ex::Expr, fn)
    # Example :((top(getfield))(Base,:max))
    val, found = get_value_getfield(ex.args[2],fn) #Look up Base in Main and returns the module
    found || return (nothing, false)
    return get_value_getfield(ex.args[3], val) #Look up max in Base and returns the function if found.
end
get_value_getfield(sym, fn) = get_value(sym, fn)

# Determines the return type with Base.return_types of a function call using the type information of the arguments.
function get_type_call(expr::Expr)
    f_name = expr.args[1]
    # The if statement should find the f function. How f is found depends on how f is referenced
    if isa(f_name, GlobalRef) && isconst(f_name.mod,f_name.name) && isdefined(f_name.mod,f_name.name)
        ft = typeof(eval(f_name))
        found = true
    else
        ft, found = get_type(f_name, Main)
    end
    found || return (Any, false) # If the function f is not found return Any.
    args = Any[]
    for ex in expr.args[2:end] # Find the type of the function arguments
        typ, found = get_type(ex, Main)
        found ? push!(args, typ) : push!(args, Any)
    end
    # use _methods_by_ftype as the function is supplied as a type
    world = ccall(:jl_get_world_counter, UInt, ())
    mt = Base._methods_by_ftype(Tuple{ft, args...}, -1, world)
    length(mt) == 1 || return (Any, false)
    m = first(mt)
    # Typeinference
    params = Core.Inference.InferenceParams(world)
    return_type = Core.Inference.typeinf_type(m[3], m[1], m[2], true, params)
    return_type === nothing && return (Any, false)
    return (return_type, true)
end

# Returns the return type. example: get_type(:(Base.strip("", ' ')), Main) returns (String, true)
function try_get_type(sym::Expr, fn::Module)
    val, found = get_value(sym, fn)
    found && return Base.typesof(val).parameters[1], found
    if sym.head === :call
        # getfield call is special cased as the evaluation of getfield provides good type information,
        # is inexpensive and it is also performed in the complete_symbol function.
        a1 = sym.args[1]
        if isa(a1,GlobalRef) && isconst(a1.mod,a1.name) && isdefined(a1.mod,a1.name) &&
            eval(a1) === Core.getfield
            val, found = get_value_getfield(sym, Main)
            return found ? Base.typesof(val).parameters[1] : Any, found
        end
        return get_type_call(sym)
    elseif sym.head === :thunk
        thk = sym.args[1]
        rt = ccall(:jl_infer_thunk, Any, (Any, Any), thk::CodeInfo, fn)
        rt !== Any && return (rt, true)
    elseif sym.head === :ref
        # some simple cases of `expand`
        return try_get_type(Expr(:call, GlobalRef(Base, :getindex), sym.args...), fn)
    elseif sym.head === :.
        return try_get_type(Expr(:call, GlobalRef(Core, :getfield), sym.args...), fn)
    end
    return (Any, false)
end

try_get_type(other, fn::Module) = get_type(other, fn)

function get_type(sym::Expr, fn::Module)
    # try to analyze nests of calls. if this fails, try using the expanded form.
    val, found = try_get_type(sym, fn)
    found && return val, found
    return try_get_type(expand(fn, sym), fn)
end

function get_type(sym, fn::Module)
    val, found = get_value(sym, fn)
    return found ? Base.typesof(val).parameters[1] : Any, found
end

# Method completion on function call expression that look like :(max(1))
function complete_methods(ex_org::Expr)
    args_ex = Any[]
    func, found = get_value(ex_org.args[1], Main)
    !found && return String[]
    for ex in ex_org.args[2:end]
        val, found = get_type(ex, Main)
        push!(args_ex, val)
    end
    out = String[]
    t_in = Tuple{Core.Typeof(func), args_ex...} # Input types
    na = length(args_ex)+1
    ml = methods(func)
    kwtype = isdefined(ml.mt, :kwsorter) ? Nullable{DataType}(typeof(ml.mt.kwsorter)) : Nullable{DataType}()
    io = IOBuffer()
    for method in ml
        ms = method.sig

        # Do not suggest the default method from sysimg.jl.
        if Base.is_default_method(method)
            continue
        end

        # Check if the method's type signature intersects the input types
        if typeintersect(Base.rewrap_unionall(Tuple{Base.unwrap_unionall(ms).parameters[1 : min(na, end)]...}, ms), t_in) != Union{}
            show(io, method, kwtype=kwtype)
            push!(out, String(take!(io)))
        end
    end
    return out
end

include("latex_symbols.jl")
include("emoji_symbols.jl")

const non_identifier_chars = [" \t\n\r\"\\'`\$><=:;|&{}()[],+-*/?%^~"...]
const whitespace_chars = [" \t\n\r"...]
# "\"'`"... is added to whitespace_chars as non of the bslash_completions
# characters contain any of these characters. It prohibits the
# bslash_completions function to try and complete on escaped characters in strings
const bslash_separators = [whitespace_chars..., "\"'`"...]

# Aux function to detect whether we're right after a
# using or import keyword
function afterusing(string::String, startpos::Int)
    (isempty(string) || startpos == 0) && return false
    str = string[1:prevind(string,startpos)]
    isempty(str) && return false
    rstr = reverse(str)
    r = search(rstr, r"\s(gnisu|tropmi)\b")
    isempty(r) && return false
    fr = reverseind(str, last(r))
    return ismatch(r"^\b(using|import)\s*((\w+[.])*\w+\s*,\s*)*$", str[fr:end])
end

function bslash_completions(string, pos)
    slashpos = rsearch(string, '\\', pos)
    if (rsearch(string, bslash_separators, pos) < slashpos &&
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
            emoji_names = Iterators.filter(k -> startswith(k, s), keys(emoji_symbols))
            return (true, (sort!(collect(emoji_names)), slashpos:pos, true))
        else # latex
            latex_names = Iterators.filter(k -> startswith(k, s), keys(latex_symbols))
            return (true, (sort!(collect(latex_names)), slashpos:pos, true))
        end
    end
    return (false, (String[], 0:-1, false))
end

function dict_identifier_key(str,tag)
    if tag === :string
        str_close = str*"\""
    elseif tag === :cmd
        str_close = str*"`"
    else
        str_close = str
    end

    frange, end_of_identifier = find_start_brace(str_close, c_start='[', c_end=']')
    isempty(frange) && return (nothing, nothing, nothing)
    obj = Main
    for name in split(str[frange[1]:end_of_identifier], '.')
        Base.isidentifier(name) || return (nothing, nothing, nothing)
        sym = Symbol(name)
        isdefined(obj, sym) || return (nothing, nothing, nothing)
        obj = getfield(obj, sym)
        # Avoid `isdefined(::Array, ::Symbol)`
        isa(obj, Array) && return (nothing, nothing, nothing)
    end
    begin_of_key = first(search(str, r"\S", nextind(str, end_of_identifier) + 1)) # 1 for [
    begin_of_key==0 && return (true, nothing, nothing)
    partial_key = str[begin_of_key:end]
    (isa(obj, Associative) && length(obj) < 1e6) || return (true, nothing, nothing)
    return (obj, partial_key, begin_of_key)
end

# This needs to be a separate non-inlined function, see #19441
@noinline function find_dict_matches(identifier, partial_key)
    matches = []
    for key in keys(identifier)
        rkey = repr(key)
        startswith(rkey,partial_key) && push!(matches,rkey)
    end
    return matches
end

function completions(string, pos)
    # First parse everything up to the current position
    partial = string[1:pos]
    inc_tag = Base.syntax_deprecation_warnings(false) do
        Base.incomplete_tag(parse(partial, raise=false))
    end

    # if completing a key in a Dict
    identifier, partial_key, loc = dict_identifier_key(partial,inc_tag)
    if identifier !== nothing
        if partial_key !== nothing
            matches = find_dict_matches(identifier, partial_key)
            length(matches)==1 && (length(string) <= pos || string[pos+1] != ']') && (matches[1]*="]")
            length(matches)>0 && return sort!(matches), loc:pos, true
        else
            return String[], 0:-1, false
        end
    end

    # otherwise...
    if inc_tag in [:cmd, :string]
        m = match(r"[\t\n\r\"><=*?|]| (?!\\)", reverse(partial))
        startpos = nextind(partial, reverseind(partial, m.offset))
        r = startpos:pos

        expanded = complete_expanduser(replace(string[r], r"\\ ", " "), r)
        expanded[3] && return expanded  # If user expansion available, return it

        paths, r, success = complete_path(replace(string[r], r"\\ ", " "), pos)

        if inc_tag == :string &&
           length(paths) == 1 &&  # Only close if there's a single choice,
           !isdir(expanduser(replace(string[startpos:prevind(string, start(r))] * paths[1],
                                     r"\\ ", " "))) &&  # except if it's a directory
           (length(string) <= pos ||
            string[nextind(string,pos)] != '"')  # or there's already a " at the cursor.
            paths[1] *= "\""
        end

        #Latex symbols can be completed for strings
        (success || inc_tag==:cmd) && return sort!(paths), r, success
    end

    ok, ret = bslash_completions(string, pos)
    ok && return ret

    # Make sure that only bslash_completions is working on strings
    inc_tag==:string && return String[], 0:-1, false

    if inc_tag == :other && should_method_complete(partial)
        frange, method_name_end = find_start_brace(partial)
        ex = Base.syntax_deprecation_warnings(false) do
            parse(partial[frange] * ")", raise=false)
        end
        if isa(ex, Expr) && ex.head==:call
            return complete_methods(ex), start(frange):method_name_end, false
        end
    elseif inc_tag == :comment
        return String[], 0:-1, false
    end

    dotpos = rsearch(string, '.', pos)
    startpos = nextind(string, rsearch(string, non_identifier_chars, pos))

    ffunc = (mod,x)->true
    suggestions = String[]
    comp_keywords = true
    if afterusing(string, startpos)
        # We're right after using or import. Let's look only for packages
        # and modules we can reach from here

        # If there's no dot, we're in toplevel, so we should
        # also search for packages
        s = string[startpos:pos]
        if dotpos <= startpos
            for dir in [Pkg.dir(); LOAD_PATH; pwd()]
                dir isa AbstractString && isdir(dir) || continue
                for pname in readdir(dir)
                    if pname[1] != '.' && pname != "METADATA" &&
                        pname != "REQUIRE" && startswith(pname, s)
                        # Valid file paths are
                        #   <Mod>.jl
                        #   <Mod>/src/<Mod>.jl
                        #   <Mod>.jl/src/<Mod>.jl
                        if isfile(joinpath(dir, pname))
                            endswith(pname, ".jl") && push!(suggestions,
                                                            pname[1:prevind(pname, end-2)])
                        else
                            mod_name = if endswith(pname, ".jl")
                                pname[1:prevind(pname, end-2)]
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
        ffunc = (mod,x)->(isdefined(mod, x) && isa(getfield(mod, x), Module))
        comp_keywords = false
    end
    startpos == 0 && (pos = -1)
    dotpos < startpos && (dotpos = startpos - 1)
    s = string[startpos:pos]
    comp_keywords && append!(suggestions, complete_keyword(s))
    # The case where dot and start pos is equal could look like: "(""*"").d","". or  CompletionFoo.test_y_array[1].y
    # This case can be handled by finding the begining of the expresion. This is done bellow.
    if dotpos == startpos
        i = prevind(string, startpos)
        while 0 < i
            c = string[i]
            if c in [')', ']']
                if c==')'
                    c_start='('; c_end=')'
                elseif c==']'
                    c_start='['; c_end=']'
                end
                frange, end_of_identifier = find_start_brace(string[1:prevind(string, i)], c_start=c_start, c_end=c_end)
                startpos = start(frange)
                i = prevind(string, startpos)
            elseif c in ["\'\"\`"...]
                s = "$c$c"*string[startpos:pos]
                break
            else
                break
            end
            s = string[startpos:pos]
        end
    end
    append!(suggestions, complete_symbol(s, ffunc))
    return sort!(unique(suggestions)), (dotpos+1):pos, true
end

function shell_completions(string, pos)
    # First parse everything up to the current position
    scs = string[1:pos]
    local args, last_parse
    try
        args, last_parse = Base.shell_parse(scs, true)
    catch
        return String[], 0:-1, false
    end
    # Now look at the last thing we parsed
    isempty(args.args[end].args) && return String[], 0:-1, false
    arg = args.args[end].args[end]
    if all(s -> isa(s, AbstractString), args.args[end].args)
        # Treat this as a path

        # As Base.shell_parse throws away trailing spaces (unless they are escaped),
        # we need to special case here.
        # If the last char was a space, but shell_parse ignored it search on "".
        ignore_last_word = arg != " " && scs[end] == ' '
        prefix = ignore_last_word ? "" : join(args.args[end].args)

        # Also try looking into the env path if the user wants to complete the first argument
        use_envpath = !ignore_last_word && length(args.args) < 2

        return complete_path(prefix, pos, use_envpath=use_envpath)
    elseif isexpr(arg, :incomplete) || isexpr(arg, :error)
        r = first(last_parse):prevind(last_parse, last(last_parse))
        partial = scs[r]
        ret, range = completions(partial, endof(partial))
        range = range .+ (first(r) - 1)
        return ret, range, true
    end
    return String[], 0:-1, false
end

end # module
