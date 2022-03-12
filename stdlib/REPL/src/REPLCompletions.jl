# This file is a part of Julia. License is MIT: https://julialang.org/license

module REPLCompletions

export completions, shell_completions, bslash_completions, completion_text

using Base.Meta
using Base: propertynames, something

abstract type Completion end

struct TextCompletion <: Completion
    text::String
end

struct KeywordCompletion <: Completion
    keyword::String
end

struct PathCompletion <: Completion
    path::String
end

struct ModuleCompletion <: Completion
    parent::Module
    mod::String
end

struct PackageCompletion <: Completion
    package::String
end

struct PropertyCompletion <: Completion
    value
    property::Symbol
end

struct FieldCompletion <: Completion
    typ::DataType
    field::Symbol
end

struct MethodCompletion <: Completion
    tt # may be used by an external consumer to infer return type, etc.
    method::Method
    MethodCompletion(@nospecialize(tt), method::Method) = new(tt, method)
end

struct BslashCompletion <: Completion
    bslash::String
end

struct ShellCompletion <: Completion
    text::String
end

struct DictCompletion <: Completion
    dict::AbstractDict
    key::String
end

# interface definition
function Base.getproperty(c::Completion, name::Symbol)
    if name === :text
        return getfield(c, :text)::String
    elseif name === :keyword
        return getfield(c, :keyword)::String
    elseif name === :path
        return getfield(c, :path)::String
    elseif name === :parent
        return getfield(c, :parent)::Module
    elseif name === :mod
        return getfield(c, :mod)::String
    elseif name === :package
        return getfield(c, :package)::String
    elseif name === :property
        return getfield(c, :property)::Symbol
    elseif name === :field
        return getfield(c, :field)::Symbol
    elseif name === :method
        return getfield(c, :method)::Method
    elseif name === :bslash
        return getfield(c, :bslash)::String
    elseif name === :text
        return getfield(c, :text)::String
    elseif name === :key
        return getfield(c, :key)::String
    end
    return getfield(c, name)
end

_completion_text(c::TextCompletion) = c.text
_completion_text(c::KeywordCompletion) = c.keyword
_completion_text(c::PathCompletion) = c.path
_completion_text(c::ModuleCompletion) = c.mod
_completion_text(c::PackageCompletion) = c.package
_completion_text(c::PropertyCompletion) = string(c.property)
_completion_text(c::FieldCompletion) = string(c.field)
_completion_text(c::MethodCompletion) = repr(c.method)
_completion_text(c::BslashCompletion) = c.bslash
_completion_text(c::ShellCompletion) = c.text
_completion_text(c::DictCompletion) = c.key

completion_text(c) = _completion_text(c)::String

const Completions = Tuple{Vector{Completion}, UnitRange{Int}, Bool}

function completes_global(x, name)
    return startswith(x, name) && !('#' in x)
end

function appendmacro!(syms, macros, needle, endchar)
    for s in macros
        if endswith(s, needle)
            from = nextind(s, firstindex(s))
            to = prevind(s, sizeof(s)-sizeof(needle)+1)
            push!(syms, s[from:to]*endchar)
        end
    end
end

function filtered_mod_names(ffunc::Function, mod::Module, name::AbstractString, all::Bool = false, imported::Bool = false)
    ssyms = names(mod, all = all, imported = imported)
    filter!(ffunc, ssyms)
    syms = String[string(s) for s in ssyms]
    macros =  filter(x -> startswith(x, "@" * name), syms)
    appendmacro!(syms, macros, "_str", "\"")
    appendmacro!(syms, macros, "_cmd", "`")
    filter!(x->completes_global(x, name), syms)
    return [ModuleCompletion(mod, sym) for sym in syms]
end

# REPL Symbol Completions
function complete_symbol(sym::String, @nospecialize(ffunc), context_module::Module=Main)
    mod = context_module
    name = sym

    lookup_module = true
    t = Union{}
    val = nothing
    if something(findlast(in(non_identifier_chars), sym), 0) < something(findlast(isequal('.'), sym), 0)
        # Find module
        lookup_name, name = rsplit(sym, ".", limit=2)

        ex = Meta.parse(lookup_name, raise=false, depwarn=false)

        b, found = get_value(ex, context_module)
        if found
            val = b
            if isa(b, Module)
                mod = b
                lookup_module = true
            else
                lookup_module = false
                t = typeof(b)
            end
        else # If the value is not found using get_value, the expression contain an advanced expression
            lookup_module = false
            t, found = get_type(ex, context_module)
        end
        found || return Completion[]
    end

    suggestions = Completion[]
    if lookup_module
        # We will exclude the results that the user does not want, as well
        # as excluding Main.Main.Main, etc., because that's most likely not what
        # the user wants
        p = let mod=mod, modname=nameof(mod)
            s->(!Base.isdeprecated(mod, s) && s != modname && ffunc(mod, s)::Bool)
        end
        # Looking for a binding in a module
        if mod == context_module
            # Also look in modules we got through `using`
            mods = ccall(:jl_module_usings, Any, (Any,), context_module)::Vector
            for m in mods
                append!(suggestions, filtered_mod_names(p, m::Module, name))
            end
            append!(suggestions, filtered_mod_names(p, mod, name, true, true))
        else
            append!(suggestions, filtered_mod_names(p, mod, name, true, false))
        end
    elseif val !== nothing # looking for a property of an instance
        for property in propertynames(val, false)
            # TODO: support integer arguments (#36872)
            if property isa Symbol && startswith(string(property), name)
                push!(suggestions, PropertyCompletion(val, property))
            end
        end
    else
        # Looking for a member of a type
        if t isa DataType && t != Any
            # Check for cases like Type{typeof(+)}
            if t isa DataType && t.name === Base._TYPE_NAME
                t = typeof(t.parameters[1])
            end
            # Only look for fields if this is a concrete type
            if isconcretetype(t)
                fields = fieldnames(t)
                for field in fields
                    s = string(field)
                    if startswith(s, name)
                        push!(suggestions, FieldCompletion(t, field))
                    end
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

function complete_keyword(s::Union{String,SubString{String}})
    r = searchsorted(sorted_keywords, s)
    i = first(r)
    n = length(sorted_keywords)
    while i <= n && startswith(sorted_keywords[i],s)
        r = first(r):i
        i += 1
    end
    Completion[KeywordCompletion(kw) for kw in sorted_keywords[r]]
end

function complete_path(path::AbstractString, pos::Int; use_envpath=false, shell_escape=false)
    if Base.Sys.isunix() && occursin(r"^~(?:/|$)", path)
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
            return Completion[], 0:-1, false
        end
    catch
        return Completion[], 0:-1, false
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
                if isa(e, Base.IOError) || isa(e, Base.ArgumentError)
                    continue
                else
                    # We only handle IOError and ArgumentError here
                    rethrow()
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

    matchList = Completion[PathCompletion(shell_escape ? replace(s, r"\s" => s"\\\0") : s) for s in matches]
    startpos = pos - lastindex(prefix) + 1 - count(isequal(' '), prefix)
    # The pos - lastindex(prefix) + 1 is correct due to `lastindex(prefix)-lastindex(prefix)==0`,
    # hence we need to add one to get the first index. This is also correct when considering
    # pos, because pos is the `lastindex` a larger string which `endswith(path)==true`.
    return matchList, startpos:pos, !isempty(matchList)
end

function complete_expanduser(path::AbstractString, r)
    expanded = expanduser(path)
    return Completion[PathCompletion(expanded)], r, path != expanded
end

# Determines whether method_complete should be tried. It should only be done if
# the string endswiths ',' or '(' when disregarding whitespace_chars
function should_method_complete(s::AbstractString)
    method_complete = false
    for c in reverse(s)
        if c in [',', '(', ';']
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
    r = reverse(s)
    i = firstindex(r)
    in_single_quotes = false
    in_double_quotes = false
    in_back_ticks = false
    in_comment = 0
    while i <= ncodeunits(r)
        c, i = iterate(r, i)
        if c == '#' && i <= ncodeunits(r) && iterate(r, i)[1] == '='
            c, i = iterate(r, i) # consume '='
            new_comments = 1
            # handle #=#=#=#, by counting =# pairs
            while i <= ncodeunits(r) && iterate(r, i)[1] == '#'
                c, i = iterate(r, i) # consume '#'
                iterate(r, i)[1] == '=' || break
                c, i = iterate(r, i) # consume '='
                new_comments += 1
            end
            if c == '='
                in_comment += new_comments
            else
                in_comment -= new_comments
            end
        elseif !in_single_quotes && !in_double_quotes && !in_back_ticks && in_comment == 0
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
            if in_single_quotes &&
                c == '\'' && i <= ncodeunits(r) && iterate(r, i)[1] != '\\'
                in_single_quotes = false
            elseif in_double_quotes &&
                c == '"' && i <= ncodeunits(r) && iterate(r, i)[1] != '\\'
                in_double_quotes = false
            elseif in_back_ticks &&
                c == '`' && i <= ncodeunits(r) && iterate(r, i)[1] != '\\'
                in_back_ticks = false
            elseif in_comment > 0 &&
                c == '=' && i <= ncodeunits(r) && iterate(r, i)[1] == '#'
                # handle =#=#=#=, by counting #= pairs
                c, i = iterate(r, i) # consume '#'
                old_comments = 1
                while i <= ncodeunits(r) && iterate(r, i)[1] == '='
                    c, i = iterate(r, i) # consume '='
                    iterate(r, i)[1] == '#' || break
                    c, i = iterate(r, i) # consume '#'
                    old_comments += 1
                end
                if c == '#'
                    in_comment -= old_comments
                else
                    in_comment += old_comments
                end
            end
        end
        braces == 1 && break
    end
    braces != 1 && return 0:-1, -1
    method_name_end = reverseind(s, i)
    startind = nextind(s, something(findprev(in(non_identifier_chars), s, method_name_end), 0))::Int
    return (startind:lastindex(s), method_name_end)
end

# Returns the value in a expression if sym is defined in current namespace fn.
# This method is used to iterate to the value of a expression like:
# :(REPL.REPLCompletions.whitespace_chars) a `dump` of this expression
# will show it consist of Expr, QuoteNode's and Symbol's which all needs to
# be handled differently to iterate down to get the value of whitespace_chars.
function get_value(sym::Expr, fn)
    if sym.head === :quote || sym.head === :inert
        return sym.args[1], true
    end
    sym.head !== :. && return (nothing, false)
    for ex in sym.args
        ex, found = get_value(ex, fn)
        !found && return (nothing, false)
        fn, found = get_value(ex, fn)
        !found && return (nothing, false)
    end
    return (fn, true)
end
get_value(sym::Symbol, fn) = isdefined(fn, sym) ? (getfield(fn, sym), true) : (nothing, false)
get_value(sym::QuoteNode, fn) = (sym.value, true)
get_value(sym::GlobalRef, fn) = get_value(sym.name, sym.mod)
get_value(sym, fn) = (sym, true)

# Return the type of a getfield call expression
function get_type_getfield(ex::Expr, fn::Module)
    length(ex.args) == 3 || return Any, false # should never happen, but just for safety
    fld, found = get_value(ex.args[3], fn)
    fld isa Symbol || return Any, false
    obj = ex.args[2]
    objt, found = get_type(obj, fn)
    found || return Any, false
    objt isa DataType || return Any, false
    hasfield(objt, fld) || return Any, false
    return fieldtype(objt, fld), true
end

# Determines the return type with the Compiler of a function call using the type information of the arguments.
function get_type_call(expr::Expr, fn::Module)
    f_name = expr.args[1]
    f, found = get_type(f_name, fn)
    found || return (Any, false) # If the function f is not found return Any.
    args = Any[]
    for i in 2:length(expr.args) # Find the type of the function arguments
        typ, found = get_type(expr.args[i], fn)
        found ? push!(args, typ) : push!(args, Any)
    end
    world = Base.get_world_counter()
    return_type = Core.Compiler.return_type(Tuple{f, args...}, world)
    return (return_type, true)
end

# Returns the return type. example: get_type(:(Base.strip("", ' ')), Main) returns (SubString{String}, true)
function try_get_type(sym::Expr, fn::Module)
    val, found = get_value(sym, fn)
    found && return Core.Typeof(val), found
    if sym.head === :call
        # getfield call is special cased as the evaluation of getfield provides good type information,
        # is inexpensive and it is also performed in the complete_symbol function.
        a1 = sym.args[1]
        if a1 === :getfield || a1 === GlobalRef(Core, :getfield)
            return get_type_getfield(sym, fn)
        end
        return get_type_call(sym, fn)
    elseif sym.head === :thunk
        thk = sym.args[1]
        rt = ccall(:jl_infer_thunk, Any, (Any, Any), thk::Core.CodeInfo, fn)
        rt !== Any && return (rt, true)
    elseif sym.head === :ref
        # some simple cases of `expand`
        return try_get_type(Expr(:call, GlobalRef(Base, :getindex), sym.args...), fn)
    elseif sym.head === :. && sym.args[2] isa QuoteNode # second check catches broadcasting
        return try_get_type(Expr(:call, GlobalRef(Core, :getfield), sym.args...), fn)
    end
    return (Any, false)
end

try_get_type(other, fn::Module) = get_type(other, fn)

function get_type(sym::Expr, fn::Module)
    # try to analyze nests of calls. if this fails, try using the expanded form.
    val, found = try_get_type(sym, fn)
    found && return val, found
    # https://github.com/JuliaLang/julia/issues/27184
    if isexpr(sym, :macrocall)
        _, found = get_type(first(sym.args), fn)
        found || return Any, false
    end
    newsym = try
        Meta.lower(fn, sym)
    catch e
        e isa LoadError && return Any, false
        # If e is not a LoadError then Meta.lower crashed in an unexpected way.
        # Since this is not a specific to the user code but an internal error,
        # rethrow the error to allow reporting it.
        rethrow()
    end
    return try_get_type(newsym, fn)
end

function get_type(sym, fn::Module)
    val, found = get_value(sym, fn)
    return found ? Core.Typeof(val) : Any, found
end

function get_type(T, found::Bool, default_any::Bool)
    return found ? T :
           default_any ? Any : throw(ArgumentError("argument not found"))
end

# Method completion on function call expression that look like :(max(1))
MAX_METHOD_COMPLETIONS::Int = 40
function complete_methods(ex_org::Expr, context_module::Module=Main)
    out = Completion[]
    funct, found = get_type(ex_org.args[1], context_module)::Tuple{Any,Bool}
    !found && return out

    args_ex, kwargs_ex = complete_methods_args(ex_org.args[2:end], ex_org, context_module, true, true)
    push!(args_ex, Vararg{Any})
    complete_methods!(out, funct, args_ex, kwargs_ex, MAX_METHOD_COMPLETIONS)

    return out
end

MAX_ANY_METHOD_COMPLETIONS::Int = 10
function complete_any_methods(ex_org::Expr, callee_module::Module, context_module::Module, moreargs::Bool, shift::Bool)
    out = Completion[]
    args_ex, kwargs_ex = try
        # this may throw, since we set default_any to false
        complete_methods_args(ex_org.args[2:end], ex_org, context_module, false, false)
    catch ex
        ex isa ArgumentError || rethrow()
        return out
    end
    moreargs && push!(args_ex, Vararg{Any})

    seen = Base.IdSet()
    for name in names(callee_module; all=true)
        if !Base.isdeprecated(callee_module, name) && isdefined(callee_module, name)
            func = getfield(callee_module, name)
            if !isa(func, Module)
                funct = Core.Typeof(func)
                if !in(funct, seen)
                    push!(seen, funct)
                    complete_methods!(out, funct, args_ex, kwargs_ex, MAX_ANY_METHOD_COMPLETIONS)
                end
            elseif callee_module === Main && isa(func, Module)
                callee_module2 = func
                for name in names(callee_module2)
                    if !Base.isdeprecated(callee_module2, name) && isdefined(callee_module2, name)
                        func = getfield(callee_module, name)
                        if !isa(func, Module)
                            funct = Core.Typeof(func)
                            if !in(funct, seen)
                                push!(seen, funct)
                                complete_methods!(out, funct, args_ex, kwargs_ex, MAX_ANY_METHOD_COMPLETIONS)
                            end
                        end
                    end
                end
            end
        end
    end

    if !shift
        # Filter out methods where all arguments are `Any`
        filter!(out) do c
            isa(c, TextCompletion) && return false
            isa(c, MethodCompletion) || return true
            sig = Base.unwrap_unionall(c.method.sig)::DataType
            return !all(T -> T === Any || T === Vararg{Any}, sig.parameters[2:end])
        end
    end

    return out
end

function complete_methods_args(funargs::Vector{Any}, ex_org::Expr, context_module::Module, default_any::Bool, allow_broadcasting::Bool)
    args_ex = Any[]
    kwargs_ex = false
    if allow_broadcasting && ex_org.head === :. && ex_org.args[2] isa Expr
        # handle broadcasting, but only handle number of arguments instead of
        # argument types
        for _ in (ex_org.args[2]::Expr).args
            push!(args_ex, Any)
        end
    else
        for ex in funargs
            if isexpr(ex, :parameters)
                if !isempty(ex.args)
                    kwargs_ex = true
                end
            elseif isexpr(ex, :kw)
                kwargs_ex = true
            else
                push!(args_ex, get_type(get_type(ex, context_module)..., default_any))
            end
        end
    end
    return args_ex, kwargs_ex
end

function complete_methods!(out::Vector{Completion}, @nospecialize(funct), args_ex::Vector{Any}, kwargs_ex::Bool, max_method_completions::Int)
    # Input types and number of arguments
    t_in = Tuple{funct, args_ex...}
    m = Base._methods_by_ftype(t_in, nothing, max_method_completions, Base.get_world_counter(),
        #=ambig=# true, Ref(typemin(UInt)), Ref(typemax(UInt)), Ptr{Int32}(C_NULL))
    if m === false
        push!(out, TextCompletion(sprint(Base.show_signature_function, funct) * "( too many methods to show )"))
    end
    m isa Vector || return
    for match in m
        # TODO: if kwargs_ex, filter out methods without kwargs?
        push!(out, MethodCompletion(match.spec_types, match.method))
    end
end

include("latex_symbols.jl")
include("emoji_symbols.jl")

const non_identifier_chars = [" \t\n\r\"\\'`\$><=:;|&{}()[],+-*/?%^~"...]
const whitespace_chars = [" \t\n\r"...]
# "\"'`"... is added to whitespace_chars as non of the bslash_completions
# characters contain any of these characters. It prohibits the
# bslash_completions function to try and complete on escaped characters in strings
const bslash_separators = [whitespace_chars..., "\"'`"...]

const subscripts = Dict(k[3]=>v[1] for (k,v) in latex_symbols if startswith(k, "\\_") && length(k)==3)
const subscript_regex = Regex("^\\\\_[" * join(isdigit(k) || isletter(k) ? "$k" : "\\$k" for k in keys(subscripts)) * "]+\\z")
const superscripts = Dict(k[3]=>v[1] for (k,v) in latex_symbols if startswith(k, "\\^") && length(k)==3)
const superscript_regex = Regex("^\\\\\\^[" * join(isdigit(k) || isletter(k) ? "$k" : "\\$k" for k in keys(superscripts)) * "]+\\z")

# Aux function to detect whether we're right after a
# using or import keyword
function afterusing(string::String, startpos::Int)
    (isempty(string) || startpos == 0) && return false
    str = string[1:prevind(string,startpos)]
    isempty(str) && return false
    rstr = reverse(str)
    r = findfirst(r"\s(gnisu|tropmi)\b", rstr)
    r === nothing && return false
    fr = reverseind(str, last(r))
    return occursin(r"^\b(using|import)\s*((\w+[.])*\w+\s*,\s*)*$", str[fr:end])
end

function close_path_completion(str, startpos, r, paths, pos)
    length(paths) == 1 || return false  # Only close if there's a single choice...
    _path = str[startpos:prevind(str, first(r))] * (paths[1]::PathCompletion).path
    path = expanduser(replace(_path, r"\\ " => " "))
    # ...except if it's a directory...
    try
        isdir(path)
    catch e
        e isa Base.IOError || rethrow() # `path` cannot be determined to be a file
    end && return false
    # ...and except if there's already a " at the cursor.
    return lastindex(str) <= pos || str[nextind(str, pos)] != '"'
end


function bslash_completions(string::String, pos::Int)
    slashpos = something(findprev(isequal('\\'), string, pos), 0)
    if (something(findprev(in(bslash_separators), string, pos), 0) < slashpos &&
        !(1 < slashpos && (string[prevind(string, slashpos)]=='\\')))
        # latex / emoji symbol substitution
        s = string[slashpos:pos]
        latex = get(latex_symbols, s, "")
        if !isempty(latex) # complete an exact match
            return (true, (Completion[BslashCompletion(latex)], slashpos:pos, true))
        elseif occursin(subscript_regex, s)
            sub = map(c -> subscripts[c], s[3:end])
            return (true, (Completion[BslashCompletion(sub)], slashpos:pos, true))
        elseif occursin(superscript_regex, s)
            sup = map(c -> superscripts[c], s[3:end])
            return (true, (Completion[BslashCompletion(sup)], slashpos:pos, true))
        end
        emoji = get(emoji_symbols, s, "")
        if !isempty(emoji)
            return (true, (Completion[BslashCompletion(emoji)], slashpos:pos, true))
        end
        # return possible matches; these cannot be mixed with regular
        # Julian completions as only latex / emoji symbols contain the leading \
        if startswith(s, "\\:") # emoji
            namelist = Iterators.filter(k -> startswith(k, s), keys(emoji_symbols))
        else # latex
            namelist = Iterators.filter(k -> startswith(k, s), keys(latex_symbols))
        end
        return (true, (Completion[BslashCompletion(name) for name in sort!(collect(namelist))], slashpos:pos, true))
    end
    return (false, (Completion[], 0:-1, false))
end

function dict_identifier_key(str::String, tag::Symbol, context_module::Module=Main)
    if tag === :string
        str_close = str*"\""
    elseif tag === :cmd
        str_close = str*"`"
    else
        str_close = str
    end

    frange, end_of_identifier = find_start_brace(str_close, c_start='[', c_end=']')
    isempty(frange) && return (nothing, nothing, nothing)
    obj = context_module
    for name in split(str[frange[1]:end_of_identifier], '.')
        Base.isidentifier(name) || return (nothing, nothing, nothing)
        sym = Symbol(name)
        isdefined(obj, sym) || return (nothing, nothing, nothing)
        obj = getfield(obj, sym)
    end
    (isa(obj, AbstractDict) && length(obj)::Int < 1_000_000) || return (nothing, nothing, nothing)
    begin_of_key = something(findnext(!isspace, str, nextind(str, end_of_identifier) + 1), # +1 for [
                             lastindex(str)+1)
    return (obj::AbstractDict, str[begin_of_key:end], begin_of_key)
end

# This needs to be a separate non-inlined function, see #19441
@noinline function find_dict_matches(identifier::AbstractDict, partial_key)
    matches = String[]
    for key in keys(identifier)
        rkey = repr(key)
        startswith(rkey,partial_key) && push!(matches,rkey)
    end
    return matches
end

function project_deps_get_completion_candidates(pkgstarts::String, project_file::String)
    loading_candidates = String[]
    d = Base.parsed_toml(project_file)
    pkg = get(d, "name", nothing)::Union{String, Nothing}
    if pkg !== nothing && startswith(pkg, pkgstarts)
        push!(loading_candidates, pkg)
    end
    deps = get(d, "deps", nothing)::Union{Dict{String, Any}, Nothing}
    if deps !== nothing
        for (pkg, _) in deps
            startswith(pkg, pkgstarts) && push!(loading_candidates, pkg)
        end
    end
    return Completion[PackageCompletion(name) for name in loading_candidates]
end

function completions(string::String, pos::Int, context_module::Module=Main, shift::Bool=true)
    # First parse everything up to the current position
    partial = string[1:pos]
    inc_tag = Base.incomplete_tag(Meta.parse(partial, raise=false, depwarn=false))

    # ?(x, y)TAB lists methods you can call with these objects
    # ?(x, y TAB lists methods that take these objects as the first two arguments
    # MyModule.?(x, y)TAB restricts the search to names in MyModule
    rexm = match(r"(\w+\.|)\?\((.*)$", partial)
    if rexm !== nothing
        # Get the module scope
        if isempty(rexm.captures[1])
            callee_module = context_module
        else
            modname = Symbol(rexm.captures[1][1:end-1])
            if isdefined(context_module, modname)
                callee_module = getfield(context_module, modname)
                if !isa(callee_module, Module)
                    callee_module = context_module
                end
            else
                callee_module = context_module
            end
        end
        moreargs = !endswith(rexm.captures[2], ')')
        callstr = "_(" * rexm.captures[2]
        if moreargs
            callstr *= ')'
        end
        ex_org = Meta.parse(callstr, raise=false, depwarn=false)
        if isa(ex_org, Expr)
            return complete_any_methods(ex_org, callee_module::Module, context_module, moreargs, shift), (0:length(rexm.captures[1])+1) .+ rexm.offset, false
        end
    end

    # if completing a key in a Dict
    identifier, partial_key, loc = dict_identifier_key(partial, inc_tag, context_module)
    if identifier !== nothing
        matches = find_dict_matches(identifier, partial_key)
        length(matches)==1 && (lastindex(string) <= pos || string[nextind(string,pos)] != ']') && (matches[1]*=']')
        length(matches)>0 && return Completion[DictCompletion(identifier, match) for match in sort!(matches)], loc::Int:pos, true
    end

    # otherwise...
    if inc_tag in [:cmd, :string]
        m = match(r"[\t\n\r\"`><=*?|]| (?!\\)", reverse(partial))
        startpos = nextind(partial, reverseind(partial, m.offset))
        r = startpos:pos

        expanded = complete_expanduser(replace(string[r], r"\\ " => " "), r)
        expanded[3] && return expanded  # If user expansion available, return it

        paths, r, success = complete_path(replace(string[r], r"\\ " => " "), pos)

        if inc_tag === :string && close_path_completion(string, startpos, r, paths, pos)
            paths[1] = PathCompletion((paths[1]::PathCompletion).path * "\"")
        end

        #Latex symbols can be completed for strings
        (success || inc_tag==:cmd) && return sort!(paths, by=p->p.path), r, success
    end

    ok, ret = bslash_completions(string, pos)
    ok && return ret

    # Make sure that only bslash_completions is working on strings
    inc_tag==:string && return Completion[], 0:-1, false
    if inc_tag === :other && should_method_complete(partial)
        frange, method_name_end = find_start_brace(partial)
        # strip preceding ! operator
        s = replace(partial[frange], r"\!+([^=\(]+)" => s"\1")
        ex = Meta.parse(s * ")", raise=false, depwarn=false)

        if isa(ex, Expr)
            if ex.head === :call
                return complete_methods(ex, context_module), first(frange):method_name_end, false
            elseif ex.head === :. && ex.args[2] isa Expr && (ex.args[2]::Expr).head === :tuple
                return complete_methods(ex, context_module), first(frange):(method_name_end - 1), false
            end
        end
    elseif inc_tag === :comment
        return Completion[], 0:-1, false
    end

    dotpos = something(findprev(isequal('.'), string, pos), 0)
    startpos = nextind(string, something(findprev(in(non_identifier_chars), string, pos), 0))
    # strip preceding ! operator
    if (m = match(r"^\!+", string[startpos:pos])) !== nothing
        startpos += length(m.match)
    end

    ffunc = (mod,x)->true
    suggestions = Completion[]
    comp_keywords = true
    if afterusing(string, startpos)
        # We're right after using or import. Let's look only for packages
        # and modules we can reach from here

        # If there's no dot, we're in toplevel, so we should
        # also search for packages
        s = string[startpos:pos]
        if dotpos <= startpos
            for dir in Base.load_path()
                if basename(dir) in Base.project_names && isfile(dir)
                    append!(suggestions, project_deps_get_completion_candidates(s, dir))
                end
                isdir(dir) || continue
                for pname in readdir(dir)
                    if pname[1] != '.' && pname != "METADATA" &&
                        pname != "REQUIRE" && startswith(pname, s)
                        # Valid file paths are
                        #   <Mod>.jl
                        #   <Mod>/src/<Mod>.jl
                        #   <Mod>.jl/src/<Mod>.jl
                        if isfile(joinpath(dir, pname))
                            endswith(pname, ".jl") && push!(suggestions,
                                                            PackageCompletion(pname[1:prevind(pname, end-2)]))
                        else
                            mod_name = if endswith(pname, ".jl")
                                pname[1:prevind(pname, end-2)]
                            else
                                pname
                            end
                            if isfile(joinpath(dir, pname, "src",
                                               "$mod_name.jl"))
                                push!(suggestions, PackageCompletion(mod_name))
                            end
                        end
                    end
                end
            end
        end
        ffunc = (mod,x)->(Base.isbindingresolved(mod, x) && isdefined(mod, x) && isa(getfield(mod, x), Module))
        comp_keywords = false
    end
    startpos == 0 && (pos = -1)
    dotpos < startpos && (dotpos = startpos - 1)
    s = string[startpos:pos]
    comp_keywords && append!(suggestions, complete_keyword(s))
    # if the start of the string is a `.`, try to consume more input to get back to the beginning of the last expression
    if 0 < startpos <= lastindex(string) && string[startpos] == '.'
        i = prevind(string, startpos)
        while 0 < i
            c = string[i]
            if c in (')', ']')
                if c == ')'
                    c_start = '('
                    c_end = ')'
                elseif c == ']'
                    c_start = '['
                    c_end = ']'
                end
                frange, end_of_identifier = find_start_brace(string[1:prevind(string, i)], c_start=c_start, c_end=c_end)
                isempty(frange) && break # unbalanced parens
                startpos = first(frange)
                i = prevind(string, startpos)
            elseif c in ('\'', '\"', '\`')
                s = "$c$c"*string[startpos:pos]
                break
            else
                break
            end
            s = string[startpos:pos]
        end
    end
    append!(suggestions, complete_symbol(s, ffunc, context_module))
    return sort!(unique(suggestions), by=completion_text), (dotpos+1):pos, true
end

function shell_completions(string, pos)
    # First parse everything up to the current position
    scs = string[1:pos]
    local args, last_parse
    try
        args, last_parse = Base.shell_parse(scs, true)::Tuple{Expr,UnitRange{Int}}
    catch
        return Completion[], 0:-1, false
    end
    ex = args.args[end]::Expr
    # Now look at the last thing we parsed
    isempty(ex.args) && return Completion[], 0:-1, false
    arg = ex.args[end]
    if all(s -> isa(s, AbstractString), ex.args)
        arg = arg::AbstractString
        # Treat this as a path

        # As Base.shell_parse throws away trailing spaces (unless they are escaped),
        # we need to special case here.
        # If the last char was a space, but shell_parse ignored it search on "".
        ignore_last_word = arg != " " && scs[end] == ' '
        prefix = ignore_last_word ? "" : join(ex.args)

        # Also try looking into the env path if the user wants to complete the first argument
        use_envpath = !ignore_last_word && length(args.args) < 2

        return complete_path(prefix, pos, use_envpath=use_envpath, shell_escape=true)
    elseif isexpr(arg, :incomplete) || isexpr(arg, :error)
        partial = scs[last_parse]
        ret, range = completions(partial, lastindex(partial))
        range = range .+ (first(last_parse) - 1)
        return ret, range, true
    end
    return Completion[], 0:-1, false
end

function UndefVarError_hint(io::IO, ex::UndefVarError)
    var = ex.var
    if var === :or
        print(io, "\nsuggestion: Use `||` for short-circuiting boolean OR.")
    elseif var === :and
        print(io, "\nsuggestion: Use `&&` for short-circuiting boolean AND.")
    elseif var === :help
        println(io)
        # Show friendly help message when user types help or help() and help is undefined
        show(io, MIME("text/plain"), Base.Docs.parsedoc(Base.Docs.keywords[:help]))
    elseif var === :quit
        print(io, "\nsuggestion: To exit Julia, use Ctrl-D, or type exit() and press enter.")
    end
end

function __init__()
    Base.Experimental.register_error_hint(UndefVarError_hint, UndefVarError)
    nothing
end

end # module
