# Experimental "new macros" mostly for testing.  These should eventually be
# deleted and replaced with normal-looking macros lowered by JL.
#
# TODO: @inline, @noinline, @inbounds, @simd, @ccall, @assume_effects
#
# TODO: Eventually move these to proper `macro` definitions and use
# `JuliaLowering.include()` or something. Then we'll be in the fun little world
# of bootstrapping but it shouldn't be too painful :)

# Note that `@ast __context__ __context__.macrocall [:foo ...]` is unhygienic,
# since `@ast` is meant for internal lowering use (it requires an explicit
# provenance argument, and then copies any syntax context from the provenance to
# any created syntax).  A real user-facing macro to replace it should use the
# provenance of the literal :foo expression in the file instead, and should
# not copy context (this is not hard to implement, but the provenance requires
# it and callers to be JL-lowered, which this file currently isn't.)

function Base.var"@nospecialize"(__context__::MacroContext, exs::SyntaxTree...)
    if length(exs) == 0
        @ast __context__ __context__.macrocall [:meta
            "nospecialize"::identifier]
    elseif length(exs) == 1 && head(exs[1]) === :(=)
        eq = exs[1]
        @ast __context__ __context__.macrocall [:meta
            "nospecialize"::identifier [:kw(eq) children(eq)...]]
    else
        @ast __context__ __context__.macrocall [:meta
            "nospecialize"::identifier exs...]
    end
end

# TODO: support all forms that the original supports
# function Base.var"@atomic"(__context__::MacroContext, ex)
#     @jl_assert head(ex) == :identifier || head(ex) == :(::) (ex, "Expected identifier or declaration")
#     @ast __context__ __context__.macrocall [:atomic ex]
# end

# TODO: @label

function Base.var"@goto"(__context__::MacroContext, ex)
    @jl_assert head(ex) == :identifier ex
    @ast __context__ ex [:symbolicgoto ex]
end

function Base.var"@locals"(__context__::MacroContext)
    @ast __context__ __context__.macrocall [:locals]
end

@static if isdefined(Base, Symbol("@__FUNCTION__"))
function Base.var"@__FUNCTION__"(__context__::MacroContext)
    @ast __context__ __context__.macrocall [:thisfunction]
end
end

function Base.var"@isdefined"(__context__::MacroContext, ex)
    @ast __context__ __context__.macrocall [:isdefined ex]
end

function Base.var"@generated"(__context__::MacroContext)
    @ast __context__ __context__.macrocall [:generated]
end
function Base.var"@generated"(__context__::MacroContext, ex)
    if !(head(ex) === :function ||
        head(ex) === :(=) && is_eventually_call(ex[1]))
        throw(LoweringError(ex, "Expected a function argument to `@generated`"))
    end
    @ast __context__ __context__.macrocall [:function
        ex[1]
        [:block
            [:if [:generated]
                ex[2]
                [:block
                    [:meta "generated_only"::identifier]
                    [:return nothing::value]
                ]
            ]
        ]
    ]
end

function Base.var"@cfunction"(__context__::MacroContext, callable, return_type, arg_types)
    if head(arg_types) != :tuple
        throw(MacroExpansionError(arg_types, "@cfunction argument types must be a literal tuple"))
    end
    arg_types_svec = @ast __context__ arg_types [:call
        [:core "svec"::identifier]
        children(arg_types)...
    ]
    if head(callable) == :$
        fptr = callable[1]
        typ = Base.CFunction
    else
        # Kinda weird semantics here - without `$`, the callable is a top level
        # expression evaluated within the module where the `@cfunction` is
        # expanded into.
        fptr = @ast __context__ callable [:inert
            callable
        ]
        typ = Ptr{Cvoid}
    end
    @ast __context__ __context__.macrocall [:cfunction
        typ::value
        fptr
        return_type
        arg_types_svec
        [:inert "ccall"::identifier]
    ]
end

function ccall_macro_parse(ctx, exs)
    gc_safe=false
    opts = exs[1:end-1]
    ex = exs[end]
    for opt in opts
        @stm opt begin
            [:(=) [:identifier] val] -> if syntax_name(opt[1]) != "gc_safe"
                throw(MacroExpansionError(opt[1], "unknown option name for ccall"))
            elseif head(val) !== :value || !(val.value isa Bool)
                throw(MacroExpansionError(val, "gc_safe must be true or false"))
            else
                gc_safe = val.value
            end
            _ -> throw(MacroExpansionError(opt, "bad option to ccall"))
        end
    end
    if length(opts) >= 2
        throw(MacroExpansionError(opts[2], "too many options provided to @ccall"))
    end

    (func, argts, rettype) = @stm ex begin
        [:(::) [:call f as...] r] -> let f_expanded = @stm f begin
            [:. lib sym] -> @ast ctx f [:tuple sym lib]
            [:inert [:identifier]] -> @ast ctx f [:tuple f]
            [:identifier] -> @ast ctx f [:tuple [:inert f]]
            [:$ x] -> let kx = head(x)
                if kx === :tuple || kx === :string ||
                        (kx === :value && (x.value isa Tuple || x.value isa String)) ||
                        kx == :inert && !(head(x[1]) == :value && x[1].value isa Ptr)
                    throw(MacroExpansionError(
                        f, "interpolated value should be a variable or expression, not a literal name or tuple"))
                end
                x
            end
            _ -> throw(MacroExpansionError(
                f, "@ccall function name must be a symbol, a `.` node (e.g. `libc.printf`) or an interpolated function pointer (with `\$`)"))
        end
            (f_expanded, as, r)
        end
        [:call _...] -> throw(MacroExpansionError(
            ex, "expected a return type annotation `::SomeType`", position=:end))
        _ -> throw(MacroExpansionError(
            ex, "expected call expression with return type"))
    end

    # detect varargs
    varargs = nothing
    argstart = 1
    if length(argts) > 0 && head(argts[1]) == :parameters
        varargs = children(argts[1])
        argstart = 2
    end

    # collect args and types
    args = SyntaxList()
    types = SyntaxList()
    function pusharg!(at)
        @stm at begin
            [:(::) a t] -> (push!(args, a); push!(types, t))
            _ -> throw(MacroExpansionError(
                at, "argument needs a type annotation"))
        end
    end

    for e in argts[argstart:end]
        pusharg!(e)
    end

    if !isnothing(varargs)
        num_required_args = length(args)
        if num_required_args == 0
            throw(MacroExpansionError(
                argts[1],
                "C ABI prohibits varargs without one required argument"))
        end
        for e in varargs
            pusharg!(e)
        end
    else
        num_required_args = 0 # Non-vararg call
    end

    return func, rettype, types, args, gc_safe, num_required_args
end

function ccall_macro_lower(ctx, ex, convention, func, rettype, types, args, gc_safe, num_required_args)
    if convention isa Tuple
        cconv_tuple = (convention..., gc_safe)
    else
        cconv_tuple = (convention, UInt16(0), gc_safe)
    end
    return @ast ctx ex [:call
        "ccall"::identifier
        func
        [:cconv cconv_tuple::value num_required_args::value]
        rettype
        [:tuple types...]
        args...
    ]
end

function Base.var"@ccall"(ctx::MacroContext)
    throw(ArgumentError("@ccall needs a function signature with a return type"))
end

function Base.var"@ccall"(ctx::MacroContext, exs...)
    ccall_macro_lower(ctx, exs[end], :ccall, ccall_macro_parse(ctx, exs)...)
end

function Base.GC.var"@preserve"(__context__::MacroContext, exs...)
    idents = exs[1:end-1]
    for e in idents
        if head(e) != :identifier
            throw(MacroExpansionError(e, "Preserved variable must be a symbol"))
        end
    end
    @ast __context__ __context__.macrocall [:gc_preserve exs[end] exs[1:end-1]...]
end

function Base.Experimental.var"@opaque"(__context__::MacroContext, ex)
    @jl_assert head(ex) == :-> ex
    @ast __context__ __context__.macrocall [:opaque_closure
        nothing::value
        nothing::value
        nothing::value
        true::value
        ex
    ]
end

# @eval should mostly ignore hygiene against our system's best wishes.  Still
# attempt to preserve provenance.
function _at_eval_code(mc::MacroContext, mod_st::SyntaxTree, ex)
    sc = mc.macrocall.context
    val = remove_scope(@ast mc mc.macrocall ("eval_result"::identifier))
    q = _legacy_quote_to_syntax((@ast mc mc.macrocall [:quote ex]), 0, true)
    new_sc = SyntaxContext(base_layer(sc).mod, sc.edition)
    @ast mc mc.macrocall [:block
        [:local
            [:(=)
                val
                [:call JuliaLowering.eval::value
                    mod_st
                    [:call JuliaSyntax.fill_context::value q new_sc::value]
                ]
            ]
        ]
        [:var"latestworld-if-toplevel"]
        val
    ]
end
function Base.var"@eval"(__context__::MacroContext, ex)
    sc = __context__.macrocall.context
    mod = @ast __context__ __context__.macrocall base_layer(sc).mod::value
    _at_eval_code(__context__, mod, ex)
end

function Base.var"@eval"(__context__::MacroContext, mod, ex)
    _at_eval_code(__context__, mod, ex)
end

#--------------------------------------------------------------------------------
# The following `@islocal` and `@inert` are macros for special syntax known to
# lowering which don't exist in Base but arguably should.
#
# For now we have our own versions
function var"@islocal"(__context__::MacroContext, ex)
    @jl_assert head(ex) == :identifier ex
    @ast __context__ __context__.macrocall [:islocal ex]
end

"""
A non-interpolating quoted expression.

For example,

```julia
@inert quote
    \$x
end
```

does not take `x` from the surrounding scope - instead it leaves the
interpolation `\$x` intact as part of the expression tree.

TODO: What is the correct way for `@inert` to work? ie which of the following
should work?

```julia
@inert quote
   body
end

@inert begin
   body
end

@inert x

@inert \$x
```

The especially tricky cases involve nested interpolation ...
```julia
quote
    @inert \$x
end

@inert quote
    quote
        \$x
    end
end

@inert quote
    quote
        \$\$x
    end
end
```

etc. Needs careful thought - we should probably just copy what lisp does with
quote+quasiquote 😅
"""
function var"@inert"(__context__::MacroContext, ex)
    @jl_assert head(ex) == :quote ex
    @ast __context__ __context__.macrocall [:inert ex]
end

# `quote`/`inert` for syntaxtree
function var"@syntaxinert"(__context__::MacroContext, st)
    @ast __context__ __context__.macrocall [:syntaxinert st]
end
function var"@syntaxquote"(__context__::MacroContext, st)
    @ast __context__ __context__.macrocall [:syntaxquote st]
end
# not particularly good or useful, as @syntaxquote must expand first
function var"@syntaxunquote"(__context__::MacroContext, st)
    @ast __context__ __context__.macrocall [:syntaxunquote st]
end

# If the edition allows, convert quote/$ to syntaxquote/syntaxunquote.
# This is just a convenient way to create SyntaxTree with full provenance
# without dedicated surface syntax, mainly for testing metaprogramming in JL.
# It is insufficient in many ways, e.g. not all forms can be expressed (need
# surface syntax)
function var"@legacy_quote_to_syntax"(__context__::MacroContext, st)
    @jl_assert head(st) === :quote || head(st) === :inert st
    if is_flisp_compat(__context__.macrocall)
        st
    elseif head(st) === :inert
        @mknode(st; head=:syntaxinert) # parser simplifies quote to inert
    else
        _legacy_quote_to_syntax(st, 0, false)
    end
end
function _legacy_quote_to_syntax(st::SyntaxTree, depth, force::Bool)
    k = head(st)
    if k === :quote && depth == 0 && (force || !is_flisp_compat(st))
        @jl_assert numchildren(st) == 1 st
        @mknode(st; head=:syntaxquote, children=
            mapsyntax(c->_legacy_quote_to_syntax(c, depth+1, force), children(st)))
    elseif k === :$ && depth == 1 && (force || !is_flisp_compat(st))
        @jl_assert numchildren(st) == 1 (st, "bad multi-syntaxunquote")
        @mknode(st; head=:syntaxunquote)
    else
        depth2 = k === :quote ? depth + 1 : k === :$ ? depth - 1 : depth
        cs = SyntaxList()
        for c in children(st)
            # Convert multi-unquote to single unquote
            if depth2 == 1 && head(c) === :$ && numchildren(c) > 1
                for c2 in children(c)
                    push!(cs, @ast _ c [:$ c2])
                end
            else
                push!(cs, c)
            end
        end
        cs_out = mapsyntax(c->_legacy_quote_to_syntax(c, depth2, force), cs)
        cs_out == children(st) ? st : @mknode(st; children=cs_out)
    end
end
macro legacy_quote_to_syntax(x)
    esc(x)
end

"""
Retrieve the edition of the macrocall
"""
function var"@edition"(__context__::MacroContext)
    __context__.macrocall.context.edition
end
macro edition()
    JL_OLD_EDITION
end

"""
Set the edition for some syntax.  This can be used to define macros with older
signatures in newer editions.
"""
function var"@edition"(__context__::MacroContext, ver_st, st)
    head(st) === :macro || throw(LoweringError(
        st, "`@edition edition macro` only supports macro definitions"))
    ver = JuliaLowering.eval(syntax_module(ver_st), ver_st)
    en = ver isa Tuple{Int, Int} ? ver :
        ver isa VersionNumber ? (Int(ver.major), Int(ver.minor)) : throw(LoweringError(
            ver_st, "expected version `v\"...\"`"))
    _ensure_edition(st, en)
end
macro edition(_, x)
    throw(ArgumentError("@edition requires JuliaLowering"))
end

function _ensure_edition(st, en::Tuple{Int, Int},
                         scmap=Dict{SyntaxContext, SyntaxContext}())
    sc = st.context
    sc2 = get(scmap, sc, nothing)
    if isnothing(sc2)
        sc2 = scmap[sc] = sc.edition == en ? sc :
            SyntaxContext(sc.layer, sc.unexpanded, en, sc.internal)
    end
    if is_leaf(st) || numchildren(st) == 0
        sc2 == sc ? st : @mknode(st; context=sc2)
    else
        out = mapchildren(c->_ensure_edition(c, en, scmap), st)
        sc2 == sc ? out : @mknode(out; context=sc2)
    end
end
