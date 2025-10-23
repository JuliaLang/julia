# The following are versions of macros from Base which act as "standard syntax
# extensions":
#
# * They emit syntactic forms with special `Kind`s and semantics known to
#   lowering
# * There is no other Julia surface syntax for these `Kind`s.

# In order to implement these here without getting into bootstrapping problems,
# we just write them as plain old macro-named functions and add the required
# __context__ argument ourselves.
#
# TODO: @inline, @noinline, @inbounds, @simd, @ccall, @assume_effects
#
# TODO: Eventually move these to proper `macro` definitions and use
# `JuliaLowering.include()` or something. Then we'll be in the fun little world
# of bootstrapping but it shouldn't be too painful :)

function _apply_nospecialize(ctx, ex)
    k = kind(ex)
    if k == K"Identifier" || k == K"Placeholder" || k == K"tuple"
        setmeta(ex; nospecialize=true)
    elseif k == K"..." || k == K"::" || k == K"="
        if k == K"::" && numchildren(ex) == 1
            ex = @ast ctx ex [K"::" "_"::K"Placeholder" ex[1]]
        end
        mapchildren(c->_apply_nospecialize(ctx, c), ctx, ex, 1:1)
    else
        throw(LoweringError(ex, "Invalid function argument"))
    end
end

function Base.var"@nospecialize"(__context__::MacroContext, ex, exs...)
    # TODO support multi-arg version properly
    _apply_nospecialize(__context__, ex)
end

function Base.var"@atomic"(__context__::MacroContext, ex)
    @chk kind(ex) == K"Identifier" || kind(ex) == K"::" (ex, "Expected identifier or declaration")
    @ast __context__ __context__.macrocall [K"atomic" ex]
end

function Base.var"@label"(__context__::MacroContext, ex)
    @chk kind(ex) == K"Identifier"
    @ast __context__ ex ex=>K"symbolic_label"
end

function Base.var"@goto"(__context__::MacroContext, ex)
    @chk kind(ex) == K"Identifier"
    @ast __context__ ex ex=>K"symbolic_goto"
end

function Base.var"@locals"(__context__::MacroContext)
    @ast __context__ __context__.macrocall [K"extension" "locals"::K"Symbol"]
end

function Base.var"@isdefined"(__context__::MacroContext, ex)
    @ast __context__ __context__.macrocall [K"isdefined" ex]
end

function Base.var"@generated"(__context__::MacroContext)
    @ast __context__ __context__.macrocall [K"generated"]
end
function Base.var"@generated"(__context__::MacroContext, ex)
    if kind(ex) != K"function"
        throw(LoweringError(ex, "Expected a function argument to `@generated`"))
    end
    @ast __context__ __context__.macrocall [K"function"
        ex[1]
        [K"if" [K"generated"]
            ex[2]
            [K"block"
                [K"meta" "generated_only"::K"Symbol"]
                [K"return"]
            ]
        ]
    ]
end

function Base.var"@cfunction"(__context__::MacroContext, callable, return_type, arg_types)
    if kind(arg_types) != K"tuple"
        throw(MacroExpansionError(arg_types, "@cfunction argument types must be a literal tuple"))
    end
    arg_types_svec = @ast __context__ arg_types [K"call"
        "svec"::K"core"
        children(arg_types)...
    ]
    if kind(callable) == K"$"
        fptr = callable[1]
        typ = Base.CFunction
    else
        # Kinda weird semantics here - without `$`, the callable is a top level
        # expression which will be evaluated by `jl_resolve_globals_in_ir`,
        # implicitly within the module where the `@cfunction` is expanded into.
        fptr = @ast __context__ callable [K"static_eval"(
                meta=name_hint("cfunction function name"))
            callable
        ]
        typ = Ptr{Cvoid}
    end
    @ast __context__ __context__.macrocall [K"cfunction"
        typ::K"Value"
        fptr
        [K"static_eval"(meta=name_hint("cfunction return type"))
            return_type
        ]
        [K"static_eval"(meta=name_hint("cfunction argument type"))
            arg_types_svec
        ]
        "ccall"::K"Symbol"
    ]
end

function ccall_macro_parse(ctx, ex, opts)
    gc_safe=false
    for opt in opts
        if kind(opt) != K"=" || numchildren(opt) != 2 ||
                kind(opt[1]) != K"Identifier"
            throw(MacroExpansionError(opt, "Bad option to ccall"))
        else
            optname = opt[1].name_val
            if optname == "gc_safe"
                if kind(opt[2]) == K"Bool"
                    gc_safe = opt[2].value::Bool
                else
                    throw(MacroExpansionError(opt[2], "gc_safe must be true or false"))
                end
            else
                throw(MacroExpansionError(opt[1], "Unknown option name for ccall"))
            end
        end
    end

    if kind(ex) != K"::"
        throw(MacroExpansionError(ex, "Expected a return type annotation `::SomeType`", position=:end))
    end

    rettype = ex[2]
    call = ex[1]
    if kind(call) != K"call"
        throw(MacroExpansionError(call, "Expected function call syntax `f()`"))
    end

    func = call[1]
    varargs = numchildren(call) > 1 && kind(call[end]) == K"parameters" ?
        children(call[end]) : nothing

    # collect args and types
    args = SyntaxList(ctx)
    types = SyntaxList(ctx)
    function pusharg!(arg)
        if kind(arg) != K"::"
            throw(MacroExpansionError(arg, "argument needs a type annotation"))
        end
        push!(args, arg[1])
        push!(types, arg[2])
    end

    for e in call[2:(isnothing(varargs) ? end : end-1)]
        kind(e) != K"parameters" || throw(MacroExpansionError(call[end], "Multiple parameter blocks not allowed"))
        pusharg!(e)
    end

    if !isnothing(varargs)
        num_required_args = length(args)
        if num_required_args == 0
            throw(MacroExpansionError(call[end], "C ABI prohibits varargs without one required argument"))
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
    statements = SyntaxTree[]
    kf = kind(func)
    if kf == K"Identifier"
        lowered_func = @ast ctx func func=>K"Symbol"
    elseif kf == K"."
        lowered_func = @ast ctx func [K"tuple"
            func[2]=>K"Symbol"
            [K"static_eval"(meta=name_hint("@ccall library name"))
                func[1]
            ]
        ]
    elseif kf == K"$"
        check = @SyntaxTree quote
            func = $(func[1])
            if !isa(func, Ptr{Cvoid})
                name = :($(func[1]))
                throw(ArgumentError("interpolated function `$name` was not a `Ptr{Cvoid}`, but $(typeof(func))"))
            end
        end
        push!(statements, check)
        lowered_func = check[1][1]
    else
        throw(MacroExpansionError(func,
            "Function name must be a symbol like `foo`, a library and function name like `libc.printf` or an interpolated function pointer like `\$ptr`"))
    end

    roots = SyntaxTree[]
    cargs = SyntaxTree[]
    for (i, (type, arg)) in enumerate(zip(types, args))
        argi = @ast ctx arg "arg$i"::K"Identifier"
        # TODO: Does it help to emit ssavar() here for the `argi`?
        push!(statements, @SyntaxTree :(local $argi = Base.cconvert($type, $arg)))
        push!(roots, argi)
        push!(cargs, @SyntaxTree :(Base.unsafe_convert($type, $argi)))
    end
    effect_flags = UInt16(0)
    push!(statements, @ast ctx ex [K"foreigncall"
        lowered_func
        [K"static_eval"(meta=name_hint("@ccall return type"))
            rettype
        ]
        [K"static_eval"(meta=name_hint("@ccall argument type"))
            [K"call"
                "svec"::K"core"
                types...
            ]
        ]
        num_required_args::K"Integer"
        QuoteNode((convention, effect_flags, gc_safe))::K"Value"
        cargs...
        roots...
    ])

    @ast ctx ex [K"block"
        statements...
    ]
end

function Base.var"@ccall"(ctx::MacroContext, ex, opts...)
    ccall_macro_lower(ctx, ex, :ccall, ccall_macro_parse(ctx, ex, opts)...)
end

function Base.GC.var"@preserve"(__context__::MacroContext, exs...)
    idents = exs[1:end-1]
    for e in idents
        if kind(e) != K"Identifier"
            throw(MacroExpansionError(e, "Preserved variable must be a symbol"))
        end
    end
    @ast __context__ __context__.macrocall [K"block"
        [K"="
            "s"::K"Identifier"
            [K"gc_preserve_begin"
                idents...
            ]
        ]
        [K"="
            "r"::K"Identifier"
            exs[end]
        ]
        [K"gc_preserve_end" "s"::K"Identifier"]
        "r"::K"Identifier"
    ]
end

function Base.Experimental.var"@opaque"(__context__::MacroContext, ex)
    @chk kind(ex) == K"->"
    @ast __context__ __context__.macrocall [K"opaque_closure"
        "nothing"::K"core"
        "nothing"::K"core"
        "nothing"::K"core"
        true::K"Bool"
        ex
    ]
end

#--------------------------------------------------------------------------------
# The following `@islocal` and `@inert` are macros for special syntax known to
# lowering which don't exist in Base but arguably should.
#
# For now we have our own versions
function var"@islocal"(__context__::MacroContext, ex)
    @chk kind(ex) == K"Identifier"
    @ast __context__ __context__.macrocall [K"extension"
        "islocal"::K"Symbol"
        ex
    ]
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
    @chk kind(ex) == K"quote"
    @ast __context__ __context__.macrocall [K"inert" ex]
end
