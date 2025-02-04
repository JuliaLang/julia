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
# TODO: @inline, @noinline, @inbounds, @simd, @ccall, @isdefined, @assume_effects
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

function Base.var"@nospecialize"(__context__::MacroContext, ex)
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
        #
        # TODO: The existing flisp implementation is arguably broken because it
        # ignores macro hygiene when `callable` is the result of a macro
        # expansion within a different module. For now we've inherited this
        # brokenness.
        #
        # Ideally we'd fix this by bringing the scoping rules for this
        # expression back into lowering. One option may be to wrap the
        # expression in a form which pushes it to top level - maybe as a whole
        # separate top level thunk like closure lowering - then use the
        # K"captured_local" mechanism to interpolate it back in. This scheme
        # would make the complicated scope semantics explicit and let them be
        # dealt with in the right place in the frontend rather than putting the
        # rules into the runtime itself.
        fptr = @ast __context__ callable QuoteNode(Expr(callable))::K"Value"
        typ = Ptr{Cvoid}
    end
    @ast __context__ __context__.macrocall [K"cfunction"
        typ::K"Value"
        fptr
        return_type
        arg_types_svec
        "ccall"::K"Symbol"
    ]
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

