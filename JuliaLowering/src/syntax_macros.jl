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
    if k == K"Identifier" || k == K"Placeholder" || k == K"tuple" ||
        k == K"::" && numchildren(ex) == 1
        setmeta(ex, :nospecialize, true)
    elseif k == K"..." || k == K"::" || k == K"=" || k == K"kw"
        # The @nospecialize macro is responsible for converting K"=" to K"kw".
        # Desugaring uses this helper internally, so we may see K"kw" too.
        if k == K"=" && numchildren(ex) === 2
            ex = @ast ctx ex [K"kw" ex[1] ex[2]]
        end
        mapchildren(c->_apply_nospecialize(ctx, c), ctx, ex, 1:1)
    else
        throw(LoweringError(ex, "Invalid function argument"))
    end
end

function Base.var"@nospecialize"(__context__::MacroContext, exs::SyntaxTree...)
    if length(exs) == 0
        @ast __context__ __context__.macrocall [K"meta" "nospecialize"::K"Symbol"]
    elseif length(exs) == 1
        _apply_nospecialize(__context__, only(exs))
    else
        @ast __context__ __context__.macrocall [K"block"
            map(ex->_apply_nospecialize(__context__, ex), exs)...
        ]
     end
end

# TODO: support all forms that the original supports
# function Base.var"@atomic"(__context__::MacroContext, ex)
#     @chk kind(ex) == K"Identifier" || kind(ex) == K"::" (ex, "Expected identifier or declaration")
#     @ast __context__ __context__.macrocall [K"atomic" ex]
# end

function Base.var"@label"(__context__::MacroContext, ex)
    k = kind(ex)
    if k == K"Identifier"
        # `@label name` — goto label form
        @ast __context__ ex [K"symboliclabel" ex]
    elseif k == K"Placeholder"
        # `@label _` — disallowed
        throw(MacroExpansionError(ex, "use `@label expr` for anonymous blocks; `@label _` is not allowed"))
    else
        # `@label body` — 1-arg anonymous block form using `loop_exit` as the default scope
        name = @ast __context__ __context__.macrocall "loop_exit"::K"symboliclabel"
        @ast __context__ __context__.macrocall [K"symbolicblock" name ex]
    end
end

function Base.var"@label"(__context__::MacroContext, name, body)
    k = kind(name)
    if k == K"Placeholder"
        # `@label _ body` — disallowed
        throw(MacroExpansionError(name, "use `@label expr` for anonymous blocks; `@label _ expr` is not allowed"))
    elseif k == K"Identifier"
        # `@label name body` - plain identifier
    elseif is_contextual_keyword(k)
        # Contextual keyword used as label name (e.g., `@label outer body`)
    else
        throw(MacroExpansionError(name, "Expected identifier for block label"))
    end
    # If body is a syntactic loop, wrap its body in a continue block
    # This allows `continue name` to work by breaking to `name#cont`
    body_kind = kind(body)
    if body_kind == K"for" || body_kind == K"while"
        cont_name = mkleaf(name) # use name's scope and attrs
        setattr!(name, :kind, K"Identifier")
        setattr!(name, :name_val, string(name.name_val, "#cont"))
        loop_body = body[2]
        wrapped_body = @ast __context__ loop_body [K"symbolicblock"
            cont_name
            loop_body
        ]
        body = @ast __context__ body [body_kind body[1] wrapped_body]
    end
    @ast __context__ __context__.macrocall [K"symbolicblock" name body]
end

function Base.var"@goto"(__context__::MacroContext, ex)
    @chk kind(ex) == K"Identifier"
    @ast __context__ ex [K"symbolicgoto" ex]
end

function Base.var"@locals"(__context__::MacroContext)
    @ast __context__ __context__.macrocall [K"locals"]
end

function Base.var"@isdefined"(__context__::MacroContext, ex)
    @ast __context__ __context__.macrocall [K"isdefined" ex]
end

function Base.var"@generated"(__context__::MacroContext)
    @ast __context__ __context__.macrocall [K"generated"]
end
function Base.var"@generated"(__context__::MacroContext, ex)
    if !(kind(ex) === K"function" ||
        kind(ex) === K"=" && is_eventually_call(ex[1]))
        throw(LoweringError(ex, "Expected a function argument to `@generated`"))
    end
    @ast __context__ __context__.macrocall [K"function"
        ex[1]
        [K"if" [K"generated"]
            ex[2]
            [K"block"
                [K"meta" "generated_only"::K"Identifier"]
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
        [K"core" "svec"::K"Identifier"]
        children(arg_types)...
    ]
    if kind(callable) == K"$"
        fptr = callable[1]
        typ = Base.CFunction
    else
        # Kinda weird semantics here - without `$`, the callable is a top level
        # expression evaluated within the module where the `@cfunction` is
        # expanded into.
        fptr = @ast __context__ callable [K"inert"
            callable
        ]
        typ = Ptr{Cvoid}
    end
    @ast __context__ __context__.macrocall [K"cfunction"
        typ::K"Value"
        fptr
        return_type
        arg_types_svec
        [K"inert" "ccall"::K"Identifier"]
    ]
end

function ccall_macro_parse(ctx, exs)
    gc_safe=false
    opts = exs[1:end-1]
    ex = exs[end]
    for opt in opts
        @stm opt begin
            [K"=" [K"Identifier"] val] -> if opt[1].name_val != "gc_safe"
                throw(MacroExpansionError(opt[1], "unknown option name for ccall"))
            elseif !(kind(val) in KSet"Bool Value")
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
        [K"::" [K"call" f as...] r] -> let f_expanded = @stm f begin
            [K"." lib sym] -> @ast ctx f [K"tuple" sym lib]
            [K"inert" [K"Identifier"]] -> @ast ctx f [K"tuple" f]
            [K"Identifier"] -> @ast ctx f [K"tuple" [K"inert" f]]
            [K"$" x] -> let kx = kind(x)
                if kx in KSet"tuple String string" ||
                        (kx === K"Value" && x.value isa Tuple) ||
                        kx == K"inert" && !(kx[1].value isa Ptr)
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
        [K"call" _...] -> throw(MacroExpansionError(
            ex, "expected a return type annotation `::SomeType`", position=:end))
        _ -> throw(MacroExpansionError(
            ex, "expected call expression with return type"))
    end

    # detect varargs
    varargs = nothing
    argstart = 1
    if length(argts) > 0 && kind(argts[1]) == K"parameters"
        varargs = children(argts[1])
        argstart = 2
    end

    # collect args and types
    args = SyntaxList(ctx)
    types = SyntaxList(ctx)
    function pusharg!(at)
        @stm at begin
            [K"::" a t] -> (push!(args, a); push!(types, t))
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
    return @ast ctx ex [K"call"
        "ccall"::K"Identifier"
        func
        [K"cconv" cconv_tuple::K"Value" num_required_args::K"Value"]
        rettype
        [K"tuple" types...]
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
        if kind(e) != K"Identifier"
            throw(MacroExpansionError(e, "Preserved variable must be a symbol"))
        end
    end
    @ast __context__ __context__.macrocall [K"gc_preserve" exs[end] exs[1:end-1]...]
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

function _at_eval_code(ctx, srcref, mod, ex)
    @ast ctx srcref [K"block"
        [K"local"
            [K"="
                "eval_result"::K"Identifier"
                [K"call"
                    # TODO: Call "eval"::K"core" here
                    JuliaLowering.eval::K"Value"
                    [K"parameters"
                        [K"kw"
                            "expr_compat_mode"::K"Identifier"
                            ctx.expr_compat_mode::K"Bool"
                        ]
                    ]
                    mod
                    [K"quote" ex]
                ]
            ]
        ]
        [K"unknown_head"(name_val="latestworld-if-toplevel")]
        "eval_result"::K"Identifier"
    ]
end

function Base.var"@eval"(__context__::MacroContext, ex)
    mod = @ast __context__ __context__.macrocall __context__.scope_layer.mod::K"Value"
    _at_eval_code(__context__, __context__.macrocall, mod, ex)
end

function Base.var"@eval"(__context__::MacroContext, mod, ex)
    _at_eval_code(__context__, __context__.macrocall, mod, ex)
end

#--------------------------------------------------------------------------------
# The following `@islocal` and `@inert` are macros for special syntax known to
# lowering which don't exist in Base but arguably should.
#
# For now we have our own versions
function var"@islocal"(__context__::MacroContext, ex)
    @chk kind(ex) == K"Identifier"
    @ast __context__ __context__.macrocall [K"islocal" ex]
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
