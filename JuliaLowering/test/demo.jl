# Just some hacking

using JuliaSyntax
using JuliaLowering

using JuliaLowering: SyntaxGraph, SyntaxTree, ensure_attributes!, ensure_attributes, newnode!, setchildren!, is_leaf, @ast, numchildren, children, child, setattr!, sourceref, makenode, sourcetext, showprov, lookup_binding

using JuliaSyntaxFormatter

# Extract variable kind for highlighting purposes
function var_kind(ctx, ex)
    id = get(ex, :var_id, nothing)
    if isnothing(id)
        return nothing
    end
    binfo = lookup_binding(ctx, id)
    return binfo.kind == :local ?
        (binfo.is_captured ? :local_captured : :local) :
        binfo.kind
end

# Extract module of globals for highlighting
function var_mod(ctx, ex)
    id = get(ex, :var_id, nothing)
    if isnothing(id)
        return nothing
    end
    return lookup_binding(ctx, id).mod
end

function formatsrc(ex; kws...)
    Text(JuliaSyntaxFormatter.formatsrc(ex; kws...))
end

function debug_lower(mod::Module, ex::SyntaxTree; expr_compat_mode::Bool=false, verbose::Bool=false, do_eval::Bool=false)
    ctx1, ex_macroexpand = JuliaLowering.expand_forms_1(mod, ex, expr_compat_mode, Base.get_world_counter())

    verbose && @info "Macro expanded" formatsrc(ex_macroexpand, color_by=:scope_layer)

    ctx2, ex_desugar = JuliaLowering.expand_forms_2(ctx1, ex_macroexpand)
    verbose && @info "Desugared" formatsrc(ex_desugar, color_by=:scope_layer)

    ctx3, ex_scoped = JuliaLowering.resolve_scopes(ctx2, ex_desugar)
    verbose && @info "Resolved scopes" formatsrc(ex_scoped, color_by=e->var_kind(ctx2,e))

    ctx4, ex_converted = JuliaLowering.convert_closures(ctx3, ex_scoped)
    verbose && @info "Closure converted" formatsrc(ex_converted, color_by=:var_id)

    ctx5, ex_compiled = JuliaLowering.linearize_ir(ctx4, ex_converted)
    verbose && @info "Linear IR" formatsrc(ex_compiled, color_by=:var_id) Text(sprint(JuliaLowering.print_ir, ex_compiled))

    ex_expr = JuliaLowering.to_lowered_expr(mod, ex_compiled)
    verbose && @info "CodeInfo" ex_expr

    if do_eval
        eval_result = Base.eval(mod, ex_expr)
        verbose && @info "Eval" eval_result
    else
        eval_result = nothing
    end

    (ctx1, ex_macroexpand, ctx2, ex_desugar, ctx3, ex_scoped, ctx4, ex_converted, ctx5, ex_compiled, ex_expr, eval_result)
end


# Currently broken - need to push info back onto src
# function annotate_scopes(mod, ex)
#     ex = ensure_attributes(ex, var_id=Int)
#     ctx1, ex_macroexpand = JuliaLowering.expand_forms_1(mod, ex, false)
#     ctx2, ex_desugar = JuliaLowering.expand_forms_2(ctx1, ex_macroexpand)
#     ctx3, ex_scoped = JuliaLowering.resolve_scopes(ctx2, ex_desugar)
#     ex
# end

#-------------------------------------------------------------------------------
# Module containing macros used in the demo.
define_macros = true
if !define_macros
    eval(:(module M end))
else
eval(JuliaLowering.@SyntaxTree :(baremodule M
    using Base

    using JuliaLowering: JuliaLowering, @ast, @chk, adopt_scope, MacroExpansionError, makenode
    using JuliaSyntax
    using JuliaLowering: @inert, @label, @goto, @islocal
    using Base: @locals

    macro K_str(str)
        JuliaSyntax.Kind(str)
    end

    # Introspection
    macro __MODULE__()
        __context__.scope_layer.mod
    end

    macro __FILE__()
        JuliaLowering.filename(__context__.macrocall)
    end

    macro __LINE__()
        JuliaLowering.source_location(__context__.macrocall)[1]
    end

    # Macro with local variables
    module A
        another_global = "global in A"

        macro bar(ex)
            quote
                x = "`x` in @bar"
                (x, another_global, $ex)
            end
        end
    end

    someglobal = "global in module M"

    # Macro with local variables
    macro foo(ex)
        quote
            x = "`x` from @foo"
            (x, someglobal, A.@bar $ex)
            #(x, someglobal, $ex, A.@bar($ex), A.@bar(x))
        end
    end

    macro call_show(x)
        quote
            z = "z in @call_show"
            @show z $x
        end
    end

    macro call_info(x)
        quote
            z = "z in @call_info"
            @info "hi" z $x
        end
    end

    macro call_oldstyle_macro(y)
        quote
            x = "x in call_oldstyle_macro"
            @oldstyle $y x
        end
    end

    macro newstyle(x, y, z)
        quote
            x = "x in @newstyle"
            ($x, $y, $z, x)
        end
    end

    macro set_a_global(val)
        quote
            global a_global = $val
        end
    end

    macro set_global_in_parent(ex)
        e1 = adopt_scope(:(sym_introduced_from_M), __context__)
        quote
            $e1 = $ex
        end
    end

    macro baz(ex)
        quote
            let $ex = 10
                $ex
            end
        end
    end

    macro make_module()
        :(module X
              blah = 10
          end)
    end

    macro return_a_value()
        42
    end

    macro nested_return_a_value()
        :(
            @return_a_value
        )
    end

    macro inner()
        :(2)
    end

    macro outer()
        :((1, @inner))
    end

    macro K_str(str)
        JuliaSyntax.Kind(str[1].value)
    end

    # Recursive macro call
    macro recursive(N)
        Nval = if kind(N) == K"Integer" || kind(N) == K"Value"
            N.value
        end
        if !(Nval isa Integer)
            throw(MacroExpansionError(N, "argument must be an integer"))
        end
        if Nval < 1
            return N
        end
        quote
            x = $N
            (@recursive($(Nval-1)), x)
        end
    end

    xx = "xx in M"

    macro test_inert_quote()
        println(xx)
        @inert quote
            ($xx, xx)
        end
    end

    macro mmm(ex)
        :(let
              local x
              function f()
                  (x, $ex)
              end
              f()
          end)
    end

end))
end

Base.eval(M, :(
macro oldstyle(a, b)
    quote
        x = "x in @oldstyle"
        @newstyle $(esc(a)) $(esc(b)) x
    end
end
))

#
#-------------------------------------------------------------------------------
# Demos of the prototype

# src = """
# let
#     local x, (y = 2), (w::T = ww), q::S
# end
# """

# src = """
# function foo(x::f(T), y::w(let ; S end))
#     "a \$("b \$("c")")"
# end
# """

src = """
begin
    function f(x)
        nothing
    end

    f(1)
end
"""

# src = """
#     x + y
# """

# src = """
# module A
#     function f(x)::Int
#         x + 1
#     end
#
#     b = f(2)
# end
# """

# src = """
# function f()
# end
# """
#
# src = """
# # import A.B: C.c as d, E.e as f
# # import JuliaLowering
# using JuliaLowering
# """
#
# src = """
# module A
#     z = 1 + 1
# end
# """

src = raw"""
begin
    x = 10
    y = :(g(z))
    quote
        f($(x+1), $y)
    end
end
"""

function wrapscope(ex, scope_type)
    makenode(ex, ex, K"scope_block", ex; scope_type=scope_type)
end

function softscope_test(ex)
    g = ensure_attributes(ex._graph, scope_type=Symbol)
    wrapscope(wrapscope(JuliaLowering.reparent(g, ex), :neutral), :soft)
end

# src = """
# M.@test_inert_quote()
# """

# src = """
# macro mmm(a; b=2)
# end
# macro A.b(ex)
# end
# """

# src = """
# M.@set_global_in_parent "bent hygiene!"
# """

# src = """
# begin
# M.@__LINE__
# end
# """

# src = """@foo z"""

src = """
M.@recursive 3
"""

# src = """
# M.@set_global_in_parent "bent hygiene!"
# """

# src = """
# begin
#    x = 10
#    y = 20
#    let x = y + x
#        z = "some string \$x \$y"
#
#        function f(y)
#            a = M.@foo z
#            "\$z \$y \$a \$x"
#        end
#        print(x)
#    end
#    print(x)
# end
# """

# src = """
# begin
#     x = -1
#     M.@baz x
# end
# """

# src = """
#     _ = -1
# """

# src = """
# M.@make_module
# """

# src = """
# M.@nested_return_a_value
# """

# src = """
# function f(y)
#     x = 42 + y
#     M.@foo error(x)
# end
# """

src = """
let
    y = 0
    x = 1
    let x = x + 1
        y = x
    end
    (x, y)
end
"""

#src = """M.@outer"""

src = """
begin
    local a, b, c
    if a
        b
    else
        c
    end
end
"""

src = """
begin
    local i = 0
    while i < 10
        i = i + 1
        if isodd(i)
            continue
        end
        println(i)
    end
end
"""

src = """
for i in [3,1,2]
    println("i = ", i, ", j = ", j)
end
"""

# src = """
# @ccall f()::T
# """
#
# src = """
# begin
#     a = 1
#     xs = [:(a),]
#     x = :(:(\$(\$(xs...))))
# end
# """

# src = """
# try
#     a
# catch exc
#     b
# end
# """

src = """
let
    a = []
    for i = 1:2, j = 3:4
        push!(a, (i,j))
        i = 100
    end
    a
end
"""

src = """
begin
    function f(x)
        y = x + 1
        "hi", x, y
    end

    f(1)
end
"""

src = """
let
    x = try
        error("hi")
        1
    catch exc
        current_exceptions()
    else
        3
    end
    x
end
"""

src = """
function f(y)
    x =
    try
        try
            error("hi")
            1
        catch exc
            if y
                return 2
            end
            3
        else
            4
        end
    catch
        5
    end
    x
end
"""

src = """
function f(x)::Int
    if x
        42.0
    end
    0xff
end
"""

src = """
let x = 10
    global a = []
    try
        try
            return 100
        finally
            push!(a, 1)
        end
    finally
        push!(a, 2)
    end
    x
end
"""

src = """
let
    for outer i = 1:2
        body
    end
end
"""

src = """
let
    i = "hi"
    j = 1
    M.@label foo
    try
        println("i = ", i)
        i = i + 1
        if i <= 2
            M.@goto foo
        end
    catch exc
        println("Caught exception ", exc)
        j = j + 1
        if j <= 2
            println("Trying again ", exc)
            M.@goto foo
        end
    end
end
"""

src = """
let
    M.@goto foo
    M.@label foo
end
"""

src = """
x = M.@label foo
"""

src = """
begin
    local x::T = 1
    local x::S = 1
end
"""

src = """
begin
    local a, b
    if a
        b
    end
end
"""

src = """
let
    A{S} = B{S}
end
"""

src = """
let
    a = b = c = sin(1)
    (a,b,c)
end
"""

src = """
a.b = c
"""

src = """
a[i j] = c
"""

src = """
let
    as = [1,2,3,4]
    (x,ys...,z) = as
    (x,ys,z)
end
"""

src = """
let
    x = (1,2)
    (y,x) = x
    (x,y)
end
"""

src = """
let
    a = b = c = sin(1)
    (a,b,c)
end
"""

src = """
begin
    as = [(1,2), (3,4)]
    ((x,y), (z,w)) = as
end
"""

src = """
let
(x, y) = (y,x)
end
"""

src = """
let x = 1
    M.@islocal x
end
"""

src = """
let x = 1
    local y
    M.@locals
end
"""

src = """
let
    (a, bs...,) = (1,2,3)
    bs
end
"""

src = """
(; a=1, a=2)
"""

src = """
begin
    kws = (c=3, d=4)
    xs = 1:3
    f(xs...; kws..., a=1, b=2)
end
"""

src = """
"some docs"
function f()
    println("hi")
end
"""

src = """
function f(::T, ::U, ::S) where T where {U,S}
    println(T)
    println(U)
    println(S)
end
"""

src = """
function (x::XXX)(y)
    println("hi", " ", x, " ", y)
end
"""

src = """
struct X
    x
    y::String
end
"""

src = """
struct X{U,V}
    x::U
    y::V
end
"""

src = """
struct S9{T}
    x
    y

    "Docs for S9"
    S9{Int}(xs) = new(xs...)
end
"""

# Default positional args with missing arg names
src = """
function f(::Int, y=1, z=2)
    (y, z)
end
"""

# Default positional args with placeholders
src = """
function f(_::Int, x=1)
    x
end
"""

# Positional args and type parameters with transitive dependencies
# Bug in flisp lowering - see https://github.com/JuliaLang/julia/issues/49275
src = """
function f(x, y::S=[1], z) where {T, S<:AbstractVector{T}}
    (x, y, z, T)
end
"""

# Default positional args before trailing slurp are allowed
src = """
function f(x=1, ys...)
    ys
end
"""

# Default positional args after a slurp is an error
src = """
function f(x=1, ys..., z=2)
    ys
end
"""

# Positional arg with slurp and default
src = """
function f(x=1, ys...="hi")
    ys
end
"""

# Positional arg with slurp and splat
src = """
function f(x=1, ys...=(1,2)...)
    ys
end
"""

src = """
let
    x = 10
    function f(y)
        x + y
    end
end
"""

src = """
begin
    local f, set_x
    local x = 10
    local y = 100
    function f()
        z = 1 + y - x
        z
    end
    function set_x()
        x = 1
    end
    println("f = ", f())
    set_x()
    y = 10
    println("f = ", f())
end
"""

# TODO: fix this - it's interpreted in a bizarre way as a kw call.
# src = """
# function f(x=y=1)
#     x
# end
# """

function gen_stuff(ctx, N, x)
    JuliaLowering.@ast ctx ctx.macrocall [K"tuple"
        (i::K"Integer" for i in 1:N)...
    ]
end

src = raw"""
function gen(x::NTuple{N}) where {N}
    nongen_stuff = :nongen
    if @generated
        quote
            maybe_gen_stuff = ($N, $x)
        end
    else
        maybe_gen_stuff = :nongen_2
    end
    (nongen_stuff, maybe_gen_stuff)
end
"""

src = raw"""
begin
    function partially_gen(x::NTuple{N,T}) where {N,T}
        shared = :shared_stuff
        if @generated
            quote
                unshared = ($x, $N, $T)
            end
        else
            # Uuuum. How do we test both sides of this branch??
            unshared = :nongen # (typeof(x), N, T)
        end
        (shared, unshared)
    end

    partially_gen((1,2,3,4,5))
end
"""

src = """
let
    z = "z in outer ctx"
    @call_show z
end
"""

src = """
let
    x = "x in outer ctx"
    @call_oldstyle_macro x
end
"""

src = """
let
    z = "z in outer ctx"
    @call_info z
end
"""

ex = parsestmt(SyntaxTree, src, filename="foo.jl")
#ex = ensure_attributes(ex, var_id=Int)
#ex = softscope_test(ex)
@info "Input code" formatsrc(ex)

(ctx1, ex_macroexpand,
 ctx2, ex_desugar,
 ctx3, ex_scoped,
 ctx4, ex_converted,
 ctx5, ex_compiled,
 ex_expr, eval_result) = debug_lower(M, ex; verbose=true)

# Automatic test reduction
# bad = reduce_any_failing_toplevel(JuliaLowering, joinpath(@__DIR__, "../src/desugaring.jl"))
# if !isnothing(bad)
#     @error "Reduced expression as code" formatsrc(bad)
#     write("bad.jl", JuliaSyntaxFormatter.formatsrc(bad))
# end

# Old lowering
# text = read(joinpath(@__DIR__, "../src/desugaring.jl"), String)
# ex = parseall(SyntaxTree, text, filename="desugaring.jl")
# for e in Meta.parseall(text).args
#     Meta.lower(JuliaLowering, e)
# end
