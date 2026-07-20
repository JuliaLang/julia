test_mod = Module()

# Capture assigned before closure
@test JuliaLowering.include_string(test_mod, """
let
    x = 1
    f(y) = x+y
    f(2), f(3)
end
""") == (3,4)

# Capture assigned after closure
@test JuliaLowering.include_string(test_mod, """
let
    f(y) = x+y
    x = 1
    f(2)
end
""") == 3

# Capture assigned inside closure
@test JuliaLowering.include_string(test_mod, """
let
    x = 1
    function f(y)
        x = y
    end
    f(100)
    x
end
""") == 100

Base.eval(test_mod, :(call_it(f, args...) = f(args...)))

# Closure where an argument `x` is captured but not boxed
@test JuliaLowering.include_string(test_mod, """
begin
    function f_unboxed_test(x)
        z = 0
        function g()
            y = x  # x will not be boxed
            (y + 1, z)
        end
        z = 2 # will be boxed
        (x, g())
    end
    f_unboxed_test(10)
end
""") == (10,(11,2))

# Use of isdefined
@test JuliaLowering.include_string(test_mod, """
begin
    function f_isdefined(x)
        local w
        function g()
            z = 3
            (@isdefined(x), # unboxed, always defined capture
             @isdefined(y), # boxed capture
             @isdefined(z), # normal local var
             @isdefined(w)) # boxed undefined var
        end
        y = 2
        (@isdefined(y), @isdefined(w), g())
    end
    f_isdefined(1)
end
""") == (true, false, (true, true, true, false))

# Mutually recursive closures (closure capturing a closure)
@test JuliaLowering.include_string(test_mod, """
let
    function recursive_a(n)
        here = (:a, n)
        n <= 0 ? here  : (here, recursive_b(n-1))
    end
    function recursive_b(n)
        ((:b, n), recursive_a(n-1))
    end
    recursive_a(2)
end
""") == ((:a, 2), ((:b, 1), (:a, 0)))

# Global method capturing local variables
JuliaLowering.include_string(test_mod, """
begin
    local x = 1
    function f_global_method_capturing_local()
        x = x + 1
    end
end
""")
@test test_mod.f_global_method_capturing_local() == 2
@test test_mod.f_global_method_capturing_local() == 3

# quote interpolated AST
@test JuliaLowering.include_string(test_mod, """
let x = Symbol("foo"), xq = QuoteNode(x)
    global function f_global_method_capturing_sym()
        x, xq
    end
    f_global_method_capturing_sym()
end
""") == (:foo, QuoteNode(:foo))
@test JuliaLowering.include_string(test_mod, """
global dont_resolve = 1
let x = GlobalRef(@__MODULE__, :dont_resolve), xq = QuoteNode(x)
    global function f_global_method_capturing_gr()
        x, xq
    end
    f_global_method_capturing_gr()
end
""") == (GlobalRef(test_mod, :dont_resolve), QuoteNode(GlobalRef(test_mod, :dont_resolve)))

# Closure with multiple methods depending on local variables
f_closure_local_var_types = JuliaLowering.include_string(test_mod, """
let T=Int, S=Float64
    function f_closure_local_var_types(::T)
        1
    end
    function f_closure_local_var_types(::S)
        1.0
    end
end
""")
@test f_closure_local_var_types(2) == 1
@test f_closure_local_var_types(2.0) == 1.0
@test_throws MethodError f_closure_local_var_types("hi")

# Multiply nested closures. In this case g_nest needs to capture `x` in order
# to construct an instance of `h_nest()` inside it.
@test JuliaLowering.include_string(test_mod, """
begin
    function f_nest(x)
        function g_nest(y)
            function h_nest(z)
                (x,y,z)
            end
        end
    end

    f_nest(1)(2)(3)
end
""") === (1,2,3)

# Closure with return type must capture the return type
@test JuliaLowering.include_string(test_mod, """
let T = Int
    function f_captured_return_type()::T
        2.0
    end
    f_captured_return_type()
end
""") === 2

# Capturing a typed local
@test JuliaLowering.include_string(test_mod, """
let T = Int
    x::T = 1.0
    function f_captured_typed_local()
        x = 2.0
    end
    f_captured_typed_local()
    x
end
""") === 2

# Capturing a typed local where the type is a nontrivial expression
@test begin
    res = JuliaLowering.include_string(test_mod, """
    let T = Int, V=Vector
        x::V{T} = [1,2]
        function f_captured_typed_local_composite()
            x = [100.0, 200.0]
        end
        f_captured_typed_local_composite()
        x
    end
    """)
    res == [100, 200] && eltype(res) == Int
end

# Evil case where we mutate `T` which is the type of `x`, such that x is
# eventually set to a Float64.
#
# Completely dynamic types for variables should be disallowed somehow?? For
# example, by emitting the expression computing the type of `x` alongside the
# newvar node. However, for now we verify that this potentially evil behavior
# is compatible with the existing implementation :)
@test JuliaLowering.include_string(test_mod, """
let T = Int
    x::T = 1.0
    function f_captured_mutating_typed_local()
        x = 2
    end
    T = Float64
    f_captured_mutating_typed_local()
    x
end
""") === 2.0

# Anon function syntax
@test JuliaLowering.include_string(test_mod, """
begin
    local y = 2
    call_it(x->x+y, 3)
end
""") == 5

# Anon function syntax with `where`
@test JuliaLowering.include_string(test_mod, """
begin
    local y = 2
    call_it((x::T where {T<:Integer})->x+y, 3)
end
""") == 5

# Do block syntax
@test JuliaLowering.include_string(test_mod, """
begin
    local y = 2
    call_it(3) do x
        x + y
    end
end
""") == 5

# Attempt to reference capture which is not assigned
@test_throws UndefVarError(:x, :local) JuliaLowering.include_string(test_mod, """
let
    function f()
        x
    end
    f()
    x = 1
end
""")

# Opaque closure
@test JuliaLowering.include_string(test_mod, """
let y = 1
    oc = Base.Experimental.@opaque x->2x + y
    oc(3)
end
""") == 7

# Opaque closure with `...`
@test JuliaLowering.include_string(test_mod, """
let
    oc = Base.Experimental.@opaque (xs...)->xs
    oc(3,4,5)
end
""") == (3,4,5)

# Opaque closure inside a closure can capture the enclosing closure's captures
@test JuliaLowering.include_string(test_mod, """
let y = [1]
    outer = () -> begin
        inner = Base.Experimental.@opaque n -> n in y
        inner(1)
    end
    outer()
end
""") === true

# Nested opaque closure capture preserves boxed variable sharing
@test JuliaLowering.include_string(test_mod, """
let y = 1
    outer = () -> begin
        inner = Base.Experimental.@opaque () -> begin
            y = y + 1
        end
        inner()
    end
    outer()
    y
end
""") === 2

# Opaque closure nested in another opaque closure can capture the outer OC environment
@test JuliaLowering.include_string(test_mod, """
let y = [1]
    outer = Base.Experimental.@opaque () -> begin
        inner = Base.Experimental.@opaque n -> n in y
        inner(1)
    end
    outer()
end
""") === true

# Opaque closure type-bound expressions can capture enclosing closure captures
@test JuliaLowering.include_string(test_mod, """
let T = Tuple{Int}
    outer = () -> begin
        inner = Base.Experimental.@opaque T -> _ (n) -> n
        inner(1)
    end
    outer()
end
""") === 1
@test JuliaLowering.include_string(test_mod, """
let RT = Float64
    outer = () -> begin
        inner = Base.Experimental.@opaque _ -> RT () -> 1.0
        inner()
    end
    outer()
end
""") === 1.0

# OC in lambda
@test JuliaLowering.include_string(test_mod, """
(x->(y->(z->(Base.Experimental.@opaque ()->"opaque"))('z'))('y'))('x')()
""") == "opaque"
@test JuliaLowering.include_string(test_mod, """
(x->(y->(z->(Base.Experimental.@opaque ()->(x,y,z)))('z'))('y'))('x')()
""") == ('x','y','z')

# opaque_closure_method internals
method_ex = lower_str(test_mod, "Base.Experimental.@opaque x -> 2x").args[1].code[3]
@test method_ex.head === :opaque_closure_method
@test method_ex.args[1] === nothing
@test method_ex.args[4] isa LineNumberNode

# Argument reassigned in outer scope - no Box needed
@test JuliaLowering.include_string(test_mod, """
begin
    function f_arg_reassign(x)
        x = 1
        return ()->x
    end
    f_arg_reassign(100)()
end
""") == 1

# Argument reassigned in if-branch before capture - no Box needed (PR #60567 review)
@test JuliaLowering.include_string(test_mod, """
begin
    function f_arg_if_branch(x, cond)
        if cond
            x = 5
            return ()->x
        end
        return x
    end
    # When closure is returned, it captures the reassigned value
    f_arg_if_branch(100, true)()
end
""") == 5

@test JuliaLowering.include_string(test_mod, """
begin
    function f_arg_if_branch2(x, cond)
        if cond
            x = 5
            return ()->x
        end
        return x
    end
    # When original value is returned
    f_arg_if_branch2(100, false)
end
""") == 100

# Variable declared outside loop, assigned inside - needs Box (issue #37690)
@test JuliaLowering.include_string(test_mod, """
begin
    function f_loop_capture()
        local f
        local x
        for k = 1 : 2
            x = k
            if k == 1
                f = () -> x
            end
        end
        f()
    end
    f_loop_capture()
end
""") == 2

# Typed local declared outside loop, assigned inside - needs Box (issue #37690)
@test JuliaLowering.include_string(test_mod, """
begin
    function f_typed_local_loop()
        local f
        local x::Int
        for k = 1:2
            x = k
            if k == 1
                f = () -> x
            end
        end
        f()
    end
    f_typed_local_loop()
end
""") == 2

# Label can be jumped to, bypassing assignment - needs Box
@test JuliaLowering.include_string(test_mod, """
let
    @goto L
    y = 1
    @label L
    f = ()->y
    f.y
end
""") isa Core.Box

# Argument reassigned inside loop - needs Box (argument is implicitly declared outside loop) (issue #37690)
@test JuliaLowering.include_string(test_mod, """
begin
    function f_arg_loop(x)
        local f
        for i in 1:2
            x = i
            i == 1 && (f = ()->x;)
        end
        f()
    end
    f_arg_loop(0)
end
""") == 2

# Variable in while-true loop with break - needs Box (issue #37690)
let x = JuliaLowering.include_string(test_mod, """
    begin
        function f_break_loop()
            local f
            local x
            i = 1
            while true
                x = i
                if i == 1
                    f = ()->x
                end
                i >= 3 && break
                i += 1
            end
            f.x
        end
        f_break_loop()
    end
    """)
    @test x isa Core.Box
    @test x.contents == 3
end

# Variable in while-true loop with post-dominated capture (not captured in a branch) - no Box
let x = JuliaLowering.include_string(test_mod, """
    begin
        function f_break_loop2()
            local f
            local x
            i = 1
            while true
                x = i
                f = ()->x
                i >= 3 && break
                i += 1
            end
            f.x
        end
        f_break_loop2()
    end
    """)
    @test x isa Int
    @test x === 3
end

let keep = JuliaLowering.include_string(test_mod, """
    begin
        function f_for_after_capture(cond)
            if cond
                keep = Set{Base.PkgId}()
                return ()->keep
            end
            for x in 1:3; end
        end
        f_for_after_capture(true).keep
    end
    """)
    @test keep isa Set{Base.PkgId}
    @test keep == Set{Base.PkgId}()
end

# Function where arguments are captured into closure and assigned (boxed)
@test JuliaLowering.include_string(test_mod, """
begin
    function f_arg_captured_assigned(x)
        function g()
            x = 10
        end
        g()
        x
    end
    f_arg_captured_assigned(1)
end
""") == 10

# Closure declaration with no methods
@test JuliaLowering.include_string(test_mod, """
begin
    local no_method_f
    function no_method_f
    end
    no_method_f
end
""") isa Function

# Closure with keyword arguments
@test JuliaLowering.include_string(test_mod, """
let y = 10
    function f_kw_closure(; x=1)
        x + y
    end
    (f_kw_closure(), f_kw_closure(x=5))
end
""") == (11, 15)

# Adding kw methods to kw let-function
@test JuliaLowering.include_string(test_mod, """
let f(a; kw1 = nothing, kw2 = nothing) = "outer"
    f(::Integer; kwargs...) = "call me"
    f(1; kw1 = 1, kw2 = 2)
end
""") == "call me"

# Currently an error in both lowering implementations (closure-conversion ordering)
@test_broken JuliaLowering.include_string(test_mod, """
let f(a; kw1 = nothing, kw2 = nothing) = "outer"
    let
        f(::Integer; kwargs...) = error("call me")
    end
    f(1; kw1 = 1, kw2 = 2)
end
""") == "outer"

# Self-reference in let-function
@test JuliaLowering.include_string(test_mod, """
let f(x) = x <= 0 ? x : f(x-1)
    f(5)
end
""") == 0
@test JuliaLowering.include_string(test_mod, """
let f(x::typeof(f)) = x
    f(f)
end
""") isa Function # broken in flisp

# Self-reference in let-function default args
@test JuliaLowering.include_string(test_mod, """
let f(x=f) = x
    f()
end
""") isa Function
@test JuliaLowering.include_string(test_mod, """
let f(x::typeof(f)) = x
    f(f)
end
""") isa Function
@test JuliaLowering.include_string(test_mod, """
let f(;x=f) = x
    f()
end
""") isa Function
@test JuliaLowering.include_string(test_mod, """
let f(;x::typeof(f)) = x
    f(x=f)
end
""") isa Function

# Anonymous function syntax with `function`
@test JuliaLowering.include_string(test_mod, """
begin
    local y = 2
    call_it(function (x) x + y end, 3)
end
""") == 5

# Closure where static parameter is captured
@test JuliaLowering.include_string(test_mod, """
begin
    function f_static_param_capture(::T) where T
        function g()
            T
        end
        g()
    end
    f_static_param_capture(1)
end
""") == Int

# Closure with static parameter that may be undefined
JuliaLowering.include_string(test_mod, """
function f_undef_static_param(x::Union{T,Nothing}) where T
    function inner()
        return T
    end
    inner
end
""")
@test_throws UndefVarError test_mod.f_undef_static_param(nothing)()
@test test_mod.f_undef_static_param(42)() == Int

@test JuliaLowering.include_string(test_mod, """
begin
    function f_inner_sp(x::T) where T
        function inner(y::U) where U
            (T, U)
        end
        (T, inner("foo"))
    end
    f_inner_sp(1)
end
""") == (Int, (Int, String))

@test JuliaLowering.include_string(test_mod, """
begin
    function f_complex_arg_sp(a)
        function inner(x::(let z = T; Vector{z} end)) where {T <: Integer}
            T, typeof(x), @isdefined(z)
        end
        inner(a)
    end
    f_complex_arg_sp([1,2,3])
end
""") == (Int, Vector{Int}, false)

@test JuliaLowering.include_string(test_mod, """
begin
    function f_inner_rt_sp(a)
        function inner(x::T)::Tuple{T, Vector{T}} where T
            (x,T[x])
        end
        inner(a)
    end
    f_inner_rt_sp(1), f_inner_rt_sp("foo")
end
""") == ((1, [1]), ("foo", ["foo"]))

@test JuliaLowering.include_string(test_mod, """
begin
    function f_many_closure_sp()
        function (); function (); function (x::T) where T; (x, T) end; end; end
    end
    f_many_closure_sp()()()(1)
end
""") == (1, Int)

@test JuliaLowering.include_string(test_mod, """
begin
    function f_many_closure_sp_capt(x::T) where T
        function (); function (); function (); (x, T) end; end; end
    end
    f_many_closure_sp_capt(1)()()()
end
""") == (1, Int)

@test JuliaLowering.include_string(test_mod, """
begin
    function f_argcapt_sp(x::T) where T
        (inner_x::T)->(x, inner_x, T)
    end
    f_argcapt_sp(1)(2)
end
""") == (1, 2, Int)

# Inner method typevar `U` depending on a static parameter `T`: the hoisted
# method def for `inner` captures `T` as a closure type parameter, making
# `inner` parametric on `T`.  This doesn't work in flisp (UndefVarError).
@test JuliaLowering.include_string(test_mod, """
begin
    function f_typevarcapt_sp(x::T) where T
        function inner(y::U) where {U<:T}
            (x,y,T,U)
        end
    end
    f_typevarcapt_sp(1)(2)
end
""") == (1,2,Int,Int)

# https://github.com/JuliaLang/JuliaLowering.jl/issues/134#issuecomment-3739626003
JuliaLowering.include_string(test_mod, """
function f_update_outer_capture()
    local response # declare outside closure
    f = ()->begin
        response = 1
    end
    f()
    return (f, response)
end
""")
let (f, response) = test_mod.f_update_outer_capture()
    @test f.response isa Core.Box
    @test response == 1
end

# https://github.com/JuliaLang/JuliaLowering.jl/issues/147
JuliaLowering.include_string(test_mod, """
function f_box_regression147()
    function foo()
        return true
    end
    return (()->foo, foo)
end
""")
let (f, foo) = test_mod.f_box_regression147()
    @test !(f.foo isa Core.Box)
    @test f.foo === foo
end

# The internal "helper" of an (inner) kwargs function should not be boxed.
JuliaLowering.include_string(test_mod, """
function f_kwbody_box()
    function inner(x; verbose=false)
        return verbose ? x : nothing
    end
    return (inner, inner(1; verbose=true))
end
""")
let (inner, result) = test_mod.f_kwbody_box()
    @test result == 1
    # The kw body closure should be captured directly, not through a Box
    kw_body_field = only(filter(f -> startswith(string(f), "#kw_body#"), fieldnames(typeof(inner))))
    @test !(getfield(inner, kw_body_field) isa Core.Box)
end

# Any `let` variables marked always-defined && assigned-once are known to
# dominate their scope, so they should not be boxed even in the presence
# of `@label`
JuliaLowering.include_string(test_mod, """
function f_let_capture_with_label()
    for x in [1,2,3]
        let x = x
            if false
                @goto done
                @label done # force the binding analysis to give up
            else
                return (() -> x,)
            end
        end
    end
end
""")
let (f,) = test_mod.f_let_capture_with_label()
    @test !(f.x isa Core.Box)
    @test f.x == 1
end

JuliaLowering.include_string(test_mod, """
function f_arg_reassign_with_label(x)
    g() = x
    if false
        @goto done
        @label done
    end
    x = 1
    return (g, x)
end
""")
let (g, x) = test_mod.f_arg_reassign_with_label(42)
    @test g.x isa Core.Box
    @test g() == 1
    @test x == 1
end

@test JuliaLowering.include_string(test_mod, """
func_in_own_sig(x::typeof(func_in_own_sig)) = (x, 1)
""") isa Function
@test JuliaLowering.include_string(test_mod, """
func_in_own_sig(func_in_own_sig)
""") == (test_mod.func_in_own_sig, 1)
@test JuliaLowering.include_string(test_mod, """
function func_in_own_sp(x::T) where {T<:typeof(func_in_own_sp)}
(x, T)
end
""") isa Function
@test JuliaLowering.include_string(test_mod, """
func_in_own_sp(func_in_own_sp)
""") == (test_mod.func_in_own_sp, typeof(test_mod.func_in_own_sp))

@testset "(AI) Captured static parameters" begin
    # Captured static parameter used in a closure signature: dispatch must pin
    # `T` to the value captured in the closure's type parameter, not re-derive
    # it from the argument.
    JuliaLowering.include_string(test_mod, """
    function f_sigcapt_sp(x::T) where T
        g_sigcapt(y::T) = (y, T)
        g_sigcapt
    end
    """)
    @test test_mod.f_sigcapt_sp(1)(2) == (2, Int)
    @test_throws MethodError test_mod.f_sigcapt_sp(1)(2.5)

    # All methods of a closure share its captured static parameters, even
    # methods that don't reference them.
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_multimeth_sp(x::T) where T
            g() = T
            g(y) = (y, T)
            g
        end
        (f_multimeth_sp(1.5)(), f_multimeth_sp(1.5)(2))
    end
    """) == (Float64, (2, Float64))

    # Static parameter captured by an opaque closure (as a field, since opaque
    # closures have no closure type to parameterize)
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_oc_sp(x::T) where T
            Base.Experimental.@opaque y -> (x, y, T)
        end
        f_oc_sp(1)(2)
    end
    """) == (1, 2, Int)

    # A captured sparam's typevar bound may reference another sparam the closure
    # doesn't mention at all: `S` must be captured transitively or the hoisted
    # method signature contains a free TypeVar.  (flisp instead drops the bound.)
    JuliaLowering.include_string(test_mod, """
    function f_captsp_bound_dep(x::S, y::T) where {S, T<:AbstractVector{S}}
        g_bound_dep(z::T) = (z, T)
        g_bound_dep
    end
    """)
    let g = test_mod.f_captsp_bound_dep(1, [1, 2])
        @test g([3, 4]) == ([3, 4], Vector{Int})
        @test_throws MethodError g("nope")
        @test_throws MethodError g(Any[1, 2]) # T is pinned to Vector{Int}
    end

    # As above, but the dependency `S` is also captured normally by the body
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_captsp_bound_dep2(x::S, y::T) where {S, T<:AbstractVector{S}}
            g_bound_dep2(z::T) = (z, T, S)
            g_bound_dep2
        end
        f_captsp_bound_dep2(1, [1, 2])([3, 4])
    end
    """) == ([3, 4], Vector{Int}, Int)

    # Same, via a lower bound
    JuliaLowering.include_string(test_mod, """
    function f_captsp_lb_dep(x::T, ys::Vector{S}) where {T, S>:T}
        g_lb_dep(z::S) = (z, S)
        g_lb_dep
    end
    """)
    @test test_mod.f_captsp_lb_dep(1, Any[1.0])("anything") == ("anything", Any)

    # (broken in flisp, JL may or may not have the desired behaviour) Typevar
    # bound referencing a static parameter two closure levels up: `g` must
    # capture `T` in passing so its value reaches `h`'s creation site.
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_typevarcapt_sp_deep(::T) where T
            function g_deep()
                h_deep(y::U) where {U<:T} = (y, T, U)
                h_deep
            end
            g_deep
        end
        f_typevarcapt_sp_deep(1.5)()(2.0)
    end
    """) == (2.0, Float64, Float64)

    # Runtime state of enclosing functions can't be used in hoisted method
    # signatures or typevar bounds (flisp errors identically).
    @test_throws LoweringError JuliaLowering.include_string(test_mod, """
    function f_local_in_closure_sig(x)
        g(y::typeof(x)) = y
        g
    end
    """)
    @test_throws LoweringError JuliaLowering.include_string(test_mod, """
    function f_local_in_closure_spbound(x)
        g(y::T) where {T<:typeof(x)} = y
        g
    end
    """)

    # Global method definitions can't be nested inside functions (flisp errors
    # identically).
    @test_throws LoweringError JuliaLowering.include_string(test_mod, """
    function f_nested_global_methdef()
        global g_nested_global_methdef
        g_nested_global_methdef(x::T) where T = x
    end
    """)

    #-------------------------------------------------------------------------------
    # Static parameter capture: combinations with other closure features

    # do-block closure capturing a static parameter
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_do_sp(y::T) where T
            call_it(3) do x
                (x, y, T)
            end
        end
        f_do_sp(1.5)
    end
    """) == (3, 1.5, Float64)

    # comprehensions and generators capturing a static parameter
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_generator_sp(::T) where T
            ([T for _ in 1:2], first(T for _ in 1:1))
        end
        f_generator_sp(1)
    end
    """) == ([Int, Int], Int)

    # Closure defined inside an opaque closure, capturing a static parameter
    # through it.  This is broken in flisp ("Found raw symbol T in code returned
    # from lowering").
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_closure_in_oc_sp(x::T) where T
            Base.Experimental.@opaque () -> begin
                g() = (x, T)
                g()
            end
        end
        f_closure_in_oc_sp(1)()
    end
    """) == (1, Int)

    # Opaque closure nested inside a regular closure, capturing a static parameter
    # through it
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_oc_in_closure_sp(x::T) where T
            function mid()
                Base.Experimental.@opaque () -> (x, T)
            end
            mid
        end
        f_oc_in_closure_sp(2)()()
    end
    """) == (2, Int)

    # Local function overloaded with keyword and plain methods; the captured
    # static parameter appears only in a keyword default, which is evaluated in
    # the kwcall method, while the body method's signature mentions the closure's
    # type unparameterized.
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_kw_overload_sp(x::T) where T
            g(y; k=T) = (:kw, y, k)
            g(y::Int) = (:plain, y)
            (g(1), g(1.5), g(1.5; k=2))
        end
        f_kw_overload_sp(1)
    end
    """) == ((:plain, 1), (:kw, 1.5, Int), (:kw, 1.5, 2))

    # Keyword closure with the captured sparam in both signature and kw default:
    # dispatch through the kw sorter still pins `T`.
    JuliaLowering.include_string(test_mod, """
    function f_kwsig_sp(x::T) where T
        g_kwsig(y::T; k=T) = (y, k, T)
        g_kwsig
    end
    """)
    let g = test_mod.f_kwsig_sp(1)
        @test g(2) == (2, Int, Int)
        @test g(2; k=Int8) == (2, Int8, Int)
        @test_throws MethodError g(2.5)
    end

    # Self-recursive closure capturing a static parameter
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_recursive_sp(x::T) where T
            g(n) = n <= 0 ? T : g(n - 1)
            g(3)
        end
        f_recursive_sp(1.5)
    end
    """) == Float64

    # Boxed (assigned) captured local and captured static parameter in one closure
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_box_plus_sp(x::T) where T
            c = 0
            g() = (c += 1; (c, T))
            (g(), g())
        end
        f_box_plus_sp(1im)
    end
    """) == ((1, Complex{Int}), (2, Complex{Int}))

    # @isdefined of a captured static parameter inside a closure; an undefined
    # sparam throws at closure creation (as in flisp), not at the @isdefined
    JuliaLowering.include_string(test_mod, """
    function f_isdefined_sp(x::Union{T,Nothing}) where T
        g_isdefined_sp() = @isdefined(T)
        g_isdefined_sp
    end
    """)
    @test test_mod.f_isdefined_sp(1)()
    @test_throws UndefVarError test_mod.f_isdefined_sp(nothing)

    # Closure with an anonymous static parameter of its own plus a captured one
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_anon_sp_closure(x::T) where T
            g(y) where _ = (y, T)
            g(2)
        end
        f_anon_sp_closure(1)
    end
    """) == (2, Int)

    # `let` inside a closure signature referencing a captured static parameter
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_let_sig_sp(::T) where T
            g(x::(let v = T; Vector{v} end)) = x
            g
        end
        f_let_sig_sp(1)([1, 2])
    end
    """) == [1, 2]

    # Undetermined static parameter throws at closure creation (flisp parity)
    @test_throws UndefVarError JuliaLowering.include_string(test_mod, """
    begin
        function f_undet_sp_creation(x::T, y::S) where {T, S>:T}
            g(z::S) = z
            g
        end
        f_undet_sp_creation(1, 2.5)
    end
    """)

    # Signature capture through four levels of nested closures
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_sig_capture_depth4(x::T) where T
            function a()
                function b()
                    function c()
                        d(y::T) = (y, T)
                        d(x)
                    end
                    c()
                end
                b()
            end
            a()
        end
        f_sig_capture_depth4(42)
    end
    """) == (42, Int)

    # The closure's own sparam shadows the captured one, with the outer as bound
    JuliaLowering.include_string(test_mod, """
    function f_shadow_sp_bound(x::T) where T
        g_shadow(y::T) where {T<:T} = (y, T)
        g_shadow
    end
    """)
    let g = test_mod.f_shadow_sp_bound(1)
        @test g(2) == (2, Int)
        @test_throws MethodError g(2.5) # inner T <: outer T == Int
    end

    # Vararg length pinned by a captured static parameter
    JuliaLowering.include_string(test_mod, """
    function f_vararg_n_sp(::Val{N}) where N
        g_vararg_n(xs::Vararg{Int,N}) = xs
        g_vararg_n
    end
    """)
    let g = test_mod.f_vararg_n_sp(Val(2))
        @test g(1, 2) == (1, 2)
        @test_throws MethodError g(1, 2, 3) # N is pinned to 2
    end

    # Return-type annotation is the only use of the captured static parameter
    JuliaLowering.include_string(test_mod, """
    function f_rett_only_sp(x::T) where T
        g_rett(y)::T = y
        g_rett
    end
    """)
    @test test_mod.f_rett_only_sp(1)(2) === 2
    @test_throws MethodError test_mod.f_rett_only_sp(1)("s") # convert(Int, "s")

    # Two sibling closures capturing the same static parameter (and sharing its
    # typevar object in their hoisted method signatures)
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_two_closures_sp(x::T) where T
            g(y::T) = (y, :g)
            h(y::T) = (y, :h)
            (g(x), h(x))
        end
        f_two_closures_sp(2)
    end
    """) == ((2, :g), (2, :h))

    # A closure which captures nothing must be creatable even when the enclosing
    # method's sparams are undetermined: closures must capture only the sparams
    # they (transitively) reference, not all lexically enclosing ones.
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_nocapt_undet_sp(x::Union{T,Nothing}) where T
            g_nocapt() = 1
            g_nocapt
        end
        f_nocapt_undet_sp(nothing)()
    end
    """) == 1

    # The static parameter's owner is itself a nested closure: its hoisted typevar
    # assignments must be emitted before the inner closure's hoisted methods,
    # which reference them (issue found as a segfault from a forward SSA ref).
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_nested_owner_body()
            function g(x::T) where T
                () -> T
            end
            g
        end
        f_nested_owner_body()(1)()
    end
    """) == Int

    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_nested_owner_sig()
            function g(x::T) where T
                h(y::T) = (y, T)
                h(x)
            end
            g
        end
        f_nested_owner_sig()(2.5)
    end
    """) == (2.5, Float64)

    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_nested_owner_bound()
            function g(x::T) where T
                h(y::U) where {U<:T} = (y, T, U)
                h(x)
            end
            g
        end
        f_nested_owner_bound()(3)
    end
    """) == (3, Int, Int)

    # Sparams owned by two different nesting levels, captured together
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_two_level_sps(a::A) where A
            function g(x::T) where T
                () -> (A, T)
            end
            g
        end
        f_two_level_sps(1im)(2.5)()
    end
    """) == (Complex{Int}, Float64)

    # method_defs in value position (not a top-level sequence point), with a
    # nested closure capturing the sparam
    @test JuliaLowering.include_string(test_mod, """
    begin
        x_value_pos_sp = (f_value_pos_sp(y::T) where T = () -> T)
        f_value_pos_sp(42)()
    end
    """) == Int

    # CC must not lift typevar T above assignment to x
    @test JuliaLowering.include_string(test_mod, """
    for x in (Int, Float64)
        global f_local_in_tvbounds
        f_local_in_tvbounds(y::T) where {T<:x} = T
    end
    """) == nothing
    @test JuliaLowering.include_string(test_mod, """
    f_local_in_tvbounds(1)
    """) == Int
    @test JuliaLowering.include_string(test_mod, """
    f_local_in_tvbounds(1.0)
    """) == Float64

    @test JuliaLowering.include_string(test_mod, """
    global g_tvbound = Int
    for i in 1:2
        global f_global_in_tvbounds, g_tvbound
        f_global_in_tvbounds(y::T) where {T<:g_tvbound} = (i, T)
        g_tvbound = Float64
    end
    f_global_in_tvbounds(1), f_global_in_tvbounds(1.5)
    """) == ((1, Int), (2, Float64))
end

# questionable test: g_shadowed_by_sparam is not an sparam of the single-arg
# version of `f`, so capture into the single-arg method's body resolves to the
# outer typevar instead of an sparam, which scope resolution re-resolves in
# global scope (typevars are only visible in the same lambda).  However, it
# would probably make more sense to keep the sparam in both methods, and have
# the inner lambda's sig refer to the sparam instead of the global.
@test JuliaLowering.include_string(test_mod, """
global g_shadowed_by_sparam = Int
function f_sp_in_sig_in_lam_in_optarg(
        x, y=((z::g_shadowed_by_sparam)->z)) where g_shadowed_by_sparam
    y
end
f_sp_in_sig_in_lam_in_optarg(1.)(2)
""") == 2

@testset "sparam captured into static_eval" begin
    @test JuliaLowering.include_string(test_mod, """
    function f_capt_sp_in_ccall_rett(v::Vector{T}) where {T}
        g = () -> ccall(:memset, Ptr{T}, (Ptr{Cvoid}, Cint, Csize_t), v, 0, 0)
        g() isa Ptr
    end
    f_capt_sp_in_ccall_rett([1, 2])
    """) == true
    @test JuliaLowering.include_string(test_mod, """
    function f_capt_sp_in_ccall_argtype(v::Vector{T}) where {T}
        g = () -> ccall(:memset, Ptr{Cvoid}, (Ptr{T}, Cint, Csize_t), v, 0, 0)
        g() isa Ptr
    end
    f_capt_sp_in_ccall_argtype([1, 2])
    """) == true
    @test_throws LoweringError JuliaLowering.include_string(test_mod, """
    function f_local_in_ccall_in_closure(v)
        T = typeof(v)
        g = () -> ccall(:memset, Ptr{T}, (Ptr{Cvoid}, Cint, Csize_t), v, 0, 0)
        g()
    end
    """)
end

# A function definition is treated as an assignment to a name
@testset "inner functions sharing names" begin
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_midfunc_redefines_inner(exec_mid)
            function inner(); 1; end
            function mid()
                function inner(); 2; end
            end
            exec_mid && mid()
            inner
        end
    (f_midfunc_redefines_inner(false)(), f_midfunc_redefines_inner(true)())
    end
    """) == (1,2)

    # Inner methods in control flow are known to be buggy/discouraged
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_if_redefines_inner(cond)
            function inner(); 1; end
            if cond
                function inner(); 2; end
            end
            inner
        end
    (f_if_redefines_inner(false)(), f_if_redefines_inner(true)())
    end
    """) == (2,2)

    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_let_redefines_inner()
            local a,b
            a = function inner(); 1; end
            let
                b = function inner(); 2; end
            end
            a(), b(), inner()
        end
    f_let_redefines_inner()
    end
    """) == (2,2,2)

    # like normal locals, reassign doesn't happen if you add a local decl
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_midfunc_redefines_inner(exec_mid)
            function inner(); 1; end
            function mid()
                local inner
                function inner(); 2; end
            end
            exec_mid && mid()
            inner
        end
    (f_midfunc_redefines_inner(false)(), f_midfunc_redefines_inner(true)())
    end
    """) == (1,1)
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_let_local_shadows_inner()
            local a,b
            a = function inner(); 1; end
            let
                local inner
                b = function inner(); 2; end
            end
            a(), b(), inner()
        end
    f_let_local_shadows_inner()
    end
    """) == (1,2,1)

    @test JuliaLowering.include_string(test_mod, """
    begin
        function multimethod_inner(arg)
            function inner(x::Int); x; end
            function inner(x::Float64); x; end
            (length(methods(inner)), inner(arg))
        end
        multimethod_inner(1), multimethod_inner(1.0)
    end
    """) == ((2, 1), (2, 1.0))
    @test JuliaLowering.include_string(test_mod, """
    begin
        function multimethod_let_inner(arg)
            function inner(x::Int); x; end
            let
                function inner(x::Float64); x; end
            end
            (length(methods(inner)), inner(arg))
        end
        multimethod_let_inner(1), multimethod_let_inner(1.0)
    end
    """) == ((2, 1), (2, 1.0))
    # reassign does not add method
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_mid_does_not_add_method(exec_mid)
            function inner(x::Int); x; end
            function mid()
                function inner(x::Float64); x; end
            end
            exec_mid && mid()
            (length(methods(inner)), inner(exec_mid ? 123.0 : 123))
        end
    (f_mid_does_not_add_method(false), f_mid_does_not_add_method(true))
    end
    """) == ((1, 123), (1, 123.0))

    # Closure info is keyed on (binding_id, lambda_id), so a name reassigned inside a
    # *nested* function is a genuinely distinct closure type.  The cases above use
    # non-capturing closures; these check that distinct types with *different capture
    # sets* are kept separate (the case where a merged type would be most wrong).
    @testset "(AI) inner functions with differing captures across lambdas" begin
        # `inner` (in `f`) captures only `a`; the redefinition (in `mid`) captures
        # both `a` and `b`.  `v1` observes the first type, `v2`/`inner` the second.
        @test JuliaLowering.include_string(test_mod, """
        begin
            function f_diffcap(a, b)
                inner() = a
                v1 = inner
                function mid()
                    inner() = a + b
                end
                mid()
                v2 = inner
                (v1(), v2())
            end
            f_diffcap(1, 10)
        end
        """) == (1, 11)

        # The two definitions really are separate closure *types*.
        @test JuliaLowering.include_string(test_mod, """
        begin
            function f_diffcap_types(a, b)
                inner() = a
                t1 = typeof(inner)
                function mid()
                    inner() = a + b
                end
                mid()
                t1 === typeof(inner)
            end
            f_diffcap_types(1, 10)
        end
        """) == false

        # Static-parameter captures are also keyed per (name, lambda): `g` in `f`
        # captures only `T`, while the redefinition in `mid` captures both `T` and
        # `S`.  A shared closure type would merge these capture sets.
        @test JuliaLowering.include_string(test_mod, """
        begin
            function f_sp_redef(::Type{T}) where {T}
                g() = T
                v1 = g
                function mid(::Type{S}) where {S}
                    g() = (T, S)
                end
                mid(Float64)
                (v1(), g())
            end
            f_sp_redef(Int)
        end
        """) == (Int, (Int, Float64))

        @test JuliaLowering.include_string(test_mod, """
        let
            function closure2(::Type{T}) where {T}
                function f()
                    T
                end
                f
            end
            closure2(Int)()

            x2 = 2
            function closure3(a)
                b = "whatever"
                f(c) = (a, b, c)
                b = x2
                return f
            end
            f = closure3(1)
            f(3)
        end
        """) == (1,2,3)
    end
end

@testset "captured type declarations" begin
   JuliaLowering.include_string(test_mod, """
   function f_boxed_typed_capture(v, k)
       if k == -1
           k::Int = maximum(v)
       end
       findall(x -> x >= k, v)
   end
   """)
   @test test_mod.f_boxed_typed_capture([3, 1, 2], -1) == [1]
   @test only(Base.return_types(test_mod.f_boxed_typed_capture, (Vector{Int}, Int))) ===
       Vector{Int}

   # declared types may reference locals in the outer function
   JuliaLowering.include_string(test_mod, """
   function f_boxed_sparam_typed(v::Vector{T}, k) where T
       if k == -1
           k::T = maximum(v)
       end
       findall(x -> x >= k, v)
   end
   function f_boxed_localvar_typed(v, k)
       T = Int
       if k == -1
           k::T = maximum(v)
       end
       findall(x -> x >= k, v)
   end
   """)
   @test test_mod.f_boxed_sparam_typed([3, 1, 2], -1) == [1]
   @test only(Base.return_types(test_mod.f_boxed_sparam_typed, (Vector{Int}, Int))) ===
       Vector{Int}
   @test test_mod.f_boxed_localvar_typed([3, 1, 2], -1) == [1]
   @test only(Base.return_types(test_mod.f_boxed_localvar_typed, (Vector{Int}, Int))) ===
       Vector{Int}
end
