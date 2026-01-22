@testset "Closures" begin

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

# Closure where a local `x` is captured but not boxed
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

end
