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

end
