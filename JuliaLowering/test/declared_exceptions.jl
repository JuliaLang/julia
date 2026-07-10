# Declared exceptions (`except`, postfix `?`) and the `match` statement

test_mod = Module()

@testset "declared exception basics" begin

JuliaLowering.include_string(test_mod, """
struct DeclExcA <: Exception
    msg::String
end

function declexc_f(b) except DeclExcA
    b && throw(DeclExcA("boom"))?
    return 42
end
""")

@test JuliaLowering.include_string(test_mod, "declexc_f(false)") == 42
@test JuliaLowering.include_string(test_mod, """
Base.except_iserr(Base.except_call(declexc_f, true)::Base.Except)
""")
@test JuliaLowering.include_string(test_mod, """
try; declexc_f(true); "nothrow"; catch e; e isa DeclExcA ? "threw" : "wrong"; end
""") == "threw"

# Inferred `except Any` declaration for bare `?`
@test JuliaLowering.include_string(test_mod, """
declexc_g(b) = declexc_f(b)?
Base.except_iserr(Base.except_call(declexc_g, true))
""")

# Callsite filter
@test JuliaLowering.include_string(test_mod, """
function declexc_h(b) except DeclExcA
    declexc_f(b) except DeclExcA
end
Base.except_iserr(Base.except_call(declexc_h, true))
""")

# `?` on a non-call expression tests the value itself: Except values are
# unwrapped or propagated, any other value passes through unchanged
@test JuliaLowering.include_string(test_mod, """
function declexc_var(b) except DeclExcA
    r = Base.except_call(declexc_f, b)
    r?
end
(declexc_var(false), Base.except_iserr(Base.except_call(declexc_var, true)))
""") == (42, true)

end

@testset "declared exceptions and closures" begin

# Closures get their own inferred declaration; except_call dispatches to the
# closure's inner method
@test JuliaLowering.include_string(test_mod, """
let clos = x -> (x < 0 && throw(DeclExcA("neg"))?; x + 1)
    (clos(1), Base.except_iserr(Base.except_call(clos, -1)))
end
""") == (2, true)

# Optional, keyword and vararg arguments
@test JuliaLowering.include_string(test_mod, """
function declexc_kw(x, y=2; scale=1) except DeclExcA
    x < 0 && throw(DeclExcA("neg"))?
    (x + y) * scale
end
(declexc_kw(1), declexc_kw(1, 3; scale=10),
 Base.except_iserr(Base.except_call(declexc_kw, -1)),
 Base.except_result(Base.except_call(declexc_kw, 1; scale=5)::Base.Except))
""") == (3, 40, true, 15)

end

@testset "declared exceptions in comprehensions and generators" begin

JuliaLowering.include_string(test_mod, """
function declexc_incr(x) except DeclExcA
    x < 0 ? throw(DeclExcA("neg"))? : x + 1
end
""")

# `?` in a literal comprehension propagates from the comprehension
@test JuliaLowering.include_string(test_mod, """
function declexc_comp(xs) except DeclExcA
    [declexc_incr(x)? for x in xs]
end
(declexc_comp([1, 2]), Base.except_iserr(Base.except_call(declexc_comp, [1, -1])))
""") == ([2, 3], true)

# Lazy generators do not propagate: the declared exception is thrown
@test JuliaLowering.include_string(test_mod, """
function declexc_lazy(xs) except DeclExcA
    collect((declexc_incr(x)? for x in xs))
end
try; declexc_lazy([-1]); "nothrow"; catch e; e isa DeclExcA ? "threw" : "wrong"; end
""") == "threw"

end

@testset "match statement" begin

# The match expansion exercises anonymous @label blocks with valued
# breaks (including variables as break values) and bare break
@test JuliaLowering.include_string(test_mod, """
match (1, 2)
case (a, b) if a > b
    "dec"
case (a, b)
    a + b
end
""") == 3

@test JuliaLowering.include_string(test_mod, """
match 1
case 1
    break
    error("unreached")
end
""") === nothing

@test JuliaLowering.include_string(test_mod, """
match 1
case 1
    break _ 99
end
""") == 99

# case except arms match the declared-exception channel
@test JuliaLowering.include_string(test_mod, """
match declexc_f(true)
case except DeclExcA(msg)
    msg
case v
    v
end
""") == "boom"

# Multiple except arms dispatch on the exception type; `as` binds the
# payload on the exceptional path
JuliaLowering.include_string(test_mod, """
struct DeclExcB <: Exception end
function declexc_multi(sel) except Union{DeclExcA, DeclExcB}
    sel == 1 && throw(DeclExcA("a"))?
    sel == 2 && throw(DeclExcB())?
    "value"
end
""")
@test JuliaLowering.include_string(test_mod, """
map([1, 2, 0]) do sel
    match declexc_multi(sel) as it
    case except DeclExcA(m)
        "A:" * m
    case except ::DeclExcB
        "B"
    case _
        it
    end
end
""") == ["A:a", "B", "value"]

# Keyword arguments in the scrutinee call
@test JuliaLowering.include_string(test_mod, """
function declexc_kwf(x; fail=false) except DeclExcA
    fail && throw(DeclExcA("kw"))?
    x + 1
end
(match declexc_kwf(1; fail=true)
 case except DeclExcA(m)
     m
 case _
 end,
 match declexc_kwf(1)
 case except DeclExcA(m)
     m
 case _
 end)
""") == ("kw", 2)

# A pass-through value that is itself an exceptional-state Except object
# is not confused with the exceptional channel
@test JuliaLowering.include_string(test_mod, """
declexc_data() = Base.except_error(DeclExcA("data"))
r = match declexc_data()
case except ::DeclExcA
    "exc"
end
r isa Base.Except && Base.except_iserr(r) && Base.except_exception(r).msg == "data"
""")

end

@testset "break with value scope resolution" begin

# Variables (not just literals) as labeled break values
@test JuliaLowering.include_string(test_mod, """
@label begin
    let
        local t = 1
        break _ t
    end
    0
end
""") == 1

@test JuliaLowering.include_string(test_mod, """
@label myblock begin
    let v = 21
        break myblock 2v
    end
    0
end
""") == 42

end
