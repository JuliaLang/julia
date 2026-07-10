# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for declared exceptions

struct ExcTestA <: Exception
    msg::String
end
struct ExcTestB <: Exception end
struct ExcTestC <: Exception end

# Raising a declared exception; unhandled declared exceptions are thrown
function exc_basic() except ExcTestA
    throw(ExcTestA("boom"))?
end
@test_throws ExcTestA exc_basic()

# except_call returns the declared exception as a value
let r = Base.except_call(exc_basic)
    @test r isa Base.Except
    @test Base.except_iserr(r)
    @test Base.except_exception(r) isa ExcTestA
    @test Base.except_exception(r).msg == "boom"
end

# Except is a single type carrying both states: the exceptional state has
# no value (T === Union{}) and ordinary results carry no exception
@test Base.except_value(1) isa Base.Except{Int, Union{}}
@test Base.except_error(ExcTestA("x")) isa Base.Except{Union{}, ExcTestA}
@test !Base.except_iserr(Base.except_value(1))
@test Base.except_iserr(Base.except_error(ExcTestA("x")))
@test Base.except_result(Base.except_value(1)) === 1
@test Base.except_exception(Base.except_error(ExcTestA("x"))).msg == "x"
# except_iserr/except_result are total: ordinary values pass through
@test !Base.except_iserr(42)
@test Base.except_result(42) === 42

# unwrap_except extracts both states of an Except
@test Base.unwrap_except(Base.except_value(1)) == 1
@test_throws ExcTestA Base.unwrap_except(Base.except_error(ExcTestA("x")))

# The public constructors: the ordinary state converts to T like a return
# type annotation; the exceptional state is tagged with `throw`
@test Base.unwrap_except(Base.Except{Float64, ExcTestA}(1)) === 1.0
@test_throws ExcTestA Base.unwrap_except(Base.Except{Int, ExcTestA}(throw, ExcTestA("x")))

# show round-trips through the constructors
@test repr(Base.except_value(3)) == "Base.Except{$Int, Union{}}(3)"
@test repr(Base.except_error(DivideError())) == "Base.Except{Union{}, DivideError}(throw, DivideError())"

# except_call on a function without declared exceptions performs an
# ordinary call and wraps the result
@test Base.except_call(+, 1, 2) === Base.except_value(3)
@test Base.except_call(+, 1, 2) isa Base.Except

# ? propagates declared exceptions matching the caller's declaration
function exc_chain() except ExcTestA
    exc_basic()?
    error("unreached")
end
let r = Base.except_call(exc_chain)
    @test Base.except_iserr(r) && Base.except_exception(r) isa ExcTestA
end
@test_throws ExcTestA exc_chain()

# ? throws declared exceptions that don't fit the caller's declaration
function exc_mismatch() except ExcTestB
    exc_basic()?
end
@test_throws ExcTestA exc_mismatch()

# The value path is unaffected
function exc_value(b) except ExcTestA
    if b
        throw(ExcTestA("x"))?
    end
    "value"
end
@test exc_value(false) == "value"
@test Base.except_result(Base.except_call(exc_value, false)::Base.Except) == "value"

# Both states live inside the one Except type, so a function whose
# ordinary result is itself an Except object is unambiguous
exc_data() = Base.except_error(ExcTestA("data"))
let r = Base.except_call(exc_data)
    @test r isa Base.Except && !Base.except_iserr(r)
    @test Base.except_iserr(Base.except_result(r))
end
function exc_use_data() except ExcTestA
    d = exc_data()?
    d isa Base.Except && Base.except_iserr(d)
end
@test exc_use_data()

# Callsite filter form
function exc_filter() except ExcTestA
    exc_basic() except ExcTestA
    error("unreached")
end
@test Base.except_exception(Base.except_call(exc_filter)::Base.Except) isa ExcTestA

function exc_filter_narrow() except Union{ExcTestA, ExcTestC}
    exc_union(true) except ExcTestC   # ExcTestA at this site is thrown, not propagated
end
function exc_union(b) except Union{ExcTestA, ExcTestC}
    b ? throw(ExcTestA("a"))? : throw(ExcTestC())?
end
@test_throws ExcTestA exc_filter_narrow()

# Return type annotations combine with except declarations
function exc_rettype(b)::Int except ExcTestA
    b && throw(ExcTestA("x"))?
    return 1
end
@test exc_rettype(false) === 1
@test_throws ExcTestA exc_rettype(true)

# Short form definitions
exc_short() except ExcTestA = throw(ExcTestA("s"))?
@test_throws ExcTestA exc_short()
@test Base.except_exception(Base.except_call(exc_short)::Base.Except).msg == "s"

# Bare ? implies an inferred `except Any` declaration
exc_inferred() = exc_basic()?
@test_throws ExcTestA exc_inferred()
@test Base.except_exception(Base.except_call(exc_inferred)::Base.Except) isa ExcTestA

# At top level (no enclosing declaration), ? throws the declared exception
@test_throws ExcTestA exc_basic()?

# `?` on a non-call expression tests the value itself: Except values are
# unwrapped or propagated, any other value passes through unchanged
function exc_var_site(b) except ExcTestA
    r = Base.except_call(exc_value, b)
    r?
end
@test exc_var_site(false) == "value"
@test_throws ExcTestA exc_var_site(true)
@test Base.except_iserr(Base.except_call(exc_var_site, true))
function exc_plain_site() except ExcTestA
    x = 41
    (x?) + 1
end
@test exc_plain_site() == 42

# Methods with arguments, defaults, varargs, keywords
function exc_args(x, y=2; scale=1, rest...) except ExcTestA
    x < 0 && throw(ExcTestA("neg"))?
    (x + y) * scale
end
@test exc_args(1) == 3
@test exc_args(1, 3) == 4
@test exc_args(1; scale=10) == 30
@test_throws ExcTestA exc_args(-1)
@test Base.except_exception(Base.except_call(exc_args, -1)::Base.Except) isa ExcTestA
@test Base.except_result(Base.except_call(exc_args, 1; scale=5)::Base.Except) == 15

# where clauses
function exc_where(x::T) where {T<:Integer} except ExcTestA
    x < 0 && throw(ExcTestA("neg"))?
    x + one(T)
end
@test exc_where(1) === 2
@test Base.except_exception(Base.except_call(exc_where, -1)::Base.Except) isa ExcTestA

# Anonymous functions: closures get their own (inferred Any) declaration
let f = x -> (x < 0 && throw(ExcTestA("neg"))?; x + 1)
    @test f(1) == 2
    @test_throws ExcTestA f(-1)
    @test Base.except_exception(Base.except_call(f, -1)::Base.Except) isa ExcTestA
end

let f = function (x) except ExcTestA
        x < 0 && throw(ExcTestA("neg"))?
        x + 1
    end
    @test f(1) == 2
    @test Base.except_exception(Base.except_call(f, -1)::Base.Except) isa ExcTestA
end

# A `?` in a nested closure does not give the outer function a declaration
function exc_nested_closure(xs)
    f = x -> exc_basic()?
    return 1
end
@test exc_nested_closure([1]) == 1

#-------------------------------------------------------------------------------
# match integration

function exc_match_basic()
    match exc_basic()
    case except ExcTestA(msg)
        msg
    end
end
@test exc_match_basic() == "boom"

# Mixed value and exception arms
function exc_match_mixed(b) except ExcTestA
    match exc_maybe(b)
    case except ExcTestA(msg)
        "exc:" * msg
    case s::String
        "val:" * s
    end
end
function exc_maybe(b) except ExcTestA
    b ? throw(ExcTestA("e"))? : "v"
end
@test exc_match_mixed(true) == "exc:e"
@test exc_match_mixed(false) == "val:v"

# With only except arms, a non-exceptional value passes through
function exc_plain() except ExcTestA
    "plain"
end
@test (match exc_plain()
       case except ExcTestA(m)
           m
       end) == "plain"

# Guards on except arms
function exc_match_guard(b)
    match exc_maybe2(b)
    case except ExcTestA(msg) if msg == "one"
        1
    case except ExcTestA(msg)
        2
    case _
        3
    end
end
function exc_maybe2(b) except ExcTestA
    throw(ExcTestA(b ? "one" : "two"))?
end
@test exc_match_guard(true) == 1
@test exc_match_guard(false) == 2

# The empty body of an except arm evaluates to the exception object
@test (match exc_basic()
       case except ::ExcTestA
       end) isa ExcTestA

# Unmatched declared exceptions are thrown at the match
@test_throws ExcTestC (match exc_union(false)
                       case except ExcTestA(m)
                           m
                       end)

# ... unless the match expression itself is propagated (`end?` / `end except`)
function exc_match_leftover() except ExcTestC
    match exc_union(false)
    case except ExcTestA(m)
        m
    end except ExcTestC
end
@test Base.except_exception(Base.except_call(exc_match_leftover)::Base.Except) isa ExcTestC

function exc_match_leftover2() except Union{ExcTestA, ExcTestC}
    match exc_union(false)
    case except ExcTestA(m)
        m
    end?
end
@test Base.except_exception(Base.except_call(exc_match_leftover2)::Base.Except) isa ExcTestC

# Multiple except arms dispatch on the exception type, first match wins;
# `case except _` catches any declared exception
function exc_multi(sel) except Union{ExcTestA, ExcTestB, ExcTestC}
    sel == 1 && throw(ExcTestA("a"))?
    sel == 2 && throw(ExcTestB())?
    sel == 3 && throw(ExcTestC())?
    "value"
end
function exc_match_multi(sel)
    match exc_multi(sel)
    case except ExcTestA(m)
        "A:" * m
    case except ::ExcTestB
        "B"
    case except _
        "other declared"
    case _
    end
end
@test exc_match_multi(1) == "A:a"
@test exc_match_multi(2) == "B"
@test exc_match_multi(3) == "other declared"
@test exc_match_multi(0) == "value"

# First matching except arm wins even when a later one is more specific
@test (match exc_multi(1)
       case except ::Exception
           "general"
       case except ExcTestA(m)
           "specific"
       case _
           "value"
       end) == "general"

# Typed captures and alternation patterns in except arms
@test (match exc_multi(2)
       case except e::ExcTestB
           e isa ExcTestB
       case _
       end) === true
@test (match exc_multi(3)
       case except ::ExcTestB | ::ExcTestC
           "BC"
       case _
       end) == "BC"

# `as` binds the payload on the exceptional path and the value otherwise
function exc_match_as(sel)
    match exc_multi(sel) as it
    case except ::ExcTestA
        it
    case _
        it
    end
end
@test exc_match_as(1) isa ExcTestA
@test exc_match_as(0) == "value"

# A non-matching value still throws MatchError when value arms are present
@test_throws MatchError (match exc_multi(0)
                         case except ::ExcTestA
                             "exc"
                         case ::Int
                             "int"
                         end)

# except arms on a callee without declared exceptions never fire
exc_undeclared() = "plain"
@test (match exc_undeclared()
       case except ::Any
           "exc"
       case _
       end) == "plain"

# Keyword arguments in the scrutinee call
function exc_kwargs(x; fail=false) except ExcTestA
    fail && throw(ExcTestA("kw"))?
    x + 1
end
@test (match exc_kwargs(1; fail=false)
       case except ExcTestA(m)
           m
       case _
       end) == 2
@test (match exc_kwargs(1; fail=true)
       case except ExcTestA(m)
           m
       case _
       end) == "kw"

# Keyword arguments at a `?` propagation site
function exc_kwsite(b) except ExcTestA
    exc_kwargs(1; fail=b)?
end
@test exc_kwsite(false) == 2
@test Base.except_exception(Base.except_call(exc_kwsite, true)::Base.Except).msg == "kw"

# A pass-through value that is itself an exceptional-state Except object
# is not confused with the exceptional channel
exc_declared_data() = Base.except_error(ExcTestA("data"))
let r = (match exc_declared_data()
         case except ::ExcTestA
             "exc"
         end)
    @test r isa Base.Except && Base.except_iserr(r)
    @test Base.except_exception(r).msg == "data"
end

# return and break work in except-arm bodies
function exc_match_return(b)
    match exc_maybe(b)
    case except ::ExcTestA
        return "early"
    case _
    end
    return "late"
end
@test exc_match_return(true) == "early"
@test exc_match_return(false) == "late"

@test (match exc_basic()
       case except ::ExcTestA
           break _ "broke"
           error("unreached")
       end) == "broke"

# A leftover declared exception that does not match the postfix filter is
# thrown rather than propagated
function exc_match_leftover3() except ExcTestA
    match exc_union(false)   # raises ExcTestC
    case except ExcTestA(m)
        m
    end except ExcTestA      # filter does not cover ExcTestC
end
@test_throws ExcTestC Base.except_call(exc_match_leftover3)

#-------------------------------------------------------------------------------
# Comprehensions

function exc_comp(xs) except ExcTestA
    [exc_incr(x)? for x in xs]
end
function exc_incr(x) except ExcTestA
    x < 0 ? throw(ExcTestA("neg"))? : x + 1
end
@test exc_comp([1, 2, 3]) == [2, 3, 4]
@test eltype(exc_comp([1, 2, 3])) == Int
@test Base.except_exception(Base.except_call(exc_comp, [1, -1, 3])::Base.Except) isa ExcTestA
@test_throws ExcTestA exc_comp([1, -1])

# Lazy generators do not propagate: declared exceptions surfacing at a `?`
# inside a generator are thrown when the element is computed
function exc_lazy(xs) except ExcTestA
    collect((exc_incr(x)? for x in xs))
end
@test exc_lazy([1, 2]) == [2, 3]
@test_throws ExcTestA exc_lazy([-1])
@test_throws ExcTestA collect((exc_incr(x)? for x in [-1]))

# Comprehension with filter clause
function exc_comp_filter(xs) except ExcTestA
    [exc_incr(x)? for x in xs if x != 0]
end
@test exc_comp_filter([1, 0, 2]) == [2, 3]

# Typed comprehension
function exc_comp_typed(xs) except ExcTestA
    Float64[exc_incr(x)? for x in xs]
end
@test exc_comp_typed([1, 2]) == [2.0, 3.0]
@test exc_comp_typed([1, 2]) isa Vector{Float64}

# The comprehension aborts at the first declared exception
let seen = Int[]
    probe = function (x) except ExcTestA
        push!(seen, x)
        x < 0 && throw(ExcTestA("neg"))?
        x
    end
    g = function (xs) except ExcTestA
        [probe(x)? for x in xs]
    end
    r = Base.except_call(g, [-1, 5])
    @test Base.except_iserr(r)
    @test seen == [-1]
end

#-------------------------------------------------------------------------------
# Interaction with ordinary exception handling

# Declared exceptions that get thrown are ordinary exceptions for try/catch
@test try
    exc_basic()
    "no"
catch e
    e isa ExcTestA ? "caught" : "wrong"
end == "caught"

# except as an identifier still works
let except = 5
    @test except + 1 == 6
end
