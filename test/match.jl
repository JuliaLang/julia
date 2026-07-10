# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for the `match` statement

# Basic literal matching uses isequal
@test (match 2
       case 1
           "one"
       case 2
           "two"
       end) == "two"

@test (match 1.0
       case 1
           "int one"
       end) == "int one"

@test (match NaN
       case NaN
           "nan"
       end) == "nan"

@test (match missing
       case 1
           "one"
       case missing
           "missing"
       end) == "missing"

# Fallthrough throws MatchError
@test_throws MatchError (match 3
                         case 1
                             "one"
                         end)
let err = try; (match 42; case 0; end); catch e; e; end
    @test err isa MatchError
    @test err.value == 42
    @test occursin("42", sprint(showerror, err))
end

# Wildcard
@test (match 42
       case 1
           "one"
       case _
           "other"
       end) == "other"

# An empty arm body evaluates to the matched value
@test (match 42
       case ::Int
       end) == 42

# Type patterns and typed captures
@test (match 3.5
       case ::Int
           "int"
       case ::Float64
           "float"
       end) == "float"

@test (match 10
       case x::Int
           x + 1
       end) == 11

# Tuple patterns: isa Tuple, exact arity
@test (match (1, 2)
       case (a, b)
           a + b
       end) == 3

@test (match (1, 2, 3)
       case (a, b)
           "two"
       case (a, b, c)
           "three"
       end) == "three"

@test (match [1, 2]
       case (a, b)
           "tuple"
       case [a, b]
           "vector"
       end) == "vector"

@test (match (1, (2, 3))
       case (a, (b, c))
           a + b + c
       end) == 6

# Array patterns with slurp
@test (match [1, 2, 3, 4]
       case [a, b, rest...]
           (a, b, sum(rest))
       end) == (1, 2, 7)

# Pair patterns
@test (match (:a => 1)
       case k => v
           (k, v)
       end) == (:a, 1)

# Guards
@test (match (1, 2)
       case (a, b) if a > b
           "decreasing"
       case (a, b) if a < b
           "increasing"
       end) == "increasing"

# Alternation binds the same names in every alternative
@test (match (5, 1)
       case (1, a) | (a, 1)
           a + 100
       end) == 105
@test (match (1, 7)
       case (1, a) | (a, 1)
           a + 100
       end) == 107

# $ escapes match values by isequal
let zero = 0
    @test (match (0, 7)
           case ($zero, b)
               b
           end) == 7
end

# Resolved matchers at pattern top level
@test (match 0
       case ==(0)
           "zero"
       case _
           "other"
       end) == "zero"

@test (match 2
       case in(1:3)
           "member"
       case _
           "not"
       end) == "member"

const match_test_const = 7
@test (match 7
       case match_test_const
           "matched"
       case _
           "other"
       end) == "matched"

# A misspelled constant is an UndefVarError, not a silent capture
@test_throws UndefVarError (match 7
                            case match_test_cnost
                                "?"
                            end)

# Bare types match the type object; `::T` matches instances
@test (match Int
       case Int
           "the type"
       case _
           "other"
       end) == "the type"

@test (match 42
       case Int
           "type obj"
       case ::Int
           "int value"
       end) == "int value"

# Struct field destructuring
struct MatchTestPoint
    x
    y
end
@test (match MatchTestPoint(1, 2)
       case MatchTestPoint(a, b)
           a + b
       end) == 3
@test (match MatchTestPoint(1, 2)
       case MatchTestPoint(1, b)
           b
       case _
           "no"
       end) == 2

# Property patterns
struct MatchTestProps
    a
    b
    c
end
@test (match MatchTestProps(1, 2, 3)
       case (; a, b)
           a + b
       end) == 3
@test (match (a=1, c=2)
       case (; a, b)
           "ab"
       case (; a, c)
           "ac"
       end) == "ac"

# Regex matchers match strings
@test (match "hello123"
       case r"^[a-z]+[0-9]+$"
           "matched"
       case _
           "no"
       end) == "matched"

@test (match "abc"
       case "abc"
           1
       case _
           2
       end) == 1

# `as` binds the matched value over the whole match
@test (match 41 as it
       case ::Int
           it + 1
       end) == 42

@test (match 10 as v
       case _ if v > 5
           "big"
       case _
           "small"
       end) == "big"

# match is an expression
@test 2 * (match 3
           case 3
               10
           end) == 20

# Custom matchers via the matcher / pattern_match protocol
struct MatchTestEven end
Base.matcher(::typeof(iseven), pat) = (MatchTestEven(), pat)
Base.pattern_match(m::Tuple{MatchTestEven, Any}, v) =
    iseven(v) ? Base.pattern_match(m[2], v) : nothing
@test (match 4
       case iseven(k)
           k + 1
       case _
           "odd"
       end) == 5

# Calls to non-type functions without captures evaluate to a matcher value
@test (match 5
       case <(10)
           "small"
       case _
           "big"
       end) == "small"

# But calls with captures through unknown functions are an error
@test_throws ArgumentError (match 5
                            case sin(x)
                                x
                            end)

#-------------------------------------------------------------------------------
# break/return/continue interaction

# Bare break exits the match (with nothing)
@test (match 1
       case 1
           break
           error("unreached")
       end) === nothing

# break _ value exits the match with a value
@test (match 1
       case 1
           break _ 99
           error("unreached")
       end) == 99

# break inside a match inside a loop exits the match, not the loop
let acc = 0
    for i in 1:3
        match i
        case _
            break
        end
        acc += i
    end
    @test acc == 6
end

# Labeled break exits the enclosing loop from inside a match arm
let acc = 0
    @label outer for i in 1:10
        match i
        case 3
            break outer
        case _
        end
        acc += i
    end
    @test acc == 3
end

# Labeled continue continues the enclosing loop from inside a match arm
let acc = 0
    @label loop for i in 1:5
        match iseven(i)
        case true
            continue loop
        case false
        end
        acc += i
    end
    @test acc == 9
end

# Unlabeled continue inside a match arm is a lowering error: break and
# continue scopes are deliberately not decoupled
let m = Module(:match_continue_test)
    Base.set_syntax_version(m, v"1.14")
    err = try
        include_string(m, """
            for i in 1:3
                match i
                case 1
                    continue
                case _
                end
            end
            """)
        nothing
    catch e
        e isa LoadError ? e.error : e
    end
    @test err isa Exception
    @test occursin("continue", sprint(showerror, err))
end

# return returns from the enclosing function
function match_test_return(x)
    match x
    case 1
        return "early"
    case _
    end
    return "late"
end
@test match_test_return(1) == "early"
@test match_test_return(2) == "late"

# match works in statement position
function match_test_stmt_pos(x)
    match x
    case 1
        "one"
    end
    return "after"
end
@test match_test_stmt_pos(1) == "after"

# Nested match
@test (match 1
       case 1
           match 2
           case 2
               "nested"
           end
       end) == "nested"

#-------------------------------------------------------------------------------
# Inline match-destructuring

let
    match (a, b) = (1, 2)
    @test a == 1 && b == 2
end
@test_throws MatchError (match (a, b) = (1, 2, 3))
@test (match (a, b) = (1, 2)) == (1, 2)
let
    match MatchTestPoint(x0, y0) = MatchTestPoint(3, 4)
    @test x0 == 3 && y0 == 4
end

#-------------------------------------------------------------------------------
# Scoping

# Each arm runs in its own scope, like a let block: neither captures nor
# body assignments escape it
let
    match (1, 2)
    case (matchscope_a, matchscope_b)
        matchscope_body = matchscope_a + matchscope_b
    end
    @test !@isdefined(matchscope_a)
    @test !@isdefined(matchscope_body)
end

# match/case still work as ordinary identifiers
let match = 10, case = 20
    @test match + case == 30
end
@test Base.match(r"a(b)", "ab")[1] == "b"

# `rest...` in call patterns is delivered to `matcher` implementations as a
# MatchSlurp marker; struct-field destructuring rejects it
firstrest_probe(args...) = nothing
struct FirstRestMatcher
    first
    rest
end
Base.matcher(::typeof(firstrest_probe), p1, rest::Base.MatchSlurp) =
    FirstRestMatcher(p1, rest.pat)
function Base.pattern_match(m::FirstRestMatcher, @nospecialize(v))
    (v isa AbstractVector && !isempty(v)) || return nothing
    r1 = Base.pattern_match(m.first, v[begin])
    r1 === nothing && return nothing
    r2 = Base.pattern_match(m.rest, v[begin+1:end])
    r2 === nothing && return nothing
    return (r1..., r2...)
end
@test (match [1, 2, 3]
       case firstrest_probe(x, xs...)
           (x, xs)
       end) == (1, [2, 3])
@test_throws ArgumentError (match (1, 2)
                            case Some(x, rest...)
                                x
                            end)
