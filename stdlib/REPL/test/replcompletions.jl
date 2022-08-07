# This file is a part of Julia. License is MIT: https://julialang.org/license

using REPL.REPLCompletions
using Test
using Random
using REPL
    @testset "Check symbols previously not shown by REPL.doc_completions()" begin
    symbols = ["?","=","[]","[","]","{}","{","}",";","","'","&&","||","julia","Julia","new","@var_str"]
        for i in symbols
            @test i ∈ REPL.doc_completions(i, Main)
        end
    end
let ex = quote
    module CompletionFoo
        using Random
        import Test

        mutable struct Test_y
            yy
        end
        mutable struct Test_x
            xx :: Test_y
        end
        type_test = Test_x(Test_y(1))
        (::Test_y)() = "", ""
        module CompletionFoo2

        end
        const bar = 1
        foo() = bar
        macro foobar()
            :()
        end
        macro barfoo(ex)
            ex
        end
        macro error_expanding()
            error("cannot expand @error_expanding")
            :()
        end
        macro error_lowering_conditional(a)
            if isa(a, Number)
                return a
            end
            throw(AssertionError("Not a Number"))
            :()
        end
        macro error_throwing()
            return quote
                error("@error_throwing throws an error")
            end
        end

        primitive type NonStruct 8 end
        Base.propertynames(::NonStruct) = (:a, :b, :c)
        x = reinterpret(NonStruct, 0x00)

        # Support non-Dict AbstractDicts, #19441
        mutable struct CustomDict{K, V} <: AbstractDict{K, V}
            mydict::Dict{K, V}
        end

        Base.keys(d::CustomDict) = collect(keys(d.mydict))
        Base.length(d::CustomDict) = length(d.mydict)

        test(x::T, y::T) where {T<:Real} = pass
        test(x::Real, y::Real) = pass
        test(x::AbstractArray{T}, y) where {T<:Real} = pass
        test(args...) = pass

        test1(x::Type{Float64}) = pass

        test2(x::AbstractString) = pass
        test2(x::Char) = pass
        test2(x::Cmd) = pass

        test3(x::AbstractArray{Int}, y::Int) = pass
        test3(x::AbstractArray{Float64}, y::Float64) = pass

        test4(x::AbstractString, y::AbstractString) = pass
        test4(x::AbstractString, y::Regex) = pass

        test5(x::Array{Bool,1}) = pass
        test5(x::BitArray{1}) = pass
        test5(x::Float64) = pass
        const a=x->x
        test6()=[a, a]
        test7() = rand(Bool) ? 1 : 1.0
        test8() = Any[1][1]
        test9(x::Char) = pass
        test9(x::Char, i::Int) = pass

        test10(a, x::Int...) = pass
        test10(a::Integer, b::Integer, c) = pass
        test10(a, y::Bool...) = pass
        test10(a, d::Integer, z::Signed...) = pass
        test10(s::String...) = pass

        test11(a::Integer, b, c) = pass
        test11(u, v::Integer, w) = pass
        test11(x::Int, y::Int, z) = pass
        test11(_, _, s::String) = pass

        test!12() = pass

        kwtest(; x=1, y=2, w...) = pass
        kwtest2(a; x=1, y=2, w...) = pass
        kwtest3(a::Number; length, len2, foobar, kwargs...) = pass
        kwtest3(a::Real; another!kwarg, len2) = pass
        kwtest3(a::Integer; namedarg, foobar, slurp...) = pass
        kwtest4(a::AbstractString; _a1b, x23) = pass
        kwtest4(a::String; _a1b, xαβγ) = pass
        kwtest4(a::SubString; x23, _something) = pass
        kwtest5(a::Int, b, x...; somekwarg, somekotherkwarg) = pass
        kwtest5(a::Char, b; xyz) = pass

        const named = (; len2=3)

        array = [1, 1]
        varfloat = 0.1

        const tuple = (1, 2)

        test_y_array=[CompletionFoo.Test_y(rand()) for i in 1:10]
        test_dict = Dict("abc"=>1, "abcd"=>10, :bar=>2, :bar2=>9, Base=>3,
                         occursin=>4, `ls`=>5, 66=>7, 67=>8, ("q",3)=>11,
                         "α"=>12, :α=>13)
        test_customdict = CustomDict(test_dict)

        macro teststr_str(s) end
        macro tϵsτstρ_str(s) end
        macro testcmd_cmd(s) end
        macro tϵsτcmδ_cmd(s) end

        end
        test_repl_comp_dict = CompletionFoo.test_dict
        test_repl_comp_customdict = CompletionFoo.test_customdict
        test_dict_ℂ = Dict(1=>2)
    end
    ex.head = :toplevel
    Core.eval(Main, ex)
end

function map_completion_text(completions)
    c, r, res = completions
    return map(completion_text, c), r, res
end

test_complete(s) = map_completion_text(@inferred(completions(s, lastindex(s))))
test_scomplete(s) =  map_completion_text(@inferred(shell_completions(s, lastindex(s))))
test_bslashcomplete(s) =  map_completion_text(@inferred(bslash_completions(s, lastindex(s)))[2])
test_complete_context(s, m) =  map_completion_text(@inferred(completions(s,lastindex(s), m)))
test_complete_foo(s) = test_complete_context(s, Main.CompletionFoo)
test_complete_noshift(s) = map_completion_text(@inferred(completions(s, lastindex(s), Main, false)))

module M32377 end
test_complete_32377(s) = map_completion_text(completions(s,lastindex(s), M32377))

macro test_nocompletion(s)
    tests = [
        :(@test c == String[]),
        :(@test res === false)
    ]
    for t in tests
        t.args[2] = __source__ # fix the LineNumberNode
    end
    return Expr(:let, Expr(:(=), :((c, _, res)), :(test_complete($(esc(s))))), Expr(:block, tests...))
end

let s = ""
    c, r = test_complete(s)
    @test "CompletionFoo" in c
    @test isempty(r)
    @test s[r] == ""
end

let s = "using REP"
    c, r = test_complete_32377(s)
    @test count(isequal("REPL"), c) == 1
    # issue #30234
    @test !Base.isbindingresolved(M32377, :tanh)
    # check what happens if REPL is already imported
    M32377.eval(:(using REPL))
    c, r = test_complete_32377(s)
    @test count(isequal("REPL"), c) == 1
end

let s = "Comp"
    c, r = test_complete(s)
    @test "CompletionFoo" in c
    @test r == 1:4
    @test s[r] == "Comp"
end

let s = "Main.Comp"
    c, r = test_complete(s)
    @test "CompletionFoo" in c
    @test r == 6:9
    @test s[r] == "Comp"
end

let s = "Main.CompletionFoo."
    c, r = test_complete(s)
    @test "bar" in c
    @test r === 20:19
    @test s[r] == ""
end

let s = "Main.CompletionFoo.f"
    c, r = test_complete(s)
    @test "foo" in c
    @test r == 20:20
    @test s[r] == "f"
    @test !("foobar" in c)
end

# test method completions when `!` operator precedes
let
    s = "!is"
    c, r = test_complete(s)
    @test "isa" in c
    @test s[r] == "is"
    @test !("!" in c)

    s = "!!is"
    c, r = test_complete(s)
    @test "isa" in c
    @test s[r] == "is"
    @test !("!" in c)
end

# issue #6424
let s = "Main.CompletionFoo.@f"
    c, r = test_complete(s)
    @test "@foobar" in c
    @test r == 20:21
    @test s[r] == "@f"
    @test !("foo" in c)
end

let s = "Main.CompletionFoo.type_test.x"
    c, r = test_complete(s)
    @test "xx" in c
    @test r == 30:30
    @test s[r] == "x"
end

let s = "Main.CompletionFoo.bar.no_val_available"
    c, r = test_complete(s)
    @test length(c)==0
end

#cannot do dot completion on infix operator
let s = "+."
    c, r = test_complete(s)
    @test length(c)==0
end

# To complete on a variable of a type, the type T of the variable
# must be a concrete type, hence Base.isstructtype(T) returns true,
# for the completion to succeed. That why `xx :: Test_y` of `Test_x`.
let s = "Main.CompletionFoo.type_test.xx.y"
    c, r = test_complete(s)
    @test "yy" in c
    @test r == 33:33
    @test s[r] == "y"
end

# issue #6333
let s = "Base.return_types(getin"
    c, r = test_complete(s)
    @test "getindex" in c
    @test r == 19:23
    @test s[r] == "getin"
end

# issue #23193: after `using`, identifiers can be prefixed by module names
let s = "using Test, Random"
    c, r = test_complete(s)
    @test !("RandomDevice" in c)
end

# issue #23226: identifiers must be separated by a comma (not a newline)
let s = "using Base\nusi"
    c, r = test_complete(s)
    @test "using" in c
end

# issue 23292
let
    @test_nowarn test_complete("test7().")
    c, r = test_complete("test8().")
    @test isempty(c)
end

# inexistent completion inside a string
@test_nocompletion("Base.print(\"lol")

# inexistent completion inside a cmd
@test_nocompletion("run(`lol")

# test latex symbol completions
let s = "\\alpha"
    c, r = test_bslashcomplete(s)
    @test c[1] == "α"
    @test r == 1:length(s)
    @test length(c) == 1
end

# test latex symbol completions after unicode #9209
let s = "α\\alpha"
    c, r = test_bslashcomplete(s)
    @test c[1] == "α"
    @test r == 3:sizeof(s)
    @test length(c) == 1
end

# test emoji symbol completions
let s = "\\:koala:"
    c, r = test_bslashcomplete(s)
    @test c[1] == "🐨"
    @test r == 1:sizeof(s)
    @test length(c) == 1
end

let s = "\\:ko"
    c, r = test_bslashcomplete(s)
    @test "\\:koala:" in c
end

# test emoji symbol completions after unicode #9209
let s = "α\\:koala:"
    c, r = test_bslashcomplete(s)
    @test c[1] == "🐨"
    @test r == 3:sizeof(s)
    @test length(c) == 1
end

# test latex symbol completions in strings should not work when there
# is a backslash in front of `\alpha` because it interferes with path completion on windows
let s = "cd(\"path_to_an_empty_folder_should_not_complete_latex\\\\\\alpha"
    c, r, res = test_complete(s)
    @test length(c) == 0
end

# test latex symbol completions in strings
let s = "\"C:\\\\ \\alpha"
    c, r, res = test_complete(s)
    @test c[1] == "α"
    @test r == 7:12
    @test length(c) == 1
end

# test latex symbol completion in getindex expressions (#24705)
let s = "tuple[\\alpha"
    c, r, res = test_complete_foo(s)
    @test c[1] == "α"
    @test r == 7:12
    @test length(c) == 1
end

let s = "\\a"
    c, r, res = test_complete(s)
    "\\alpha" in c
    @test r == 1:2
    @test s[r] == "\\a"
end

# `cd("C:\U should not make the repl crash due to escaping see comment #9137
let s = "cd(\"C:\\U"
    c, r, res = test_complete(s)
end

# Test method completions
let s = "max("
    c, r, res = test_complete(s)
    @test !res
    @test let found = false
        for m in methods(max)
            if !found
                found = (c[1] == string(m))
            end
        end
        found
    end
    @test r == 1:3
    @test s[r] == "max"
end

# test method completions when `!` operator precedes
let
    s = "!("
    c, r, res = test_complete(s)
    @test !res
    @test all(m -> string(m) in c, methods(!))
    @test s[r] == s[1:end-1]

    s = "!isnothing("
    c, r, res = test_complete(s)
    @test !res
    @test all(m -> string(m) in c, methods(isnothing))
    @test s[r] == s[1:end-1]

    s = "!!isnothing("
    c, r, res = test_complete(s)
    @test !res
    @test all(m -> string(m) in c, methods(isnothing))
    @test s[r] == s[1:end-1]
end

# Test completion of methods with input concrete args and args where typeinference determine their type
let s = "CompletionFoo.test(1, 1, "
    c, r, res = test_complete(s)
    @test !res
    @test c[1] == string(first(methods(Main.CompletionFoo.test, Tuple{Int, Int})))
    @test c[2] == string(first(methods(Main.CompletionFoo.test, Tuple{}))) # corresponding to the vararg
    @test length(c) == 2
    # In particular, this checks that test(x::Real, y::Real) is not a valid completion
    # since it is strictly less specific than test(x::T, y::T) where T
    @test r == 1:18
    @test s[r] == "CompletionFoo.test"
end

let s = "CompletionFoo.test(CompletionFoo.array,"
    c, r, res = test_complete(s)
    @test !res
    @test c[1] == string(first(methods(Main.CompletionFoo.test, Tuple{Array{Int, 1}, Any})))
    @test length(c) == 2
    @test r == 1:18
    @test s[r] == "CompletionFoo.test"
end

let s = "CompletionFoo.test(1,1,1,"
    c, r, res = test_complete(s)
    @test !res
    @test c[1] == string(first(methods(Main.CompletionFoo.test, Tuple{Any, Any, Any})))
    @test length(c) == 1
    @test r == 1:18
    @test s[r] == "CompletionFoo.test"
end

let s = "CompletionFoo.test1(Int,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 0
    @test r == 1:19
    @test s[r] == "CompletionFoo.test1"
end

let s = "CompletionFoo.test1(Float64,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
    @test r == 1:19
    @test s[r] == "CompletionFoo.test1"
end

let s = "prevind(\"θ\",1,"
    c, r, res = test_complete(s)
    @test c[1] == string(first(methods(prevind, Tuple{String, Int})))
    @test r == 1:7
    @test s[r] == "prevind"
end

for (T, arg) in [(String,"\")\""),(Char, "')'")]
    s = "(1, CompletionFoo.test2($arg,"
    c, r, res = test_complete(s)
    @test length(c) == 1
    @test c[1] == string(first(methods(Main.CompletionFoo.test2, Tuple{T,})))
    @test r == 5:23
    @test s[r] == "CompletionFoo.test2"
end

let s = "(1, CompletionFoo.test2(`')'`,"
    c, r, res = test_complete(s)
    @test length(c) == 1
    @test c[1] == string(first(methods(Main.CompletionFoo.test2, Tuple{Cmd})))
end

let s = "CompletionFoo.test3([1, 2] .+ CompletionFoo.varfloat,"
    c, r, res = test_complete(s)
    @test !res
    @test_broken only(c) == string(first(methods(Main.CompletionFoo.test3, Tuple{Array{Float64, 1}, Float64})))
end

let s = "CompletionFoo.test3([1.,2.], 1.,"
    c, r, res = test_complete(s)
    @test !res
    @test c[1] == string(first(methods(Main.CompletionFoo.test3, Tuple{Array{Float64, 1}, Float64})))
    @test r == 1:19
    @test length(c) == 1
    @test s[r] == "CompletionFoo.test3"
end

let s = "CompletionFoo.test4(\"e\",r\" \","
    c, r, res = test_complete(s)
    @test !res
    @test c[1] == string(first(methods(Main.CompletionFoo.test4, Tuple{String, Regex})))
    @test r == 1:19
    @test length(c) == 1
    @test s[r] == "CompletionFoo.test4"
end

# (As discussed in #19829, the Base.REPLCompletions.get_type function isn't
#  powerful enough to analyze anonymous functions.)
let s = "CompletionFoo.test5(broadcast((x,y)->x==y, push!(Base.split(\"\",' '),\"\",\"\"), \"\"),"
    c, r, res = test_complete(s)
    @test !res
    @test_broken only(c) == string(first(methods(Main.CompletionFoo.test5, Tuple{BitArray{1}})))
end

# test partial expression expansion
let s = "CompletionFoo.test5(Bool[x==1 for x=1:4],"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
    @test c[1] == string(first(methods(Main.CompletionFoo.test5, Tuple{Array{Bool,1}})))
end

let s = "CompletionFoo.test4(CompletionFoo.test_y_array[1]()[1], CompletionFoo.test_y_array[1]()[2], "
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
    @test c[1] == string(first(methods(Main.CompletionFoo.test4, Tuple{String, String})))
end

# Test that string escaption is handled correct
let s = """CompletionFoo.test4("\\"","""
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 2
end

# Test max method suggestions
let s = "convert("
    c, _, res = test_complete_noshift(s)
    @test !res
    @test only(c) == "convert( too many methods, use SHIFT-TAB to show )"
    c2, _, res2 = test_complete(s)
    @test !res2
    @test any(==(string(first(methods(convert)))), c2)
    @test length(c2) > REPL.REPLCompletions.MAX_METHOD_COMPLETIONS
end

########## Test where the current inference logic fails ########
# Fails due to inference fails to determine a concrete type for arg 1
# But it returns AbstractArray{T,N} and hence is able to remove test5(x::Float64) from the suggestions
let s = "CompletionFoo.test5(AbstractArray[[]][1],"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 2
end

# equivalent to above but due to the time macro the completion fails to find the concrete type
let s = "CompletionFoo.test3(@time([1, 2] + CompletionFoo.varfloat),"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 2
end
#################################################################

# method completions with kwargs
let s = "CompletionFoo.kwtest( "
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
    @test occursin("x, y, w...", c[1])
    @test (c, r, res) == test_complete("CompletionFoo.kwtest(;")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest(; x=1, ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest(; kw=1, ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest(x=1, ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest(x=1; ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest(x=kw=1, ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest(; x=kw=1, ")
end

let s = "CompletionFoo.kwtest2(1, x=1,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
    @test occursin("a; x, y, w...", c[1])
    @test (c, r, res) == test_complete("CompletionFoo.kwtest2(1; x=1, ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest2(1, x=1; ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest2(1, kw=1, ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest2(1; kw=1, ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest2(1, kw=1; ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest2(y=3, 1, ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest2(y=3, 1; ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest2(kw=3, 1, ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest2(kw=3, 1; ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest2(1; ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest2(1, ")
end

let s = "CompletionFoo.kwtest4(x23=18, x; "
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 3 # TODO: remove "kwtest4(a::String; _a1b, xαβγ)"
    @test any(str->occursin("kwtest4(a::SubString", str), c)
    @test any(str->occursin("kwtest4(a::AbstractString", str), c)
    @test (c, r, res) == test_complete("CompletionFoo.kwtest4(x23=18, x, ")
    @test (c, r, res) == test_complete("CompletionFoo.kwtest4(x23=18, ")
end

# TODO: @test_nocompletion("CompletionFoo.kwtest4(x23=17; ")
# TODO: @test_nocompletion("CompletionFoo.kwtest4.(x23=17; ")

let s = "CompletionFoo.kwtest5(3, somekwarg=6,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
    @test occursin("kwtest5(a::$(Int), b, x...; somekwarg, somekotherkwarg)", c[1])
    @test (c, r, res) == test_complete("CompletionFoo.kwtest5(3, somekwarg=6, anything, ")
end

# TODO: @test_nocompletion("CompletionFoo.kwtest5(3; somekwarg=6,")
# TODO: @test_nocompletion("CompletionFoo.kwtest5(3;")
# TODO: @test_nocompletion("CompletionFoo.kwtest5(3; somekwarg=6, anything, ")

#################################################################

# method completion with `?` (arbitrary method with given argument types)
let s = "CompletionFoo.?([1,2,3], 2.0)"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
    @test occursin("test(x::AbstractArray{T}, y) where T<:Real", c[1])
    # In particular, this checks that test(args...) is not a valid completion
    # since it is strictly less specific than test(x::AbstractArray{T}, y)
end

let s = "CompletionFoo.?([1,2,3], 2.0"
    c, r, res = test_complete(s)
    @test !res
    @test  any(str->occursin("test(x::AbstractArray{T}, y) where T<:Real", str), c)
    @test  any(str->occursin("test(args...)", str), c)
    @test !any(str->occursin("test3(x::AbstractArray{Int", str), c)
    @test !any(str->occursin("test4", str), c)
end

let s = "CompletionFoo.?('c')"
    c, r, res = test_complete(s)
    @test !res
    @test  any(str->occursin("test9(x::Char)", str), c)
    @test  any(str->occursin("test10(a, ", str), c)
    @test !any(str->occursin("test9(x::Char, i::Int", str), c)
end

let s = "CompletionFoo.?('c'"
    c, r, res = test_complete(s)
    @test !res
    @test  any(str->occursin("test9(x::Char)", str), c)
    @test  any(str->occursin("test10(a, ", str), c)
    @test  any(str->occursin("test9(x::Char, i::Int", str), c)
end

let s = "CompletionFoo.?(false, \"a\", 3, "
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 2
    @test occursin("test(args...)", c[1])
    @test occursin("test11(a::Integer, b, c)", c[2])
end

let s = "CompletionFoo.?(false, \"a\", 3, "
    c, r, res = test_complete_noshift(s)
    @test !res
    @test length(c) == 1
    @test occursin("test11(a::Integer, b, c)", c[1])
end

let s = "CompletionFoo.?(\"a\", 3, "
    c, r, res = test_complete(s)
    @test !res
    @test  any(str->occursin("test10(a, x::$Int...)", str), c)
    @test !any(str->occursin("test10(a, y::Bool...)", str), c)
    @test !any(str->occursin("test10(s::String...)", str), c)
end

let s = "CompletionFoo.?()"
    c, r, res = test_complete(s)
    @test !res
    @test any(str->occursin("foo()", str), c)
    @test any(str->occursin("kwtest(;", str), c)
    @test any(str->occursin("test(args...)", str), c)
end

let s = "CompletionFoo.?()"
    c, r, res = test_complete_noshift(s)
    @test !res
    @test length(c) == 1
    @test occursin("test10(s::String...)", c[1])
end

#= TODO: restrict the number of completions when a semicolon is present in ".?(" syntax
let s = "CompletionFoo.?(; y=2, "
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 4
    @test all(x -> occursin("kwtest", x), c)
    # We choose to include kwtest2 and kwtest3 although the number of args if wrong.
    # This is because the ".?(" syntax with no closing parenthesis does not constrain the
    # number of arguments in the methods it suggests.
end

let s = "CompletionFoo.?(3; len2=5, "
    c, r, res = test_complete_noshift(s)
    @test !res
    @test length(c) == 1
    @test occursin("kwtest3(a::Integer; namedarg, foobar, slurp...)", c[1])
    # the other two kwtest3 methods should not appear because of specificity
end
=#

# For the ".?(" syntax, do not constrain the number of arguments even with a semicolon.
@test test_complete("CompletionFoo.?(; ") ==
      test_complete("CompletionFoo.?(")

#TODO: @test test_complete("CompletionFoo.?(Any[]...; ") == test_complete("CompletionFoo.?(Cmd[]..., ") == test_complete("CompletionFoo.?(")

@test test_complete("CompletionFoo.?()") == test_complete("CompletionFoo.?(;)")

#TODO: @test_nocompletion("CompletionFoo.?(3; len2=5; ")

#################################################################

# Test method completion with varargs
let s = "CompletionFoo.test10(z, Integer[]...,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 5
    @test all(startswith("test10("), c)
    @test allunique(c)
end

let s = "CompletionFoo.test10(3, Integer[]...,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 4
    @test all(startswith("test10("), c)
    @test allunique(c)
    @test !any(str->occursin("test10(s::String...)", str), c)
end

let s = "CompletionFoo.test10(3, 4,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 3
    @test any(str->occursin("test10(a, x::$Int...)", str), c)
    @test any(str->occursin("test10(a::Integer, b::Integer, c)", str), c)
    @test any(str->occursin("test10(a, d::Integer, z::Signed...)", str), c)
end

let s = "CompletionFoo.test10(3, 4, 5,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 3
    @test any(str->occursin("test10(a, x::$Int...)", str), c)
    @test any(str->occursin("test10(a::Integer, b::Integer, c)", str), c) # show it even though the call would result in an ambiguity error
    @test any(str->occursin("test10(a, d::Integer, z::Signed...)", str), c)
    # the last one is not eliminated by specificity since the complete call could be
    # test10(3, 4, 5, Int8(6)) for instance
end

let s = "CompletionFoo.test10(z, z, 0, "
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 3
    @test any(str->occursin("test10(a, x::$Int...)", str), c)
    @test any(str->occursin("test10(a::Integer, b::Integer, c)", str), c) # show it even though the call would result in an ambiguity error
    @test any(str->occursin("test10(a, d::Integer, z::Signed...)", str), c)
end

let s = "CompletionFoo.test10(\"a\", Union{Signed,Bool,String}[3][1], "
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 4
    @test all(startswith("test10("), c)
    @test allunique(c)
    @test !any(str->occursin("test10(a::Integer, b::Integer, c)", str), c)
end

# Test method completion with ambiguity
let s = "CompletionFoo.test11(Integer[false][1], Integer[14][1], "
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 4
    @test all(startswith("test11("), c)
    @test allunique(c)
end

let s = "CompletionFoo.test11(Integer[-7][1], Integer[0x6][1], 6,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 3
    @test any(str->occursin("test11(a::Integer, b, c)", str), c)
    @test any(str->occursin("test11(u, v::Integer, w)", str), c)
    @test any(str->occursin("test11(x::$Int, y::$Int, z)", str), c)
end

let s = "CompletionFoo.test11(3, 4,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 4
    @test any(str->occursin("test11(x::$Int, y::$Int, z)", str), c)
    @test any(str->occursin("test11(::Any, ::Any, s::String)", str), c)
end

let s = "CompletionFoo.test11(0x8, 5,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 3
    @test any(str->occursin("test11(a::Integer, b, c)", str), c)
    @test any(str->occursin("test11(u, v::Integer, w)", str), c)
    @test any(str->occursin("test11(::Any, ::Any, s::String)", str), c)
end

let s = "CompletionFoo.test11(0x8, 'c',"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 2
    @test any(str->occursin("test11(a::Integer, b, c)", str), c)
    @test any(str->occursin("test11(::Any, ::Any, s::String)", str), c)
end

let s = "CompletionFoo.test11('d', 3,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 2
    @test any(str->occursin("test11(u, v::Integer, w)", str), c)
    @test any(str->occursin("test11(::Any, ::Any, s::String)", str), c)
end

let s = "CompletionFoo.test!12("
    c, r, res = test_complete(s)
    @test !res
    @test occursin("test!12()", only(c))
end

#= TODO: Test method completion depending on the number of arguments with splatting

@test_nocompletion("CompletionFoo.test3(unknown; ")
@test_nocompletion("CompletionFoo.test3.(unknown; ")

let s = "CompletionFoo.test2(unknown..., somethingelse..., xyz...; " # splat may be empty
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 3
    @test all(str->occursin("test2(", str), c)
    @test (c, r, res) == test_complete("CompletionFoo.test2(unknown..., somethingelse..., xyz, ")
    @test (c, r, res) == test_complete("CompletionFoo.test2(unknown..., somethingelse..., xyz; ")
end

let s = "CompletionFoo.test('a', args..., 'b';"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
    @test occursin("test(args...)", c[1])
    @test (c, r, res) == test_complete("CompletionFoo.test(a, args..., b, c;")
end

let s = "CompletionFoo.test(3, 5, args...,;"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 2
    @test any(str->occursin("test(x::T, y::T) where T<:Real", str), c)
    @test any(str->occursin("test(args...)", str), c)
end
=#

# Test that method calls with ill-formed kwarg syntax are not completed

@test_nocompletion("CompletionFoo.kwtest(; x=2, y=4; kw=3, ")
@test_nocompletion("CompletionFoo.kwtest(x=2; y=4; ")
@test_nocompletion("CompletionFoo.kwtest((x=y)=4, ")
@test_nocompletion("CompletionFoo.kwtest(; (x=y)=4, ")
@test_nocompletion("CompletionFoo.kwtest(; w...=16, ")
@test_nocompletion("CompletionFoo.kwtest(; 2, ")
@test_nocompletion("CompletionFoo.kwtest(; 2=3, ")
@test_nocompletion("CompletionFoo.kwtest3(im; (true ? length : length), ")
@test_nocompletion("CompletionFoo.kwtest.(x=2; y=4; ")
@test_nocompletion("CompletionFoo.kwtest.(; w...=16, ")

# Test of inference based getfield completion
let s = "(1+2im)."
    c,r = test_complete(s)
    @test length(c) == 2
    @test r == (lastindex(s) + 1):lastindex(s)
    @test c == ["im", "re"]
end

let s = "((1+2im))."
    c, r = test_complete(s)
    @test length(c) == 2
    @test r == (lastindex(s) + 1):lastindex(s)
    @test c == ["im", "re"]
end

let s = "CompletionFoo.test_y_array[1]."
    c, r = test_complete(s)
    @test length(c) == 1
    @test r == (lastindex(s) + 1):lastindex(s)
    @test c[1] == "yy"
end

let s = "CompletionFoo.Test_y(rand()).y"
    c, r = test_complete(s)
    @test length(c) == 1
    @test r == lastindex(s):lastindex(s)
    @test c[1] == "yy"
end

let s = "CompletionFoo.test6()[1](CompletionFoo.Test_y(rand())).y"
    c, r = test_complete(s)
    @test length(c) == 1
    @test r == lastindex(s):lastindex(s)
    @test c[1] == "yy"
end

let s = "CompletionFoo.named."
    c, r = test_complete(s)
    @test length(c) == 1
    @test r == (lastindex(s) + 1):lastindex(s)
    @test c[1] == "len2"
end

# Test completion in multi-line comments
let s = "#=\n\\alpha"
    c, r, res = test_complete(s)
    @test c[1] == "α"
    @test r == 4:9
    @test length(c) == 1
end

# Test that completion do not work in multi-line comments
let s = "#=\nmax"
    c, r, res = test_complete(s)
    @test length(c) == 0
end

# Test completion of packages
path = joinpath(tempdir(),randstring())
pushfirst!(LOAD_PATH, path)
try
    mkpath(path)
    write(joinpath(path, "Project.toml"),
        """
        name = "MyProj"

        [deps]
        MyPack = "09ebe64f-f76c-4f21-bef2-bd9be6c77e76"
        """)
        c, r, res = test_complete("using MyP")
        @test "MyPack" in c
        @test "MyProj" in c
finally
    @test popfirst!(LOAD_PATH) == path
    rm(path, recursive=true)
end


path = joinpath(tempdir(),randstring())
push!(LOAD_PATH, path)
try
    # Should not throw an error even though the path do no exist
    test_complete("using ")
    Pack_folder = joinpath(path, "Test_pack")
    mkpath(Pack_folder)

    Pack_folder2 = joinpath(path, "Test_pack2", "src")
    mkpath(Pack_folder2)
    touch(joinpath(Pack_folder2, "Test_pack2.jl"))

    # Test it completes on folders
    local c, r, res # workaround for issue #24331
    c, r, res = test_complete("using Test_p")
    @test !("Test_pack" in c)
    @test "Test_pack2" in c
finally
    @test pop!(LOAD_PATH) == path
    rm(path, recursive=true)
end

# Test $ in shell-mode
let s = "cd \$(max"
    c, r, res = test_scomplete(s)
    @test "max" in c
    @test r == 6:8
    @test s[r] == "max"
end

# The return type is of importance, before #8995 it would return nothing
# which would raise an error in the repl code.
let c, r, res
    c, r, res = test_scomplete("\$a")
    @test c == String[]
    @test r === 0:-1
    @test res === false
end

if Sys.isunix()
let s, c, r
    #Assume that we can rely on the existence and accessibility of /tmp

    # Tests path in Julia code and closing " if it's a file
    # Issue #8047
    s = "@show \"/dev/nul"
    c,r = test_complete(s)
    @test "null\"" in c
    @test r == 13:15
    @test s[r] == "nul"

    # Tests path in Julia code and not closing " if it's a directory
    # Issue #8047
    s = "@show \"/tm"
    c,r = test_complete(s)
    @test "tmp/" in c
    @test r == 9:10
    @test s[r] == "tm"

    # Tests path in Julia code and not double-closing "
    # Issue #8047
    s = "@show \"/dev/nul\""
    c,r = completions(s, 15)
    c = map(completion_text, c)
    @test "null" in c
    @test r == 13:15
    @test s[r] == "nul"

    s = "/t"
    c,r = test_scomplete(s)
    @test "tmp/" in c
    @test r == 2:2
    @test s[r] == "t"

    s = "/tmp"
    c,r = test_scomplete(s)
    @test "tmp/" in c
    @test r == 2:4
    @test s[r] == "tmp"

    # This should match things that are inside the tmp directory
    s = tempdir()
    if !endswith(s, "/")
        s = string(s, "/")
    end
    if !isdir(joinpath(s, "tmp"))
        c,r = test_scomplete(s)
        @test !("tmp/" in c)
        @test r === length(s) + 1:0
        @test s[r] == ""
    end

    s = "cd \$(Iter"
    c,r = test_scomplete(s)
    @test "Iterators" in c
    @test r == 6:9
    @test s[r] == "Iter"

    # Pressing tab after having entered "/tmp " should not
    # attempt to complete "/tmp" but rather work on the current
    # working directory again.
    let s, c, r, file
        file = joinpath(path, "repl completions")
        s = "/tmp "
        c,r = test_scomplete(s)
        @test r === 6:5
    end

    # Test completing paths with an escaped trailing space
    let s, c, r, file
        file = joinpath(tempdir(), "repl completions")
        touch(file)
        s = string(tempdir(), "/repl\\ ")
        c,r = test_scomplete(s)
        @test ["repl\\ completions"] == c
        @test s[r] == "repl\\ "
        rm(file)
    end

    # Tests homedir expansion
    mktempdir() do tmphome
        withenv("HOME" => tmphome, "USERPROFILE" => tmphome) do
            path = homedir()
            dir = joinpath(path, "tmpfoobar")
            mkdir(dir)
            s = "\"" * path * "/tmpfoob"
            c,r = test_complete(s)
            @test "tmpfoobar/" in c
            l = 3 + length(path)
            @test r == l:l+6
            @test s[r] == "tmpfoob"
            s = "\"~"
            @test "tmpfoobar/" in c
            c,r = test_complete(s)
            rm(dir)
        end
    end

    # Tests detecting of files in the env path (in shell mode)
    let path, file
        path = tempdir()
        unreadable = joinpath(tempdir(), "replcompletion-unreadable")

        try
            file = joinpath(path, "tmp-executable")
            touch(file)
            chmod(file, 0o755)
            mkdir(unreadable)
            chmod(unreadable, 0o000)

            # PATH can also contain folders which we aren't actually allowed to read.
            withenv("PATH" => string(path, ":", unreadable)) do
                s = "tmp-execu"
                c,r = test_scomplete(s)
                @test "tmp-executable" in c
                @test r == 1:9
                @test s[r] == "tmp-execu"
            end
        finally
            rm(file)
            rm(unreadable)
        end
    end

    # Make sure completion results are unique in case things are in the env path twice.
    let
        file0 = joinpath(tempdir(), "repl-completion")
        dir = joinpath(tempdir(), "repl-completion-subdir")
        file1 = joinpath(dir, "repl-completion")

        try
            # Create /tmp/repl-completion and /tmp/repl-completion-subdir/repl-completion
            mkdir(dir)
            touch(file0)
            touch(file1)

            withenv("PATH" => string(tempdir(), ":", dir)) do
                s = string("repl-completio")
                c,r = test_scomplete(s)
                @test ["repl-completion"] == c
                @test s[r] == "repl-completio"
            end

        finally
            rm(file0)
            rm(file1)
            rm(dir)
        end
    end
end
end

#test that it does not crash on files for which `stat` errors
let current_dir, forbidden
    # Issue #36855
    if !Sys.iswindows() || Sys.windows_version() >= Sys.WINDOWS_VISTA_VER
        mktempdir() do path
            selfsymlink = joinpath(path, "selfsymlink")
            symlink(selfsymlink, selfsymlink)
            @test try
                stat(selfsymlink) # should crash with a IOError
                false
            catch e
                e isa Base.IOError && occursin("ELOOP", e.msg)
            end
            c, r = test_complete("\"$(joinpath(path, "selfsym"))")
            @test c == ["selfsymlink"]
        end
    end

    # Issue #32797
    forbidden = Sys.iswindows() ? "C:\\S" : "/root/x"
    test_complete(forbidden); @test true # simply check that it did not crash

     # Issue #19310
    if Sys.iswindows()
        current_dir = pwd()
        cd("C:\\")
        test_complete("C"); @test true
        test_complete("C:"); @test true
        test_complete("C:\\"); @test true
        if isdir("D:\\")
            cd("D:\\")
            test_complete("C"); @test true
            test_complete("C:"); @test true
            test_complete("C:\\"); @test true
        end
        cd(current_dir)
    end
end

#test that it can auto complete with spaces in file/path
mktempdir() do path
    space_folder = randstring() * " α"
    dir = joinpath(path, space_folder)
    dir_space = replace(space_folder, " " => "\\ ")

    mkdir(dir)
    cd(path) do
        open(joinpath(space_folder, "space .file"),"w") do f
            s = Sys.iswindows() ? "rm $dir_space\\\\space" : "cd $dir_space/space"
            c, r = test_scomplete(s)
            @test r == lastindex(s)-4:lastindex(s)
            @test "space\\ .file" in c

            s = Sys.iswindows() ? "cd(\"β $dir_space\\\\space" : "cd(\"β $dir_space/space"
            c, r = test_complete(s)
            @test r == lastindex(s)-4:lastindex(s)
            @test "space .file\"" in c
        end
        # Test for issue #10324
        s = "cd(\"$dir_space"
        c, r = test_complete(s)
        @test r == 5:15
        @test s[r] ==  dir_space

        #Test for #18479
        for c in "'`@\$;&"
            test_dir = "test$(c)test"
            mkdir(joinpath(path, test_dir))
            try
                if !(c in ['\'','$']) # As these characters hold special meaning
                    # in shell commands the shell path completion cannot complete
                    # paths with these characters
                    c, r, res = test_scomplete(test_dir)
                    @test c[1] == test_dir*(Sys.iswindows() ? "\\\\" : "/")
                    @test res
                end
                c, r, res = test_complete("\""*test_dir)
                @test c[1] == test_dir*(Sys.iswindows() ? "\\\\" : "/")
                @test res
            finally
                rm(joinpath(path, test_dir), recursive=true)
            end
        end
    end
    rm(dir, recursive=true)
end

# Test tilde path completion
let (c, r, res) = test_complete("\"~/julia")
    if !Sys.iswindows()
        @test res && c == String[homedir() * "/julia"]
    else
        @test !res
    end

    c, r, res = test_complete("\"foo~bar")
    @test !res
end

# Test the completion returns nothing when the folder do not exist
let (c, r) = test_complete("cd(\"folder_do_not_exist_77/file")
    @test length(c) == 0
end

if Sys.iswindows()
    tmp = tempname()
    touch(tmp)
    path = realpath(dirname(tmp))
    file = basename(tmp)
    temp_name = basename(path)
    cd(path) do
        s = "cd ..\\\\"
        c,r = test_scomplete(s)
        @test r == length(s)+1:length(s)
        @test temp_name * "\\\\" in c

        s = "ls $(file[1:2])"
        c,r = test_scomplete(s)
        @test r == length(s)-1:length(s)
        @test file in c

        s = "cd(\"..\\"
        c,r = test_complete(s)
        @test r == length(s)+1:length(s)
        @test temp_name * "\\\\" in c

        s = "cd(\"$(file[1:2])"
        c,r = test_complete(s)
        @test r == length(s) - 1:length(s)
        @test (length(c) > 1 && file in c) || (["$file\""] == c)
    end
    rm(tmp)
end

# auto completions of true and false... issue #14101
let s = "tru"
    c, r, res = test_complete(s)
    @test "true" in c
end

let s = "fals"
    c, r, res = test_complete(s)
    @test "false" in c
end

# Don't crash when attempting to complete a tuple, #15329
let s = "CompletionFoo.tuple."
    c, r, res = test_complete(s)
    @test isempty(c)
end

@testset "sub/superscripts" begin
    @test "⁽¹²³⁾ⁿ" in test_complete("\\^(123)n")[1]
    @test "ⁿ" in test_complete("\\^n")[1]
    @test "ᵞ" in test_complete("\\^gamma")[1]
    @test isempty(test_complete("\\^(123)nq")[1])
    @test "₍₁₂₃₎ₙ" in test_complete("\\_(123)n")[1]
    @test "ₙ" in test_complete("\\_n")[1]
    @test "ᵧ" in test_complete("\\_gamma")[1]
    @test isempty(test_complete("\\_(123)nq")[1])
end

# test Dicts
function test_dict_completion(dict_name)
    s = "$dict_name[\"ab"
    c, r = test_complete(s)
    @test c == Any["\"abc\"", "\"abcd\""]
    s = "$dict_name[\"abcd"
    c, r = test_complete(s)
    @test c == Any["\"abcd\"]"]
    s = "$dict_name[ \"abcd"  # leading whitespace
    c, r = test_complete(s)
    @test c == Any["\"abcd\"]"]
    s = "$dict_name[\"abcd]"  # trailing close bracket
    c, r = completions(s, lastindex(s) - 1)
    c = map(completion_text, c)
    @test c == Any["\"abcd\""]
    s = "$dict_name[:b"
    c, r = test_complete(s)
    @test c == Any[":bar", ":bar2"]
    s = "$dict_name[:bar2"
    c, r = test_complete(s)
    @test c == Any[":bar2]"]
    s = "$dict_name[Ba"
    c, r = test_complete(s)
    @test c == Any["Base]"]
    s = "$dict_name[occ"
    c, r = test_complete(s)
    @test c == Any["occursin]"]
    s = "$dict_name[`l"
    c, r = test_complete(s)
    @test c == Any["`ls`]"]
    s = "$dict_name[6"
    c, r = test_complete(s)
    @test c == Any["66", "67"]
    s = "$dict_name[66"
    c, r = test_complete(s)
    @test c == Any["66]"]
    s = "$dict_name[("
    c, r = test_complete(s)
    @test c == Any["(\"q\", 3)]"]
    s = "$dict_name[\"\\alp"
    c, r = test_complete(s)
    @test c == String["\\alpha"]
    s = "$dict_name[\"\\alpha"
    c, r = test_complete(s)
    @test c == String["α"]
    s = "$dict_name[\"α"
    c, r = test_complete(s)
    @test c == Any["\"α\"]"]
    s = "$dict_name[:\\alp"
    c, r = test_complete(s)
    @test c == String["\\alpha"]
    s = "$dict_name[:\\alpha"
    c, r = test_complete(s)
    @test c == String["α"]
    s = "$dict_name[:α"
    c, r = test_complete(s)
    @test c == Any[":α]"]
    s = "$dict_name["
    c, r = test_complete(s)
    @test c == sort!(repr.(keys(Main.CompletionFoo.test_dict)))
end
test_dict_completion("CompletionFoo.test_dict")
test_dict_completion("CompletionFoo.test_customdict")
test_dict_completion("test_repl_comp_dict")
test_dict_completion("test_repl_comp_customdict")

# Issue #23004: this should not throw:
@test REPLCompletions.dict_identifier_key("test_dict_ℂ[\\", :other) isa Tuple

@testset "completion of string/cmd macros (#22577)" begin
    c, r, res = test_complete("ra")
    @test "raw\"" in c
    c, r, res = test_complete("CompletionFoo.tests")
    @test "teststr\"" in c
    c, r, res = test_complete("CompletionFoo.tϵsτs")
    @test "tϵsτstρ\"" in c
    c, r, res = test_complete("CompletionFoo.testc")
    @test "testcmd`" in c
    c, r, res = test_complete("CompletionFoo.tϵsτc")
    @test "tϵsτcmδ`" in c
end

@testset "Keyword-argument completion" begin
    c, r = test_complete("CompletionFoo.kwtest3(a;foob")
    @test c == ["foobar="]
    c, r = test_complete("CompletionFoo.kwtest3(a; le")
    @test "length" ∈ c # provide this kind of completion in case the user wants to splat a variable
    @test "length=" ∈ c
    @test "len2=" ∈ c
    @test "len2" ∉ c
    c, r = test_complete("CompletionFoo.kwtest3.(a;\nlength")
    @test "length" ∈ c
    @test "length=" ∈ c
    c, r = test_complete("CompletionFoo.kwtest3(a, length=4, l")
    @test "length" ∈ c
    @test "length=" ∉ c # since it was already used, do not suggest it again
    @test "len2=" ∈ c
    c, r = test_complete("CompletionFoo.kwtest3(a; kwargs..., fo")
    @test "foreach" ∈ c # provide this kind of completion in case the user wants to splat a variable
    @test "foobar=" ∈ c
    c, r = test_complete("CompletionFoo.kwtest3(a; another!kwarg=0, le")
    @test "length" ∈ c
    @test "length=" ∈ c # the first method could be called and `anotherkwarg` slurped
    @test "len2=" ∈ c
    c, r = test_complete("CompletionFoo.kwtest3(a; another!")
    @test c == ["another!kwarg="]
    c, r = test_complete("CompletionFoo.kwtest3(a; another!kwarg=0, foob")
    @test c == ["foobar="] # the first method could be called and `anotherkwarg` slurped
    c, r = test_complete("CompletionFoo.kwtest3(a; namedarg=0, foob")
    @test c == ["foobar="]

    # Check for confusion with CompletionFoo.named
    c, r = test_complete_foo("kwtest3(blabla; unknown=4, namedar")
    @test c == ["namedarg="]
    c, r = test_complete_foo("kwtest3(blabla; named")
    @test "named" ∈ c
    @test "namedarg=" ∈ c
    @test "len2" ∉ c
    c, r = test_complete_foo("kwtest3(blabla; named.")
    @test c == ["len2"]
    c, r = test_complete_foo("kwtest3(blabla; named..., another!")
    @test c == ["another!kwarg="]
    c, r = test_complete_foo("kwtest3(blabla; named..., len")
    @test "length" ∈ c
    @test "length=" ∈ c
    @test "len2=" ∈ c
    c, r = test_complete_foo("kwtest3(1+3im; named")
    @test "named" ∈ c
    # TODO: @test "namedarg=" ∉ c
    @test "len2" ∉ c
    c, r = test_complete_foo("kwtest3(1+3im; named.")
    @test c == ["len2"]

    c, r = test_complete("CompletionFoo.kwtest4(a; x23=0, _")
    @test "_a1b=" ∈ c
    @test "_something=" ∈ c
    c, r = test_complete("CompletionFoo.kwtest4(a; xαβγ=1, _")
    @test "_a1b=" ∈ c
    # TODO: @test "_something=" ∉ c # no such keyword for the method with keyword `xαβγ`
    c, r = test_complete("CompletionFoo.kwtest4.(a; xαβγ=1, _")
    @test "_a1b=" ∈ c
    # TODO: @test "_something=" ∉ c # broadcasting does not affect the existence of kwargs
    c, r = test_complete("CompletionFoo.kwtest4(a; x23=0, x")
    @test "x23=" ∉ c
    # TODO: @test "xαβγ=" ∉ c
    c, r = test_complete("CompletionFoo.kwtest4.(a; x23=0, x")
    @test "x23=" ∉ c
    # TODO: @test "xαβγ=" ∉ c
    c, r = test_complete("CompletionFoo.kwtest4(a; _a1b=1, x")
    @test "x23=" ∈ c
    @test "xαβγ=" ∈ c

    c, r = test_complete("CompletionFoo.kwtest5(3, 5; somek")
    @test c == ["somekotherkwarg=", "somekwarg="]
    c, r = test_complete("CompletionFoo.kwtest5(3, 5, somekwarg=4, somek")
    @test c == ["somekotherkwarg="]
    c, r = test_complete("CompletionFoo.kwtest5(3, 5, 7; somekw")
    @test c == ["somekwarg="]
    c, r = test_complete("CompletionFoo.kwtest5(3, 5, 7, 9; somekw")
    @test c == ["somekwarg="]
    c, r = test_complete("CompletionFoo.kwtest5(3, 5, 7, 9, Any[]...; somek")
    @test c == ["somekotherkwarg=", "somekwarg="]
    c, r = test_complete("CompletionFoo.kwtest5(unknownsplat...; somekw")
    @test c == ["somekwarg="]
    c, r = test_complete("CompletionFoo.kwtest5(3, 5, 7, 9, somekwarg=4, somek")
    @test c == ["somekotherkwarg="]
    c, r = test_complete("CompletionFoo.kwtest5(String[]..., unknownsplat...; xy")
    @test c == ["xyz="]
    c, r = test_complete("CompletionFoo.kwtest5('a', unknownsplat...; xy")
    @test c == ["xyz="]
    c, r = test_complete("CompletionFoo.kwtest5('a', 3, String[]...; xy")
    @test c == ["xyz="]

    # return true if no completion suggests a keyword argument
    function hasnokwsuggestions(str)
        c, _ = test_complete(str)
        return !any(x -> endswith(x, r"[a-z]="), c)
    end
    @test hasnokwsuggestions("Completio")
    @test hasnokwsuggestions("CompletionFoo.kwt")
    @test hasnokwsuggestions("CompletionFoo.kwtest3(")
    @test hasnokwsuggestions("CompletionFoo.kwtest3(a;")
    @test hasnokwsuggestions("CompletionFoo.kwtest3(a; len2=")
    @test hasnokwsuggestions("CompletionFoo.kwtest3(a; len2=le")
    @test hasnokwsuggestions("CompletionFoo.kwtest3(a; len2=3 ")
    @test hasnokwsuggestions("CompletionFoo.kwtest3(a; [le")
    @test hasnokwsuggestions("CompletionFoo.kwtest3([length; le")
    @test hasnokwsuggestions("CompletionFoo.kwtest3(a; (le")
    @test hasnokwsuggestions("CompletionFoo.kwtest3(a; foo(le")
    @test hasnokwsuggestions("CompletionFoo.kwtest3(a; (; le")
    @test hasnokwsuggestions("CompletionFoo.kwtest3(a; length, ")
    @test hasnokwsuggestions("CompletionFoo.kwtest3(a; kwargs..., ")

    #= TODO: Test the absence of kwarg completion the call is incompatible with the method bearing the kwarg.
    @test hasnokwsuggestions("CompletionFoo.kwtest3(a")
    @test hasnokwsuggestions("CompletionFoo.kwtest3(le")
    @test hasnokwsuggestions("CompletionFoo.kwtest3(a; unknown=4, another!kw") # only methods 1 and 3 could slurp `unknown`
    @test hasnokwsuggestions("CompletionFoo.kwtest3(1+3im; nameda")
    @test hasnokwsuggestions("CompletionFoo.kwtest3(12//7; foob") # because of specificity
    @test hasnokwsuggestions("CompletionFoo.kwtest3(a, len2=b, length, foob") # length is not length=length
    @test hasnokwsuggestions("CompletionFoo.kwtest5('a', 3, 5, unknownsplat...; xy")
    @test hasnokwsuggestions("CompletionFoo.kwtest5(3; somek")
    =#
end

# Test completion in context

# No CompletionFoo.CompletionFoo
let s = ""
    c, r = test_complete_foo(s)
    @test !("CompletionFoo" in c)
end

# Can see `rand()` after `using Random`
let s = "r"
    c, r = test_complete_foo(s)
    @test "rand" in c
    @test r == 1:1
    @test s[r] == "r"
end

# Can see `Test.AbstractTestSet` after `import Test`
let s = "Test.A"
    c, r = test_complete_foo(s)
    @test "AbstractTestSet" in c
    @test r == 6:6
    @test s[r] == "A"
end

# Can complete relative import
let s = "import ..M"
    c, r = test_complete_foo(s)
    @test_broken "Main" in c
    @test r == 10:10
    @test s[r] == "M"
end

let s = ""
    c, r = test_complete_foo(s)
    @test "bar" in c
    @test r === 1:0
    @test s[r] == ""
end

let s = "f"
    c, r = test_complete_foo(s)
    @test "foo" in c
    @test r == 1:1
    @test s[r] == "f"
    @test !("foobar" in c)
end

let s = "@f"
    c, r = test_complete_foo(s)
    @test "@foobar" in c
    @test r == 1:2
    @test s[r] == "@f"
    @test !("foo" in c)
end

let s = "type_test.x"
    c, r = test_complete_foo(s)
    @test "xx" in c
    @test r == 11:11
    @test s[r] == "x"
end

let s = "bar.no_val_available"
    c, r = test_complete_foo(s)
    @test length(c)==0
end

let s = "type_test.xx.y"
    c, r = test_complete_foo(s)
    @test "yy" in c
    @test r == 14:14
    @test s[r] == "y"
end

let s = ":(function foo(::Int) end).args[1].args[2]."
    c, r = test_complete_foo(s)
    @test c == Any[]
end

let s = "log(log.(x),"
    c, r = test_complete_foo(s)
    @test !isempty(c)
end

let s = "Base.return_types(getin"
    c, r = test_complete_foo(s)
    @test "getindex" in c
    @test r == 19:23
    @test s[r] == "getin"
end

let s = "using Test, Random"
    c, r = test_complete_foo(s)
    @test !("RandomDevice" in c)
end

let s = "test(1,1, "
    c, r, res = test_complete_foo(s)
    @test !res
    @test c[1] == string(first(methods(Main.CompletionFoo.test, Tuple{Int, Int})))
    @test c[2] == string(first(methods(Main.CompletionFoo.test, Tuple{})))  # corresponding to the vararg
    @test length(c) == 2
    # In particular, this checks that test(x::Real, y::Real) is not a valid completion
    # since it is strictly less specific than test(x::T, y::T) where T
    @test r == 1:4
    @test s[r] == "test"
end

let s = "test.(1,1, "
    c, r, res = test_complete_foo(s)
    @test !res
    @test length(c) == 4
    @test r == 1:4
    @test s[r] == "test"
    # TODO: @test (c, r, res) == test_complete_foo("test.(1, 1, String[]..., ")
    # TODO: @test (c, r, res) == test_complete_foo("test.(1, Any[]..., 2, ")
end

let s = "prevind(\"θ\",1,"
    c, r, res = test_complete_foo(s)
    @test c[1] == string(first(methods(prevind, Tuple{String, Int})))
    @test r == 1:7
    @test s[r] == "prevind"
end

# Issue #32840
let s = "typeof(+)."
    c, r = test_complete_foo(s)
    @test length(c) == length(fieldnames(DataType))
end

let s = "test_dict[\"ab"
    c, r = test_complete_foo(s)
    @test c == Any["\"abc\"", "\"abcd\""]
end

let s = "CompletionFoo.x."
    c, r = test_complete(s)
    @test "a" in c
end

# https://github.com/JuliaLang/julia/issues/27184
let
    (test_complete("@noexist."); @test true)
    (test_complete("Main.@noexist."); @test true)
    (test_complete("@Main.noexist."); @test true)
end

let # Check that completion does not crash on (possibly invalid) macro calls
    (test_complete("@show."); @test true)
    (test_complete("@macroexpand."); @test true)
    (test_complete("@.."); @test true)
    (test_complete("CompletionFoo.@foobar."); @test true)
    (test_complete("CompletionFoo.@foobar()."); @test true)
    (test_complete("CompletionFoo.@foobar(4)."); @test true)
    (test_complete("CompletionFoo.@barfoo."); @test true)
    (test_complete("CompletionFoo.@barfoo()."); @test true)
    (test_complete("CompletionFoo.@barfoo(6)."); @test true)
    (test_complete("CompletionFoo.@error_expanding."); @test true)
    (test_complete("CompletionFoo.@error_expanding()."); @test true)
    (test_complete("CompletionFoo.@error_lowering_conditional."); @test true)
    (test_complete("CompletionFoo.@error_lowering_conditional()."); @test true)
    (test_complete("CompletionFoo.@error_lowering_conditional(3)."); @test true)
    (test_complete("CompletionFoo.@error_lowering_conditional('a')."); @test true)
    (test_complete("CompletionFoo.@error_throwing."); @test true)
    (test_complete("CompletionFoo.@error_throwing()."); @test true)
end

@testset "https://github.com/JuliaLang/julia/issues/40247" begin
    # getfield type completion can work for complicated expression

    let
        m = Module()
        @eval m begin
            struct Rs
                rs::Vector{Regex}
            end
            var = nothing
            function foo()
                global var = 1
                return Rs([r"foo"])
            end
        end

        c, r = test_complete_context("foo(#=#==#=##==#).rs[1].", m)
        @test m.var === nothing # getfield type completion should never execute `foo()`
        @test length(c) == fieldcount(Regex)
    end

    let
        m = Module()
        @eval m begin
            struct R
                r::Regex
            end
            var = nothing
            function foo()
                global var = 1
                return R(r"foo")
            end
        end

        c, r = test_complete_context("foo().r.", m)
        @test m.var === nothing # getfield type completion should never execute `foo()`
        @test length(c) == fieldcount(Regex)

        c, r = test_complete_context("foo(#=#=# =#= =#).r.", m)
        @test m.var === nothing # getfield type completion should never execute `foo()`
        @test length(c) == fieldcount(Regex)
    end
end
