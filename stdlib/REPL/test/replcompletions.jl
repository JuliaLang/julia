# This file is a part of Julia. License is MIT: https://julialang.org/license

using REPL.REPLCompletions
using Test
using Random
using REPL

@testset "Check symbols previously not shown by REPL.doc_completions()" begin
    symbols = ["?","=","[]","[","]","{}","{","}",";","","'","&&","||","julia","Julia","new","@var_str"]
    for i in symbols
        @test i ∈ string.(REPL.doc_completions(i, Main))
    end
end

let ex =
    quote
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
            unicode_αβγ = Test_y(1)

            Base.:(+)(x::Test_x, y::Test_y) = Test_x(Test_y(x.xx.yy + y.yy))
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

            # Support AbstractDict with unknown length, #55931
            struct NoLengthDict{K,V} <: AbstractDict{K,V}
                dict::Dict{K,V}
                NoLengthDict{K,V}() where {K,V} = new(Dict{K,V}())
            end
            Base.iterate(d::NoLengthDict, s...) = iterate(d.dict, s...)
            Base.IteratorSize(::Type{<:NoLengthDict}) = Base.SizeUnknown()
            Base.eltype(::Type{NoLengthDict{K,V}}) where {K,V} = Pair{K,V}
            Base.setindex!(d::NoLengthDict, v, k) = d.dict[k] = v

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
            const fmsoebelkv = (; len2=3)

            array = [1, 1]
            varfloat = 0.1

            const tuple = (1, 2)

            test_y_array=[(@__MODULE__).Test_y(rand()) for i in 1:10]
            test_dict = Dict("abc"=>1, "abcd"=>10, :bar=>2, :bar2=>9, Base=>3,
                            occursin=>4, `ls`=>5, 66=>7, 67=>8, ("q",3)=>11,
                            "α"=>12, :α=>13)
            test_customdict = CustomDict(test_dict)

            macro teststr_str(s) end
            macro tϵsτstρ_str(s) end
            macro testcmd_cmd(s) end
            macro tϵsτcmδ_cmd(s) end

            var"complicated symbol with spaces" = 5

            struct WeirdNames end
            Base.propertynames(::WeirdNames) = (Symbol("oh no!"), Symbol("oh yes!"))

            # https://github.com/JuliaLang/julia/issues/52551#issuecomment-1858543413
            export exported_symbol
            exported_symbol(::WeirdNames) = nothing

        end # module CompletionFoo
        test_repl_comp_dict = CompletionFoo.test_dict
        test_repl_comp_customdict = CompletionFoo.test_customdict
        test_dict_ℂ = Dict(1=>2)
        test_dict_no_length = CompletionFoo.NoLengthDict{Int,Int}()
    end
    ex.head = :toplevel
    Core.eval(Main, ex)
end

function map_completion_text(completions)
    c, r, res = completions
    return map(x -> named_completion(x).completion, c), r, res
end

function map_named_completion(completions)
    c, r, res = completions
    return map(named_completion, c), r, res
end

test_complete(s) = map_completion_text(@inferred(completions(s, lastindex(s))))
test_scomplete(s) =  map_completion_text(@inferred(shell_completions(s, lastindex(s))))
test_complete_context(s, m=@__MODULE__; shift::Bool=true) =
    map_completion_text(@inferred(completions(s,lastindex(s), m, shift)))
test_complete_foo(s) = test_complete_context(s, Main.CompletionFoo)
test_complete_noshift(s) = map_completion_text(@inferred(completions(s, lastindex(s), Main, false)))

test_bslashcomplete(s) =  map_named_completion(@inferred(bslash_completions(s, lastindex(s)))[2])

test_methods_list(@nospecialize(f), tt) = map(x -> string(x.method), Base._methods_by_ftype(Base.signature_type(f, tt), 10, Base.get_world_counter()))


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

let s = "Main.CompletionFoo.unicode_αβγ.y"
    c, r = test_complete(s)
    @test "yy" in c
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

# issue 46800: (3,2).<TAB> errors in the REPL
let
    c, r = test_complete("(3,2).")
    @test isempty(c)
end

# inexistent completion inside a string
@test_nocompletion("Base.print(\"lol")

# inexistent completion inside a cmd
@test_nocompletion("run(`lol")

# issue 55856: copy(A').<TAB> errors in the REPL
let
    c, r = test_complete("copy(A').")
    @test isempty(c)
end

# test latex symbol completions
let s = "\\alpha"
    c, r = test_bslashcomplete(s)
    @test c[1].completion == "α"
    @test c[1].name == "α"
    @test r == 1:lastindex(s)
    @test length(c) == 1
end

# test latex symbol completions after unicode #9209
let s = "α\\alpha"
    c, r = test_bslashcomplete(s)
    @test c[1].completion == "α"
    @test c[1].name == "α"
    @test r == 3:sizeof(s)
    @test length(c) == 1
end

# test emoji symbol completions
let s = "\\:koala:"
    c, r = test_bslashcomplete(s)
    @test c[1].completion == "🐨"
    @test c[1].name == "🐨"
    @test r == 1:sizeof(s)
    @test length(c) == 1
end

let s = "\\:ko"
    c, r = test_bslashcomplete(s)
    ko = only(filter(c) do namedcompletion
        namedcompletion.completion == "\\:koala:"
    end)
    @test ko.name == "🐨 \\:koala:"
end

# test emoji symbol completions after unicode #9209
let s = "α\\:koala:"
    c, r = test_bslashcomplete(s)
    @test c[1].name == "🐨"
    @test c[1].completion == "🐨"
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
    m = test_methods_list(Main.CompletionFoo.test, Tuple{Int, Int, Vararg})
    @test c[1] == m[1]
    @test c[2] == m[2]
    @test length(c) == 2
    # In particular, this checks that test(x::Real, y::Real) is not a valid completion
    # since it is strictly less specific than test(x::T, y::T) where T
    @test r == 1:18
    @test s[r] == "CompletionFoo.test"
end

let s = "CompletionFoo.test(CompletionFoo.array,"
    c, r, res = test_complete(s)
    @test !res
    @test c[1] == first(test_methods_list(Main.CompletionFoo.test, Tuple{Array{Int, 1}, Any, Vararg}))
    @test length(c) == 2
    @test r == 1:18
    @test s[r] == "CompletionFoo.test"
end

let s = "CompletionFoo.test(1,1,1,"
    c, r, res = test_complete(s)
    @test !res
    @test c[1] == first(test_methods_list(Main.CompletionFoo.test, Tuple{Any, Any, Any, Vararg}))
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
    @test c[1] == first(test_methods_list(prevind, Tuple{String, Int, Vararg}))
    @test r == 1:7
    @test s[r] == "prevind"
end

for (T, arg) in [(String,"\")\""),(Char, "')'")]
    s = "(1, CompletionFoo.test2($arg,"
    c, r, res = test_complete(s)
    @test length(c) == 1
    @test c[1] == first(test_methods_list(Main.CompletionFoo.test2, Tuple{T, Vararg}))
    @test r == 5:23
    @test s[r] == "CompletionFoo.test2"
end

let s = "(1, CompletionFoo.test2(`')'`,"
    c, r, res = test_complete(s)
    @test length(c) == 1
    @test c[1] == first(test_methods_list(Main.CompletionFoo.test2, Tuple{Cmd, Vararg}))
end

let s = "CompletionFoo.test3([1, 2] .+ CompletionFoo.varfloat,"
    c, r, res = test_complete(s)
    @test !res
    @test only(c) == first(test_methods_list(Main.CompletionFoo.test3, Tuple{Array{Float64, 1}, Float64, Vararg}))
end

let s = "CompletionFoo.test3([1.,2.], 1.,"
    c, r, res = test_complete(s)
    @test !res
    @test c[1] == first(test_methods_list(Main.CompletionFoo.test3, Tuple{Array{Float64, 1}, Float64, Vararg}))
    @test r == 1:19
    @test length(c) == 1
    @test s[r] == "CompletionFoo.test3"
end

let s = "CompletionFoo.test4(\"e\",r\" \","
    c, r, res = test_complete(s)
    @test !res
    @test c[1] == first(test_methods_list(Main.CompletionFoo.test4, Tuple{String, Regex, Vararg}))
    @test r == 1:19
    @test length(c) == 1
    @test s[r] == "CompletionFoo.test4"
end

# (As discussed in #19829, the Base.REPLCompletions.get_type function isn't
#  powerful enough to analyze anonymous functions.)
let s = "CompletionFoo.test5(broadcast((x,y)->x==y, push!(Base.split(\"\",' '),\"\",\"\"), \"\"),"
    c, r, res = test_complete(s)
    @test !res
    @test_broken only(c) == first(test_methods_list(Main.CompletionFoo.test5, Tuple{BitArray{1}, Vararg}))
end

# test partial expression expansion
let s = "CompletionFoo.test5(Bool[x==1 for x=1:4],"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
    @test c[1] == first(test_methods_list(Main.CompletionFoo.test5, Tuple{Array{Bool,1}, Vararg}))
end

let s = "CompletionFoo.test4(CompletionFoo.test_y_array[1]()[1], CompletionFoo.test_y_array[1]()[2], "
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
    @test c[1] == first(test_methods_list(Main.CompletionFoo.test4, Tuple{String, String, Vararg}))
end

# Test that string escaping is handled correct
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

let s = "CompletionFoo.test5(AbstractArray[Bool[]][1],"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
end

let s = "CompletionFoo.test3(@time([1, 2] .+ CompletionFoo.varfloat),"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
end

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
    @test occursin("test(x::AbstractArray{T}, y) where T<:Real", only(c))
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
    @test any(s->occursin("test(args...)", s), c)
    @test any(s->occursin("test11(a::Integer, b, c)", s), c)
end

let s = "CompletionFoo.?(false, \"a\", 3, "
    c, r, res = test_complete_noshift(s)
    @test !res
    @test length(c) == 1
    @test occursin("test11(a::Integer, b, c)", only(c))
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
    @test occursin("test10(s::String...)", only(c))
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
    @test occursin("kwtest3(a::Integer; namedarg, foobar, slurp...)", only(c))
    # the other two kwtest3 methods should not appear because of specificity
end
=#

# For the ".?(" syntax, do not constrain the number of arguments even with a semicolon.
@test test_complete("CompletionFoo.?(; ") ==
      test_complete("CompletionFoo.?(")

#TODO: @test test_complete("CompletionFoo.?(Any[]...; ") == test_complete("CompletionFoo.?(Cmd[]..., ") == test_complete("CompletionFoo.?(")

@test test_complete("CompletionFoo.?()") == test_complete("CompletionFoo.?(;)")

#TODO: @test_nocompletion("CompletionFoo.?(3; len2=5; ")

# https://github.com/JuliaLang/julia/issues/52551
@test !isempty(test_complete("?("))

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
    @test length(c) == 2
    @test all(startswith("test10("), c)
    @test allunique(c)
    @test !any(str->occursin("test10(a::Integer, b::Integer, c)", str), c)
end

# Test method completion with ambiguity
let s = "CompletionFoo.test11(Integer[false][1], Integer[14][1], "
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 3
    @test all(startswith("test11("), c)
    @test allunique(c)
end

let s = "CompletionFoo.test11(Integer[-7][1], Integer[0x6][1], 6,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 2
    @test any(str->occursin("test11(a::Integer, b, c)", str), c)
    @test any(str->occursin("test11(u, v::Integer, w)", str), c)
    @test !any(str->occursin("test11(x::$Int, y::$Int, z)", str), c)
end

let s = "CompletionFoo.test11(3, 4,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 2
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
    c = map(named_completion, c)
    @test "null\"" in [_c.completion for _c in c]
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
        @test !("$s/tmp/" in c)
        @test r === (sizeof(s) + 1):sizeof(s)
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
        @test ["'repl completions'"] == c
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
            s = "\"~user"
            c, r = test_complete(s)
            @test isempty(c)
            rm(dir)
        end
    end

    # Tests detecting of files in the env path (in shell mode)
    mktempdir() do path
        unreadable = joinpath(path, "replcompletion-unreadable")
        file = joinpath(path, "tmp-executable")
        touch(file)
        chmod(file, 0o755)
        mkdir(unreadable)
        hidden_file = joinpath(unreadable, "hidden")
        touch(hidden_file)

        # Create symlink to a file that is in an unreadable directory
        chmod(hidden_file, 0o755)
        chmod(unreadable, 0o000)
        symlink(hidden_file, joinpath(path, "replcompletions-link"))

        try
            # PATH can also contain folders which we aren't actually allowed to read.
            withenv("PATH" => string(path, ":", unreadable)) do
                s = "tmp-execu"
                # Files reachable by PATH are cached async when PATH is seen to have been changed by `complete_path`
                # so changes are unlikely to appear in the first complete. For testing purposes we can wait for
                # caching to finish
                @lock REPL.REPLCompletions.PATH_cache_lock begin
                    # force the next cache update to happen immediately
                    REPL.REPLCompletions.next_cache_update = 0
                end
                c,r = test_scomplete(s)
                timedwait(()->REPL.REPLCompletions.next_cache_update != 0, 5) # wait for caching to complete
                c,r = test_scomplete(s)
                @test "tmp-executable" in c
                @test r == 1:9
                @test s[r] == "tmp-execu"

                c,r = test_scomplete("replcompletions-link")
                @test isempty(c)
            end
        finally
            # If we don't fix the permissions here, our cleanup fails.
            chmod(unreadable, 0o700)
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
                @lock REPL.REPLCompletions.PATH_cache_lock begin
                    # force the next cache update to happen immediately
                    REPL.REPLCompletions.next_cache_update = 0
                end
                c,r = test_scomplete(s)
                timedwait(()->REPL.REPLCompletions.next_cache_update != 0, 5) # wait for caching to complete
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
            c, r = test_complete("\"$(escape_string(path))/selfsym")
            @test c == ["selfsymlink\""]
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
    mkdir(dir)
    cd(path) do
        touch(joinpath(space_folder, "space .file"))

        dir_space = replace(space_folder, " " => "\\ ")
        s = Sys.iswindows() ? "cd $dir_space\\\\space" : "cd $dir_space/space"
        c, r = test_scomplete(s)
        @test s[r] == (Sys.iswindows() ? "$dir_space\\\\space" : "$dir_space/space")
        @test "'$space_folder'/'space .file'" in c
        # Also use shell escape rules within cmd backticks
        s = "`$s"
        c, r = test_scomplete(s)
        @test s[r] == (Sys.iswindows() ? "$dir_space\\\\space" : "$dir_space/space")
        @test "'$space_folder'/'space .file'" in c

        # escape string according to Julia escaping rules
        julia_esc(str) = REPL.REPLCompletions.do_string_escape(str)

        # For normal strings the string should be properly escaped according to
        # the usual rules for Julia strings.
        s = "cd(\"" * julia_esc(joinpath(path, space_folder) * "/space")
        c, r = test_complete(s)
        @test s[r] == "space"
        @test "space .file\"" in c

        # '$' is the only character which can appear in a windows filename and
        # which needs to be escaped in Julia strings (on unix we could do this
        # test with all sorts of special chars)
        touch(joinpath(space_folder, "needs_escape\$.file"))
        escpath = julia_esc(joinpath(path, space_folder) * "/needs_escape\$")
        s = "cd(\"$escpath"
        c, r = test_complete(s)
        @test s[r] == "needs_escape\\\$"
        @test "needs_escape\\\$.file\"" in c

        if !Sys.iswindows()
            touch(joinpath(space_folder, "needs_escape2\n\".file"))
            escpath = julia_esc(joinpath(path, space_folder, "needs_escape2\n\""))
            s = "cd(\"$escpath"
            c, r = test_complete(s)
            @test s[r] == "needs_escape2\\n\\\""
            @test "needs_escape2\\n\\\".file\"" in c

            touch(joinpath(space_folder, "needs_escape3\\.file"))
            escpath = julia_esc(joinpath(path, space_folder, "needs_escape3\\"))
            s = "cd(\"$escpath"
            c, r = test_complete(s)
            @test s[r] == "needs_escape3\\\\"
            @test "needs_escape3\\\\.file\"" in c
        end

        # Test for issue #10324
        s = "cd(\"$space_folder"
        c, r = test_complete(s)
        @test r == 5:14
        @test s[r] == space_folder

        #Test for #18479
        for c in "'`@\$;&"
            test_dir = "test$(c)test"
            mkdir(joinpath(path, test_dir))
            try
                if !(c in ['\'','$']) # As these characters hold special meaning
                    # in shell commands the shell path completion cannot complete
                    # paths with these characters
                    c, r, res = test_scomplete(test_dir)
                    @test c[1] == "'$test_dir/'"
                    @test res
                end
                escdir = julia_esc(test_dir)
                c, r, res = test_complete("\""*escdir)
                @test c[1] == escdir * "/"
                @test res
            finally
                rm(joinpath(path, test_dir), recursive=true)
            end
        end
    end
    rm(dir, recursive=true)
end

# Test tilde path completion
let (c, r, res) = test_complete("\"~/ka8w5rsz")
    if !Sys.iswindows()
        @test res && c == String[homedir() * "/ka8w5rsz"]
    else
        @test !res
    end

    c, r, res = test_complete("\"foo~bar")
    @test !res
end
if !Sys.iswindows()
    # create a dir and file temporarily in the home directory
    path = mkpath(joinpath(homedir(), "Zx6Wa0GkC0"))
    touch(joinpath(path, "my_file"))
    try
        let (c, r, res) = test_complete("\"~/Zx6Wa0GkC")
            @test res
            @test c == String["Zx6Wa0GkC0/"]
        end
        let (c, r, res) = test_complete("\"~/Zx6Wa0GkC0")
            @test res
            @test c == String[homedir() * "/Zx6Wa0GkC0"]
        end
        let (c, r, res) = test_complete("\"~/Zx6Wa0GkC0/my_")
            @test res
            @test c == String["my_file\""]
        end
        let (c, r, res) = test_complete("\"~/Zx6Wa0GkC0/my_file")
            @test res
            @test c == String[homedir() * "/Zx6Wa0GkC0/my_file"]
        end
    finally
        rm(path, recursive=true)
    end
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
        @test r == lastindex(s)-3:lastindex(s)
        @test "../$temp_name/" in c

        s = "cd ../"
        c,r = test_scomplete(s)
        @test r == lastindex(s)+1:lastindex(s)
        @test "$temp_name/" in c

        s = "ls $(file[1:2])"
        c,r = test_scomplete(s)
        @test r == lastindex(s)-1:lastindex(s)
        @test file in c

        s = "cd(\"..\\\\"
        c,r = test_complete(s)
        @test r == lastindex(s)-3:lastindex(s)
        @test "../$temp_name/" in c

        s = "cd(\"../"
        c,r = test_complete(s)
        @test r == lastindex(s)+1:lastindex(s)
        @test "$temp_name/" in c

        s = "cd(\"$(file[1:2])"
        c,r = test_complete(s)
        @test r == lastindex(s) - 1:lastindex(s)
        @test (length(c) > 1 && file in c) || (["$file\""] == c)
    end
    rm(tmp)
end

# issue 51985
let s = "`\\"
    c,r = test_scomplete(s)
    @test r == lastindex(s)+1:lastindex(s)
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
    c = map(x -> named_completion(x).completion, c)
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

@testset "dict_identifier_key" begin
    # Issue #23004: this should not throw:
    @test REPLCompletions.dict_identifier_key("test_dict_ℂ[\\", :other) isa Tuple
    # Issue #55931: neither should this:
    @test REPLCompletions.dict_identifier_key("test_dict_no_length[", :other) isa NTuple{3,Nothing}
end

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

    # Issue #56071: don't complete string and command macros when the input matches the internal name like `r_` to `r"`
    c, r, res = test_complete("CompletionFoo.teststr_")
    @test isempty(c)
    c, r, res = test_complete("CompletionFoo.teststr_s")
    @test isempty(c)
    c, r, res = test_complete("CompletionFoo.testcmd_")
    @test isempty(c)
    c, r, res = test_complete("CompletionFoo.testcmd_c")
    @test isempty(c)
end

@testset "Keyword-argument completion" begin
    c, r = test_complete("CompletionFoo.kwtest3(a;foob")
    @test c == ["foobar="]
    c, r = test_complete("CompletionFoo.kwtest3(a; le")
    @test "length" ∉ c
    @test "length=" ∈ c
    @test "len2=" ∈ c
    @test "len2" ∉ c
    c, r = test_complete("CompletionFoo.kwtest3.(a;\nlength")
    @test "length" ∉ c
    @test "length=" ∈ c
    c, r = test_complete("CompletionFoo.kwtest3(a, length=4, l")
    @test "length" ∈ c
    @test "length=" ∉ c # since it was already used, do not suggest it again
    @test "len2=" ∈ c
    c, r = test_complete("CompletionFoo.kwtest3(a; kwargs..., fo")
    @test "foreach" ∉ c
    @test "foobar=" ∈ c
    c, r = test_complete("CompletionFoo.kwtest3(a; another!kwarg=0, le")
    @test "length" ∉ c
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
    @test "named" ∉ c
    @test "namedarg=" ∈ c
    @test "len2" ∉ c
    c, r = test_complete_foo("kwtest3(blabla; named.")
    @test c == ["len2"]
    c, r = test_complete_foo("kwtest3(blabla; named..., another!")
    @test c == ["another!kwarg="]
    c, r = test_complete_foo("kwtest3(blabla; named..., len")
    @test "length" ∉ c
    @test "length=" ∈ c
    @test "len2=" ∈ c
    c, r = test_complete_foo("kwtest3(1+3im; named")
    @test "named" ∉ c
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

let s = "log(log.(varfloat),"
    c, r = test_complete_foo(s)
    @test !isempty(c)
end

# TODO: this is a bad test
#let s = "log(log.(noexist),"
#    c, r = test_complete_foo(s)
#    @test isempty(c)
#end

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
    m = test_methods_list(Main.CompletionFoo.test, Tuple{Int, Int, Vararg})
    @test length(m) == 2 == length(c)
    @test c[1] == m[1]
    @test c[2] == m[2]
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
    @test c[1] == first(test_methods_list(prevind, Tuple{String, Int, Vararg}))
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

    let m = Module()
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

    let m = Module()
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

@testset "https://github.com/JuliaLang/julia/issues/47593" begin
    let m = Module()
        @eval m begin
            struct TEST_47594
                var"("::Int
            end
            test_47594 = TEST_47594(1)
        end

        c, r = test_complete_context("test_47594.", m)
        @test c == Any["var\"(\""]
    end
end

# https://github.com/JuliaLang/julia/issues/36437
struct Issue36437{T}
    v::T
end
Base.propertynames(::Issue36437) = (:a, :b, :c)
function Base.getproperty(v::Issue36437, s::Symbol)
    if s === :a
        return 1
    elseif s === :b
        return 2
    elseif s === :c
        return getfield(v, :v)
    else
        throw(ArgumentError(lazy"`(v::Issue36437).$s` is not supported"))
    end
end

let s = "Issue36437(42)."
    c, r, res = test_complete_context(s)
    @test res
    for n in ("a", "b", "c")
        @test n in c
    end
end

let s = "Some(Issue36437(42)).value."
    c, r, res = test_complete_context(s)
    @test res
    for n in ("a", "b", "c")
        @test n in c
    end
end

some_issue36437 = Some(Issue36437(42))

let s = "some_issue36437.value."
    c, r, res = test_complete_context(s)
    @test res
    for n in ("a", "b", "c")
        @test n in c
    end
end

# get completions for :toplevel/:tuple expressions
let s = "some_issue36437.value.a, some_issue36437.value."
    c, r, res = test_complete_context(s)
    @test res
    for n in ("a", "b", "c")
        @test n in c
    end
end
let s = "@show some_issue36437.value.a; some_issue36437.value."
    c, r, res = test_complete_context(s)
    @test res
    for n in ("a", "b", "c")
        @test n in c
    end
end
# https://github.com/JuliaLang/julia/issues/51505
let s = "()."
    c, r, res = test_complete_context(s)
    @test res
end

# aggressive concrete evaluation on mutable allocation in `repl_frame`
let s = "Ref(Issue36437(42))[]."
    c, r, res = test_complete_context(s)
    @test res
    for n in ("a", "b", "c")
        @test n in c
    end
    @test "v" ∉ c
end

# concrete evaluation through `getindex`ing dictionary
global_dict = Dict{Symbol, Any}(:r => r"foo")
let s = "global_dict[:r]."
    c, r, res = test_complete_context(s)
    @test res
    for fname in fieldnames(Regex)
        @test String(fname) in c
    end
end
global_dict_nested = Dict{Symbol, Any}(:g => global_dict)
let s = "global_dict_nested[:g][:r]."
    c, r, res = test_complete_context(s)
    @test res
    for fname in fieldnames(Regex)
        @test String(fname) in c
    end
end

# dict completions through nested `getindex`ing
let s = "global_dict_nested["
    c, r, res = test_complete_context(s)
    @test res
    @test ":g]" in c
end
let s = "global_dict_nested[:g]["
    c, r, res = test_complete_context(s)
    @test res
    @test ":r]" in c
end

const global_xs = [Some(42)]
let s = "pop!(global_xs)."
    c, r, res = test_complete_context(s)
    @test res
    @test "value" in c
end
@test length(global_xs) == 1 # the completion above shouldn't evaluate `pop!` call

# https://github.com/JuliaLang/julia/issues/51499
# allow aggressive concrete evaluation for child uncached frames
struct Issue51499CompletionDict
    inner::Dict{Symbol,Any}
    leaf_func # Function that gets invoked on leaf objects before being returned.
    function Issue51499CompletionDict(inner::Dict, leaf_func=identity)
        inner = Dict{Symbol,Any}(Symbol(k) => v for (k, v) in inner)
        return new(inner, leaf_func)
    end
end
function Base.getproperty(tcd::Issue51499CompletionDict, name::Symbol)
    prop = getfield(tcd, :inner)[name]
    isa(prop, Issue51499CompletionDict) && return prop
    return getfield(tcd, :leaf_func)(prop)
end
Base.propertynames(tcd::Issue51499CompletionDict) = keys(getfield(tcd, :inner))

const issue51499 = Ref{Any}(nothing)
tcd3 = Issue51499CompletionDict(
    Dict(:a => 1.0, :b => 2.0),
    function (x)
        issue51499[] = x
        return sin(x)
    end)
tcd2 = Issue51499CompletionDict(
    Dict(:v => tcd3, :w => 1.0))
tcd1 = Issue51499CompletionDict(
    Dict(:x => tcd2, :y => 1.0))
let (c, r, res) = test_complete_context("tcd1.")
    @test res
    @test "x" in c && "y" in c
    @test isnothing(issue51499[])
end
let (c, r, res) = test_complete_context("tcd1.x.")
    @test res
    @test "v" in c && "w" in c
    @test isnothing(issue51499[])
end
let (c, r, res) = test_complete_context("tcd1.x.v.")
    @test res
    @test "a" in c && "b" in c
    @test isnothing(issue51499[])
end
@test tcd1.x.v.a == sin(1.0)
@test issue51499[] == 1.0

# aggressive constant propagation for mutable `Const`s
mutable_const_prop = Dict{Symbol,Any}(:key => Any[Some(r"x")])
getkeyelem(d) = d[:key][1]
let (c, r, res) = test_complete_context("getkeyelem(mutable_const_prop).")
    @test res
    @test "value" in c
end
let (c, r, res) = test_complete_context("getkeyelem(mutable_const_prop).value.")
    @test res
    for name in fieldnames(Regex)
        @test String(name) in c
    end
end

# JuliaLang/julia/#51548
# don't return wrong result due to mutable inconsistency
function issue51548(T, a)
    # if we fold `xs = getindex(T)` to `xs::Const(Vector{T}())`, then we may wrongly
    # constant-fold `isempty(xs)::Const(true)` and return wrong result
    xs = T[]
    if a isa T
        push!(xs, a)
    end
    return Val(isempty(xs))
end;
let inferred = REPL.REPLCompletions.repl_eval_ex(
        :(issue51548(Any, r"issue51548")), @__MODULE__; limit_aggressive_inference=true)
    @test !isnothing(inferred)
    RT = Core.Compiler.widenconst(inferred)
    @test Val{false} <: RT
end
module TestLimitAggressiveInferenceGetProp
global global_var = 1
end
function test_limit_aggressive_inference_getprop()
    return getproperty(TestLimitAggressiveInferenceGetProp, :global_var)
end
let inferred = REPL.REPLCompletions.repl_eval_ex(
        :(test_limit_aggressive_inference_getprop()), @__MODULE__; limit_aggressive_inference=true)
    @test inferred == Core.Const(1)
end

# Test completion of var"" identifiers (#49280)
let s = "var\"complicated "
    c, r = test_complete_foo(s)
    @test c == Any["var\"complicated symbol with spaces\""]
end

for s in ("WeirdNames().var\"oh ", "WeirdNames().var\"")
    c, r = test_complete_foo(s)
    @test c == Any["var\"oh no!\"", "var\"oh yes!\""]
end

# Test completion of non-Expr literals
let s = "\"abc\"."
    c, r = test_complete(s)
    # (no completion, but shouldn't error)
    @test isempty(c)
end

let s = "`abc`.e"
    c, r = test_complete(s)
    # (completions for the fields of `Cmd`)
    @test c == Any["env", "exec"]
end

# suppress false positive field completions (when `getproperty`/`propertynames` is overloaded)
struct Issue51499_2
    inner::Dict{Symbol,Any}
end
Base.getproperty(issue51499::Issue51499_2, name::Symbol) = getfield(issue51499, :inner)[name]
Base.propertynames(issue51499::Issue51499_2) = keys(getfield(issue51499, :inner))
const issue51499_2_1 = Issue51499_2(Dict(:a => nothing))
const issue51499_2_2 = Issue51499_2(Dict(:b => nothing))
let s = "(rand(Bool) ? issue51499_2_1 : issue51499_2_2)."
    c, r, res = test_complete_context(s)
    @test "inner" ∉ c
end

# Test completion for a case when type inference returned `Union` of the same types
union_somes(a, b) = rand() < 0.5 ? Some(a) : Some(b)
let s = "union_somes(1, 1.0)."
    c, r, res = test_complete_context(s)
    @test res
    @test "value" in c
end
union_some_ref(a, b) = rand() < 0.5 ? Some(a) : Ref(b)
let s = "union_some_ref(1, 1.0)."
    c, r, res = test_complete_context(s)
    @test res
    @test "value" in c && "x" in c
end

Issue49892(x) = x
let s = "Issue49892(fal"
    c, r, res = test_complete_context(s)
    @test res
    for n in ("false", "falses")
        @test n in c
    end
end

@testset "public but non-exported symbols only complete qualified (#51331)" begin
    c, r, res = test_complete("ispub")
    @test res
    @test "ispublic" ∉ c

    c, r, res = test_complete("Base.ispub")
    @test res
    @test "ispublic" ∈ c

    @test Base.ispublic(Base, :ispublic)
    # If this last test starts failing, that's okay, just pick a new example symbol:
    @test !Base.isexported(Base, :ispublic)
end

# issue #51194
for (s, compl) in (("2*CompletionFoo.fmsoe", "fmsoebelkv"),
                   (":a isa CompletionFoo.test!1", "test!12"),
                   ("-CompletionFoo.Test_y(3).", "yy"),
                   ("99 ⨷⁻ᵨ⁷ CompletionFoo.type_test.", "xx"),
                   ("CompletionFoo.type_test + CompletionFoo.Test_y(2).", "yy"),
                   ("(CompletionFoo.type_test + CompletionFoo.Test_y(2)).", "xx"),
                   ("CompletionFoo.type_test + CompletionFoo.unicode_αβγ.", "yy"),
                   ("(CompletionFoo.type_test + CompletionFoo.unicode_αβγ).", "xx"),
                   ("foo'CompletionFoo.test!1", "test!12"))
    @testset let s=s, compl=compl
        c, r = test_complete_noshift(s)
        @test length(c) == 1
        @test only(c) == compl
    end
end

# allows symbol completion within incomplete :macrocall
# https://github.com/JuliaLang/julia/issues/51827
macro issue51827(args...)
    length(args) ≥ 2 || error("@issue51827: incomplete arguments")
    return args
end
let s = "@issue51827 Base.ac"
    c, r, res = test_complete_context(s)
    @test res
    @test "acquire" in c
end

let t = REPLCompletions.repl_eval_ex(:(`a b`), @__MODULE__; limit_aggressive_inference=true)
    @test t isa Core.Const
    @test t.val == `a b`
end

# issue #51823
@test "include" in test_complete_context("inc", Main)[1]

# REPL completions should not try to concrete-evaluate !:noub methods
function very_unsafe_method(i::Int)
    xs = Any[]
    @inbounds xs[i]
end
let t = REPLCompletions.repl_eval_ex(:(unsafe_method(42)), @__MODULE__)
    @test isnothing(t)
end

# https://github.com/JuliaLang/julia/issues/52099
const issue52099 = []
let t = REPLCompletions.repl_eval_ex(:(Base.PersistentDict(issue52099 => 3)), @__MODULE__)
    if t isa Core.Const
        @test length(t.val) == 1
    end
end

# test REPLInterpreter effects for `getindex(::Dict, key)`
for (DictT, KeyT) = Any[(Dict{Symbol,Any}, Symbol),
                        (Dict{Int,Any}, Int),
                        (Dict{String,Any}, String)]
    @testset let DictT=DictT, KeyT=KeyT
        effects = Base.infer_effects(getindex, (DictT,KeyT); interp=REPL.REPLCompletions.REPLInterpreter())
        @test Core.Compiler.is_effect_free(effects)
        @test Core.Compiler.is_terminates(effects)
        @test Core.Compiler.is_noub(effects)
        effects = Base.infer_effects((DictT,KeyT); interp=REPL.REPLCompletions.REPLInterpreter()) do d, key
            key in keys(d)
        end
        @test Core.Compiler.is_effect_free(effects)
        @test Core.Compiler.is_terminates(effects)
        @test Core.Compiler.is_noub(effects)
    end
end

# test invalidation support
replinterp_invalidation_callee(c::Bool=rand(Bool)) = Some(c ? r"foo" : r"bar")
replinterp_invalidation_caller() = replinterp_invalidation_callee().value
@test REPLCompletions.repl_eval_ex(:(replinterp_invalidation_caller()), @__MODULE__) == Regex
replinterp_invalidation_callee(c::Bool=rand(Bool)) = Some(c ? "foo" : "bar")
@test REPLCompletions.repl_eval_ex(:(replinterp_invalidation_caller()), @__MODULE__) == String

# JuliaLang/julia#52922
let s = "using Base.Th"
    c, r, res = test_complete_context(s)
    @test res
    @test "Threads" in c
end
let s = "using Base."
    c, r, res = test_complete_context(s)
    @test res
    @test "BinaryPlatforms" in c
end
# JuliaLang/julia#53999
let s = "using Base.Sort, Base.Th"
    c, r, res = test_complete_context(s)
    @test res
    @test "Threads" in c
end
# test cases with the `.` accessor
module Issue52922
module Inner1
module Inner12 end
end
module Inner2 end
end
let s = "using .Iss"
    c, r, res = test_complete_context(s)
    @test res
    @test "Issue52922" in c
end
let s = " using .Iss"
    c, r, res = test_complete_context(s)
    @test res
    @test "Issue52922" in c
end
let s = "@time using .Iss"
    c, r, res = test_complete_context(s)
    @test res
    @test "Issue52922" in c
end
let s = " @time using .Iss"
    c, r, res = test_complete_context(s)
    @test res
    @test "Issue52922" in c
end
let s = "@time(using .Iss"
    c, r, res = test_complete_context(s)
    @test res
    @test "Issue52922" in c
end
let s = "using .Issue52922.Inn"
    c, r, res = test_complete_context(s)
    @test res
    @test "Inner1" in c
end
let s = "using .Issue52922.Inner1."
    c, r, res = test_complete_context(s)
    @test res
    @test "Inner12" in c
end
let s = "using .Inner1.Inn"
    c, r, res = test_complete_context(s, Issue52922)
    @test res
    @test "Inner12" in c
end
let s = "using ..Issue52922.Inn"
    c, r, res = test_complete_context(s, Issue52922.Inner1)
    @test res
    @test "Inner2" in c
end
let s = "using ...Issue52922.Inn"
    c, r, res = test_complete_context(s, Issue52922.Inner1.Inner12)
    @test res
    @test "Inner2" in c
end

struct Issue53126 end
Base.propertynames(::Issue53126) = error("this should not be called")
let s = "Issue53126()."
    c, r, res = test_complete_context(s)
    @test res
    @test isempty(c)
end

# complete explicitly `using`ed names
baremodule TestExplicitUsing
using Base: @assume_effects
end # baremodule TestExplicitUsing
let s = "@assu"
    c, r, res = test_complete_context(s, TestExplicitUsing)
    @test res
    @test "@assume_effects" in c
end
let s = "TestExplicitUsing.@assu"
    c, r, res = test_complete_context(s)
    @test res
    @test "@assume_effects" in c
end
baremodule TestExplicitUsingNegative end
let s = "@assu"
    c, r, res = test_complete_context(s, TestExplicitUsingNegative)
    @test res
    @test "@assume_effects" ∉ c
end
let s = "TestExplicitUsingNegative.@assu"
    c, r, res = test_complete_context(s)
    @test res
    @test "@assume_effects" ∉ c
end
# should complete implicitly `using`ed names
module TestImplicitUsing end
let s = "@asse"
    c, r, res = test_complete_context(s, TestImplicitUsing)
    @test res
    @test "@assert" in c
end
let s = "TestImplicitUsing.@asse"
    c, r, res = test_complete_context(s)
    @test res
    @test "@assert" in c
end

# JuliaLang/julia#23374: completion for `import Mod.name`
module Issue23374
global v23374 = nothing
global w23374 = missing
end
let s = "import .Issue23374.v"
    c, r, res = test_complete_context(s)
    @test res
    @test "v23374" in c
end
let s = "import Base.sin, .Issue23374.v"
    c, r, res = test_complete_context(s)
    @test res
    @test "v23374" in c
end
let s = "using .Issue23374.v"
    c, r, res = test_complete_context(s)
    @test res
    @test isempty(c)
end
# JuliaLang/julia#23374: completion for `using Mod: name`
let s = "using Base: @ass"
    c, r, res = test_complete_context(s)
    @test res
    @test "@assume_effects" in c
end
let s = "using .Issue23374: v"
    c, r, res = test_complete_context(s)
    @test res
    @test "v23374" in c
end
let s = "using .Issue23374: v23374, w"
    c, r, res = test_complete_context(s)
    @test res
    @test "w23374" in c
end
# completes `using ` to `using [list of available modules]`
let s = "using "
    c, r, res = test_complete_context(s)
    @test res
    @test !isempty(c)
end

baremodule _TestInternalBindingOnly
export binding1, binding2
global binding1 = global binding2 = nothing
end
baremodule TestInternalBindingOnly
using .._TestInternalBindingOnly
global binding = nothing
export binding
end
for s = ("TestInternalBindingOnly.bind", "using .TestInternalBindingOnly: bind")
    # when module is explicitly accessed, completion should show internal names only
    let (c, r, res) = test_complete_context(s; shift=false)
        @test res
        @test "binding" ∈ c
        @test "binding1" ∉ c && "binding2" ∉ c
    end
    # unless completion is forced via shift key
    let (c, r, res) = test_complete_context(s, TestInternalBindingOnly)
        @test res
        @test "binding" ∈ c
        @test "binding1" ∈ c && "binding2" ∈ c
    end
end
# without explicit module access, completion should show all available names
let (c, r, res) = test_complete_context("bind", TestInternalBindingOnly; shift=false)
    @test res
    @test "binding" ∈ c
    @test "binding1" ∈ c && "binding2" ∈ c
end
let (c, r, res) = test_complete_context("si", Main; shift=false)
    @test res
    @test "sin" ∈ c
end

let (c, r, res) = test_complete_context("const xxx = Base.si", Main)
    @test res
    @test "sin" ∈ c
end

let (c, r, res) = test_complete_context("global xxx::Number = Base.", Main)
    @test res
    @test "pi" ∈ c
end

# JuliaLang/julia#57780
const issue57780 = ["a", "b", "c"]
const issue57780_orig = copy(issue57780)
test_complete_context("empty!(issue57780).", Main)
@test issue57780 == issue57780_orig
