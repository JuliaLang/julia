# This file is a part of Julia. License is MIT: http://julialang.org/license

function test_have_color(buf, color, no_color)
    if Base.have_color
        @test takebuf_string(buf) == color
    else
        @test takebuf_string(buf) == no_color
    end
end

method_c1(x::Float64, s::AbstractString...) = true

buf = IOBuffer()
Base.show_method_candidates(buf, Base.MethodError(method_c1,(1, 1, "")))
no_color = "\nClosest candidates are:\n  method_c1(!Matched::Float64, !Matched::AbstractString...)"
test_have_color(buf,
                "\e[0m\nClosest candidates are:\n  method_c1(\e[1m\e[31m::Float64\e[0m, \e[1m\e[31m::AbstractString...\e[0m)\e[0m",
                no_color)

no_color = "\nClosest candidates are:\n  method_c1(!Matched::Float64, ::AbstractString...)"
Base.show_method_candidates(buf, Base.MethodError(method_c1,(1, "", "")))
test_have_color(buf,
                "\e[0m\nClosest candidates are:\n  method_c1(\e[1m\e[31m::Float64\e[0m, ::AbstractString...)\e[0m",
                no_color)

# should match
no_color = "\nClosest candidates are:\n  method_c1(::Float64, ::AbstractString...)"
Base.show_method_candidates(buf, Base.MethodError(method_c1,(1., "", "")))
test_have_color(buf,
                "\e[0m\nClosest candidates are:\n  method_c1(::Float64, ::AbstractString...)\e[0m",
                no_color)

# Have no matches so should return empty
Base.show_method_candidates(buf, Base.MethodError(method_c1,(1, 1, 1)))
test_have_color(buf, "", "")

method_c2(x::Int32, args...) = true
method_c2(x::Int32, y::Float64 ,args...) = true
method_c2(x::Int32, y::Float64) = true
method_c2{T<:Real}(x::T, y::T, z::T) = true

Base.show_method_candidates(buf, Base.MethodError(method_c2,(1., 1., 2)))
color = "\e[0m\nClosest candidates are:\n  method_c2(\e[1m\e[31m::Int32\e[0m, ::Float64, ::Any...)\n  method_c2(\e[1m\e[31m::Int32\e[0m, ::Any...)\n  method_c2{T<:Real}(::T<:Real, ::T<:Real, \e[1m\e[31m::T<:Real\e[0m)\n  ...\e[0m"
no_color = no_color = "\nClosest candidates are:\n  method_c2(!Matched::Int32, ::Float64, ::Any...)\n  method_c2(!Matched::Int32, ::Any...)\n  method_c2{T<:Real}(::T<:Real, ::T<:Real, !Matched::T<:Real)\n  ..."
test_have_color(buf, color, no_color)

method_c3(x::Float64, y::Float64) = true
Base.show_method_candidates(buf, Base.MethodError(method_c3,(1.,)))
color = "\e[0m\nClosest candidates are:\n  method_c3(::Float64, \e[1m\e[31m::Float64\e[0m)\e[0m"
no_color = no_color = "\nClosest candidates are:\n  method_c3(::Float64, !Matched::Float64)"
test_have_color(buf, color, no_color)

# Test for the method error in issue #8651
Base.show_method_candidates(buf, MethodError(readline,("",)))
test_have_color(buf, "\e[0m\nClosest candidates are:\n  readline(::AbstractString)\e[0m", "\nClosest candidates are:\n  readline(::AbstractString)")

method_c4(::Type{Float64}) = true
Base.show_method_candidates(buf, MethodError(method_c4,(Float64,)))
test_have_color(buf, "\e[0m\nClosest candidates are:\n  method_c4(::Type{Float64})\e[0m",
                "\nClosest candidates are:\n  method_c4(::Type{Float64})")

Base.show_method_candidates(buf, MethodError(method_c4,(Int32,)))
test_have_color(buf, "", "")

type Test_type end
test_type = Test_type()
for f in [getindex, setindex!]
    Base.show_method_candidates(buf, MethodError(f,(test_type, 1,1)))
    test_have_color(buf, "", "")
end

macro except_str(expr, err_type)
    return quote
        let
            local err::$(esc(err_type))
            try
                $(esc(expr))
            catch err
            end
            @test typeof(err) === $err_type
            buff = IOBuffer()
            showerror(buff, err)
            takebuf_string(buff)
        end
    end
end

macro except_stackframe(expr, err_type)
    return quote
       let
           local st
           try
               $(esc(expr))
           catch err
               st = catch_stacktrace()
               @test typeof(err) === $err_type
           end
           sprint(show, st[1])
       end
    end
end

# Pull Request 11007
abstract InvokeType11007
abstract MethodType11007 <: InvokeType11007
type InstanceType11007 <: MethodType11007
end
let
    f11007(::MethodType11007) = nothing
    err_str = @except_str(invoke(f11007, Tuple{InvokeType11007},
                                 InstanceType11007()), MethodError)
    @test !contains(err_str, "::InstanceType11007")
    @test contains(err_str, "::InvokeType11007")
end

module __tmp_replutil
using Base.Test
import Main.@except_str
global +
+() = nothing
err_str = @except_str 1 + 2 MethodError
@test contains(err_str, "Base.+")
end

let
    g11007(::AbstractVector) = nothing
    err_str = @except_str g11007([[1] [1]]) MethodError
    @test contains(err_str, "row vector")
    @test contains(err_str, "column vector")
end

abstract T11007
let
    err_str = @except_str T11007() MethodError
    @test contains(err_str, "no method matching T11007()")
end

# issue 11845
let
    buff = IOBuffer()
    showerror(buff, MethodError(convert, (3, 1.0)))
    showerror(buff, MethodError(convert, (Int, 1.0)))
    showerror(buff, MethodError(convert, Tuple{Type, Float64}))
    showerror(buff, MethodError(convert, Tuple{DataType, Float64}))
end

# Issue #14884
bitstype 8 EightBitType
bitstype 8 EightBitTypeT{T}
immutable FunctionLike <: Function; end
let err_str,
    i = reinterpret(EightBitType, 0x54),
    j = reinterpret(EightBitTypeT{Int32}, 0x54)

    err_str = @except_str Symbol() MethodError
    @test contains(err_str, "no method matching Symbol()")
    err_str = @except_str :a() MethodError
    @test contains(err_str, "::Symbol is not callable")
    err_str = @except_str EightBitType() MethodError
    @test contains(err_str, "no method matching EightBitType()")
    err_str = @except_str i() MethodError
    @test contains(err_str, "::EightBitType is not callable")
    err_str = @except_str EightBitTypeT() MethodError
    @test contains(err_str, "no method matching EightBitTypeT{T}()")
    err_str = @except_str EightBitTypeT{Int32}() MethodError
    @test contains(err_str, "no method matching EightBitTypeT{Int32}()")
    err_str = @except_str j() MethodError
    @test contains(err_str, "::EightBitTypeT{Int32} is not callable")
    err_str = @except_str FunctionLike()() MethodError
    @test contains(err_str, "no method matching (::FunctionLike)()")
end
@test stringmime("text/plain", FunctionLike()) == "(::FunctionLike) (generic function with 0 methods)"
@test ismatch(r"@doc \(macro with \d+ method[s]?\)", stringmime("text/plain", Base.(symbol("@doc"))))

method_defs_lineno = @__LINE__
Base.Symbol() = error(1)
(::Symbol)() = error(2)
EightBitType() = error(3)
(::EightBitType)() = error(4)
EightBitTypeT() = error(5)
(::Type{EightBitTypeT{T}}){T}() = error(6)
(::EightBitTypeT)() = error(7)
(::FunctionLike)() = error(8)


let err_str,
    i = reinterpret(EightBitType, 0x54),
    j = reinterpret(EightBitTypeT{Int32}, 0x54),
    sp = Base.source_path()
    sn = basename(sp)

    @test sprint(show, which(Symbol, Tuple{})) == "Symbol() at $sp:$(method_defs_lineno + 0)"
    @test sprint(show, which(:a, Tuple{})) == "(::Symbol)() at $sp:$(method_defs_lineno + 1)"
    @test sprint(show, which(EightBitType, Tuple{})) == "EightBitType() at $sp:$(method_defs_lineno + 2)"
    @test sprint(show, which(reinterpret(EightBitType, 0x54), Tuple{})) == "(::EightBitType)() at $sp:$(method_defs_lineno + 3)"
    @test sprint(show, which(EightBitTypeT, Tuple{})) == "(::Type{EightBitTypeT{T<:Any}})() at $sp:$(method_defs_lineno + 4)"
    @test sprint(show, which(EightBitTypeT{Int32}, Tuple{})) == "(::Type{EightBitTypeT{T}}){T}() at $sp:$(method_defs_lineno + 5)"
    @test sprint(show, which(reinterpret(EightBitTypeT{Int32}, 0x54), Tuple{})) == "(::EightBitTypeT{T<:Any})() at $sp:$(method_defs_lineno + 6)"
    @test startswith(sprint(show, which(Base.(symbol("@doc")), Tuple{Vararg{Any}})), "@doc(args...) at docs/bootstrap.jl:")
    @test startswith(sprint(show, which(FunctionLike(), Tuple{})), "(::FunctionLike)() at $sp:$(method_defs_lineno + 7)")
    @test stringmime("text/plain", FunctionLike()) == "(::FunctionLike) (generic function with 1 method)"

    err_str = @except_stackframe Symbol() ErrorException
    @test err_str == " in Symbol() at $sn:$(method_defs_lineno + 0)"
    err_str = @except_stackframe :a() ErrorException
    @test err_str == " in (::Symbol)() at $sn:$(method_defs_lineno + 1)"
    err_str = @except_stackframe EightBitType() ErrorException
    @test err_str == " in EightBitType() at $sn:$(method_defs_lineno + 2)"
    err_str = @except_stackframe i() ErrorException
    @test err_str == " in (::EightBitType)() at $sn:$(method_defs_lineno + 3)"
    err_str = @except_stackframe EightBitTypeT() ErrorException
    @test err_str == " in EightBitTypeT{T}() at $sn:$(method_defs_lineno + 4)"
    err_str = @except_stackframe EightBitTypeT{Int32}() ErrorException
    @test err_str == " in EightBitTypeT{Int32}() at $sn:$(method_defs_lineno + 5)"
    err_str = @except_stackframe j() ErrorException
    @test err_str == " in (::EightBitTypeT{Int32})() at $sn:$(method_defs_lineno + 6)"
    err_str = @except_stackframe FunctionLike()() ErrorException
    @test err_str == " in (::FunctionLike)() at $sn:$(method_defs_lineno + 7)"
end


# Issue #13032
withenv("JULIA_EDITOR" => nothing, "VISUAL" => nothing, "EDITOR" => nothing) do

    # Make sure editor doesn't error when no ENV editor is set.
    @test isa(Base.editor(), Array)

    # Invalid editor
    ENV["JULIA_EDITOR"] = ""
    @test_throws ErrorException Base.editor()

    # Note: The following testcases should work regardless of whether these editors are
    # installed or not.

    # Editor on the path.
    ENV["JULIA_EDITOR"] = "vim"
    @test Base.editor() == ["vim"]

    # Absolute path to editor.
    ENV["JULIA_EDITOR"] = "/usr/bin/vim"
    @test Base.editor() == ["/usr/bin/vim"]

    # Editor on the path using arguments.
    ENV["JULIA_EDITOR"] = "subl -w"
    @test Base.editor() == ["subl", "-w"]

    # Absolute path to editor with spaces.
    ENV["JULIA_EDITOR"] = "/Applications/Sublime\\ Text.app/Contents/SharedSupport/bin/subl"
    @test Base.editor() == ["/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl"]

    # Paths with spaces and arguments (#13032).
    ENV["JULIA_EDITOR"] = "/Applications/Sublime\\ Text.app/Contents/SharedSupport/bin/subl -w"
    @test Base.editor() == ["/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl", "-w"]

    ENV["JULIA_EDITOR"] = "'/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl' -w"
    @test Base.editor() == ["/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl", "-w"]

    ENV["JULIA_EDITOR"] = "\"/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl\" -w"
    @test Base.editor() == ["/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl", "-w"]
end


# Issue 14940
let
    err_str = @except_str randn(1)() MethodError
    @test contains(err_str, "::Array{Float64,1} is not callable")
end
