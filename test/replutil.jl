# This file is a part of Julia. License is MIT: https://julialang.org/license

# For curmod_*
include("testenv.jl")

function test_have_color(buf, color, no_color)
    if Base.have_color
        @test String(take!(buf)) == color
    else
        @test String(take!(buf)) == no_color
    end
end

cfile = " at $(@__FILE__):"
c1line = @__LINE__() + 1
method_c1(x::Float64, s::AbstractString...) = true

buf = IOBuffer()
Base.show_method_candidates(buf, Base.MethodError(method_c1,(1, 1, "")))
no_color = "\nClosest candidates are:\n  method_c1(!Matched::Float64, !Matched::AbstractString...)$cfile$c1line"
@test length(methods(method_c1)) <= 3 # because of '...' in candidate printing
test_have_color(buf,
                "\e[0m\nClosest candidates are:\n  method_c1(\e[1m\e[31m::Float64\e[0m, \e[1m\e[31m::AbstractString...\e[0m)$cfile$c1line\e[0m",
                no_color)

no_color = "\nClosest candidates are:\n  method_c1(!Matched::Float64, ::AbstractString...)$cfile$c1line"
Base.show_method_candidates(buf, Base.MethodError(method_c1,(1, "", "")))
test_have_color(buf,
                "\e[0m\nClosest candidates are:\n  method_c1(\e[1m\e[31m::Float64\e[0m, ::AbstractString...)$cfile$c1line\e[0m",
                no_color)

# should match
no_color = "\nClosest candidates are:\n  method_c1(::Float64, ::AbstractString...)$cfile$c1line"
Base.show_method_candidates(buf, Base.MethodError(method_c1,(1., "", "")))
test_have_color(buf,
                "\e[0m\nClosest candidates are:\n  method_c1(::Float64, ::AbstractString...)$cfile$c1line\e[0m",
                no_color)

# Have no matches so should return empty
Base.show_method_candidates(buf, Base.MethodError(method_c1,(1, 1, 1)))
test_have_color(buf, "", "")

# matches the implicit constructor -> convert method
Base.show_method_candidates(buf, Base.MethodError(Tuple{}, (1, 1, 1)))
let mc = String(take!(buf))
    @test contains(mc, "\nClosest candidates are:\n  Tuple{}")
    @test !contains(mc, cfile)
end

c2line = @__LINE__
method_c2(x::Int32, args...) = true
method_c2(x::Int32, y::Float64, args...) = true
method_c2(x::Int32, y::Float64) = true
method_c2(x::Int32, y::Int32, z::Int32) = true
method_c2(x::T, y::T, z::T) where {T<:Real} = true

Base.show_method_candidates(buf, Base.MethodError(method_c2,(1., 1., 2)))
color = "\e[0m\nClosest candidates are:\n  method_c2(\e[1m\e[31m::Int32\e[0m, ::Float64, ::Any...)$cfile$(c2line+2)\n  method_c2(\e[1m\e[31m::Int32\e[0m, ::Any...)$cfile$(c2line+1)\n  method_c2(::T<:Real, ::T<:Real, \e[1m\e[31m::T<:Real\e[0m)$cfile$(c2line+5)\n  ...\e[0m"
no_color = no_color = "\nClosest candidates are:\n  method_c2(!Matched::Int32, ::Float64, ::Any...)$cfile$(c2line+2)\n  method_c2(!Matched::Int32, ::Any...)$cfile$(c2line+1)\n  method_c2(::T<:Real, ::T<:Real, !Matched::T<:Real) where T<:Real$cfile$(c2line+5)\n  ..."
test_have_color(buf, color, no_color)

c3line = @__LINE__() + 1
method_c3(x::Float64, y::Float64) = true
Base.show_method_candidates(buf, Base.MethodError(method_c3,(1.,)))
color = "\e[0m\nClosest candidates are:\n  method_c3(::Float64, \e[1m\e[31m::Float64\e[0m)$cfile$c3line\e[0m"
no_color = no_color = "\nClosest candidates are:\n  method_c3(::Float64, !Matched::Float64)$cfile$c3line"
test_have_color(buf, color, no_color)

# Test for the method error in issue #8651
c4line = @__LINE__
method_c4() = true
method_c4(x::AbstractString) = false
Base.show_method_candidates(buf, MethodError(method_c4,("",)))
test_have_color(buf,
    "\e[0m\nClosest candidates are:\n  method_c4(::AbstractString)$cfile$(c4line+2)\n  method_c4()$cfile$(c4line+1)\e[0m",
    "\nClosest candidates are:\n  method_c4(::AbstractString)$cfile$(c4line+2)\n  method_c4()$cfile$(c4line+1)")

c5line = @__LINE__() + 1
method_c5(::Type{Float64}) = true
Base.show_method_candidates(buf, MethodError(method_c5,(Float64,)))
test_have_color(buf, "\e[0m\nClosest candidates are:\n  method_c5(::Type{Float64})$cfile$c5line\e[0m",
                "\nClosest candidates are:\n  method_c5(::Type{Float64})$cfile$c5line")

Base.show_method_candidates(buf, MethodError(method_c5,(Int32,)))
test_have_color(buf, "\e[0m\nClosest candidates are:\n  method_c5(\e[1m\e[31m::Type{Float64}\e[0m)$cfile$c5line\e[0m",
                "\nClosest candidates are:\n  method_c5(!Matched::Type{Float64})$cfile$c5line")

mutable struct Test_type end
test_type = Test_type()
for f in [getindex, setindex!]
    Base.show_method_candidates(buf, MethodError(f,(test_type, 1,1)))
    test_have_color(buf, "", "")
end

PR16155line = @__LINE__() + 2
mutable struct PR16155
    a::Int64
    b
end
PR16155line2 = @__LINE__() + 1
(::Type{T})(arg::Any) where {T<:PR16155} = "replace call-to-convert method from sysimg"

Base.show_method_candidates(buf, MethodError(PR16155,(1.0, 2.0, Int64(3))))
test_have_color(buf, "\e[0m\nClosest candidates are:\n  $(curmod_prefix)PR16155(::Any, ::Any)$cfile$PR16155line\n  $(curmod_prefix)PR16155(\e[1m\e[31m::Int64\e[0m, ::Any)$cfile$PR16155line\n  $(curmod_prefix)PR16155(::Any) where T<:$(curmod_prefix)PR16155$cfile$PR16155line2\e[0m",
                     "\nClosest candidates are:\n  $(curmod_prefix)PR16155(::Any, ::Any)$cfile$PR16155line\n  $(curmod_prefix)PR16155(!Matched::Int64, ::Any)$cfile$PR16155line\n  $(curmod_prefix)PR16155(::Any) where T<:$(curmod_prefix)PR16155$cfile$PR16155line2")

Base.show_method_candidates(buf, MethodError(PR16155,(Int64(3), 2.0, Int64(3))))
test_have_color(buf, "\e[0m\nClosest candidates are:\n  $(curmod_prefix)PR16155(::Int64, ::Any)$cfile$PR16155line\n  $(curmod_prefix)PR16155(::Any, ::Any)$cfile$PR16155line\n  $(curmod_prefix)PR16155(::Any) where T<:$(curmod_prefix)PR16155$cfile$PR16155line2\e[0m",
                     "\nClosest candidates are:\n  $(curmod_prefix)PR16155(::Int64, ::Any)$cfile$PR16155line\n  $(curmod_prefix)PR16155(::Any, ::Any)$cfile$PR16155line\n  $(curmod_prefix)PR16155(::Any) where T<:$(curmod_prefix)PR16155$cfile$PR16155line2")

c6line = @__LINE__
method_c6(; x=1) = x
method_c6(a; y=1) = y
m_error = try method_c6(y=1) catch e; e; end
showerror(buf, m_error)
error_out = String(take!(buf))
m_error = try method_c6(1, x=1) catch e; e; end
showerror(buf, m_error)
error_out1 = String(take!(buf))

c6mline = @__LINE__
module TestKWError
method_c6_in_module(; x=1) = x
method_c6_in_module(a; y=1) = y
end
m_error = try TestKWError.method_c6_in_module(y=1) catch e; e; end
showerror(buf, m_error)
error_out2 = String(take!(buf))
m_error = try TestKWError.method_c6_in_module(1, x=1) catch e; e; end
showerror(buf, m_error)
error_out3 = String(take!(buf))

if Base.have_color
    @test contains(error_out, "method_c6(; x)$cfile$(c6line + 1)\e[1m\e[31m got unsupported keyword argument \"y\"\e[0m")
    @test contains(error_out, "method_c6(\e[1m\e[31m::Any\e[0m; y)$cfile$(c6line + 2)")
    @test contains(error_out1, "method_c6(::Any; y)$cfile$(c6line + 2)\e[1m\e[31m got unsupported keyword argument \"x\"\e[0m")
    @test contains(error_out2, "method_c6_in_module(; x)$cfile$(c6mline + 2)\e[1m\e[31m got unsupported keyword argument \"y\"\e[0m")
    @test contains(error_out2, "method_c6_in_module(\e[1m\e[31m::Any\e[0m; y)$cfile$(c6mline + 3)")
    @test contains(error_out3, "method_c6_in_module(::Any; y)$cfile$(c6mline + 3)\e[1m\e[31m got unsupported keyword argument \"x\"\e[0m")
else
    @test contains(error_out, "method_c6(; x)$cfile$(c6line + 1) got unsupported keyword argument \"y\"")
    @test contains(error_out, "method_c6(!Matched::Any; y)$cfile$(c6line + 2)")
    @test contains(error_out1, "method_c6(::Any; y)$cfile$(c6line + 2) got unsupported keyword argument \"x\"")
    @test contains(error_out2, "method_c6_in_module(; x)$cfile$(c6mline + 2) got unsupported keyword argument \"y\"")
    @test contains(error_out2, "method_c6_in_module(!Matched::Any; y)$cfile$(c6mline + 3)")
    @test contains(error_out3, "method_c6_in_module(::Any; y)$cfile$(c6mline + 3) got unsupported keyword argument \"x\"")
end

c7line = @__LINE__() + 1
method_c7(a, b; kargs...) = a
Base.show_method_candidates(buf, MethodError(method_c7, (1, 1)), [(:x, 1), (:y, 2)])
test_have_color(buf, "\e[0m\nClosest candidates are:\n  method_c7(::Any, ::Any; kargs...)$cfile$c7line\e[0m",
                     "\nClosest candidates are:\n  method_c7(::Any, ::Any; kargs...)$cfile$c7line")
c8line = @__LINE__() + 1
method_c8(a, b; y=1, w=1) = a
Base.show_method_candidates(buf, MethodError(method_c8, (1, 1)), [(:x, 1), (:y, 2), (:z, 1), (:w, 1)])
test_have_color(buf, "\e[0m\nClosest candidates are:\n  method_c8(::Any, ::Any; y, w)$cfile$c8line\e[1m\e[31m got unsupported keyword arguments \"x\", \"z\"\e[0m\e[0m",
                     "\nClosest candidates are:\n  method_c8(::Any, ::Any; y, w)$cfile$c8line got unsupported keyword arguments \"x\", \"z\"")

ac15639line = @__LINE__
addConstraint_15639(c::Int32) = c
addConstraint_15639(c::Int64; uncset=nothing) = addConstraint_15639(Int32(c), uncset=uncset)

Base.show_method_candidates(buf, MethodError(addConstraint_15639, (Int32(1),)), [(:uncset, nothing)])
test_have_color(buf, "\e[0m\nClosest candidates are:\n  addConstraint_15639(::Int32)$cfile$(ac15639line + 1)\e[1m\e[31m got unsupported keyword argument \"uncset\"\e[0m\n  addConstraint_15639(\e[1m\e[31m::Int64\e[0m; uncset)$cfile$(ac15639line + 2)\e[0m",
                     "\nClosest candidates are:\n  addConstraint_15639(::Int32)$cfile$(ac15639line + 1) got unsupported keyword argument \"uncset\"\n  addConstraint_15639(!Matched::Int64; uncset)$cfile$(ac15639line + 2)")

macro except_str(expr, err_type)
    return quote
        let err = nothing
            try
                $(esc(expr))
            catch err
            end
            err === nothing && error("expected failure, but no exception thrown")
            @test typeof(err) === $(esc(err_type))
            buf = IOBuffer()
            showerror(buf, err)
            String(take!(buf))
        end
    end
end

macro except_strbt(expr, err_type)
    errmsg = "expected failure, but no exception thrown for $expr"
    return quote
        let err = nothing
            try
                $(esc(expr))
            catch err
            end
            err === nothing && error($errmsg)
            @test typeof(err) === $(esc(err_type))
            buf = IOBuffer()
            showerror(buf, err, catch_backtrace())
            String(take!(buf))
        end
    end
end

macro except_stackframe(expr, err_type)
    return quote
       let err = nothing
           local st
           try
               $(esc(expr))
           catch err
               st = catch_stacktrace()
           end
           err === nothing && error("expected failure, but no exception thrown")
           @test typeof(err) === $(esc(err_type))
           sprint(show, st[1])
       end
    end
end

# Pull Request 11007
abstract type InvokeType11007 end
abstract type MethodType11007 <: InvokeType11007 end
mutable struct InstanceType11007 <: MethodType11007
end
let
    f11007(::MethodType11007) = nothing
    err_str = @except_str(invoke(f11007, Tuple{InvokeType11007},
                                 InstanceType11007()), MethodError)
    @test !contains(err_str, "::$(curmod_prefix)InstanceType11007")
    @test contains(err_str, "::$(curmod_prefix)InvokeType11007")
end

module __tmp_replutil

using Test
import ..@except_str
global +
+() = nothing
err_str = @except_str 1 + 2 MethodError
@test contains(err_str, "import Base.+")

err_str = @except_str Float64[](1) MethodError
@test !contains(err_str, "import Base.Array")

Array() = 1
err_str = @except_str Array(1) MethodError
@test contains(err_str, "import Base.Array")

end

let
    g11007(::AbstractVector) = nothing
    err_str = @except_str g11007([[1] [1]]) MethodError
    @test contains(err_str, "row vector")
    @test contains(err_str, "column vector")
end

struct TypeWithIntParam{T <: Integer} end
let undefvar
    err_str = @except_strbt sqrt(-1) DomainError
    @test contains(err_str, "Try sqrt(Complex(x)).")
    err_str = @except_strbt 2^(-1) DomainError
    @test contains(err_str, "Cannot raise an integer x to a negative power -1")
    err_str = @except_strbt (-1)^0.25 DomainError
    @test contains(err_str, "Exponentiation yielding a complex result requires a complex argument")
    A = zeros(10, 10)
    A[2,1] = 1
    A[1,2] = -1
    err_str = @except_strbt eigmax(A) DomainError
    @test contains(err_str, "DomainError with [0.0 -1.0 …")

    err_str = @except_str (1, 2, 3)[4] BoundsError
    @test err_str == "BoundsError: attempt to access (1, 2, 3)\n  at index [4]"

    err_str = @except_str [5, 4, 3][-2, 1] BoundsError
    @test err_str == "BoundsError: attempt to access 3-element Array{$Int,1} at index [-2, 1]"
    err_str = @except_str [5, 4, 3][1:5] BoundsError
    @test err_str == "BoundsError: attempt to access 3-element Array{$Int,1} at index [1:5]"

    err_str = @except_str 0::Bool TypeError
    @test err_str == "TypeError: non-boolean ($Int) used in boolean context"
    err_str = @except_str 0::AbstractFloat TypeError
    @test err_str == "TypeError: in typeassert, expected AbstractFloat, got $Int"
    err_str = @except_str 0::7 TypeError
    @test err_str == "TypeError: in typeassert, expected Type, got $Int"
    err_str = @except_str "" <: AbstractString TypeError
    @test err_str == "TypeError: in <:, expected Type, got String"
    err_str = @except_str AbstractString <: "" TypeError
    @test err_str == "TypeError: in <:, expected Type, got String"
    err_str = @except_str Type{""} TypeError
    @test err_str == "TypeError: in Type, in parameter, expected Type, got String"
    err_str = @except_str TypeWithIntParam{Any} TypeError
    @test err_str == "TypeError: in TypeWithIntParam, in T, expected T<:Integer, got Type{Any}"

    err_str = @except_str mod(1,0) DivideError
    @test err_str == "DivideError: integer division error"
    err_str = @except_str Array{Any,1}(1)[1] UndefRefError
    @test err_str == "UndefRefError: access to undefined reference"
    err_str = @except_str undefvar UndefVarError
    @test err_str == "UndefVarError: undefvar not defined"
    err_str = @except_str read(IOBuffer(), UInt8) EOFError
    @test err_str == "EOFError: read end of file"
    err_str = @except_str Dict()[:doesnotexist] KeyError
    @test err_str == "KeyError: key :doesnotexist not found"
    err_str = @except_str throw(InterruptException()) InterruptException
    @test err_str == "InterruptException:"
    err_str = @except_str throw(ArgumentError("not an error")) ArgumentError
    @test err_str == "ArgumentError: not an error"
    err_str = @except_str @assert(false) AssertionError
    @test err_str == "AssertionError: false"
end


# issue 11845
let buf = IOBuffer()
    showerror(buf, MethodError(convert, (3, 1.0)))
    showerror(buf, MethodError(convert, (Int, 1.0)))
    showerror(buf, MethodError(convert, Tuple{Type, Float64}))
    showerror(buf, MethodError(convert, Tuple{DataType, Float64}))
end

# Issue #14884
primitive type EightBitType 8 end
primitive type EightBitTypeT{T} 8 end
struct FunctionLike <: Function; end
let err_str,
    i = reinterpret(EightBitType, 0x54),
    j = reinterpret(EightBitTypeT{Int32}, 0x54)

    err_str = @except_str Bool() MethodError
    @test contains(err_str, "MethodError: no method matching Bool()")
    err_str = @except_str :a() MethodError
    @test contains(err_str, "MethodError: objects of type Symbol are not callable")
    err_str = @except_str EightBitType() MethodError
    @test contains(err_str, "MethodError: no method matching $(curmod_prefix)EightBitType()")
    err_str = @except_str i() MethodError
    @test contains(err_str, "MethodError: objects of type $(curmod_prefix)EightBitType are not callable")
    err_str = @except_str EightBitTypeT() MethodError
    @test contains(err_str, "MethodError: no method matching $(curmod_prefix)EightBitTypeT()")
    err_str = @except_str EightBitTypeT{Int32}() MethodError
    @test contains(err_str, "MethodError: no method matching $(curmod_prefix)EightBitTypeT{Int32}()")
    err_str = @except_str j() MethodError
    @test contains(err_str, "MethodError: objects of type $(curmod_prefix)EightBitTypeT{Int32} are not callable")
    err_str = @except_str FunctionLike()() MethodError
    @test contains(err_str, "MethodError: no method matching (::$(curmod_prefix)FunctionLike)()")
    err_str = @except_str [1,2](1) MethodError
    @test contains(err_str, "MethodError: objects of type Array{$Int,1} are not callable\nUse square brackets [] for indexing an Array.")
    # Issue 14940
    err_str = @except_str randn(1)() MethodError
    @test contains(err_str, "MethodError: objects of type Array{Float64,1} are not callable")
end
@test stringmime("text/plain", FunctionLike()) == "(::$(curmod_prefix)FunctionLike) (generic function with 0 methods)"
@test ismatch(r"^@doc \(macro with \d+ method[s]?\)$", stringmime("text/plain", getfield(Base, Symbol("@doc"))))

method_defs_lineno = @__LINE__() + 1
Base.Symbol() = throw(ErrorException("1"))
(::Symbol)() = throw(ErrorException("2"))
EightBitType() = throw(ErrorException("3"))
(::EightBitType)() = throw(ErrorException("4"))
EightBitTypeT() = throw(ErrorException("5"))
EightBitTypeT{T}() where {T} = throw(ErrorException("6"))
(::EightBitTypeT)() = throw(ErrorException("7"))
(::FunctionLike)() = throw(ErrorException("8"))


let err_str,
    i = reinterpret(EightBitType, 0x54),
    j = reinterpret(EightBitTypeT{Int32}, 0x54),
    sp = Base.source_path()
    sn = basename(sp)

    @test sprint(show, which(Symbol, Tuple{})) ==
        "Symbol() in $curmod_str at $sp:$(method_defs_lineno + 0)"
    @test sprint(show, which(:a, Tuple{})) ==
        "(::Symbol)() in $curmod_str at $sp:$(method_defs_lineno + 1)"
    @test sprint(show, which(EightBitType, Tuple{})) ==
        "$(curmod_prefix)EightBitType() in $curmod_str at $sp:$(method_defs_lineno + 2)"
    @test sprint(show, which(reinterpret(EightBitType, 0x54), Tuple{})) ==
        "(::$(curmod_prefix)EightBitType)() in $curmod_str at $sp:$(method_defs_lineno + 3)"
    @test sprint(show, which(EightBitTypeT, Tuple{})) ==
        "(::Type{$(curmod_prefix)EightBitTypeT})() in $curmod_str at $sp:$(method_defs_lineno + 4)"
    @test sprint(show, which(EightBitTypeT{Int32}, Tuple{})) ==
        "(::Type{$(curmod_prefix)EightBitTypeT{T}})() where T in $curmod_str at $sp:$(method_defs_lineno + 5)"
    @test sprint(show, which(reinterpret(EightBitTypeT{Int32}, 0x54), Tuple{})) ==
        "(::$(curmod_prefix)EightBitTypeT)() in $curmod_str at $sp:$(method_defs_lineno + 6)"
    @test startswith(sprint(show, which(getfield(Base, Symbol("@doc")), Tuple{LineNumberNode, Module, Vararg{Any}})),
                     "@doc(__source__::LineNumberNode, __module__::Module, x...) in Core at boot.jl:")
    @test startswith(sprint(show, which(FunctionLike(), Tuple{})),
                     "(::$(curmod_prefix)FunctionLike)() in $curmod_str at $sp:$(method_defs_lineno + 7)")
    @test stringmime("text/plain", FunctionLike()) == "(::$(curmod_prefix)FunctionLike) (generic function with 1 method)"
    @test stringmime("text/plain", Core.arraysize) == "arraysize (built-in function)"

    err_str = @except_stackframe Symbol() ErrorException
    @test err_str == "Symbol() at $sn:$(method_defs_lineno + 0)"
    err_str = @except_stackframe :a() ErrorException
    @test err_str == "(::Symbol)() at $sn:$(method_defs_lineno + 1)"
    err_str = @except_stackframe EightBitType() ErrorException
    @test err_str == "$(curmod_prefix)EightBitType() at $sn:$(method_defs_lineno + 2)"
    err_str = @except_stackframe i() ErrorException
    @test err_str == "(::$(curmod_prefix)EightBitType)() at $sn:$(method_defs_lineno + 3)"
    err_str = @except_stackframe EightBitTypeT() ErrorException
    @test err_str == "$(curmod_prefix)EightBitTypeT() at $sn:$(method_defs_lineno + 4)"
    err_str = @except_stackframe EightBitTypeT{Int32}() ErrorException
    @test err_str == "$(curmod_prefix)EightBitTypeT{Int32}() at $sn:$(method_defs_lineno + 5)"
    err_str = @except_stackframe j() ErrorException
    @test err_str == "(::$(curmod_prefix)EightBitTypeT{Int32})() at $sn:$(method_defs_lineno + 6)"
    err_str = @except_stackframe FunctionLike()() ErrorException
    @test err_str == "(::$(curmod_prefix)FunctionLike)() at $sn:$(method_defs_lineno + 7)"
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

# Issue #14684: `display` should print associative types in full.
let d = Dict(1 => 2, 3 => 45),
    buf = IOBuffer(),
    td = TextDisplay(buf)

    display(td, d)
    result = String(take!(td.io))
    @test contains(result, summary(d))

    # Is every pair in the string?
    for el in d
        @test contains(result, string(el))
    end
end

# Issue #20108
let err, buf = IOBuffer()
    try Array() catch err end
    Base.show_method_candidates(buf,err)
    @test isa(err, MethodError)
    @test contains(String(take!(buf)), "Closest candidates are:")
end

# Issue 20111
let K20111(x) = y -> x, buf = IOBuffer()
    show(buf, methods(K20111(1)))
    @test contains(String(take!(buf)), " 1 method for generic function")
end

# @macroexpand tests
macro seven_dollar(ex)
    # simonbyrne example 18240
    isa(ex,Expr) && ex.head == :$ ? 7 : esc(ex)
end

let
    @test (@macroexpand @macroexpand x) == macroexpand(@__MODULE__, :(@macroexpand x))
    @test (@macroexpand  :(1+$y) ) == macroexpand(@__MODULE__, :( :(1+ $y) ))
    @test (@macroexpand @fastmath 1+2    ) == :(Base.FastMath.add_fast(1,2))
    @test (@macroexpand @fastmath +      ) == :(Base.FastMath.add_fast)
    @test (@macroexpand @fastmath min(1) ) == :(Base.FastMath.min_fast(1))
    let err = try; @macroexpand @doc "" f() = @x; catch ex; ex; end
        file, line = @__FILE__, @__LINE__() - 1
        err = err::LoadError
        @test err.file == file && err.line == line
        err = err.error::LoadError
        @test err.file == file && err.line == line
        err = err.error::UndefVarError
        @test err == UndefVarError(Symbol("@x"))
    end
    @test (@macroexpand @seven_dollar $bar) == 7
    x = 2
    @test (@macroexpand @seven_dollar 1+$x) == :(1 + $(Expr(:$, :x)))
end

macro nest1(code)
    code
end

macro nest2(code)
    :(@nest1 $code)
end

macro nest2b(code)
    :(@nest1($code); @nest1($code))
end

@testset "@macroexpand1" begin
    M = @__MODULE__
    _macroexpand1(ex) = macroexpand(M, ex, recursive=false)
    ex = :(@nest1 42)
    @test _macroexpand1(ex) == macroexpand(M,ex)
    ex = :(@nest2 42)
    @test _macroexpand1(ex) != macroexpand(M,ex)
    @test _macroexpand1(_macroexpand1(ex)) == macroexpand(M,ex)
    ex = :(@nest2b 42)
    @test _macroexpand1(ex) != macroexpand(M,ex)
    @test _macroexpand1(_macroexpand1(ex)) == macroexpand(M, ex)
    @test (@macroexpand1 @nest2b 42) == _macroexpand1(ex)
end

foo_9965(x::Float64; w=false) = x
foo_9965(x::Int) = 2x

@testset "closest candidates kwarg #9965" begin
    ex = try
        foo_9965(1, w=true)
    catch e
        e
    end
    @test typeof(ex) == MethodError
    io = IOBuffer()
    Base.show_method_candidates(io, ex, [(:w,true)])
    @test contains(String(take!(io)), "got unsupported keyword argument \"w\"")
end

# Issue #20556
module EnclosingModule
    abstract type AbstractTypeNoConstructors end
end
let
    method_error = MethodError(EnclosingModule.AbstractTypeNoConstructors, ())

    # Test that it shows a special message when no constructors have been defined by the user.
    @test sprint(showerror, method_error) ==
        "MethodError: no constructors have been defined for $(EnclosingModule.AbstractTypeNoConstructors)"

    # Does it go back to previous behaviour when there *is* at least
    # one constructor defined?
    EnclosingModule.AbstractTypeNoConstructors(x, y) = x + y
    @test startswith(sprint(showerror, method_error),
        "MethodError: no method matching $(EnclosingModule.AbstractTypeNoConstructors)()")

    # Test that the 'default' sysimg.jl method is not displayed.
    @test !contains(sprint(showerror, method_error), "where T at sysimg.jl")

    # Test that tab-completion will not show the 'default' sysimg.jl method.
    for method_string in Base.REPLCompletions.complete_methods(:(EnclosingModule.AbstractTypeNoConstructors()))
        @test !startswith(method_string, "(::Type{T})(arg) where T in Base at sysimg.jl")
    end
end

@testset "show for manually thrown MethodError" begin
    global f21006

    f21006() = nothing
    # Normal method error should not warn about world age.
    ex1 = try
        f21006(())
    catch e
        e
    end::MethodError
    str = sprint(Base.showerror, ex1)
    @test startswith(str, "MethodError: no method matching f21006(::Tuple{})")
    @test !contains(str, "The applicable method may be too new")

    # If newer applicable methods are available, world age should be mentioned.
    f21006(x) = x
    @test f21006(()) === ()
    str = sprint(Base.showerror, ex1)
    @test startswith(str, "MethodError: no method matching f21006(::Tuple{})")
    @test contains(str, "The applicable method may be too new: running in world age $(ex1.world)")

    # This method error should be thrown in a world new enough for `f21006(())`.
    # Also makes sure it's printed correctly.
    ex2 = try
        f21006((), ())
    catch e
        e
    end::MethodError
    str = sprint(Base.showerror, ex2)
    @test startswith(str, "MethodError: no method matching f21006(::Tuple{}, ::Tuple{})")
    @test !contains(str, "The applicable method may be too new")

    # If the method is available in the exception world or if the exception world is invalid,
    # don't warn about world age
    for ex3 in (MethodError(ex1.f, ex1.args, ex2.world),
                MethodError(ex1.f, ex1.args, typemax(UInt)))
        str = sprint(Base.showerror, ex3)
        @test startswith(str, "MethodError: no method matching f21006(::Tuple{})")
        @test !contains(str, "The applicable method may be too new")
    end
end

# issue #22798
@generated f22798(x::Integer, y) = :x
let buf = IOBuffer()
    show(buf, methods(f22798))
    @test contains(String(take!(buf)), "f22798(x::Integer, y)")
end

@testset "Dict printing with limited rows" begin
    local buf
    buf = IOBuffer()
    io = IOContext(buf, :displaysize => (4, 80), :limit => true)
    d = Base.ImmutableDict(1=>2)
    show(io, MIME"text/plain"(), d)
    @test String(take!(buf)) == "Base.ImmutableDict{$Int,$Int} with 1 entry: …"
    show(io, MIME"text/plain"(), keys(d))
    @test String(take!(buf)) ==
        "Base.KeyIterator for a Base.ImmutableDict{$Int,$Int} with 1 entry. Keys: …"

    io = IOContext(io, :displaysize => (5, 80))
    show(io, MIME"text/plain"(), d)
    @test String(take!(buf)) == "Base.ImmutableDict{$Int,$Int} with 1 entry:\n  1 => 2"
    show(io, MIME"text/plain"(), keys(d))
    @test String(take!(buf)) ==
        "Base.KeyIterator for a Base.ImmutableDict{$Int,$Int} with 1 entry. Keys:\n  1"
    d = Base.ImmutableDict(d, 3=>4)
    show(io, MIME"text/plain"(), d)
    @test String(take!(buf)) == "Base.ImmutableDict{$Int,$Int} with 2 entries:\n  ⋮ => ⋮"
    show(io, MIME"text/plain"(), keys(d))
    @test String(take!(buf)) ==
        "Base.KeyIterator for a Base.ImmutableDict{$Int,$Int} with 2 entries. Keys:\n  ⋮"

    io = IOContext(io, :displaysize => (6, 80))
    show(io, MIME"text/plain"(), d)
    @test String(take!(buf)) ==
        "Base.ImmutableDict{$Int,$Int} with 2 entries:\n  3 => 4\n  1 => 2"
    show(io, MIME"text/plain"(), keys(d))
    @test String(take!(buf)) ==
        "Base.KeyIterator for a Base.ImmutableDict{$Int,$Int} with 2 entries. Keys:\n  3\n  1"
    d = Base.ImmutableDict(d, 5=>6)
    show(io, MIME"text/plain"(), d)
    @test String(take!(buf)) ==
        "Base.ImmutableDict{$Int,$Int} with 3 entries:\n  5 => 6\n  ⋮ => ⋮"
    show(io, MIME"text/plain"(), keys(d))
    @test String(take!(buf)) ==
        "Base.KeyIterator for a Base.ImmutableDict{$Int,$Int} with 3 entries. Keys:\n  5\n  ⋮"
end
