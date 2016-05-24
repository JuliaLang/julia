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
method_c2(x::Int32, y::Float64, args...) = true
method_c2(x::Int32, y::Float64) = true
method_c2(x::Int32, y::Int32, z::Int32) = true
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
method_c4() = true
method_c4(x::AbstractString) = false
Base.show_method_candidates(buf, MethodError(method_c4,("",)))
test_have_color(buf, "\e[0m\nClosest candidates are:\n  method_c4(::AbstractString)\n  method_c4()\e[0m", "\nClosest candidates are:\n  method_c4(::AbstractString)\n  method_c4()")

method_c5(::Type{Float64}) = true
Base.show_method_candidates(buf, MethodError(method_c5,(Float64,)))
test_have_color(buf, "\e[0m\nClosest candidates are:\n  method_c5(::Type{Float64})\e[0m",
                "\nClosest candidates are:\n  method_c5(::Type{Float64})")

Base.show_method_candidates(buf, MethodError(method_c5,(Int32,)))
test_have_color(buf, "\e[0m\nClosest candidates are:\n  method_c5(\e[1m\e[31m::Type{Float64}\e[0m)\e[0m",
                "\nClosest candidates are:\n  method_c5(!Matched::Type{Float64})")

type Test_type end
test_type = Test_type()
for f in [getindex, setindex!]
    Base.show_method_candidates(buf, MethodError(f,(test_type, 1,1)))
    test_have_color(buf, "", "")
end

type PR16155
    a::Int64
    b
end

Base.show_method_candidates(buf, MethodError(PR16155,(1.0, 2.0, Int64(3))))
test_have_color(buf, "\e[0m\nClosest candidates are:\n  PR16155(::Any, ::Any)\n  PR16155(\e[1m\e[31m::Int64\e[0m, ::Any)\n  PR16155{T}(::Any)\e[0m",
                     "\nClosest candidates are:\n  PR16155(::Any, ::Any)\n  PR16155(!Matched::Int64, ::Any)\n  PR16155{T}(::Any)")

Base.show_method_candidates(buf, MethodError(PR16155,(Int64(3), 2.0, Int64(3))))
test_have_color(buf, "\e[0m\nClosest candidates are:\n  PR16155(::Int64, ::Any)\n  PR16155(::Any, ::Any)\n  PR16155{T}(::Any)\e[0m",
                     "\nClosest candidates are:\n  PR16155(::Int64, ::Any)\n  PR16155(::Any, ::Any)\n  PR16155{T}(::Any)")

method_c6(; x=1) = x
method_c6(a; y=1) = y
m_error = try method_c6(y=1) catch e; e; end
showerror(buf, m_error)
error_out = takebuf_string(buf)
m_error = try method_c6(1, x=1) catch e; e; end
showerror(buf, m_error)
error_out1 = takebuf_string(buf)

if Base.have_color
    @test contains(error_out, "method_c6(; x)\e[1m\e[31m got an unsupported keyword argument \"y\"\e[0m")
    @test contains(error_out, "method_c6(\e[1m\e[31m::Any\e[0m; y)")
    @test contains(error_out1, "method_c6(::Any; y)\e[1m\e[31m got an unsupported keyword argument \"x\"\e[0m")
else
    @test contains(error_out, "method_c6(; x) got an unsupported keyword argument \"y\"")
    @test contains(error_out, "method_c6(!Matched::Any; y)")
    @test contains(error_out1, "method_c6(::Any; y) got an unsupported keyword argument \"x\"")
end

method_c7(a, b; kargs...) = a
Base.show_method_candidates(buf, MethodError(method_c7, (1, 1)), [(:x, 1), (:y, 2)])
test_have_color(buf, "\e[0m\nClosest candidates are:\n  method_c7(::Any, ::Any; kargs...)\e[0m",
                     "\nClosest candidates are:\n  method_c7(::Any, ::Any; kargs...)")
method_c8(a, b; y=1, w=1) = a
Base.show_method_candidates(buf, MethodError(method_c8, (1, 1)), [(:x, 1), (:y, 2), (:z, 1), (:w, 1)])
test_have_color(buf, "\e[0m\nClosest candidates are:\n  method_c8(::Any, ::Any; y, w)\e[1m\e[31m got an unsupported keyword argument \"x\", \"z\"\e[0m\e[0m",
                     "\nClosest candidates are:\n  method_c8(::Any, ::Any; y, w) got an unsupported keyword argument \"x\", \"z\"")

addConstraint_15639(c::Int32) = c
addConstraint_15639(c::Int64; uncset=nothing) = addConstraint_15639(Int32(c), uncset=uncset)

Base.show_method_candidates(buf, MethodError(addConstraint_15639, (Int32(1),)), [(:uncset, nothing)])
test_have_color(buf, "\e[0m\nClosest candidates are:\n  addConstraint_15639(::Int32)\e[1m\e[31m got an unsupported keyword argument \"uncset\"\e[0m\n  addConstraint_15639(\e[1m\e[31m::Int64\e[0m; uncset)\e[0m",
                     "\nClosest candidates are:\n  addConstraint_15639(::Int32) got an unsupported keyword argument \"uncset\"\n  addConstraint_15639(!Matched::Int64; uncset)")

macro except_str(expr, err_type)
    return quote
        let err = nothing
            try
                $(esc(expr))
            catch err
            end
            err === nothing && error("expected failure, but no exception thrown")
            @test typeof(err) === $(esc(err_type))
            buff = IOBuffer()
            showerror(buff, err)
            takebuf_string(buff)
        end
    end
end

macro except_strbt(expr, err_type)
    return quote
        let err = nothing
            try
                $(esc(expr))
            catch err
            end
            err === nothing && error("expected failure, but no exception thrown")
            @test typeof(err) === $(esc(err_type))
            buff = IOBuffer()
            showerror(buff, err, catch_backtrace())
            takebuf_string(buff)
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

abstract T11007
let
    err_str = @except_str T11007() MethodError
    @test contains(err_str, "no method matching T11007()")
end

immutable TypeWithIntParam{T <: Integer} end
let undefvar
    err_str = @except_strbt sqrt(-1) DomainError
    @test contains(err_str, "Try sqrt(complex(x)).")
    err_str = @except_strbt 1^(-1) DomainError
    @test contains(err_str, "Cannot raise an integer x to a negative power -n")
    err_str = @except_strbt (-1)^0.25 DomainError
    @test contains(err_str, "Exponentiation yielding a complex result requires a complex argument")

    err_str = @except_str [5,4,3][-2,1] BoundsError
    @test err_str == "BoundsError: attempt to access 3-element Array{$Int,1} at index [-2,1]"
    err_str = @except_str [5,4,3][1:5] BoundsError
    @test err_str == "BoundsError: attempt to access 3-element Array{$Int,1} at index [1:5]"

    err_str = @except_str 0::Bool TypeError
    @test err_str == "TypeError: non-boolean ($Int) used in boolean context"
    err_str = @except_str 0::AbstractFloat TypeError
    @test err_str == "TypeError: typeassert: expected AbstractFloat, got $Int"
    err_str = @except_str 0::7 TypeError
    @test err_str == "TypeError: typeassert: expected Type{T}, got $Int"
    err_str = @except_str "" <: AbstractString TypeError
    @test err_str == "TypeError: subtype: expected Type{T}, got String"
    err_str = @except_str AbstractString <: "" TypeError
    @test err_str == "TypeError: subtype: expected Type{T}, got String"
    err_str = @except_str Type{""} TypeError
    @test err_str == "TypeError: Type: in parameter, expected Type{T}, got String"
    err_str = @except_str TypeWithIntParam{Any} TypeError
    @test err_str == "TypeError: TypeWithIntParam: in T, expected T<:Integer, got Type{Any}"

    err_str = @except_str mod(1,0) DivideError
    @test err_str == "DivideError: integer division error"
    err_str = @except_str Array(Any,1)[1] UndefRefError
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

    err_str = @except_str Bool() MethodError
    @test contains(err_str, "MethodError: no method matching Bool()")
    err_str = @except_str :a() MethodError
    @test contains(err_str, "MethodError: objects of type Symbol are not callable")
    err_str = @except_str EightBitType() MethodError
    @test contains(err_str, "MethodError: no method matching EightBitType()")
    err_str = @except_str i() MethodError
    @test contains(err_str, "MethodError: objects of type EightBitType are not callable")
    err_str = @except_str EightBitTypeT() MethodError
    @test contains(err_str, "MethodError: no method matching EightBitTypeT{T}()")
    err_str = @except_str EightBitTypeT{Int32}() MethodError
    @test contains(err_str, "MethodError: no method matching EightBitTypeT{Int32}()")
    err_str = @except_str j() MethodError
    @test contains(err_str, "MethodError: objects of type EightBitTypeT{Int32} are not callable")
    err_str = @except_str FunctionLike()() MethodError
    @test contains(err_str, "MethodError: no method matching (::FunctionLike)()")
    err_str = @except_str [1,2](1) MethodError
    @test contains(err_str, "MethodError: objects of type Array{$Int,1} are not callable\nUse square brackets [] for indexing an Array.")
    # Issue 14940
    err_str = @except_str randn(1)() MethodError
    @test contains(err_str, "MethodError: objects of type Array{Float64,1} are not callable")
end
@test stringmime("text/plain", FunctionLike()) == "(::FunctionLike) (generic function with 0 methods)"
@test ismatch(r"^@doc \(macro with \d+ method[s]?\)$", stringmime("text/plain", getfield(Base, Symbol("@doc"))))

method_defs_lineno = @__LINE__+1
Base.Symbol() = throw(ErrorException("1"))
(::Symbol)() = throw(ErrorException("2"))
EightBitType() = throw(ErrorException("3"))
(::EightBitType)() = throw(ErrorException("4"))
EightBitTypeT() = throw(ErrorException("5"))
(::Type{EightBitTypeT{T}}){T}() = throw(ErrorException("6"))
(::EightBitTypeT)() = throw(ErrorException("7"))
(::FunctionLike)() = throw(ErrorException("8"))


let err_str,
    i = reinterpret(EightBitType, 0x54),
    j = reinterpret(EightBitTypeT{Int32}, 0x54),
    sp = Base.source_path()
    sn = basename(sp)

    @test sprint(show, which(Symbol, Tuple{})) == "Symbol() at $sp:$(method_defs_lineno + 0)"
    @test sprint(show, which(:a, Tuple{})) == "(::Symbol)() at $sp:$(method_defs_lineno + 1)"
    @test sprint(show, which(EightBitType, Tuple{})) == "EightBitType() at $sp:$(method_defs_lineno + 2)"
    @test sprint(show, which(reinterpret(EightBitType, 0x54), Tuple{})) == "(::EightBitType)() at $sp:$(method_defs_lineno + 3)"
    @test sprint(show, which(EightBitTypeT, Tuple{})) == "(::Type{EightBitTypeT})() at $sp:$(method_defs_lineno + 4)"
    @test sprint(show, which(EightBitTypeT{Int32}, Tuple{})) == "(::Type{EightBitTypeT{T}}){T}() at $sp:$(method_defs_lineno + 5)"
    @test sprint(show, which(reinterpret(EightBitTypeT{Int32}, 0x54), Tuple{})) == "(::EightBitTypeT)() at $sp:$(method_defs_lineno + 6)"
    @test startswith(sprint(show, which(getfield(Base, Symbol("@doc")), Tuple{Vararg{Any}})), "@doc(x...) at boot.jl:")
    @test startswith(sprint(show, which(FunctionLike(), Tuple{})), "(::FunctionLike)() at $sp:$(method_defs_lineno + 7)")
    @test stringmime("text/plain", FunctionLike()) == "(::FunctionLike) (generic function with 1 method)"
    @test stringmime("text/plain", Core.arraysize) == "arraysize (built-in function)"

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

# Issue #14684: `display` should print associative types in full.
let d = Dict(1 => 2, 3 => 45)
    buf = IOBuffer()
    td = TextDisplay(buf)
    display(td, d)
    result = String(td.io)

    @test contains(result, summary(d))

    # Is every pair in the string?
    # Compare by removing spaces
    for el in d
        @test contains(replace(result, " ", ""), string(el))
    end
end
