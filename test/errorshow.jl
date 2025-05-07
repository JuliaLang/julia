# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random, LinearAlgebra

# For curmod_*
include("testenv.jl")

# re-register only the error hints that are being tested here (
Base.Experimental.register_error_hint(Base.noncallable_number_hint_handler, MethodError)
Base.Experimental.register_error_hint(Base.string_concatenation_hint_handler, MethodError)
Base.Experimental.register_error_hint(Base.methods_on_iterable, MethodError)
Base.Experimental.register_error_hint(Base.nonsetable_type_hint_handler, MethodError)
Base.Experimental.register_error_hint(Base.fielderror_listfields_hint_handler, FieldError)
Base.Experimental.register_error_hint(Base.fielderror_dict_hint_handler, FieldError)
@testset "SystemError" begin
    err = try; systemerror("reason", Cint(0)); false; catch ex; ex; end::SystemError
    errs = sprint(Base.showerror, err)
    @test startswith(errs, "SystemError: reason: ")

    err = try; systemerror("reason", Cint(0), extrainfo="addend"); false; catch ex; ex; end::SystemError
    errs = sprint(Base.showerror, err)
    @test startswith(errs, "SystemError (with addend): reason: ")

    err = try
            Libc.errno(0xc0ffee)
            systemerror("reason", true)
            false
        catch ex
            ex
        end::SystemError
    errs = sprint(Base.showerror, err)
    @test startswith(errs, "SystemError: reason: ")

    err = try; Base.windowserror("reason", UInt32(0)); false; catch ex; ex; end::SystemError
    errs = sprint(Base.showerror, err)
    @test startswith(errs, Sys.iswindows() ? "SystemError: reason: " :
        "SystemError (with Base.WindowsErrorInfo(0x00000000, nothing)): reason: ")

    err = try; Base.windowserror("reason", UInt32(0); extrainfo="addend"); false; catch ex; ex; end::SystemError
    errs = sprint(Base.showerror, err)
    @test startswith(errs, Sys.iswindows() ? "SystemError (with addend): reason: " :
        "SystemError (with Base.WindowsErrorInfo(0x00000000, \"addend\")): reason: ")

    @static if Sys.iswindows()
        err = try
                ccall(:SetLastError, stdcall, Cvoid, (UInt32,), 0x00000000)
                Base.windowserror("reason", true)
                false
            catch ex
                ex
            end::SystemError
        errs = sprint(Base.showerror, err)
        @test startswith(errs, "SystemError: reason: ")
    end
end

file = @__FILE__
sep = Base.Filesystem.path_separator
modul = @__MODULE__
Base.stacktrace_contract_userdir() && (file = Base.contractuser(file))
fname = basename(file)
dname = dirname(file)
cmod = "\n   @ $modul"
cfile = " $file:"
c1line = @__LINE__() + 1
method_c1(x::Float64, s::AbstractString...) = true

buf = IOBuffer()
Base.show_method_candidates(buf, Base.MethodError(method_c1,(1, 1, "")))
@test occursin("\n\nClosest candidates are:\n  method_c1(!Matched::Float64, !Matched::AbstractString...)$cmod$cfile$c1line\n", String(take!(buf)))
@test length(methods(method_c1)) <= 3 # because of '...' in candidate printing
Base.show_method_candidates(IOContext(buf, :color => true), Base.MethodError(method_c1,(1, 1, "")))

mod_col = Base.text_colors[Base.STACKTRACE_FIXEDCOLORS[modul]]
@test occursin("\n\n\e[0mClosest candidates are:\n\e[0m  method_c1(\e[91m::Float64\e[39m, \e[91m::AbstractString...\e[39m)\n\e[0m\e[90m   @\e[39m $mod_col$modul\e[39m \e[90m$dname$sep\e[39m\e[90m\e[4m$fname:$c1line\e[24m\e[39m\n", String(take!(buf)))
Base.show_method_candidates(buf, Base.MethodError(method_c1,(1, "", "")))
@test occursin("\n\nClosest candidates are:\n  method_c1(!Matched::Float64, ::AbstractString...)$cmod$cfile$c1line\n", String(take!(buf)))

# should match
Base.show_method_candidates(buf, Base.MethodError(method_c1,(1., "", "")))
@test occursin("\n\nClosest candidates are:\n  method_c1(::Float64, ::AbstractString...)$cmod$cfile$c1line\n", String(take!(buf)))

# Have no matches, but still print up to 3
Base.show_method_candidates(buf, Base.MethodError(method_c1,(1, 1, 1)))
@test occursin("\n\nClosest candidates are:\n  method_c1(!Matched::Float64, !Matched::AbstractString...)$cmod$cfile$c1line\n", String(take!(buf)))

function nomethodsfunc end
Base.show_method_candidates(buf, Base.MethodError(nomethodsfunc,(1, 1, 1)))
@test isempty(String(take!(buf)))

# matches the implicit constructor -> convert method
Base.show_method_candidates(buf, Base.MethodError(Tuple{}, (1, 1, 1)))
let mc = String(take!(buf))
    @test occursin("\nClosest candidates are:\n  (::Type{T})", mc)
    @test !occursin(cfile, mc)
end

c2line = @__LINE__
method_c2(x::Int32, args...) = true
method_c2(x::Int32, y::Float64, args...) = true
method_c2(x::Int32, y::Float64) = true
method_c2(x::Int32, y::Int32, z::Int32) = true
method_c2(x::T, y::T, z::T) where {T<:Real} = true

let s
    Base.show_method_candidates(buf, Base.MethodError(method_c2, (1., 1., 2)))
    s = String(take!(buf))
    @test occursin("\n\nClosest candidates are:\n  ", s)
    @test occursin("\n  method_c2(!Matched::Int32, ::Float64, ::Any...)$cmod$cfile$(c2line+2)\n  ", s)
    @test occursin("\n  method_c2(::T, ::T, !Matched::T) where T<:Real$cmod$cfile$(c2line+5)\n  ", s)
    @test occursin("\n  method_c2(!Matched::Int32, ::Any...)$cmod$cfile$(c2line+1)\n  ", s)
    @test occursin("\n  ...\n", s)
end

c3line = @__LINE__() + 1
method_c3(x::Float64, y::Float64) = true
Base.show_method_candidates(buf, Base.MethodError(method_c3,(1.,)))
@test occursin( "\n\nClosest candidates are:\n  method_c3(::Float64, !Matched::Float64)$cmod$cfile$c3line\n", String(take!(buf)))

# Test for the method error in issue #8651
c4line = @__LINE__
method_c4() = true
method_c4(x::AbstractString) = false
Base.show_method_candidates(buf, MethodError(method_c4,("",)))
@test occursin("\n\nClosest candidates are:\n  method_c4(::AbstractString)$cmod$cfile$(c4line+2)\n  method_c4()$cmod$cfile$(c4line+1)\n", String(take!(buf)))

c5line = @__LINE__() + 1
method_c5(::Type{Float64}) = true
Base.show_method_candidates(buf, MethodError(method_c5,(Float64,)))
@test occursin("\nClosest candidates are:\n  method_c5(::Type{Float64})$cmod$cfile$c5line", String(take!(buf)))

Base.show_method_candidates(buf, MethodError(method_c5,(Int32,)))
@test occursin("\nClosest candidates are:\n  method_c5(!Matched::Type{Float64})$cmod$cfile$c5line", String(take!(buf)))

mutable struct Test_type end
test_type = Test_type()
for f in [getindex, setindex!]
    Base.show_method_candidates(buf, MethodError(f,(test_type, 1,1)))
    @test String(take!(buf)) == ""
end

PR16155line = @__LINE__() + 2
mutable struct PR16155
    a::Int64
    b
end
PR16155line2 = @__LINE__() + 1
(::Type{T})(arg::Any) where {T<:PR16155} = "replace call-to-convert method from sysimg"

Base.show_method_candidates(buf, MethodError(PR16155,(1.0, 2.0, Int64(3))))
@test occursin("\nClosest candidates are:\n  $(curmod_prefix)PR16155(::Any, ::Any)$cmod$cfile$PR16155line\n  $(curmod_prefix)PR16155(!Matched::Int64, ::Any)$cmod$cfile$PR16155line\n  (::Type{T})(::Any) where T<:$(curmod_prefix)PR16155$cmod$cfile$PR16155line2", String(take!(buf)))

Base.show_method_candidates(buf, MethodError(PR16155,(Int64(3), 2.0, Int64(3))))
@test occursin("\nClosest candidates are:\n  $(curmod_prefix)PR16155(::Int64, ::Any)$cmod$cfile$PR16155line\n  $(curmod_prefix)PR16155(::Any, ::Any)$cmod$cfile$PR16155line\n  (::Type{T})(::Any) where T<:$(curmod_prefix)PR16155$cmod$cfile$PR16155line2", String(take!(buf)))

Base.show_method_candidates(buf, MethodError(Complex{T} where T<:Integer, (1.2,)))
@test startswith(String(take!(buf)), "\n\nClosest candidates are:\n  (::Type{T})(::T) where T<:Number")

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

@test occursin("method_c6(; x) got unsupported keyword argument \"y\"$cmod$cfile$(c6line + 1)", error_out)
@test occursin("method_c6(!Matched::Any; y)$cmod$cfile$(c6line + 2)", error_out)
@test occursin("method_c6(::Any; y) got unsupported keyword argument \"x\"$cmod$cfile$(c6line + 2)", error_out1)
@test occursin("method_c6_in_module(; x) got unsupported keyword argument \"y\"$cmod$cfile$(c6mline + 2)", error_out2)
@test occursin("method_c6_in_module(!Matched::Any; y)$cmod$cfile$(c6mline + 3)", error_out2)
@test occursin("method_c6_in_module(::Any; y) got unsupported keyword argument \"x\"$cmod$cfile$(c6mline + 3)", error_out3)

c7line = @__LINE__() + 1
method_c7(a, b; kargs...) = a
Base.show_method_candidates(buf, MethodError(method_c7, (1, 1)), pairs((x = 1, y = 2)))
@test occursin("\nClosest candidates are:\n  method_c7(::Any, ::Any; kargs...)$cmod$cfile$c7line", String(take!(buf)))
c8line = @__LINE__() + 1
method_c8(a, b; y=1, w=1) = a
Base.show_method_candidates(buf, MethodError(method_c8, (1, 1)), pairs((x = 1, y = 2, z = 1, w = 1)))
@test occursin("\nClosest candidates are:\n  method_c8(::Any, ::Any; y, w) got unsupported keyword arguments \"x\", \"z\"$cmod$cfile$c8line", String(take!(buf)))

let no_kwsorter_match, e
    no_kwsorter_match() = 0
    no_kwsorter_match(a;y=1) = y
    e = try no_kwsorter_match(y=1) catch ex; ex; end
    @test occursin(Regex("no method matching.+\\(; y::$(Int)\\)"), sprint(showerror, e))
end

ac15639line = @__LINE__
addConstraint_15639(c::Int32) = c
addConstraint_15639(c::Int64; uncset=nothing) = addConstraint_15639(Int32(c), uncset=uncset)

Base.show_method_candidates(buf, MethodError(addConstraint_15639, (Int32(1),)), pairs((uncset = nothing,)))
@test occursin("\nClosest candidates are:\n  addConstraint_15639(::Int32) got unsupported keyword argument \"uncset\"$cmod$cfile$(ac15639line + 1)\n  addConstraint_15639(!Matched::Int64; uncset)$cmod$cfile$(ac15639line + 2)", String(take!(buf)))

# Busted Vararg method definitions
bad_vararg_decl(x::Int, y::Vararg) = 1   # don't do this, instead use (x::Int, y...)
Base.show_method_candidates(buf, try bad_vararg_decl("hello", 3) catch e e end)
@test occursin("bad_vararg_decl(!Matched::$Int, ::Any...)", String(take!(buf)))

macro except_str(expr, err_type)
    source_info = __source__
    errmsg = "expected failure, but no exception thrown for $expr"
    return quote
        let err = nothing
            try
                $(esc(expr))
            catch err
            end
            err === nothing && error($errmsg)
            @testset let expr=$(repr(expr))
                $(Expr(:macrocall, Symbol("@test"), source_info, :(typeof(err) === $(esc(err_type)))))
            end
            buf = IOBuffer()
            showerror(buf, err)
            String(take!(buf))
        end
    end
end

macro except_strbt(expr, err_type)
    source_info = __source__
    errmsg = "expected failure, but no exception thrown for $expr"
    return quote
        let err = nothing
            try
                $(esc(expr))
            catch err
            end
            err === nothing && error($errmsg)
            @testset let expr=$(repr(expr))
                $(Expr(:macrocall, Symbol("@test"), source_info, :(typeof(err) === $(esc(err_type)))))
            end
            buf = IOBuffer()
            showerror(buf, err, catch_backtrace())
            String(take!(buf))
        end
    end
end

macro except_stackframe(expr, err_type)
    source_info = __source__
    errmsg = "expected failure, but no exception thrown for $expr"
    return quote
       let err = nothing
           local st
           try
               $(esc(expr))
           catch err
               st = stacktrace(catch_backtrace())
           end
           err === nothing && error($errmsg)
           @testset let expr=$(repr(expr))
               $(Expr(:macrocall, Symbol("@test"), source_info, :(typeof(err) === $(esc(err_type)))))
           end
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
    @test !occursin("::$(curmod_prefix)InstanceType11007", err_str)
    @test occursin("::$(curmod_prefix)InvokeType11007", err_str)
end

module __tmp_replutil

using Test
import ..@except_str
global +
+() = nothing
err_str = @except_str 1 + 2 MethodError
@test occursin("import Base.:+", err_str)

err_str = @except_str Float64[](1) MethodError
@test !occursin("import Base.Array", err_str)

global Array
Array() = 1
err_str = @except_str Array([1]) MethodError
@test occursin("import Base.Array", err_str)

end

let
    g11007(::AbstractVector) = nothing
    err_str = @except_str g11007([[1] [1]]) MethodError
    @test occursin("row vector", err_str)
    @test occursin("column vector", err_str)
end

struct TypeWithIntParam{T<:Integer, Vector{T}<:A<:AbstractArray{T}} end
struct Bounded  # not an AbstractArray
    bound::Int
end
Base.getindex(b::Bounded, i) = checkindex(Bool, 1:b.bound, i) || throw(BoundsError(b, i))
Base.summary(io::IO, b::Bounded) = print(io, "$(b.bound)-size Bounded")
let undefvar
    err_str = @except_strbt sqrt(-1) DomainError
    @test occursin("Try sqrt(Complex(x)).", err_str)
    err_str = @except_strbt 2^(1-2) DomainError
    @test occursin("Cannot raise an integer x to a negative power -1", err_str)
    err_str = @except_strbt (-1)^0.25 DomainError
    @test occursin("Exponentiation yielding a complex result requires a complex argument", err_str)
    A = zeros(10, 10)
    A[2,1] = 1
    A[1,2] = -1
    err_str = @except_strbt eigmax(A) DomainError
    @test occursin("DomainError with [0.0 -1.0 …", err_str)

    err_str = @except_str (1, 2, 3)[4] BoundsError
    @test err_str == "BoundsError: attempt to access Tuple{$Int, $Int, $Int} at index [4]"

    err_str = @except_str [5, 4, 3][-2, 1] BoundsError
    @test err_str == "BoundsError: attempt to access 3-element Vector{$Int} at index [-2, 1]"
    err_str = @except_str [5, 4, 3][1:5] BoundsError
    @test err_str == "BoundsError: attempt to access 3-element Vector{$Int} at index [1:5]"
    err_str = @except_str [5, 4, 3][trues(6,7)] BoundsError
    @test err_str == "BoundsError: attempt to access 3-element Vector{$Int} at index [6×7 BitMatrix]"

    err_str = @except_str Bounded(2)[3] BoundsError
    @test err_str == "BoundsError: attempt to access 2-size Bounded at index [3]"

    err_str = @except_str 0::Bool TypeError
    @test err_str == "TypeError: non-boolean ($Int) used in boolean context"
    err_str = @except_str 0::AbstractFloat TypeError
    @test err_str == "TypeError: in typeassert, expected AbstractFloat, got a value of type $Int"
    err_str = @except_str 0::7 TypeError
    @test err_str == "TypeError: in typeassert, expected Type, got a value of type $Int"
    err_str = @except_str "" <: AbstractString TypeError
    @test err_str == "TypeError: in <:, expected Type, got a value of type String"
    err_str = @except_str AbstractString <: "" TypeError
    @test err_str == "TypeError: in <:, expected Type, got a value of type String"
    err_str = @except_str Type{""} TypeError
    @test err_str == "TypeError: in Type, in parameter, expected Type, got a value of type String"
    err_str = @except_str TypeWithIntParam{Any} TypeError
    @test err_str == "TypeError: in TypeWithIntParam, in T, expected T<:Integer, got Type{Any}"
    err_str = @except_str TypeWithIntParam{Int64,Vector{Float64}} TypeError
    @test err_str == "TypeError: in TypeWithIntParam, in A, expected Vector{Int64}<:A<:(AbstractArray{Int64}), got Type{Vector{Float64}}"
    err_str = @except_str TypeWithIntParam{Int64}{Vector{Float64}} TypeError
    @test err_str == "TypeError: in TypeWithIntParam, in A, expected Vector{Int64}<:A<:(AbstractArray{Int64}), got Type{Vector{Float64}}"
    err_str = @except_str Type{Vararg} TypeError
    @test err_str == "TypeError: in Type, in parameter, expected Type, got Vararg"
    err_str = @except_str Ref{Vararg} TypeError
    @test err_str == "TypeError: in Type, in parameter, expected Type, got Vararg"

    err_str = @except_str mod(1,0) DivideError
    @test err_str == "DivideError: integer division error"
    err_str = @except_str Vector{Any}(undef, 1)[1] UndefRefError
    @test err_str == "UndefRefError: access to undefined reference"
    err_str = @except_str undefvar UndefVarError
    @test err_str == "UndefVarError: `undefvar` not defined in local scope"
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
    @test occursin("MethodError: no method matching Bool()", err_str)
    err_str = @except_str :a() MethodError
    @test occursin("MethodError: objects of type Symbol are not callable", err_str)
    err_str = @except_str EightBitType() MethodError
    @test occursin("MethodError: no method matching $(curmod_prefix)EightBitType()", err_str)
    err_str = @except_str i() MethodError
    @test occursin("MethodError: objects of type $(curmod_prefix)EightBitType are not callable", err_str)
    err_str = @except_str EightBitTypeT() MethodError
    @test occursin("MethodError: no method matching $(curmod_prefix)EightBitTypeT()", err_str)
    err_str = @except_str EightBitTypeT{Int32}() MethodError
    @test occursin("MethodError: no method matching $(curmod_prefix)EightBitTypeT{Int32}()", err_str)
    err_str = @except_str j() MethodError
    @test occursin("MethodError: objects of type $(curmod_prefix)EightBitTypeT{Int32} are not callable", err_str)
    err_str = @except_str FunctionLike()() MethodError
    @test occursin("MethodError: no method matching (::$(curmod_prefix)FunctionLike)()", err_str)
    err_str = @except_str [1,2](1) MethodError
    @test occursin("MethodError: objects of type Vector{$Int} are not callable.\n"*
        "In case you did not try calling it explicitly, check if a Vector{$Int}"*
        " has been passed as an argument to a method that expects a callable instead.\n"*
        "In case you're trying to index into the array, use square brackets [] instead of parentheses ().", err_str)
    # Issue 14940
    err_str = @except_str randn(1)() MethodError
    @test occursin("MethodError: objects of type Vector{Float64} are not callable", err_str)
end
@test repr("text/plain", FunctionLike()) == "(::$(curmod_prefix)FunctionLike) (generic function with 0 methods)"
@test occursin(r"^@doc \(macro with \d+ method[s]?\)$", repr("text/plain", getfield(Base, Symbol("@doc"))))

# Issue 34636
let err_str
    err_str = @except_str 1 + rand(5) MethodError
    @test occursin("MethodError: no method matching +(::$Int, ::Vector{Float64})", err_str)
    @test occursin("For element-wise addition, use broadcasting with dot syntax: scalar .+ array", err_str)
    err_str = @except_str rand(5) - 1//3 MethodError
    @test occursin("MethodError: no method matching -(::Vector{Float64}, ::Rational{$Int})", err_str)
    @test occursin("For element-wise subtraction, use broadcasting with dot syntax: array .- scalar", err_str)
end

import Core: String
method_defs_lineno = @__LINE__() + 1
String() = throw(ErrorException("1"))
(::String)() = throw(ErrorException("2"))
EightBitType() = throw(ErrorException("3"))
(::EightBitType)() = throw(ErrorException("4"))
EightBitTypeT() = throw(ErrorException("5"))
EightBitTypeT{T}() where {T} = throw(ErrorException("6"))
(::EightBitTypeT)() = throw(ErrorException("7"))
(::FunctionLike)() = throw(ErrorException("8"))

struct StructWithUnionAllMethodDefs{T}
end
(::Type{StructWithUnionAllMethodDefs{T} where T<:Integer})(x) = x

let err_str,
    i = reinterpret(EightBitType, 0x54),
    j = reinterpret(EightBitTypeT{Int32}, 0x54),
    sp = Base.source_path()
    sn = basename(sp)
    Base.stacktrace_contract_userdir() && (sp = Base.contractuser(sp))

    @test sprint(show, which(String, Tuple{})) ==
        "String() @ $curmod_str $sp:$(method_defs_lineno + 0)"
    @test sprint(show, which("a", Tuple{})) ==
        "(::String)() @ $curmod_str $sp:$(method_defs_lineno + 1)"
    @test sprint(show, which(EightBitType, Tuple{})) ==
        "$(curmod_prefix)EightBitType() @ $curmod_str $sp:$(method_defs_lineno + 2)"
    @test sprint(show, which(reinterpret(EightBitType, 0x54), Tuple{})) ==
        "(::$(curmod_prefix)EightBitType)() @ $curmod_str $sp:$(method_defs_lineno + 3)"
    @test sprint(show, which(EightBitTypeT, Tuple{})) ==
        "$(curmod_prefix)EightBitTypeT() @ $curmod_str $sp:$(method_defs_lineno + 4)"
    @test sprint(show, which(EightBitTypeT{Int32}, Tuple{})) ==
        "$(curmod_prefix)EightBitTypeT{T}() where T @ $curmod_str $sp:$(method_defs_lineno + 5)"
    @test sprint(show, which(reinterpret(EightBitTypeT{Int32}, 0x54), Tuple{})) ==
        "(::$(curmod_prefix)EightBitTypeT)() @ $curmod_str $sp:$(method_defs_lineno + 6)"
    @test startswith(sprint(show, which(Complex{Int}, Tuple{Int})),
                     "Complex{T}(")
    @test startswith(sprint(show, which(getfield(Base, Symbol("@doc")), Tuple{LineNumberNode, Module, Vararg{Any}})),
                     "var\"@doc\"(__source__::LineNumberNode, __module__::Module, x...) @ Core boot.jl:")
    @test startswith(sprint(show, which(FunctionLike(), Tuple{})),
                     "(::$(curmod_prefix)FunctionLike)() @ $curmod_str $sp:$(method_defs_lineno + 7)")
    @test startswith(sprint(show, which(StructWithUnionAllMethodDefs{<:Integer}, (Any,))),
                     "($(curmod_prefix)StructWithUnionAllMethodDefs{T} where T<:Integer)(x)")
    @test repr("text/plain", FunctionLike()) == "(::$(curmod_prefix)FunctionLike) (generic function with 1 method)"
    @test repr("text/plain", Core.getfield) == "getfield (built-in function)"

    err_str = @except_stackframe String() ErrorException
    @test err_str == "String() at $sn:$(method_defs_lineno + 0)"
    err_str = @except_stackframe "a"() ErrorException
    @test err_str == "(::String)() at $sn:$(method_defs_lineno + 1)"
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

# Issue #20108
let err, buf = IOBuffer()
    try Array() catch err end
    Base.show_method_candidates(buf,err)
    @test isa(err, MethodError)
    @test occursin("Closest candidates are:", String(take!(buf)))
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
        @test err == UndefVarError(Symbol("@x"), @__MODULE__)
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
    @test (@macroexpand1 @nest2b 42) == _macroexpand1(:(@nest2b 42))
end

module TwoargMacroExpand
macro modulecontext(); return __module__; end
end
@test (@__MODULE__) == @macroexpand TwoargMacroExpand.@modulecontext
@test TwoargMacroExpand == @macroexpand TwoargMacroExpand @modulecontext
@test (@__MODULE__) == @macroexpand1 TwoargMacroExpand.@modulecontext
@test TwoargMacroExpand == @macroexpand1 TwoargMacroExpand @modulecontext

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
    Base.show_method_candidates(io, ex, pairs((w = true,)))
    @test occursin("got unsupported keyword argument \"w\"", String(take!(io)))
end

@testset "MethodError with long types (#50803)" begin
    a = view(reinterpret(reshape, UInt8, PermutedDimsArray(rand(5, 7), (2, 1))), 2:3, 2:4, 1:4) # a mildly-complex type
    function f50803 end
    ex50803 = try
        f50803(a, a, a, a, a, a)
    catch e
        e
    end::MethodError
    tlf = Ref(false)
    str = sprint(Base.showerror, ex50803; context=(:displaysize=>(1000, 120), :stacktrace_types_limited=>tlf))
    @test tlf[]
    @test occursin("::SubArray{…}", str)
    tlf[] = false
    str = sprint(Base.showerror, ex50803; context=(:displaysize=>(1000, 10000), :stacktrace_types_limited=>tlf))
    @test !tlf[]
    str = sprint(Base.showerror, ex50803; context=(:displaysize=>(1000, 120)))
    @test !occursin("::SubArray{…}", str)
end

# Issue #20556
import REPL
module EnclosingModule
    abstract type AbstractTypeNoConstructors end
end
let
    method_error = MethodError(EnclosingModule.AbstractTypeNoConstructors, (), Base.get_world_counter())

    # Test that it shows a special message when no constructors have been defined by the user.
    @test startswith(sprint(showerror, method_error),
        """MethodError: no constructors have been defined for $(EnclosingModule.AbstractTypeNoConstructors)
           The type `$(EnclosingModule.AbstractTypeNoConstructors)` exists, but no method is defined for this combination of argument types when trying to construct it.""")

    # Does it go back to previous behaviour when there *is* at least
    # one constructor defined?
    EnclosingModule.AbstractTypeNoConstructors(x, y) = x + y
    @test startswith(sprint(showerror, method_error),
        "MethodError: no method matching $(EnclosingModule.AbstractTypeNoConstructors)()")

    # Test that the 'default' sysimg.jl method is not displayed.
    @test !occursin("where T at sysimg.jl", sprint(showerror, method_error))

    # Test that tab-completion will not show the 'default' sysimg.jl method.
    completions = REPL.REPLCompletions.complete_methods(:(EnclosingModule.AbstractTypeNoConstructors()), @__MODULE__)
    @test !isempty(completions)
    for method_string in completions
        @test !startswith(REPL.REPLCompletions.completion_text(method_string), "(::Type{T})(arg) where T in Base at sysimg.jl")
    end
end

@testset "show for MethodError with world age issue" begin
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
    @test !occursin("The applicable method may be too new", str)

    # If newer applicable methods are available, world age should be mentioned.
    f21006(x) = x
    @test f21006(()) === ()
    str = sprint(Base.showerror, ex1)
    @test startswith(str, "MethodError: no method matching f21006(::Tuple{})")
    @test occursin("The applicable method may be too new: running in world age $(ex1.world)", str)

    # This method error should be thrown in a world new enough for `f21006(())`.
    # Also makes sure it's printed correctly.
    ex2 = try
        f21006((), ())
    catch e
        e
    end::MethodError
    str = sprint(Base.showerror, ex2)
    @test startswith(str, "MethodError: no method matching f21006(::Tuple{}, ::Tuple{})")
    @test !occursin("The applicable method may be too new", str)

    # If the method is available in the exception world or if the exception world is invalid,
    # don't warn about world age
    for ex3 in (MethodError(ex1.f, ex1.args, ex2.world),
                MethodError(ex1.f, ex1.args, typemax(UInt)))
        str = sprint(Base.showerror, ex3)
        @test startswith(str, "MethodError: no method matching f21006(::Tuple{})")
        @test !occursin("The applicable method may be too new", str)
    end

    str = sprint(Base.showerror, MethodError(+, (1.0, 2.0)))
    @test startswith(str, "MethodError: no method matching +(::Float64, ::Float64)")
    @test occursin("This error has been manually thrown, explicitly", str)

    str = sprint(Base.showerror, MethodError(+, (1.0, 2.0), Base.get_world_counter()))
    @test startswith(str, "MethodError: no method matching +(::Float64, ::Float64)")
    @test occursin("This error has been manually thrown, explicitly", str)

    str = sprint(Base.showerror, MethodError(Core.kwcall, ((; a=3.0), +, 1.0, 2.0)))
    @test startswith(str, "MethodError: no method matching +(::Float64, ::Float64; a::Float64)")
    @test occursin("This error has been manually thrown, explicitly", str)

    str = sprint(Base.showerror, MethodError(Core.kwcall, ((; a=3.0), +, 1.0, 2.0), Base.get_world_counter()))
    @test startswith(str, "MethodError: no method matching +(::Float64, ::Float64; a::Float64)")
    @test occursin("This method does not support all of the given keyword arguments", str)

    @test_throws "MethodError: no method matching kwcall()" Core.kwcall()
end

# Issue #50200
using Base.Experimental: @opaque
@testset "show for MethodError with world age issue (kwarg)" begin
    test_no_error(f) = @test f() === nothing
    function test_worldage_error(f)
        ex = try; f(); error("Should not have been reached") catch ex; ex; end
        strex = sprint(Base.showerror, ex)
        @test occursin("The applicable method may be too new", strex)
        @test !occursin("!Matched::", sprint(Base.showerror, strex))
    end

    global callback50200

    # First the no-kwargs version
    callback50200 = (args...)->nothing
    f = @opaque ()->callback50200()
    test_no_error(f)
    callback50200 = (args...)->nothing
    test_worldage_error(f)

    callback50200 = (args...; kwargs...)->nothing
    f = @opaque ()->callback50200(;a=1)
    test_no_error(f)
    callback50200 = (args...; kwargs...)->nothing
    test_worldage_error(f)
end

# Custom hints
struct HasNoOne end
function recommend_oneunit(io, ex, arg_types, kwargs)
    if ex.f === Base.one && length(arg_types) == 1 && arg_types[1] === HasNoOne
        if isempty(kwargs)
            print(io, "\nHasNoOne does not support `one`; did you mean `oneunit`?")
        else
            print(io, "\n`one` doesn't take keyword arguments, that would be silly")
        end
    end
end
@test Base.Experimental.register_error_hint(recommend_oneunit, MethodError) === nothing
let err_str
    err_str = @except_str one(HasNoOne()) MethodError
    @test occursin(r"MethodError: no method matching one\(::.*HasNoOne\)", err_str)
    @test occursin("HasNoOne does not support `one`; did you mean `oneunit`?", err_str)
    err_str = @except_str one(HasNoOne(); value=2) MethodError
    @test occursin(Regex("MethodError: no method matching one\\(::.*HasNoOne; value::$(Int)\\)"), err_str)
    @test occursin("`one` doesn't take keyword arguments, that would be silly", err_str)
end
pop!(Base.Experimental._hint_handlers[MethodError])  # order is undefined, don't copy this

function busted_hint(io, exc, notarg)  # wrong number of args
    print(io, "\nI don't have a hint for you, sorry")
end
@test Base.Experimental.register_error_hint(busted_hint, DomainError) === nothing
try
    sqrt(-2)
catch ex
    io = IOBuffer()
    @test_logs (:error, "Hint-handler busted_hint for DomainError in $(@__MODULE__) caused an error") showerror(io, ex)
end
pop!(Base.Experimental._hint_handlers[DomainError])  # order is undefined, don't copy this

struct ANumber <: Number end
let err_str = @except_str ANumber()(3 + 4) MethodError
    @test occursin("objects of type $(curmod_prefix)ANumber are not callable", err_str)
    @test count(==("Maybe you forgot to use an operator such as *, ^, %, / etc. ?"), split(err_str, '\n')) == 1
    # issue 40478
    err_str = @except_str ANumber()(3 + 4) MethodError
    @test count(==("Maybe you forgot to use an operator such as *, ^, %, / etc. ?"), split(err_str, '\n')) == 1
end

let a = [1 2; 3 4];
    err_str = @except_str (a[1][2] = 5) MethodError
    @test occursin("\nAre you trying to index into an array? For multi-dimensional arrays, separate the indices with commas: ", err_str)
    @test occursin("a[1, 2]", err_str)
    @test occursin("rather than a[1][2]", err_str)
end

let d = Dict
    err_str = @except_str (d[1] = 5) MethodError
    @test occursin("\nYou attempted to index the type Dict, rather than an instance of the type. Make sure you create the type using its constructor: ", err_str)
    @test occursin("d = Dict([...])", err_str)
    @test occursin(" rather than d = Dict", err_str)
end

let s = Some("foo")
    err_str = @except_str (s[] = "bar") MethodError
    @test !occursin("You attempted to index the type String", err_str)
end

# Execute backtrace once before checking formatting, see #38858
backtrace()

# issue #28442
@testset "Long stacktrace printing" begin
    f28442(c) = g28442(c + 1)
    g28442(c) = c > 10000 ? (return backtrace()) : f28442(c+1)
    bt = f28442(1)
    io = IOBuffer()
    Base.show_backtrace(io, bt)
    output = split(String(take!(io)), '\n')
    length(output) >= 8 || println(output) # for better errors when this fails
    @test lstrip(output[3])[1:3] == "[1]"
    @test occursin("g28442", output[3])
    @test lstrip(output[5])[1:3] == "[2]"
    @test occursin("f28442", output[5])
    is_windows_32_bit = Sys.iswindows() && (Sys.WORD_SIZE == 32)
    if is_windows_32_bit
        # These tests are currently broken (intermittently/non-determistically) on 32-bit Windows.
        # https://github.com/JuliaLang/julia/issues/55900
        # Instead of skipping them entirely, we skip one, and we loosen the other.

        # Broken test: @test occursin("the above 2 lines are repeated 5000 more times", output[7])
        @test occursin("the above 2 lines are repeated ", output[7])
        @test occursin(" more times", output[7])

        # Broken test: @test lstrip(output[8])[1:7] == "[10003]"
        @test_broken false
    else
        @test occursin("the above 2 lines are repeated 5000 more times", output[7])
        @test lstrip(output[8])[1:7] == "[10003]"
    end
end

@testset "Line number correction" begin
    getbt() = backtrace()
    bt = getbt()
    Base.update_stackframes_callback[] = function(list)
        modify((sf, n)) = sf.func === :getbt ? (StackTraces.StackFrame(sf.func, sf.file, sf.line+2, sf.linfo, sf.from_c, sf.inlined, sf.pointer), n) : (sf, n)
        map!(modify, list, list)
    end
    io = IOBuffer()
    Base.show_backtrace(io, bt)
    outputc = split(String(take!(io)), '\n')
    Base.update_stackframes_callback[] = identity
    Base.show_backtrace(io, bt)
    output0 = split(String(take!(io)), '\n')
    function getline(output)
        idx = findfirst(str->occursin("getbt", str), output)
        return parse(Int, match(r":(\d*)$", output[idx+1]).captures[1])
    end
    @test getline(outputc) == getline(output0) + 2
end


# issue #30633
@test_throws ArgumentError("invalid index: \"foo\" of type String") [1]["foo"]
@test_throws ArgumentError("invalid index: nothing of type Nothing") [1][nothing]

# issue #53618, pr #55165
@testset "FieldErrorHints" begin
    struct FieldFoo
        a::Float32
        b::Int
    end
    Base.propertynames(foo::FieldFoo) = (:a, :x, :y)

    s = FieldFoo(1, 2)

    test = @test_throws FieldError s.c

    ex = test.value::FieldError

    # Check error message first
    errorMsg = sprint(Base.showerror, ex)
    @test occursin("FieldError: type FieldFoo has no field `c`", errorMsg)
    @test occursin("available fields: `a`, `b`", errorMsg)
    @test occursin("Available properties: `x`, `y`", errorMsg)

    d = Dict(s => 1)

    for fld in fieldnames(Dict)
        ex = try
            getfield(d, fld)
        catch e
            print(e)
        end
        @test !(ex isa Type) || ex <: FieldError
    end
    test = @test_throws FieldError d.c

    ex = test.value::FieldError

    errorMsg = sprint(Base.showerror, ex)
    @test occursin("FieldError: type Dict has no field `c`", errorMsg)
    # Check hint message
    hintExpected = "Did you mean to access dict values using key: `:c` ? Consider using indexing syntax dict[:c]\n"
    @test occursin(hintExpected, errorMsg)
end

# UndefVar error hints
module A53000
    export f
    f() = 0.0
end

module C_outer_53000
    import ..A53000: f
    public f

    module C_inner_53000
    import ..C_outer_53000: f
    export f
    end
end

module D_53000
    public f
    f() = 1.0
end

C_inner_53000 = "I'm a decoy with the same name as C_inner_53000!"

Base.Experimental.register_error_hint(Base.UndefVarError_hint, UndefVarError)

@testset "undefvar error hints" begin
    old_modules_order = Base.loaded_modules_order
    append!(Base.loaded_modules_order, [A53000, C_outer_53000, C_outer_53000.C_inner_53000, D_53000])
    test = @test_throws UndefVarError f
    ex = test.value::UndefVarError
    errormsg = sprint(Base.showerror, ex)
    mod = @__MODULE__
    @test occursin("Hint: a global variable of this name also exists in $mod.A53000.", errormsg)
    @test occursin("Hint: a global variable of this name also exists in $mod.D_53000.", errormsg)
    @test occursin("- Also declared public in $mod.C_outer_53000", errormsg)
    @test occursin("- Also exported by $mod.C_outer_53000.C_inner_53000 (loaded but not imported in Main).", errormsg)
    copy!(Base.loaded_modules_order, old_modules_order)
end
@testset " test the functionality of `UndefVarError_hint` against import clashes" begin
    @eval module X
        module A
        export x
        x = 1
        end # A

        module B
        export x
        x = 2
        end # B

        using .A, .B

    end # X

    expected_message = string("\nHint: It looks like two or more modules export different ",
                              "bindings with this name, resulting in ambiguity. Try explicitly ",
                              "importing it from a particular module, or qualifying the name ",
                              "with the module it should come from.")
    @test_throws expected_message X.x
end

# test showing MethodError with type argument
struct NoMethodsDefinedHere; end
let buf = IOBuffer()
    Base.show_method_candidates(buf, Base.MethodError(sin, Tuple{NoMethodsDefinedHere}))
    @test length(take!(buf)) !== 0
end

# pr #32814
let t1 = @async(error(1)),
    t2 = @async(wait(t1))
    local e
    try
        wait(t2)
    catch e_
        e = e_
    end
    buf = IOBuffer()
    showerror(buf, e)
    s = String(take!(buf))
    @test length(findall("Stacktrace:", s)) == 2
    @test occursin("[1] error(s::Int", s)
end

module TestMethodShadow
    struct Foo; x; end
    +(a::Foo, b::Foo) = Foo(a.x + b.x)
    ==(a::Foo, b::Foo) = Foo(a.x == b.x)
    div(a::Foo, b::Foo) = Foo(div(a.x, b.x))
end
for (func,str) in ((TestMethodShadow.:+,":+"), (TestMethodShadow.:(==),":(==)"), (TestMethodShadow.:div,"div"))
    ex = try
        foo = TestMethodShadow.Foo(3)
        func(foo,foo)
    catch e
       e
    end::MethodError
    @test occursin("You may have intended to import Base.$str", sprint(Base.showerror, ex))
end

# Test that implementation detail of include() is hidden from the user by default
let bt = try
        @noinline include("testhelpers/include_error.jl")
    catch
        catch_backtrace()
    end
    bt_str = sprint(Base.show_backtrace, bt)
    @test occursin(" include(", bt_str)
    @test !occursin(" _include(", bt_str)
end

# Test backtrace printing
module B
    module C
        @noinline f(x; y=2.0) = error()
    end
    module D
        import ..C: f
        g() = f(2.0; y=3.0)
    end
end

@testset "backtrace" begin
    bt = try
        B.D.g()
    catch
        catch_backtrace()
    end
    bt_str = sprint(Base.show_backtrace, bt)
    m = @__MODULE__
    @test contains(bt_str, "f(x::Float64; y::Float64)")
    @test contains(bt_str, "@ $m.B.C")
    @test contains(bt_str, "@ $m.B.D")
end
# 1d/2d error shouldn't appear in unsupported keywords arg #36325
let err = nothing
    try
        identity([1 1]; bad_kwards = :julia)
    catch err
        err_str = sprint(showerror, err)
        @test !occursin("2d", err_str)
    end
end

# issue #37587
# TODO: enable on more platforms
if (Sys.isapple() || Sys.islinux()) && Sys.ARCH === :x86_64
    single_repeater() = single_repeater()
    pair_repeater_a() = pair_repeater_b()
    pair_repeater_b() = pair_repeater_a()

    @testset "repeated stack frames" begin
        let bt = try
                single_repeater()
            catch
                catch_backtrace()
            end
            bt_str = sprint(Base.show_backtrace, bt)
            @test occursin(r"repeats \d+ times", bt_str)
        end

        let bt = try
                pair_repeater_a()
            catch
                catch_backtrace()
            end
            bt_str = sprint(Base.show_backtrace, bt)
            @test occursin(r"the above 2 lines are repeated \d+ more times", bt_str)
        end
    end
end

@testset "ScheduledAfterSyncException" begin
    t = :DummyTask
    msg = sprint(showerror, Base.ScheduledAfterSyncException(Any[t]))
    @test occursin(":DummyTask is registered after the end of a `@sync` block", msg)
    msg = sprint(showerror, Base.ScheduledAfterSyncException(Any[t, t]))
    @test occursin(
        ":DummyTask and one more Symbol are registered after the end of a `@sync` block",
        msg,
    )
    msg = sprint(showerror, Base.ScheduledAfterSyncException(Any[t, t, t]))
    @test occursin(
        ":DummyTask and 2 more objects are registered after the end of a `@sync` block",
        msg,
    )
end

@testset "error message hints relative modules #40959" begin
    m = Module()
    expr = :(module Foo
        module Bar
        end

        using Bar
    end)
    try
        Base.eval(m, expr)
    catch err
        err_str = sprint(showerror, err)
        @test contains(err_str, "maybe you meant `import/using .Bar`")
    end

    m = Module()
    expr = :(module Foo
        Bar = 3

        using Bar
    end)
    try
        Base.eval(m, expr)
    catch err
        err_str = sprint(showerror, err)
        @test !contains(err_str, "maybe you meant `import/using .Bar`")
    end

    m = Module()
    expr = :(module Foo
        using Bar
    end)
    try
        Base.eval(m, expr)
    catch err
        err_str = sprint(showerror, err)
        @test !contains(err_str, "maybe you meant `import/using .Bar`")
    end

    m = Module()
    expr = :(module Foo
        module Bar end
        module Buzz
            using Bar
        end
    end)
    try
        Base.eval(m, expr)
    catch err
        err_str = sprint(showerror, err)
        @test contains(err_str, "maybe you meant `import/using ..Bar`")
    end

    m = Module()
    expr = :(module Foo
        Bar = 3
        module Buzz
            using Bar
        end
    end)
    try
        Base.eval(m, expr)
    catch err
        err_str = sprint(showerror, err)
        @test !contains(err_str, "maybe you meant `import/using ..Bar`")
    end

    m = Module()
    expr = :(module Foo
        module Bar end
        module Buzz
            module Bar end
            using Bar
        end
    end)
    try
        Base.eval(m, expr)
    catch err
        err_str = sprint(showerror, err)
        @test contains(err_str, "maybe you meant `import/using .Bar`")
    end
end

for (expr, errmsg) in
    [
        (:(struct Foo <: 1 end),       "can only subtype data types"),
        (:(struct Foo <: Float64 end), "can only subtype abstract types"),
        (:(struct Foo <: Foo end),     "a type cannot subtype itself"),
        (:(struct Foo <: Tuple{Float64} end), "cannot subtype a tuple type"),
        (:(struct Foo <: NamedTuple{(:a,), Tuple{Int64}} end), "cannot subtype a named tuple type"),
        (:(struct Foo <: Type{Float64} end), "cannot add subtypes to Type"),
        (:(struct Foo <: Type{Float64} end), "cannot add subtypes to Type"),
        (:(struct Foo <: typeof(Core.apply_type) end), "cannot add subtypes to Core.Builtin"),
    ]
    err = try @eval $expr
    catch e
        e
    end
    @test contains(sprint(showerror, err), errmsg)
end

let err_str
    err_str = @except_str "a" + "b" MethodError
    @test occursin("String concatenation is performed with *", err_str)
end

# https://github.com/JuliaLang/julia/issues/55745
let err_str
    err_str = @except_str +() MethodError
    @test !occursin("String concatenation is performed with *", err_str)
end

struct MissingLength; end
struct MissingSize; end
Base.IteratorSize(::Type{MissingSize}) = Base.HasShape{2}()
Base.iterate(::MissingLength) = nothing
Base.iterate(::MissingSize) = nothing

let err_str
    expected = "Finding the minimum of an iterable is performed with `minimum`."
    err_str = @except_str min([1,2,3]) MethodError
    @test occursin(expected, err_str)
    err_str = @except_str min((i for i in 1:3)) MethodError
    @test occursin(expected, err_str)
    expected = "Finding the maximum of an iterable is performed with `maximum`."
    err_str = @except_str max([1,2,3]) MethodError
    @test occursin(expected, err_str)

    expected = "You may need to implement the `length` method or define `IteratorSize` for this type to be `SizeUnknown`."
    err_str = @except_str length(MissingLength()) MethodError
    @test occursin(expected, err_str)
    err_str = @except_str collect(MissingLength()) MethodError
    @test occursin(expected, err_str)
    expected = "You may need to implement the `length` and `size` methods for `IteratorSize` `HasShape`."
    err_str = @except_str size(MissingSize()) MethodError
    @test occursin(expected, err_str)
    err_str = @except_str collect(MissingSize()) MethodError
    @test occursin(expected, err_str)
end

@testset "unused argument names" begin
    g(::Int) = backtrace()
    bt = g(1)
    @test !contains(sprint(Base.show_backtrace, bt), "#unused#")
end

# issue #49002
let buf = IOBuffer()
    Base.show_method_candidates(buf, Base.MethodError(typeof, (17,)), pairs((foo = :bar,)))
    @test isempty(take!(buf))
    Base.show_method_candidates(buf, Base.MethodError(isa, ()), pairs((a = 5,)))
    @test isempty(take!(buf))
end

f_internal_wrap(g, a; kw...) = error();
@inline f_internal_wrap(a; kw...) = f_internal_wrap(identity, a; kw...);
let bt
    @test try
        f_internal_wrap(1)
        false
    catch
        bt = catch_backtrace()
        true
    end
    @test !occursin("#f_internal_wrap#", sprint(Base.show_backtrace, bt))
end

g_collapse_pos(x, y=1.0, z=2.0) = error()
let bt
    @test try
        g_collapse_pos(1.0)
        false
    catch
        bt = catch_backtrace()
        true
    end
    bt_str = sprint(Base.show_backtrace, bt)
    @test occursin("g_collapse_pos(x::Float64, y::Float64, z::Float64)", bt_str)
    @test !occursin("g_collapse_pos(x::Float64)", bt_str)
end

g_collapse_kw(x; y=2.0) = error()
let bt
    @test try
        g_collapse_kw(1.0)
        false
    catch
        bt = catch_backtrace()
        true
    end
    bt_str = sprint(Base.show_backtrace, bt)
    @test occursin("g_collapse_kw(x::Float64; y::Float64)", bt_str)
    @test !occursin("g_collapse_kw(x::Float64)", bt_str)
end

g_collapse_pos_kw(x, y=1.0; z=2.0) = error()
let bt
    @test try
        g_collapse_pos_kw(1.0)
        false
    catch
        bt = catch_backtrace()
        true
    end
    bt_str = sprint(Base.show_backtrace, bt)
    @test occursin("g_collapse_pos_kw(x::Float64, y::Float64; z::Float64)", bt_str)
    @test !occursin("g_collapse_pos_kw(x::Float64, y::Float64)", bt_str)
    @test !occursin("g_collapse_pos_kw(x::Float64)", bt_str)
end

simplify_kwargs_type(pos; kws...) = (pos, sum(kws))
let bt
    res = try
        simplify_kwargs_type(0; kw1=1.0, kw2="2.0")
        false
    catch
        bt = catch_backtrace()
        true
    end
    @test res
    bt_str = sprint(Base.show_backtrace, bt)
    @test occursin("simplify_kwargs_type(pos::$Int; kws::@Kwargs{kw1::Float64, kw2::String})", bt_str)
end

# Test Base.print_with_compare in convert MethodErrors
struct TypeCompareError{A,B} <: Exception end
let e = @test_throws MethodError convert(TypeCompareError{Float64,1}, TypeCompareError{Float64,2}())
    str = sprint(Base.showerror, e.value)
    @test  occursin("TypeCompareError{Float64,2}", str)
    @test  occursin("TypeCompareError{Float64,1}", str)
    @test !occursin("TypeCompareError{Float64{},2}", str) # No {...} for types without params
end

@testset "InexactError for Inf16 should print '16' (#51087)" begin
    @test sprint(showerror, InexactError(:UInt128, UInt128, Inf16)) == "InexactError: UInt128(Inf16)"

    for IntType in [Int8, Int16, Int32, Int64, Int128, UInt8, UInt16, UInt32, UInt64, UInt128]
        IntStr = string(IntType)
        for InfVal in Any[Inf, Inf16, Inf32, Inf64]
            InfStr = repr(InfVal)
            e = @test_throws InexactError IntType(InfVal)
            str = sprint(Base.showerror, e.value)
            @test occursin("InexactError: $IntStr($InfStr)", str)
        end
    end
end

# error message hint from PR #22647
@test_throws "Many shells" cd("~")
@test occursin("Many shells", sprint(showerror, Base.IOError("~", Base.UV_ENOENT)))

# issue #47559"
@test_throws("MethodError: no method matching invoke Returns(::Any, ::Val{N}) where N",
             invoke(Returns, Tuple{Any,Val{N}} where N, 1, Val(1)))

f33793(x::Float32, y::Float32) = 1
@test_throws "\nClosest candidates are:\n  f33793(!Matched::Float32, !Matched::Float32)\n" f33793(Float64(0.0), Float64(0.0))

# https://github.com/JuliaLang/julia/issues/56325
let err_str
    f56325 = x->x+1
    err_str = @except_str f56325(1,2) MethodError
    @test occursin("The anonymous function", err_str)
end
