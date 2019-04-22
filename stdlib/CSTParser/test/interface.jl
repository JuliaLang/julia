
@testset "function defs" begin
    @test CSTParser.defines_function(CSTParser.parse("function f end"))
    @test CSTParser.defines_function(CSTParser.parse("function f() end"))
    @test CSTParser.defines_function(CSTParser.parse("function f()::T end"))
    @test CSTParser.defines_function(CSTParser.parse("function f(x::T) where T end"))
    @test CSTParser.defines_function(CSTParser.parse("function f{T}() end"))
    @test CSTParser.defines_function(CSTParser.parse("f(x) = x"))
    @test CSTParser.defines_function(CSTParser.parse("f(x)::T = x"))
    @test CSTParser.defines_function(CSTParser.parse("f{T}(x)::T = x"))
    @test CSTParser.defines_function(CSTParser.parse("f{T}(x)::T = x"))
    @test CSTParser.defines_function(CSTParser.parse("*(x,y) = x"))
    @test CSTParser.defines_function(CSTParser.parse("*(x,y)::T = x"))
    @test CSTParser.defines_function(CSTParser.parse("!(x::T)::T = x"))
end

@testset "datatype defs" begin
    @test CSTParser.defines_struct(CSTParser.parse("struct T end"))
    @test CSTParser.defines_struct(CSTParser.parse("mutable struct T end"))
    @test CSTParser.defines_mutable(CSTParser.parse("mutable struct T end"))
    @test CSTParser.defines_abstract(CSTParser.parse("abstract type T end"))
    @test CSTParser.defines_abstract(CSTParser.parse("abstract T"))
    @test CSTParser.defines_primitive(CSTParser.parse("primitive type a b end"))
end

@testset "get_name" begin
    @test CSTParser.get_name(CSTParser.parse("struct T end")).val == "T"
    @test CSTParser.get_name(CSTParser.parse("struct T{T} end")).val == "T"
    @test CSTParser.get_name(CSTParser.parse("struct T <: T end")).val == "T"
    @test CSTParser.get_name(CSTParser.parse("struct T{T} <: T end")).val == "T"

    @test CSTParser.get_name(CSTParser.parse("mutable struct T end")).val == "T"
    @test CSTParser.get_name(CSTParser.parse("mutable struct T{T} end")).val == "T"
    @test CSTParser.get_name(CSTParser.parse("mutable struct T <: T end")).val == "T"
    @test CSTParser.get_name(CSTParser.parse("mutable struct T{T} <: T end")).val == "T"

    @test CSTParser.get_name(CSTParser.parse("abstract type T end")).val == "T"
    @test CSTParser.get_name(CSTParser.parse("abstract type T{T} end")).val == "T"
    @test CSTParser.get_name(CSTParser.parse("abstract type T <: T end")).val == "T"
    @test CSTParser.get_name(CSTParser.parse("abstract type T{T} <: T end")).val == "T"
    # NEEDS FIX: v0.6 dep
    @test CSTParser.get_name(CSTParser.parse("abstract T")).val == "T"
    @test CSTParser.get_name(CSTParser.parse("abstract T{T}")).val == "T"
    @test CSTParser.get_name(CSTParser.parse("abstract T <: T")).val == "T"
    @test CSTParser.get_name(CSTParser.parse("abstract T{T} <: T")).val == "T"

    @test CSTParser.get_name(CSTParser.parse("function f end")).val == "f"
    @test CSTParser.get_name(CSTParser.parse("function f() end")).val == "f"
    @test CSTParser.get_name(CSTParser.parse("function f()::T end")).val == "f"
    @test CSTParser.get_name(CSTParser.parse("function f(x::T) where T end")).val == "f"
    @test CSTParser.get_name(CSTParser.parse("function f{T}() end")).val == "f"

    # Operators
    @test CSTParser.str_value(CSTParser.get_name(CSTParser.parse("function +() end"))) == "+"
    @test CSTParser.str_value(CSTParser.get_name(CSTParser.parse("function (+)() end"))) == "+"
    @test CSTParser.str_value(CSTParser.get_name(CSTParser.parse("+(x,y) = x"))) == "+"
    @test CSTParser.str_value(CSTParser.get_name(CSTParser.parse("+(x,y)::T = x"))) == "+"
    @test CSTParser.str_value(CSTParser.get_name(CSTParser.parse("!(x)::T = x"))) == "!"
    @test CSTParser.str_value(CSTParser.get_name(CSTParser.parse("!(x) = x"))) == "!"
end

@testset "get_sig_params" begin
    @test CSTParser.get_where_params(CSTParser.parse("a where T")) == ["T"]
    @test CSTParser.get_where_params(CSTParser.parse("a where {T}")) == ["T"]
    @test CSTParser.get_where_params(CSTParser.parse("a where {T,S}")) == ["T", "S"]
    @test CSTParser.get_where_params(CSTParser.parse("a where T <: S")) == ["T"]
    @test CSTParser.get_where_params(CSTParser.parse("a where T{S}")) == ["T"]
    @test CSTParser.get_where_params(CSTParser.parse("a where T{S} <: R")) == ["T"]
    @test CSTParser.get_sig_params(CSTParser.parse("f() where T")) == ["T"]
    @test CSTParser.get_sig_params(CSTParser.parse("f{T}()")) == ["T"]
    @test CSTParser.get_sig_params(CSTParser.parse("f{T}() where S")) == ["S", "T"]
    @test CSTParser.get_sig_params(CSTParser.parse("f{T}() where S{R} where J")) == ["J", "S", "T"]
end

@testset "get_sig_params" begin
    f = x -> CSTParser.str_value.(CSTParser.get_args(CSTParser.parse(x)))
    @test f("function f(a) end") == ["a"]
    @test f("function f(a::T) end") == ["a"]
    @test f("function f(a,b) end") == ["a", "b"]
    @test f("function f(a::T,b::T) end") == ["a", "b"]
    @test f("function f(a::T,b::T) where T end") == ["a", "b"]
    @test f("function f{T}(a::T,b::T) where T end") == ["a", "b"]
    @test f("function f{T}(a::T,b::T;c = 1) where T end") == ["a", "b", "c"]

    @test f("a -> a") == ["a"]
    @test f("a::T -> a") == ["a"]
    @test f("(a::T) -> a") == ["a"]
    @test f("(a,b) -> a") == ["a", "b"]

    @test f("map(1:10) do a 
        a
    end") == ["a"]
    @test f("map(1:10) do a,b
        a
    end") == ["a", "b"]
end

