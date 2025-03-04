# This file is a part of Julia. License is MIT: https://julialang.org/license

toml_str(a; kwargs...) = sprint(io -> TOML.print(io, a; kwargs...))
toml_str(f, a; kwargs...) = sprint(io -> TOML.print(f, io, a; kwargs...))

@test toml_str(Dict("b" => 1, "c" => 2, "a" => 3); sorted=true) ==
    """
    a = 3
    b = 1
    c = 2
    """

@test toml_str(Dict("b" => 1, "ac" => 2, "abc" => 3); sorted=true, by=length) ==
    """
    b = 1
    ac = 2
    abc = 3
    """

struct MyStruct
    a::Int
end
@test_throws ErrorException toml_str(Dict("foo" => MyStruct(1)))
# simple value
@test toml_str(Dict("foo" => MyStruct(1))) do x
        x isa MyStruct && return x.a
    end == """
        foo = 1
        """

# tabular values
@test toml_str(Dict("foo" => MyStruct(1)); sorted=true) do x
         x isa MyStruct && return [x.a]
     end == """
         foo = [1]
         """
@test toml_str(Dict("foo" => MyStruct(1)); sorted=true) do x
        x isa MyStruct && return Dict(:bar => x.a)
    end == """
        [foo]
        bar = 1
        """

# validation against the usual case
@test toml_str(Dict("foo" => MyStruct(1)); sorted=true) do x
         x isa MyStruct && return [x.a]
     end == toml_str(Dict("foo" => [1]); sorted=true)
@test toml_str(Dict("foo" => MyStruct(1)); sorted=true) do x
        x isa MyStruct && return Dict(:bar => x.a)
    end == toml_str(Dict("foo" => Dict(:bar => 1)); sorted=true)

@test toml_str(Dict("b" => SubString("foo"))) == "b = \"foo\"\n"

@testset "empty dict print" begin
    s = """
    user = "me"
    [julia]
    [option]
    """
    d = TOML.parse(s)
    @test toml_str(d) == "user = \"me\"\n\n[julia]\n\n[option]\n"
end

@testset "special characters" begin
    s = """
    "\U1f355 \0 \x0 \x1 \t \b" = "\U1f355 \0 \x0 \x1 \t \b"
    "\x7f" = "\x7f"
    """
    @test roundtrip(s)

    d = Dict("str" => string(Char(0xd800)))
    @test_throws ErrorException TOML.print(devnull, d)
end

str = """
[[dataset.loader]]
driver = "nested"
loaders = ["gzip", { driver = "csv", args = {delim = "\t"}}]
"""
@test roundtrip(str)


@testset "vec with dicts and non-dicts" begin
    # https://github.com/JuliaLang/julia/issues/45340
    d =  Dict("b" => Any[111, Dict("a" =>  222, "d" => 333)])
    @test toml_str(d) == "b = [111, {a = 222, d = 333}]\n"

    d =  Dict("b" => Any[Dict("a" =>  222, "d" => 333), 111])
    @test toml_str(d) == "b = [{a = 222, d = 333}, 111]\n"

    d =  Dict("b" => Any[Dict("a" =>  222, "d" => 333)])
    @test toml_str(d) == """
    [[b]]
    a = 222
    d = 333
    """

    # https://github.com/JuliaLang/julia/pull/57584
    d = Dict("b" => [MyStruct(1), MyStruct(2)])
    @test toml_str(d) do x
        x isa MyStruct && return Dict("a" => x.a)
    end == """
    b = [{a = 1}, {a = 2}]
    """
end

@testset "unsigned integers" for (x, s) in [
            0x1a0 => "0x01a0",
            0x1aea8 => "0x01aea8",
            0x1aeee8 => "0x1aeee8",
            0x1aea01231 => "0x01aea01231",
            0x1aea01231213ae13125 => "0x01aea01231213ae13125",
        ]
    d = Dict("x" => x)
    @test toml_str(d) == """
    x = $s
    """
end

struct Foo
    a::Int64
    b::Float64
end

struct Bar
    c::Float64
    d::String
end


f = Foo(2,9.9)
b = Bar(1.345, "hello")

dd = Dict("hello"=>"world", "f"=>f,  "b"=>b)

to_dict(foo::Foo) = Dict("a"=>foo.a, "b"=>foo.b)
to_dict(bar::Bar) = Dict("c"=>bar.c, "d"=>bar.d)

@test toml_str(to_dict, dd; sorted=true) ==
"""
hello = "world"

[b]
c = 1.345
d = "hello"

[f]
a = 2
b = 9.9
"""


inline_dict = Dict("a" => [1,2], "b" => Dict("a" => "b"), "c" => "foo")
d = Dict(
    "x" => "y",
    "y" => inline_dict,
    "z" => [1,2,3],
)
inline_tables = IdSet{Dict}()
push!(inline_tables, inline_dict)
@test toml_str(d; sorted=true, inline_tables) ==
"""
x = "y"
y = {a = [1, 2], b = {a = "b"}, c = "foo"}
z = [1, 2, 3]
"""


d = Dict("deps" => Dict(
        "LocalPkg" => "fcf55292-0d03-4e8a-9e0b-701580031fc3",
        "Example" => "7876af07-990d-54b4-ab0e-23690620f79a"),
   "sources" => Dict(
        "LocalPkg" => Dict("path" => "LocalPkg"),
        "Example" => Dict("url" => "https://github.com/JuliaLang/Example.jl")))

inline_tables = IdSet{Dict}()
push!(inline_tables, d["sources"]["LocalPkg"])
push!(inline_tables, d["sources"]["Example"])

@test toml_str(d; sorted=true, inline_tables) ==
"""
[deps]
Example = "7876af07-990d-54b4-ab0e-23690620f79a"
LocalPkg = "fcf55292-0d03-4e8a-9e0b-701580031fc3"

[sources]
Example = {url = "https://github.com/JuliaLang/Example.jl"}
LocalPkg = {path = "LocalPkg"}
"""

inline_tables = IdSet{Dict}()
push!(inline_tables, d["sources"]["LocalPkg"])
s = """
[deps]
Example = "7876af07-990d-54b4-ab0e-23690620f79a"
LocalPkg = "fcf55292-0d03-4e8a-9e0b-701580031fc3"

[sources]
LocalPkg = {path = "LocalPkg"}

    [sources.Example]
    url = "https://github.com/JuliaLang/Example.jl"
"""
@test toml_str(d; sorted=true, inline_tables) == s
@test roundtrip(s)


# https://github.com/JuliaLang/julia/pull/57584
d = Dict("a" => 1, "b" => 2)
inline_tables = IdSet{Dict}([d])
s = "{a = 1, b = 2}"
@test toml_str(d; sorted=true, inline_tables) == s


# multiline strings (#55083)
s = """
a = \"\"\"lorem ipsum



alpha\"\"\"
"""
@test roundtrip(s)
