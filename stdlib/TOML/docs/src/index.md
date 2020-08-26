# TOML

TOML.jl is a Julia standard library for parsing and writing [TOML
v1.0](https://toml.io/en/) files.

## Parsing TOML data

```jldoctest
julia> using TOML

julia> data = """
           [database]
           server = "192.168.1.1"
           ports = [ 8001, 8001, 8002 ]
       """;

julia> TOML.parse(data)
Dict{String,Any} with 1 entry:
  "database" => Dict{String,Any}("server"=>"192.168.1.1","ports"=>[8001, 8001, …
```

To parse a file, use [`TOML.parsefile`](@ref). If the file has a syntax error,
an exception is thrown:

```jldoctest
julia> using TOML

julia> TOML.parse("""
           value = 0.0.0
       """)
ERROR: TOML Parser error:
none:1:16 error: failed to parse value
      value = 0.0.0
                 ^
[...]
```

There are other versions of the parse functions ([`TOML.tryparse`](@ref)
and [`TOML.tryparsefile`]) that instead of throwing exceptions on parser error
returns a [`TOML.ParserError`](@ref) with information:

```jldoctest
julia> using TOML

julia> err = TOML.tryparse("""
           value = 0.0.0
       """);

julia> err.type
ErrGenericValueError::ErrorType = 14

julia> err.line
1

julia> err.column
16
```


## Exporting data to TOML file

The [`TOML.print`](@ref) function is used to print (or serialize) data into TOML
format.

```jldoctest
julia> using TOML

julia> fname = tempname();

julia> data = Dict(
          "names" => ["Julia", "Julio"],
          "age" => [10, 20],
       );

julia> TOML.print(data)
names = ["Julia", "Julio"]
age = [10, 20]
```

Keys can be sorted according to some value

```jldoctest
julia> using TOML

julia> TOML.print(Dict(
       "abc"  => 1,
       "ab"   => 2,
       "abcd" => 3,
       ); sorted=true, by=length)
ab = 2
abc = 1
abcd = 3
```

For custom structs, pass a function that converts the struct to a supported
type

```jldoctest
julia> using TOML

julia> struct MyStruct
           a::Int
           b::String
       end

julia> TOML.print(Dict("foo" => MyStruct(5, "bar"))) do x
           x isa MyStruct && return [x.a, x.b]
           error("unhandled type $(typeof(x))")
       end
foo = [5, "bar"]
```


## References
```@docs
TOML.parse
TOML.parsefile
TOML.tryparse
TOML.tryparsefile
TOML.print
TOML.Parser
TOML.ParserError
```
