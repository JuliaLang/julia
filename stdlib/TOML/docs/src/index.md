```@meta
EditURL = "https://github.com/JuliaLang/julia/blob/master/stdlib/TOML/docs/src/index.md"
```

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
Dict{String, Any} with 1 entry:
  "database" => Dict{String, Any}("server"=>"192.168.1.1", "ports"=>[8001, 8001…
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
and [`TOML.tryparsefile`](@ref)) that instead of throwing exceptions on parser error
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

```jldoctest; filter = r"^\s*\S+\s*=.*"m
julia> using TOML

julia> data = Dict(
          "names" => ["Julia", "Julio"],
          "age" => [10, 20],
       );

julia> TOML.print(data)
names = ["Julia", "Julio"]
age = [10, 20]

julia> fname = tempname();

julia> open(fname, "w") do io
           TOML.print(io, data)
       end

julia> TOML.parsefile(fname)
Dict{String, Any} with 2 entries:
  "names" => ["Julia", "Julio"]
  "age"   => [10, 20]
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


## Preserving comments

By default, comments in a TOML document are discarded when parsing, so writing
the data out again loses them. To preserve comments, pass a [`TOML.Comments`](@ref)
object to the parsing functions via the `comments` keyword argument and pass it
back to [`TOML.print`](@ref):

```jldoctest
julia> using TOML

julia> comments = TOML.Comments();

julia> data = TOML.parse("""
       # A comment attached to the entry below it
       name = "MyPkg"
       [compat]
       Dep = "~1.1" # an inline comment
       """; comments);

julia> data["compat"]["OtherDep"] = "2";

julia> TOML.print(data; comments, sorted=true)
# A comment attached to the entry below it
name = "MyPkg"

[compat]
Dep = "~1.1" # an inline comment
OtherDep = "2"
```

Comments are associated with the *items* of the document (`key = value` entries
and `[table]` headers) rather than with positions in the file, so the data can
be freely modified and reformatted (e.g. with `sorted=true`) while the comments
follow the items they belong to. The rules are:

- A block of whole-line comments with no blank line between the block and the
  following item is *attached* to that item, like a docstring, and is printed
  directly above it.
- A comment on the same line as an item is attached to that item and is printed
  on the same line, after the value.
- Any other whole-line comment (i.e. separated from the following item by a
  blank line, or at the end of a table or of the document) is *floating*: it is
  associated with the table it appears in and is printed at the top of that
  table, followed by a blank line.
- A comment attached (or associated) to an item that is deleted from the data
  is not printed; deleting an item deletes its comments.
- Comments inside a value that spans multiple lines (such as a multi-line
  array) are attached to the entry that owns the value and are printed above
  it.
- Comments on or inside the elements of an array of tables (`[[...]]`) are
  *not* preserved, except for a comment block attached to the first `[[...]]`
  header, which is printed above the first element. The key paths used to
  associate comments with items cannot distinguish between the elements of an
  array of tables.

!!! compat "Julia 1.14"
    Comment preservation requires Julia 1.14 or later.

## References
```@docs
TOML.parse
TOML.parsefile
TOML.tryparse
TOML.tryparsefile
TOML.print
TOML.Parser
TOML.ParserError
TOML.Comments
```
