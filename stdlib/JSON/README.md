# JSON.jl
### Parsing and printing JSON in pure Julia.

[![Build Status](https://travis-ci.org/JuliaIO/JSON.jl.svg)](https://travis-ci.org/JuliaIO/JSON.jl)
[![Build status](https://ci.appveyor.com/api/projects/status/2sfomjwl29k6y6oy)](https://ci.appveyor.com/project/staticfloat/json-jl)
[![codecov.io](http://codecov.io/github/JuliaIO/JSON.jl/coverage.svg?branch=master)](http://codecov.io/github/JuliaIO/JSON.jl?branch=master)

[![JSON](http://pkg.julialang.org/badges/JSON_0.3.svg)](http://pkg.julialang.org/?pkg=JSON&ver=0.3)
[![JSON](http://pkg.julialang.org/badges/JSON_0.4.svg)](http://pkg.julialang.org/?pkg=JSON&ver=0.4)
[![JSON](http://pkg.julialang.org/badges/JSON_0.5.svg)](http://pkg.julialang.org/?pkg=JSON&ver=0.5)
[![JSON](http://pkg.julialang.org/badges/JSON_0.6.svg)](http://pkg.julialang.org/?pkg=JSON&ver=0.6)

**Installation**: `julia> Pkg.add("JSON")`


## Basic Usage


```julia
import JSON

# JSON.parse - string or stream to Julia data structures
s = "{\"a_number\" : 5.0, \"an_array\" : [\"string\", 9]}"
j = JSON.parse(s)
#  Dict{AbstractString,Any} with 2 entries:
#    "an_array" => {"string",9}
#    "a_number" => 5.0

# JSON.json - Julia data structures to a string
JSON.json([2,3])
#  "[2,3]"
JSON.json(j)
#  "{\"an_array\":[\"string\",9],\"a_number\":5.0}"
```

## Documentation


```julia
JSON.print(io::IO, s::AbstractString)
JSON.print(io::IO, s::Union{Integer, AbstractFloat})
JSON.print(io::IO, n::Nothing)
JSON.print(io::IO, b::Bool)
JSON.print(io::IO, a::AbstractDict)
JSON.print(io::IO, v::AbstractVector)
JSON.print{T, N}(io::IO, v::Array{T, N})
```

Writes a compact (no extra whitespace or indentation) JSON representation
to the supplied IO.

```julia
JSON.print(a::AbstractDict, indent)
JSON.print(io::IO, a::AbstractDict, indent)
```

Writes a JSON representation with newlines, and indentation if specified. Non-zero `indent` will be applied recursively to nested elements.


```julia
json(a::Any)
```

Returns a compact JSON representation as an `AbstractString`.

```julia
JSON.parse(s::AbstractString; dicttype=Dict, inttype=Int64)
JSON.parse(io::IO; dicttype=Dict, inttype=Int64)
JSON.parsefile(filename::AbstractString; dicttype=Dict, inttype=Int64, use_mmap=true)
```

Parses a JSON `AbstractString` or IO stream into a nested `Array` or `Dict`.

The `dicttype` indicates the dictionary type (`<: Associative`), or a function that
returns an instance of a dictionary type,
that JSON objects are parsed to.  It defaults to `Dict` (the built-in Julia
dictionary), but a different type can be passed for additional functionality.
For example, if you `import DataStructures`
(assuming the [DataStructures
package](https://github.com/JuliaLang/DataStructures.jl) is
installed)

 - you can pass `dicttype=DataStructures.OrderedDict` to maintain the insertion order
   of the items in the object;
 - or you can pass `()->DefaultDict{String,Any}(Missing)` to having any non-found keys
   return `missing` when you index the result.


The `inttype` argument controls how integers are parsed.  If a number in a JSON
file is recognized to be an integer, it is parsed as one; otherwise it is parsed
as a `Float64`.  The `inttype` defaults to `Int64`, but, for example, if you know
that your integer numbers are all small and want to save space, you can pass
`inttype=Int32`.  Alternatively, if your JSON input has integers which are too large
for Int64, you can pass `inttype=Int128` or `inttype=BigInt`.  `inttype` can be any
subtype of `Real`.

```julia
JSONText(s::AbstractString)
```
A wrapper around a Julia string representing JSON-formatted text,
which is inserted *as-is* in the JSON output of `JSON.print` and `JSON.json`.

```julia
JSON.lower(p::Point2D) = [p.x, p.y]
```

Define a custom serialization rule for a particular data type. Must return a
value that can be directly serialized; see help for more details.
