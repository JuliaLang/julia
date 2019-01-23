module TestSerializer

using JSON
using Test

# to define a new serialization behaviour, import these first
import JSON.Serializations: CommonSerialization, StandardSerialization
import JSON: StructuralContext

# those names are long so we can define some type aliases
const CS = CommonSerialization
const SC = StructuralContext

# for test harness purposes
function sprint_kwarg(f, args...; kwargs...)
    b = IOBuffer()
    f(b, args...; kwargs...)
    String(take!(b))
end

# issue #168: Print NaN and Inf as Julia would
struct NaNSerialization <: CS end
JSON.show_json(io::SC, ::NaNSerialization, f::AbstractFloat) = Base.print(io, f)

@test sprint(JSON.show_json, NaNSerialization(), [NaN, Inf, -Inf, 0.0]) ==
    "[NaN,Inf,-Inf,0.0]"

@test sprint_kwarg(
    JSON.show_json,
    NaNSerialization(),
    [NaN, Inf, -Inf, 0.0];
    indent=4
) == """
[
    NaN,
    Inf,
    -Inf,
    0.0
]
"""

# issue #170: Print JavaScript functions directly
struct JSSerialization <: CS end
struct JSFunction
    data::String
end

function JSON.show_json(io::SC, ::JSSerialization, f::JSFunction)
    first = true
    for line in split(f.data, '\n')
        if !first
            JSON.indent(io)
        end
        first = false
        Base.print(io, line)
    end
end

@test sprint_kwarg(JSON.show_json, JSSerialization(), Any[
    1,
    2,
    JSFunction("function test() {\n  return 1;\n}")
]; indent=2) == """
[
  1,
  2,
  function test() {
    return 1;
  }
]
"""

# test serializing a type without any fields
struct SingletonType end
@test_throws ErrorException json(SingletonType())

# test printing to stdout
let filename = tempname()
    open(filename, "w") do f
        redirect_stdout(f) do
            JSON.print(Any[1, 2, 3.0])
        end
    end
    @test read(filename, String) == "[1,2,3.0]"
    rm(filename)
end

# issue #184: serializing a 0-dimensional array
@test sprint(JSON.show_json, JSON.StandardSerialization(), view([184], 1)) == "184"

# test serializing with a JSONText object
@test json([JSONText("{\"bar\":  [3,4,5]}"),314159]) == "[{\"bar\":  [3,4,5]},314159]"
@test json([JSONText("{\"bar\":  [3,4,5]}"),314159], 1) == "[\n {\n  \"bar\": [\n   3,\n   4,\n   5\n  ]\n },\n 314159\n]\n"

end
