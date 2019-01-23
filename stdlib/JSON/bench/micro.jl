# JSON Microbenchmarks
# 0.6 required for running benchmarks

using JSON
using BenchmarkTools
using Dates

const suite = BenchmarkGroup()

suite["print"] = BenchmarkGroup(["serialize"])
suite["pretty-print"] = BenchmarkGroup(["serialize"])

struct CustomListType
    x::Int
    y::Float64
    z::Union{CustomListType, Nothing}
end

struct CustomTreeType
    x::String
    y::Union{CustomTreeType, Nothing}
    z::Union{CustomTreeType, Nothing}
end

list(x) = x == 0 ? nothing : CustomListType(1, 1.0, list(x - 1))
tree(x) = x == 0 ? nothing : CustomTreeType("!!!", tree(x - 1), tree(x - 1))

const micros = Dict(
    "integer" => 88,
    "float" => -88.8,
    "ascii" => "Hello World!",
    "ascii-1024" => "x" ^ 1024,
    "unicode" => "ສະ​ບາຍ​ດີ​ຊາວ​ໂລກ!",
    "unicode-1024" => "ℜ" ^ 1024,
    "bool" => true,
    "null" => nothing,
    "flat-homogenous-array-16" => collect(1:16),
    "flat-homogenous-array-1024" => collect(1:1024),
    "heterogenous-array" => [
        1, 2, 3, 7, "A", "C", "E", "N", "Q", "R", "Shuttle to Grand Central"],
    "nested-array-16^2" => [collect(1:16) for _ in 1:16],
    "nested-array-16^3" => [[collect(1:16) for _ in 1:16] for _ in 1:16],
    "small-dict" => Dict(
        :a => :b, :c => "💙💙💙💙💙💙", :e => 10, :f => Dict(:a => :b)),
    "flat-dict-128" => Dict(zip(collect(1:128), collect(1:128))),
    "date" => Date(2016, 08, 09),
    "matrix-16" => [i == j ? 1.0 : 0.0 for i in 1:16, j in 1:16],
    "custom-list-128" => list(128),
    "custom-tree-8" => tree(8))

for (k, v) in micros
    io = IOBuffer()
    suite["print"][k] = @benchmarkable JSON.print($(IOBuffer()), $v)
    suite["pretty-print"][k] = @benchmarkable JSON.print(
        $(IOBuffer()), $v, 4)
end
