import Pkg

Pkg.activate(".")

using Test
using JSON
using Serialization

@test length(ARGS) == 1
bindir = dirname(ARGS[1])

let exe_suffix = splitext(Base.julia_exename())[2]

    hello_exe = joinpath(bindir, "hello" * exe_suffix)
    @test readchomp(`$hello_exe arg1 arg2`) == "Hello, world!"
    @test filesize(hello_exe) < 1_900_000

    trimmability_exe = joinpath(bindir, "trimmability" * exe_suffix)
    lines = split(readchomp(`$trimmability_exe arg1 arg2`), "\n")
    @test lines[1] == "Hello, world!"
    @test lines[2] == trimmability_exe
    @test lines[3] == "arg1"
    @test lines[4] == "arg2"
    @test lines[5] == string(4.0+pi)
    @test parse(Float64, lines[6]) isa Float64
    @test lines[7] == "Version: 1.1.0"
    @test lines[8] == "# preferences: 0"

    basic_jll_exe = joinpath(bindir, "basic_jll" * exe_suffix)
    lines = split(readchomp(`$basic_jll_exe`), "\n")
    @test lines[1] == "Julia! Hello, world!"
    @test lines[2] == lines[3]
    @test Base.VersionNumber(lines[2]) ≥ v"1.5.7"
    @test filesize(basic_jll_exe) < filesize(unsafe_string(Base.JLOptions().image_file))/10

    # Test that the shared library can be used in a C application
    capplication_exe = joinpath(bindir, "capplication" * exe_suffix)
    lines = split(readchomp(`$capplication_exe`), "\n")
    @test length(lines) == 2
    @test lines[1] == "Sum of copied values: 6.000000"
    @test lines[2] == "Count of same vectors: 1"

    # Test that the logging of entrypoints and types works correctly
    str = read(joinpath(bindir, "bindinginfo_libsimple.json"), String)

    # The log should parse as valid JSON
    abi = JSON.parse(str)

    # `copyto_and_sum` should have been exported
    @test any(Bool[func["symbol"] == "copyto_and_sum" for func in abi["functions"]])

    # `CVector{Float32}` should have been exported with the correct info
    @test any(Bool[type["name"] == "CVector{Float32}" for type in abi["types"]])
    CVector_Float32 = abi["types"][findfirst(type["name"] == "CVector{Float32}" for type in abi["types"])]
    @test length(CVector_Float32["fields"]) == 2
    @test CVector_Float32["fields"][1]["offset"] == 0
    @test CVector_Float32["fields"][2]["offset"] == 8
    @test abi["types"][CVector_Float32["fields"][1]["type_id"]]["name"] == "Int32"
    @test abi["types"][CVector_Float32["fields"][2]["type_id"]]["name"] == "Ptr{Float32}"
    @test CVector_Float32["size"] == 16

    # `CVectorPair{Float32}` should have been exported with the correct info
    @test any(Bool[type["name"] == "CVectorPair{Float32}" for type in abi["types"]])
    CVectorPair_Float32 = abi["types"][findfirst(type["name"] == "CVectorPair{Float32}" for type in abi["types"])]
    @test length(CVectorPair_Float32["fields"]) == 2
    @test CVectorPair_Float32["fields"][1]["offset"] == 0
    @test CVectorPair_Float32["fields"][2]["offset"] == 16
    @test abi["types"][CVectorPair_Float32["fields"][1]["type_id"]]["name"] == "CVector{Float32}"
    @test abi["types"][CVectorPair_Float32["fields"][2]["type_id"]]["name"] == "CVector{Float32}"
    @test CVectorPair_Float32["size"] == 32

    # `CTree{Float64}` should have been exported with the correct info
    @test any(Bool[type["name"] == "CTree{Float64}" for type in abi["types"]])
    CTree_Float64_id = findfirst(type["name"] == "CTree{Float64}" for type in abi["types"])
    CTree_Float64 = abi["types"][CTree_Float64_id]
    @test length(CTree_Float64["fields"]) == 1
    @test CTree_Float64["fields"][1]["offset"] == 0
    CVector_CTree_Float64 = abi["types"][CTree_Float64["fields"][1]["type_id"]]
    @test CVector_CTree_Float64["name"] == "CVector{CTree{Float64}}"
    @test CTree_Float64["size"] == sizeof(UInt) * 2

    # `CVector{CTree{Float64}}` should have been exported with the correct info
    @test length(CVector_CTree_Float64["fields"]) == 2
    @test CVector_CTree_Float64["fields"][1]["offset"] == 0
    @test CVector_CTree_Float64["fields"][2]["offset"] == sizeof(UInt)
    @test abi["types"][CVector_CTree_Float64["fields"][1]["type_id"]]["name"] == "Int32"
    @test abi["types"][CVector_CTree_Float64["fields"][2]["type_id"]]["name"] == "Ptr{CTree{Float64}}"
    @test CVector_CTree_Float64["size"] == sizeof(UInt) * 2

    # `Ptr{CTree{Float64}}` should refer (recursively) back to the original type id
    Ptr_CTree_Float64 = abi["types"][CVector_CTree_Float64["fields"][2]["type_id"]]
    @test Ptr_CTree_Float64["pointee_type_id"] == CTree_Float64_id

    # Test that Serialization.serialize supports trimming
    serialization_exe = joinpath(bindir, "serialization" * exe_suffix)
    @test readchomp(`$serialization_exe`) == "serialization ok"
    # Verify stdlib types were serialized correctly
    result = deserialize(joinpath(bindir, "_trim_stdlib.jls"))
    @test result == (42, "hello", :sym, (1, 2.0), Int[10, 20, 30])
    # Verify custom struct file was produced and round-trips, including the
    # `Memory{Union{Int,String}}` field (regression test for trim-safe
    # serialization of `Union`-valued DataType parameters). The struct
    # definitions must match those in `serialization.jl`.
    @eval mutable struct TrimSerMut
        s::String
        m::Memory{Int}
        u::Union{Int, Nothing}
        t::Tuple{Int, String}
        mu::Memory{Union{Int, String}}
    end
    @eval struct TrimSerImm
        a::Int
        b::TrimSerMut
    end
    @test filesize(joinpath(bindir, "_trim_custom.jls")) > 0
    custom = deserialize(joinpath(bindir, "_trim_custom.jls"))
    @test custom.a == 42
    @test custom.b.s == "hello"
    @test custom.b.u === 7
    @test custom.b.t == (3, "world")
    @test custom.b.mu isa Memory{Union{Int, String}}
    @test custom.b.mu[1] === 123
    @test custom.b.mu[2] == "abc"
    # Clean up
    rm(joinpath(bindir, "_trim_stdlib.jls"), force=true)
    rm(joinpath(bindir, "_trim_custom.jls"), force=true)
end
