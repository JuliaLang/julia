#!/usr/bin/env julia

#!project begin
# name = "PortableScriptTest"
# uuid = "f7e12c4d-9a2b-4c3f-8e5d-6a7b8c9d0e1f"
# version = "0.1.0"
# [deps]
# Random = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"
# Test = "8dfed614-e22c-5e08-85e1-65c5234f0b40"
# Rot13 = "43ef800a-eac4-47f4-949b-25107b932e8f"
#!project end

using Random
using Test
using Rot13

# Verify the portable script environment is active
println("Active project: ", Base.active_project())
println("Active manifest: ", Base.active_manifest())
println()

# Test that stdlib packages work
@testset "Portable Script Tests" begin
    # Test Random (stdlib)
    Random.seed!(42)
    r = rand()
    @test 0 <= r <= 1
    println("✓ Random (stdlib) loaded successfully")

    # Test Rot13 (path-based dependency)
    @test Rot13.rot13("Hello") == "Uryyb"
    @test Rot13.rot13("World") == "Jbeyq"
    println("✓ Rot13 (path dependency) loaded successfully")

    # Test that Rot13 module has expected functions
    @test hasmethod(Rot13.rot13, (Char,))
    @test hasmethod(Rot13.rot13, (AbstractString,))
    println("✓ Rot13 methods available")
end


#!manifest begin
# julia_version = "1.13.0"
# manifest_format = "2.0"
# project_hash = "abc123"
#
# [[deps.Random]]
# uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"
# version = "1.11.0"
#
# [[deps.Test]]
# deps = ["InteractiveUtils", "Logging", "Random", "Serialization"]
# uuid = "8dfed614-e22c-5e08-85e1-65c5234f0b40"
# version = "1.11.0"
#
# [[deps.InteractiveUtils]]
# deps = ["Markdown"]
# uuid = "b77e0a4c-d291-57a0-90e8-8db25a27a240"
# version = "1.11.0"
#
# [[deps.Logging]]
# uuid = "56ddb016-857b-54e1-b83d-db4d58db5568"
# version = "1.11.0"
#
# [[deps.Serialization]]
# uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"
# version = "1.11.0"
#
# [[deps.Markdown]]
# deps = ["Base64", "JuliaSyntaxHighlighting", "StyledStrings"]
# uuid = "d6f4376e-aef5-505a-96c1-9c027394607a"
# version = "1.11.0"
#
# [[deps.Base64]]
# uuid = "2a0f44e3-6c83-55bd-87e4-b1978d98bd5f"
# version = "1.11.0"
#
# [[deps.JuliaSyntaxHighlighting]]
# deps = ["StyledStrings"]
# uuid = "ac6e5ff7-fb65-4e79-a425-ec3bc9c03011"
# version = "1.12.0"
#
# [[deps.StyledStrings]]
# uuid = "f489334b-da3d-4c2e-b8f0-e476e12c162b"
# version = "1.11.0"
#
# [[deps.Rot13]]
# path = "Rot13"
# uuid = "43ef800a-eac4-47f4-949b-25107b932e8f"
# version = "0.1.0"
#!manifest end
