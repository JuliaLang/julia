#!/usr/bin/env julia

#!project begin
#=
name = "PortableScriptMultilineTest"
uuid = "a1b2c3d4-e5f6-7890-abcd-ef1234567890"
version = "0.1.0"

[deps]
Random = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"
=#
#!project end

#!manifest begin
#=
julia_version = "1.13.0"
manifest_format = "2.0"
project_hash = "xyz789"

[[deps.Random]]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"
version = "1.11.0"
=#
#!manifest end

using Random

println("✓ Portable script with multiline comment syntax works!")
println("Active project: ", Base.active_project())
println("Active manifest: ", Base.active_manifest())

# Simple test
Random.seed!(123)
x = rand()
println("✓ Random.rand() = ", x)
@assert 0 <= x <= 1

println("✓ All checks passed!")
