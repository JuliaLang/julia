#!/usr/bin/env julia
#!script

#!project begin
# name = "PortableScriptCustomManifestTest"
# uuid = "1a2b3c4d-5e6f-7890-abcd-ef1234567890"
# version = "0.1.0"
# manifest = "script_custom.toml"
# [deps]
# Random = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"
#!project end

using Random

# Test that the custom manifest is being used
println("Active project: ", Base.active_project())
println("Active manifest: ", Base.active_manifest())

# Verify that the active manifest points to our custom manifest file
expected_manifest = joinpath(dirname(Base.active_project()), "script_custom.toml")
actual_manifest = Base.active_manifest()

if actual_manifest == expected_manifest
    println("✓ Custom manifest file is being used: $actual_manifest")
else
    error("Expected manifest: $expected_manifest, but got: $actual_manifest")
end

# Test that Random works
Random.seed!(456)
x = rand()
@assert 0 <= x <= 1
println("✓ Random.rand() = ", x)

println("✓ All checks passed!")
