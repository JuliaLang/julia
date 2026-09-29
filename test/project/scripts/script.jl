#!/usr/bin/env julia
# /// project
# name = "InlineScriptTest"
# [deps]
# Random = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"
# Rot13 = "43ef800a-eac4-47f4-949b-25107b932e8f"
# ///

using Random
using Rot13

println("Active project: ", Base.active_project())
println("Active manifest: ", Base.active_manifest())
println("rot13: ", Rot13.rot13("Hello"))
println("rand: ", rand(MersenneTwister(1), 1:10) isa Int)

# /// manifest
# julia_version = "1.13.0"
# manifest_format = "2.0"
#
# [[deps.Random]]
# uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"
# version = "1.11.0"
#
# [[deps.Rot13]]
# path = "../Rot13"
# uuid = "43ef800a-eac4-47f4-949b-25107b932e8f"
# version = "0.1.0"
# ///
