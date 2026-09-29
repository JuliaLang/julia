#!/usr/bin/env julia
# A comment before the block is fine.
# /// project
# manifest = "external.toml"
# [deps]
# Random = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"
# ///
using Random
println("Active project: ", Base.active_project())
println("Active manifest: ", Base.active_manifest())
