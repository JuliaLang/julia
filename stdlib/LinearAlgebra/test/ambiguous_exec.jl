# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, LinearAlgebra
@test detect_ambiguities(LinearAlgebra; imported=true, recursive=true) == []
