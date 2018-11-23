# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, SparseArrays
@test detect_ambiguities(SparseArrays; imported=true, recursive=true) == []
