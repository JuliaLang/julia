# This file is a part of Julia. License is MIT: http://julialang.org/license

## generic string uses only endof and next; used for testing ##
@testset "strings" begin
include("strings/basic.jl")
include("strings/types.jl")
include("strings/search.jl")
include("strings/util.jl")
include("strings/io.jl")
end
