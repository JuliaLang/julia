# This file is a part of Julia. License is MIT: http://julialang.org/license

module TestDates

using Base.Dates
using Base.Test
@testset "dates" begin
include("dates/types.jl")
include("dates/periods.jl")
include("dates/accessors.jl")
include("dates/query.jl")
include("dates/arithmetic.jl")
include("dates/conversions.jl")
include("dates/ranges.jl")
include("dates/adjusters.jl")
include("dates/io.jl")
end
end
