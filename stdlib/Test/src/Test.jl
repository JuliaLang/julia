# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

"""
Simple unit testing functionality:

* `@test`
* `@test_throws`

All tests belong to a *test set*. There is a default, task-level
test set that throws on the first failure. Users can choose to wrap
their tests in (possibly nested) test sets that will store results
and summarize them at the end of the test set with `@testset`.
"""
module Test

export @test, @test_throws, @test_broken, @test_skip,
    @test_warn, @test_nowarn,
    @test_logs, @test_deprecated
export @testset
# Legacy approximate testing functions, yet to be included
export @inferred
export detect_ambiguities, detect_unbound_args
export GenericString, GenericSet, GenericDict, GenericArray
export guardsrand, TestSetException

import Distributed: myid

#-----------------------------------------------------------------------
include("utils.jl")
include("result_type.jl")
include("execution.jl")

include("testsets.jl")
include("fallback_testset.jl")
include("default_testset.jl")



include("method_tests.jl")
include("generic_types.jl")


include("deprecations.jl")
include("logging.jl")

include("specialised_tests.jl")

end # module
