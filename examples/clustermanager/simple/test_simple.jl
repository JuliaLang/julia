# This file is a part of Julia. License is MIT: http://julialang.org/license

cmanpath = joinpath(dirname(@__FILE__), "UnixDomainCM.jl")
include(cmanpath)

npids = addprocs(UnixDomainCM(2))
assert(length(npids) == 2)
test_pids = [remotecall_fetch(myid, x) for x in npids]
assert(npids == test_pids)
rmprocs(npids; waitfor=1.0)

exit(0)
