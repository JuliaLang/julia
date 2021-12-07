include("stdlib/AllocProfile/src/AllocProfile.jl")

ccall(:jl_start_alloc_profile, Cvoid, (Cint,), 0)

using Base64

raw_results = ccall(:jl_stop_alloc_profile, AllocProfile.RawAllocResults, ())
raw_alloc = unsafe_load(raw_results.allocs, 3)
AllocProfile._reformat_bt(raw_alloc.backtrace)

