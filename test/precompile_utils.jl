# This file is a part of Julia. License is MIT: https://julialang.org/license

include("tempdepot.jl")

function precompile_test_harness(@nospecialize(f), testset::String)
    @testset "$testset" precompile_test_harness(f, true)
end
function precompile_test_harness(@nospecialize(f), separate::Bool=true)
    load_path = mkdepottempdir()
    load_cache_path = separate ? mkdepottempdir() : load_path
    try
        pushfirst!(LOAD_PATH, load_path)
        pushfirst!(DEPOT_PATH, load_cache_path)
        f(load_path)
    finally
        filter!((≠)(load_path), LOAD_PATH)
        separate && filter!((≠)(load_cache_path), DEPOT_PATH)
    end
    return nothing
end

let original_depot_path = copy(Base.DEPOT_PATH)
    original_load_path = copy(Base.LOAD_PATH)

    global function finish_precompile_test!()
        empty!(Base.DEPOT_PATH)
        append!(Base.DEPOT_PATH, original_depot_path)
        empty!(Base.LOAD_PATH)
        append!(Base.LOAD_PATH, original_load_path)
    end
end

function check_presence(mi, token)
    ci = isdefined(mi, :cache) ? mi.cache : nothing
    while ci !== nothing
        @assert ci.max_world != Base.ReinferUtils.WORLD_AGE_REVALIDATION_SENTINEL
        if ci.owner === token
            @test ci.max_world == Base.ReinferUtils.WORLD_AGE_REVALIDATION_SENTINEL || ci.min_world <= 1
            return ci
        end
        ci = isdefined(ci, :next) ? ci.next : nothing
    end
    return nothing
end
