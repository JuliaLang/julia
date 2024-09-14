# This file is a part of Julia. License is MIT: https://julialang.org/license

function precompile_test_harness(@nospecialize(f), testset::String)
    @testset "$testset" precompile_test_harness(f, true)
end
function precompile_test_harness(@nospecialize(f), separate::Bool=true)
    load_path = mktempdir()
    load_cache_path = separate ? mktempdir() : load_path
    try
        pushfirst!(LOAD_PATH, load_path)
        pushfirst!(DEPOT_PATH, load_cache_path)
        f(load_path)
    finally
        try
            rm(load_path, force=true, recursive=true)
        catch err
            @show err
        end
        if separate
            try
                rm(load_cache_path, force=true, recursive=true)
            catch err
                @show err
            end
        end
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
