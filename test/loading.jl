# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

@test @__LINE__ == 5

include("test_sourcepath.jl")
thefname = "the fname!//\\&\1*"
@test include_string("include_string_test() = @__FILE__", thefname)() == Base.source_path()
@test include_string("Base.source_path()", thefname) == Base.source_path()
@test basename(@__FILE__) == "loading.jl"
@test isabspath(@__FILE__)

# Issue #5789 and PR #13542:
let true_filename = "cAsEtEsT.jl", lowered_filename="casetest.jl"
    touch(true_filename)
    @test Base.isfile_casesensitive(true_filename)
    @test !Base.isfile_casesensitive(lowered_filename)
    rm(true_filename)
end

# Test Unicode normalization; pertinent for OS X
let nfc_name = "\U00F4.jl"
    touch(nfc_name)
    @test Base.isfile_casesensitive(nfc_name)
    rm(nfc_name)
end

let paddedname = "Ztest_sourcepath.jl"
    filename = SubString(paddedname, 2, length(paddedname))
    @test Base.find_in_path(filename) == abspath(paddedname[2:end])
end

# Verify that a symlink to a module does not result in that module being
# recompiled unnecessarily (see Issue #16007).
@unix_only let original_dir = mktempdir()
    new_dir = mktempdir()
    try
        # To test this, we create a new empty module called SymlinkPrecompileTest
        # which will live inside a temporary folder.
        pkg_dir = joinpath(original_dir, "SymlinkPrecompileTest")
        mkdir(pkg_dir)
        src_dir = joinpath(pkg_dir, "src")
        mkdir(src_dir)

        # The module code contains an explicit __precompile__ directive:
        open(joinpath(src_dir, "SymlinkPrecompileTest.jl"), "w") do f
            write(f, """
            __precompile__(true)
            module SymlinkPrecompileTest
            end
            """)
        end

        # Now we spawn a new julia process, with our module available on the
        # LOAD_PATH. This will trigger precompilation of the module.
        run(`$(JULIA_HOME)/julia -e "unshift!(LOAD_PATH, \"$(original_dir)\"); using SymlinkPrecompileTest"`)

        # We can verify that the module was compiled and store the timestamp of the
        # cache file.
        cache_path = joinpath(Base.LOAD_CACHE_PATH[1], "SymlinkPrecompileTest.ji")
        @test Base.isfile(cache_path)
        cache_modification_time = mtime(cache_path)

        # Now we create a new directory and symlink our module's folder into that
        # directory
        symlink(pkg_dir, joinpath(new_dir, "SymlinkPrecompileTest"))

        # Running julia with the new directory in LOAD_PATH should *not* trigger
        # recompilation of the module, so the cache file's modification time should
        # not change.
        run(`$(JULIA_HOME)/julia -e "unshift!(LOAD_PATH, \"$(new_dir)\"); using SymlinkPrecompileTest"`)
        @test mtime(cache_path) == cache_modification_time
    finally
        rm(original_dir, recursive=true)
        rm(new_dir, recursive=true)
    end
end
