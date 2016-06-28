# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "libdl" begin
# these could fail on an embedded installation
# but for now, we don't handle that case
dlls = Libdl.dllist()
@test !isempty(dlls)
@test length(dlls) > 3 # at a bare minimum, probably have some version of libstdc, libgcc, libjulia, ...
if !is_windows() || Sys.windows_version() >= Sys.WINDOWS_VISTA_VER
    for dl in dlls
        if isfile(dl) && (Libdl.dlopen_e(dl) != C_NULL)
            @test Base.samefile(Libdl.dlpath(dl), dl)
        end
    end
end
@test length(filter(dlls) do dl
        return ismatch(Regex("^libjulia(?:.*)\.$(Libdl.dlext)(?:\..+)?\$"), basename(dl))
    end) == 1 # look for something libjulia-like (but only one)

# library handle pointer must not be NULL
@test_throws ArgumentError Libdl.dlsym(C_NULL, :foo)
@test_throws ArgumentError Libdl.dlsym_e(C_NULL, :foo)

cd(dirname(@__FILE__)) do

# Find the library directory by finding the path of libjulia (or libjulia-debug, as the case may be)
# and then adding on /julia to that directory path to get the private library directory, if we need
# to (where "need to" is defined as private_libdir/julia/libccalltest.dlext exists
private_libdir = if ccall(:jl_is_debugbuild, Cint, ()) != 0
    dirname(abspath(Libdl.dlpath("libjulia-debug")))
else
    dirname(abspath(Libdl.dlpath("libjulia")))
end

if isfile(joinpath(private_libdir,"julia","libccalltest."*Libdl.dlext))
    private_libdir = joinpath(private_libdir, "julia")
end

@test !isempty(Libdl.find_library(["libccalltest"], [private_libdir]))
@test !isempty(Libdl.find_library("libccalltest", [private_libdir]))
@test !isempty(Libdl.find_library(:libccalltest, [private_libdir]))

# dlopen should be able to handle absolute and relative paths, with and without dlext
let dl = C_NULL
    try
        dl = Libdl.dlopen_e(abspath(joinpath(private_libdir, "libccalltest")))
        @test dl != C_NULL
    finally
        Libdl.dlclose(dl)
    end
end

let dl = C_NULL
    try
        dl = Libdl.dlopen_e(abspath(joinpath(private_libdir, "libccalltest.$(Libdl.dlext)")))
        @test dl != C_NULL
    finally
        Libdl.dlclose(dl)
    end
end

let dl = C_NULL
    try
        dl = Libdl.dlopen_e(relpath(joinpath(private_libdir, "libccalltest")))
        @test dl != C_NULL
    finally
        Libdl.dlclose(dl)
    end
end

let dl = C_NULL
    try
        dl = Libdl.dlopen_e(relpath(joinpath(private_libdir, "libccalltest.$(Libdl.dlext)")))
        @test dl != C_NULL
    finally
        Libdl.dlclose(dl)
    end
end

let dl = C_NULL
    try
        dl = Libdl.dlopen_e("./foo")
        @test dl == C_NULL
    finally
        Libdl.dlclose(dl)
    end
end

# unqualified names present in DL_LOAD_PATH
let dl = C_NULL
    try
        dl = Libdl.dlopen_e("libccalltest")
        @test dl != C_NULL
    finally
        Libdl.dlclose(dl)
    end
end

let dl = C_NULL
    try
        dl = Libdl.dlopen_e(string("libccalltest",".",Libdl.dlext))
        @test dl != C_NULL
    finally
        Libdl.dlclose(dl)
    end
end

# path with dlopen-able file first in load path
#=
let dl = C_NULL,
    tmpdir = mktempdir(),
    fpath = joinpath(tmpdir,"libccalltest")
    try
        write(open(fpath,"w"))
        push!(Libdl.DL_LOAD_PATH, dirname(@__FILE__))
        push!(Libdl.DL_LOAD_PATH, dirname(fpath))
        dl = Libdl.dlopen_e("libccalltest")
        @test dl != C_NULL
    finally
        pop!(Libdl.DL_LOAD_PATH)
        pop!(Libdl.DL_LOAD_PATH)
        rm(tmpdir, recursive=true)
    end
end
=#

# path with dlopen-able file second in load path
#=
let dl = C_NULL,
    tmpdir = mktempdir(),
    fpath = joinpath(tmpdir,"libccalltest")
    try
        write(open(fpath,"w"))
        push!(Libdl.DL_LOAD_PATH, dirname(fpath))
        push!(Libdl.DL_LOAD_PATH, dirname(@__FILE__))
        dl = Libdl.dlopen_e("libccalltest")
        @test dl != C_NULL
    finally
        pop!(Libdl.DL_LOAD_PATH)
        pop!(Libdl.DL_LOAD_PATH)
        rm(tmpdir, recursive=true)
    end
end
=#

# test dlpath
let dl = C_NULL
    try
        path = abspath(joinpath(private_libdir, "libccalltest"))
        dl = Libdl.dlopen(path)
        @test dl != C_NULL
        @test Base.samefile(abspath(Libdl.dlpath(dl)),
                            abspath(Libdl.dlpath(path)))
        @test Base.samefile(abspath(Libdl.dlpath(dl)),
                            string(path,".",Libdl.dlext))
    finally
        Libdl.dlclose(dl)
    end
end

# opening a library that does not exist throws an ErrorException
@test_throws ErrorException Libdl.dlopen("./foo")

# test dlsym
let dl = C_NULL
    try
        dl = Libdl.dlopen(abspath(joinpath(private_libdir, "libccalltest")))
        fptr = Libdl.dlsym(dl, :set_verbose)
        @test fptr != C_NULL
        @test_throws ErrorException Libdl.dlsym(dl, :foo)

        fptr = Libdl.dlsym_e(dl, :set_verbose)
        @test fptr != C_NULL
        fptr = Libdl.dlsym_e(dl, :foo)
        @test fptr == C_NULL
    finally
        Libdl.dlclose(dl)
    end
end

if Sys.KERNEL in (:Linux, :FreeBSD)
    ccall(:jl_read_sonames, Void, ())
end

end

end
