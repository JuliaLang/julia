# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
import Libdl

# these could fail on an embedded installation
# but for now, we don't handle that case
dlls = Libdl.dllist()
@test !isempty(dlls)
@test length(dlls) > 3 # at a bare minimum, probably have some version of libstdc, libgcc, libjulia, ...
if !Sys.iswindows() || Sys.windows_version() >= Sys.WINDOWS_VISTA_VER
    for dl in dlls
        if isfile(dl) && (Libdl.dlopen(dl; throw_error=false) !== nothing)
            @test Base.samefile(Libdl.dlpath(dl), dl)
        end
    end
end
@test length(filter(dlls) do dl
      if Base.DARWIN_FRAMEWORK
          return occursin(Regex("^$(Base.DARWIN_FRAMEWORK_NAME)(?:_debug)?\$"), basename(dl))
      else
          return occursin(Regex("^libjulia(?:.*)\\.$(Libdl.dlext)(?:\\..+)?\$"), basename(dl))
      end
    end) == 1 # look for something libjulia-like (but only one)

# library handle pointer must not be NULL
@test_throws ArgumentError Libdl.dlsym(C_NULL, :foo)
@test_throws ArgumentError Libdl.dlsym_e(C_NULL, :foo)

cd(@__DIR__) do

# Find the library directory by finding the path of libjulia (or libjulia-debug, as the case may be)
# and then adding on /julia to that directory path to get the private library directory, if we need
# to (where "need to" is defined as private_libdir/julia/libccalltest.dlext exists
private_libdir = if Base.DARWIN_FRAMEWORK
    if ccall(:jl_is_debugbuild, Cint, ()) != 0
        dirname(abspath(Libdl.dlpath(Base.DARWIN_FRAMEWORK_NAME * "_debug")))
    else
        joinpath(dirname(abspath(Libdl.dlpath(Base.DARWIN_FRAMEWORK_NAME))),"Frameworks")
    end
elseif ccall(:jl_is_debugbuild, Cint, ()) != 0
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
        dl = Libdl.dlopen(abspath(joinpath(private_libdir, "libccalltest")); throw_error=false)
        @test dl !== nothing
    finally
        Libdl.dlclose(dl)
    end
end

let dl = C_NULL
    try
        dl = Libdl.dlopen(abspath(joinpath(private_libdir, "libccalltest.$(Libdl.dlext)")); throw_error=false)
        @test dl !== nothing
    finally
        Libdl.dlclose(dl)
    end
end

let dl = C_NULL
    try
        dl = Libdl.dlopen(relpath(joinpath(private_libdir, "libccalltest")); throw_error=false)
        @test dl !== nothing
    finally
        Libdl.dlclose(dl)
    end
end

let dl = C_NULL
    try
        dl = Libdl.dlopen(relpath(joinpath(private_libdir, "libccalltest.$(Libdl.dlext)")); throw_error=false)
        @test dl !== nothing
    finally
        Libdl.dlclose(dl)
    end
end

let dl = C_NULL
    try
        dl = Libdl.dlopen("./foo"; throw_error=false)
        @test dl === nothing
    finally
        Libdl.dlclose(dl)
    end
end

# unqualified names present in DL_LOAD_PATH
let dl = C_NULL
    try
        dl = Libdl.dlopen("libccalltest"; throw_error=false)
        @test dl !== nothing
    finally
        Libdl.dlclose(dl)
    end
end

let dl = C_NULL
    try
        dl = Libdl.dlopen(string("libccalltest",".",Libdl.dlext); throw_error=false)
        @test dl !== nothing
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
        push!(Libdl.DL_LOAD_PATH, @__DIR__)
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
        push!(Libdl.DL_LOAD_PATH, @__DIR__)
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
        dl = Libdl.dlopen(path; throw_error=false)
        @test dl !== nothing
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

# opening a versioned library that does not exist does not result in adding extension twice
err = @test_throws ErrorException Libdl.dlopen("./foo.$(Libdl.dlext).0")
@test !occursin("foo.$(Libdl.dlext).0.$(Libdl.dlext)", err.value.msg)
err = @test_throws ErrorException Libdl.dlopen("./foo.$(Libdl.dlext).0.22.1")
@test !occursin("foo.$(Libdl.dlext).0.22.1.$(Libdl.dlext)", err.value.msg)

# test dlsym
let dl = C_NULL
    try
        dl = Libdl.dlopen(abspath(joinpath(private_libdir, "libccalltest")))
        fptr = Libdl.dlsym(dl, :set_verbose)
        @test fptr !== nothing
        @test_throws ErrorException Libdl.dlsym(dl, :foo)

        fptr = Libdl.dlsym_e(dl, :set_verbose)
        @test fptr != C_NULL
        fptr = Libdl.dlsym_e(dl, :foo)
        @test fptr == C_NULL
    finally
        Libdl.dlclose(dl)
    end
end

# test do-block dlopen
Libdl.dlopen(abspath(joinpath(private_libdir, "libccalltest"))) do dl
    fptr = Libdl.dlsym(dl, :set_verbose)
    @test fptr !== nothing
    @test_throws ErrorException Libdl.dlsym(dl, :foo)

    fptr = Libdl.dlsym_e(dl, :set_verbose)
    @test fptr != C_NULL
    fptr = Libdl.dlsym_e(dl, :foo)
    @test fptr == C_NULL
end

# test dlclose
# If dl is NULL, jl_dlclose should return -1 and dlclose should return false
# dlclose should return true on success and false on failure
let dl = C_NULL
    @test -1 == ccall(:jl_dlclose, Cint, (Ptr{Cvoid},), dl)
    @test !Libdl.dlclose(dl)

    dl = Libdl.dlopen("libccalltest"; throw_error=false)
    @test dl !== nothing

    @test Libdl.dlclose(dl)
    @test_skip !Libdl.dlclose(dl)   # Syscall doesn't fail on Win32
end

# test DL_LOAD_PATH handling and @executable_path expansion
mktempdir() do dir
    # Create a `libdcalltest` in a directory that is not on our load path
    src_path = joinpath(private_libdir, "libccalltest.$(Libdl.dlext)")
    dst_path = joinpath(dir, "libdcalltest.$(Libdl.dlext)")
    cp(src_path, dst_path)

    # Add an absurdly long entry to the load path to verify it doesn't lead to a buffer overflow
    push!(Base.DL_LOAD_PATH, joinpath(dir, join(rand('a':'z', 10000))))

    # Add the temporary directors to load path by absolute path
    push!(Base.DL_LOAD_PATH, dir)

    # Test that we can now open that file
    Libdl.dlopen("libdcalltest") do dl
        fptr = Libdl.dlsym(dl, :set_verbose)
        @test fptr !== nothing
        @test_throws ErrorException Libdl.dlsym(dl, :foo)

        fptr = Libdl.dlsym_e(dl, :set_verbose)
        @test fptr != C_NULL
        fptr = Libdl.dlsym_e(dl, :foo)
        @test fptr == C_NULL
    end

    # Skip these tests if the temporary directory is not on the same filesystem
    # as the BINDIR, as in that case, a relative path will never work.
    if Base.Filesystem.splitdrive(dir)[1] != Base.Filesystem.splitdrive(Sys.BINDIR)[1]
        return
    end

    empty!(Base.DL_LOAD_PATH)
    push!(Base.DL_LOAD_PATH, joinpath(dir, join(rand('a':'z', 10000))))

    # Add this temporary directory to our load path, now using `@executable_path` to do so.
    push!(Base.DL_LOAD_PATH, joinpath("@executable_path", relpath(dir, Sys.BINDIR)))

    # Test that we can now open that file
    Libdl.dlopen("libdcalltest") do dl
        fptr = Libdl.dlsym(dl, :set_verbose)
        @test fptr !== nothing
        @test_throws ErrorException Libdl.dlsym(dl, :foo)

        fptr = Libdl.dlsym_e(dl, :set_verbose)
        @test fptr != C_NULL
        fptr = Libdl.dlsym_e(dl, :foo)
        @test fptr == C_NULL
    end
end

end
