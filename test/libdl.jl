# This file is a part of Julia. License is MIT: http://julialang.org/license

# these could fail on an embedded installation
# but for now, we don't handle that case
dlls = Libdl.dllist()
@test !isempty(dlls)
@test length(dlls) > 3 # at a bare minimum, probably have some version of libstdc, libgcc, libjulia, ...
if @unix? true : (Base.windows_version() >= Base.WINDOWS_VISTA_VER)
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

# @test !isempty(Libdl.find_library(["libccalltest"], [dirname(@__FILE__)]))

# dlopen should be able to handle absolute and relative paths, with and without dlext
let dl = C_NULL
    try
        dl = Libdl.dlopen_e(abspath("./libccalltest"))
        @test dl != C_NULL
    finally
        Libdl.dlclose(dl)
    end
end

let dl = C_NULL
    try
        dl = Libdl.dlopen_e(join((abspath("./libccalltest"), string(".", Libdl.dlext))))
        @test dl != C_NULL
    finally
        Libdl.dlclose(dl)
    end
end

let dl = C_NULL
    try
        dl = Libdl.dlopen_e("./libccalltest")
        @test dl != C_NULL
    finally
        Libdl.dlclose(dl)
    end
end

let dl = C_NULL
    try
        dl = Libdl.dlopen_e(join(("./libccalltest", string(".", Libdl.dlext))))
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
    push!(Libdl.DL_LOAD_PATH, pwd())
    try
        dl = Libdl.dlopen_e("libccalltest")
        @test dl != C_NULL
    finally
        Libdl.dlclose(dl)
        pop!(Libdl.DL_LOAD_PATH)
    end
end

let dl = C_NULL
    push!(Libdl.DL_LOAD_PATH, pwd())
    try
        dl = Libdl.dlopen_e(string("libccalltest",".",Libdl.dlext))
        @test dl != C_NULL
    finally
        Libdl.dlclose(dl)
        pop!(Libdl.DL_LOAD_PATH)
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
        dl = Libdl.dlopen("./libccalltest")
        @test dl != C_NULL
        @test Base.samefile(abspath(Libdl.dlpath(dl)),
                            abspath(Libdl.dlpath("./libccalltest")))
        @test Base.samefile(abspath(Libdl.dlpath(dl)),
                            abspath(string("./libccalltest",".", Libdl.dlext)))
    finally
        Libdl.dlclose(dl)
    end
end

# opening a library that does not exist throws an ErrorException
@test_throws ErrorException Libdl.dlopen("./foo")

# test dlsym
let dl = C_NULL
    try
        dl = Libdl.dlopen("./libccalltest")
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

end
