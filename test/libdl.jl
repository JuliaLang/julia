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

@test !isempty(Libdl.find_library(["libccalltest"], [dirname(@__FILE__)]))

# dlopen should be able to handle absolute and relative paths, with and without dlext
let dl = Libdl.dlopen_e(abspath("./libccalltest"))
    @test dl != C_NULL
    Libdl.dlclose(dl)
end

let dl = Libdl.dlopen_e(join((abspath("./libccalltest"), string(".", Libdl.dlext))))
    @test dl != C_NULL
    Libdl.dlclose(dl)
end

let dl = Libdl.dlopen_e("./libccalltest")
    @test dl != C_NULL
    Libdl.dlclose(dl)
end

let dl = Libdl.dlopen_e(join(("./libccalltest", string(".", Libdl.dlext))))
    @test dl != C_NULL
    Libdl.dlclose(dl)
end

let dl = Libdl.dlopen_e("./foo")
    @test dl == C_NULL
end

# test dlpath
let dl = Libdl.dlopen("./libccalltest")
    try
        @test dl != C_NULL
        @test Base.samefile(abspath(Libdl.dlpath(dl)),
                            abspath(Libdl.dlpath("./libccalltest")))
        @test Base.samefile(abspath(Libdl.dlpath(dl)),
                            abspath(join(("./libccalltest",Libdl.dlext), '.')))
    finally
        Libdl.dlclose(dl)
    end
end

# opening a library that does not exist throws an ErrorException
@test_throws ErrorException Libdl.dlopen("./foo")

# test dlsym
let dl = Libdl.dlopen("./libccalltest")
    try
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
