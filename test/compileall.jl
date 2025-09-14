# This test builds a full system image, so it can take a little while.
# We make it a separate test target here, so that it can run in parallel
# with the rest of the tests.

function precompile_test_harness(@nospecialize(f))
    load_path = mktempdir()
    try
        pushfirst!(LOAD_PATH, load_path)
        pushfirst!(DEPOT_PATH, load_path)
        f(load_path)
    finally
        try
            rm(load_path, force=true, recursive=true)
        catch err
            @show err
        end
        filter!((≠)(load_path), LOAD_PATH)
        filter!((≠)(load_path), DEPOT_PATH)
    end
    return nothing
end

precompile_test_harness() do dir
    Foo_file = joinpath(dir, "OncePerFoo.jl")
    image_file = joinpath(dir, "img.jl")
    write(Foo_file,
    """module OncePerFoo

    const f = OncePerThread{Nothing}() do
        println(Core.stdout, "Running thread init...")
    end

    f() # Executed during pre-compilation

    end # module OncePerFoo
    """)

    write(image_file,
    """
    Base.init_depot_path()
    Base.init_load_path()
    using OncePerFoo

    function main()
        OncePerFoo.f()
        return 0
    end

    OncePerFoo.f() # fire init during compilation time

    """)
    Base.compilecache(Base.PkgId("OncePerFoo"))
    new_env = Dict(["JULIA_DEPOT_PATH" => join(DEPOT_PATH, Sys.iswindows() ? ';' : ':'),
               "JULIA_LOAD_PATH" => join(LOAD_PATH, Sys.iswindows() ? ';' : ':')])
    @test success(pipeline(addenv(`$(Base.julia_cmd()) --compile=all -t1,0 --strip-ir --output-o $(dir)/sys.o.a $(image_file) `, new_env), stderr=stderr, stdout=stdout)) skip=(Sys.WORD_SIZE == 32)
    if isfile(joinpath(dir, "sys.o.a"))
        Base.Linking.link_image(joinpath(dir, "sys.o.a"), joinpath(dir, "sys.so"))
        str = readchomp(`$(Base.julia_cmd()) -t1,0 -J  $(dir)/sys.so -e 'Base.scrub_repl_backtrace(nothing); println("loaded"); main()'`)
        @test split(str, '\n') == ["loaded", "Running thread init..."]
    end
end
