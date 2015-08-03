# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

dir = mktempdir()
insert!(LOAD_PATH, 1, dir)
insert!(Base.LOAD_CACHE_PATH, 1, dir)
Foo_module = :Foo4b3a94a1a081a8cb
try
    file = joinpath(dir, "$Foo_module.jl")

    open(file, "w") do f
        print(f, """
              module $Foo_module
              @doc "foo function" foo(x) = x + 1
              include_dependency("foo.jl")
              module Bar
              @doc "bar function" bar(x) = x + 2
              include_dependency("bar.jl")
              end
              end
              """)
    end

    cachefile = Base.compile(Foo_module)

    # use _require_from_serialized to ensure that the test fails if
    # the module doesn't load from the image:
    @test nothing !== Base._require_from_serialized(myid(), Foo_module, true)

    let Foo = eval(Main, Foo_module)
        @test Foo.foo(17) == 18
        @test Foo.Bar.bar(17) == 19

        # issue #12284:
        @test stringmime("text/plain", Base.Docs.doc(Foo.foo)) == "foo function\n"
        @test stringmime("text/plain", Base.Docs.doc(Foo.Bar.bar)) == "bar function\n"

        deps = Base.cache_dependencies(cachefile)
        @test sort(deps[1]) == map(s -> (s, Base.module_uuid(eval(s))),
                                   [:Base,:Core,:Main])
        @test sort(deps[2]) == [file,joinpath(dir,"bar.jl"),joinpath(dir,"foo.jl")]
    end

finally
    splice!(Base.LOAD_CACHE_PATH, 1)
    splice!(LOAD_PATH, 1)
    rm(dir, recursive=true)
end
