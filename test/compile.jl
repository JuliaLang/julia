# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Distributed

import Base: root_module

Foo_module = :Foo4b3a94a1a081a8cb
Foo2_module = :F2oo4b3a94a1a081a8cb
FooBase_module = :FooBase4b3a94a1a081a8cb
@eval module ConflictingBindings
    export $Foo_module, $FooBase_module
    $Foo_module = 232
    $FooBase_module = 9134
end
using .ConflictingBindings

# FIXME: withenv() is a leftover from previous tests.  Oddly, one test below
# fails without it, in a mysterious way.
withenv( "JULIA_DEBUG_LOADING" => nothing ) do

dir = mktempdir()
dir2 = mktempdir()
insert!(LOAD_PATH, 1, dir)
insert!(Base.LOAD_CACHE_PATH, 1, dir)
try
    Foo_file = joinpath(dir, "$Foo_module.jl")
    Foo2_file = joinpath(dir, "$Foo2_module.jl")
    FooBase_file = joinpath(dir, "$FooBase_module.jl")

    write(FooBase_file,
          """
          __precompile__(true)

          module $FooBase_module
              import Base: hash, >
              struct fmpz end
              struct typeA end
              >(x::fmpz, y::Int) = Base.cmp(x, y) > 0
              function hash(a::typeA, h::UInt)
                  d = den(a)
                  return h
              end
          end
          """)
    write(Foo2_file,
          """
          __precompile__(true)

          module $Foo2_module
              export override
              override(x::Integer) = 2
              override(x::AbstractFloat) = Float64(override(1))
          end
          """)
    write(Foo_file,
          """
          __precompile__(true)

          module $Foo_module
              import $FooBase_module, $FooBase_module.typeA
              import $Foo2_module: $Foo2_module, override
              import $FooBase_module.hash
              import Test

              struct typeB
                  y::typeA
              end
              hash(x::typeB) = hash(x.y)

              # test that docs get reconnected
              @doc "foo function" foo(x) = x + 1
              include_dependency("foo.jl")
              include_dependency("foo.jl")
              module Bar
                  @doc "bar function" bar(x) = x + 2
                  include_dependency("bar.jl")
              end

              # test for creation of some reasonably complicated type
              struct MyType{T} end
              const t17809s = Any[
                    Tuple{
                        Type{Ptr{MyType{i}}},
                        Array{Ptr{MyType{MyType{:sym}()}}(0), 0},
                        Val{Complex{Int}(1, 2)},
                        Val{3},
                        Val{nothing}}
                    for i = 0:25]

              # test that types and methods get reconnected correctly
              # issue 16529 (adding a method to a type with no instances)
              (::Task)(::UInt8, ::UInt16, ::UInt32) = 2

              # issue 16471 (capturing references to a kwfunc)
              Test.@test_throws ErrorException Core.kwfunc(Base.nothing)
              Base.nothing(::UInt8, ::UInt16, ::UInt32; x = 52) = x
              const nothingkw = Core.kwfunc(Base.nothing)

              # issue 16908 (some complicated types and external method definitions)
              abstract type CategoricalPool{T, R <: Integer, V} end
              abstract type CategoricalValue{T, R <: Integer} end
              struct NominalPool{T, R <: Integer, V} <: CategoricalPool{T, R, V}
                  index::Vector{T}
                  invindex::Dict{T, R}
                  order::Vector{R}
                  ordered::Vector{T}
                  valindex::Vector{V}
              end
              struct NominalValue{T, R <: Integer} <: CategoricalValue{T, R}
                  level::R
                  pool::NominalPool{T, R, NominalValue{T, R}}
              end
              struct OrdinalValue{T, R <: Integer} <: CategoricalValue{T, R}
                  level::R
                  pool::NominalPool{T, R, NominalValue{T, R}}
              end
              (::Union{Type{NominalValue}, Type{OrdinalValue}})() = 1
              (::Union{Type{NominalValue{T}}, Type{OrdinalValue{T}}})() where {T} = 2
              (::Type{Vector{NominalValue{T, R}}})() where {T, R} = 3
              (::Type{Vector{NominalValue{T, T}}})() where {T} = 4
              (::Type{Vector{NominalValue{Int, Int}}})() = 5

              # more tests for method signature involving a complicated type
              # issue 18343
              struct Pool18343{R, V}
                  valindex::Vector{V}
              end
              struct Value18343{T, R}
                  pool::Pool18343{R, Value18343{T, R}}
              end
              Base.convert(::Type{Some{S}}, ::Value18343{Some}) where {S} = 2
              Base.convert(::Type{Some{Value18343}}, ::Value18343{Some}) = 2
              Base.convert(::Type{Ref}, ::Value18343{T}) where {T} = 3


              let some_method = @which Base.include("string")
                    # global const some_method // FIXME: support for serializing a direct reference to an external Method not implemented
                  global const some_linfo =
                      ccall(:jl_specializations_get_linfo, Ref{Core.MethodInstance}, (Any, Any, Any, UInt),
                          some_method, Tuple{typeof(Base.include), String}, Core.svec(), typemax(UInt))
              end

              g() = override(1.0)
              Test.@test g() === 2.0 # compile this
          end
          """)
    @test_throws ErrorException Core.kwfunc(Base.nothing) # make sure `nothing` didn't have a kwfunc (which would invalidate the attempted test)

    # Issue #12623
    @test __precompile__(true) === nothing

    # Issue #21307
    Base.require(Foo2_module)
    @eval let Foo2_module = $(QuoteNode(Foo2_module)), # use @eval to see the results of loading the compile
              Foo = root_module(Foo2_module)
        Foo.override(::Int) = 'a'
        Foo.override(::Float32) = 'b'
    end

    Base.require(Foo_module)

    @eval let Foo_module = $(QuoteNode(Foo_module)), # use @eval to see the results of loading the compile
              Foo = root_module(Foo_module)
        @test Foo.foo(17) == 18
        @test Foo.Bar.bar(17) == 19

        # Issue #21307
        @test Foo.g() === 97.0
        @test Foo.override(1.0e0) == Float64('a')
        @test Foo.override(1.0f0) == 'b'
        @test Foo.override(UInt(1)) == 2
    end

    cachefile = joinpath(dir, "$Foo_module.ji")
    # use _require_from_serialized to ensure that the test fails if
    # the module doesn't reload from the image:
    @test_logs (:warn, "Replacing module `$Foo_module`") begin
        ms = Base._require_from_serialized(cachefile)
        @test isa(ms, Array{Any,1})
    end

    let Foo = root_module(Foo_module)
        @test_throws MethodError Foo.foo(17) # world shouldn't be visible yet
    end
    @eval let Foo_module = $(QuoteNode(Foo_module)), # use @eval to see the results of loading the compile
              Foo2_module = $(QuoteNode(Foo2_module)),
              FooBase_module = $(QuoteNode(FooBase_module)),
              Foo = root_module(Foo_module),
              dir = $(QuoteNode(dir)),
              cachefile = $(QuoteNode(cachefile)),
              Foo_file = $(QuoteNode(Foo_file))
        @test Foo.foo(17) == 18
        @test Foo.Bar.bar(17) == 19

        # Issue #21307
        @test Foo.g() === 97.0
        @test Foo.override(1.0e0) == Float64('a')
        @test Foo.override(1.0f0) == 'b'
        @test Foo.override(UInt(1)) == 2

        # issue #12284:
        @test string(Base.Docs.doc(Foo.foo)) == "foo function\n"
        @test string(Base.Docs.doc(Foo.Bar.bar)) == "bar function\n"

        modules, deps, required_modules = Base.parse_cache_header(cachefile)
        discard_module = mod_fl_mt -> (mod_fl_mt[2], mod_fl_mt[3])
        @test modules == [Foo_module => Base.module_uuid(Foo)]
        @test map(x -> x[1],  sort(discard_module.(deps))) == [Foo_file, joinpath(dir, "bar.jl"), joinpath(dir, "foo.jl")]
        srctxt = Base.read_dependency_src(cachefile, Foo_file)
        @test !isempty(srctxt) && srctxt == read(Foo_file, String)
        @test_throws ErrorException Base.read_dependency_src(cachefile, "/tmp/nonexistent.txt")
        # dependencies declared with `include_dependency` should not be stored
        @test_throws ErrorException Base.read_dependency_src(cachefile, joinpath(dir, "foo.jl"))

        modules, deps1 = Base.cache_dependencies(cachefile)
        @test Dict(modules) == merge(
            Dict(s => Base.module_uuid(getfield(Foo, s)) for s in
                [:Base, :Core, Foo2_module, FooBase_module, :Main]),
            # plus modules included in the system image
            Dict(s => Base.module_uuid(Base.root_module(s)) for s in
                [:Base64, :CRC32c, :Dates, :DelimitedFiles, :FileWatching, :Future,
                 :IterativeEigensolvers, :Logging, :Mmap, :Printf, :Profile, :SharedArrays,
                 :SuiteSparse, :Test, :Strings, :Distributed]))
        @test discard_module.(deps) == deps1

        @test current_task()(0x01, 0x4000, 0x30031234) == 2
        @test nothing(0x01, 0x4000, 0x30031234) == 52
        @test nothing(0x01, 0x4000, 0x30031234; x = 9142) == 9142
        @test Foo.nothingkw === Core.kwfunc(Base.nothing)

        @test Foo.NominalValue() == 1
        @test Foo.OrdinalValue() == 1
        @test Foo.NominalValue{Int}() == 2
        @test Foo.OrdinalValue{Int}() == 2
        let T = Vector{Foo.NominalValue{Int}}
            @test isa(T(), T)
        end
        @test Vector{Foo.NominalValue{Int32, Int64}}() == 3
        @test Vector{Foo.NominalValue{UInt, UInt}}() == 4
        @test Vector{Foo.NominalValue{Int, Int}}() == 5
        @test all(i -> Foo.t17809s[i + 1] ===
            Tuple{
                Type{Ptr{Foo.MyType{i}}},
                Array{Ptr{Foo.MyType{Foo.MyType{:sym}()}}(0), 0},
                Val{Complex{Int}(1, 2)},
                Val{3},
                Val{nothing}},
            0:25)
        some_method = @which Base.include("string")
        some_linfo =
                ccall(:jl_specializations_get_linfo, Ref{Core.MethodInstance}, (Any, Any, Any, UInt),
                    some_method, Tuple{typeof(Base.include), String}, Core.svec(), typemax(UInt))
        @test Foo.some_linfo::Core.MethodInstance === some_linfo

        PV = Foo.Value18343{Some}.body.types[1]
        VR = PV.types[1].parameters[1]
        @test PV.types[1] === Array{VR,1}
        @test pointer_from_objref(PV.types[1]) ===
              pointer_from_objref(PV.types[1].parameters[1].types[1].types[1])
        @test PV === PV.types[1].parameters[1].types[1]
        @test pointer_from_objref(PV) === pointer_from_objref(PV.types[1].parameters[1].types[1])
    end

    Baz_file = joinpath(dir, "Baz.jl")
    write(Baz_file,
          """
          __precompile__(false)
          module Baz
          end
          """)

    @test_warn "ERROR: LoadError: Declaring __precompile__(false) is not allowed in files that are being precompiled.\nStacktrace:\n [1] __precompile__" try
        Base.compilecache("Baz") # from __precompile__(false)
        error("__precompile__ disabled test failed")
    catch exc
        isa(exc, ErrorException) || rethrow(exc)
        contains(exc.msg, "__precompile__(false)") && rethrow(exc)
    end

    # Issue #12720
    FooBar1_file = joinpath(dir, "FooBar1.jl")
    write(FooBar1_file,
          """
          __precompile__(true)
          module FooBar1
              using FooBar
          end
          """)
    sleep(2) # give FooBar and FooBar1 different timestamps, in reverse order too
    FooBar_file = joinpath(dir, "FooBar.jl")
    write(FooBar_file,
          """
          __precompile__(true)
          module FooBar
          end
          """)

    Base.compilecache("FooBar")
    @test isfile(joinpath(dir, "FooBar.ji"))
    @test Base.stale_cachefile(FooBar_file, joinpath(dir, "FooBar.ji")) isa Vector
    @test !isdefined(Main, :FooBar)
    @test !isdefined(Main, :FooBar1)

    relFooBar_file = joinpath(dir, "subfolder", "..", "FooBar.jl")
    @test Base.stale_cachefile(relFooBar_file, joinpath(dir, "FooBar.ji")) isa (Sys.iswindows() ? Vector : Bool) # `..` is not a symlink on Windows
    mkdir(joinpath(dir, "subfolder"))
    @test Base.stale_cachefile(relFooBar_file, joinpath(dir, "FooBar.ji")) isa Vector

    @eval using FooBar
    fb_uuid = Base.module_uuid(FooBar)
    sleep(2); touch(FooBar_file)
    insert!(Base.LOAD_CACHE_PATH, 1, dir2)
    @test Base.stale_cachefile(FooBar_file, joinpath(dir, "FooBar.ji")) === true
    @eval using FooBar1
    @test !isfile(joinpath(dir2, "FooBar.ji"))
    @test !isfile(joinpath(dir, "FooBar1.ji"))
    @test isfile(joinpath(dir2, "FooBar1.ji"))
    @test Base.stale_cachefile(FooBar_file, joinpath(dir, "FooBar.ji")) === true
    @test Base.stale_cachefile(FooBar1_file, joinpath(dir2, "FooBar1.ji")) isa Vector
    @test fb_uuid == Base.module_uuid(FooBar)
    fb_uuid1 = Base.module_uuid(FooBar1)
    @test fb_uuid != fb_uuid1

    # test checksum
    open(joinpath(dir2, "FooBar1.ji"), "a") do f
        write(f, 0x076cac96) # append 4 random bytes
    end
    @test Base.stale_cachefile(FooBar1_file, joinpath(dir2, "FooBar1.ji")) === true

    # test behavior of precompile modules that throw errors
    FooBar2_file = joinpath(dir, "FooBar2.jl")
    write(FooBar2_file,
          """
          __precompile__(true)
          module FooBar2
          error("break me")
          end
          """)
    @test_warn "ERROR: LoadError: break me\nStacktrace:\n [1] error" try
        Base.require(:FooBar2)
        error("\"LoadError: break me\" test failed")
    catch exc
        isa(exc, ErrorException) || rethrow(exc)
        contains(exc.msg, "ERROR: LoadError: break me") && rethrow(exc)
    end

    # Test transitive dependency for #21266
    FooBarT_file = joinpath(dir, "FooBarT.jl")
    write(FooBarT_file,
          """
          __precompile__(true)
          module FooBarT
          end
          """)
    FooBarT1_file = joinpath(dir, "FooBarT1.jl")
    write(FooBarT1_file,
          """
          __precompile__(true)
          module FooBarT1
              using FooBarT
          end
          """)
    FooBarT2_file = joinpath(dir, "FooBarT2.jl")
    write(FooBarT2_file,
          """
          __precompile__(true)
          module FooBarT2
              using FooBarT1
          end
          """)
    Base.compilecache("FooBarT2")
    write(FooBarT1_file,
          """
          __precompile__(true)
          module FooBarT1
          end
          """)
    rm(FooBarT_file)
    @test Base.stale_cachefile(FooBarT2_file, joinpath(dir2, "FooBarT2.ji")) === true
    @test Base.require(:FooBarT2) isa Module
finally
    splice!(Base.LOAD_CACHE_PATH, 1:2)
    splice!(LOAD_PATH, 1)
    rm(dir, recursive=true)
    rm(dir2, recursive=true)
end

# test --compiled-modules=no command line option
let dir = mktempdir(),
    Time_module = :Time4b3a94a1a081a8cb

    try
        write(joinpath(dir, "$Time_module.jl"),
              """
              module $Time_module
                  __precompile__(true)
                  time = Base.time()
              end
              """)

        eval(quote
            insert!(LOAD_PATH, 1, $(dir))
            insert!(Base.LOAD_CACHE_PATH, 1, $(dir))
            Base.compilecache(:Time4b3a94a1a081a8cb)
        end)

        exename = `$(Base.julia_cmd()) --compiled-modules=yes --startup-file=no`

        testcode = """
            insert!(LOAD_PATH, 1, $(repr(dir)))
            insert!(Base.LOAD_CACHE_PATH, 1, $(repr(dir)))
            using $Time_module
            getfield($Time_module, :time)
        """

        t1_yes = readchomp(`$exename --compiled-modules=yes -E $(testcode)`)
        t2_yes = readchomp(`$exename --compiled-modules=yes -E $(testcode)`)
        @test t1_yes == t2_yes

        t1_no = readchomp(`$exename --compiled-modules=no -E $(testcode)`)
        t2_no = readchomp(`$exename --compiled-modules=no -E $(testcode)`)
        @test t1_no != t2_no
        @test parse(Float64, t1_no) < parse(Float64, t2_no)

    finally
        splice!(Base.LOAD_CACHE_PATH, 1)
        splice!(LOAD_PATH, 1)
        rm(dir, recursive=true)
    end
end

# test loading a package with conflicting namespace
let dir = mktempdir()
    Test_module = :Test6c92f26
    try
        write(joinpath(dir, "Iterators.jl"),
              """
              module Iterators
                   __precompile__(true)
              end
              """)

        write(joinpath(dir, "$Test_module.jl"),
              """
              module $Test_module
                   __precompile__(true)
                   using Iterators
              end
              """)

        testcode = """
            insert!(LOAD_PATH, 1, $(repr(dir)))
            insert!(Base.LOAD_CACHE_PATH, 1, $(repr(dir)))
            using $Test_module
        """

        exename = `$(Base.julia_cmd()) --startup-file=no`
        let fname = tempname()
            try
                @test readchomp(pipeline(`$exename -E $(testcode)`, stderr=fname)) == "nothing"
                @test contains(read(fname, String), Regex("Replacing module `$Test_module`"))
            finally
                rm(fname, force=true)
            end
        end
        # Loading $Test_module from the cache should not bring `Base.Iterators`
        # into `Main`, since that would lead to a namespace conflict with
        # the module `Iterators` defined above.
        let fname = tempname()
            try
                @test readchomp(pipeline(`$exename -E $(testcode)`, stderr=fname)) == "nothing"
                # e.g `@test_nowarn`
                @test Test.contains_warn(read(fname, String), r"^(?!.)"s)
            finally
                rm(fname, force=true)
            end
        end
    finally
        rm(dir, recursive=true)
    end
end

let dir = mktempdir()
    try
        insert!(LOAD_PATH, 1, dir)
        insert!(Base.LOAD_CACHE_PATH, 1, dir)

        loaded_modules = Channel{Symbol}(32)
        callback = (mod::Symbol) -> put!(loaded_modules, mod)
        push!(Base.package_callbacks, callback)

        Test1_module = :Teste4095a81
        Test2_module = :Teste4095a82
        Test3_module = :Teste4095a83

        write(joinpath(dir, "$(Test1_module).jl"),
              """
              module $(Test1_module)
                  __precompile__(true)
              end
              """)

        Base.compilecache("$(Test1_module)")
        write(joinpath(dir, "$(Test2_module).jl"),
              """
              module $(Test2_module)
                  __precompile__(true)
                  using $(Test1_module)
              end
              """)
        Base.compilecache("$(Test2_module)")
        @test !Base.isbindingresolved(Main, Test2_module)
        Base.require(Test2_module)
        @test take!(loaded_modules) == Test1_module
        @test take!(loaded_modules) == Test2_module
        write(joinpath(dir, "$(Test3_module).jl"),
              """
              module $(Test3_module)
                  using $(Test3_module)
              end
              """)
        Base.require(Test3_module)
        @test take!(loaded_modules) == Test3_module
    finally
        pop!(Base.package_callbacks)
        splice!(Base.LOAD_CACHE_PATH, 1)
        splice!(LOAD_PATH, 1)
        rm(dir, recursive=true)
    end
end

# Issue #19960
let
    test_workers = addprocs(1)
    push!(test_workers, myid())
    save_cwd = pwd()
    temp_path = mktempdir()
    try
        cd(temp_path)
        load_path = mktempdir(temp_path)
        load_cache_path = mktempdir(temp_path)

        ModuleA = :Issue19960A
        ModuleB = :Issue19960B

        write(joinpath(load_path, "$ModuleA.jl"),
            """
            __precompile__(true)
            module $ModuleA
                import Distributed: myid
                export f
                f() = myid()
            end
            """)

        write(joinpath(load_path, "$ModuleB.jl"),
            """
            __precompile__(true)
            module $ModuleB
                using $ModuleA
                export g
                g() = f()
            end
            """)

        @everywhere test_workers begin
            pushfirst!(LOAD_PATH, $load_path)
            pushfirst!(Base.LOAD_CACHE_PATH, $load_cache_path)
        end
        try
            @eval using $ModuleB
            uuid = Base.module_uuid(root_module(ModuleB))
            for wid in test_workers
                @test Distributed.remotecall_eval(Main, wid, :( Base.module_uuid(Base.root_module($(QuoteNode(ModuleB)))) )) == uuid
                if wid != myid() # avoid world-age errors on the local proc
                    @test remotecall_fetch(g, wid) == wid
                end
            end
        finally
            @everywhere test_workers begin
                popfirst!(LOAD_PATH)
                popfirst!(Base.LOAD_CACHE_PATH)
            end
        end
    finally
        cd(save_cwd)
        rm(temp_path, recursive=true)
        pop!(test_workers) # remove myid
        rmprocs(test_workers)
    end
end

# Ensure that module-loading plays nicely with Base.delete_method
dir = mktempdir()
insert!(LOAD_PATH, 1, dir)
insert!(Base.LOAD_CACHE_PATH, 1, dir)
try
    A_module = :Aedb164bd3a126418
    B_module = :Bedb164bd3a126418
    A_file = joinpath(dir, "$A_module.jl")
    B_file = joinpath(dir, "$B_module.jl")

    write(A_file,
          """
          __precompile__()

          module $A_module

          export apc, anopc

          apc(::Int, ::Int) = 1
          apc(::Any, ::Any) = 2

          anopc(::Int, ::Int) = 1
          anopc(::Any, ::Any) = 2

          end
          """)
    write(B_file,
          """
          __precompile__()

          module $B_module

          using $A_module

          bpc(x) = apc(x, x)
          bnopc(x) = anopc(x, x)

          precompile(bpc, (Int,))
          precompile(bpc, (Float64,))

          end
          """)
    Base.require(A_module)
    A = root_module(A_module)
    for mths in (collect(methods(A.apc)), collect(methods(A.anopc)))
        Base.delete_method(mths[1])
    end
    Base.require(B_module)
    B = root_module(B_module)
    @test Base.invokelatest(B.bpc, 1) == Base.invokelatest(B.bpc, 1.0) == 2
    @test Base.invokelatest(B.bnopc, 1) == Base.invokelatest(B.bnopc, 1.0) == 2
finally
    popfirst!(LOAD_PATH)
    popfirst!(Base.LOAD_CACHE_PATH)
    rm(dir, recursive=true)
end

end # !withenv
