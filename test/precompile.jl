# This file is a part of Julia. License is MIT: https://julialang.org/license

original_depot_path = copy(Base.DEPOT_PATH)
original_load_path = copy(Base.LOAD_PATH)

using Test, Distributed, Random

Foo_module = :Foo4b3a94a1a081a8cb
Foo2_module = :F2oo4b3a94a1a081a8cb
FooBase_module = :FooBase4b3a94a1a081a8cb
@eval module ConflictingBindings
    export $Foo_module, $FooBase_module
    $Foo_module = 232
    $FooBase_module = 9134
end
using .ConflictingBindings

function precompile_test_harness(@nospecialize(f), testset::String)
    @testset "$testset" begin
        precompile_test_harness(f, true)
    end
end
function precompile_test_harness(@nospecialize(f), separate::Bool)
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
    nothing
end

# method root provenance

rootid(m::Module) = Base.module_build_id(Base.parentmodule(m)) % UInt64
rootid(m::Method) = rootid(m.module)

function root_provenance(m::Method, i::Int)
    mid = rootid(m)
    isdefined(m, :root_blocks) || return mid
    idxs = view(m.root_blocks, 2:2:length(m.root_blocks))
    j = searchsortedfirst(idxs, i) - 1   # RLE roots are 0-indexed
    j == 0 && return mid
    return m.root_blocks[2*j-1]
end

struct RLEIterator{T}   # for method roots, T = UInt64 (even on 32-bit)
    items::Vector{Any}
    blocks::Vector{T}
    defaultid::T
end
function RLEIterator(roots, blocks, defaultid)
    T = promote_type(eltype(blocks), typeof(defaultid))
    return RLEIterator{T}(convert(Vector{Any}, roots), blocks, defaultid)
end
RLEIterator(m::Method) = RLEIterator(m.roots, m.root_blocks, rootid(m))
Base.iterate(iter::RLEIterator) = iterate(iter, (0, 0, iter.defaultid))
function Base.iterate(iter::RLEIterator, (i, j, cid))
    i += 1
    i > length(iter.items) && return nothing
    r = iter.items[i]
    while (j + 1 < length(iter.blocks) && i > iter.blocks[j+2])
        cid = iter.blocks[j+1]
        j += 2
    end
    return cid => r, (i, j, cid)
end

function group_roots(m::Method)
    mid = rootid(m)
    isdefined(m, :root_blocks) || return Dict(mid => m.roots)
    group_roots(RLEIterator(m.roots, m.root_blocks, mid))
end
function group_roots(iter::RLEIterator)
    rootsby = Dict{typeof(iter.defaultid),Vector{Any}}()
    for (id, r) in iter
        list = get!(valtype(rootsby), rootsby, id)
        push!(list, r)
    end
    return rootsby
end

precompile_test_harness("basic precompile functionality") do dir2
precompile_test_harness(false) do dir
    Foo_file = joinpath(dir, "$Foo_module.jl")
    Foo2_file = joinpath(dir, "$Foo2_module.jl")
    FooBase_file = joinpath(dir, "$FooBase_module.jl")

    write(FooBase_file,
          """
          false && __precompile__(false)
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
          module $Foo2_module
              export override, overridenc
              override(x::Integer) = 2
              override(x::AbstractFloat) = Float64(override(1))
              overridenc(x::Integer) = rand()+1
              overridenc(x::AbstractFloat) = Float64(overridenc(1))
          end
          """)
    write(Foo_file,
          """
          module $Foo_module
              import $FooBase_module, $FooBase_module.typeA
              import $Foo2_module: $Foo2_module, override, overridenc
              import $FooBase_module.hash
              import Test
              module Inner
                  import $FooBase_module.hash
                  using ..$Foo_module
                  import ..$Foo2_module
              end

              struct typeB
                  y::typeA
              end
              hash(x::typeB) = hash(x.y)

              # test that docs get reconnected
              @doc "foo function" foo(x) = x + 1
              include_dependency("foo.jl")
              include_dependency("foo.jl")
              module Bar
                  include_dependency("bar.jl")
              end
              @doc "Bar module" Bar # this needs to define the META dictionary via eval
              @eval Bar @doc "bar function" bar(x) = x + 2

              # test for creation of some reasonably complicated type
              struct MyType{T} end
              const t17809s = Any[
                    Tuple{
                        Type{Ptr{MyType{i}}},
                        Ptr{Type{MyType{i}}},
                        Array{Ptr{MyType{MyType{:sym}()}}(0), 0},
                        Val{Complex{Int}(1, 2)},
                        Val{3},
                        Val{nothing}}
                    for i = 0:25]

              # test that types and methods get reconnected correctly
              # issue 16529 (adding a method to a type with no instances)
              (::Task)(::UInt8, ::UInt16, ::UInt32) = 2

              # issue 16471
              Base.sin(::UInt8, ::UInt16, ::UInt32; x = 52) = x
              const sinkw = Core.kwcall

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


              # issue #28297
              mutable struct Result
                  result::Union{Int,Missing}
              end

              const x28297 = Result(missing)

              const d29936a = UnionAll(Dict.var, UnionAll(Dict.body.var, Dict.body.body))
              const d29936b = UnionAll(Dict.body.var, UnionAll(Dict.var, Dict.body.body))

              # issue #28998
              const x28998 = [missing, 2, missing, 6, missing,
                              missing, missing, missing,
                              missing, missing, missing,
                              missing, missing, 6]

              let some_method = which(Base.include, (Module, String,))
                    # global const some_method // FIXME: support for serializing a direct reference to an external Method not implemented
                  global const some_linfo = Core.Compiler.specialize_method(some_method,
                      Tuple{typeof(Base.include), Module, String}, Core.svec())
              end

              g() = override(1.0)
              Test.@test g() === 2.0 # compile this
              gnc() = overridenc(1.0)
              Test.@test 1 < gnc() < 5 # compile this

              const abigfloat_f() = big"12.34"
              const abigfloat_x = big"43.21"
              const abigint_f() = big"123"
              const abigint_x = big"124"

              # issue #31488
              _v31488 = Base.StringVector(2)
              resize!(_v31488, 0)
              const a31488 = fill(String(_v31488), 100)

              const ptr1 = Ptr{UInt8}(1)
              ptr2 = Ptr{UInt8}(1)
              const ptr3 = Ptr{UInt8}(-1)
              const layout1 = Ptr{Int8}[Ptr{Int8}(0), Ptr{Int8}(1), Ptr{Int8}(-1)]
              const layout2 = Any[Ptr{Int8}(0), Ptr{Int16}(1), Ptr{Int32}(-1)]
              const layout3 = collect(x.match for x in eachmatch(r"..", "abcdefghijk"))::Vector{SubString{String}}

              # create a backedge that includes Type{Union{}}, to ensure lookup can handle that
              call_bottom() = show(stdout, Union{})
              Core.Compiler.return_type(call_bottom, Tuple{})

              # check that @ccallable works from precompiled modules
              Base.@ccallable Cint f35014(x::Cint) = x+Cint(1)

              # check that Tasks work from serialized state
              ch1 = Channel(x -> nothing)
              ch2 = Channel(x -> (push!(x, 2); nothing), Inf)
          end
          """)
    # Issue #12623
    @test __precompile__(false) === nothing

    # Issue #21307
    Foo2 = Base.require(Main, Foo2_module)
    @eval $Foo2.override(::Int) = 'a'
    @eval $Foo2.override(::Float32) = 'b'
    @eval $Foo2.overridenc(::Int) = rand() + 97.0
    @eval $Foo2.overridenc(::Float32) = rand() + 100.0

    Foo = Base.require(Main, Foo_module)
    Base.invokelatest() do # use invokelatest to see the results of loading the compile
        @test Foo.foo(17) == 18
        @test Foo.Bar.bar(17) == 19

        # Issue #21307
        @test Foo.g() === 97.0
        @test 96 < Foo.gnc() < 99
        @test Foo.override(1.0e0) == Float64('a')
        @test Foo.override(1.0f0) == 'b'
        @test Foo.override(UInt(1)) == 2
        @test 96 < Foo.overridenc(1.0e0) < 99
        @test 99 < Foo.overridenc(1.0f0) < 102
        @test 0 < Foo.overridenc(UInt(1)) < 3

        # Issue #15722
        @test Foo.abigfloat_f()::BigFloat == big"12.34"
        @test (Foo.abigfloat_x::BigFloat + 21) == big"64.21"
        @test Foo.abigint_f()::BigInt == big"123"
        @test Foo.abigint_x::BigInt + 1 == big"125"

        @test Foo.x28297.result === missing

        @test Foo.d29936a === Dict
        @test Foo.d29936b === Dict{K,V} where {V,K}

        @test Foo.x28998[end] == 6

        @test Foo.a31488 == fill("", 100)

        @test Foo.ptr1 === Ptr{UInt8}(1)
        @test Foo.ptr2 === Ptr{UInt8}(0)
        @test Foo.ptr3 === Ptr{UInt8}(-1)
        @test Foo.layout1::Vector{Ptr{Int8}} == Ptr{Int8}[Ptr{Int8}(0), Ptr{Int8}(0), Ptr{Int8}(-1)]
        @test Foo.layout2 == Any[Ptr{Int8}(0), Ptr{Int16}(0), Ptr{Int32}(-1)]
        @test typeof.(Foo.layout2) == [Ptr{Int8}, Ptr{Int16}, Ptr{Int32}]
        @test Foo.layout3 == ["ab", "cd", "ef", "gh", "ij"]

        @test !isopen(Foo.ch1)
        @test !isopen(Foo.ch2)
        @test !isready(Foo.ch1)
        @test isready(Foo.ch2)
        @test take!(Foo.ch2) === 2
        @test !isready(Foo.ch2)
    end

    @eval begin function ccallable_test()
        Base.llvmcall(
        ("""declare i32 @f35014(i32)
            define i32 @entry() {
            0:
                %1 = call i32 @f35014(i32 3)
                ret i32 %1
            }""", "entry"
        ), Cint, Tuple{})
    end
    @test ccallable_test() == 4
    end

    cachedir = joinpath(dir, "compiled", "v$(VERSION.major).$(VERSION.minor)")
    cachedir2 = joinpath(dir2, "compiled", "v$(VERSION.major).$(VERSION.minor)")
    cachefile = joinpath(cachedir, "$Foo_module.ji")
    if Base.JLOptions().use_pkgimages == 1
        ocachefile = Base.ocachefile_from_cachefile(cachefile)
    else
        ocachefile = nothing
    end
    # use _require_from_serialized to ensure that the test fails if
    # the module doesn't reload from the image:
    @test_warn "@ccallable was already defined for this method name" begin
        @test_logs (:warn, "Replacing module `$Foo_module`") begin
            m = Base._require_from_serialized(Base.PkgId(Foo), cachefile, ocachefile)
            @test isa(m, Module)
        end
    end

    @test_throws MethodError Foo.foo(17) # world shouldn't be visible yet
    Base.invokelatest() do # use invokelatest to see the results of loading the compile
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
        @test string(Base.Docs.doc(Foo.Bar)) == "Bar module\n"

        modules, (deps, requires), required_modules, _... = Base.parse_cache_header(cachefile)
        discard_module = mod_fl_mt -> (mod_fl_mt.filename, mod_fl_mt.mtime)
        @test modules == [ Base.PkgId(Foo) => Base.module_build_id(Foo) % UInt64 ]
        @test map(x -> x.filename, deps) == [ Foo_file, joinpath(dir, "foo.jl"), joinpath(dir, "bar.jl") ]
        @test requires == [ Base.PkgId(Foo) => Base.PkgId(string(FooBase_module)),
                            Base.PkgId(Foo) => Base.PkgId(Foo2),
                            Base.PkgId(Foo) => Base.PkgId(Test),
                            Base.PkgId(Foo) => Base.PkgId(string(FooBase_module)) ]
        srctxt = Base.read_dependency_src(cachefile, Foo_file)
        @test !isempty(srctxt) && srctxt == read(Foo_file, String)
        @test_throws ErrorException Base.read_dependency_src(cachefile, "/tmp/nonexistent.txt")
        # dependencies declared with `include_dependency` should not be stored
        @test_throws ErrorException Base.read_dependency_src(cachefile, joinpath(dir, "foo.jl"))

        modules, deps1 = Base.cache_dependencies(cachefile)
        @test Dict(modules) == merge(
            Dict(let m = Base.PkgId(s)
                    m => Base.module_build_id(Base.root_module(m))
                 end for s in
                 [ "Base", "Core", "Main",
                   string(Foo2_module), string(FooBase_module) ]),
            # plus modules included in the system image
            Dict(let m = Base.root_module(Base, s)
                     Base.PkgId(m) => Base.module_build_id(m)
                 end for s in
                [:ArgTools, :Artifacts, :Base64, :CompilerSupportLibraries_jll, :CRC32c, :Dates,
                 :Distributed, :Downloads, :FileWatching, :Future, :InteractiveUtils, :libblastrampoline_jll,
                 :LazyArtifacts, :LibCURL, :LibCURL_jll, :LibGit2, :Libdl, :LinearAlgebra,
                 :Logging, :Markdown, :Mmap, :MozillaCACerts_jll, :NetworkOptions, :OpenBLAS_jll, :Pkg, :Printf,
                 :Profile, :p7zip_jll, :REPL, :Random, :SHA, :Serialization, :SharedArrays, :Sockets,
                 :TOML, :Tar, :Test, :UUIDs, :Unicode,
                 :nghttp2_jll]
            ),
        )
        @test discard_module.(deps) == deps1
        modules, (deps, requires), required_modules, _... = Base.parse_cache_header(cachefile; srcfiles_only=true)
        @test map(x -> x.filename, deps) == [Foo_file]

        @test current_task()(0x01, 0x4000, 0x30031234) == 2
        @test sin(0x01, 0x4000, 0x30031234) == 52
        @test sin(0x01, 0x4000, 0x30031234; x = 9142) == 9142
        @test Foo.sinkw === Core.kwcall

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
                Ptr{Type{Foo.MyType{i}}},
                Array{Ptr{Foo.MyType{Foo.MyType{:sym}()}}(0), 0},
                Val{Complex{Int}(1, 2)},
                Val{3},
                Val{nothing}},
            0:25)
        some_method = which(Base.include, (Module, String,))
        some_linfo = Core.Compiler.specialize_method(some_method, Tuple{typeof(Base.include), Module, String}, Core.svec())
        @test Foo.some_linfo::Core.MethodInstance === some_linfo

        ft = Base.datatype_fieldtypes
        PV = ft(Foo.Value18343{Some}.body)[1]
        VR = ft(PV)[1].parameters[1]
        @test ft(PV)[1] === Array{VR,1}
        @test pointer_from_objref(ft(PV)[1]) ===
              pointer_from_objref(ft(ft(ft(PV)[1].parameters[1])[1])[1])
        @test PV === ft(ft(PV)[1].parameters[1])[1]
        @test pointer_from_objref(PV) === pointer_from_objref(ft(ft(PV)[1].parameters[1])[1])
    end

    Nest_module = :Nest4b3a94a1a081a8cb
    Nest_file = joinpath(dir, "$Nest_module.jl")
    NestInner_file = joinpath(dir, "$(Nest_module)Inner.jl")
    NestInner2_file = joinpath(dir, "$(Nest_module)Inner2.jl")
    write(Nest_file,
        """
        module $Nest_module
        include("$(escape_string(NestInner_file))")
        end
        """)
    write(NestInner_file,
        """
        module NestInner
        include("$(escape_string(NestInner2_file))")
        end
        """)
    write(NestInner2_file,
        """
        f() = 22
        """)
    Nest = Base.require(Main, Nest_module)
    cachefile = joinpath(cachedir, "$Nest_module.ji")
    modules, (deps, requires), required_modules, _... = Base.parse_cache_header(cachefile)
    @test last(deps).modpath == ["NestInner"]

    UsesB_module = :UsesB4b3a94a1a081a8cb
    B_module     = :UsesB4b3a94a1a081a8cb_B
    UsesB_file = joinpath(dir, "$UsesB_module.jl")
    B_file = joinpath(dir, "$(B_module).jl")
    write(UsesB_file,
        """
        module $UsesB_module
        using $B_module
        end
        """)
    write(B_file,
        """
        module $B_module
        export bfunc
        bfunc() = 33
        end
        """)
    UsesB = Base.require(Main, UsesB_module)
    cachefile = joinpath(cachedir, "$UsesB_module.ji")
    modules, (deps, requires), required_modules, _... = Base.parse_cache_header(cachefile)
    id1, id2 = only(requires)
    @test Base.pkgorigins[id1].cachepath == cachefile
    @test Base.pkgorigins[id2].cachepath == joinpath(cachedir, "$B_module.ji")

    Baz_file = joinpath(dir, "Baz.jl")
    write(Baz_file,
          """
          true && __precompile__(false)
          module Baz
          baz() = 1
          end
          """)

    @test Base.compilecache(Base.PkgId("Baz")) == Base.PrecompilableError() # due to __precompile__(false)
    @eval using Baz
    @test Base.invokelatest(Baz.baz) == 1

    # Issue #12720
    FooBar1_file = joinpath(dir, "FooBar1.jl")
    write(FooBar1_file,
          """
          module FooBar1
              using FooBar
          end
          """)
    sleep(2) # give FooBar and FooBar1 different timestamps, in reverse order too
    FooBar_file = joinpath(dir, "FooBar.jl")
    write(FooBar_file,
          """
          module FooBar
          end
          """)

    cachefile, _ = Base.compilecache(Base.PkgId("FooBar"))
    empty_prefs_hash = Base.get_preferences_hash(nothing, String[])
    @test cachefile == Base.compilecache_path(Base.PkgId("FooBar"), empty_prefs_hash)
    @test isfile(joinpath(cachedir, "FooBar.ji"))
    Tsc = Bool(Base.JLOptions().use_pkgimages) ? Tuple{<:Vector, String} : Tuple{<:Vector, Nothing}
    @test Base.stale_cachefile(FooBar_file, joinpath(cachedir, "FooBar.ji")) isa Tsc
    @test !isdefined(Main, :FooBar)
    @test !isdefined(Main, :FooBar1)

    relFooBar_file = joinpath(dir, "subfolder", "..", "FooBar.jl")
    @test Base.stale_cachefile(relFooBar_file, joinpath(cachedir, "FooBar.ji")) isa (Sys.iswindows() ? Tuple{<:Vector, String} : Bool) # `..` is not a symlink on Windows
    mkdir(joinpath(dir, "subfolder"))
    @test Base.stale_cachefile(relFooBar_file, joinpath(cachedir, "FooBar.ji")) isa Tsc

    @eval using FooBar
    fb_uuid = Base.module_build_id(FooBar)
    sleep(2); touch(FooBar_file)
    insert!(DEPOT_PATH, 1, dir2)
    @test Base.stale_cachefile(FooBar_file, joinpath(cachedir, "FooBar.ji")) === true
    @eval using FooBar1
    @test !isfile(joinpath(cachedir2, "FooBar.ji"))
    @test !isfile(joinpath(cachedir, "FooBar1.ji"))
    @test isfile(joinpath(cachedir2, "FooBar1.ji"))
    @test Base.stale_cachefile(FooBar_file, joinpath(cachedir, "FooBar.ji")) === true
    @test Base.stale_cachefile(FooBar1_file, joinpath(cachedir2, "FooBar1.ji")) isa Tsc
    @test fb_uuid == Base.module_build_id(FooBar)
    fb_uuid1 = Base.module_build_id(FooBar1)
    @test fb_uuid != fb_uuid1

    # test checksum
    open(joinpath(cachedir2, "FooBar1.ji"), "a") do f
        write(f, 0x076cac96) # append 4 random bytes
    end
    @test Base.stale_cachefile(FooBar1_file, joinpath(cachedir2, "FooBar1.ji")) === true

    # test behavior of precompile modules that throw errors
    FooBar2_file = joinpath(dir, "FooBar2.jl")
    write(FooBar2_file,
          """
          module FooBar2
          error("break me")
          end
          """)
    @test_warn r"LoadError: break me\nStacktrace:\n \[1\] [\e01m\[]*error" try
            Base.require(Main, :FooBar2)
            error("the \"break me\" test failed")
        catch exc
            isa(exc, ErrorException) || rethrow()
            occursin("ERROR: LoadError: break me", exc.msg) && rethrow()
        end

    # Test that trying to eval into closed modules during precompilation is an error
    FooBar3_file = joinpath(dir, "FooBar3.jl")
    FooBar3_inc = joinpath(dir, "FooBar3_inc.jl")
    write(FooBar3_inc, "x=1\n")
    for code in ["Core.eval(Base, :(x=1))", "Base.include(Base, \"FooBar3_inc.jl\")"]
        write(FooBar3_file, code)
        @test_warn "Evaluation into the closed module `Base` breaks incremental compilation" try
                Base.require(Main, :FooBar3)
            catch exc
                isa(exc, ErrorException) || rethrow()
            end
    end

    # Test transitive dependency for #21266
    FooBarT_file = joinpath(dir, "FooBarT.jl")
    write(FooBarT_file,
          """
          module FooBarT
          end
          """)
    FooBarT1_file = joinpath(dir, "FooBarT1.jl")
    write(FooBarT1_file,
          """
          module FooBarT1
              using FooBarT
          end
          """)
    FooBarT2_file = joinpath(dir, "FooBarT2.jl")
    write(FooBarT2_file,
          """
          module FooBarT2
              using FooBarT1
          end
          """)
    Base.compilecache(Base.PkgId("FooBarT2"))
    write(FooBarT1_file,
          """
          module FooBarT1
          end
          """)
    rm(FooBarT_file)
    @test Base.stale_cachefile(FooBarT2_file, joinpath(cachedir2, "FooBarT2.ji")) === true
    @test Base.require(Main, :FooBarT2) isa Module
end
end

# method root provenance & external code caching
precompile_test_harness("code caching") do dir
    Bid = rootid(Base)
    Cache_module = :Cacheb8321416e8a3e2f1
    # Note: calling setindex!(::Dict{K,V}, ::Any, ::K) adds both compression and codegen roots
    write(joinpath(dir, "$Cache_module.jl"),
          """
          module $Cache_module
              struct X end
              struct X2 end
              @noinline function f(d)
                  @noinline
                  d[X()] = nothing
              end
              @noinline fpush(dest) = push!(dest, X())
              function callboth()
                  f(Dict{X,Any}())
                  fpush(X[])
                  nothing
              end
              function getelsize(list::Vector{T}) where T
                  n = 0
                  for item in list
                      n += sizeof(T)
                  end
                  return n
              end
              precompile(callboth, ())
              precompile(getelsize, (Vector{Int32},))
          end
          """)
    Base.compilecache(Base.PkgId(string(Cache_module)))
    @eval using $Cache_module
    M = getfield(@__MODULE__, Cache_module)
    # Test that this cache file "owns" all the roots
    Mid = rootid(M)
    for name in (:f, :fpush, :callboth)
        func = getfield(M, name)
        m = only(collect(methods(func)))
        @test all(i -> root_provenance(m, i) == Mid, 1:length(m.roots))
    end
    # Check that we can cache external CodeInstances:
    # size(::Vector) has an inferred specialization for Vector{X}
    msize = which(size, (Vector{<:Any},))
    hasspec = false
    msizespecs = msize.specializations::Core.SimpleVector
    for i = 1:length(msizespecs)
        mi = msizespecs[i]
        if isa(mi, Core.MethodInstance) && mi.specTypes == Tuple{typeof(size),Vector{Cacheb8321416e8a3e2f1.X}}
            if isdefined(mi, :cache) && isa(mi.cache, Core.CodeInstance) && mi.cache.max_world == typemax(UInt) && mi.cache.inferred !== nothing
                hasspec = true
                break
            end
        end
    end
    @test hasspec
    # Test that compilation adds to method roots with appropriate provenance
    m = which(setindex!, (Dict{M.X,Any}, Any, M.X))
    @test M.X ∈ m.roots
    # Check that roots added outside of incremental builds get attributed to a moduleid of 0
    Base.invokelatest() do
        Dict{M.X2,Any}()[M.X2()] = nothing
    end
    @test M.X2 ∈ m.roots
    groups = group_roots(m)
    @test M.X ∈ groups[Mid]           # attributed to M
    @test M.X2 ∈ groups[0]            # activate module is not known
    @test !isempty(groups[Bid])
    # Check that internal methods and their roots are accounted appropriately
    minternal = which(M.getelsize, (Vector,))
    mi = minternal.specializations::Core.MethodInstance
    @test mi.specTypes == Tuple{typeof(M.getelsize),Vector{Int32}}
    ci = mi.cache
    @test ci.relocatability == 1
    @test ci.inferred !== nothing
    # ...and that we can add "untracked" roots & non-relocatable CodeInstances to them too
    Base.invokelatest() do
        M.getelsize(M.X2[])
    end
    mispecs = minternal.specializations::Core.SimpleVector
    @test mispecs[1] === mi
    mi = mispecs[2]::Core.MethodInstance
    ci = mi.cache
    @test ci.relocatability == 0
    # PkgA loads PkgB, and both add roots to the same `push!` method (both before and after loading B)
    Cache_module2 = :Cachea1544c83560f0c99
    write(joinpath(dir, "$Cache_module2.jl"),
          """
          module $Cache_module2
              struct Y end
              @noinline f(dest) = push!(dest, Y())
              callf() = f(Y[])
              callf()
              using $(Cache_module)
              struct Z end
              @noinline g(dest) = push!(dest, Z())
              callg() = g(Z[])
              callg()
          end
          """)
    Base.compilecache(Base.PkgId(string(Cache_module2)))
    @eval using $Cache_module2
    M2 = getfield(@__MODULE__, Cache_module2)
    M2id = rootid(M2)
    dest = []
    Base.invokelatest() do  # use invokelatest to see the results of loading the compile
        M2.f(dest)
        M.fpush(dest)
        M2.g(dest)
        @test dest == [M2.Y(), M.X(), M2.Z()]
        @test M2.callf() == [M2.Y()]
        @test M2.callg() == [M2.Z()]
        @test M.fpush(M.X[]) == [M.X()]
    end
    mT = which(push!, (Vector{T} where T, Any))
    groups = group_roots(mT)
    @test M2.Y ∈ groups[M2id]
    @test M2.Z ∈ groups[M2id]
    @test M.X ∈ groups[Mid]
    @test M.X ∉ groups[M2id]
    # backedges of external MethodInstances
    # Root gets used by RootA and RootB, and both consumers end up inferring the same MethodInstance from Root
    # Do both callers get listed as backedges?
    RootModule = :Root_0xab07d60518763a7e
    write(joinpath(dir, "$RootModule.jl"),
          """
          module $RootModule
          function f(x)
              while x < 10
                  x += oftype(x, 1)
              end
              return x
          end
          g1() = f(Int16(9))
          g2() = f(Int16(9))
          # all deliberately uncompiled
          end
          """)
    RootA = :RootA_0xab07d60518763a7e
    write(joinpath(dir, "$RootA.jl"),
          """
          module $RootA
          using $RootModule
          fA() = $RootModule.f(Int8(4))
          fA()
          $RootModule.g1()
          end
          """)
    RootB = :RootB_0xab07d60518763a7e
    write(joinpath(dir, "$RootB.jl"),
          """
          module $RootB
          using $RootModule
          fB() = $RootModule.f(Int8(4))
          fB()
          $RootModule.g2()
          end
          """)
    Base.compilecache(Base.PkgId(string(RootA)))
    Base.compilecache(Base.PkgId(string(RootB)))
    @eval using $RootA
    @eval using $RootB
    MA = getfield(@__MODULE__, RootA)
    MB = getfield(@__MODULE__, RootB)
    M = getfield(MA, RootModule)
    m = which(M.f, (Any,))
    mspecs = m.specializations
    mspecs isa Core.SimpleVector || (mspecs = Core.svec(mspecs))
    for mi in mspecs
        mi === nothing && continue
        mi = mi::Core.MethodInstance
        if mi.specTypes.parameters[2] === Int8
            # external callers
            mods = Module[]
            for be in mi.backedges
                push!(mods, be.def.module)
            end
            @test MA ∈ mods
            @test MB ∈ mods
            @test length(mods) == 2
        elseif mi.specTypes.parameters[2] === Int16
            # internal callers
            meths = Method[]
            for be in mi.backedges
                push!(meths, be.def)
            end
            @test which(M.g1, ()) ∈ meths
            @test which(M.g2, ()) ∈ meths
            @test length(meths) == 2
        end
    end

    # Invalidations (this test is adapted from SnoopCompile)
    function hasvalid(mi, world)
        isdefined(mi, :cache) || return false
        ci = mi.cache
        while true
            ci.max_world >= world && return true
            isdefined(ci, :next) || return false
            ci = ci.next
        end
    end

    StaleA = :StaleA_0xab07d60518763a7e
    StaleB = :StaleB_0xab07d60518763a7e
    StaleC = :StaleC_0xab07d60518763a7e
    write(joinpath(dir, "$StaleA.jl"),
        """
        module $StaleA

        stale(x) = rand(1:8)
        stale(x::Int) = length(digits(x))

        not_stale(x::String) = first(x)

        use_stale(c) = stale(c[1]) + not_stale("hello")
        build_stale(x) = use_stale(Any[x])

        # force precompilation
        build_stale(37)
        stale('c')

        ## Reporting tests (unrelated to the above)
        nbits(::Int8) = 8
        nbits(::Int16) = 16

        end
        """
    )
    write(joinpath(dir, "$StaleB.jl"),
        """
        module $StaleB

        # StaleB does not know about StaleC when it is being built.
        # However, if StaleC is loaded first, we get `"jl_insert_method_instance"`
        # invalidations.
        using $StaleA

        # This will be invalidated if StaleC is loaded
        useA() = $StaleA.stale("hello")

        # force precompilation
        useA()

        ## Reporting tests
        call_nbits(x::Integer) = $StaleA.nbits(x)
        map_nbits() = map(call_nbits, Integer[Int8(1), Int16(1)])
        map_nbits()

        end
        """
    )
    write(joinpath(dir, "$StaleC.jl"),
        """
        module $StaleC

        using $StaleA

        $StaleA.stale(x::String) = length(x)
        call_buildstale(x) = $StaleA.build_stale(x)

        call_buildstale("hey")

        end # module
        """
    )
    for pkg in (StaleA, StaleB, StaleC)
        Base.compilecache(Base.PkgId(string(pkg)))
    end
    @eval using $StaleA
    MA = getfield(@__MODULE__, StaleA)
    Base.eval(MA, :(nbits(::UInt8) = 8))
    @eval using $StaleC
    invalidations = ccall(:jl_debug_method_invalidation, Any, (Cint,), 1)
    @eval using $StaleB
    ccall(:jl_debug_method_invalidation, Any, (Cint,), 0)
    MB = getfield(@__MODULE__, StaleB)
    MC = getfield(@__MODULE__, StaleC)
    world = Base.get_world_counter()
    m = only(methods(MA.use_stale))
    mi = m.specializations::Core.MethodInstance
    @test hasvalid(mi, world)   # it was re-inferred by StaleC
    m = only(methods(MA.build_stale))
    mis = filter(!isnothing, collect(m.specializations::Core.SimpleVector))
    @test length(mis) == 2
    for mi in mis
        mi = mi::Core.MethodInstance
        if mi.specTypes.parameters[2] == Int
            @test mi.cache.max_world < world
        else
            # The variant for String got "healed" by recompilation in StaleC
            @test mi.specTypes.parameters[2] == String
            @test mi.cache.max_world == typemax(UInt)
        end
    end
    m = only(methods(MB.useA))
    mi = m.specializations::Core.MethodInstance
    @test !hasvalid(mi, world)      # invalidated by the stale(x::String) method in StaleC
    m = only(methods(MC.call_buildstale))
    mi = m.specializations::Core.MethodInstance
    @test hasvalid(mi, world)       # was compiled with the new method

    # Reporting test (ensure SnoopCompile works)
    @test all(i -> isassigned(invalidations, i), eachindex(invalidations))
    m = only(methods(MB.call_nbits))
    for mi in m.specializations::Core.SimpleVector
        mi === nothing && continue
        hv = hasvalid(mi, world)
        @test mi.specTypes.parameters[end] === Integer ? !hv : hv
    end

    setglobal!(Main, :inval, invalidations)
    idxs = findall(==("verify_methods"), invalidations)
    idxsbits = filter(idxs) do i
        mi = invalidations[i-1]
        mi.def == m
    end
    idx = only(idxsbits)
    tagbad = invalidations[idx+1]
    @test isa(tagbad, Int32)
    j = findfirst(==(tagbad), invalidations)
    @test invalidations[j-1] == "insert_backedges_callee"
    @test isa(invalidations[j-2], Type)
    @test isa(invalidations[j+1], Vector{Any}) # [nbits(::UInt8)]

    m = only(methods(MB.map_nbits))
    @test !hasvalid(m.specializations::Core.MethodInstance, world+1) # insert_backedges invalidations also trigger their backedges
end

precompile_test_harness("invoke") do dir
    InvokeModule = :Invoke0x030e7e97c2365aad
    CallerModule = :Caller0x030e7e97c2365aad
    write(joinpath(dir, "$InvokeModule.jl"),
          """
          module $InvokeModule
              export f, g, h, q, fnc, gnc, hnc, qnc   # nc variants do not infer to a Const
              export f44320, g44320
              export getlast
              # f is for testing invoke that occurs within a dependency
              f(x::Real) = 0
              f(x::Int) = x < 5 ? 1 : invoke(f, Tuple{Real}, x)
              fnc(x::Real) = rand()-1
              fnc(x::Int) = x < 5 ? rand()+1 : invoke(fnc, Tuple{Real}, x)
              # g is for testing invoke that occurs from a dependent
              g(x::Real) = 0
              g(x::Int) = 1
              gnc(x::Real) = rand()-1
              gnc(x::Int) = rand()+1
              # h will be entirely superseded by a new method (full invalidation)
              h(x::Real) = 0
              h(x::Int) = x < 5 ? 1 : invoke(h, Tuple{Integer}, x)
              hnc(x::Real) = rand()-1
              hnc(x::Int) = x < 5 ? rand()+1 : invoke(hnc, Tuple{Integer}, x)
              # q will have some callers invalidated
              q(x::Integer) = 0
              qnc(x::Integer) = rand()-1
              # Issue #44320
              f44320(::Int) = 1
              f44320(::Any) = 2
              g44320() = invoke(f44320, Tuple{Any}, 0)
              g44320()

              # Adding new specializations should not invalidate `invoke`s
              function getlast(itr)
                  x = nothing
                  for y in itr
                      x = y
                  end
                  return x
              end
              getlast(a::AbstractArray) = invoke(getlast, Tuple{Any}, a)
          end
          """)
          write(joinpath(dir, "$CallerModule.jl"),
          """
          module $CallerModule
              using $InvokeModule
              # involving external modules
              callf(x) = f(x)
              callg(x) = x < 5 ? g(x) : invoke(g, Tuple{Real}, x)
              callh(x) = h(x)
              callq(x) = q(x)
              callqi(x) = invoke(q, Tuple{Integer}, x)
              callfnc(x) = fnc(x)
              callgnc(x) = x < 5 ? gnc(x) : invoke(gnc, Tuple{Real}, x)
              callhnc(x) = hnc(x)
              callqnc(x) = qnc(x)
              callqnci(x) = invoke(qnc, Tuple{Integer}, x)

              # Purely internal
              internal(x::Real) = 0
              internal(x::Int) = x < 5 ? 1 : invoke(internal, Tuple{Real}, x)
              internalnc(x::Real) = rand()-1
              internalnc(x::Int) = x < 5 ? rand()+1 : invoke(internalnc, Tuple{Real}, x)

              # Issue #44320
              f44320(::Real) = 3

              call_getlast(x) = getlast(x)

              # force precompilation
              begin
                  Base.Experimental.@force_compile
                  callf(3)
                  callg(3)
                  callh(3)
                  callq(3)
                  callqi(3)
                  callfnc(3)
                  callgnc(3)
                  callhnc(3)
                  callqnc(3)
                  callqnci(3)
                  internal(3)
                  internalnc(3)
                  call_getlast([1,2,3])
              end

              # Now that we've precompiled, invalidate with a new method that overrides the `invoke` dispatch
              $InvokeModule.h(x::Integer) = -1
              $InvokeModule.hnc(x::Integer) = rand() - 20
              # ...and for q, override with a more specialized method that should leave only the invoked version still valid
              $InvokeModule.q(x::Int) = -1
              $InvokeModule.qnc(x::Int) = rand()+1
          end
          """)
    Base.compilecache(Base.PkgId(string(CallerModule)))
    @eval using $InvokeModule: $InvokeModule
    MI = getfield(@__MODULE__, InvokeModule)
    @eval $MI.getlast(a::UnitRange) = a.stop
    @eval using $CallerModule
    M = getfield(@__MODULE__, CallerModule)

    function get_method_for_type(func, @nospecialize(T))   # return the method func(::T)
        for m in methods(func)
            m.sig.parameters[end] === T && return m
        end
        error("no ::Real method found for $func")
    end
    function nvalid(mi::Core.MethodInstance)
        isdefined(mi, :cache) || return 0
        ci = mi.cache
        n = Int(ci.max_world == typemax(UInt))
        while isdefined(ci, :next)
            ci = ci.next
            n += ci.max_world == typemax(UInt)
        end
        return n
    end

    for func in (M.f, M.g, M.internal, M.fnc, M.gnc, M.internalnc)
        m = get_method_for_type(func, Real)
        mi = m.specializations::Core.MethodInstance
        @test length(mi.backedges) == 2
        @test mi.backedges[1] === Tuple{typeof(func), Real}
        @test isa(mi.backedges[2], Core.MethodInstance)
        @test mi.cache.max_world == typemax(mi.cache.max_world)
    end
    for func in (M.q, M.qnc)
        m = get_method_for_type(func, Integer)
        mi = m.specializations::Core.MethodInstance
        @test length(mi.backedges) == 2
        @test mi.backedges[1] === Tuple{typeof(func), Integer}
        @test isa(mi.backedges[2], Core.MethodInstance)
        @test mi.cache.max_world == typemax(mi.cache.max_world)
    end

    m = get_method_for_type(M.h, Real)
    @test m.specializations === Core.svec()
    m = get_method_for_type(M.hnc, Real)
    @test m.specializations === Core.svec()
    m = only(methods(M.callq))
    @test m.specializations === Core.svec() || nvalid(m.specializations::Core.MethodInstance) == 0
    m = only(methods(M.callqnc))
    @test m.specializations === Core.svec() || nvalid(m.specializations::Core.MethodInstance) == 0
    m = only(methods(M.callqi))
    @test (m.specializations::Core.MethodInstance).specTypes == Tuple{typeof(M.callqi), Int}
    m = only(methods(M.callqnci))
    @test (m.specializations::Core.MethodInstance).specTypes == Tuple{typeof(M.callqnci), Int}

    m = only(methods(M.g44320))
    @test (m.specializations::Core.MethodInstance).cache.max_world == typemax(UInt)

    m = which(MI.getlast, (Any,))
    @test (m.specializations::Core.MethodInstance).cache.max_world == typemax(UInt)

    # Precompile specific methods for arbitrary arg types
    invokeme(x) = 1
    invokeme(::Int) = 2
    m_any, m_int = sort(collect(methods(invokeme)); by=m->(m.file,m.line))
    @test precompile(invokeme, (Int,), m_any)
    @test (m_any.specializations::Core.MethodInstance).specTypes === Tuple{typeof(invokeme), Int}
    @test m_int.specializations === Core.svec()
end

# test --compiled-modules=no command line option
precompile_test_harness("--compiled-modules=no") do dir
    Time_module = :Time4b3a94a1a081a8cb
    write(joinpath(dir, "$Time_module.jl"),
          """
          module $Time_module
              time = Base.time()
          end
          """)
    Base.compilecache(Base.PkgId("Time4b3a94a1a081a8cb"))
    exename = `$(Base.julia_cmd()) --compiled-modules=yes --startup-file=no`
    testcode = """
        insert!(LOAD_PATH, 1, $(repr(dir)))
        insert!(DEPOT_PATH, 1, $(repr(dir)))
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
end

# test loading a package with conflicting namespace
precompile_test_harness("conflicting namespaces") do dir
    Test_module = :Test6c92f26
    write(joinpath(dir, "Iterators.jl"),
          """
          module Iterators
          end
          """)
    write(joinpath(dir, "$Test_module.jl"),
          """
          module $Test_module
               import Iterators # FIXME: use `using`
          end
          """)
    testcode = """
        insert!(LOAD_PATH, 1, $(repr(dir)))
        insert!(DEPOT_PATH, 1, $(repr(dir)))
        using $Test_module
        println(stderr, $Test_module.Iterators)
    """

    exename = `$(Base.julia_cmd()) --startup-file=no`
    let fname = tempname()
        try
            for i = 1:2
                @test readchomp(pipeline(`$exename -E $(testcode)`, stderr=fname)) == "nothing"
                @test read(fname, String) == "Iterators\n"
            end
        finally
            rm(fname, force=true)
        end
    end
end

precompile_test_harness("package_callbacks") do dir
    loaded_modules = Channel{Symbol}(32)
    callback = (mod::Base.PkgId) -> put!(loaded_modules, Symbol(mod.name))
    push!(Base.package_callbacks, callback)
    try
        Test1_module = :Teste4095a81
        Test2_module = :Teste4095a82
        Test3_module = :Teste4095a83

        write(joinpath(dir, "$(Test1_module).jl"),
              """
              module $(Test1_module)
              end
              """)
        Base.compilecache(Base.PkgId("$(Test1_module)"))

        write(joinpath(dir, "$(Test2_module).jl"),
              """
              module $(Test2_module)
                  using $(Test1_module)
              end
              """)
        Base.compilecache(Base.PkgId("$(Test2_module)"))

        @test !Base.isbindingresolved(Main, Test2_module)
        Base.require(Main, Test2_module)
        @test take!(loaded_modules) == Test1_module
        @test take!(loaded_modules) == Test2_module
        write(joinpath(dir, "$(Test3_module).jl"),
              """
              module $(Test3_module)
                  using $(Test3_module)
              end
              """)
        Base.require(Main, Test3_module)
        @test take!(loaded_modules) == Test3_module
    finally
        pop!(Base.package_callbacks)
    end
    L = ReentrantLock()
    E = Base.Event()
    t = errormonitor(@async lock(L) do
                     wait(E)
                     Base.root_module_key(Base)
                     end)
    Test4_module = :Teste4095a84
    write(joinpath(dir, "$(Test4_module).jl"),
          """
          module $(Test4_module)
          end
          """)
    Base.compilecache(Base.PkgId("$(Test4_module)"))
    push!(Base.package_callbacks, _->(notify(E); lock(L) do; end))
    # should not hang here
    try
        @eval using $(Symbol(Test4_module))
        wait(t)
    finally
        pop!(Base.package_callbacks)
    end
end

# Issue #19960
(f -> f())() do # wrap in function scope, so we can test world errors
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
            module $ModuleA
                import Distributed: myid
                export f
                f() = myid()
            end
            """)

        write(joinpath(load_path, "$ModuleB.jl"),
            """
            module $ModuleB
                using $ModuleA
                export g
                g() = f()
            end
            """)

        @everywhere test_workers begin
            pushfirst!(LOAD_PATH, $load_path)
            pushfirst!(DEPOT_PATH, $load_cache_path)
        end
        try
            @eval using $ModuleB
            uuid = Base.module_build_id(Base.root_module(Main, ModuleB))
            for wid in test_workers
                @test Distributed.remotecall_eval(Main, wid, quote
                        Base.module_build_id(Base.root_module(Main, $(QuoteNode(ModuleB))))
                    end) == uuid
                if wid != myid() # avoid world-age errors on the local proc
                    @test remotecall_fetch(g, wid) == wid
                end
            end
        finally
            @everywhere test_workers begin
                popfirst!(LOAD_PATH)
                popfirst!(DEPOT_PATH)
            end
        end
    finally
        cd(save_cwd)
        try
            rm(temp_path, recursive=true)
        catch err
            @show err
        end
        pop!(test_workers) # remove myid
        rmprocs(test_workers)
    end
end

# Ensure that module-loading plays nicely with Base.delete_method
# wrapped in function scope, so we can test world errors
precompile_test_harness("delete_method") do dir
    A_module = :Aedb164bd3a126418
    B_module = :Bedb164bd3a126418
    A_file = joinpath(dir, "$A_module.jl")
    B_file = joinpath(dir, "$B_module.jl")
    write(A_file,
          """
          module $A_module

          export apc, anopc, apcnc, anopcnc

          # Infer to a const
          apc(::Int, ::Int) = 1
          apc(::Any, ::Any) = 2

          anopc(::Int, ::Int) = 1
          anopc(::Any, ::Any) = 2

          # Do not infer to a const
          apcnc(::Int, ::Int) = rand() - 1
          apcnc(::Any, ::Any) = rand() + 1

          anopcnc(::Int, ::Int) = rand() - 1
          anopcnc(::Any, ::Any) = rand() + 1

          end
          """)
    write(B_file,
          """
          module $B_module

          using $A_module

          bpc(x) = apc(x, x)
          bnopc(x) = anopc(x, x)
          bpcnc(x) = apcnc(x, x)
          bnopcnc(x) = anopcnc(x, x)

          precompile(bpc, (Int,))
          precompile(bpc, (Float64,))
          precompile(bpcnc, (Int,))
          precompile(bpcnc, (Float64,))

          end
          """)
    A = Base.require(Main, A_module)
    for mths in (collect(methods(A.apc)), collect(methods(A.anopc)), collect(methods(A.apcnc)), collect(methods(A.anopcnc)))
        idx = findfirst(m -> m.sig.parameters[end] === Int, mths)
        Base.delete_method(mths[idx])
    end
    B = Base.require(Main, B_module)
    for f in (B.bpc, B.bnopc, B.bpcnc, B.bnopcnc)
        @test Base.invokelatest(f, 1) > 1
        @test Base.invokelatest(f, 1.0) > 1
    end
end

precompile_test_harness("Issues #19030 and #25279") do load_path
    ModuleA = :Issue19030
    write(joinpath(load_path, "$ModuleA.jl"),
        """
        module $ModuleA
            __init__() = push!(Base.package_callbacks, sym->nothing)
        end
        """)
    l0 = length(Base.package_callbacks)
    @eval using $ModuleA
    @test length(Base.package_callbacks) == l0 + 1
end

precompile_test_harness("Issue #25604") do load_path
    write(joinpath(load_path, "A25604.jl"),
        """
        module A25604
        using B25604
        using C25604
        end
        """)
    write(joinpath(load_path, "B25604.jl"),
        """
        module B25604
        end
        """)
    write(joinpath(load_path, "C25604.jl"),
        """
        module C25604
        using B25604
        end
        """)
    Base.compilecache(Base.PkgId("A25604"))
    @test_nowarn @eval using A25604
end

precompile_test_harness("Issue #26028") do load_path
    write(joinpath(load_path, "Foo26028.jl"),
        """
        module Foo26028
        module Bar26028
            x = 0
        end
        function __init__()
            include(joinpath(@__DIR__, "Baz26028.jl"))
        end
        end
        """)
    write(joinpath(load_path, "Baz26028.jl"),
        """
        module Baz26028
        import Foo26028.Bar26028.x
        end
        """)
    Base.compilecache(Base.PkgId("Foo26028"))
    @test_nowarn @eval using Foo26028
end

precompile_test_harness("Issue #29936") do load_path
    write(joinpath(load_path, "Foo29936.jl"),
          """
          module Foo29936
          const global m = Val{nothing}()
          const global h = Val{:hey}()
          wab = [("a", m), ("b", h),]
          end
          """)
    @eval using Foo29936
    @test [("Plan", Foo29936.m), ("Plan", Foo29936.h),] isa Vector{Tuple{String,Val}}
end

precompile_test_harness("Issue #25971") do load_path
    sourcefile = joinpath(load_path, "Foo25971.jl")
    write(sourcefile, "module Foo25971 end")
    chmod(sourcefile, 0o666)
    cachefile, _ = Base.compilecache(Base.PkgId("Foo25971"))
    @test filemode(sourcefile) == filemode(cachefile)
    chmod(sourcefile, 0o600)
    cachefile, _ = Base.compilecache(Base.PkgId("Foo25971"))
    @test filemode(sourcefile) == filemode(cachefile)
    chmod(sourcefile, 0o444)
    cachefile, _ = Base.compilecache(Base.PkgId("Foo25971"))
    # Check writable
    @test touch(cachefile) == cachefile
end

precompile_test_harness("Issue #38312") do load_path
    TheType = """Array{Ref{Val{1}}, 1}"""
    write(joinpath(load_path, "Foo38312.jl"),
        """
        module Foo38312
        const TheType = $TheType
        end
        """)
    write(joinpath(load_path, "Bar38312.jl"),
        """
        module Bar38312
        const TheType = $TheType
        end
        """)
    Base.compilecache(Base.PkgId("Foo38312"))
    Base.compilecache(Base.PkgId("Bar38312"))
    @test pointer_from_objref((@eval (using Foo38312; Foo38312)).TheType) ===
          pointer_from_objref(eval(Meta.parse(TheType))) ===
          pointer_from_objref((@eval (using Bar38312; Bar38312)).TheType)
end

precompile_test_harness("Opaque Closure") do load_path
    write(joinpath(load_path, "OCPrecompile.jl"),
        """
        module OCPrecompile
        using Base.Experimental: @opaque
        f(x) = @opaque y->x+y
        end
        """)
    Base.compilecache(Base.PkgId("OCPrecompile"))
    f = (@eval (using OCPrecompile; OCPrecompile)).f
    @test Base.invokelatest(f, 1)(2) == 3
end

# issue #39405
precompile_test_harness("Renamed Imports") do load_path
    write(joinpath(load_path, "RenameImports.jl"),
          """
          module RenameImports
          import Base.Experimental as ex
          test() = ex
          end
          """)
    Base.compilecache(Base.PkgId("RenameImports"))
    @test (@eval (using RenameImports; RenameImports.test())) isa Module
end

# issue #41872 (example from #38983)
precompile_test_harness("No external edges") do load_path
    write(joinpath(load_path, "NoExternalEdges.jl"),
          """
          module NoExternalEdges
          bar(x::Int) = hcat(rand())
          @inline bar() = hcat(rand())
          bar(x::Float64) = bar()
          foo1() = bar(1)
          foo2() = bar(1.0)
          foo3() = bar()
          foo4() = hcat(rand())
          precompile(foo1, ())
          precompile(foo2, ())
          precompile(foo3, ())
          precompile(foo4, ())
          end
          """)
    Base.compilecache(Base.PkgId("NoExternalEdges"))
    @eval begin
        using NoExternalEdges
        @test (only(methods(NoExternalEdges.foo1)).specializations::Core.MethodInstance).cache.max_world != 0
        @test (only(methods(NoExternalEdges.foo2)).specializations::Core.MethodInstance).cache.max_world != 0
        @test (only(methods(NoExternalEdges.foo3)).specializations::Core.MethodInstance).cache.max_world != 0
        @test (only(methods(NoExternalEdges.foo4)).specializations::Core.MethodInstance).cache.max_world != 0
    end
end

@testset "issue 38149" begin
    M = Module()
    @eval M begin
        @nospecialize
        f(x, y) = x + y
        f(x::Int, y) = 2x + y
    end
    @test precompile(M.f, (Int, Any))
    @test precompile(M.f, (AbstractFloat, Any))
    mis = map(methods(M.f)) do m
        m.specializations::Core.MethodInstance
    end
    @test any(mi -> mi.specTypes.parameters[2] === Any, mis)
    @test all(mi -> isa(mi.cache, Core.CodeInstance), mis)
end

# Test that the cachepath is available in pkgorigins during the
# __init__ callback
precompile_test_harness("__init__ cachepath") do load_path
    write(joinpath(load_path, "InitCachePath.jl"),
          """
          module InitCachePath
            __init__() = Base.pkgorigins[Base.PkgId(InitCachePath)]
          end
          """)
    @test isa((@eval (using InitCachePath; InitCachePath)), Module)
end

# Test that precompilation can handle invalidated methods created from `precompile`,
# not via backedges.
precompile_test_harness("Issue #46558") do load_path
    write(joinpath(load_path, "Foo46558.jl"),
        """
        module Foo46558
        foo(x::Real) = 1
        end
        """)
    write(joinpath(load_path, "Bar46558.jl"),
        """
        module Bar46558
        using Foo46558
        precompile(Foo46558.foo, (Int,))
        end
        """)
    Base.compilecache(Base.PkgId("Foo46558"))
    Base.compilecache(Base.PkgId("Bar46558"))
    Foo = (@eval (using Foo46558; Foo46558))
    @eval ($Foo.foo)(x::Int) = 2
    Bar = (@eval (using Bar46558; Bar46558))
    @test (@eval $Foo.foo(1)) == 2
end

precompile_test_harness("issue #46296") do load_path
    write(joinpath(load_path, "CodeInstancePrecompile.jl"),
        """
        module CodeInstancePrecompile

        mi = first(methods(identity)).specializations[1]
        ci = Core.CodeInstance(mi, Any, nothing, nothing, zero(Int32), typemin(UInt),
                               typemax(UInt), zero(UInt32), zero(UInt32), nothing, 0x00)

        __init__() = @assert ci isa Core.CodeInstance

        end
        """)
    Base.compilecache(Base.PkgId("CodeInstancePrecompile"))
    (@eval (using CodeInstancePrecompile))
end

precompile_test_harness("Recursive types") do load_path
    write(joinpath(load_path, "RecursiveTypeDef.jl"),
        """
        module RecursiveTypeDef

        struct C{T,O} end
        struct A{T,N,O} <: AbstractArray{C{T,A{T,N,O}},N}
            sz::NTuple{N,Int}
        end

        end
        """)
    Base.compilecache(Base.PkgId("RecursiveTypeDef"))
    (@eval (using RecursiveTypeDef))
    a = Base.invokelatest(RecursiveTypeDef.A{Float64,2,String}, (3, 3))
    @test isa(a, AbstractArray)
end

@testset "issue 46778" begin
    f46778(::Any, ::Type{Int}) = 1
    f46778(::Any, ::DataType) = 2
    @test precompile(Tuple{typeof(f46778), Int, DataType})
    @test (which(f46778, Tuple{Any,DataType}).specializations::Core.MethodInstance).cache.invoke != C_NULL
end


precompile_test_harness("Module tparams") do load_path
    write(joinpath(load_path, "ModuleTparams.jl"),
        """
        module ModuleTparams
            module TheTParam
            end

            struct ParamStruct{T}; end
            const the_struct = ParamStruct{TheTParam}()
        end
        """)
    Base.compilecache(Base.PkgId("ModuleTparams"))
    (@eval (using ModuleTparams))
    @test ModuleTparams.the_struct === Base.invokelatest(ModuleTparams.ParamStruct{ModuleTparams.TheTParam})
end

precompile_test_harness("PkgCacheInspector") do load_path
    # Test functionality needed by PkgCacheInspector.jl
    write(joinpath(load_path, "PCI.jl"),
        """
        module PCI
        Base.repl_cmd() = 55            # external method
        f() = Base.repl_cmd(7, "hello")   # external specialization (should never exist otherwise)
        try
            f()
        catch
        end
        end
        """)
    cachefile, ocachefile = Base.compilecache(Base.PkgId("PCI"))

    # Get the depmods
    local depmods
    @lock Base.require_lock begin
        local depmodnames
        io = open(cachefile, "r")
        try
            # isvalid_cache_header returns checksum id or zero
            Base.isvalid_cache_header(io) == 0 && throw(ArgumentError("Invalid header in cache file $cachefile."))
            depmodnames = Base.parse_cache_header(io)[3]
            Base.isvalid_file_crc(io) || throw(ArgumentError("Invalid checksum in cache file $cachefile."))
        finally
            close(io)
        end
        ndeps = length(depmodnames)
        depmods = Vector{Any}(undef, ndeps)
        for i in 1:ndeps
            modkey, build_id = depmodnames[i]
            dep = Base._tryrequire_from_serialized(modkey, build_id)
            if !isa(dep, Module)
                return dep
            end
            depmods[i] = dep
        end
    end

    if ocachefile !== nothing
        sv = ccall(:jl_restore_package_image_from_file, Any, (Cstring, Any, Cint), ocachefile, depmods, true)
    else
        sv = ccall(:jl_restore_incremental, Any, (Cstring, Any, Cint), cachefile, depmods, true)
    end

    modules, init_order, external_methods, new_specializations, new_method_roots, external_targets, edges = sv
    m = only(external_methods)
    @test m.name == :repl_cmd && m.nargs < 2
    @test any(new_specializations) do ci
        mi = ci.def
        mi.specTypes == Tuple{typeof(Base.repl_cmd), Int, String}
    end
end

precompile_test_harness("DynamicExpressions") do load_path
    # https://github.com/JuliaLang/julia/pull/47184#issuecomment-1364716312
    write(joinpath(load_path, "Float16MWE.jl"),
        """
        module Float16MWE
        struct Node{T}
            val::T
        end
        doconvert(::Type{<:Node}, val) = convert(Float16, val)
        precompile(Tuple{typeof(doconvert), Type{Node{Float16}}, Float64})
        end # module Float16MWE
        """)
    Base.compilecache(Base.PkgId("Float16MWE"))
    @eval using Float16MWE
    @test @invokelatest(Float16MWE.doconvert(Float16MWE.Node{Float16}, -1.2)) === Float16(-1.2)
end

precompile_test_harness("BadInvalidations") do load_path
    write(joinpath(load_path, "BadInvalidations.jl"),
        """
        module BadInvalidations
        Base.Experimental.@compiler_options compile=min optimize=1
        getval() = Base.a_method_to_overwrite_in_test()
        getval()
        end # module BadInvalidations
        """)
    Base.compilecache(Base.PkgId("BadInvalidations"))
    @eval Base a_method_to_overwrite_in_test() = inferencebarrier(2)
    @eval using BadInvalidations
    @test Base.invokelatest(BadInvalidations.getval) === 2
end

# https://github.com/JuliaLang/julia/issues/48074
precompile_test_harness("WindowsCacheOverwrite") do load_path
    # https://github.com/JuliaLang/julia/pull/47184#issuecomment-1364716312
    write(joinpath(load_path, "WindowsCacheOverwrite.jl"),
        """
        module WindowsCacheOverwrite
        end # module
        """)
    ji, ofile = Base.compilecache(Base.PkgId("WindowsCacheOverwrite"))
    @eval using WindowsCacheOverwrite

    write(joinpath(load_path, "WindowsCacheOverwrite.jl"),
        """
        module WindowsCacheOverwrite
        f() = "something new"
        end # module
        """)

    ji_2, ofile_2 = Base.compilecache(Base.PkgId("WindowsCacheOverwrite"))
    @test ofile_2 == Base.ocachefile_from_cachefile(ji_2)
end

precompile_test_harness("Issue #48391") do load_path
    write(joinpath(load_path, "I48391.jl"),
        """
        module I48391
        struct SurrealFinite <: Real end
        precompile(Tuple{typeof(Base.isless), SurrealFinite, SurrealFinite})
        Base.:(<)(x::SurrealFinite, y::SurrealFinite) = "good"
        end
        """)
    ji, ofile = Base.compilecache(Base.PkgId("I48391"))
    @eval using I48391
    x = Base.invokelatest(I48391.SurrealFinite)
    @test Base.invokelatest(isless, x, x) === "good"
    @test_throws ErrorException isless(x, x)
end

empty!(Base.DEPOT_PATH)
append!(Base.DEPOT_PATH, original_depot_path)
empty!(Base.LOAD_PATH)
append!(Base.LOAD_PATH, original_load_path)
