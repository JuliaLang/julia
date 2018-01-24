# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base

# initialize some necessary state that currently isn't
# handled particularly well automatically in build processes
Base.reinit_stdio()
Sys._early_init()
Base.init_depot_path(Sys.BINDIR)
Base.init_load_path(Sys.BINDIR)
push!(Base.LOAD_CACHE_PATH, abspath(Sys.BINDIR, Base.PRIVATE_LIBDIR))
Base.global_logger(Base.SimpleLogger(STDERR, Base.CoreLogging.Debug)) # be somewhat noisy about package loading status

# load some stdlib packages but don't put their names in Main
Base.require(Base, :Base64)
Base.require(Base, :CRC32c)
Base.require(Base, :Dates)
Base.require(Base, :DelimitedFiles)
Base.require(Base, :Serialization)
Base.require(Base, :Distributed)
Base.require(Base, :FileWatching)
Base.require(Base, :Future)
Base.require(Base, :IterativeEigensolvers)
Base.require(Base, :Libdl)
Base.require(Base, :LinearAlgebra)
Base.require(Base, :Logging)
Base.require(Base, :Mmap)
Base.require(Base, :Printf)
Base.require(Base, :Profile)
Base.require(Base, :Random)
Base.require(Base, :SharedArrays)
Base.require(Base, :SparseArrays)
Base.require(Base, :SuiteSparse)
Base.require(Base, :Test)
Base.require(Base, :Unicode)
Base.require(Base, :REPL)

@eval Base begin
    @deprecate_binding Test root_module(Base, :Test) true ", run `using Test` instead"
    @deprecate_binding Mmap root_module(Base, :Mmap) true ", run `using Mmap` instead"
    @deprecate_binding Profile root_module(Base, :Profile) true ", run `using Profile` instead"
    @deprecate_binding Dates root_module(Base, :Dates) true ", run `using Dates` instead"
    @deprecate_binding Distributed root_module(Base, :Distributed) true ", run `using Distributed` instead"
    @deprecate_binding Random root_module(Base, :Random) true ", run `using Random` instead"
    @deprecate_binding Serializer root_module(Base, :Serialization) true ", run `using Serialization` instead"
    @deprecate_binding Libdl root_module(Base, :Libdl) true ", run `using Libdl` instead"

    # PR #25249
    @deprecate_binding SparseArrays root_module(Base, :SparseArrays) true ", run `using SparseArrays` instead"
    @deprecate_binding(AbstractSparseArray, root_module(Base, :SparseArrays).AbstractSparseArray, true,
        ", run `using SparseArrays` to load sparse array functionality")
    @deprecate_binding(AbstractSparseMatrix, root_module(Base, :SparseArrays).AbstractSparseMatrix, true,
        ", run `using SparseArrays` to load sparse array functionality")
    @deprecate_binding(AbstractSparseVector, root_module(Base, :SparseArrays).AbstractSparseVector, true,
        ", run `using SparseArrays` to load sparse array functionality")
    @deprecate_binding(SparseMatrixCSC, root_module(Base, :SparseArrays).SparseMatrixCSC, true,
        ", run `using SparseArrays` to load sparse array functionality")
    @deprecate_binding(SparseVector, root_module(Base, :SparseArrays).SparseVector, true,
        ", run `using SparseArrays` to load sparse array functionality")

    # PR #25571
    @deprecate_binding LinAlg root_module(Base, :LinearAlgebra) true ", run `using LinearAlgebra` instead"
    @deprecate_binding(I, root_module(Base, :LinearAlgebra).I, true,
        ", run `using LinearAlgebra` to load linear algebra functionality.")

    # PR 25544
    @deprecate_binding REPL            root_module(Base, :REPL)                 true ", run `using REPL` instead"
    @deprecate_binding LineEdit        root_module(Base, :REPL).LineEdit        true ", use `REPL.LineEdit` instead"
    @deprecate_binding REPLCompletions root_module(Base, :REPL).REPLCompletions true ", use `REPL.REPLCompletions` instead"
    @deprecate_binding Terminals       root_module(Base, :REPL).Terminals       true ", use `REPL.Terminals` instead"
end

empty!(DEPOT_PATH)
empty!(LOAD_PATH)

Base.isfile("userimg.jl") && Base.include(Main, "userimg.jl")

Base.include(Base, "precompile.jl")
Base.global_logger(Base.SimpleLogger())
