# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, UUIDs

# This file is loaded as part of special_loading.jl
Base.compilecache(Base.PkgId(UUID(0x95defb8a_f82d_44d7_b2c9_37d658f648c1), "CompilerLoadingTest"))

using CompilerLoadingTest
@test Base.maybe_loaded_precompile(Base.PkgId(UUID(0x807dbc54_b67e_4c79_8afb_eafe4df6f2e1), "Compiler"), Base.module_build_id(Base.Compiler)) !== nothing

using Compiler
@test CompilerLoadingTest.Compiler === Compiler === Base.Compiler
