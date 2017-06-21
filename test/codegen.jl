# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests for codegen and optimizations

const opt_level = Base.JLOptions().opt_level

# `_dump_function` might be more efficient but it doesn't really matter here...
get_llvm(f::ANY, t::ANY, strip_ir_metadata=true, dump_module=false) =
    sprint(code_llvm, f, t, strip_ir_metadata, dump_module)

if opt_level > 0
    # Make sure getptls call is removed at IR level with optimization on
    @test !contains(get_llvm(identity, Tuple{String}), " call ")
end
