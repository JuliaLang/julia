# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck %s

## Test that TBAA and alias.scope/noalias metadata are correctly separated:
## - TBAA encodes type information (struct-path, per-type)
## - alias.scope/noalias encodes memory regions (stack, data, gcframe, constant)
## - GenericMemory metadata loads get invariant.load
## - All Julia loads get !noundef

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# CHECK-LABEL: @julia_memory_metadata_invariant
# Test that GenericMemory length loads get !invariant.load
function memory_metadata_invariant(m::Memory{Float64})
    return length(m)
# CHECK: load {{.*}}!invariant.load
end

# CHECK-LABEL: @julia_noundef_on_loads
# Test that decorated loads get !noundef metadata
function noundef_on_loads(x::Vector{Float64})
    return x[1]
# CHECK: load double, {{.*}}!noundef
end

# Verify jtbaa_arraybuf is under jtbaa_data (not a separate tbaa_array subtree)
# CHECK-DAG: !{!"jtbaa_arraybuf", [[DATA_SCALAR:![0-9]+]], i64 0}
# CHECK-DAG: [[DATA_SCALAR]] = !{!"jtbaa_data", {{![0-9]+}}, i64 0}

# Verify jnoalias_stack and jnoalias_data are distinct scopes in the same domain
# CHECK-DAG: !{!"jnoalias_stack", [[JNOALIAS_DOMAIN:![0-9]+]]}
# CHECK-DAG: !{!"jnoalias_data", [[JNOALIAS_DOMAIN]]}
# CHECK-DAG: [[JNOALIAS_DOMAIN]] = !{!"jnoalias"}

emit(memory_metadata_invariant, Memory{Float64})
emit(noundef_on_loads, Vector{Float64})
