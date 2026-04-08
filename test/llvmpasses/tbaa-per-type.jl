# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck %s

## Test that per-type TBAA metadata is emitted for concrete struct types
## (including struct-path TBAA for field access) and array element types.

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# CHECK-LABEL: @julia_different_mutable_types
mutable struct MutFoo1; x::Int; end
mutable struct MutBar1; y::Int; end
function different_mutable_types(a::MutFoo1, b::MutBar1, val::Int)
    a.x = val
    return b.y
# Struct-path access tags: {struct_type, field_scalar, offset}
# CHECK: store i64 %{{.*}}, {{.*}} !tbaa [[TBAA_FOO_X:![0-9]+]]
# CHECK: load i64, {{.*}} !tbaa [[TBAA_BAR_Y:![0-9]+]]
end

# CHECK-LABEL: @julia_array_different_eltypes
function array_different_eltypes(a::Vector{Int64}, b::Vector{Float64}, val::Int64)
    a[1] = val
    return b[1]
# CHECK: store i64 %{{.*}}, {{.*}} !tbaa [[TBAA_ARRAYBUF_INT:![0-9]+]]
# CHECK: load double, {{.*}} !tbaa [[TBAA_ARRAYBUF_FLOAT:![0-9]+]]
end

# Verify struct-path TBAA: field access tags reference struct type nodes
# MutFoo1.x: {struct_MutFoo1, Int64_scalar, offset 0}
# CHECK-DAG: [[TBAA_FOO_X]] = !{[[STRUCT_FOO:![0-9]+]], [[INT_SCALAR:![0-9]+]], i64 0}
# CHECK-DAG: [[STRUCT_FOO]] = !{!"jtbaa_struct_MutFoo1", [[INT_SCALAR]], i64 0}
# CHECK-DAG: [[INT_SCALAR]] = !{!"jtbaa_Int64", [[IMMUT_SCALAR:![0-9]+]], i64 0}

# MutBar1.y: {struct_MutBar1, Int64_scalar, offset 0}
# CHECK-DAG: [[TBAA_BAR_Y]] = !{[[STRUCT_BAR:![0-9]+]], [[INT_SCALAR]], i64 0}
# CHECK-DAG: [[STRUCT_BAR]] = !{!"jtbaa_struct_MutBar1", [[INT_SCALAR]], i64 0}

# Array element types are children of jtbaa_arraybuf
# CHECK-DAG: [[TBAA_ARRAYBUF_INT]] = !{[[ARRAYBUF_INT_SCALAR:![0-9]+]], [[ARRAYBUF_INT_SCALAR]], i64 0}
# CHECK-DAG: [[ARRAYBUF_INT_SCALAR]] = !{!"jtbaa_arraybuf_Int64", [[ARRAYBUF_SCALAR:![0-9]+]], i64 0}
# CHECK-DAG: [[ARRAYBUF_SCALAR]] = !{!"jtbaa_arraybuf", [[DATA_SCALAR:![0-9]+]], i64 0}

# CHECK-DAG: [[TBAA_ARRAYBUF_FLOAT]] = !{[[ARRAYBUF_FLOAT_SCALAR:![0-9]+]], [[ARRAYBUF_FLOAT_SCALAR]], i64 0}
# CHECK-DAG: [[ARRAYBUF_FLOAT_SCALAR]] = !{!"jtbaa_arraybuf_Float64", [[ARRAYBUF_SCALAR]], i64 0}

emit(different_mutable_types, MutFoo1, MutBar1, Int)
emit(array_different_eltypes, Vector{Int64}, Vector{Float64}, Int64)
