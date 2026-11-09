# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t && cat %t/f.ll | FileCheck %s

# Checks the DWARF type descriptions emitted for unboxed values at -g2:
# primitive types carry a real encoding, structs list their fields as named
# members with offsets, and type names include the type parameters.

using InteractiveUtils

dir = ARGS[1]
rm(dir, force=true, recursive=true)
mkdir(dir)

struct Pose
    x::Float64
    n::Int32
    tag::String
end

struct MaybeInt
    u::Union{Int64, Nothing}
end

@noinline function f(p::Pose, c::Complex{Float64}, b::Bool, ptr::Ptr{UInt8},
                     t::Tuple{Int64, Float32},
                     nt::NamedTuple{(:a, :b), Tuple{Int8, UInt16}}, m::MaybeInt)
    return p.x + c.re + t[2]
end

open(joinpath(dir, "f.ll"), "w") do io
    params = Base.CodegenParams(debug_info_level=Cint(2))
    code_llvm(io, f, (Pose, Complex{Float64}, Bool, Ptr{UInt8}, Tuple{Int64, Float32},
                      NamedTuple{(:a, :b), Tuple{Int8, UInt16}}, MaybeInt);
              raw=true, dump_module=true, debuginfo=:source, optimize=false, params=params)
end

# Primitive types get a DWARF encoding matching their Julia semantics.
# CHECK-DAG: [[F64:![0-9]+]] = !DIBasicType(name: "Float64", size: 64, encoding: DW_ATE_float)
# CHECK-DAG: [[F32:![0-9]+]] = !DIBasicType(name: "Float32", size: 32, encoding: DW_ATE_float)
# CHECK-DAG: [[I64:![0-9]+]] = !DIBasicType(name: "Int64", size: 64, encoding: DW_ATE_signed)
# CHECK-DAG: [[I32:![0-9]+]] = !DIBasicType(name: "Int32", size: 32, encoding: DW_ATE_signed)
# CHECK-DAG: [[I8:![0-9]+]] = !DIBasicType(name: "Int8", size: 8, encoding: DW_ATE_signed)
# CHECK-DAG: [[U16:![0-9]+]] = !DIBasicType(name: "UInt16", size: 16, encoding: DW_ATE_unsigned)
# CHECK-DAG: [[U8:![0-9]+]] = !DIBasicType(name: "UInt8", size: 8, encoding: DW_ATE_unsigned)
# CHECK-DAG: !DIBasicType(name: "Bool", size: 8, encoding: DW_ATE_boolean)

# Boxed fields are jl_value_t*; Ptr{T} is a named pointer type.
# CHECK-DAG: [[JLV:![0-9]+]] = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: {{![0-9]+}}, size: 64, align: 64)
# CHECK-DAG: !DIDerivedType(tag: DW_TAG_pointer_type, name: "Ptr{UInt8}", baseType: [[U8]], size: 64, align: 64)

# Structs: full name, and one DW_TAG_member per field with name and offset.
# CHECK-DAG: [[POSE:![0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Main.Pose", size: 192, align: 64, elements: [[POSE_ELTS:![0-9]+]], runtimeLang: DW_LANG_Julia
# CHECK-DAG: [[POSE_ELTS]] = !{[[POSE_X:![0-9]+]], [[POSE_N:![0-9]+]], [[POSE_TAG:![0-9]+]]}
# CHECK-DAG: [[POSE_X]] = !DIDerivedType(tag: DW_TAG_member, name: "x", scope: [[POSE]], baseType: [[F64]], size: 64, align: 64)
# CHECK-DAG: [[POSE_N]] = !DIDerivedType(tag: DW_TAG_member, name: "n", scope: [[POSE]], baseType: [[I32]], size: 32, align: 32, offset: 64)
# CHECK-DAG: [[POSE_TAG]] = !DIDerivedType(tag: DW_TAG_member, name: "tag", scope: [[POSE]], baseType: [[JLV]], size: 64, align: 64, offset: 128)

# CHECK-DAG: [[CPLX:![0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Base.Complex{Float64}", size: 128, align: 64, elements: [[CPLX_ELTS:![0-9]+]], runtimeLang: DW_LANG_Julia
# CHECK-DAG: [[CPLX_ELTS]] = !{[[CPLX_RE:![0-9]+]], [[CPLX_IM:![0-9]+]]}
# CHECK-DAG: [[CPLX_RE]] = !DIDerivedType(tag: DW_TAG_member, name: "re", scope: [[CPLX]], baseType: [[F64]], size: 64, align: 64)
# CHECK-DAG: [[CPLX_IM]] = !DIDerivedType(tag: DW_TAG_member, name: "im", scope: [[CPLX]], baseType: [[F64]], size: 64, align: 64, offset: 64)

# Tuples number their fields from 1; NamedTuples use the declared names.
# CHECK-DAG: [[TUP:![0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Tuple{Int64, Float32}", size: 128, align: 64, elements: [[TUP_ELTS:![0-9]+]], runtimeLang: DW_LANG_Julia
# CHECK-DAG: [[TUP_ELTS]] = !{[[TUP_1:![0-9]+]], [[TUP_2:![0-9]+]]}
# CHECK-DAG: [[TUP_1]] = !DIDerivedType(tag: DW_TAG_member, name: "1", scope: [[TUP]], baseType: [[I64]], size: 64, align: 64)
# CHECK-DAG: [[TUP_2]] = !DIDerivedType(tag: DW_TAG_member, name: "2", scope: [[TUP]], baseType: [[F32]], size: 32, align: 32, offset: 64)

# CHECK-DAG: [[NT:![0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "NamedTuple{(:a, :b), Tuple{Int8, UInt16}}", size: 32, align: 16, elements: [[NT_ELTS:![0-9]+]], runtimeLang: DW_LANG_Julia
# CHECK-DAG: [[NT_ELTS]] = !{[[NT_A:![0-9]+]], [[NT_B:![0-9]+]]}
# CHECK-DAG: [[NT_A]] = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: [[NT]], baseType: [[I8]], size: 8, align: 8)
# CHECK-DAG: [[NT_B]] = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: [[NT]], baseType: [[U16]], size: 16, align: 16, offset: 16)

# An inline-allocated Union field is exposed as its raw bytes (payload + selector).
# CHECK-DAG: [[MI:![0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Main.MaybeInt", size: 128, align: 64, elements: [[MI_ELTS:![0-9]+]], runtimeLang: DW_LANG_Julia
# CHECK-DAG: [[MI_ELTS]] = !{[[MI_U:![0-9]+]]}
# CHECK-DAG: [[MI_U]] = !DIDerivedType(tag: DW_TAG_member, name: "u", scope: [[MI]], baseType: [[MI_BYTES:![0-9]+]], size: 72, align: 8)
# CHECK-DAG: [[MI_BYTES]] = !DICompositeType(tag: DW_TAG_array_type, baseType: [[U8]], size: 72, align: 8, elements: [[MI_SUB:![0-9]+]])
# CHECK-DAG: [[MI_SUB]] = !{[[MI_RANGE:![0-9]+]]}
# CHECK-DAG: [[MI_RANGE]] = !DISubrange(count: 9, lowerBound: 0)
