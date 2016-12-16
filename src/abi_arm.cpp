// This file is a part of Julia. License is MIT: http://julialang.org/license

//===----------------------------------------------------------------------===//
//
// The ABI implementation used for ARM targets.
//
//===----------------------------------------------------------------------===//
//
// The Procedure Call Standard can be found here:
// http://infocenter.arm.com/help/topic/com.arm.doc.ihi0042f/IHI0042F_aapcs.pdf
//
//===----------------------------------------------------------------------===//

#if defined _CPU_ARM_
#ifndef __ARM_EABI__
#  error "the Julia ARM ABI implementation only supports EABI"
#endif

#ifndef __ARM_PCS_VFP
#  error "the Julia ARM ABI implementation requires VFP support"
#endif
#endif

struct ABI_ARMLayout : AbiLayout {

bool needPassByRef(jl_datatype_t *dt, AttrBuilder &ab) override
{
    return false;
}

Type *get_llvm_fptype(jl_datatype_t *dt) const
{
    // Assume jl_is_datatype(dt) && !jl_is_abstracttype(dt)
    if (dt->mutabl || jl_datatype_nfields(dt) != 0)
        return NULL;
    Type *lltype;
    // Check size first since it's cheaper.
    switch (jl_datatype_size(dt)) {
    case 2:
        lltype = T_float16;
        break;
    case 4:
        lltype = T_float32;
        break;
    case 8:
        lltype = T_float64;
        break;
    default:
        return NULL;
    }
    return ((jl_floatingpoint_type && jl_is_floattype((jl_value_t*)dt)) ?
            lltype : NULL);
}

// Check whether a type contained by a candidate homogeneous aggregate is valid
// fundamental type.
//
// Returns the corresponding LLVM type.
Type *isLegalHAType(jl_datatype_t *dt) const
{
    // single- or double-precision floating-point type
    if (Type *fp = get_llvm_fptype(dt))
        return fp;

    // NOT SUPPORTED: 64- or 128-bit containerized vectors

    return NULL;
}

// Check whether a type is a legal homogeneous aggregate.
// Returns the number of fundamental members.
//
// Legality of the HA is determined by a nonzero return value.
// In case of a non-legal HA, the value of 'base' is undefined.
size_t isLegalHA(jl_datatype_t *dt, Type *&base) const
{
    // Homogeneous aggregates are only used for VFP registers,
    // so use that definition of legality (section 6.1.2.1)

    if (jl_is_structtype(dt)) {
        // Fast path checks before descending the type hierarchy
        // (4 x 128b vector == 64B max size)
        if (jl_datatype_size(dt) > 64 || !dt->layout->pointerfree || dt->layout->haspadding)
            return 0;

        base = NULL;
        size_t total_members = 0;

        size_t parent_members = jl_datatype_nfields(dt);
        for (size_t i = 0; i < parent_members; ++i) {
            jl_datatype_t *fdt = (jl_datatype_t*)jl_field_type(dt,i);

            Type *T = isLegalHAType(fdt);
            if (T)
                total_members++;
            else if (size_t field_members = isLegalHA(fdt, T))
                // recursive application (expanding nested composite types)
                total_members += field_members;
            else
                return 0;

            if (!base)
                base = T;
            else if (base != T)
                return 0;
        }

        // ... with one to four Elements.
        if (total_members < 1 || total_members > 4)
            return 0;

        return total_members;
    }

    return 0;
}

// Determine if an argument can be passed through a coprocessor register.
//
// All the out parameters should be default to `false`.
void classify_cprc(jl_datatype_t *dt, bool *vfp) const
{
    // Based on section 6.1 of the Procedure Call Standard

    // VFP: 6.1.2.1
    // - A half-precision floating-point type.
    // - A single-precision floating-point type.
    // - A double-precision floating-point type.
    if (get_llvm_fptype(dt)) {
        *vfp = true;
        return;
    }

    // NOT SUPPORTED: A 64-bit or 128-bit containerized vector type.

    // - A Homogeneous Aggregate
    Type *base = NULL;
    if (isLegalHA(dt, base)) {
        *vfp = true;
        return;
    }
}

void classify_return_arg(jl_datatype_t *dt, bool *reg,
                         bool *onstack, bool *need_rewrite) const
{
    // Based on section 5.4 of the Procedure Call Standard

    // VFP standard variant: see 6.1.2.2
    //   Any result whose type would satisfy the conditions for a VFP CPRC is
    //   returned in the appropriate number of consecutive VFP registers
    //   starting with the lowest numbered register (s0, d0, q0).
    classify_cprc(dt, reg);
    if (*reg)
        return;

    // - A Half-precision Floating Point Type is returned in the least
    //   significant 16 bits of r0.
    if (dt == jl_float16_type) {
        *reg = true;
        return;
    }

    // - A Fundamental Data Type that is smaller than 4 bytes is zero- or
    //   sign-extended to a word and returned in r0.
    // - A double-word sized Fundamental Data Type (e.g., long long, double and
    //   64-bit containerized vectors) is returned in r0 and r1.
    // - A word-sized Fundamental Data Type (eg., int, float) is returned in r0.
    // NOTE: assuming "fundamental type" == jl_is_bitstype, might need exact def
    if (jl_is_bitstype(dt) && jl_datatype_size(dt) <= 8) {
        *reg = true;
        return;
    }

    // If we ever support containerized vectors on an ARMv7 without VFP,
    // these can be returned in r0-r3 as well.

    // NOTE: we don't check for jl_is_structtype below, because at this point
    //       everything will be rewritten to look like a composite aggregate
    *need_rewrite = true;

    // - A Composite Type not larger than 4 bytes is returned in r0. The format
    //   is as if the result had been stored in memory at a word-aligned address
    //   and then loaded into r0 with an LDR instruction. Any bits in r0 that
    //   lie outside the bounds of the result have unspecified values.
    // - A Composite Type larger than 4 bytes, or whose size cannot be
    //   determined statically by both caller and callee, is stored in memory at
    //   an address passed as an extra argument when the function was called
    //   (§5.5, rule A.4). The memory to be used for the result may be modified
    //   at any point during the function call.
    if (jl_datatype_size(dt) <= 4)
        *reg = true;
    else
        *onstack = true;
}

bool use_sret(jl_datatype_t *dt) override
{
    bool reg = false;
    bool onstack = false;
    bool need_rewrite = false;
    classify_return_arg(dt, &reg, &onstack, &need_rewrite);

    return onstack;
}

// Determine which kind of register the argument will be passed in and
// if the argument has to be passed on stack (including by reference).
//
// If the argument should be passed in SIMD and floating-point registers,
// we may need to rewrite the argument types to [n x ftype].
// If the argument should be passed in general purpose registers, we may need
// to rewrite the argument types to [n x i64].
//
// If the argument has to be passed on stack, we need to use sret.
//
// All the out parameters should be default to `false`.
void classify_arg(jl_datatype_t *dt, bool *reg,
                  bool *onstack, bool *need_rewrite) const
{
    // Based on section 5.5 of the Procedure Call Standard

    // C.1.cp
    //   If the argument is a CPRC and there are sufficient unallocated
    //   co-processor registers of the appropriate class, the argument is
    //   allocated to co-processor registers.
    classify_cprc(dt, reg);
    if (*reg)
        return;

    // Handle fundamental types
    if (jl_is_bitstype(dt) && jl_datatype_size(dt) <= 8) {
        *reg = true;
        return;
    }

    *need_rewrite = true;
}

Type *preferred_llvm_type(jl_datatype_t *dt, bool isret) const override
{
    if (Type *fptype = get_llvm_fptype(dt))
        return fptype;

    bool reg = false;
    bool onstack = false;
    bool need_rewrite = false;
    if (isret)
        classify_return_arg(dt, &reg, &onstack, &need_rewrite);
    else
        classify_arg(dt, &reg, &onstack, &need_rewrite);

    if (!need_rewrite)
        return NULL;

    // Based on section 4 of the Procedure Call Standard

    // If some type is illegal and needs to be rewritten,
    // represent it as an aggregate composite type.

    // 4.3.1: aggregates
    // - The alignment of an aggregate shall be the alignment of its
    //   most-aligned component.
    // - The size of an aggregate shall be the smallest multiple of its
    //   alignment that is sufficient to hold all of its members when they are
    //   laid out according to these rules.
    // 5.5 B.5
    //   For a Composite Type, the alignment of the copy will have 4-byte
    //   alignment if its natural alignment is <= 4 and 8-byte alignment if
    //   its natural alignment is >= 8
    size_t align = dt->layout->alignment;
    if (align < 4)
        align = 4;
    if (align > 8)
        align = 8;

    Type *T = Type::getIntNTy(jl_LLVMContext, align*8);
    return ArrayType::get(T, (jl_datatype_size(dt) + align - 1) / align);
}

};
