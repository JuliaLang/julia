// This file is a part of Julia. License is MIT: http://julialang.org/license

//===----------------------------------------------------------------------===//
//
// The ABI implementation used for AArch64 targets.
//
//===----------------------------------------------------------------------===//
//
// The Procedure Call Standard can be found here:
// http://infocenter.arm.com/help/topic/com.arm.doc.ihi0055b/IHI0055B_aapcs64.pdf
//
//===----------------------------------------------------------------------===//

struct ABI_AArch64Layout : AbiLayout {

Type *get_llvm_vectype(jl_datatype_t *dt) const
{
    // Assume jl_is_datatype(dt) && !jl_is_abstracttype(dt)
    // `!dt->mutabl && dt->pointerfree && !dt->haspadding && dt->nfields > 0`
    if (dt->layout == NULL)
        return nullptr;
    size_t nfields = dt->layout->nfields;
    assert(nfields > 0);
    if (nfields < 2)
        return nullptr;
    static Type *T_vec64 = VectorType::get(T_int32, 2);
    static Type *T_vec128 = VectorType::get(T_int32, 4);
    Type *lltype;
    // Short vector should be either 8 bytes or 16 bytes.
    // Note that there are only two distinct fundamental types for
    // short vectors so we normalize them to <2 x i32> and <4 x i32>
    switch (jl_datatype_size(dt)) {
    case 8:
        lltype = T_vec64;
        break;
    case 16:
        lltype = T_vec128;
        break;
    default:
        return nullptr;
    }
    // Since `dt` is pointer free and has no padding and is 8 or 16 in size,
    // `ft0` must be concrete, immutable with no padding and we don't need
    // to check if its size is legal since it is included in
    // the homogeneity check.
    jl_datatype_t *ft0 = (jl_datatype_t*)jl_field_type(dt, 0);
    // `ft0` should be a `VecElement` type and the true element type
    // should be a `bitstype`
    if (ft0->name != jl_vecelement_typename ||
        ((jl_datatype_t*)jl_field_type(ft0, 0))->layout->nfields)
        return nullptr;
    for (size_t i = 1; i < nfields; i++) {
        if (jl_field_type(dt, i) != (jl_value_t*)ft0) {
            // Not homogeneous
            return nullptr;
        }
    }
    return lltype;
}

Type *get_llvm_fptype(jl_datatype_t *dt) const
{
    // Assume jl_is_datatype(dt) && !jl_is_abstracttype(dt)
    // `!dt->mutabl && dt->pointerfree && !dt->haspadding && dt->nfields == 0`
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
    case 16:
        lltype = T_float128;
        break;
    default:
        return nullptr;
    }
    return ((jl_floatingpoint_type && jl_is_floattype((jl_value_t*)dt)) ?
            lltype : nullptr);
}

Type *get_llvm_fp_or_vectype(jl_datatype_t *dt) const
{
    // Assume jl_is_datatype(dt) && !jl_is_abstracttype(dt)
    if (dt->mutabl || !dt->layout->pointerfree || dt->layout->haspadding)
        return nullptr;
    return dt->layout->nfields ? get_llvm_vectype(dt) : get_llvm_fptype(dt);
}

struct ElementType {
    Type *type;
    size_t sz;
    ElementType() : type(nullptr), sz(0) {};
};

// Whether a type is a homogeneous floating-point aggregates (HFA) or a
// homogeneous short-vector aggregates (HVA). Returns the element type.
// An Homogeneous Aggregate is a Composite Type where all of the Fundamental
// Data Types of the members that compose the type are the same.
// Note that it is the fundamental types that are important and not the member
// types.
bool isHFAorHVA(jl_datatype_t *dt, size_t dsz, size_t &nele, ElementType &ele) const
{
    // Assume:
    //     dt is a pointerfree type, (all members are isbits)
    //     dsz == jl_datatype_size(dt) > 0
    //     0 <= nele <= 3
    //     dt has no padding

    // We ignore zero sized member here. This isn't really consistent with
    // GCC for zero-sized array members. GCC seems to treat structs with
    // zero sized array members as non-HFA and non-HVA. Clang (3.7 and 3.8)
    // handles this slightly differently.
    // Ref https://llvm.org/bugs/show_bug.cgi?id=26162
    while (size_t nfields = jl_datatype_nfields(dt)) {
        // For composite types, find the first non zero sized member
        size_t i;
        size_t fieldsz;
        for (i = 0;i < nfields;i++) {
            if ((fieldsz = jl_field_size(dt, i))) {
                break;
            }
        }
        assert(i < nfields);
        // If there's only one non zero sized member, try again on this member
        if (fieldsz == dsz) {
            dt = (jl_datatype_t*)jl_field_type(dt, i);
            continue;
        }
        if (Type *vectype = get_llvm_vectype(dt)) {
            if ((ele.sz && dsz != ele.sz) || (ele.type && ele.type != vectype))
                return false;
            ele.type = vectype;
            ele.sz = dsz;
            nele++;
            return true;
        }
        // Otherwise, process each members
        for (;i < nfields;i++) {
            size_t fieldsz = jl_field_size(dt, i);
            if (fieldsz == 0)
                continue;
            jl_datatype_t *fieldtype = (jl_datatype_t*)jl_field_type(dt, i);
            // Check element count.
            // This needs to be done after the zero size member check
            if (nele > 3 || !isHFAorHVA(fieldtype, fieldsz, nele, ele)) {
                return false;
            }
        }
        return true;
    }
    // For bitstypes
    if (ele.sz && dsz != ele.sz)
        return false;
    Type *new_type = get_llvm_fptype(dt);
    if (new_type && (!ele.type || ele.type == new_type)) {
        ele.type = new_type;
        ele.sz = dsz;
        nele++;
        return true;
    }
    return false;
}

Type *isHFAorHVA(jl_datatype_t *dt, size_t &nele) const
{
    // Assume jl_is_datatype(dt) && !jl_is_abstracttype(dt)

    // An Homogeneous Floating-point Aggregate (HFA) is an Homogeneous Aggregate
    // with a Fundamental Data Type that is a Floating-Point type and at most
    // four uniquely addressable members.
    // An Homogeneous Short-Vector Aggregate (HVA) is an Homogeneous Aggregate
    // with a Fundamental Data Type that is a Short-Vector type and at most four
    // uniquely addressable members.
    // Maximum HFA and HVA size is 64 bytes (4 x fp128 or 16bytes vector)
    size_t dsz = jl_datatype_size(dt);
    if (dsz > 64 || !dt->layout || !dt->layout->pointerfree || dt->layout->haspadding)
        return NULL;
    nele = 0;
    ElementType eltype;
    if (isHFAorHVA(dt, dsz, nele, eltype))
        return eltype.type;
    return NULL;
}

bool needPassByRef(jl_datatype_t *dt, AttrBuilder &ab) override
{
    // B.2
    //   If the argument type is an HFA or an HVA, then the argument is used
    //   unmodified.
    size_t size;
    if (isHFAorHVA(dt, size))
        return false;
    // B.3
    //   If the argument type is a Composite Type that is larger than 16 bytes,
    //   then the argument is copied to memory allocated by the caller and the
    //   argument is replaced by a pointer to the copy.
    // We only check for the total size and not whether it is a composite type
    // since there's no corresponding C type and we just treat such large
    // bitstype as a composite type of the right size.
    return jl_datatype_size(dt) > 16;
    // B.4
    //   If the argument type is a Composite Type then the size of the argument
    //   is rounded up to the nearest multiple of 8 bytes.
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
Type *classify_arg(jl_datatype_t *dt, bool *fpreg, bool *onstack,
                   size_t *rewrite_len) const
{
    // Based on section 5.4 C of the Procedure Call Standard
    // C.1
    //   If the argument is a Half-, Single-, Double- or Quad- precision
    //   Floating-point or Short Vector Type and the NSRN is less than 8, then
    //   the argument is allocated to the least significant bits of register
    //   v[NSRN]. The NSRN is incremented by one. The argument has now been
    //   allocated.
    if (get_llvm_fp_or_vectype(dt)) {
        *fpreg = true;
        return NULL;
    }

    // C.2
    //   If the argument is an HFA or an HVA and there are sufficient
    //   unallocated SIMD and Floating-point registers (NSRN + number of
    //   members <= 8), then the argument is allocated to SIMD and
    //   Floating-point Registers (with one register per member of the HFA
    //   or HVA). The NSRN is incremented by the number of registers used.
    //   The argument has now been allocated.
    if (Type *eltype = isHFAorHVA(dt, *rewrite_len)) {
        assert(*rewrite_len > 0 && *rewrite_len <= 4);
        // HFA and HVA have <= 4 members
        *fpreg = true;
        // Rewrite to [n x eltype] where n is the number of fundamental types.
        return eltype;
    }

    // Check if the argument needs to be passed by reference. This should be
    // done before starting step C but we do this here to avoid checking for
    // HFA and HVA twice. We don't check whether it is a composite type.
    // See `needPassByRef` above.
    if (jl_datatype_size(dt) > 16) {
        *onstack = true;
        return NULL;
    }

    // C.3
    //   If the argument is an HFA or an HVA then the NSRN is set to 8 and the
    //   size of the argument is rounded up to the nearest multiple of 8 bytes.
    // C.4
    //   If the argument is an HFA, an HVA, a Quad-precision Floating-point or
    //   Short Vector Type then the NSAA is rounded up to the larger of 8 or
    //   the Natural Alignment of the argument’s type.
    // C.5
    //   If the argument is a Half- or Single- precision Floating Point type,
    //   then the size of the argument is set to 8 bytes. The effect is as if
    //   the argument had been copied to the least significant bits of a 64-bit
    //   register and the remaining bits filled with unspecified values.
    // C.6
    //   If the argument is an HFA, an HVA, a Half-, Single-, Double- or
    //   Quad- precision Floating-point or Short Vector Type, then the argument
    //   is copied to memory at the adjusted NSAA. The NSAA is incremented
    //   by the size of the argument. The argument has now been allocated.
    // <already included in the C.2 case above>
    // C.7
    //   If the argument is an Integral or Pointer Type, the size of the
    //   argument is less than or equal to 8 bytes and the NGRN is less than 8,
    //   the argument is copied to the least significant bits in x[NGRN].
    //   The NGRN is incremented by one. The argument has now been allocated.
    // Here we treat any bitstype of the right size as integers or pointers
    // This is needed for types like Cstring which should be treated as
    // pointers. We don't need to worry about floating points here since they
    // are handled above.
    if (jl_is_immutable(dt) && jl_datatype_nfields(dt) == 0 &&
        (jl_datatype_size(dt) == 1 || jl_datatype_size(dt) == 2 ||
         jl_datatype_size(dt) == 4 || jl_datatype_size(dt) == 8 ||
         jl_datatype_size(dt) == 16))
        return NULL;

    // C.8
    //   If the argument has an alignment of 16 then the NGRN is rounded up to
    //   the next even number.
    // C.9
    //   If the argument is an Integral Type, the size of the argument is equal
    //   to 16 and the NGRN is less than 7, the argument is copied to x[NGRN]
    //   and x[NGRN+1]. x[NGRN] shall contain the lower addressed double-word
    //   of the memory representation of the argument. The NGRN is incremented
    //   by two. The argument has now been allocated.
    // <merged into C.7 above>
    // C.10
    //   If the argument is a Composite Type and the size in double-words of
    //   the argument is not more than 8 minus NGRN, then the argument is
    //   copied into consecutive general-purpose registers, starting at x[NGRN].
    //   The argument is passed as though it had been loaded into the registers
    //   from a double-word-aligned address with an appropriate sequence of LDR
    //   instructions loading consecutive registers from memory (the contents of
    //   any unused parts of the registers are unspecified by this standard).
    //   The NGRN is incremented by the number of registers used. The argument
    //   has now been allocated.
    // We don't check for composite types here since the ones that have
    // corresponding C types are already handled and we just treat the ones
    // with weird size as a black box composite type.
    // The type can fit in 8 x 8 bytes since it is handled by
    // need_pass_by_ref otherwise.
    // 0-size types (Void) won't be rewritten and that is what we want
    assert(jl_datatype_size(dt) <= 16); // Should be pass by reference otherwise
    *rewrite_len = (jl_datatype_size(dt) + 7) >> 3;
    // Rewrite to [n x Int64] where n is the **size in dword**
    return jl_datatype_size(dt) ? T_int64 : NULL;

    // C.11
    //   The NGRN is set to 8.
    // C.12
    //   The NSAA is rounded up to the larger of 8 or the Natural Alignment
    //   of the argument’s type.
    // C.13
    //   If the argument is a composite type then the argument is copied to
    //   memory at the adjusted NSAA. The NSAA is incremented by the size of
    //   the argument. The argument has now been allocated.
    // <handled by C.10 above>
    // C.14
    //   If the size of the argument is less than 8 bytes then the size of the
    //   argument is set to 8 bytes. The effect is as if the argument was
    //   copied to the least significant bits of a 64-bit register and the
    //   remaining bits filled with unspecified values.
    // C.15
    //   The argument is copied to memory at the adjusted NSAA. The NSAA is
    //   incremented by the size of the argument. The argument has now been
    //   allocated.
    // <handled by C.10 above>
}

bool use_sret(jl_datatype_t *dt) override
{
    // Section 5.5
    // If the type, T, of the result of a function is such that
    //
    //     void func(T arg)
    //
    // would require that arg be passed as a value in a register (or set of
    // registers) according to the rules in section 5.4 Parameter Passing,
    // then the result is returned in the same registers as would be used for
    // such an argument.
    bool fpreg = false;
    bool onstack = false;
    size_t rewrite_len = 0;
    classify_arg(dt, &fpreg, &onstack, &rewrite_len);
    return onstack;
}

Type *preferred_llvm_type(jl_datatype_t *dt, bool isret) const override
{
    if (Type *fptype = get_llvm_fp_or_vectype(dt))
        return fptype;
    bool fpreg = false;
    bool onstack = false;
    size_t rewrite_len = 0;
    if (Type *rewrite_ty = classify_arg(dt, &fpreg, &onstack, &rewrite_len))
        return ArrayType::get(rewrite_ty, rewrite_len);
    return NULL;
}

};
