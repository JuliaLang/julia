// This file is a part of Julia. License is MIT: https://julialang.org/license

//===----------------------------------------------------------------------===//
//
// The ABI implementation used for RISC-V targets.
//
//===----------------------------------------------------------------------===//
//
// The Procedure Call Standard can be found here:
// https://github.com/riscv-non-isa/riscv-elf-psabi-doc/blob/master/riscv-cc.adoc
//
// This code is based on:
// - The Rust implementation:
//    https://github.com/rust-lang/rust/blob/master/compiler/rustc_target/src/abi/call/riscv.rs
// - The LLVM RISC-V backend:
//   https://github.com/llvm/llvm-project/blob/78533528cf5ed04ac78722afff7c9f2f91aa8359/llvm/lib/Target/RISCV/RISCVISelLowering.cpp#L10865
//
//===----------------------------------------------------------------------===//


struct ABI_RiscvLayout : AbiLayout {

static const size_t XLen = 8;
static const size_t FLen = 8;
static const int NumArgGPRs = 8;
static const int NumArgFPRs = 8;

// available register num is needed to determine if fp pair or int-fp pair in a struct should be unpacked
// WARN: with this, use_sret must only be called once before the next
// needPassByRef call, otherwise avail_gprs is wrong
int avail_gprs, avail_fprs;

// preferred type is determined in the same time of use_sret & needPassByRef
// cache it here to avoid computing it again in preferred_llvm_type
Type *cached_llvmtype = NULL;

ABI_RiscvLayout() : avail_gprs(NumArgGPRs), avail_fprs(NumArgFPRs) {}

enum RegPassKind { UNKNOWN = 0, INTEGER = 1, FLOAT = 2 };

struct ElementType {
    RegPassKind type;
    jl_datatype_t *dt;
    ElementType() : type(RegPassKind::UNKNOWN), dt(NULL) {};
};

bool is_floattype(jl_datatype_t *dt) const
{
    return dt == jl_float16_type || dt == jl_float32_type || dt == jl_float64_type;
}

Type *get_llvm_fptype(jl_datatype_t *dt, LLVMContext &ctx) const
{
    assert(is_floattype(dt));
    switch (jl_datatype_size(dt)) {
    case 2: return Type::getHalfTy(ctx);
    case 4: return Type::getFloatTy(ctx);
    case 8: return Type::getDoubleTy(ctx);
    case 16: return Type::getFP128Ty(ctx);
    default: assert(0 && "abi_riscv: unsupported floating point type"); return NULL;
    }
}

// for primitive types that can be passed as integer
// includes integer, bittypes, pointer
Type *get_llvm_inttype(jl_datatype_t *dt, LLVMContext &ctx) const
{
    assert(jl_is_primitivetype(dt));
    // XXX: without Zfh, Float16 is passed in integer registers
    if (dt == jl_float16_type)
        return Type::getInt32Ty(ctx);
    assert(!is_floattype(dt));
    if (dt == jl_bool_type)
        return getInt8Ty(ctx);
    if (dt == jl_int32_type)
        return getInt32Ty(ctx);
    if (dt == jl_int64_type)
        return getInt64Ty(ctx);
    int nb = jl_datatype_size(dt);
    return Type::getIntNTy(ctx, nb * 8);
}

bool should_use_fp_conv(jl_datatype_t *dt, ElementType &ele1, ElementType &ele2) const
{
    if (jl_is_primitivetype(dt)) {
        size_t dsz = jl_datatype_size(dt);
        if (dsz > FLen) {
            return false;
        }
        if (is_floattype(dt)) {
            if (ele1.type == RegPassKind::UNKNOWN) {
                ele1.type = RegPassKind::FLOAT;
                ele1.dt = dt;
            }
            else if (ele2.type == RegPassKind::UNKNOWN) {
                ele2.type = RegPassKind::FLOAT;
                ele2.dt = dt;
            }
            else {
                // 3 elements not eligible, must be a pair
                return false;
            }
        }
        // integer or pointer type or bitstypes
        else {
            if (ele1.type == RegPassKind::UNKNOWN) {
                ele1.type = RegPassKind::INTEGER;
                ele1.dt = dt;
            }
            else if (ele1.type == RegPassKind::INTEGER) {
                // two integers not eligible
                return false;
            }
            // ele1.type == RegPassKind::FLOAT
            else {
                if (ele2.type == RegPassKind::UNKNOWN) {
                    ele2.type = RegPassKind::INTEGER;
                    ele2.dt = dt;
                }
                else {
                    // 3 elements not eligible, must be a pair
                    return false;
                }
            }
        }
    }
    else { // aggregates
        while (size_t nfields = jl_datatype_nfields(dt)) {
            size_t i;
            size_t fieldsz;
            for (i = 0; i < nfields; i++) {
                if ((fieldsz = jl_field_size(dt, i))) {
                    break;
                }
            }
            assert(i < nfields);
            // If there's only one non zero sized member, try again on this member
            if (fieldsz == jl_datatype_size(dt)) {
                dt = (jl_datatype_t *)jl_field_type(dt, i);
                if (!jl_is_datatype(dt)) // could be inline union #46787
                    return false;
                continue;
            }
            for (; i < nfields; i++) {
                size_t fieldsz = jl_field_size(dt, i);
                if (fieldsz == 0)
                    continue;
                jl_datatype_t *fieldtype = (jl_datatype_t *)jl_field_type(dt, i);
                if (!jl_is_datatype(dt)) // could be inline union
                    return false;
                // This needs to be done after the zero size member check
                if (ele2.type != RegPassKind::UNKNOWN) {
                    // we already have a pair and can't accept more elements
                    return false;
                }
                if (!should_use_fp_conv(fieldtype, ele1, ele2)) {
                    return false;
                }
            }
            break;
        }
    }
    // Tuple{Int,} can reach here as well, but doesn't really hurt
    return true;
}

Type *get_llvm_inttype_byxlen(size_t xlen, LLVMContext &ctx) const
{
    if (xlen == 8) {
        return getInt64Ty(ctx);
    }
    else if (xlen == 4) {
        return getInt32Ty(ctx);
    }
    else {
        assert(0 && "abi_riscv: unsupported xlen");
        return NULL;
    }
}

Type *classify_arg(jl_datatype_t *ty, int &avail_gprs, int &avail_fprs, bool &onstack,
                   LLVMContext &ctx) const
{
    onstack = false;
    if (ty == jl_nothing_type) {
        return NULL;
    }
    ElementType ele1, ele2;
    if (should_use_fp_conv(ty, ele1, ele2)) {
        if (ele1.type == RegPassKind::FLOAT) {
            if (ele2.type == RegPassKind::FLOAT) {
                if (avail_fprs >= 2) {
                    avail_fprs -= 2;
                    SmallVector<Type *, 2> eles;
                    eles.push_back(get_llvm_fptype(ele1.dt, ctx));
                    eles.push_back(get_llvm_fptype(ele2.dt, ctx));
                    return StructType::get(ctx, eles);
                }
            }
            else if (ele2.type == RegPassKind::INTEGER) {
                if (avail_fprs >= 1 && avail_gprs >= 1) {
                    avail_fprs -= 1;
                    avail_gprs -= 1;
                    SmallVector<Type *, 2> eles;
                    eles.push_back(get_llvm_fptype(ele1.dt, ctx));
                    eles.push_back(get_llvm_inttype(ele2.dt, ctx));
                    return StructType::get(ctx, eles);
                }
            }
            else {
                // A struct containing just one floating-point real is passed
                // as though it were a standalone floating-point real.
                if (avail_fprs >= 1) {
                    avail_fprs -= 1;
                    return get_llvm_fptype(ele1.dt, ctx);
                }
            }
        }
        else if (ele1.type == RegPassKind::INTEGER) {
            if (ele2.type == RegPassKind::FLOAT) {
                if (avail_fprs >= 1 && avail_gprs >= 1) {
                    avail_fprs -= 1;
                    avail_gprs -= 1;
                    return StructType::get(get_llvm_inttype(ele1.dt, ctx),
                                           get_llvm_fptype(ele2.dt, ctx));
                }
            }
        }
    }
    size_t dsz = jl_datatype_size(ty);
    if (dsz > 2 * XLen) {
        if (!jl_is_primitivetype(ty)) {
            onstack = true;
        }
        // else let llvm backend handle scalars
        if (avail_gprs >= 1) {
            avail_gprs -= 1;
        }
        return NULL;
    }

    if (dsz > XLen) {
        size_t alignment = jl_datatype_align(ty);
        bool align_regs = alignment > XLen;
        if (avail_gprs >= 2) {
            avail_gprs -= 2;
        }
        // should we handle variadic as well?
        // Variadic arguments with 2×XLEN-bit alignment and size at most 2×XLEN
        // bits are passed in an aligned register pair
        else {
            avail_gprs = 0;
        }

        if (!jl_is_primitivetype(ty)) {
            // Aggregates or scalars passed on the stack are aligned to the
            // greater of the type alignment and XLen bits, but never more than
            // the stack alignment.
            if (align_regs) {
                if (alignment == 16) {
                    return Type::getInt128Ty(ctx);
                }
                else {
                    return Type::getInt64Ty(ctx);
                }
            }
            else {
                return ArrayType::get(get_llvm_inttype_byxlen(XLen, ctx), 2);
            }
        }
        // let llvm backend handle scalars
        return NULL;
    }

    //else dsz <= XLen
    if (avail_gprs >= 1) {
        avail_gprs -= 1;
    }
    if (!jl_is_primitivetype(ty)) {
        return get_llvm_inttype_byxlen(XLen, ctx);
    }
    return get_llvm_inttype(ty, ctx);
}

bool use_sret(jl_datatype_t *ty, LLVMContext &ctx) override
{
    bool onstack = false;
    int gprs = 2;
    int fprs = FLen ? 2 : 0;
    this->cached_llvmtype = classify_arg(ty, gprs, fprs, onstack, ctx);
    if (onstack) {
        this->avail_gprs -= 1;
        return true;
    }
    else {
        return false;
    }
}

bool needPassByRef(jl_datatype_t *ty, AttrBuilder &ab, LLVMContext &ctx,
                   Type *Ty) override
{
    bool onstack = false;
    this->cached_llvmtype =
        classify_arg(ty, this->avail_gprs, this->avail_fprs, onstack, ctx);
    return onstack;
}

Type *preferred_llvm_type(jl_datatype_t *ty, bool isret,
                          LLVMContext &ctx) const override
{
    return this->cached_llvmtype;
}

};
