// This file is a part of Julia. License is MIT: https://julialang.org/license

//===----------------------------------------------------------------------===//
//
// The ABI implementation used for LoongArch 64-bit targets.
//
//===----------------------------------------------------------------------===//
//
// The Procedure Call Standard can be found here:
// https://loongson.github.io/LoongArch-Documentation/LoongArch-ABI-EN.html
//
// This implementation is adapted from Julia's RISC-V ABI code (src/abi_riscv.cpp)
// because the calling conventions are very similar.
//
//===----------------------------------------------------------------------===//

struct ABI_LoongArchLayout : AbiLayout {

    // --- Basic LoongArch64 ABI parameters ---
    static const size_t XLen = 8;      // General-purpose register width (64-bit)
    static const size_t FLen = 8;      // Floating-point register width (64-bit)
    static const int NumArgGPRs = 8;   // Number of argument GPRs: a0-a7
    static const int NumArgFPRs = 8;   // Number of argument FPRs: fa0-fa7

    // Remaining available registers.
    // These counters are decremented as arguments are processed,
    // simulating the actual register allocation during a call.
    int avail_gprs, avail_fprs;

    // Cache for the preferred LLVM type, computed in use_sret or needPassByRef
    // and reused by preferred_llvm_type to avoid recomputation.
    Type *cached_llvmtype = NULL;

    // Constructor: initially all argument registers are available
    ABI_LoongArchLayout() : avail_gprs(NumArgGPRs), avail_fprs(NumArgFPRs) {}

    // --- Helper classification enums and structs ---

    enum RegPassKind { UNKNOWN = 0, INTEGER = 1, FLOAT = 2 };

    struct ElementType {
        RegPassKind type;
        jl_datatype_t *dt;
        ElementType() : type(RegPassKind::UNKNOWN), dt(NULL) {};
    };

    // Check if the Julia datatype is a hardware floating-point type
    bool is_floattype(jl_datatype_t *dt) const
    {
        return dt == jl_float16_type || dt == jl_float32_type || dt == jl_float64_type;
    }

    // Map a Julia floating-point type to the corresponding LLVM FP type
    Type *get_llvm_fptype(jl_datatype_t *dt, LLVMContext &ctx) const
    {
        assert(is_floattype(dt));
        switch (jl_datatype_size(dt)) {
        case 2: return Type::getHalfTy(ctx);
        case 4: return Type::getFloatTy(ctx);
        case 8: return Type::getDoubleTy(ctx);
        default: assert(0 && "abi_loongarch: unsupported floating point type"); return NULL;
        }
    }

    // Map Julia primitive types (integers, pointers, bitstypes) to LLVM integer types
    Type *get_llvm_inttype(jl_datatype_t *dt, LLVMContext &ctx) const
    {
        assert(jl_is_primitivetype(dt));
        // LoongArch ABI does not define hardware half-float; pass as integer
        if (dt == jl_float16_type)
            return Type::getInt32Ty(ctx);
        assert(!is_floattype(dt));
        if (dt == jl_bool_type)
            return Type::getInt8Ty(ctx);
        if (dt == jl_int32_type)
            return Type::getInt32Ty(ctx);
        if (dt == jl_int64_type)
            return Type::getInt64Ty(ctx);
        int nb = jl_datatype_size(dt);
        return Type::getIntNTy(ctx, nb * 8);
    }

    // Determine if a struct can be split into fundamental types (Float/Int)
    // for register passing. This follows the LoongArch ABI rules for small
    // aggregates containing floating-point values.
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
                    // More than two elements: cannot split
                    return false;
                }
            }
            else {
                // Integer/pointer/bitstype
                if (ele1.type == RegPassKind::UNKNOWN) {
                    ele1.type = RegPassKind::INTEGER;
                    ele1.dt = dt;
                }
                else if (ele1.type == RegPassKind::INTEGER) {
                    // Two integers: not a FP conversion case
                    return false;
                }
                else if (ele1.type == RegPassKind::FLOAT) {
                    // Mixed pair: first element is float, second is int
                    if (ele2.type == RegPassKind::UNKNOWN) {
                        ele2.type = RegPassKind::INTEGER;
                        ele2.dt = dt;
                    }
                    else {
                        return false;
                    }
                }
            }
        }
        else {
            // Aggregates (structs/tuples)
            while (size_t nfields = jl_datatype_nfields(dt)) {
                size_t i;
                size_t fieldsz;
                // Skip zero-sized fields
                for (i = 0; i < nfields; i++) {
                    if ((fieldsz = jl_field_size(dt, i))) {
                        break;
                    }
                }
                assert(i < nfields);
                // If there is only one non-zero sized member, try again on that member
                if (fieldsz == jl_datatype_size(dt)) {
                    dt = (jl_datatype_t *)jl_field_type(dt, i);
                    if (!jl_is_datatype(dt)) // could be inline union
                        return false;
                    continue;
                }
                // Process all non-zero fields
                for (; i < nfields; i++) {
                    size_t fieldsz = jl_field_size(dt, i);
                    if (fieldsz == 0)
                        continue;
                    jl_datatype_t *fieldtype = (jl_datatype_t *)jl_field_type(dt, i);
                    if (!jl_is_datatype(fieldtype))
                        return false;
                    // We already have two elements; cannot add more
                    if (ele2.type != RegPassKind::UNKNOWN) {
                        return false;
                    }
                    if (!should_use_fp_conv(fieldtype, ele1, ele2)) {
                        return false;
                    }
                }
                break;
            }
        }
        return true;
    }

    // Return the LLVM integer type corresponding to the given XLen (always i64 here)
    Type *get_llvm_inttype_byxlen(size_t xlen, LLVMContext &ctx) const
    {
        if (xlen == 8) {
            return Type::getInt64Ty(ctx);
        }
        else {
            assert(0 && "abi_loongarch: only XLen=8 is supported");
            return NULL;
        }
    }

    // Classify an argument type and return the LLVM type to be used for it.
    // This function also updates the available register counts and sets `onstack`
    // if the argument must be passed on the stack.
    Type *classify_arg(jl_datatype_t *ty, int &avail_gprs, int &avail_fprs, bool &onstack,
                       LLVMContext &ctx) const
    {
        onstack = false;
        if (ty == jl_nothing_type) {
            return NULL;
        }

        ElementType ele1, ele2;
        // Handle small aggregates that can be split into FP or FP+int registers
        if (should_use_fp_conv(ty, ele1, ele2)) {
            if (ele1.type == RegPassKind::FLOAT) {
                if (ele2.type == RegPassKind::FLOAT) {
                    // Two floats: pass in two FP registers
                    if (avail_fprs >= 2) {
                        avail_fprs -= 2;
                        SmallVector<Type *, 2> eles;
                        eles.push_back(get_llvm_fptype(ele1.dt, ctx));
                        eles.push_back(get_llvm_fptype(ele2.dt, ctx));
                        return StructType::get(ctx, eles);
                    }
                }
                else if (ele2.type == RegPassKind::INTEGER) {
                    // Float + integer: pass in one FP and one GPR
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
                    // Single float: pass as standalone float
                    if (avail_fprs >= 1) {
                        avail_fprs -= 1;
                        return get_llvm_fptype(ele1.dt, ctx);
                    }
                }
            }
            else if (ele1.type == RegPassKind::INTEGER && ele2.type == RegPassKind::FLOAT) {
                // Integer + float: pass in one GPR and one FP register
                if (avail_gprs >= 1 && avail_fprs >= 1) {
                    avail_gprs -= 1;
                    avail_fprs -= 1;
                    return StructType::get(get_llvm_inttype(ele1.dt, ctx),
                                           get_llvm_fptype(ele2.dt, ctx));
                }
            }
        }

        size_t dsz = jl_datatype_size(ty);

        // Large types (> 16 bytes): pass on stack (unless primitive scalar, e.g. Int128)
        if (dsz > 2 * XLen) {
            if (!jl_is_primitivetype(ty)) {
                onstack = true;
            }
            // Consume one GPR for the address if needed
            if (avail_gprs >= 1) {
                avail_gprs -= 1;
            }
            return NULL;
        }

        // Medium types (9–16 bytes)
        if (dsz > XLen) {
            size_t alignment = jl_datatype_align(ty);
            bool align_regs = alignment > XLen;
            // Need two GPRs (or none, if not enough registers -> stack)
            if (avail_gprs >= 2) {
                avail_gprs -= 2;
            }
            else {
                avail_gprs = 0;
            }

            if (!jl_is_primitivetype(ty)) {
                // Aggregates: if 16-byte aligned, use i128; otherwise an array of two i64s.
                // This ensures the LLVM backend generates the correct register pair layout.
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
            // Primitive type (like Int128): let LLVM backend handle it
            return NULL;
        }

        // Small types (<= 8 bytes)
        if (avail_gprs >= 1) {
            avail_gprs -= 1;
        }
        if (!jl_is_primitivetype(ty)) {
            // Small aggregates: pass as single i64
            return get_llvm_inttype_byxlen(XLen, ctx);
        }
        return get_llvm_inttype(ty, ctx);
    }

    // Determine if a return value must be passed via hidden pointer (sret).
    // Uses a separate register set (2 GPRs / 2 FPRs) for return values.
    bool use_sret(jl_datatype_t *ty, LLVMContext &ctx) override
    {
        bool onstack = false;
        int gprs = 2;                 // Return values can use at most 2 GPRs
        int fprs = FLen ? 2 : 0;      // and 2 FPRs
        this->cached_llvmtype = classify_arg(ty, gprs, fprs, onstack, ctx);
        if (onstack) {
            this->avail_gprs -= 1;    // sret pointer consumes a GPR
            return true;
        }
        else {
            return false;
        }
    }

    // Determine if an argument must be passed by reference (on stack).
    bool needPassByRef(jl_datatype_t *ty, AttrBuilder &ab, LLVMContext &ctx,
                       Type *Ty) override
    {
        bool onstack = false;
        // Use the instance's current available register counts
        this->cached_llvmtype =
            classify_arg(ty, this->avail_gprs, this->avail_fprs, onstack, ctx);
        return onstack;
    }

    // Return the preferred LLVM type for a Julia datatype.
    // The result was already computed and cached by the previous call to
    // use_sret or needPassByRef.
    Type *preferred_llvm_type(jl_datatype_t *ty, bool isret,
                              LLVMContext &ctx) const override
    {
        return this->cached_llvmtype;
    }

};
