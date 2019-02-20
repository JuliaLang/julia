// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef LLVM_PASS_HELPERS_H
#define LLVM_PASS_HELPERS_H

#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

struct JuliaPassContext;

// A namespace for Julia intrinsic descriptions.
namespace jl_intrinsics {
    // A description of an intrinsic that can be used to find existing
    // intrinsics and declare new intrinsics if necessary.
    struct IntrinsicDescription final {
        // The type of function that declares an intrinsic.
        typedef llvm::Function *(*DeclarationFunction)(const JuliaPassContext&);

        // Creates an intrinsic description with a particular
        // name and declaration function.
        IntrinsicDescription(
            const llvm::StringRef &name,
            const DeclarationFunction &declare)
            : name(name), declare(declare)
        { }

        // The intrinsic's name.
        llvm::StringRef name;
        // A function that declares the intrinsic in a module.
        DeclarationFunction declare;
    };
}

// A data structure that can read Julia-specific intrinsics
// from modules or add them if they're not available yet.
// Mainly useful for building Julia-specific LLVM passes.
struct JuliaPassContext {
    // Standard types.
    llvm::Type *T_size;
    llvm::Type *T_int8;
    llvm::Type *T_int32;
    llvm::PointerType *T_pint8;

    // Types derived from 'jl_value_t'.
    llvm::Type *T_jlvalue;
    llvm::PointerType *T_prjlvalue;
    llvm::PointerType *T_ppjlvalue;
    llvm::PointerType *T_pjlvalue;
    llvm::PointerType *T_pjlvalue_der;
    llvm::PointerType *T_ppjlvalue_der;

    // TBAA metadata nodes.
    llvm::MDNode *tbaa_gcframe;
    llvm::MDNode *tbaa_tag;

    // Intrinsics.
    llvm::Function *ptls_getter;
    llvm::Function *gc_flush_func;
    llvm::Function *gc_preserve_begin_func;
    llvm::Function *gc_preserve_end_func;
    llvm::Function *pointer_from_objref_func;
    llvm::Function *alloc_obj_func;
    llvm::Function *typeof_func;
    llvm::Function *write_barrier_func;

    // Creates a pass context. Type and function pointers
    // are set to `nullptr`. Metadata nodes are initialized.
    JuliaPassContext();

    // Populates a pass context by inspecting a module.
    // Also sets the current module to the given module.
    void initAll(llvm::Module &M);

    // Initializes a pass context's functions only.
    // Also sets the current module to the given module.
    void initFunctions(llvm::Module &M);

    // Gets the LLVM context for this pass context.
    llvm::LLVMContext &getLLVMContext() const
    {
        return module->getContext();
    }

    // Gets a call to the `julia.ptls_states` intrinisc in the entry
    // point of the given function, if there exists such a call.
    // Otherwise, `nullptr` is returned.
    llvm::CallInst *getPtls(llvm::Function &F) const;

    // Gets the intrinsic or well-known function that conforms to
    // the given description if it exists in the module. If not,
    // `nullptr` is returned.
    llvm::Function *getOrNull(
        const jl_intrinsics::IntrinsicDescription &desc) const;

    // Gets the intrinsic or well-known function that conforms to
    // the given description if it exists in the module. If not,
    // declares the intrinsic or well-known function and adds it
    // to the module.
    llvm::Function *getOrDeclare(
        const jl_intrinsics::IntrinsicDescription &desc);

private:
    llvm::Module *module;
};

namespace jl_intrinsics {
    // `julia.get_gc_frame_slot`: an intrinsic that creates a
    // pointer to a GC frame slot.
    extern const IntrinsicDescription getGCFrameSlot;

    // `julia.gc_alloc_bytes`: an intrinsic that allocates
    // the bytes for an object, but does not initialize the
    // tag field. That is, its semantics and signature are
    // the same as for `julia.gc_alloc_obj`, except that
    // the object's tag field is neither initialized nor
    // passed as an argument.
    extern const IntrinsicDescription GCAllocBytes;

    // `julia.new_gc_frame`: an intrinsic that creates a new GC frame.
    extern const IntrinsicDescription newGCFrame;

    // `julia.push_gc_frame`: an intrinsic that pushes a GC frame.
    extern const IntrinsicDescription pushGCFrame;

    // `julia.pop_gc_frame`: an intrinsic that pops a GC frame.
    extern const IntrinsicDescription popGCFrame;

    // `julia.queue_gc_root`: an intrinsic that queues a GC root.
    extern const IntrinsicDescription queueGCRoot;
}

// A namespace for well-known Julia runtime function descriptions.
namespace jl_well_known {
    // A description of a well-known function that can be used to
    // find existing declarations of that function and create new
    // declarations if necessary.
    //
    // Aliased to `jl_intrinsics::IntrinsicDescription` because
    // intrinsic descriptions are essentially the same thing.
    typedef jl_intrinsics::IntrinsicDescription WellKnownFunctionDescription;

    // `jl_gc_big_alloc`: allocates bytes.
    extern const WellKnownFunctionDescription GCBigAlloc;

    // `jl_gc_pool_alloc`: allocates bytes.
    extern const WellKnownFunctionDescription GCPoolAlloc;

    // `jl_gc_queue_root`: queues a GC root.
    extern const WellKnownFunctionDescription GCQueueRoot;
}

#endif
