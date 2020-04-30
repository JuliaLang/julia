// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <utility>
#include <llvm/Support/Debug.h>
#include <llvm/IR/DebugLoc.h>
#include <llvm/IR/IRBuilder.h>

enum AddressSpace {
    Generic = 0,
    Tracked = 10,
    Derived = 11,
    CalleeRooted = 12,
    Loaded = 13,
    FirstSpecial = Tracked,
    LastSpecial = Loaded,
};

// JLCALL with API arguments ([extra], arg0, arg1, arg2, ...) has the following ABI calling conventions defined:
#define JLCALL_F_CC (CallingConv::ID)37     // (jl_value_t *arg0, jl_value_t **argv, uint32_t nargv)
#define JLCALL_F2_CC (CallingConv::ID)38    // (jl_value_t *arg0, jl_value_t **argv, uint32_t nargv, jl_value_t *extra)

// return how many Tracked pointers are in T (count > 0),
// and if there is anything else in T (all == false)
struct CountTrackedPointers {
    unsigned count = 0;
    bool all = true;
    bool derived = false;
    CountTrackedPointers(llvm::Type *T);
};

#if JL_LLVM_VERSION >= 110000
unsigned TrackWithShadow(llvm::Value *Src, llvm::Type *T, bool isptr, llvm::Value *Dst, llvm::IRBuilder<> &irbuilder);
std::vector<llvm::Value*> ExtractTrackedValues(llvm::Value *Src, llvm::Type *STy, bool isptr, llvm::IRBuilder<> &irbuilder);
#else
unsigned TrackWithShadow(llvm::Value *Src, llvm::Type *T, bool isptr, llvm::Value *Dst, llvm::IRBuilder<> irbuilder);
std::vector<llvm::Value*> ExtractTrackedValues(llvm::Value *Src, llvm::Type *STy, bool isptr, llvm::IRBuilder<> irbuilder);
#endif

static inline void llvm_dump(llvm::Value *v)
{
    v->print(llvm::dbgs(), true);
    llvm::dbgs() << "\n";
}

static inline void llvm_dump(llvm::Type *v)
{
    v->print(llvm::dbgs(), true);
    llvm::dbgs() << "\n";
}

static inline void llvm_dump(llvm::Function *f)
{
    f->print(llvm::dbgs(), nullptr, false, true);
}

static inline void llvm_dump(llvm::Module *m)
{
    m->print(llvm::dbgs(), nullptr);
}

static inline void llvm_dump(llvm::Metadata *m)
{
    m->print(llvm::dbgs());
    llvm::dbgs() << "\n";
}

static inline void llvm_dump(llvm::DebugLoc *dbg)
{
    dbg->print(llvm::dbgs());
    llvm::dbgs() << "\n";
}
