// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <llvm/Support/Debug.h>
#include <llvm/IR/DebugLoc.h>

enum AddressSpace {
    Generic = 0,
    Tracked = 10,
    Derived = 11,
    CalleeRooted = 12,
    Loaded = 13,
    FirstSpecial = Tracked,
    LastSpecial = Loaded,
};

#define JLCALL_CC (CallingConv::ID)36
#define JLCALL_F_CC (CallingConv::ID)37

static inline void llvm_dump(llvm::Value *v)
{
#if JL_LLVM_VERSION >= 50000
    v->print(llvm::dbgs(), true);
    llvm::dbgs() << "\n";
#else
    v->dump();
#endif
}

static inline void llvm_dump(llvm::Type *v)
{
#if JL_LLVM_VERSION >= 50000
    v->print(llvm::dbgs(), true);
#else
    v->dump();
#endif
    llvm::dbgs() << "\n";
}

static inline void llvm_dump(llvm::Function *f)
{
#if JL_LLVM_VERSION >= 50000
    f->print(llvm::dbgs(), nullptr, false, true);
#else
    f->dump();
#endif
}

static inline void llvm_dump(llvm::Module *m)
{
#if JL_LLVM_VERSION >= 50000
    m->print(llvm::dbgs(), nullptr);
#else
    m->dump();
#endif
}

static inline void llvm_dump(llvm::Metadata *m)
{
#if JL_LLVM_VERSION >= 50000
    m->print(llvm::dbgs());
    llvm::dbgs() << "\n";
#else
    m->dump();
#endif
}

static inline void llvm_dump(llvm::DebugLoc *dbg)
{
#if JL_LLVM_VERSION >= 50000
    dbg->print(llvm::dbgs());
#else
    dbg->dump();
#endif
    llvm::dbgs() << "\n";
}
