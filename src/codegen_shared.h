enum AddressSpace {
    Generic = 0,
    Tracked = 10, Derived = 11, CalleeRooted = 12,
    FirstSpecial = Tracked,
    LastSpecial = CalleeRooted,
};

#define JLCALL_CC (CallingConv::ID)36
#define JLCALL_F_CC (CallingConv::ID)37

static inline void llvm_dump(llvm::Value *v)
{
#if JL_LLVM_VERSION >= 50000
    v->print(llvm::dbgs(), true);
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
    putchar('\n');
#else
    m->dump();
#endif
}
