// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/DebugProgramInstruction.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

#include "passes.h"
#include "llvm-codegen-shared.h"

#define DEBUG_TYPE "remove_addrspaces"

using namespace llvm;

using AddrspaceRemapFunction = std::function<unsigned(unsigned)>;


//
// Helpers
//

namespace {
class AddrspaceRemoveTypeRemapper : public ValueMapTypeRemapper {
    AddrspaceRemapFunction ASRemapper;

public:
    AddrspaceRemoveTypeRemapper(AddrspaceRemapFunction ASRemapper)
        : ASRemapper(ASRemapper)
    {
    }

    Type *remapType(Type *SrcTy)
    {
        // If we already have an entry for this type, return it.
        Type *DstTy = MappedTypes[SrcTy];
        if (DstTy)
            return DstTy;

        DstTy = SrcTy;
        if (auto Ty = dyn_cast<PointerType>(SrcTy)) {
            DstTy = PointerType::get(Ty->getContext(), ASRemapper(Ty->getAddressSpace()));
        }
        else if (auto Ty = dyn_cast<FunctionType>(SrcTy)) {
            SmallVector<Type *, 4> Params;
            for (unsigned Index = 0; Index < Ty->getNumParams(); ++Index)
                Params.push_back(remapType(Ty->getParamType(Index)));
            DstTy = FunctionType::get(
                    remapType(Ty->getReturnType()), Params, Ty->isVarArg());
        }
        else if (auto Ty = dyn_cast<StructType>(SrcTy)) {
            if (Ty->isLiteral()) {
                // Since a literal type has to have the body when it is created,
                // we need to remap the element types first. This is safe only
                // for literal types (i.e., no self-reference) and thus treated
                // separately.
                assert(!Ty->hasName()); // literal type has no name.
                SmallVector<Type *, 4> NewElTys;
                NewElTys.reserve(Ty->getNumElements());
                for (auto E: Ty->elements())
                    NewElTys.push_back(remapType(E));
                DstTy = StructType::get(Ty->getContext(), NewElTys, Ty->isPacked());
            } else if (!Ty->isOpaque()) {
                // If the struct type is not literal and not opaque, it can have
                // self-referential fields (i.e., pointer type of itself as a
                // field).
                StructType *DstTy_ = StructType::create(Ty->getContext());
                if (Ty->hasName()) {
                    auto Name = std::string(Ty->getName());
                    Ty->setName(Name + ".bad");
                    DstTy_->setName(Name);
                }
                // To avoid infinite recursion, shove the placeholder of the DstTy before
                // recursing into the element types:
                MappedTypes[SrcTy] = DstTy_;

                auto Els = Ty->getNumElements();
                SmallVector<Type *, 4> NewElTys(Els);
                for (unsigned i = 0; i < Els; ++i)
                    NewElTys[i] = remapType(Ty->getElementType(i));
                DstTy_->setBody(NewElTys, Ty->isPacked());
                DstTy = DstTy_;
            }
        }
        else if (auto Ty = dyn_cast<ArrayType>(SrcTy))
            DstTy = ArrayType::get(
                    remapType(Ty->getElementType()), Ty->getNumElements());
        else if (auto Ty = dyn_cast<VectorType>(SrcTy))
            DstTy = VectorType::get(remapType(Ty->getElementType()), Ty);

        if (DstTy != SrcTy)
            LLVM_DEBUG(
                    dbgs() << "Remapping type:\n"
                           << "  from " << *SrcTy << "\n"
                           << "  to   " << *DstTy << "\n");

        MappedTypes[SrcTy] = DstTy;
        return DstTy;
    }

private:
    DenseMap<Type *, Type *> MappedTypes;
};
}  // anonymous namespace


namespace {
class AddrspaceRemoveValueMaterializer : public ValueMaterializer {
    ValueToValueMapTy &VM;
    RemapFlags Flags;
    ValueMapTypeRemapper *TypeMapper = nullptr;

public:
    AddrspaceRemoveValueMaterializer(
            ValueToValueMapTy &VM,
            RemapFlags Flags = RF_None,
            ValueMapTypeRemapper *TypeMapper = nullptr)
        : VM(VM), Flags(Flags), TypeMapper(TypeMapper)
    {
    }

    Value *materialize(Value *SrcV)
    {
        Value *DstV = nullptr;
        if (auto CE = dyn_cast<ConstantExpr>(SrcV)) {
            Type *Ty = remapType(CE->getType());
            if (CE->getOpcode() == Instruction::AddrSpaceCast) {
                // peek through addrspacecasts if their address spaces match
                // (like RemoveNoopAddrSpaceCasts, but for const exprs)
                Constant *Src = mapConstant(CE->getOperand(0));
                if (Src->getType()->getPointerAddressSpace() ==
                    Ty->getPointerAddressSpace())
                    DstV = Src;
            }
            else {
                // recreate other const exprs with their operands remapped
                SmallVector<Constant *, 4> Ops;
                for (unsigned Index = 0; Index < CE->getNumOperands();
                     ++Index) {
                    Constant *Op = CE->getOperand(Index);
                    Constant *NewOp = mapConstant(Op);
                    Ops.push_back(NewOp ? cast<Constant>(NewOp) : Op);
                }

                if (CE->getOpcode() != Instruction::GetElementPtr)
                    DstV = CE->getWithOperands(Ops, Ty);
            }
        }

        if (DstV)
            LLVM_DEBUG(
                    dbgs() << "Materializing value:\n"
                           << "  from " << *SrcV << "\n"
                           << "  to   " << *DstV << "\n");
        return DstV;
    }

private:
    Type *remapType(Type *SrcTy)
    {
        if (TypeMapper)
            return TypeMapper->remapType(SrcTy);
        else
            return SrcTy;
    }

    Value *mapValue(Value *V)
    {
        return MapValue(V, VM, Flags, TypeMapper, this);
    }

    Constant *mapConstant(Constant *V)
    {
        return MapValue(V, VM, Flags, TypeMapper, this);
    }
};
}  // anonymous namespace

static bool RemoveNoopAddrSpaceCasts(Function *F)
{
    bool Changed = false;

    SmallVector<AddrSpaceCastInst *, 4> NoopCasts;
    for (Instruction &I : instructions(F)) {
        if (auto *ASC = dyn_cast<AddrSpaceCastInst>(&I)) {
            if (ASC->getSrcAddressSpace() == ASC->getDestAddressSpace()) {
                LLVM_DEBUG(
                        dbgs() << "Removing noop address space cast:\n"
                               << I << "\n");
                if (ASC->getType() == ASC->getOperand(0)->getType()) {
                    ASC->replaceAllUsesWith(ASC->getOperand(0));
                } else {
                    // uncanonicalized addrspacecast; just use the value
                    ASC->replaceAllUsesWith(ASC->getOperand(0));
                }
                NoopCasts.push_back(ASC);
            }
        }
    }
    for (auto &I : NoopCasts)
        I->eraseFromParent();

    return Changed;
}

static void copyComdat(GlobalObject *Dst, const GlobalObject *Src)
{
    const Comdat *SC = Src->getComdat();
    if (!SC)
        return;
    Comdat *DC = Dst->getParent()->getOrInsertComdat(SC->getName());
    DC->setSelectionKind(SC->getSelectionKind());
    Dst->setComdat(DC);
}


//
// Actual pass
//

static unsigned removeAllAddrspaces(unsigned AS)
{
    return AddressSpace::Generic;
}

static bool removeAddrspaces(Module &M, AddrspaceRemapFunction ASRemapper)
{
    ValueToValueMapTy VMap;
    AddrspaceRemoveTypeRemapper TypeRemapper(ASRemapper);
    AddrspaceRemoveValueMaterializer Materializer(
            VMap, RF_None, &TypeRemapper);

    // Loop over all of the global variables, creating versions without address
    // spaces. We only add the new globals to the VMap, attributes and
    // initializers come later.
    SmallVector<GlobalVariable *, 4> Globals;
    for (auto &GV : M.globals())
        Globals.push_back(&GV);
    for (auto &GV : Globals) {
        std::string Name;
        if (GV->hasName()) {
            Name = std::string(GV->getName());
            GV->setName(Name + ".bad");
        }
        else
            Name = "";

        GlobalVariable *NGV = new GlobalVariable(
                M,
                TypeRemapper.remapType(GV->getValueType()),
                GV->isConstant(),
                GV->getLinkage(),
                (Constant *)nullptr,
                Name,
                (GlobalVariable *)nullptr,
                GV->getThreadLocalMode(),
                cast<PointerType>(TypeRemapper.remapType(GV->getType()))->getAddressSpace());
        NGV->copyAttributesFrom(GV);
        VMap[GV] = NGV;
    }

    // Loop over the aliases in the module.
    SmallVector<GlobalAlias *, 4> Aliases;
    for (auto &GA : M.aliases())
        Aliases.push_back(&GA);
    for (auto &GA : Aliases) {
        std::string Name;
        if (GA->hasName()) {
            Name = std::string(GA->getName());
            GA->setName(Name + ".bad");
        }
        else
            Name = "";

        auto *NGA = GlobalAlias::create(
                TypeRemapper.remapType(GA->getValueType()),
                cast<PointerType>(TypeRemapper.remapType(GA->getType()))->getAddressSpace(),
                GA->getLinkage(),
                Name,
                &M);
        NGA->copyAttributesFrom(GA);
        VMap[GA] = NGA;
    }

    // Loop over the functions in the module, creating new ones as before.
    SmallVector<Function *, 4> Functions;
    for (Function &F : M)
        Functions.push_back(&F);
    for (Function *F : Functions) {
        std::string Name;
        if (F->hasName()) {
            Name = std::string(F->getName());
            F->setName(Name + ".bad");
        }
        else
            Name = "";

        FunctionType *FTy = cast<FunctionType>(F->getValueType());
        SmallVector<Type *, 3> Tys;
        for (Type *Ty : FTy->params())
            Tys.push_back(TypeRemapper.remapType(Ty));
        FunctionType *NFTy = FunctionType::get(
                TypeRemapper.remapType(FTy->getReturnType()),
                Tys,
                FTy->isVarArg());

        Function *NF = Function::Create(
                NFTy, F->getLinkage(), F->getAddressSpace(), Name, &M);
        // no need to copy attributes here, that's done by CloneFunctionInto
        VMap[F] = NF;
    }

    // Now that all of the things that global variable initializer can refer to
    // have been created, loop through and copy the global variable referrers
    // over...  We also set the attributes on the globals now.
    for (GlobalVariable *GV : Globals) {
        if (GV->isDeclaration())
            continue;

        GlobalVariable *NGV = cast<GlobalVariable>(VMap[GV]);
        if (GV->hasInitializer())
            NGV->setInitializer(MapValue(GV->getInitializer(), VMap, RF_None, &TypeRemapper, &Materializer));

        SmallVector<std::pair<unsigned, MDNode *>, 1> MDs;
        GV->getAllMetadata(MDs);
        for (auto MD : MDs)
            NGV->addMetadata(
                    MD.first,
                    *MapMetadata(MD.second, VMap));

        copyComdat(NGV, GV);

        GV->setInitializer(nullptr);
    }
    // Same workaround as in CloneCtx::prepare_vmap to avoid LLVM bug when cloning
    auto &MD = VMap.MD();
    if (M.getNamedMetadata("llvm.dbg.cu"))
        for (auto cu: M.getNamedMetadata("llvm.dbg.cu")->operands()) {
            MD[cu].reset(cu);
        }
    // Similarly, copy over and rewrite function bodies
    for (Function *F : Functions) {
        Function *NF = cast<Function>(VMap[F]);
        LLVM_DEBUG(dbgs() << "Processing function " << NF->getName() << "\n");
        // we also need this to run for declarations, or attributes won't be copied

        Function::arg_iterator DestI = NF->arg_begin();
        for (Function::const_arg_iterator I = F->arg_begin(); I != F->arg_end();
             ++I) {
            DestI->setName(I->getName());
            VMap[&*I] = &*DestI++;
        }

        SmallVector<ReturnInst *, 8> Returns; // Ignore returns cloned.
        CloneFunctionInto(
                NF,
                F,
                VMap,
                CloneFunctionChangeType::GlobalChanges,
                Returns,
                "",
                nullptr,
                &TypeRemapper,
                &Materializer);

        // Update function attributes that contain types
        AttributeList Attrs = F->getAttributes();
        LLVMContext &C = F->getContext();
        for (unsigned i = 0; i < Attrs.getNumAttrSets(); ++i) {
            for (Attribute::AttrKind TypedAttr :
                 {Attribute::ByVal, Attribute::StructRet, Attribute::ByRef}) {
                auto Attr = Attrs.getAttributeAtIndex(i, TypedAttr);
                if (Type *Ty = Attr.getValueAsType()) {
                    Attrs = Attrs.replaceAttributeTypeAtIndex(
                        C, i, TypedAttr, TypeRemapper.remapType(Ty));
                    break;
                }
            }
        }
        NF->setAttributes(Attrs);

        copyComdat(NF, F);

        RemoveNoopAddrSpaceCasts(NF);
        F->deleteBody();
    }

    // And aliases
    for (GlobalAlias *GA : Aliases) {
        GlobalAlias *NGA = cast<GlobalAlias>(VMap[GA]);
        if (const Constant *C = GA->getAliasee())
            NGA->setAliasee(MapValue(C, VMap, RF_None, &TypeRemapper, &Materializer));

        GA->setAliasee(nullptr);
    }

    // And named metadata
    for (auto &NMD : M.named_metadata()) {
        for (unsigned i = 0, e = NMD.getNumOperands(); i != e; ++i)
            NMD.setOperand(i, MapMetadata(NMD.getOperand(i), VMap));
    }

    // Now that we've duplicated everything, remove the old references
    for (GlobalVariable *GV : Globals)
        GV->eraseFromParent();
    for (GlobalAlias *GA : Aliases)
        GA->eraseFromParent();
    for (Function *F : Functions)
        F->eraseFromParent();

    // Finally, remangle calls to intrinsic
    for (Module::iterator FI = M.begin(), FE = M.end(); FI != FE;) {
        Function *F = &*FI++;
        if (auto Remangled = Intrinsic::remangleIntrinsicFunction(F)) {
            F->replaceAllUsesWith(*Remangled);
            F->eraseFromParent();
        }
    }


    return true;
}


//
// In-place variant: with opaque pointers, address spaces only live in value types, so
// mutate types and constant operands instead of cloning, and only recreate functions
// whose signature changes (moving their bodies). Modules with constructs this does not
// handle fall back to the cloning version above.
//

namespace {
class InPlaceAddrspaceRemover {
    AddrspaceRemapFunction ASRemapper;
    DenseMap<Type *, Type *> MappedTypes;
    DenseMap<Constant *, Constant *> MappedConstants;
    LLVMContext &C;

public:
    bool Failed = false;

    InPlaceAddrspaceRemover(LLVMContext &C, AddrspaceRemapFunction ASRemapper)
        : ASRemapper(ASRemapper), C(C) {}

    Type *remapType(Type *SrcTy)
    {
        auto it = MappedTypes.find(SrcTy);
        if (it != MappedTypes.end())
            return it->second;
        Type *DstTy = SrcTy;
        if (auto Ty = dyn_cast<PointerType>(SrcTy)) {
            unsigned AS = ASRemapper(Ty->getAddressSpace());
            if (AS != Ty->getAddressSpace())
                DstTy = PointerType::get(C, AS);
        }
        else if (auto Ty = dyn_cast<FunctionType>(SrcTy)) {
            bool changed = false;
            SmallVector<Type *, 8> Params;
            for (Type *P : Ty->params()) {
                Params.push_back(remapType(P));
                changed |= Params.back() != P;
            }
            Type *Ret = remapType(Ty->getReturnType());
            changed |= Ret != Ty->getReturnType();
            if (changed)
                DstTy = FunctionType::get(Ret, Params, Ty->isVarArg());
        }
        else if (auto Ty = dyn_cast<StructType>(SrcTy)) {
            bool changed = false;
            SmallVector<Type *, 8> Els;
            for (Type *E : Ty->elements()) {
                Els.push_back(remapType(E));
                changed |= Els.back() != E;
            }
            if (changed) {
                if (Ty->isLiteral())
                    DstTy = StructType::get(C, Els, Ty->isPacked());
                else
                    Failed = true; // named struct bodies cannot be changed in place
            }
        }
        else if (auto Ty = dyn_cast<ArrayType>(SrcTy)) {
            Type *E = remapType(Ty->getElementType());
            if (E != Ty->getElementType())
                DstTy = ArrayType::get(E, Ty->getNumElements());
        }
        else if (auto Ty = dyn_cast<VectorType>(SrcTy)) {
            Type *E = remapType(Ty->getElementType());
            if (E != Ty->getElementType())
                DstTy = VectorType::get(E, Ty);
        }
        MappedTypes[SrcTy] = DstTy;
        return DstTy;
    }

    Constant *mapConstant(Constant *Src)
    {
        if (isa<GlobalValue>(Src) || (isa<ConstantData>(Src) && !isa<ConstantPointerNull>(Src) &&
                !isa<UndefValue>(Src) && !isa<ConstantAggregateZero>(Src) && !isa<ConstantDataSequential>(Src)))
            return Src;
        auto it = MappedConstants.find(Src);
        if (it != MappedConstants.end())
            return it->second;
        Type *Ty = remapType(Src->getType());
        Constant *Dst = Src;
        if (isa<ConstantPointerNull>(Src)) {
            if (Ty != Src->getType())
                Dst = ConstantPointerNull::get(cast<PointerType>(Ty));
        }
        else if (isa<PoisonValue>(Src)) {
            if (Ty != Src->getType())
                Dst = PoisonValue::get(Ty);
        }
        else if (isa<UndefValue>(Src)) {
            if (Ty != Src->getType())
                Dst = UndefValue::get(Ty);
        }
        else if (isa<ConstantAggregateZero>(Src)) {
            if (Ty != Src->getType())
                Dst = ConstantAggregateZero::get(Ty);
        }
        else if (isa<ConstantDataSequential>(Src)) {
        }
        else if (auto CE = dyn_cast<ConstantExpr>(Src)) {
            SmallVector<Constant *, 4> Ops;
            bool changed = Ty != Src->getType();
            for (Use &U : CE->operands()) {
                Ops.push_back(mapConstant(cast<Constant>(U.get())));
                changed |= Ops.back() != U.get();
            }
            if (CE->getOpcode() == Instruction::AddrSpaceCast &&
                    Ops[0]->getType() == Ty) {
                Dst = Ops[0];
            }
            else if (changed) {
                Type *SrcElTy = nullptr;
                if (auto GEP = dyn_cast<GEPOperator>(CE))
                    SrcElTy = remapType(GEP->getSourceElementType());
                Dst = CE->getWithOperands(Ops, Ty, false, SrcElTy);
            }
        }
        else if (isa<ConstantAggregate>(Src)) {
            SmallVector<Constant *, 8> Ops;
            bool changed = Ty != Src->getType();
            for (Use &U : Src->operands()) {
                Ops.push_back(mapConstant(cast<Constant>(U.get())));
                changed |= Ops.back() != U.get();
            }
            if (changed) {
                if (auto STy = dyn_cast<StructType>(Ty))
                    Dst = ConstantStruct::get(STy, Ops);
                else if (auto ATy = dyn_cast<ArrayType>(Ty))
                    Dst = ConstantArray::get(ATy, Ops);
                else
                    Dst = ConstantVector::get(Ops);
            }
        }
        else if (Ty != Src->getType()) {
            Failed = true; // a kind of constant we do not know how to rebuild
        }
        MappedConstants[Src] = Dst;
        return Dst;
    }

    AttributeList remapAttributes(AttributeList Attrs)
    {
        for (unsigned i = 0; i < Attrs.getNumAttrSets(); ++i) {
            for (Attribute::AttrKind TypedAttr :
                 {Attribute::ByVal, Attribute::StructRet, Attribute::ByRef, Attribute::ElementType}) {
                auto Attr = Attrs.getAttributeAtIndex(i, TypedAttr);
                if (Type *Ty = Attr.getValueAsType()) {
                    Type *NTy = remapType(Ty);
                    if (NTy != Ty)
                        Attrs = Attrs.replaceAttributeTypeAtIndex(C, i, TypedAttr, NTy);
                }
            }
        }
        return Attrs;
    }

    // With Mutate = false, only reports whether anything would change (and sets Failed
    // for unsupported constructs).
    bool visit(Module &M, bool Mutate)
    {
        bool Changed = false;
        for (GlobalVariable &GV : M.globals()) {
            if (remapType(GV.getValueType()) != GV.getValueType() ||
                    ASRemapper(GV.getAddressSpace()) != GV.getAddressSpace() ||
                    (GV.hasInitializer() && mapConstant(GV.getInitializer()) != GV.getInitializer()))
                Failed = true;
        }
        for (GlobalAlias &GA : M.aliases()) {
            if (remapType(GA.getValueType()) != GA.getValueType() ||
                    ASRemapper(GA.getAddressSpace()) != GA.getAddressSpace())
                Failed = true;
        }
        if (!M.ifunc_empty())
            Failed = true;
        if (Failed)
            return true;
        SmallVector<Instruction *, 8> NoopCasts;
        for (Function &F : M) {
            if (ASRemapper(F.getAddressSpace()) != F.getAddressSpace()) {
                Failed = true;
                return true;
            }
            if (remapType(F.getFunctionType()) != F.getFunctionType())
                Changed = true;
            if (F.hasPersonalityFn() && mapConstant(F.getPersonalityFn()) != F.getPersonalityFn())
                Failed = true;
            if (F.hasPrefixData() || F.hasPrologueData())
                Failed = true;
            for (Argument &A : F.args()) {
                Type *NTy = remapType(A.getType());
                if (NTy != A.getType()) {
                    Changed = true;
                    if (Mutate)
                        A.mutateType(NTy);
                }
            }
            for (BasicBlock &BB : F) {
                if (BB.hasAddressTaken())
                    Failed = true;
                for (Instruction &I : BB) {
                    Type *NTy = remapType(I.getType());
                    if (NTy != I.getType()) {
                        Changed = true;
                        if (Mutate)
                            I.mutateType(NTy);
                    }
                    if (auto AI = dyn_cast<AllocaInst>(&I)) {
                        Type *T = AI->getAllocatedType(), *NT = remapType(T);
                        if (NT != T) {
                            Changed = true;
                            if (Mutate)
                                AI->setAllocatedType(NT);
                        }
                        if (ASRemapper(AI->getAddressSpace()) != AI->getAddressSpace())
                            Failed = true;
                    }
                    else if (auto GEP = dyn_cast<GetElementPtrInst>(&I)) {
                        Type *T = GEP->getSourceElementType(), *NT = remapType(T);
                        if (NT != T) {
                            Changed = true;
                            if (Mutate)
                                GEP->setSourceElementType(NT);
                        }
                        T = GEP->getResultElementType(), NT = remapType(T);
                        if (NT != T) {
                            Changed = true;
                            if (Mutate)
                                GEP->setResultElementType(NT);
                        }
                    }
                    else if (auto CB = dyn_cast<CallBase>(&I)) {
                        FunctionType *FT = CB->getFunctionType();
                        auto NFT = cast<FunctionType>(remapType(FT));
                        if (NFT != FT) {
                            Changed = true;
                            if (Mutate)
                                CB->mutateFunctionType(NFT);
                            if (auto IA = dyn_cast<InlineAsm>(CB->getCalledOperand())) {
                                if (Mutate)
                                    CB->setCalledOperand(InlineAsm::get(NFT, IA->getAsmString(), IA->getConstraintString(),
                                                                        IA->hasSideEffects(), IA->isAlignStack(),
                                                                        IA->getDialect(), IA->canThrow()));
                            }
                        }
                        AttributeList Attrs = CB->getAttributes();
                        AttributeList NAttrs = remapAttributes(Attrs);
                        if (NAttrs != Attrs) {
                            Changed = true;
                            if (Mutate)
                                CB->setAttributes(NAttrs);
                        }
                    }
                    else if (isa<LandingPadInst>(&I) || isa<FuncletPadInst>(&I) || isa<CatchSwitchInst>(&I)) {
                        for (Use &U : I.operands())
                            if (auto Cst = dyn_cast<Constant>(U.get()))
                                if (mapConstant(Cst) != Cst)
                                    Failed = true;
                    }
                    for (Use &U : I.operands()) {
                        auto Cst = dyn_cast<Constant>(U.get());
                        if (!Cst || isa<InlineAsm>(U.get()))
                            continue;
                        Constant *NCst = mapConstant(Cst);
                        if (NCst != Cst) {
                            Changed = true;
                            if (Mutate)
                                U.set(NCst);
                        }
                    }
                    for (DbgVariableRecord &DVR : filterDbgVars(I.getDbgRecordRange())) {
                        SmallVector<Constant *, 2> Csts;
                        for (Value *V : DVR.location_ops())
                            if (auto Cst = dyn_cast_or_null<Constant>(V))
                                Csts.push_back(Cst);
                        for (Constant *Cst : Csts) {
                            Constant *NCst = mapConstant(Cst);
                            if (NCst != Cst) {
                                Changed = true;
                                if (Mutate)
                                    DVR.replaceVariableLocationOp(Cst, NCst);
                            }
                        }
                    }
                }
            }
            if (remapAttributes(F.getAttributes()) != F.getAttributes())
                Changed = true;
            if (Failed)
                return true;
        }
        if (Mutate) {
            // operands can be defined after their uses, so check once all types are rewritten
            for (Function &F : M)
                for (Instruction &I : instructions(F))
                    if (auto ASC = dyn_cast<AddrSpaceCastInst>(&I))
                        if (ASC->getSrcAddressSpace() == ASC->getDestAddressSpace())
                            NoopCasts.push_back(ASC);
        }
        for (Instruction *I : NoopCasts) {
            I->replaceAllUsesWith(I->getOperand(0));
            I->eraseFromParent();
        }
        return Changed;
    }

    void replaceFunctions(Module &M)
    {
        SmallVector<Function *, 16> ToReplace;
        for (Function &F : M) {
            if (remapType(F.getFunctionType()) != F.getFunctionType() ||
                    remapAttributes(F.getAttributes()) != F.getAttributes())
                ToReplace.push_back(&F);
        }
        for (Function *F : ToReplace) {
            auto NFTy = cast<FunctionType>(remapType(F->getFunctionType()));
            if (NFTy == F->getFunctionType()) {
                F->setAttributes(remapAttributes(F->getAttributes()));
                continue;
            }
            // create with the final name, so that intrinsic IDs are recognized
            std::string Name = F->getName().str();
            if (!Name.empty())
                F->setName(Name + ".bad");
            Function *NF = Function::Create(NFTy, F->getLinkage(), F->getAddressSpace(), Name, &M);
            NF->copyAttributesFrom(F);
            NF->setAttributes(remapAttributes(F->getAttributes()));
            NF->setComdat(F->getComdat());
            NF->copyMetadata(F, 0);
            F->clearMetadata();
            NF->splice(NF->begin(), F);
            for (auto [A, NA] : zip(F->args(), NF->args())) {
                A.replaceAllUsesWith(&NA);
                NA.takeName(&A);
            }
            F->replaceAllUsesWith(NF);
            F->eraseFromParent();
        }
        // remangle intrinsics whose overloaded types changed
        for (Module::iterator FI = M.begin(), FE = M.end(); FI != FE;) {
            Function *F = &*FI++;
            if (auto Remangled = Intrinsic::remangleIntrinsicFunction(F)) {
                F->replaceAllUsesWith(*Remangled);
                F->eraseFromParent();
            }
        }
    }
};
} // anonymous namespace

static std::optional<bool> removeAddrspacesInPlace(Module &M, AddrspaceRemapFunction ASRemapper)
{
    InPlaceAddrspaceRemover R(M.getContext(), ASRemapper);
    bool Changed = R.visit(M, /*Mutate=*/false);
    if (R.Failed)
        return std::nullopt;
    if (!Changed)
        return false;
    R.visit(M, /*Mutate=*/true);
    assert(!R.Failed);
    R.replaceFunctions(M);
    return true;
}


RemoveAddrspacesPass::RemoveAddrspacesPass() : RemoveAddrspacesPass(removeAllAddrspaces) {}

PreservedAnalyses RemoveAddrspacesPass::run(Module &M, ModuleAnalysisManager &AM) {
    std::optional<bool> inplace = removeAddrspacesInPlace(M, ASRemapper);
    bool modified = inplace ? *inplace : removeAddrspaces(M, ASRemapper);
#ifdef JL_VERIFY_PASSES
    assert(!verifyLLVMIR(M));
#endif
    if (modified) {
        return PreservedAnalyses::allInSet<CFGAnalyses>();
    } else {
        return PreservedAnalyses::all();
    }
}


//
// Julia-specific pass
//

static unsigned removeJuliaAddrspaces(unsigned AS)
{
    if (AddressSpace::FirstSpecial <= AS && AS <= AddressSpace::LastSpecial)
        return AddressSpace::Generic;
    else
        return AS;
}


PreservedAnalyses RemoveJuliaAddrspacesPass::run(Module &M, ModuleAnalysisManager &AM) {
    return RemoveAddrspacesPass(removeJuliaAddrspaces).run(M, AM);
}
