// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "llvm-alloc-helpers.h"
#include "llvm-codegen-shared.h"
#include "julia_assert.h"

#include <llvm/IR/IntrinsicInst.h>

#define DEBUG_TYPE "escape-analysis"

using namespace llvm;
using namespace jl_alloc;

static bool hasObjref(Type *ty)
{
    if (auto ptrty = dyn_cast<PointerType>(ty))
        return ptrty->getAddressSpace() == AddressSpace::Tracked;
    if (isa<ArrayType>(ty) || isa<VectorType>(ty))
        return hasObjref(GetElementPtrInst::getTypeAtIndex(ty, (uint64_t)0));
    if (auto structty = dyn_cast<StructType>(ty)) {
        for (auto elty: structty->elements()) {
            if (hasObjref(elty)) {
                return true;
            }
        }
    }
    return false;
}

std::pair<const uint32_t,Field>&
AllocUseInfo::getField(uint32_t offset, uint32_t size, Type *elty)
{
    // get next slot beyond containing slots (-1 to ignore slot starting at offset + size)
    auto ub = memops.upper_bound(offset + size - 1);
    if (ub == memops.begin()) {
        // We need to create a new slot, since there is no slot containing offset + size
        return *memops.emplace(offset, Field(size, elty)).first;
    }
    assert(!memops.empty());
    auto lb = memops.upper_bound(offset);
    if (lb == memops.begin()) {
        // must create an entry that contains lb
        if (size <= lb->first - offset) {
            // create entry for entire range
            return *memops.emplace(offset, Field(size, elty)).first;
        }
        lb = memops.emplace(offset, Field(lb->first - offset, elty)).first;
    } else {
        --lb;
        // lb is dereferenceable since we know memops is not empty
        if (lb->first + lb->second.size <= offset) {
            // lb does not actually contain offset
            ++lb;
            if (lb == memops.end()) {
                // create entry for entire range
                return *memops.emplace(offset, Field(size, elty)).first;
            } else {
                // create entry for range between offset and lb
                if (size <= lb->first - offset) {
                    return *memops.emplace(offset, Field(size, elty)).first;
                }
                lb = memops.emplace(offset, Field(lb->first - offset, elty)).first;
            }
        }
    }
    // lb must definitely contain offset at this point
    assert(lb->first <= offset && lb->first + lb->second.size > offset);
    assert(lb != ub);
    if (lb->first + lb->second.size >= offset + size) {
        // lb contains entire range
        return *lb;
    }
    size_t off = lb->first;
    Field field(offset - lb->first + size, elty);
    field.multiloc = true;
    for (auto it = lb; it != ub; ++it) {
        field.hasobjref |= it->second.hasobjref;
        field.hasload |= it->second.hasload;
        field.hasaggr |= it->second.hasaggr;
        field.accesses.append(it->second.accesses.begin(), it->second.accesses.end());
    }
    memops.erase(lb, ub);
    return *memops.emplace(off, std::move(field)).first;
}

bool AllocUseInfo::addMemOp(Instruction *inst, unsigned opno, uint32_t offset,
                                       Type *elty, bool isstore, const DataLayout &DL)
{
    MemOp memop(inst, opno);
    memop.offset = offset;
    uint64_t size = DL.getTypeStoreSize(elty);
    memop.size = size;
    memop.isaggr = isa<StructType>(elty) || isa<ArrayType>(elty) || isa<VectorType>(elty);
    memop.isobjref = hasObjref(elty);
    auto &field = getField(offset, size, elty);
    if (field.second.hasobjref != memop.isobjref && !field.second.accesses.empty())
        field.second.multiloc = true; // can't split this field, since it contains a mix of references and bits
    if (!isstore)
        field.second.hasload = true;
    if (memop.isobjref) {
        if (isstore) {
            refstore = true;
        }
        else {
            refload = true;
        }
        if (memop.isaggr)
            field.second.hasaggr = true;
        field.second.hasobjref = true;
    }
    else if (memop.isaggr) {
        field.second.hasaggr = true;
    }
    field.second.accesses.push_back(memop);
    if (size >= UINT32_MAX - offset)
        return false;
    return true;
}

JL_USED_FUNC void AllocUseInfo::dump(llvm::raw_ostream &OS)
{
    OS << "AllocUseInfo:\n";
    OS << "escaped: " << escaped << '\n';
    OS << "addrescaped: " << addrescaped << '\n';
    OS << "returned: " << returned << '\n';
    OS << "hasload: " << hasload << '\n';
    OS << "haspreserve: " << haspreserve << '\n';
    OS << "hasunknownmem: " << hasunknownmem << '\n';
    OS << "hastypeof: " << hastypeof << '\n';
    OS << "refload: " << refload << '\n';
    OS << "refstore: " << refstore << '\n';
    OS << "allockind:";
    if ((allockind & AllocFnKind::Uninitialized) != AllocFnKind::Unknown)
      OS << " uninitialized";
    if ((allockind & AllocFnKind::Zeroed) != AllocFnKind::Unknown)
      OS << " zeroed";
    OS << '\n';
    OS << "Uses: " << uses.size() << '\n';
    for (auto inst: uses)
        inst->print(OS);
    OS << '\n';
    if (!preserves.empty()) {
        OS << "Preserves: " << preserves.size() << '\n';
        for (auto inst: preserves)
            inst->print(OS);
        OS << '\n';
    }
    if (!errorbbs.empty()) {
        OS << "ErrorBBs: " << errorbbs.size() << '\n';
        for (auto bb: errorbbs) {
            bb->printAsOperand(OS);
            OS << '\n';
        }
    }
    OS << "MemOps: " << memops.size() << '\n';
    for (auto &field: memops) {
        OS << "  offset: " << field.first << '\n';
        OS << "  size: " << field.second.size << '\n';
        OS << "  hasobjref: " << field.second.hasobjref << '\n';
        OS << "  hasload: " << field.second.hasload << '\n';
        OS << "  hasaggr: " << field.second.hasaggr << '\n';
        OS << "  multiloc: " << field.second.multiloc << '\n';
        OS << "  accesses: " << field.second.accesses.size() << '\n';
        for (auto &memop: field.second.accesses) {
            OS << "    ";
            memop.inst->print(OS);
            OS << '\n';
            OS << "    " << (memop.isaggr ? "aggr" : "scalar") << '\n';
            OS << "    " << (memop.isobjref ? "objref" : "bits") << '\n';
            OS << "    " << memop.offset << '\n';
            OS << "    " << memop.size << '\n';
        }
    }
}

JL_USED_FUNC void AllocUseInfo::dump()
{
    dump(dbgs());
}

#ifndef __clang_gcanalyzer__
#define REMARK(remark) if (options.ORE) options.ORE->emit(remark)
#else
#define REMARK(remark)
#endif

void jl_alloc::runEscapeAnalysis(llvm::Instruction *I, EscapeAnalysisRequiredArgs required, EscapeAnalysisOptionalArgs options) {
    required.use_info.reset();
    if (auto CI = dyn_cast<CallInst>(I)) {
        Attribute allockind = CI->getFnAttr(Attribute::AllocKind);
        if (allockind.isValid())
            required.use_info.allockind = allockind.getAllocKind();
    }
    if (I->use_empty())
        return;
    CheckInst::Frame cur{I, 0, I->use_begin(), I->use_end()};
    required.check_stack.clear();

    // Recursion
    auto push_inst = [&] (Instruction *inst) {
        if (cur.use_it != cur.use_end)
            required.check_stack.push_back(cur);
        cur.parent = inst;
        cur.use_it = inst->use_begin();
        cur.use_end = inst->use_end();
    };

    auto check_inst = [&] (Instruction *inst, Use *use) {
        LLVM_DEBUG(dbgs() << "Checking: " << *inst << "\n");
        if (isa<LoadInst>(inst)) {
            required.use_info.hasload = true;
            if (cur.offset == UINT32_MAX) {
                LLVM_DEBUG(dbgs() << "Load inst has unknown offset\n");
                auto elty = inst->getType();
                required.use_info.has_unknown_objref |= hasObjref(elty);
                required.use_info.has_unknown_objrefaggr |= hasObjref(elty) && !isa<PointerType>(elty);
                required.use_info.hasunknownmem = true;
            } else if (!required.use_info.addMemOp(inst, 0, cur.offset,
                                                               inst->getType(),
                                                               false, required.DL))
                required.use_info.hasunknownmem = true;
            return true;
        }
        if (auto call = dyn_cast<CallInst>(inst)) {
            // TODO handle `memcmp`
            // None of the intrinsics should care if the memory is stack or heap allocated.
            auto callee = call->getCalledOperand();
            if (auto II = dyn_cast<IntrinsicInst>(call)) {
                if (auto id = II->getIntrinsicID()) {
                    if (id == Intrinsic::memset) {
                        assert(call->arg_size() == 4);
                        if (cur.offset == UINT32_MAX ||
                            !isa<ConstantInt>(call->getArgOperand(2)) ||
                            !isa<ConstantInt>(call->getArgOperand(1)) ||
                            (cast<ConstantInt>(call->getArgOperand(2))->getLimitedValue() >=
                             UINT32_MAX - cur.offset)) {
                            LLVM_DEBUG(dbgs() << "Memset inst has unknown offset\n");
                            required.use_info.hasunknownmem = true;
                        }
                        return true;
                    }
                    if (id == Intrinsic::lifetime_start || id == Intrinsic::lifetime_end ||
                        isa<DbgInfoIntrinsic>(II))
                        return true;
                    LLVM_DEBUG(dbgs() << "Unknown intrinsic, marking addrescape\n");
                    required.use_info.addrescaped = true;
                    return true;
                }
                if (required.pass.gc_preserve_begin_func == callee) {
                    for (auto user: call->users())
                        required.use_info.uses.insert(cast<Instruction>(user));
                    required.use_info.preserves.insert(call);
                    required.use_info.haspreserve = true;
                    return true;
                }
            }
            if (required.pass.pointer_from_objref_func == callee) {
                required.use_info.addrescaped = true;
                return true;
            }
            if (required.pass.typeof_func == callee) {
                required.use_info.hastypeof = true;
                assert(use->get() == I);
                return true;
            }
            if (required.pass.write_barrier_func == callee)
                return true;
            if (required.pass.gc_loaded_func == callee)
                return true;
            auto opno = use->getOperandNo();
            // Uses in `jl_roots` operand bundle are not counted as escaping, everything else is.
            if (!call->isBundleOperand(opno) ||
                call->getOperandBundleForOperand(opno).getTagName() != "jl_roots") {
                if (isa<UnreachableInst>(call->getParent()->getTerminator())) {
                    LLVM_DEBUG(dbgs() << "Detected use of allocation in block terminating with unreachable, likely error function\n");
                    required.use_info.errorbbs.insert(call->getParent());
                    return true;
                }
                LLVM_DEBUG(dbgs() << "Unknown call, marking escape\n");
                REMARK([&]() {
                    return OptimizationRemarkMissed(DEBUG_TYPE, "UnknownCall",
                                                    inst)
                           << "Unknown call, marking escape (" << ore::NV("Call", inst) << ")";
                });
                required.use_info.escaped = true;
                return false;
            }
            LLVM_DEBUG(dbgs() << "Call is in jl_roots bundle, marking haspreserve\n");
            required.use_info.haspreserve = true;
            return true;
        }
        if (auto store = dyn_cast<StoreInst>(inst)) {
            // Only store value count
            if (use->getOperandNo() != StoreInst::getPointerOperandIndex()) {
                if (isa<UnreachableInst>(store->getParent()->getTerminator())) {
                    LLVM_DEBUG(dbgs() << "Detected use of allocation in block terminating with unreachable, likely error function\n");
                    required.use_info.errorbbs.insert(store->getParent());
                    return true;
                }
                LLVM_DEBUG(dbgs() << "Object address is stored somewhere, marking escape\n");
                REMARK([&]() {
                    return OptimizationRemarkMissed(DEBUG_TYPE, "StoreObjAddr",
                                                    inst)
                           << "Object address is stored somewhere, marking escape (" << ore::NV("Store", inst) << ")";
                });
                required.use_info.escaped = true;
                return false;
            }
            auto storev = store->getValueOperand();
            if (cur.offset == UINT32_MAX) {
                LLVM_DEBUG(dbgs() << "Store inst has unknown offset\n");
                auto elty = storev->getType();
                required.use_info.has_unknown_objref |= hasObjref(elty);
                required.use_info.has_unknown_objrefaggr |= hasObjref(elty) && !isa<PointerType>(elty);
                required.use_info.hasunknownmem = true;
            } else if (!required.use_info.addMemOp(inst, use->getOperandNo(),
                                                               cur.offset, storev->getType(),
                                                               true, required.DL))
                required.use_info.hasunknownmem = true;
            return true;
        }
        if (isa<AtomicCmpXchgInst>(inst) || isa<AtomicRMWInst>(inst)) {
            // Only store value count
            if (use->getOperandNo() != isa<AtomicCmpXchgInst>(inst) ? AtomicCmpXchgInst::getPointerOperandIndex() : AtomicRMWInst::getPointerOperandIndex()) {
                LLVM_DEBUG(dbgs() << "Object address is cmpxchg/rmw-ed somewhere, marking escape\n");
                REMARK([&]() {
                    return OptimizationRemarkMissed(DEBUG_TYPE, "StoreObjAddr",
                                                    inst)
                           << "Object address is cmpxchg/rmw-ed somewhere, marking escape (" << ore::NV("Store", inst) << ")";
                });
                required.use_info.escaped = true;
                return false;
            }
            required.use_info.hasload = true;
            auto storev = isa<AtomicCmpXchgInst>(inst) ? cast<AtomicCmpXchgInst>(inst)->getNewValOperand() : cast<AtomicRMWInst>(inst)->getValOperand();
            if (cur.offset == UINT32_MAX || !required.use_info.addMemOp(inst, use->getOperandNo(),
                                                               cur.offset, storev->getType(),
                                                               true, required.DL)) {
                LLVM_DEBUG(dbgs() << "Atomic inst has unknown offset\n");
                required.use_info.hasunknownmem = true;
            }
            required.use_info.refload = true;
            return true;
        }
        if (isa<AddrSpaceCastInst>(inst) || isa<BitCastInst>(inst)) {
            push_inst(inst);
            return true;
        }
        if (auto gep = dyn_cast<GetElementPtrInst>(inst)) {
            uint64_t next_offset = cur.offset;
            if (cur.offset != UINT32_MAX) {
                APInt apoffset(sizeof(void*) * 8, cur.offset, true);
                if (!gep->accumulateConstantOffset(required.DL, apoffset) || apoffset.isNegative()) {
                    next_offset = UINT32_MAX;
                    LLVM_DEBUG(dbgs() << "GEP inst has unknown offset\n");
                }
                else {
                    next_offset = apoffset.getLimitedValue();
                    if (next_offset > UINT32_MAX) {
                        LLVM_DEBUG(dbgs() << "GEP inst exceeds 32-bit offset\n");
                        next_offset = UINT32_MAX;
                    }
                }
            }
            push_inst(inst);
            cur.offset = (uint32_t)next_offset;
            return true;
        }
        if (isa<ReturnInst>(inst)) {
            LLVM_DEBUG(dbgs() << "Allocation is returned\n");
            required.use_info.returned = true;
            return true;
        }
        if (isa<PHINode>(inst)) {
            // PHI nodes are immediate, always escapes
            // many parts of alloc-opt and julia-licm assume no phis exist,
            // so the whole infrastructure would have to be rewritten for it
            LLVM_DEBUG(dbgs() << "PHI node, marking escape\n");
            REMARK([&]() {
                return OptimizationRemarkMissed(DEBUG_TYPE, "PhiNode",
                                                inst)
                       << "PHI node, marking escape (" << ore::NV("Phi", inst) << ")";
            });
            required.use_info.escaped = true;
            return false;
        }
        switch (inst->getOpcode()) {
        case Instruction::Select:
        case Instruction::PtrToInt:
        case Instruction::Freeze:
        // These are safe ops that just don't have handling yet in alloc opt, they're fine in error blocks
        {
            if (isa<UnreachableInst>(inst->getParent()->getTerminator())) {
                LLVM_DEBUG(dbgs() << "Detected use of allocation in block terminating with unreachable, likely error function\n");
                required.use_info.errorbbs.insert(inst->getParent());
                return true;
            }
            break;
        }
        default:
            break;
        }
        LLVM_DEBUG(dbgs() << "Unknown instruction, marking escape\n");
        REMARK([&]() {
            return OptimizationRemarkMissed(DEBUG_TYPE, "UnknownInst",
                                            inst)
                   << "Unknown instruction, marking escape (" << ore::NV("Inst", inst) << ")";
        });
        required.use_info.escaped = true;
        return false;
    };

    while (true) {
        assert(cur.use_it != cur.use_end);
        auto use = &*cur.use_it;
        auto inst = dyn_cast<Instruction>(use->getUser());
        ++cur.use_it;
        if (!inst) {
            required.use_info.escaped = true;
            return;
        }
        if (!options.valid_set || options.valid_set->contains(inst->getParent())) {
            if (!check_inst(inst, use))
                return;
            required.use_info.uses.insert(inst);
        }
        if (cur.use_it == cur.use_end) {
            if (required.check_stack.empty())
                return;
            cur = required.check_stack.back();
            required.check_stack.pop_back();
        }
    }
}
