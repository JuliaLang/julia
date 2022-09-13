// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "llvm-alloc-helpers.h"
#include "codegen_shared.h"
#include "julia_assert.h"

#include <llvm/IR/IntrinsicInst.h>

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
    auto it = findLowerField(offset);
    auto end = memops.end();
    auto lb = end; // first overlap
    auto ub = end; // last overlap
    if (it != end) {
        // The slot found contains the current location
        if (it->first + it->second.size >= offset + size) {
            if (it->second.elty != elty)
                it->second.elty = nullptr;
            assert(it->second.elty == nullptr || (it->first == offset && it->second.size == size));
            return *it;
        }
        if (it->first + it->second.size > offset) {
            lb = it;
            ub = it;
        }
    }
    else {
        it = memops.begin();
    }
    // Now find the last slot that overlaps with the current memory location.
    // Also set `lb` if we didn't find any above.
    for (; it != end && it->first < offset + size; ++it) {
        if (lb == end)
            lb = it;
        ub = it;
    }
    // no overlap found just create a new one.
    if (lb == end)
        return *memops.emplace(offset, Field(size, elty)).first;
    // We find overlapping but not containing slot we need to merge slot/create new one
    uint32_t new_offset = std::min(offset, lb->first);
    uint32_t new_addrub = std::max(offset + uint32_t(size), ub->first + ub->second.size);
    uint32_t new_size = new_addrub - new_offset;
    Field field(new_size, nullptr);
    field.multiloc = true;
    ++ub;
    for (it = lb; it != ub; ++it) {
        field.hasobjref |= it->second.hasobjref;
        field.hasload |= it->second.hasload;
        field.hasaggr |= it->second.hasaggr;
        field.accesses.append(it->second.accesses.begin(), it->second.accesses.end());
    }
    memops.erase(lb, ub);
    return *memops.emplace(new_offset, std::move(field)).first;
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
    if (field.second.hasobjref != memop.isobjref)
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

JL_USED_FUNC void AllocUseInfo::dump()
{
    jl_safe_printf("escaped: %d\n", escaped);
    jl_safe_printf("addrescaped: %d\n", addrescaped);
    jl_safe_printf("returned: %d\n", returned);
    jl_safe_printf("haserror: %d\n", haserror);
    jl_safe_printf("hasload: %d\n", hasload);
    jl_safe_printf("haspreserve: %d\n", haspreserve);
    jl_safe_printf("hasunknownmem: %d\n", hasunknownmem);
    jl_safe_printf("hastypeof: %d\n", hastypeof);
    jl_safe_printf("refload: %d\n", refload);
    jl_safe_printf("refstore: %d\n", refstore);
    jl_safe_printf("Uses: %d\n", (unsigned)uses.size());
    for (auto inst: uses)
        llvm_dump(inst);
    if (!preserves.empty()) {
        jl_safe_printf("Preserves: %d\n", (unsigned)preserves.size());
        for (auto inst: preserves) {
            llvm_dump(inst);
        }
    }
    if (!memops.empty()) {
        jl_safe_printf("Memops: %d\n", (unsigned)memops.size());
        for (auto &field: memops) {
            jl_safe_printf("  Field %d @ %d\n", field.second.size, field.first);
            jl_safe_printf("    Accesses:\n");
            for (auto memop: field.second.accesses) {
                jl_safe_printf("    ");
                llvm_dump(memop.inst);
            }
        }
    }
}

void jl_alloc::runEscapeAnalysis(llvm::Instruction *I, EscapeAnalysisRequiredArgs required, EscapeAnalysisOptionalArgs options) {
    required.use_info.reset();
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
        if (isa<LoadInst>(inst)) {
            required.use_info.hasload = true;
            if (cur.offset == UINT32_MAX || !required.use_info.addMemOp(inst, 0, cur.offset,
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
                             UINT32_MAX - cur.offset))
                            required.use_info.hasunknownmem = true;
                        return true;
                    }
                    if (id == Intrinsic::lifetime_start || id == Intrinsic::lifetime_end ||
                        isa<DbgInfoIntrinsic>(II))
                        return true;
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
            if (required.pass.write_barrier_func == callee ||
                required.pass.write_barrier_binding_func == callee)
                return true;
            auto opno = use->getOperandNo();
            // Uses in `jl_roots` operand bundle are not counted as escaping, everything else is.
            if (!call->isBundleOperand(opno) ||
                call->getOperandBundleForOperand(opno).getTagName() != "jl_roots") {
                if (isa<UnreachableInst>(call->getParent()->getTerminator())) {
                    required.use_info.haserror = true;
                    return true;
                }
                required.use_info.escaped = true;
                return false;
            }
            required.use_info.haspreserve = true;
            return true;
        }
        if (auto store = dyn_cast<StoreInst>(inst)) {
            // Only store value count
            if (use->getOperandNo() != StoreInst::getPointerOperandIndex()) {
                required.use_info.escaped = true;
                return false;
            }
            auto storev = store->getValueOperand();
            if (cur.offset == UINT32_MAX || !required.use_info.addMemOp(inst, use->getOperandNo(),
                                                               cur.offset, storev->getType(),
                                                               true, required.DL))
                required.use_info.hasunknownmem = true;
            return true;
        }
        if (isa<AtomicCmpXchgInst>(inst) || isa<AtomicRMWInst>(inst)) {
            // Only store value count
            if (use->getOperandNo() != isa<AtomicCmpXchgInst>(inst) ? AtomicCmpXchgInst::getPointerOperandIndex() : AtomicRMWInst::getPointerOperandIndex()) {
                required.use_info.escaped = true;
                return false;
            }
            required.use_info.hasload = true;
            auto storev = isa<AtomicCmpXchgInst>(inst) ? cast<AtomicCmpXchgInst>(inst)->getNewValOperand() : cast<AtomicRMWInst>(inst)->getValOperand();
            if (cur.offset == UINT32_MAX || !required.use_info.addMemOp(inst, use->getOperandNo(),
                                                               cur.offset, storev->getType(),
                                                               true, required.DL))
                required.use_info.hasunknownmem = true;
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
                }
                else {
                    next_offset = apoffset.getLimitedValue();
                    if (next_offset > UINT32_MAX) {
                        next_offset = UINT32_MAX;
                    }
                }
            }
            push_inst(inst);
            cur.offset = (uint32_t)next_offset;
            return true;
        }
        if (isa<ReturnInst>(inst)) {
            required.use_info.returned = true;
            return true;
        }
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
