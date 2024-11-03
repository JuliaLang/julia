// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <mutex>
#include <condition_variable>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/SmallVector.h>
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

using namespace llvm;

struct ReservationInfo {
    int16_t tid = 0;
    jl_code_instance_t *ci = nullptr;
};

struct InferKey {
    jl_method_instance_t *mi = nullptr;
    jl_value_t *owner = nullptr;
};

template<> struct llvm::DenseMapInfo<InferKey> {
  using FirstInfo = DenseMapInfo<jl_method_instance_t*>;
  using SecondInfo = DenseMapInfo<jl_value_t*>;

  static inline InferKey getEmptyKey() {
    return InferKey{FirstInfo::getEmptyKey(),
                    SecondInfo::getEmptyKey()};
  }

  static inline InferKey getTombstoneKey() {
    return InferKey{FirstInfo::getTombstoneKey(),
                    SecondInfo::getTombstoneKey()};
  }

  static unsigned getHashValue(const InferKey& PairVal) {
    return detail::combineHashValue(FirstInfo::getHashValue(PairVal.mi),
                                    SecondInfo::getHashValue(PairVal.owner));
  }

  static bool isEqual(const InferKey &LHS, const InferKey &RHS) {
    return LHS.mi == RHS.mi && LHS.owner == RHS.owner;
  }
};

static std::mutex engine_lock; // n.b. this lock is only ever held briefly
static std::condition_variable engine_wait; // but it may be waiting a while in this state
// map from MethodInstance to threadid that owns it currently for inference
static DenseMap<InferKey, ReservationInfo> Reservations;
// vector of which threads are blocked and which lease they need
static SmallVector<InferKey, 0> Awaiting; // (this could be merged into ptls also)


#ifdef __cplusplus
extern "C" {
#endif

jl_code_instance_t *jl_engine_reserve(jl_method_instance_t *m, jl_value_t *owner)
{
    jl_task_t *ct = jl_current_task;
    ct->ptls->engine_nqueued++; // disables finalizers until inference is finished on this method graph
    jl_code_instance_t *ci = jl_new_codeinst_uninit(m, owner); // allocate a placeholder
    JL_GC_PUSH1(&ci);
    auto tid = jl_atomic_load_relaxed(&ct->tid);
    if (([tid, m, owner, ci] () -> bool { // necessary scope block / lambda for unique_lock
            jl_unique_gcsafe_lock lock(engine_lock);
            InferKey key{m, owner};
            if ((signed)Awaiting.size() < tid + 1)
                Awaiting.resize(tid + 1);
            while (1) {
                auto record = Reservations.find(key);
                if (record == Reservations.end()) {
                    Reservations[key] = ReservationInfo{tid, ci};
                    return false;
                }
                // before waiting, need to run deadlock/cycle detection
                // there is a cycle if the thread holding our lease is blocked
                // and waiting for (transitively) any lease that is held by this thread
                auto wait_tid = record->second.tid;
                while (1) {
                    if (wait_tid == tid)
                        return true;
                    if ((signed)Awaiting.size() <= wait_tid)
                        break; // no cycle, since it is running (and this should be unreachable)
                    auto key2 = Awaiting[wait_tid];
                    if (key2.mi == nullptr)
                        break; // no cycle, since it is running
                    auto record2 = Reservations.find(key2);
                    if (record2 == Reservations.end())
                        break; // no cycle, since it is about to resume
                    assert(wait_tid != record2->second.tid);
                    wait_tid = record2->second.tid;
                }
                Awaiting[tid] = key;
                lock.wait(engine_wait);
                Awaiting[tid] = InferKey{};
            }
        })())
        ct->ptls->engine_nqueued--;
    JL_GC_POP();
    return ci;
}

int jl_engine_hasreserved(jl_method_instance_t *m, jl_value_t *owner)
{
    jl_task_t *ct = jl_current_task;
    InferKey key = {m, owner};
    std::unique_lock lock(engine_lock);
    auto record = Reservations.find(key);
    return record != Reservations.end() && record->second.tid == jl_atomic_load_relaxed(&ct->tid);
}

STATIC_INLINE int gc_marked(uintptr_t bits) JL_NOTSAFEPOINT
{
    return (bits & GC_MARKED) != 0;
}

void jl_engine_sweep(jl_ptls_t *gc_all_tls_states)
{
    std::unique_lock lock(engine_lock);
    bool any = false;
    for (auto I = Reservations.begin(); I != Reservations.end(); ++I) {
        jl_code_instance_t *ci = I->second.ci;
        if (!gc_marked(jl_astaggedvalue(ci)->bits.gc)) {
            auto tid = I->second.tid;
            Reservations.erase(I);
            jl_ptls_t ptls2 = gc_all_tls_states[tid];
            ptls2->engine_nqueued--;
            any = true;
        }
    }
    if (any)
        engine_wait.notify_all();
}

void jl_engine_fulfill(jl_code_instance_t *ci, jl_code_info_t *src)
{
    jl_task_t *ct = jl_current_task;
    std::unique_lock lock(engine_lock);
    auto record = Reservations.find(InferKey{ci->def, ci->owner});
    if (record == Reservations.end() || record->second.ci != ci)
        return;
    assert(jl_atomic_load_relaxed(&ct->tid) == record->second.tid);
    ct->ptls->engine_nqueued--; // re-enables finalizers, but doesn't immediately try to run them
    Reservations.erase(record);
    engine_wait.notify_all();
}

#ifdef __cplusplus
}
#endif
