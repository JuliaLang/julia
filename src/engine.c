// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "uv.h"
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

typedef struct {
    jl_method_instance_t *mi;
    jl_value_t *owner;
} infer_key_t;

typedef struct {
    int16_t tid;
    jl_code_instance_t *ci;
} reservation_info_t;

// map from MethodInstance to threadid that owns it currently for inference
static htable_t native_reservations;
// map from (foreign) owner to reservation map (which is a map from MI to threadid)
static htable_t foreign_reservations;

static uv_mutex_t engine_lock; // n.b. this lock is only ever held briefly
static uv_cond_t engine_wait; // but it may be waiting a while in this state
// vector of which threads are blocked and which lease they need
static arraylist_t awaiting; // (this could be merged into ptls also)

static htable_t *reservation_map_for_owner(jl_value_t *owner) JL_NOTSAFEPOINT
{
    if (owner == jl_nothing)
        return &native_reservations;
    htable_t **reservations = (htable_t **)ptrhash_bp(&foreign_reservations, owner);
    if (*reservations == HT_NOTFOUND)
        *reservations = htable_new((htable_t*)malloc_s(sizeof(htable_t)), 1);
    return *reservations;
}

static reservation_info_t *get_reservation(jl_method_instance_t *mi, jl_value_t *owner) JL_NOTSAFEPOINT
{
    htable_t *reservations = reservation_map_for_owner(owner);
    return (reservation_info_t *)ptrhash_get(reservations, mi);
}

static void remove_reservation(jl_method_instance_t *mi, jl_value_t *owner) JL_NOTSAFEPOINT
{
    htable_t *reservations = reservation_map_for_owner(owner);
    ptrhash_remove(reservations, mi);
}

static reservation_info_t *try_insert_reservation(infer_key_t key, reservation_info_t to_insert, int *inserted) JL_NOTSAFEPOINT
{
    htable_t *reservations = reservation_map_for_owner(key.owner);
    reservation_info_t **record = (reservation_info_t **)ptrhash_bp(reservations, key.mi);
    reservation_info_t *info = *record;
    if (info == HT_NOTFOUND) {
        info = (reservation_info_t *)malloc_s(sizeof(reservation_info_t));
        *info = to_insert;
        *record = info;
        *inserted = 1;
        return info;
    }
    *inserted = 0;
    return info;
}

jl_code_instance_t *jl_engine_reserve(jl_method_instance_t *mi, jl_value_t *owner)
{
    jl_task_t *ct = jl_current_task;
    ct->ptls->engine_nqueued++; // disables finalizers until inference is finished on this method graph
    jl_code_instance_t *ci = jl_new_codeinst_uninit(mi, owner); // allocate a placeholder
    JL_GC_PUSH1(&ci);
    int16_t tid = jl_atomic_load_relaxed(&ct->tid);
    int cond = 0;
    {
        int8_t gc_state = jl_gc_safe_enter(ct->ptls); // contains jl_gc_safepoint after enter
        uv_mutex_lock(&engine_lock);
        size_t max_tid = awaiting.len;
        if (max_tid < (size_t)tid + 1) {
            size_t n = (size_t)tid + 1 - max_tid;
            arraylist_grow(&awaiting, n);
            memset(&awaiting.items[max_tid], 0, n * sizeof(void *));
            max_tid += n;
        }
        while (1) {
            int inserted = 0;
            reservation_info_t *info = try_insert_reservation(
                (infer_key_t){ mi, owner }, (reservation_info_t){ tid, ci }, &inserted
            );
            if (inserted)
                break;
            // before waiting, need to run deadlock/cycle detection
            // there is a cycle if the thread holding our lease is blocked
            // and waiting for (transitively) any lease that is held by this thread
            int16_t wait_tid = info->tid;
            while (1) {
                if (wait_tid == tid) {
                    cond = 1;
                    break;
                }
                if (awaiting.len <= (size_t)wait_tid)
                    break; // no cycle, since it is running (and this should be unreachable)
                infer_key_t *key2 = (infer_key_t *)awaiting.items[wait_tid];
                if (key2 == NULL)
                    break; // no cycle, since it is running
                reservation_info_t *info2 = get_reservation(key2->mi, key2->owner);
                if (info2 == HT_NOTFOUND)
                    break; // no cycle, since it is about to resume
                assert(wait_tid != info2->tid);
                wait_tid = info2->tid;
            }
            if (cond)
                break;
            infer_key_t *key = (infer_key_t *)malloc_s(sizeof(infer_key_t));
            key->mi = mi;
            key->owner = owner;
            awaiting.items[tid] = key;
            uv_cond_wait(&engine_wait, &engine_lock);
            awaiting.items[tid] = NULL;
            free(key);
        }
        uv_mutex_unlock(&engine_lock);
        jl_gc_safe_leave(ct->ptls, gc_state); // contains jl_gc_safepoint after leave
    }
    if (cond) {
        ct->ptls->engine_nqueued--;
    }
    JL_GC_POP();
    return ci;
}

int jl_engine_hasreserved(jl_method_instance_t *mi, jl_value_t *owner)
{
    jl_task_t *ct = jl_current_task;
    uv_mutex_lock(&engine_lock);
    reservation_info_t *info = get_reservation(mi, owner);
    int found = info != HT_NOTFOUND && info->tid == jl_atomic_load_relaxed(&ct->tid);
    uv_mutex_unlock(&engine_lock);
    return found;
}

STATIC_INLINE int gc_marked(uintptr_t bits) JL_NOTSAFEPOINT
{
    return (bits & GC_MARKED) != 0;
}

static int sweep_reservations(htable_t *h, jl_ptls_t *gc_all_tls_states) JL_NOTSAFEPOINT
{
    int any = 0;
    for (size_t i = 0; i < h->size; i += 2) {
        if (h->table[i+1] == HT_NOTFOUND)
            continue;
        reservation_info_t *info = (reservation_info_t *)h->table[i+1];
        jl_code_instance_t *ci = info->ci;
        if (!gc_marked(jl_astaggedvalue(ci)->bits.gc)) {
            int16_t tid = info->tid;
            h->table[i+1] = HT_NOTFOUND; // remove (tombstone)
            h->live--;
            free(info);
            jl_ptls_t ptls2 = gc_all_tls_states[tid];
            ptls2->engine_nqueued--;
            any = 1;
        }
    }
    return any;
}

void jl_engine_sweep(jl_ptls_t *gc_all_tls_states)
{
    uv_mutex_lock(&engine_lock);
    int any = sweep_reservations(&native_reservations, gc_all_tls_states);
    for (size_t i = 0; i < foreign_reservations.size; i += 2) {
        if (foreign_reservations.table[i+1] == HT_NOTFOUND)
            continue;
        htable_t *reservations = (htable_t *)foreign_reservations.table[i+1];
        if (sweep_reservations(reservations, gc_all_tls_states))
            any = 1;
    }
    if (any)
        uv_cond_broadcast(&engine_wait);
    uv_mutex_unlock(&engine_lock);
}

void jl_engine_fulfill(jl_code_instance_t *ci, jl_code_info_t *src)
{
    jl_task_t *ct = jl_current_task;
    uv_mutex_lock(&engine_lock);
    jl_method_instance_t *mi = jl_get_ci_mi(ci);
    reservation_info_t *info = get_reservation(mi, ci->owner);
    if (info == HT_NOTFOUND || info->ci != ci) {
        uv_mutex_unlock(&engine_lock);
        return;
    }
    assert(jl_atomic_load_relaxed(&ct->tid) == info->tid);
    ct->ptls->engine_nqueued--; // re-enables finalizers, but doesn't immediately try to run them
    remove_reservation(mi, ci->owner);
    free(info);
    uv_cond_broadcast(&engine_wait);
    uv_mutex_unlock(&engine_lock);
}

void jl_init_engine(void)
{
    uv_mutex_init(&engine_lock);
    uv_cond_init(&engine_wait);
    htable_new(&native_reservations, 0);
    htable_new(&foreign_reservations, 0);
    arraylist_new(&awaiting, 0);
}
