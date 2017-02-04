// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  threading infrastructure
  . threadgroup abstraction
  . fork/join/barrier
*/

#include <stdlib.h>
#include <string.h>

#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

#include "options.h"
#include "threadgroup.h"

int ti_threadgroup_create(uint8_t num_sockets, uint8_t num_cores,
                          uint8_t num_threads_per_core,
                          ti_threadgroup_t **newtg)
{
    int i;
    ti_threadgroup_t *tg;
    int num_threads = num_sockets * num_cores * num_threads_per_core;
    char *cp;

    tg = (ti_threadgroup_t*)jl_malloc_aligned(sizeof(ti_threadgroup_t), 64);
    tg->tid_map = (int16_t*)jl_malloc_aligned(num_threads * sizeof(int16_t), 64);
    for (i = 0;  i < num_threads;  ++i)
        tg->tid_map[i] = -1;
    tg->num_sockets = num_sockets;
    tg->num_cores = num_cores;
    tg->num_threads_per_core = num_threads_per_core;
    tg->num_threads = num_threads;
    tg->added_threads = 0;
    tg->thread_sense = (ti_thread_sense_t**)
        jl_malloc_aligned(num_threads * sizeof(ti_thread_sense_t*), 64);
    for (i = 0;  i < num_threads;  i++)
        tg->thread_sense[i] = NULL;
    jl_atomic_store_release(&tg->group_sense, 0);

    uv_mutex_init(&tg->alarm_lock);
    uv_cond_init(&tg->alarm);

    tg->sleep_threshold = DEFAULT_THREAD_SLEEP_THRESHOLD;
    cp = getenv(THREAD_SLEEP_THRESHOLD_NAME);
    if (cp) {
        if (!strncasecmp(cp, "infinite", 8))
            tg->sleep_threshold = 0;
        else
            tg->sleep_threshold = (uint64_t)strtol(cp, NULL, 10);
    }

    *newtg = tg;
    return 0;
}

int ti_threadgroup_addthread(ti_threadgroup_t *tg, int16_t ext_tid,
                             int16_t *tgtid)
{
    if (ext_tid < 0 || ext_tid >= tg->num_threads)
        return -1;
    if (tg->tid_map[ext_tid] != -1)
        return -2;
    if (tg->added_threads == tg->num_threads)
        return -3;

    tg->tid_map[ext_tid] = tg->added_threads++;
    if (tgtid) *tgtid = tg->tid_map[ext_tid];

    return 0;
}

int ti_threadgroup_initthread(ti_threadgroup_t *tg, int16_t ext_tid)
{
    ti_thread_sense_t *ts;

    if (ext_tid < 0 || ext_tid >= tg->num_threads)
        return -1;
    if (tg->thread_sense[tg->tid_map[ext_tid]] != NULL)
        return -2;
    if (tg->num_threads == 0)
        return -3;

    ts = (ti_thread_sense_t*)jl_malloc_aligned(sizeof(ti_thread_sense_t), 64);
    ts->sense = 1;
    tg->thread_sense[tg->tid_map[ext_tid]] = ts;

    return 0;
}

int ti_threadgroup_member(ti_threadgroup_t *tg, int16_t ext_tid, int16_t *tgtid)
{
    if (ext_tid < 0 || ext_tid >= tg->num_threads)
        return -1;
    if (tg == NULL) {
        if (tgtid) *tgtid = -1;
        return -2;
    }
    if (tg->tid_map[ext_tid] == -1) {
        if (tgtid) *tgtid = -1;
        return -3;
    }
    if (tgtid) *tgtid = tg->tid_map[ext_tid];

    return 0;
}

int ti_threadgroup_size(ti_threadgroup_t *tg, int16_t *tgsize)
{
    *tgsize = tg->num_threads;
    return 0;
}

int ti_threadgroup_fork(ti_threadgroup_t *tg, int16_t ext_tid, void **bcast_val)
{
    uint8_t *group_sense = &tg->group_sense;
    int16_t tid = tg->tid_map[ext_tid];
    int thread_sense = tg->thread_sense[tid]->sense;
    if (tid == 0) {
        tg->envelope = bcast_val ? *bcast_val : NULL;
        // synchronize `tg->envelope` and `tg->group_sense`
        jl_atomic_store_release(group_sense, thread_sense);

        // if it's possible that threads are sleeping, signal them
        if (tg->sleep_threshold) {
            uv_mutex_lock(&tg->alarm_lock);
            uv_cond_broadcast(&tg->alarm);
            uv_mutex_unlock(&tg->alarm_lock);
        }
    }
    else {
        // spin up to threshold ns (count sheep), then sleep
        uint64_t spin_ns;
        uint64_t spin_start = 0;
        // synchronize `tg->envelope` and `tg->group_sense`
        while (jl_atomic_load_acquire(group_sense) != thread_sense) {
            if (tg->sleep_threshold) {
                if (!spin_start) {
                    // Lazily initialize spin_start since uv_hrtime is expensive
                    spin_start = uv_hrtime();
                    continue;
                }
                spin_ns = uv_hrtime() - spin_start;
                // In case uv_hrtime is not monotonic, we'll sleep earlier
                if (spin_ns >= tg->sleep_threshold) {
                    uv_mutex_lock(&tg->alarm_lock);
                    if (jl_atomic_load_acquire(group_sense) != thread_sense) {
                        uv_cond_wait(&tg->alarm, &tg->alarm_lock);
                    }
                    uv_mutex_unlock(&tg->alarm_lock);
                    spin_start = 0;
                    continue;
                }
            }
            jl_cpu_pause();
        }
        if (bcast_val)
            *bcast_val = tg->envelope;
    }

    return 0;
}

int ti_threadgroup_join(ti_threadgroup_t *tg, int16_t ext_tid)
{
    int *p_thread_sense = &tg->thread_sense[tg->tid_map[ext_tid]]->sense;
    jl_atomic_store_release(p_thread_sense, !*p_thread_sense);
    if (tg->tid_map[ext_tid] == 0) {
        jl_ptls_t ptls = jl_get_ptls_states();
        int8_t group_sense = tg->group_sense;
        for (int i = 1; i < tg->num_threads; ++i) {
            while (jl_atomic_load_acquire(&tg->thread_sense[i]->sense) == group_sense) {
                jl_gc_safepoint_(ptls);
                jl_cpu_pause();
            }
        }
    }

    return 0;
}

int ti_threadgroup_destroy(ti_threadgroup_t *tg)
{
    int i;

    uv_mutex_destroy(&tg->alarm_lock);
    uv_cond_destroy(&tg->alarm);

    for (i = 0;  i < tg->num_threads;  i++)
        jl_free_aligned(tg->thread_sense[i]);
    jl_free_aligned(tg->thread_sense);
    jl_free_aligned(tg->tid_map);
    jl_free_aligned(tg);

    return 0;
}

#ifdef __cplusplus
}
#endif
