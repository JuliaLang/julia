#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <inttypes.h>
#include "julia.h"
#include "julia_internal.h"
#include "threading.h"
#include "gc.h"


//
// Shared infrastructure
//

// data structures
volatile jl_bt_element_t *bt_data_prof = NULL;
volatile size_t bt_size_max = 0;
volatile size_t bt_size_cur = 0;
volatile uint8_t bt_overflow = 0;
/// only for sampling profiler
static volatile uint64_t profile_delay_nsec = 0;
/// only for memory profiler
static volatile uint16_t memprof_tag_filter = 0xffff;

volatile int profile_running = 0;
volatile int memprof_running = 0;

JL_DLLEXPORT void jl_profile_clear_data(void)
{
    bt_size_cur = 0;
    bt_overflow = 0;
}

JL_DLLEXPORT int jl_profile_init(size_t maxsize, uint64_t delay_nsec, uint16_t tag_filter)
{
    bt_size_max = maxsize;
    profile_delay_nsec = delay_nsec;
    memprof_tag_filter = tag_filter | JL_MEMPROF_TAG_DEALLOC;   // always track deallocs

    // Free previous profile buffers, if we have any
    if (bt_data_prof != NULL)
        free((void*)bt_data_prof);

    // Initialize new profile buffers.  We assume at least 10x
    bt_data_prof = (jl_bt_element_t*) calloc(bt_size_max, sizeof(jl_bt_element_t));
    if (bt_data_prof == NULL && bt_size_max > 0)
        return -1;

    jl_profile_clear_data();
    return 0;
}

JL_DLLEXPORT uint8_t *jl_profile_get_data(void)
{
    return (uint8_t*) bt_data_prof;
}

JL_DLLEXPORT size_t jl_profile_len_data(void)
{
    return bt_size_cur;
}

JL_DLLEXPORT size_t jl_profile_maxlen_data(void)
{
    return bt_size_max;
}

JL_DLLEXPORT int jl_profile_did_overflow(void)
{
    return bt_overflow;
}


//
// Sampling profiler
//

JL_DLLEXPORT uint64_t jl_profile_delay_nsec(void)
{
    return profile_delay_nsec;
}

JL_DLLEXPORT int jl_profile_is_running(void)
{
    return profile_running;
}

// jl_profile_start_timer and jl_profile_stop_timer defined in signal-handling.c


//
// Memory profiler
//

JL_DLLEXPORT uint16_t jl_memprofile_tag_filter(void)
{
    return memprof_tag_filter;
}

JL_DLLEXPORT int jl_memprofile_is_running(void)
{
    return memprof_running;
}

JL_DLLEXPORT void jl_memprofile_start(void)
{
    memprof_running = 1;
}

JL_DLLEXPORT void jl_memprofile_stop(void) JL_NOTSAFEPOINT
{
    memprof_running = 0;
}

// Helper function that makes it easy to take a chunk of plain-old-data that was
// allocated for an Array and find the "holding" jl_array_t object.
JL_DLLEXPORT jl_array_t * jl_memprofile_find_malloc_array(void * adata)
{
    // We walk every thread, so we need to disable the GC while we do this.
    int prev_value = jl_gc_enable(0);

    // For each thread
    for (int t_i = 0; t_i < jl_n_threads; t_i++) {
        // Get its thread-local storage
        jl_ptls_t ptls2 = jl_all_tls_states[t_i];

        // Take a look at the malloc'ed arrays for this thread
        mallocarray_t *ma = ptls2->heap.mallocarrays;

        // Zoom through seeing if the given pointer matches this array's data pointer
        while (ma != NULL) {
            if (ma->a->data == adata) {
                // If it matches, re-enable the GC and return that value
                jl_gc_enable(prev_value);
                return ma->a;
            }
            ma = ma->next;
        }
    }

    // We were unable to find it. :(
    jl_gc_enable(prev_value);
    return NULL;
}

JL_DLLEXPORT void jl_memprofile_track_alloc(void *v, uint16_t tag, size_t allocsz, void *ty) JL_NOTSAFEPOINT
{
    // Filter out this call with our tag filter
    if ((tag & memprof_tag_filter) != tag)
        return;

    // Store the current backtrace location into our buffer, and increment the
    // buffer index by the number of elements added.
    size_t bt_size_step = 0;
    int incomplete = rec_backtrace((jl_bt_element_t*) bt_data_prof + bt_size_cur,
                                   &bt_size_step, bt_size_max - bt_size_cur - 1,
                                   0, 1);

    // If we overran this buffer or don't have the place to store the allocation info frame,
    // then don't record the memory trace and quit.
    size_t alloc_size_step = 7;
    if (incomplete || bt_size_cur + bt_size_step
                                  + alloc_size_step >= bt_size_max) {
        bt_overflow |= JL_MEMPROF_BT_OVERFLOW;
        jl_memprofile_stop();
        return;
    }

    // Store an allocation information frame into the buffer
    uintptr_t entry_tags = jl_bt_entry_descriptor(1, 4, JL_BT_ALLOCINFO_FRAME_TAG, 0);
    jl_bt_element_t *bt_entry = (jl_bt_element_t*) bt_data_prof + bt_size_cur + bt_size_step;
    bt_entry[0].uintptr = JL_BT_NON_PTR_ENTRY;
    bt_entry[1].uintptr = entry_tags;
    // The location of the type information for this chunk of memory (tracked value)
    bt_entry[2].jlvalue = (jl_value_t*)(ty ? ty : jl_nothing);
    // The location of the data in memory, used to match allocations with deallocations.
    bt_entry[3].uintptr = (uintptr_t) v;
    // The time at which this happened
    bt_entry[4].uintptr = jl_clock_now();   // FIXME: double to a potentially 32-bit uintptr
    // The size of the allocation, or 0 if this was a free.
    bt_entry[5].uintptr = allocsz;
    // Used to "tag" this allocation within a particular domain (CPU, GPU, other)
    // or within a particular allocator (Pool, std, malloc), or as a free instead.
    bt_entry[6].uintptr = tag;

    // Add a NULL-separator
    bt_size_cur += bt_size_step + alloc_size_step;
    bt_data_prof[bt_size_cur++].uintptr = 0;
    assert(bt_size_cur < bt_size_max);
}

JL_DLLEXPORT void jl_memprofile_set_typeof(void * v, void * ty) JL_NOTSAFEPOINT
{
    if (__unlikely(jl_memprofile_is_running())) {
        if (bt_size_cur > 0) {
            assert(bt_size_cur >= 8);  // one full allocation info frame + end-of-block NULL
            // if the type is valid (the field defaults to jl_nothing)
            // and the memory location matches, update the type field
            // FIXME: are there other types we shouldn't save because they can't be scanned?
            if (ty && ty != (void*) jl_buff_tag &&
                bt_data_prof[bt_size_cur-5].uintptr == (uintptr_t) v)
                bt_data_prof[bt_size_cur-6].jlvalue = (jl_value_t*) ty;
        }
    }
}

JL_DLLEXPORT void jl_memprofile_track_dealloc(void *v, uint16_t tag) JL_NOTSAFEPOINT
{
    jl_memprofile_track_alloc(v, tag | JL_MEMPROF_TAG_DEALLOC, 0, NULL);
}
