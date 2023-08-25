// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <inttypes.h>
#include "julia.h"
#include "julia_internal.h"
#include "timing.h"

typedef struct jl_timing_counts_event_t {
    const char *name;
    _Atomic(uint64_t) self;
    _Atomic(uint64_t) total;
} jl_timing_counts_event_t;

typedef struct _jl_timing_counts_t {
    uint64_t total;
    uint64_t start;
    uint64_t t0;
#ifdef JL_DEBUG_BUILD
    uint8_t running;
#endif
} jl_timing_counts_t;

typedef struct {
    _Atomic(uint64_t) basic_counter;
} jl_timing_counter_t;

JL_DLLEXPORT jl_timing_counter_t jl_timing_counters[JL_TIMING_COUNTER_LAST];

static arraylist_t jl_timing_counts_events;
static jl_mutex_t jl_timing_counts_events_lock;
static uint64_t t0;

static int cmp_counts_events(const void *a, const void *b) {
    jl_timing_counts_event_t *event_a = *(jl_timing_counts_event_t **)a;
    jl_timing_counts_event_t *event_b = *(jl_timing_counts_event_t **)b;
    return strcmp(event_a->name, event_b->name);
}

JL_DLLEXPORT int jl_timing_available_impl(void) {
    return 1;
}

JL_DLLEXPORT void jl_timing_counter_inc_impl(int counter, uint64_t val) JL_NOTSAFEPOINT {
    jl_atomic_fetch_add_relaxed(&jl_timing_counters[counter].basic_counter, val);
}

JL_DLLEXPORT void jl_timing_counter_dec_impl(int counter, uint64_t val) JL_NOTSAFEPOINT {
    jl_atomic_fetch_add_relaxed(&jl_timing_counters[counter].basic_counter, -(int64_t)val);
}

JL_DLLEXPORT void jl_timing_init_impl(void) {
    t0 = cycleclock();

    _Static_assert(JL_TIMING_SUBSYSTEM_LAST < sizeof(uint64_t) * CHAR_BIT, "Too many timing subsystems!");

    JL_MUTEX_INIT(&jl_timing_counts_events_lock, "jl_timing_counts_events_lock");

    // Create events list for counts backend
    arraylist_new(&jl_timing_counts_events, 1);

    jl_timing_counts_event_t *root_event = (jl_timing_counts_event_t *)malloc(sizeof(jl_timing_counts_event_t));
    arraylist_push(&jl_timing_counts_events, (void *)root_event);

    root_event->name = "ROOT";
    jl_atomic_store_relaxed(&root_event->self, 0);
    jl_atomic_store_relaxed(&root_event->total, 0);
}

JL_DLLEXPORT void jl_timing_print_impl(void) {
    qsort(jl_timing_counts_events.items, jl_timing_counts_events.len,
          sizeof(jl_timing_counts_event_t *), cmp_counts_events);

    JL_LOCK_NOGC(&jl_timing_counts_events_lock);
    uint64_t total_time = cycleclock() - t0;
    uint64_t root_time = total_time;
    jl_timing_counts_event_t *root_event;
    for (int i = 0; i < jl_timing_counts_events.len; i++) {
        jl_timing_counts_event_t *other_event = (jl_timing_counts_event_t *)jl_timing_counts_events.items[i];
        if (strcmp(other_event->name, "ROOT") == 0) {
            root_event = other_event;
        } else {
            root_time -= jl_atomic_load_relaxed(&other_event->self);
        }
    }
    jl_atomic_store_relaxed(&root_event->self, root_time);
    jl_atomic_store_relaxed(&root_event->total, total_time);

    fprintf(stderr, "\nJULIA TIMINGS\n");
    fprintf(stderr, "%-25s, %-30s, %-30s\n", "Event", "Self Cycles (% of Total)", "Total Cycles (% of Total)");
    for (int i = 0; i < jl_timing_counts_events.len; i++) {
        jl_timing_counts_event_t *event = (jl_timing_counts_event_t *)jl_timing_counts_events.items[i];
        uint64_t self = jl_atomic_load_relaxed(&event->self);
        uint64_t total = jl_atomic_load_relaxed(&event->total);
        if (total != 0)
            fprintf(stderr, "%-25s, %20" PRIu64 " (%5.2f %%), %20" PRIu64 " (%5.2f %%)\n",
                    event->name,
                    self, 100 * (((double)self) / total_time),
                    total, 100 * (((double)total) / total_time));
    }
    JL_UNLOCK_NOGC(&jl_timing_counts_events_lock);

    fprintf(stderr, "\nJULIA COUNTERS\n");
    fprintf(stderr, "%-25s, %-20s\n", "Counter", "Value");
#define X(name) do { \
        int64_t val = (int64_t) jl_atomic_load_relaxed(&jl_timing_counters[(int)JL_TIMING_COUNTER_##name].basic_counter); \
        if (val != 0) \
            fprintf(stderr, "%-25s, %20" PRIi64 "\n", #name, val); \
    } while (0);

    JL_TIMING_COUNTERS
#undef X
}
