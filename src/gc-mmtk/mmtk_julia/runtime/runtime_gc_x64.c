#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

long JULIA_HEADER_SIZE = 0;
long BI_METADATA_START_ALIGNED_DOWN = 0;
long BI_METADATA_END_ALIGNED_UP = 0;

void init_boot_image_metadata_info(long start_aligned_down, long end_aligned_up) {
    BI_METADATA_START_ALIGNED_DOWN = start_aligned_down;
    BI_METADATA_END_ALIGNED_UP = end_aligned_up;
}

void* get_mutator_ref(void* mutator) {
    return mutator;
}

void* get_mutator_from_ref(void* mutator) {
    return mutator;
}

int mutator_cursor = 0;

void reset_mutator_count() {
    mutator_cursor = 0;
}

int get_next_julia_mutator() {
    return mutator_cursor++;
}

extern void start_spawned_worker_thread(void*, void*);
extern void start_spawned_controller_thread(void*, void*);

struct thread_args {
    void* tls;
    void* ctx;
};

struct tls_collector {
    void* tls;
    void* collector;
};

void *fn_spawn_worker_thread(void* args) {
    struct thread_args *ta = (struct thread_args *)  args;
    void* tls = (*ta).tls;
    void* ctx = (*ta).ctx;
    start_spawned_worker_thread(tls, ctx);
    return NULL;
}

void *fn_spawn_controller_thread(void* args) {
    struct thread_args *ta = (struct thread_args *)  args;
    void* tls = (*ta).tls;
    void* ctx = (*ta).ctx;
    start_spawned_controller_thread(tls, ctx);
    return NULL;
}

void spawn_collector_thread(void *tls, void *ctx, int kind) {
    pthread_t thread;
    struct thread_args *args = (struct thread_args *)malloc(sizeof(struct thread_args));
    struct tls_collector *new_tls = (struct tls_collector *)malloc(sizeof(struct tls_collector));
    new_tls->tls = tls;
    new_tls->collector = ctx;
    args->tls = (void*) new_tls;
    args->ctx = ctx;
    if (kind == 0) {
        pthread_create(&thread, NULL, fn_spawn_controller_thread, (void*)args);
    } else {
        pthread_create(&thread, NULL, fn_spawn_worker_thread, (void*)args);
    }
}

void set_julia_obj_header_size(long size) {
    JULIA_HEADER_SIZE = size;
}
