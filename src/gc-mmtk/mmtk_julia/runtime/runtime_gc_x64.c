#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

long JULIA_HEADER_SIZE = 0;
unsigned long JULIA_BUFF_TAG = 0;

extern void mmtk_start_spawned_worker_thread(void*, void*);
extern void mmtk_start_spawned_controller_thread(void*, void*);

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
    mmtk_start_spawned_worker_thread(tls, ctx);
    return NULL;
}

void *fn_spawn_controller_thread(void* args) {
    struct thread_args *ta = (struct thread_args *)  args;
    void* tls = (*ta).tls;
    void* ctx = (*ta).ctx;
    mmtk_start_spawned_controller_thread(tls, ctx);
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

void set_julia_obj_header_size_and_buffer_tag(long size, long tag) {
    JULIA_HEADER_SIZE = size;
    JULIA_BUFF_TAG = tag; 
}
