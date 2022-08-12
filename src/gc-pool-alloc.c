// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc-markqueue.h"
#include "gc.h"

#ifdef __cplusplus
extern "C" {
#endif

static size_t pow2[10] = {1, 2, 4, 8. 16, 32, 64, 128, 256, 512, 1024};

static ws_queue_t gc_recycled_pages;

jl_taggedvalue_t *gc_pool_alloc(jl_ptls_t ptls, size_t sz)
{
	int i = 0;
	for (; pow2[i] < sz; i++)
		;

	jl_thread_heap_t *pheap = &ptls->heap;
	jl_gc_pagedir_entry_t *entry = &pheap->pa_dir_entries[i];

	jl_taggedvalue_t *obj;

	if (entry->head->pfl != NULL) {
		obj = entry->head->pfl;
		entry->head->pfl = obj->next;
	}

	return obj;
}

#ifdef __cplusplus
}
#endif