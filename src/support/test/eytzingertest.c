// This file is a part of Julia. License is MIT: https://julialang.org/license

// Regression tests for the Eytzinger address-range tree (see eytzinger.h).
// Covers the object-pointer (start, end] containment convention (a blob's
// base is a tag word, never an object; a trailing zero-size singleton's value
// pointer equals the blob's end — issue #62521) and the abutting ranges that
// were mishandled in issue #61385.

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "../eytzinger.h"

static int in_range(eyt_tree_t *t, uintptr_t a) { return eyt_tree_is_in_range(t, a); }
static void *find(eyt_tree_t *t, uintptr_t a) { return eyt_tree_find_data(t, a); }

// Brute-force reference over a set of non-overlapping (possibly abutting) ranges.
typedef struct { uintptr_t s, e; void *d; } rng_t;
static void *ref_find(rng_t *ranges, int nr, uintptr_t a) {
    for (int i = 0; i < nr; i++)
        if (a > ranges[i].s && a <= ranges[i].e)
            return ranges[i].d;
    return EYT_NOTFOUND;
}

int main(void)
{
    // Empty tree: nothing is in range.
    {
        eyt_tree_t t; eyt_tree_init(&t);
        assert(!in_range(&t, 0));
        assert(!in_range(&t, 100));
        assert(find(&t, 100) == EYT_NOTFOUND);
    }

    // Single range [100, 200): start inclusive, end exclusive.
    {
        eyt_tree_t t; eyt_tree_init(&t);
        eyt_tree_add_range(&t, 100, 200, (void*)0x10);
        assert(!in_range(&t, 96));   // before
        assert(!in_range(&t, 100));  // start (exclusive: blob base is a tag word)
        assert(in_range(&t, 104));
        assert(in_range(&t, 196));   // last element
        assert(in_range(&t, 200));   // end (inclusive: trailing zero-size singleton)
        assert(!in_range(&t, 204));  // after
        assert(find(&t, 100) == EYT_NOTFOUND);
        assert(find(&t, 196) == (void*)0x10);
        assert(find(&t, 200) == (void*)0x10);
    }

    // Two disjoint ranges [100, 200) and [300, 400).
    {
        eyt_tree_t t; eyt_tree_init(&t);
        eyt_tree_add_range(&t, 100, 200, (void*)0x10);
        eyt_tree_add_range(&t, 300, 400, (void*)0x20);
        assert(!in_range(&t, 100));
        assert(in_range(&t, 104) && find(&t, 104) == (void*)0x10);
        assert(in_range(&t, 200) && find(&t, 200) == (void*)0x10);
        assert(!in_range(&t, 252));  // gap
        assert(!in_range(&t, 300));
        assert(in_range(&t, 304) && find(&t, 304) == (void*)0x20);
        assert(in_range(&t, 396));
        assert(in_range(&t, 400) && find(&t, 400) == (void*)0x20);
        assert(!in_range(&t, 404));
    }

    // Abutting ranges (100, 200] and (200, 300]: the shared boundary belongs
    // to the *first* range (a trailing zero-size singleton of blob 1, never an
    // object of blob 2, whose base is its first object's tag word).
    {
        eyt_tree_t t; eyt_tree_init(&t);
        eyt_tree_add_range(&t, 100, 200, (void*)0x10);
        eyt_tree_add_range(&t, 200, 300, (void*)0x20);
        assert(!in_range(&t, 100));
        assert(in_range(&t, 196) && find(&t, 196) == (void*)0x10);
        assert(in_range(&t, 200) && find(&t, 200) == (void*)0x10);
        for (uintptr_t a = 204; a <= 300; a += 4)
            assert(in_range(&t, a) && find(&t, a) == (void*)0x20);
        assert(!in_range(&t, 304));
    }

    // Ranges added out of address order are still handled correctly.
    {
        eyt_tree_t t; eyt_tree_init(&t);
        eyt_tree_add_range(&t, 300, 400, (void*)0x20);
        eyt_tree_add_range(&t, 100, 200, (void*)0x10);
        eyt_tree_add_range(&t, 200, 300, (void*)0x30);  // abuts both neighbors
        assert(find(&t, 104) == (void*)0x10);
        assert(find(&t, 200) == (void*)0x10);
        assert(find(&t, 300) == (void*)0x30);
        assert(find(&t, 400) == (void*)0x20);
        assert(!in_range(&t, 404));
        assert(!in_range(&t, 100));
    }

    // Deterministic fuzz: random non-overlapping ranges (some abutting), added
    // in random order, exhaustively compared against the brute-force reference.
    {
        srand(20250709);
        for (int trial = 0; trial < 3000; trial++) {
            int nr = 1 + rand() % 6;
            rng_t ranges[8];
            uintptr_t cur = 4 * (1 + rand() % 10);
            eyt_tree_t t; eyt_tree_init(&t);
            for (int i = 0; i < nr; i++) {
                uintptr_t gap = 4 * (rand() % 4);      // 0 (abut) .. 12
                uintptr_t len = 4 * (1 + rand() % 8);  // 4 .. 32
                cur += gap;
                ranges[i].s = cur;
                ranges[i].e = cur + len;
                ranges[i].d = (void*)(uintptr_t)(0x100 + i);
                cur += len;
            }
            int order[8];
            for (int i = 0; i < nr; i++) order[i] = i;
            for (int i = nr - 1; i > 0; i--) {
                int j = rand() % (i + 1);
                int tmp = order[i]; order[i] = order[j]; order[j] = tmp;
            }
            for (int i = 0; i < nr; i++)
                eyt_tree_add_range(&t, ranges[order[i]].s, ranges[order[i]].e, ranges[order[i]].d);

            uintptr_t lo = ranges[0].s > 16 ? ranges[0].s - 16 : 0;
            uintptr_t hi = ranges[nr - 1].e + 16;
            for (uintptr_t a = lo; a <= hi; a += 4) {
                void *ref = ref_find(ranges, nr, a);
                assert(find(&t, a) == ref);
                assert(in_range(&t, a) == (ref != EYT_NOTFOUND));
            }
        }
    }

    printf("eytzingertest: all tests passed\n");
    return 0;
}
