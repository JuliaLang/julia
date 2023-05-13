# Garbage Collection in Julia

## Summary

We present in this document an overview of Julia's garbage collector, and describe
instrumentation that can be used to gather useful statistics about garbage collection
performance in Julia.

Our overview of Julia's garbage collector has been roughly divided into: a discussion of our
dynamic memaory allocation algorithm, a discussion of the mechanism that we use to halt
Julia compute threads whenever a collection is about to start (e.g. safepoints) and a
discussion of our mark-sweep algorithm, focusing on the implementation of generational
behavior in the collector.

The instrumentation section focuses on describing the metrics exported through Julia's
`gc_num` that can be used to gather statistics about garbage collection performance, ranging
from number of allocated bytes throughout the program execution to length of
stop-the-world pauses.

There are a number of other very useful tools that can be used to instrument the garbage
collector and the Julia heap (e.g. BPFTrace probes and heap snapshots). We won't be focusing
on them in this document, but more information about these tools can be found in other
chapters of the developer documentation.

## Introduction

Julia has a stop-the-world, generational, non-moving, precise mark-sweep garbage collector.
The mark phase may run in parallel with up to `--gcthreads` threads, while sweeping is
serial.

## Dynamic Memory Allocation Algorithm

Sufficiently small objects (up to 2032 bytes) are allocated on per-thread object
pools.

A three-level tree (analogous to a three-level page-table) is used to keep metadata
(e.g. whether a page has been allocated, whether contains marked objects, number of free objects etc.)
about address ranges spanning at least one page.
Sweeping a pool allocated object consists of inserting it back into the free list
maintained by its pool.

Two lists are used to keep track of the remaining allocated objects:
one for sufficiently large malloc'd arrays (`mallocarray_t`) and one for
sufficiently large objects (`bigval_t`).

Sweeping these objects consists of unlinking them from their list and calling `free` on the
corresponding address.

## Generational and remembered sets

Field writes into old objects trigger a write barrier if the written field
points to a young object and if a write barrier has not been triggered on the old object yet.
In this case, the old object being written to is enqueued into a remembered set, and
its mark bit is set to indicate that a write barrier has already been triggered on it.

There is no explicit flag to determine whether a marking pass will scan the
entire heap or only through young objects and remembered set.
The mark bits of the objects themselves are used to determine whether a full mark happens.
The mark-sweep algorithm follows this sequence of steps:

- Objects in the remembered set have their GC mark bits reset
(these are set once write barrier is triggered, as described above) and are enqueued.

- Roots (e.g. thread locals) are enqueued.

- Object graph is traversed and mark bits are set.

- Object pools, malloc'd arrays and big objects are sweeped. On a full sweep,
the mark bits of all marked objects are reset. On a generational sweep,
only the mark bits of marked young objects are reset.

- Mark bits of objects in the remembered set are set,
so we don't trigger the write barrier on them again.

After these stages, old objects will be left with their mark bits set,
so that references from them are not explored in a subsequent generational collection.
This scheme eliminates the need of explicitly keeping a flag to indicate a full mark
(though a flag to indicate a full sweep is necessary).

## Memory layout of objects and GC bits

An opaque tag is stored in the front of GC managed objects, and its lowest two bits are
used for garbage collection.  The lowest bit is set for marked objects and the second
lowest bit stores age information (e.g. it's only set for old objects).

Objects are aligned by a multiple of 4 bytes to ensure this pointer tagging is legal.

## Heuristics

GC heuristics tune the GC by changing the size of the allocation interval between garbage collections.
If a GC was unproductive, then we increase the size of the allocation interval to allow objects more time to die.
If a GC returns a lot of space we can shrink the interval. The goal is to find a steady state where we are
allocating just about the same amount as we are collecting.
