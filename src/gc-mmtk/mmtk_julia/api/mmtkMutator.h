#ifndef MMTK_JULIA_MMTK_MUTATOR_H
#define MMTK_JULIA_MMTK_MUTATOR_H

enum Allocator {
  AllocatorDefault = 0,
  AllocatorImmortal = 1,
  AllocatorLos = 2,
  AllocatorCode = 3,
  AllocatorReadOnly = 4,
};

typedef struct {
  void* data;
  void* vtable;
} RustDynPtr;

// These constants should match the constants defind in mmtk::util::alloc::allocators
// const int MAX_BUMP_ALLOCATORS = 6;
// const int MAX_LARGE_OBJECT_ALLOCATORS = 2;
// const int MAX_MALLOC_ALLOCATORS = 1;
// const int MAX_IMMIX_ALLOCATORS = 1;
// const int MAX_MARK_COMPACT_ALLOCATORS = 1;

// The following types should have the same layout as the types with the same name in MMTk core (Rust)

typedef struct {
  void* tls;
  void* cursor;
  void* limit;
  RustDynPtr space;
  RustDynPtr plan;
} BumpAllocator;

typedef struct {
  void* tls;
  void* space;
  RustDynPtr plan;
} LargeObjectAllocator;

typedef struct {
  void* tls;
  void* cursor;
  void* limit;
  void* immix_space;
  RustDynPtr plan;
  uint8_t hot;
  uint8_t copy;
  void* large_cursor;
  void* large_limit;
  uint8_t request_for_large;
  uint8_t _align[7];
  uint8_t line_opt_tag;
  uintptr_t line_opt;
  uint8_t alloc_slow_for_stress;
} ImmixAllocator;

typedef struct {
  void* tls;
  void* space;
  RustDynPtr plan;
} MMTkMallocAllocator;

typedef struct {
  BumpAllocator bump_allocator;
} MarkCompactAllocator;

typedef struct {
  BumpAllocator bump_pointer[6];
  LargeObjectAllocator large_object[2];
  MMTkMallocAllocator malloc[1];
  ImmixAllocator immix[1];
  MarkCompactAllocator markcompact[1];
} Allocators;

typedef struct {
  void* allocator_mapping;
  void* space_mapping;
  RustDynPtr prepare_func;
  RustDynPtr release_func;
} MutatorConfig;

typedef struct {
  Allocators allocators;
  void* barrier;
  void* mutator_tls;
  RustDynPtr plan;
  MutatorConfig config;
} MMTkMutatorContext;
#endif // MMTK_MUTATOR_HPP