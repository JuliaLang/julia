#ifndef MMTK_JULIA_MMTK_MUTATOR_H
#define MMTK_JULIA_MMTK_MUTATOR_H

// mmtk_julia_types.h refers to the types in this file.
// So if this file is updated, make sure you regenerate Rust types for mmtk_julia_types.h.

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
#define MAX_BUMP_ALLOCATORS 6
#define MAX_LARGE_OBJECT_ALLOCATORS 2
#define MAX_MALLOC_ALLOCATORS 1
#define MAX_IMMIX_ALLOCATORS 1
#define MAX_FREE_LIST_ALLOCATORS 2
#define MAX_MARK_COMPACT_ALLOCATORS 1

// The following types should have the same layout as the types with the same name in MMTk core (Rust)

typedef struct {
  void* tls;
  void* cursor;
  void* limit;
  RustDynPtr space;
  void* context;
} BumpAllocator;

typedef struct {
  void* tls;
  void* space;
  void* context;
} LargeObjectAllocator;

typedef struct {
  void* tls;
  void* cursor;
  void* limit;
  void* immix_space;
  void* context;
  uint8_t hot;
  uint8_t copy;
  void* large_cursor;
  void* large_limit;
  uint8_t request_for_large;
  uint8_t _align[7];
  uint8_t line_opt_tag;
  uintptr_t line_opt;
} ImmixAllocator;

typedef struct {
  void* Address;
} FLBlock;

typedef struct {
  FLBlock first;
  FLBlock last;
  size_t size;
  char lock;
} FLBlockList;

typedef struct {
  void* tls;
  void* space;
  void* context;
  FLBlockList* available_blocks;
  FLBlockList* available_blocks_stress;
  FLBlockList* unswept_blocks;
  FLBlockList* consumed_blocks;
} FreeListAllocator;

typedef struct {
  void* tls;
  void* space;
  void* context;
} MMTkMallocAllocator; // Prefix with MMTk to avoid name clash

typedef struct {
  BumpAllocator bump_allocator;
} MarkCompactAllocator;

typedef struct {
  BumpAllocator bump_pointer[MAX_BUMP_ALLOCATORS];
  LargeObjectAllocator large_object[MAX_LARGE_OBJECT_ALLOCATORS];
  MMTkMallocAllocator malloc[MAX_MALLOC_ALLOCATORS];
  ImmixAllocator immix[MAX_IMMIX_ALLOCATORS];
  FreeListAllocator free_list[MAX_FREE_LIST_ALLOCATORS];
  MarkCompactAllocator markcompact[MAX_MARK_COMPACT_ALLOCATORS];
} Allocators;

typedef struct {
  void* allocator_mapping;
  void* space_mapping;
  RustDynPtr prepare_func;
  RustDynPtr release_func;
} MutatorConfig;

typedef struct {
  Allocators allocators;
  RustDynPtr barrier;
  void* mutator_tls;
  RustDynPtr plan;
  MutatorConfig config;
} MMTkMutatorContext;

#endif // MMTK_MUTATOR_HPP