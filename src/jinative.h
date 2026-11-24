// This file is a part of Julia. License is MIT: https://julialang.org/license

// On-disk layout of the native code section of a .ji file.
//
// The section is written by the prelinker (jinative-prelink.cpp), which links the
// objects of an image into one position-independent blob, and loaded by
// jinative-load.c without the system dynamic loader. It does not depend on the
// object format of the platform: the only load-time work is to map three
// segments and patch two kinds of 64-bit slots in the data segment.

#ifndef JL_JINATIVE_H
#define JL_JINATIVE_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#define JL_NATIVE_MAGIC "\177JLNATV"
#define JL_NATIVE_VERSION 3

// All offsets are relative to the start of the section, which is page aligned in
// the file. For the four segments the file offset is also the offset from the
// load address.
typedef struct {
    char magic[8];
    uint32_t version;
    uint32_t page_size;
    uint64_t file_size; // size of the section in the file
    uint64_t vm_size; // address space the image occupies
    uint64_t text_off, text_size; // r-x, never written at load time
    uint64_t rodata_off, rodata_size; // r--, never written at load time
    uint64_t relro_off, relro_size; // fixed up, then made read-only (GOT, constant pointer tables)
    uint64_t data_off, data_size; // rw-
    // fixups (not mapped)
    uint64_t rebase_off; // ULEB128 deltas between slots that hold an image offset
    uint64_t bind_off; // jl_native_bind_t[nbind]
    uint64_t imports_off; // uint32_t[nimports], offsets into the string table
    uint64_t syms_off; // jl_native_sym_t[nsyms], sorted by address
    uint64_t objects_off; // jl_native_object_t[nobjects], sorted by address
    uint64_t debug_off; // jl_native_debug_section_t[ndebug]
    uint64_t strtab_off, strtab_size;
    uint32_t nrebase, nbind, nimports, nsyms, nobjects, ndebug;
    uint64_t link_base; // the address the image was linked at, which the debug info uses
    // image offsets of the symbols the loader needs (0 if absent)
    uint64_t image_pointers; // jl_image_pointers
    uint64_t image_coverage; // jl_image_coverage
    // ad-hoc code signature over [0, relro_off), which macOS needs to map the text
    uint64_t codesig_off, codesig_size;
} jl_native_header_t;

typedef struct {
    uint64_t slot; // image offset of the slot
    int64_t addend;
    uint32_t import;
    uint32_t pad;
} jl_native_bind_t;

typedef struct {
    uint64_t addr; // image offset
    uint32_t size;
    uint32_t name; // offset into the string table
} jl_native_sym_t;

// One of the objects that were linked into the image: its code, its unwind tables,
// and its debug info, which stays in the file until a backtrace asks for it.
typedef struct {
    uint64_t text_start, text_end;
    uint64_t eh_frame, eh_frame_size;
    uint64_t unwind_info, unwind_info_size; // Mach-O compact unwind
    uint32_t debug, ndebug; // range of jl_native_debug_section_t
} jl_native_object_t;

// A DWARF section, with addresses as if the image were loaded at `link_base`.
// `slides` are the offsets in it of the 64-bit addresses, for a consumer that
// needs them to be those of the loaded image (a debugger).
typedef struct {
    uint64_t off, size; // as stored
    uint64_t usize; // size once decompressed (zstd), or 0 if it is stored as it is
    uint64_t slides_off; // uint32_t[nslides]
    uint32_t name; // offset into the string table, e.g. "debug_line"
    uint32_t nslides;
} jl_native_debug_section_t;

// A loaded image
typedef struct _jl_native_image_t {
    struct _jl_native_image_t *next;
    char *base;
    const jl_native_header_t *hdr; // == base
    const char *meta; // fixup tables, symbols and strings
    char *path;
} jl_native_image_t;

#ifdef JL_DLLEXPORT
// The image and the linked object that `addr` is in
JL_DLLEXPORT const jl_native_object_t *jl_native_object_for_addr(uintptr_t addr, jl_native_image_t **pimg) JL_NOTSAFEPOINT;
#endif

#ifdef __cplusplus
}
#endif

#endif
