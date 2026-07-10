#include "../src/support/platform.h"
#include "loader.h"

#ifdef _OS_LINUX_

#include <stdio.h>

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include <linux/elf.h>

static Elf64_Shdr *Elf64_get_section(Elf64_Ehdr *hdr, size_t i)
{
    size_t section_header_sz = hdr->e_shentsize;
    size_t byte_offset = hdr->e_shoff + i * section_header_sz;
    return (Elf64_Shdr *)&((char *)hdr)[byte_offset];
}

static const char *Elf64_get_strtab(Elf64_Ehdr *hdr, Elf64_Shdr *section)
{
    return &((const char *)hdr)[section->sh_offset];
}

static Elf64_Sym *Elf64_get_symbol(Elf64_Ehdr *hdr, Elf64_Shdr *section, size_t i)
{
    size_t byte_offset = section->sh_offset + i * section->sh_entsize;
    return (Elf64_Sym *)&((char *)hdr)[byte_offset];
}

static int Elf64_locate_symbol(Elf64_Ehdr *hdr, const char *symbol)
{
    if (hdr->e_type != ET_DYN)
        return 0;

    for (size_t sect_idx = 0; sect_idx < hdr->e_shnum; sect_idx++) {
        Elf64_Shdr *shdr = Elf64_get_section(hdr, sect_idx);
        if (shdr->sh_type != SHT_DYNSYM)
            continue;

        Elf64_Shdr *strtab_shdr = Elf64_get_section(hdr, shdr->sh_link);
        const char *strtab = Elf64_get_strtab(hdr, strtab_shdr);

        size_t nsymbols = shdr->sh_size / shdr->sh_entsize;
        for (size_t i = 0; i < nsymbols; i++) {
            Elf64_Sym *sym = Elf64_get_symbol(hdr, shdr, i);
            const char *name = &strtab[sym->st_name];
            if (strcmp(name, symbol) == 0)
                return 1;
        }
    }
    return 0;
}

static Elf32_Shdr *Elf32_get_section(Elf32_Ehdr *hdr, size_t i)
{
    size_t section_header_sz = hdr->e_shentsize;
    size_t byte_offset = hdr->e_shoff + i * section_header_sz;
    return (Elf32_Shdr *)&((char *)hdr)[byte_offset];
}

static const char *Elf32_get_strtab(Elf32_Ehdr *hdr, Elf32_Shdr *section)
{
    return &((const char *)hdr)[section->sh_offset];
}

static Elf32_Sym *Elf32_get_symbol(Elf32_Ehdr *hdr, Elf32_Shdr *section, size_t i)
{
    size_t byte_offset = section->sh_offset + i * section->sh_entsize;
    return (Elf32_Sym *)&((char *)hdr)[byte_offset];
}

static int Elf32_locate_symbol(Elf32_Ehdr *hdr, const char *symbol)
{
    if (hdr->e_type != ET_DYN)
        return 0;

    for (size_t sect_idx = 0; sect_idx < hdr->e_shnum; sect_idx++) {
        Elf32_Shdr *shdr = Elf32_get_section(hdr, sect_idx);
        if (shdr->sh_type != SHT_DYNSYM)
            continue;

        Elf32_Shdr *strtab_shdr = Elf32_get_section(hdr, shdr->sh_link);
        const char *strtab = Elf32_get_strtab(hdr, strtab_shdr);

        size_t nsymbols = shdr->sh_size / shdr->sh_entsize;
        for (size_t i = 0; i < nsymbols; i++) {
            Elf32_Sym *sym = Elf32_get_symbol(hdr, shdr, i);
            const char *name = &strtab[sym->st_name];
            if (strcmp(name, symbol) == 0)
                return 1;
        }
    }
    return 0;
}

int jl_loader_locate_symbol(const char *library, const char *symbol)
{
    size_t library_sz;
    void *elf_file = jl_loader_open_via_mmap(library, &library_sz);
    if (elf_file == NULL)
        return 0;

    int found = 0;
    const char *hdr = (const char *)elf_file;
    if (strncmp(hdr, ELFMAG, SELFMAG) != 0)
        goto bail;

    assert(hdr[5] == ELFDATA2LSB);
    if (hdr[4] == ELFCLASS32) {
        found = Elf32_locate_symbol((Elf32_Ehdr *)hdr, symbol);
    } else if (hdr[4] == ELFCLASS64) {
        found = Elf64_locate_symbol((Elf64_Ehdr *)hdr, symbol);
    }

bail:
    munmap(elf_file, library_sz);
    return found;
}

#endif
