// This file is a part of Julia. License is MIT: https://julialang.org/license

// Emission of object files that contain nothing but one data symbol, written
// directly rather than through LLVM's code generator so that large data, such
// as the serialized heap image, can be streamed to disk without being copied.

#include <llvm/Object/COFF.h>
#include <llvm/Object/ELFObjectFile.h>
#include <llvm/Object/MachO.h>
#include <llvm/IR/Mangler.h>

#include "jitlayers.h"

using namespace llvm;

#include "julia_assert.h"

// Write `Size` bytes at file offset `Off`, zero-filling from the current position
static void write_at(raw_ostream &OS, uint64_t Off, const void *Data, size_t Size)
{
    assert(OS.tell() <= Off);
    OS.write_zeros(Off - OS.tell());
    OS.write((const char *)Data, Size);
}

template <class ELFT>
static void write_elf_data_object(raw_ostream &OS, const object::ELFFile<ELFT> &Ref,
                                 StringRef Name, ArrayRef<char> Data, Align Alignment)
{
    using namespace ELF;
    enum { DataSec = 1, SymtabSec, StrtabSec, NoteSec, NumSecs };
    // On x86-64, use a large data section, which linkers place after .bss so
    // that a big object does not push other data out of reach of the code
    bool Large = Ref.getHeader().e_machine == EM_X86_64;
    // A single string table holds both section and symbol names
    std::string Strtab(1, '\0');
    auto AddStr = [&](StringRef S) {
        size_t Off = Strtab.size();
        Strtab += S;
        Strtab += '\0';
        return Off;
    };
    typename ELFT::Shdr Shdrs[NumSecs] = {};
    typename ELFT::Sym Syms[2] = {};
    Shdrs[DataSec].sh_name = AddStr(Large ? ".ldata" : ".data");
    Shdrs[SymtabSec].sh_name = AddStr(".symtab");
    Shdrs[StrtabSec].sh_name = AddStr(".strtab");
    Shdrs[NoteSec].sh_name = AddStr(".note.GNU-stack");
    Syms[1].st_name = AddStr(Name);

    uint64_t SymtabOff = alignTo(sizeof(typename ELFT::Ehdr), 8);
    uint64_t StrtabOff = SymtabOff + sizeof(Syms);
    uint64_t DataOff = alignTo(StrtabOff + Strtab.size(), Alignment);
    uint64_t ShdrOff = alignTo(DataOff + Data.size(), 8);
    if (!ELFT::Is64Bits && ShdrOff + sizeof(Shdrs) > UINT32_MAX)
        report_fatal_error("data is too large for an ELF32 object");

    typename ELFT::Ehdr Ehdr = Ref.getHeader();
    Ehdr.e_shoff = ShdrOff;
    Ehdr.e_shentsize = sizeof(typename ELFT::Shdr);
    Ehdr.e_shnum = NumSecs;
    Ehdr.e_shstrndx = StrtabSec;

    Syms[1].setBindingAndType(STB_GLOBAL, STT_OBJECT);
    Syms[1].st_shndx = DataSec;
    Syms[1].st_size = Data.size();

    auto &D = Shdrs[DataSec];
    D.sh_type = SHT_PROGBITS;
    D.sh_flags = SHF_ALLOC | SHF_WRITE | (Large ? SHF_X86_64_LARGE : 0);
    D.sh_offset = DataOff;
    D.sh_size = Data.size();
    D.sh_addralign = Alignment.value();
    auto &S = Shdrs[SymtabSec];
    S.sh_type = SHT_SYMTAB;
    S.sh_offset = SymtabOff;
    S.sh_size = sizeof(Syms);
    S.sh_link = StrtabSec;
    S.sh_info = 1; // index of the first non-local symbol
    S.sh_addralign = 8;
    S.sh_entsize = sizeof(Syms[0]);
    auto &T = Shdrs[StrtabSec];
    T.sh_type = SHT_STRTAB;
    T.sh_offset = StrtabOff;
    T.sh_size = Strtab.size();
    T.sh_addralign = 1;
    // Without this, GNU ld makes the stack executable
    Shdrs[NoteSec].sh_type = SHT_PROGBITS;
    Shdrs[NoteSec].sh_addralign = 1;

    write_at(OS, 0, &Ehdr, sizeof(Ehdr));
    write_at(OS, SymtabOff, Syms, sizeof(Syms));
    write_at(OS, StrtabOff, Strtab.data(), Strtab.size());
    write_at(OS, DataOff, Data.data(), Data.size());
    write_at(OS, ShdrOff, Shdrs, sizeof(Shdrs));
}

static void write_macho_data_object(raw_ostream &OS, const object::MachOObjectFile &Ref,
                                   StringRef Name, ArrayRef<char> Data, Align Alignment)
{
    using namespace MachO;
    assert(Ref.is64Bit() && sys::IsLittleEndianHost);
    // Copy the platform load command; the linker warns if there is none
    StringRef Version;
    for (const auto &LC : Ref.load_commands())
        if (LC.C.cmd == LC_BUILD_VERSION || LC.C.cmd == LC_VERSION_MIN_MACOSX)
            Version = StringRef(LC.Ptr, LC.C.cmdsize);
    SmallString<32> Strtab;
    Strtab += '\0';
    Strtab += Name;
    Strtab += '\0';

    uint32_t CmdsSize = sizeof(segment_command_64) + sizeof(section_64) + Version.size() +
                        sizeof(symtab_command);
    uint64_t DataOff = alignTo(sizeof(mach_header_64) + CmdsSize, Alignment);
    // ld64 requires the symbol table after the section data, and its offset is 32-bit
    uint64_t SymOff = alignTo(DataOff + Data.size(), 8);
    uint64_t StrOff = SymOff + sizeof(nlist_64);
    if (StrOff + Strtab.size() > UINT32_MAX)
        report_fatal_error("data is too large for a Mach-O object");

    mach_header_64 Header = {};
    Header.magic = MH_MAGIC_64;
    Header.cputype = Ref.getHeader64().cputype;
    Header.cpusubtype = Ref.getHeader64().cpusubtype;
    Header.filetype = MH_OBJECT;
    Header.ncmds = Version.empty() ? 2 : 3;
    Header.sizeofcmds = CmdsSize;
    segment_command_64 Seg = {};
    Seg.cmd = LC_SEGMENT_64;
    Seg.cmdsize = sizeof(segment_command_64) + sizeof(section_64);
    Seg.vmsize = Data.size();
    Seg.fileoff = DataOff;
    Seg.filesize = Data.size();
    Seg.maxprot = Seg.initprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
    Seg.nsects = 1;
    section_64 Sect = {};
    memcpy(Sect.sectname, "__data", 6);
    memcpy(Sect.segname, "__DATA", 6);
    Sect.size = Data.size();
    Sect.offset = DataOff;
    Sect.align = Log2(Alignment);
    symtab_command Symtab = {LC_SYMTAB, sizeof(symtab_command), (uint32_t)SymOff, 1,
                             (uint32_t)StrOff, (uint32_t)Strtab.size()};
    nlist_64 Sym = {};
    Sym.n_strx = 1;
    Sym.n_type = N_SECT | N_EXT;
    Sym.n_sect = 1;

    write_at(OS, 0, &Header, sizeof(Header));
    OS.write((const char *)&Seg, sizeof(Seg));
    OS.write((const char *)&Sect, sizeof(Sect));
    OS << Version;
    OS.write((const char *)&Symtab, sizeof(Symtab));
    write_at(OS, DataOff, Data.data(), Data.size());
    write_at(OS, SymOff, &Sym, sizeof(Sym));
    write_at(OS, StrOff, Strtab.data(), Strtab.size());
}

static void write_coff_data_object(raw_ostream &OS, const object::COFFObjectFile &Ref,
                                  StringRef Name, StringRef Mangled, ArrayRef<char> Data,
                                  Align Alignment)
{
    using namespace COFF;
    if (Data.size() > UINT32_MAX)
        report_fatal_error("data is too large for a COFF object");
    // Export the symbol from the DLL, as LLVM does for dllexport globals (MinGW syntax)
    std::string Directive = (" -export:" + Name + ",data").str();
    // The string table starts with its own size
    SmallString<32> Strtab;
    Strtab.resize(4);
    Strtab += Mangled;
    Strtab += '\0';
    support::endian::write32le(Strtab.data(), Strtab.size());

    object::coff_section Sects[2] = {};
    uint64_t DirectiveOff = sizeof(object::coff_file_header) + sizeof(Sects);
    uint64_t SymOff = DirectiveOff + Directive.size();
    uint64_t StrOff = SymOff + sizeof(object::coff_symbol16);
    uint64_t DataOff = alignTo(StrOff + Strtab.size(), Alignment);

    object::coff_file_header Header = {};
    Header.Machine = Ref.getMachine();
    Header.NumberOfSections = 2;
    Header.PointerToSymbolTable = SymOff;
    Header.NumberOfSymbols = 1;
    memcpy(Sects[0].Name, ".data", 5);
    Sects[0].SizeOfRawData = Data.size();
    Sects[0].PointerToRawData = DataOff;
    Sects[0].Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ |
                               IMAGE_SCN_MEM_WRITE |
                               (std::min(Log2(Alignment), 13u) + 1) << 20; // IMAGE_SCN_ALIGN_*, at most 8K
    memcpy(Sects[1].Name, ".drectve", 8);
    Sects[1].SizeOfRawData = Directive.size();
    Sects[1].PointerToRawData = DirectiveOff;
    Sects[1].Characteristics = IMAGE_SCN_LNK_INFO | IMAGE_SCN_LNK_REMOVE |
                               IMAGE_SCN_ALIGN_1BYTES;
    object::coff_symbol16 Sym = {};
    Sym.Name.Offset.Offset = 4;
    Sym.SectionNumber = 1;
    Sym.StorageClass = IMAGE_SYM_CLASS_EXTERNAL;

    write_at(OS, 0, &Header, sizeof(Header));
    OS.write((const char *)Sects, sizeof(Sects));
    OS << Directive;
    write_at(OS, SymOff, &Sym, sizeof(Sym));
    write_at(OS, StrOff, Strtab.data(), Strtab.size());
    write_at(OS, DataOff, Data.data(), Data.size());
}

// Write a minimal relocatable object that defines only the writable,
// exported symbol `Name`, holding `Data`.  Target-specific header fields (ELF
// e_flags, the Mach-O platform, the COFF machine) are copied from `Ref`, an
// object LLVM emitted for the same target.
void jl_write_data_object(raw_ostream &OS, MemoryBufferRef Ref, const DataLayout &DL,
                          StringRef Name, ArrayRef<char> Data, Align Alignment)
{
    SmallString<32> Mangled;
    Mangler::getNameWithPrefix(Mangled, Name, DL);
    auto Obj = object::ObjectFile::createObjectFile(Ref);
    if (!Obj)
        report_fatal_error(Obj.takeError());
    if (auto *E = dyn_cast<object::ELF32LEObjectFile>(Obj->get()))
        write_elf_data_object(OS, E->getELFFile(), Mangled, Data, Alignment);
    else if (auto *E = dyn_cast<object::ELF64LEObjectFile>(Obj->get()))
        write_elf_data_object(OS, E->getELFFile(), Mangled, Data, Alignment);
    else if (auto *M = dyn_cast<object::MachOObjectFile>(Obj->get()))
        write_macho_data_object(OS, *M, Mangled, Data, Alignment);
    else if (auto *C = dyn_cast<object::COFFObjectFile>(Obj->get()))
        write_coff_data_object(OS, *C, Name, Mangled, Data, Alignment);
    else
        report_fatal_error("unsupported object format for a data object");
}
