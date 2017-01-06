//===------------- Disassembler for in-memory function --------------------===//
//
// Modified for use in The Julia Language from code in the  llvm-mc project:
//      llvm-mc.cpp and Disassembler.cpp
//
// Original copyright:
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This class implements a disassembler of a memory block, given a function
// pointer and size.
//
//===----------------------------------------------------------------------===//

#include <cstdio>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <string>

#include "llvm-version.h"
#include <llvm/Object/ObjectFile.h>
#include <llvm/Support/MachO.h>
#include <llvm/Support/COFF.h>
#include <llvm/MC/MCInst.h>
#include <llvm/MC/MCStreamer.h>
#include <llvm/MC/MCSubtargetInfo.h>
#include <llvm/MC/MCObjectFileInfo.h>
#include <llvm/MC/MCRegisterInfo.h>
#include <llvm/MC/MCAsmInfo.h>
#include <llvm/MC/MCAsmBackend.h>
#include <llvm/MC/MCCodeEmitter.h>
#include <llvm/MC/MCInstPrinter.h>
#include <llvm/MC/MCInstrInfo.h>
#include <llvm/MC/MCContext.h>
#include <llvm/MC/MCExpr.h>
#include <llvm/MC/MCInstrAnalysis.h>
#include <llvm/MC/MCSymbol.h>
#if JL_LLVM_VERSION >= 30500
#  include <llvm/AsmParser/Parser.h>
#  if JL_LLVM_VERSION >= 30900
#    include <llvm/MC/MCDisassembler/MCDisassembler.h>
#    include <llvm/MC/MCDisassembler/MCExternalSymbolizer.h>
#  else
#    include <llvm/MC/MCExternalSymbolizer.h>
#    include <llvm/MC/MCDisassembler.h>
#  endif
#else
#  include <llvm/Assembly/Parser.h>
#  include <llvm/ADT/OwningPtr.h>
#  include <llvm/MC/MCDisassembler.h>
#endif
#include <llvm/ADT/Triple.h>
#include <llvm/Support/MemoryBuffer.h>
#if JL_LLVM_VERSION < 30600
#include <llvm/Support/MemoryObject.h>
#endif
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/Host.h>
#include "llvm/Support/TargetSelect.h"
#include <llvm/Support/raw_ostream.h>
#include "llvm/Support/FormattedStream.h"
#if JL_LLVM_VERSION < 30500
#include <llvm/Support/system_error.h>
#endif
#include <llvm/ExecutionEngine/JITEventListener.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/DebugInfo/DIContext.h>
#if JL_LLVM_VERSION >= 30700
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#endif
#if JL_LLVM_VERSION >= 30500
#include <llvm/IR/DebugInfo.h>
#else
#include <llvm/DebugInfo.h>
#endif

#include "julia.h"
#include "julia_internal.h"

using namespace llvm;

extern JL_DLLEXPORT LLVMContext &jl_LLVMContext;

namespace {
#if JL_LLVM_VERSION >= 30600
#define FuncMCView ArrayRef<uint8_t>
#else
class FuncMCView : public MemoryObject {
private:
    const char *Fptr;
    const size_t Fsize;
public:
    FuncMCView(const void *fptr, size_t size) : Fptr((const char*)fptr), Fsize(size) {}
    uint64_t getBase() const { return 0; }
    uint64_t getExtent() const { return Fsize; }
    int readByte(uint64_t Addr, uint8_t *Byte) const {
        if (Addr >= getExtent())
            return -1;
        *Byte = Fptr[Addr];
        return 0;
    }

    // ArrayRef-like accessors:
    const char operator[] (const size_t idx) const { return Fptr[idx]; }
    uint64_t size() const { return Fsize; }
    const uint8_t *data() const { return (const uint8_t*)Fptr; }
    FuncMCView slice(unsigned N) const { return FuncMCView(Fptr+N, Fsize-N); }
};
#endif

// Look up a symbol, and return a const char* to its name when the
// address matches. We currently just use "L<address>" as name for the
// symbol. We could easily get more fancy, e.g. numbering symbols
// sequentially or encoding the line number, but that doesn't seem
// necessary.
class SymbolTable {
    typedef std::map<uint64_t, MCSymbol*> TableType;
    TableType Table;
    std::string TempName;
    MCContext& Ctx;
    const FuncMCView &MemObj;
    int Pass;
    const object::ObjectFile *object;
    uint64_t ip; // virtual instruction pointer of the current instruction
    int64_t slide;
public:
    SymbolTable(MCContext &Ctx, const object::ObjectFile *object, int64_t slide, const FuncMCView &MemObj):
        Ctx(Ctx), MemObj(MemObj), object(object), ip(0), slide(slide) {}
    const FuncMCView &getMemoryObject() const { return MemObj; }
    void setPass(int Pass) { this->Pass = Pass; }
    int getPass() const { return Pass; }
    void insertAddress(uint64_t addr);
    // void createSymbol(const char *name, uint64_t addr);
    void createSymbols();
    const char *lookupSymbolName(uint64_t addr, bool LocalOnly);
    MCSymbol *lookupSymbol(uint64_t addr);
    StringRef getSymbolNameAt(uint64_t offset) const;
    const char *lookupLocalPC(size_t addr);
    void setIP(uint64_t addr);
    uint64_t getIP() const;
};

void SymbolTable::setIP(uint64_t addr)
{
    ip = addr;
}
uint64_t SymbolTable::getIP() const
{
    return ip;
}

const char *SymbolTable::lookupLocalPC(size_t addr) {
    jl_frame_t *frame = NULL;
    jl_getFunctionInfo(&frame,
            addr,
            /*skipC*/0,
            /*noInline*/1/* the entry pointer shouldn't have inlining */);
    char *name = frame->func_name; // TODO: free me
    free(frame->file_name);
    free(frame);
    return name;
}

StringRef SymbolTable::getSymbolNameAt(uint64_t offset) const
{
    if (object == NULL) return StringRef();
#if JL_LLVM_VERSION >= 30700
    object::section_iterator ESection = object->section_end();
    for (const object::SymbolRef &Sym : object->symbols()) {
#else
    llvm::error_code err;
    object::section_iterator ESection = object->end_sections();
    for (object::symbol_iterator I = object->begin_symbols(), E = object->end_symbols();
            !err && I != E; I.increment(err)) {
        object::SymbolRef Sym = *I;
#endif
        uint64_t Addr, SAddr;
        object::section_iterator Sect = ESection;
#if JL_LLVM_VERSION >= 30800
        auto SectOrError = Sym.getSection();
        assert(SectOrError);
        Sect = SectOrError.get();
#else
        if (Sym.getSection(Sect)) continue;
#endif
        if (Sect == ESection) continue;
#if JL_LLVM_VERSION >= 30500
        SAddr = Sect->getAddress();
        if (SAddr == 0) continue;
#else
        if (Sym.getAddress(SAddr) || SAddr == 0) continue;
#endif
#if JL_LLVM_VERSION >= 30700
        auto AddrOrError = Sym.getAddress();
        assert(AddrOrError);
        Addr = AddrOrError.get();
        if (Addr == offset) {
            auto sNameOrError = Sym.getName();
            if (sNameOrError)
                return sNameOrError.get();
        }
#else
        if (Sym.getAddress(Addr)) continue;
        if (Addr == offset) {
            StringRef Name;
            if (!Sym.getName(Name))
                return Name;
        }
#endif
    }
    return StringRef();
}

// Insert an address
void SymbolTable::insertAddress(uint64_t addr)
{
    Table[addr] = NULL;
}
// Create a symbol
// void SymbolTable::createSymbol(const char *name, uint64_t addr)
// {
//     MCSymbol *symb = Ctx.GetOrCreateSymbol(StringRef(name));
//     symb->setVariableValue(MCConstantExpr::Create(addr, Ctx));
// }
// Create symbols for all addresses
void SymbolTable::createSymbols()
{
    uintptr_t Fptr = (uintptr_t)MemObj.data();
    uintptr_t Fsize = MemObj.size();
    for (TableType::iterator isymb = Table.begin(), esymb = Table.end();
         isymb != esymb; ++isymb) {
        std::ostringstream name;
        uintptr_t rel = isymb->first - ip;
        uintptr_t addr = isymb->first;
        if (Fptr <= addr && addr < Fptr + Fsize) {
            name << "L" << rel;
        }
        else {
            const char *global = lookupLocalPC(addr);
            if (!global)
                continue;
            name << global;
        }

#if JL_LLVM_VERSION >= 30700
        MCSymbol *symb = Ctx.getOrCreateSymbol(StringRef(name.str()));
        assert(symb->isUndefined());
#else
        MCSymbol *symb = Ctx.GetOrCreateSymbol(StringRef(name.str()));
        symb->setVariableValue(MCConstantExpr::Create(rel, Ctx));
#endif
        isymb->second = symb;
    }
}

const char *SymbolTable::lookupSymbolName(uint64_t addr, bool LocalOnly)
{
    TempName = std::string();
    TableType::iterator Sym = Table.find(addr);
    if (Sym != Table.end() && Sym->second) {
        TempName = Sym->second->getName().str();
    }
    else if (!LocalOnly) {
        TempName = getSymbolNameAt(addr + slide).str();
    }
    return TempName.empty() ? NULL : TempName.c_str();
}

MCSymbol *SymbolTable::lookupSymbol(uint64_t addr)
{
    if (!Table.count(addr)) return NULL;
    return Table[addr];
}

static const char *SymbolLookup(void *DisInfo, uint64_t ReferenceValue, uint64_t *ReferenceType,
                                uint64_t ReferencePC, const char **ReferenceName)
{
    SymbolTable *SymTab = (SymbolTable*)DisInfo;
    if (SymTab->getPass() != 0) {
        uint64_t addr = ReferenceValue + SymTab->getIP();
        if (*ReferenceType == LLVMDisassembler_ReferenceType_In_Branch) {
            const char *symbolName = SymTab->lookupSymbolName(addr, false);
            //*ReferenceType = LLVMDisassembler_ReferenceType_Out_SymbolStub;
            *ReferenceType = LLVMDisassembler_ReferenceType_InOut_None;
            *ReferenceName = NULL;
            return symbolName;
        }
        else if (*ReferenceType == LLVMDisassembler_ReferenceType_In_PCrel_Load) {
            const char *symbolName = SymTab->lookupSymbolName(addr, false);
            if (symbolName) {
#if JL_LLVM_VERSION >= 30700
                *ReferenceType = LLVMDisassembler_ReferenceType_Out_LitPool_SymAddr;
#else
                *ReferenceType = LLVMDisassembler_ReferenceType_Out_LitPool_CstrAddr;
#endif
                *ReferenceName = symbolName;
                return NULL;
            }
        }
    }
    *ReferenceType = LLVMDisassembler_ReferenceType_InOut_None;
    *ReferenceName = NULL;
    return NULL;
}

static int OpInfoLookup(void *DisInfo, uint64_t PC, uint64_t Offset, uint64_t Size,
                        int TagType, void *TagBuf)
{
    SymbolTable *SymTab = (SymbolTable*)DisInfo;
    PC += SymTab->getIP() - (uint64_t)(uintptr_t)SymTab->getMemoryObject().data(); // add offset from MemoryObject base
    if (TagType != 1)
        return 0;               // Unknown data format
    LLVMOpInfo1 *info = (LLVMOpInfo1*)TagBuf;
    uint8_t *bytes = (uint8_t*) alloca(Size*sizeof(uint8_t));
    if (PC+Offset+Size > SymTab->getMemoryObject().size())
        return 0;              // Invalid memory location
    for (uint64_t i=0; i<Size; ++i)
        bytes[i] = SymTab->getMemoryObject()[PC+Offset+i];
    size_t pointer;
    switch (Size) {
    case 1: { uint8_t  val; std::memcpy(&val, bytes, 1); pointer = val; break; }
    case 2: { uint16_t val; std::memcpy(&val, bytes, 2); pointer = val; break; }
    case 4: { uint32_t val; std::memcpy(&val, bytes, 4); pointer = val; break; }
    case 8: { uint64_t val; std::memcpy(&val, bytes, 8); pointer = val; break; }
    default: return 0;          // Cannot handle input address size
    }
    const char *name = SymTab->lookupLocalPC(pointer);
    if (!name)
        return 0;               // Did not find symbolic information
    // Describe the symbol
    info->AddSymbol.Present = 1;
    info->AddSymbol.Name = name;
    info->AddSymbol.Value = pointer; // unused by LLVM
    info->Value = 0;                 // offset
    return 1;                        // Success
}
} // namespace

extern "C" {
JL_DLLEXPORT LLVMDisasmContextRef jl_LLVMCreateDisasm(const char *TripleName, void *DisInfo, int TagType, LLVMOpInfoCallback GetOpInfo, LLVMSymbolLookupCallback SymbolLookUp)
{
    return LLVMCreateDisasm(TripleName, DisInfo, TagType, GetOpInfo, SymbolLookUp);
}

JL_DLLEXPORT size_t jl_LLVMDisasmInstruction(LLVMDisasmContextRef DC, uint8_t *Bytes, uint64_t BytesSize, uint64_t PC, char *OutString, size_t OutStringSize)
{
    return LLVMDisasmInstruction(DC, Bytes, BytesSize, PC, OutString, OutStringSize);
}

void jl_dump_asm_internal(uintptr_t Fptr, size_t Fsize, int64_t slide,
#ifndef USE_MCJIT
                          std::vector<JITEvent_EmittedFunctionDetails::LineStart> lineinfo,
#endif
                          const object::ObjectFile *object,
                          DIContext *di_ctx,
#if JL_LLVM_VERSION >= 30700
                          raw_ostream &rstream,
#else
                          formatted_raw_ostream &stream,
#endif
                          const char* asm_variant="att"
                          )
{
    // GC safe
    // Get the host information
    std::string TripleName = sys::getDefaultTargetTriple();
    Triple TheTriple(Triple::normalize(TripleName));

    std::string MCPU = sys::getHostCPUName();
#ifdef _CPU_ARM_
    // The Raspberry Pi CPU is misdetected by LLVM (at least of version
    // 3.6); correct this.
    if (MCPU == "arm1176jz-s")
        MCPU = "arm1176jzf-s";
#endif
    SubtargetFeatures Features;
    Features.getDefaultSubtargetFeatures(TheTriple);

    std::string err;
    const Target *TheTarget = TargetRegistry::lookupTarget(TripleName, err);

    // Set up required helpers and streamer
#if JL_LLVM_VERSION >= 30500
    std::unique_ptr<MCStreamer> Streamer;
#else
    OwningPtr<MCStreamer> Streamer;
#endif
    SourceMgr SrcMgr;

#if JL_LLVM_VERSION >= 30500
    std::unique_ptr<MCAsmInfo> MAI(TheTarget->createMCAsmInfo(*TheTarget->createMCRegInfo(TripleName),TripleName));
#elif JL_LLVM_VERSION >= 30400
    llvm::OwningPtr<MCAsmInfo> MAI(TheTarget->createMCAsmInfo(*TheTarget->createMCRegInfo(TripleName),TripleName));
#else
    llvm::OwningPtr<MCAsmInfo> MAI(TheTarget->createMCAsmInfo(TripleName));
#endif
    assert(MAI && "Unable to create target asm info!");

#if JL_LLVM_VERSION >= 30500
    std::unique_ptr<MCRegisterInfo> MRI(TheTarget->createMCRegInfo(TripleName));
#else
    llvm::OwningPtr<MCRegisterInfo> MRI(TheTarget->createMCRegInfo(TripleName));
#endif
    assert(MRI && "Unable to create target register info!");

#if JL_LLVM_VERSION >= 30500
    std::unique_ptr<MCObjectFileInfo> MOFI(new MCObjectFileInfo());
#else
    OwningPtr<MCObjectFileInfo> MOFI(new MCObjectFileInfo());
#endif
#if JL_LLVM_VERSION >= 30400
    MCContext Ctx(MAI.get(), MRI.get(), MOFI.get(), &SrcMgr);
#else
    MCContext Ctx(*MAI, *MRI, MOFI.get(), &SrcMgr);
#endif
#if JL_LLVM_VERSION >= 30900
    MOFI->InitMCObjectFileInfo(TheTriple, /* PIC */ false,
                               CodeModel::Default, Ctx);
#elif JL_LLVM_VERSION >= 30700
    MOFI->InitMCObjectFileInfo(TheTriple, Reloc::Default, CodeModel::Default, Ctx);
#else
    MOFI->InitMCObjectFileInfo(TripleName, Reloc::Default, CodeModel::Default, Ctx);
#endif

    // Set up Subtarget and Disassembler
#if JL_LLVM_VERSION >= 30500
    std::unique_ptr<MCSubtargetInfo>
        STI(TheTarget->createMCSubtargetInfo(TripleName, MCPU, Features.getString()));
    std::unique_ptr<MCDisassembler> DisAsm(TheTarget->createMCDisassembler(*STI, Ctx));
#else
    OwningPtr<MCSubtargetInfo>
        STI(TheTarget->createMCSubtargetInfo(TripleName, MCPU, Features.getString()));
    OwningPtr<MCDisassembler> DisAsm(TheTarget->createMCDisassembler(*STI));
#endif
    if (!DisAsm) {
        jl_printf(JL_STDERR, "ERROR: no disassembler for target %s\n",
                  TripleName.c_str());
        return;
    }
    unsigned OutputAsmVariant = 0; // ATT or Intel-style assembly

    if (strcmp(asm_variant, "intel")==0) {
        OutputAsmVariant = 1;
    }
    bool ShowEncoding = false;

#if JL_LLVM_VERSION >= 30500
    std::unique_ptr<MCInstrInfo> MCII(TheTarget->createMCInstrInfo());
    std::unique_ptr<MCInstrAnalysis>
        MCIA(TheTarget->createMCInstrAnalysis(MCII.get()));
#else
    OwningPtr<MCInstrInfo> MCII(TheTarget->createMCInstrInfo());
    OwningPtr<MCInstrAnalysis>
        MCIA(TheTarget->createMCInstrAnalysis(MCII.get()));
#endif
#if JL_LLVM_VERSION >= 30700
    MCInstPrinter *IP =
        TheTarget->createMCInstPrinter(TheTriple, OutputAsmVariant, *MAI, *MCII, *MRI);
#else
    MCInstPrinter *IP =
        TheTarget->createMCInstPrinter(OutputAsmVariant, *MAI, *MCII, *MRI, *STI);
#endif
    //IP->setPrintImmHex(true); // prefer hex or decimal immediates
    MCCodeEmitter *CE = 0;
    MCAsmBackend *MAB = 0;
    if (ShowEncoding) {
#if JL_LLVM_VERSION >= 30700
        CE = TheTarget->createMCCodeEmitter(*MCII, *MRI, Ctx);
#else
        CE = TheTarget->createMCCodeEmitter(*MCII, *MRI, *STI, Ctx);
#endif
#if JL_LLVM_VERSION >= 40000
        MCTargetOptions Options;
        MAB = TheTarget->createMCAsmBackend(*MRI, TripleName, MCPU, Options);
#elif JL_LLVM_VERSION >= 30400
        MAB = TheTarget->createMCAsmBackend(*MRI, TripleName, MCPU);
#else
        MAB = TheTarget->createMCAsmBackend(TripleName, MCPU);
#endif
    }

#if JL_LLVM_VERSION >= 30700
    // createAsmStreamer expects a unique_ptr to a formatted stream, which means
    // it will destruct the stream when it is done. We cannot have this, so we
    // start out with a raw stream, and create formatted stream from it here.
    // LLVM will desctruct the formatted stream, and we keep the raw stream.
    auto ustream = llvm::make_unique<formatted_raw_ostream>(rstream);
    Streamer.reset(TheTarget->createAsmStreamer(Ctx, std::move(ustream), /*asmverbose*/true,
#else
    Streamer.reset(TheTarget->createAsmStreamer(Ctx, stream, /*asmverbose*/true,
#endif
#if JL_LLVM_VERSION < 30500
                                                /*useLoc*/ true,
                                                /*useCFI*/ true,
#endif
                                                /*useDwarfDirectory*/ true,
                                                IP, CE, MAB, /*ShowInst*/ false));
#if JL_LLVM_VERSION >= 30600
    Streamer->InitSections(true);
#else
    Streamer->InitSections();
#endif

    // Make the MemoryObject wrapper
#if JL_LLVM_VERSION >= 30600
    ArrayRef<uint8_t> memoryObject(const_cast<uint8_t*>((const uint8_t*)Fptr),Fsize);
#else
    FuncMCView memoryObject((const uint8_t*)Fptr, Fsize);
#endif
    SymbolTable DisInfo(Ctx, object, slide, memoryObject);

#ifndef USE_MCJIT
    typedef std::vector<JITEvent_EmittedFunctionDetails::LineStart> LInfoVec;
#endif

    DILineInfoTable di_lineinfo;
    if (di_ctx)
         di_lineinfo = di_ctx->getLineInfoForAddressRange(Fptr+slide, Fsize);

    // Take two passes: In the first pass we record all branch labels,
    // in the second we actually perform the output
    for (int pass = 0; pass < 2; ++ pass) {
        DisInfo.setPass(pass);
        if (pass != 0) {
            // Switch to symbolic disassembly. We cannot do this
            // before the first pass, because this changes branch
            // targets from immediate values (constants) to
            // expressions, which are not handled correctly by
            // MCIA->evaluateBranch. (It should be possible to rewrite
            // this routine to handle this case correctly as well.)
            // Could add OpInfoLookup here
#if JL_LLVM_VERSION >= 30500
            DisAsm->setSymbolizer(std::unique_ptr<MCSymbolizer>(new MCExternalSymbolizer(
                        Ctx,
                        std::unique_ptr<MCRelocationInfo>(new MCRelocationInfo(Ctx)),
                        OpInfoLookup,
                        SymbolLookup,
                        &DisInfo)));
#elif JL_LLVM_VERSION >= 30400
            OwningPtr<MCRelocationInfo> relinfo(new MCRelocationInfo(Ctx));
            DisAsm->setupForSymbolicDisassembly(OpInfoLookup, SymbolLookup, &DisInfo, &Ctx,
                                                relinfo);
#else
            DisAsm->setupForSymbolicDisassembly(OpInfoLookup, SymbolLookup, &DisInfo, &Ctx);
#endif
        }

        uint64_t nextLineAddr = -1;
        DILineInfoTable::iterator di_lineIter = di_lineinfo.begin();
        DILineInfoTable::iterator di_lineEnd = di_lineinfo.end();
#ifndef USE_MCJIT
        LInfoVec::iterator lineIter = lineinfo.begin();
        LInfoVec::iterator lineEnd  = lineinfo.end();
#endif
        if (pass != 0) {
            if (di_ctx) {
                // Set up the line info
                if (di_lineIter != di_lineEnd) {
#if JL_LLVM_VERSION >= 30700
                    std::ostringstream buf;
                    buf << "Filename: " << di_lineIter->second.FileName << "\n";
                    Streamer->EmitRawText(buf.str());
#elif JL_LLVM_VERSION >= 30500
                    stream << "Filename: " << di_lineIter->second.FileName << "\n";
#else
                    stream << "Filename: " << di_lineIter->second.getFileName() << "\n";
#endif
#if JL_LLVM_VERSION >= 30500
                    if (di_lineIter->second.Line <= 0)
#else
                    if (di_lineIter->second.getLine() <= 0)
#endif
                        ++di_lineIter;
                    nextLineAddr = di_lineIter->first;
                }
            }
#ifndef USE_MCJIT
            else {
            // Set up the line info
                if (lineIter != lineEnd) {
                    DebugLoc Loc = (*lineIter).Loc;
                    MDNode *outer = Loc.getInlinedAt(jl_LLVMContext);
                    while (outer) {
                        Loc = DebugLoc::getFromDILocation(outer);
                        outer = Loc.getInlinedAt(jl_LLVMContext);
                    }
                    StringRef FileName;
                    DISubprogram debugscope = DISubprogram(Loc.getScope(jl_LLVMContext));
                    stream << "Filename: " << debugscope.getFilename() << "\n";
                    if (Loc.getLine() > 0)
                        stream << "Source line: " << Loc.getLine() << "\n";
#if JL_LLVM_VERSION >= 30500
                    nextLineAddr = (*lineIter).Address;
#endif
                }
            }
#endif
        }

        uint64_t Index = 0;
        uint64_t insSize = 0;

        // Do the disassembly
        for (Index = 0; Index < Fsize; Index += insSize) {

            if (nextLineAddr != (uint64_t)-1 && Index + Fptr + slide == nextLineAddr) {
                if (di_ctx) {
#if JL_LLVM_VERSION >= 30700
                    std::ostringstream buf;
                    buf << "Source line: " << di_lineIter->second.Line << "\n";
                    Streamer->EmitRawText(buf.str());
#elif JL_LLVM_VERSION >= 30500
                    stream << "Source line: " << di_lineIter->second.Line << "\n";
#else
                    stream << "Source line: " << di_lineIter->second.getLine() << "\n";
#endif
                    nextLineAddr = (++di_lineIter)->first;
                }
#ifndef USE_MCJIT
                else {
                    DebugLoc Loc = (*lineIter).Loc;
                    MDNode *outer = Loc.getInlinedAt(jl_LLVMContext);
                    while (outer) {
                        Loc = DebugLoc::getFromDILocation(outer);
                        outer = Loc.getInlinedAt(jl_LLVMContext);
                    }
                    stream << "Source line: " << Loc.getLine() << "\n";
                    nextLineAddr = (*++lineIter).Address;
                }
#endif
            }

            DisInfo.setIP(Fptr+Index);
            if (pass != 0) {
                // Uncomment this to output addresses for all instructions
                // stream << Index << ": ";
#if JL_LLVM_VERSION >= 30700
                MCSymbol *symbol = DisInfo.lookupSymbol(Fptr+Index);
                if (symbol)
                    Streamer->EmitLabel(symbol);
                    // emitInstructionAnnot
#else
                const char *symbolName = DisInfo.lookupSymbolName(Fptr + Index, true);
                if (symbolName)
                    stream << symbolName << ":";
#endif
            }

            MCInst Inst;
            MCDisassembler::DecodeStatus S;
            FuncMCView view = memoryObject.slice(Index);
            S = DisAsm->getInstruction(Inst, insSize, view, 0,
                                      /*VStream*/ nulls(),
                                      /*CStream*/ pass != 0 ? Streamer->GetCommentOS() : nulls());
            if (pass != 0 && Streamer->GetCommentOS().tell() > 0)
                Streamer->GetCommentOS() << '\n';
            switch (S) {
            case MCDisassembler::Fail:
                if (insSize == 0) // skip illegible bytes
#if defined(_CPU_PPC_) || defined(_CPU_PPC64_) || defined(_CPU_ARM_) || defined(_CPU_AARCH64_)
                    insSize = 4; // instructions are always 4 bytes
#else
                    insSize = 1; // attempt to slide 1 byte forward
#endif
                if (pass != 0) {
                    std::ostringstream buf;
                    if (insSize == 4)
                        buf << "\t.long\t0x" << std::hex
                            << std::setfill('0') << std::setw(8)
                            << *(uint32_t*)(Fptr+Index);
                    else
                        for (uint64_t i=0; i<insSize; ++i)
                            buf << "\t.byte\t0x" << std::hex
                                << std::setfill('0') << std::setw(2)
                                << (int)*(uint8_t*)(Fptr+Index+i);
                    Streamer->EmitRawText(StringRef(buf.str()));
                }
                break;

            case MCDisassembler::SoftFail:
                if (pass != 0)
                    Streamer->EmitRawText(StringRef("potentially undefined instruction encoding:"));
                // Fall through

            case MCDisassembler::Success:
                if (pass == 0) {
                    // Pass 0: Record all branch targets
                    if (MCIA && (MCIA->isBranch(Inst) || MCIA->isCall(Inst))) {
                        uint64_t addr;
#if JL_LLVM_VERSION >= 30400
                        if (MCIA->evaluateBranch(Inst, Fptr+Index, insSize, addr))
#else
                        if ((addr = MCIA->evaluateBranch(Inst, Fptr+Index, insSize)) != (uint64_t)-1)
#endif
                            DisInfo.insertAddress(addr);
                    }
                }
                else {
                    // Pass 1: Output instruction
#if JL_LLVM_VERSION >= 30500
                    Streamer->EmitInstruction(Inst, *STI);
#else
                    Streamer->EmitInstruction(Inst);
#endif
                }
                break;
            }
        }

        DisInfo.setIP(Fptr);
        if (pass == 0)
            DisInfo.createSymbols();
    }
}
}
