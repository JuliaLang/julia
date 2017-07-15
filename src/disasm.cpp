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
#if JL_LLVM_VERSION >= 50000
#include <llvm/BinaryFormat/MachO.h>
#include <llvm/BinaryFormat/COFF.h>
#else
#include <llvm/Support/MachO.h>
#include <llvm/Support/COFF.h>
#endif
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
#include <llvm/AsmParser/Parser.h>
#include <llvm/MC/MCDisassembler/MCDisassembler.h>
#include <llvm/MC/MCDisassembler/MCExternalSymbolizer.h>
#include <llvm/ADT/Triple.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/Host.h>
#include "llvm/Support/TargetSelect.h"
#include <llvm/Support/raw_ostream.h>
#include "llvm/Support/FormattedStream.h"
#include <llvm/ExecutionEngine/JITEventListener.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/DebugInfo/DIContext.h>
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#include <llvm/IR/DebugInfo.h>
#include "fix_llvm_assert.h"

#include "julia.h"
#include "julia_internal.h"

using namespace llvm;

extern JL_DLLEXPORT LLVMContext &jl_LLVMContext;

namespace {
#define FuncMCView ArrayRef<uint8_t>

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
    object::section_iterator ESection = object->section_end();
    for (const object::SymbolRef &Sym : object->symbols()) {
        uint64_t Addr, SAddr;
        object::section_iterator Sect = ESection;
        auto SectOrError = Sym.getSection();
        assert(SectOrError);
        Sect = SectOrError.get();
        if (Sect == ESection) continue;
        SAddr = Sect->getAddress();
        if (SAddr == 0) continue;
        auto AddrOrError = Sym.getAddress();
        assert(AddrOrError);
        Addr = AddrOrError.get();
        if (Addr == offset) {
            auto sNameOrError = Sym.getName();
            if (sNameOrError)
                return sNameOrError.get();
        }
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

        MCSymbol *symb = Ctx.getOrCreateSymbol(StringRef(name.str()));
        assert(symb->isUndefined());
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
                *ReferenceType = LLVMDisassembler_ReferenceType_Out_LitPool_SymAddr;
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
                          const object::ObjectFile *object,
                          DIContext *di_ctx,
                          raw_ostream &rstream,
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
    std::unique_ptr<MCStreamer> Streamer;
    SourceMgr SrcMgr;

    std::unique_ptr<MCAsmInfo> MAI(TheTarget->createMCAsmInfo(*TheTarget->createMCRegInfo(TripleName),TripleName));
    assert(MAI && "Unable to create target asm info!");

    std::unique_ptr<MCRegisterInfo> MRI(TheTarget->createMCRegInfo(TripleName));
    assert(MRI && "Unable to create target register info!");

    std::unique_ptr<MCObjectFileInfo> MOFI(new MCObjectFileInfo());
    MCContext Ctx(MAI.get(), MRI.get(), MOFI.get(), &SrcMgr);
    MOFI->InitMCObjectFileInfo(TheTriple, /* PIC */ false,
                               CodeModel::Default, Ctx);

    // Set up Subtarget and Disassembler
    std::unique_ptr<MCSubtargetInfo>
        STI(TheTarget->createMCSubtargetInfo(TripleName, MCPU, Features.getString()));
    std::unique_ptr<MCDisassembler> DisAsm(TheTarget->createMCDisassembler(*STI, Ctx));
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

    std::unique_ptr<MCInstrInfo> MCII(TheTarget->createMCInstrInfo());
    std::unique_ptr<MCInstrAnalysis>
        MCIA(TheTarget->createMCInstrAnalysis(MCII.get()));
    MCInstPrinter *IP =
        TheTarget->createMCInstPrinter(TheTriple, OutputAsmVariant, *MAI, *MCII, *MRI);
    //IP->setPrintImmHex(true); // prefer hex or decimal immediates
    MCCodeEmitter *CE = 0;
    MCAsmBackend *MAB = 0;
    if (ShowEncoding) {
        CE = TheTarget->createMCCodeEmitter(*MCII, *MRI, Ctx);
#if JL_LLVM_VERSION >= 40000
        MCTargetOptions Options;
        MAB = TheTarget->createMCAsmBackend(*MRI, TripleName, MCPU, Options);
#else
        MAB = TheTarget->createMCAsmBackend(*MRI, TripleName, MCPU);
#endif
    }

    // createAsmStreamer expects a unique_ptr to a formatted stream, which means
    // it will destruct the stream when it is done. We cannot have this, so we
    // start out with a raw stream, and create formatted stream from it here.
    // LLVM will desctruct the formatted stream, and we keep the raw stream.
    auto ustream = llvm::make_unique<formatted_raw_ostream>(rstream);
    Streamer.reset(TheTarget->createAsmStreamer(Ctx, std::move(ustream), /*asmverbose*/true,
                                                /*useDwarfDirectory*/ true,
                                                IP, CE, MAB, /*ShowInst*/ false));
    Streamer->InitSections(true);

    // Make the MemoryObject wrapper
    ArrayRef<uint8_t> memoryObject(const_cast<uint8_t*>((const uint8_t*)Fptr),Fsize);
    SymbolTable DisInfo(Ctx, object, slide, memoryObject);

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
            DisAsm->setSymbolizer(std::unique_ptr<MCSymbolizer>(new MCExternalSymbolizer(
                        Ctx,
                        std::unique_ptr<MCRelocationInfo>(new MCRelocationInfo(Ctx)),
                        OpInfoLookup,
                        SymbolLookup,
                        &DisInfo)));
        }

        uint64_t nextLineAddr = -1;
        DILineInfoTable::iterator di_lineIter = di_lineinfo.begin();
        DILineInfoTable::iterator di_lineEnd = di_lineinfo.end();
        if (pass != 0) {
            if (di_ctx) {
                // Set up the line info
                if (di_lineIter != di_lineEnd) {
                    std::ostringstream buf;
                    buf << "Filename: " << di_lineIter->second.FileName << "\n";
                    Streamer->EmitRawText(buf.str());
                    if (di_lineIter->second.Line <= 0)
                        ++di_lineIter;
                    nextLineAddr = di_lineIter->first;
                }
            }
        }

        uint64_t Index = 0;
        uint64_t insSize = 0;

        // Do the disassembly
        for (Index = 0; Index < Fsize; Index += insSize) {

            if (nextLineAddr != (uint64_t)-1 && Index + Fptr + slide == nextLineAddr) {
                if (di_ctx) {
                    std::ostringstream buf;
                    buf << "Source line: " << di_lineIter->second.Line << "\n";
                    Streamer->EmitRawText(buf.str());
                    nextLineAddr = (++di_lineIter)->first;
                }
            }

            DisInfo.setIP(Fptr+Index);
            if (pass != 0) {
                // Uncomment this to output addresses for all instructions
                // stream << Index << ": ";
                MCSymbol *symbol = DisInfo.lookupSymbol(Fptr+Index);
                if (symbol)
                    Streamer->EmitLabel(symbol);
                    // emitInstructionAnnot
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
                        if (MCIA->evaluateBranch(Inst, Fptr+Index, insSize, addr))
                            DisInfo.insertAddress(addr);
                    }
                }
                else {
                    // Pass 1: Output instruction
                    Streamer->EmitInstruction(Inst, *STI);
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
