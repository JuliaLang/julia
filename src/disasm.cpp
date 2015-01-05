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
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <string>

#include "llvm-version.h"
#include <llvm/Object/ObjectFile.h>
#include <llvm/Support/MachO.h>
#include <llvm/Support/COFF.h>
#include <llvm/MC/MCDisassembler.h>
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
#ifdef LLVM35
#include <llvm/AsmParser/Parser.h>
#include <llvm/MC/MCExternalSymbolizer.h>
#else
#include <llvm/Assembly/Parser.h>
#include <llvm/ADT/OwningPtr.h>
#endif
#include <llvm/ADT/Triple.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/MemoryObject.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/Host.h>
#include "llvm/Support/TargetSelect.h"
#include <llvm/Support/raw_ostream.h>
#include "llvm/Support/FormattedStream.h"
#ifndef LLVM35
#include <llvm/Support/system_error.h>
#endif
#include <llvm/ExecutionEngine/JITEventListener.h>
#ifdef LLVM33
#include <llvm/IR/LLVMContext.h>
#else
#include <llvm/LLVMContext.h>
#endif
#include <llvm/DebugInfo/DIContext.h>
#ifdef LLVM35
#include <llvm/IR/DebugInfo.h>
#elif defined(LLVM32)
#include <llvm/DebugInfo.h>
#else
#include <llvm/Analysis/DebugInfo.h>
#endif

#include "julia.h"

using namespace llvm;

extern DLLEXPORT LLVMContext &jl_LLVMContext;

namespace {
#ifdef LLVM36
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
public:
    SymbolTable(MCContext &Ctx, const FuncMCView &MemObj):
        Ctx(Ctx), MemObj(MemObj) {}
    const FuncMCView &getMemoryObject() const { return MemObj; }
    void setPass(int Pass) { this->Pass = Pass; }
    int getPass() const { return Pass; }
    void insertAddress(uint64_t addr);
    // void createSymbol(const char *name, uint64_t addr);
    void createSymbols();
    const char *lookupSymbol(uint64_t addr);
};
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
    for (TableType::iterator isymb = Table.begin(), esymb = Table.end();
         isymb != esymb; ++isymb) {
        uint64_t addr = isymb->first;
        std::ostringstream name;
        name << "L" << addr;
        MCSymbol *symb = Ctx.GetOrCreateSymbol(StringRef(name.str()));
        symb->setVariableValue(MCConstantExpr::Create(addr, Ctx));
        isymb->second = symb;
    }
}
const char *SymbolTable::lookupSymbol(uint64_t addr)
{
    if (!Table.count(addr)) return NULL;
    MCSymbol *symb = Table[addr];
    TempName = symb->getName().str();
    return TempName.c_str();
}

const char *SymbolLookup(void *DisInfo,
                         uint64_t ReferenceValue,
                         uint64_t *ReferenceType,
                         uint64_t ReferencePC,
                         const char **ReferenceName)
{
    SymbolTable *SymTab = (SymbolTable*)DisInfo;
    if (SymTab->getPass() != 0) {
        if (*ReferenceType == LLVMDisassembler_ReferenceType_In_Branch) {
            uint64_t addr = ReferenceValue;
            const char *symbolName = SymTab->lookupSymbol(addr);
            *ReferenceType = LLVMDisassembler_ReferenceType_InOut_None;
            *ReferenceName = NULL;
            return symbolName;
        }
    }
    *ReferenceType = LLVMDisassembler_ReferenceType_InOut_None;
    *ReferenceName = NULL;
    return NULL;
}

extern "C" void jl_getFunctionInfo
  (const char **name, size_t *line, const char **filename, uintptr_t pointer,
   int *fromC, int skipC);
int OpInfoLookup(void *DisInfo, uint64_t PC,
                 uint64_t Offset, uint64_t Size,
                 int TagType, void *TagBuf)
{
    SymbolTable *SymTab = (SymbolTable*)DisInfo;
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
    int skipC = 0;
    const char *name;
    size_t line;
    const char *filename;
    int fromC;
    jl_getFunctionInfo(&name, &line, &filename, pointer, &fromC, skipC);
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

extern "C"
void jl_dump_function_asm(const char *Fptr, size_t Fsize,
#ifndef USE_MCJIT
                          std::vector<JITEvent_EmittedFunctionDetails::LineStart> lineinfo,
#else
                          const object::ObjectFile *objectfile,
#endif
                          formatted_raw_ostream &stream)
{
    // Initialize targets and assembly printers/parsers.
    // Avoids hard-coded targets - will generally be only host CPU anyway.
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetDisassembler();

    // Get the host information
    std::string TripleName;
    if (TripleName.empty())
        TripleName = sys::getDefaultTargetTriple();
    Triple TheTriple(Triple::normalize(TripleName));

    std::string MCPU = sys::getHostCPUName();
    SubtargetFeatures Features;
    Features.getDefaultSubtargetFeatures(TheTriple);

    std::string err;
    const Target* TheTarget = TargetRegistry::lookupTarget(TripleName, err);

    // Set up required helpers and streamer
#ifdef LLVM35
    std::unique_ptr<MCStreamer> Streamer;
#else
    OwningPtr<MCStreamer> Streamer;
#endif
    SourceMgr SrcMgr;

#ifdef LLVM35
    std::unique_ptr<MCAsmInfo> MAI(TheTarget->createMCAsmInfo(*TheTarget->createMCRegInfo(TripleName),TripleName));
#elif defined(LLVM34)
    llvm::OwningPtr<MCAsmInfo> MAI(TheTarget->createMCAsmInfo(*TheTarget->createMCRegInfo(TripleName),TripleName));
#else
    llvm::OwningPtr<MCAsmInfo> MAI(TheTarget->createMCAsmInfo(TripleName));
#endif
    assert(MAI && "Unable to create target asm info!");

#ifdef LLVM35
    std::unique_ptr<MCRegisterInfo> MRI(TheTarget->createMCRegInfo(TripleName));
#else
    llvm::OwningPtr<MCRegisterInfo> MRI(TheTarget->createMCRegInfo(TripleName));
#endif
    assert(MRI && "Unable to create target register info!");

#ifdef LLVM35
    std::unique_ptr<MCObjectFileInfo> MOFI(new MCObjectFileInfo());
#else
    OwningPtr<MCObjectFileInfo> MOFI(new MCObjectFileInfo());
#endif
#ifdef LLVM34
    MCContext Ctx(MAI.get(), MRI.get(), MOFI.get(), &SrcMgr);
#else
    MCContext Ctx(*MAI, *MRI, MOFI.get(), &SrcMgr);
#endif
    MOFI->InitMCObjectFileInfo(TripleName, Reloc::Default, CodeModel::Default, Ctx);

    // Set up Subtarget and Disassembler
#ifdef LLVM35
    std::unique_ptr<MCSubtargetInfo>
        STI(TheTarget->createMCSubtargetInfo(TripleName, MCPU, Features.getString()));
    std::unique_ptr<MCDisassembler> DisAsm(TheTarget->createMCDisassembler(*STI, Ctx));
#else
    OwningPtr<MCSubtargetInfo>
        STI(TheTarget->createMCSubtargetInfo(TripleName, MCPU, Features.getString()));
    OwningPtr<MCDisassembler> DisAsm(TheTarget->createMCDisassembler(*STI));
#endif
    if (!DisAsm) {
        JL_PRINTF(JL_STDERR, "error: no disassembler for target", TripleName.c_str(), "\n");
        return;
    }

    unsigned OutputAsmVariant = 1;
    bool ShowEncoding = false;
    bool ShowInst = false;

#ifdef LLVM35
    std::unique_ptr<MCInstrInfo> MCII(TheTarget->createMCInstrInfo());
    std::unique_ptr<MCInstrAnalysis>
        MCIA(TheTarget->createMCInstrAnalysis(MCII.get()));
#else
    OwningPtr<MCInstrInfo> MCII(TheTarget->createMCInstrInfo());
    OwningPtr<MCInstrAnalysis>
        MCIA(TheTarget->createMCInstrAnalysis(MCII.get()));
#endif
    MCInstPrinter* IP =
        TheTarget->createMCInstPrinter(OutputAsmVariant, *MAI, *MCII, *MRI, *STI);
    MCCodeEmitter *CE = 0;
    MCAsmBackend *MAB = 0;
    if (ShowEncoding) {
        CE = TheTarget->createMCCodeEmitter(*MCII, *MRI, *STI, Ctx);
#ifdef LLVM34
        MAB = TheTarget->createMCAsmBackend(*MRI, TripleName, MCPU);
#else
        MAB = TheTarget->createMCAsmBackend(TripleName, MCPU);
#endif
    }

    Streamer.reset(TheTarget->createAsmStreamer(Ctx, stream, /*asmverbose*/true,
#ifndef LLVM35
                                           /*useLoc*/ true,
                                           /*useCFI*/ true,
#endif
                                           /*useDwarfDirectory*/ true,
                                           IP, CE, MAB, ShowInst));
#ifdef LLVM36
    Streamer->InitSections(true);
#else
    Streamer->InitSections();
#endif

    // Make the MemoryObject wrapper
#ifdef LLVM36
    ArrayRef<uint8_t> memoryObject(const_cast<uint8_t*>((const uint8_t*)Fptr),Fsize);
#else
    FuncMCView memoryObject(Fptr, Fsize);
#endif
    SymbolTable DisInfo(Ctx, memoryObject);

#ifdef USE_MCJIT
    if (!objectfile) return;
#ifdef LLVM36
    DIContext *di_ctx = DIContext::getDWARFContext(*objectfile);
#else
    DIContext *di_ctx = DIContext::getDWARFContext(const_cast<object::ObjectFile*>(objectfile));
#endif
    if (di_ctx == NULL) return;
    DILineInfoTable lineinfo = di_ctx->getLineInfoForAddressRange((size_t)Fptr, Fsize);
#else
    typedef std::vector<JITEvent_EmittedFunctionDetails::LineStart> LInfoVec;
#endif

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
#ifdef LLVM35
            DisAsm->setSymbolizer(std::unique_ptr<MCSymbolizer>(new MCExternalSymbolizer(
                        Ctx,
                        std::unique_ptr<MCRelocationInfo>(new MCRelocationInfo(Ctx)),
                        OpInfoLookup,
                        SymbolLookup,
                        &DisInfo)));
#else
            DisAsm->setupForSymbolicDisassembly(
                    OpInfoLookup, SymbolLookup, &DisInfo, &Ctx);
#endif
        }


        uint64_t nextLineAddr = -1;
#ifdef USE_MCJIT
        // Set up the line info
        DILineInfoTable::iterator lineIter = lineinfo.begin();
        DILineInfoTable::iterator lineEnd = lineinfo.end();

        if (lineIter != lineEnd) {
            nextLineAddr = lineIter->first;
            if (pass != 0) {
#ifdef LLVM35
                stream << "Filename: " << lineIter->second.FileName << "\n";
#else
                stream << "Filename: " << lineIter->second.getFileName() << "\n";
#endif
            }
        }
#else
        // Set up the line info
        LInfoVec::iterator lineIter = lineinfo.begin();
        LInfoVec::iterator lineEnd  = lineinfo.end();

        if (lineIter != lineEnd) {
            nextLineAddr = (*lineIter).Address;
            DISubprogram debugscope = DISubprogram((*lineIter).Loc.getScope(jl_LLVMContext));
            if (pass != 0) {
                stream << "Filename: " << debugscope.getFilename() << "\n";
                stream << "Source line: " << (*lineIter).Loc.getLine() << "\n";
            }
        }
#endif

        uint64_t Index = 0;
        uint64_t absAddr = 0;
        uint64_t insSize = 0;

        // Do the disassembly
        for (Index = 0, absAddr = (uint64_t)Fptr;
             Index < Fsize; Index += insSize, absAddr += insSize) {

            if (nextLineAddr != (uint64_t)-1 && absAddr == nextLineAddr) {
#ifdef USE_MCJIT
#ifdef LLVM35
                if (pass != 0)
                    stream << "Source line: " << lineIter->second.Line << "\n";
#else
                if (pass != 0)
                    stream << "Source line: " << lineIter->second.getLine() << "\n";
#endif
                nextLineAddr = (++lineIter)->first;
#else
                if (pass != 0)
                    stream << "Source line: " << (*lineIter).Loc.getLine() << "\n";
                nextLineAddr = (*++lineIter).Address;
#endif
            }
            if (pass != 0) {
                // Uncomment this to output addresses for all instructions
                // stream << Index << ": ";
                const char *symbolName = DisInfo.lookupSymbol(Index);
                if (symbolName)
                    stream << symbolName << ":";
            }

            MCInst Inst;
            MCDisassembler::DecodeStatus S;
            S = DisAsm->getInstruction(Inst, insSize, memoryObject, Index,
                                      /*REMOVE*/ nulls(), nulls());
            switch (S) {
            case MCDisassembler::Fail:
                if (pass != 0)
                    SrcMgr.PrintMessage(SMLoc::getFromPointer(Fptr + Index),
                                        SourceMgr::DK_Warning,
                                        "invalid instruction encoding");
                if (insSize == 0)
                    insSize = 1; // skip illegible bytes
                break;

            case MCDisassembler::SoftFail:
                if (pass != 0)
                    SrcMgr.PrintMessage(SMLoc::getFromPointer(Fptr + Index),
                                        SourceMgr::DK_Warning,
                                        "potentially undefined instruction encoding");
                // Fall through

            case MCDisassembler::Success:
                if (pass == 0) {
                    // Pass 0: Record all branch targets
                    if (MCIA->isBranch(Inst)) {
                        uint64_t addr;
#ifdef LLVM35
                        if (MCIA->evaluateBranch(Inst, Index, insSize, addr))
#else
                        if ((addr = MCIA->evaluateBranch(Inst, Index, insSize)) != (uint64_t)-1)
#endif
                            DisInfo.insertAddress(addr);
                    }
                }
                else {
                    // Pass 1: Output instruction
#ifdef LLVM35
                    Streamer->EmitInstruction(Inst, *STI);
#else
                    Streamer->EmitInstruction(Inst);
#endif
                }
                break;
            }
        }

        if (pass == 0)
            DisInfo.createSymbols();
    }
}
