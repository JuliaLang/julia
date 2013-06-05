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

#include "llvm/MC/MCDisassembler.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/MemoryObject.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/system_error.h"

#include <string>
#include <iostream>

using namespace llvm;

namespace {
class FuncMCView : public MemoryObject {
private:
    const char* Fptr;
    size_t Fsize;
public:
    FuncMCView(const void* fptr, size_t size) : Fptr((char*)fptr), Fsize(size) {}

    const char* operator[] (const int idx) { return (Fptr+idx); }

    uint64_t getBase() const { return 0; }
    uint64_t getExtent() const { return Fsize; }

    int readByte(uint64_t Addr, uint8_t *Byte) const {
        if (Addr >= getExtent())
            return -1;
        *Byte = Fptr[Addr];
        return 0;
    }
};
}

extern "C"
void jl_dump_function_asm(void* Fptr, size_t Fsize,
                          formatted_raw_ostream &stream) {

    // Initialize targets and assembly printers/parsers.
    // Avoids hard-coded targets - will generally be only host CPU anyway.
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllDisassemblers();
  
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

    // Set up Subtarget and Disassembler
    OwningPtr<MCSubtargetInfo>
        STI(TheTarget->createMCSubtargetInfo(TripleName, MCPU, Features.getString()));

    OwningPtr<const MCDisassembler> DisAsm(TheTarget->createMCDisassembler(*STI));
    if (!DisAsm) {
        JL_PRINTF(JL_STDERR, "error: no disassembler for target", TripleName.c_str(), "\n");
        return;
    }
 
    // Set up required helpers and streamer 
    OwningPtr<MCStreamer> Streamer;
    SourceMgr SrcMgr;

    llvm::OwningPtr<MCAsmInfo> MAI(TheTarget->createMCAsmInfo(TripleName));
    assert(MAI && "Unable to create target asm info!");

    llvm::OwningPtr<MCRegisterInfo> MRI(TheTarget->createMCRegInfo(TripleName));
    assert(MRI && "Unable to create target register info!");

    OwningPtr<MCObjectFileInfo> MOFI(new MCObjectFileInfo());
    MCContext Ctx(*MAI, *MRI, MOFI.get(), &SrcMgr);

    unsigned OutputAsmVariant = 1;
    bool ShowEncoding = false;
    bool ShowInst = false;

    OwningPtr<MCInstrInfo> MCII(TheTarget->createMCInstrInfo());
    MCInstPrinter* IP =
        TheTarget->createMCInstPrinter(OutputAsmVariant, *MAI, *MCII, *MRI, *STI);
    MCCodeEmitter *CE = 0;
    MCAsmBackend *MAB = 0;
    if (ShowEncoding) {
        CE = TheTarget->createMCCodeEmitter(*MCII, *MRI, *STI, Ctx);
        MAB = TheTarget->createMCAsmBackend(TripleName, MCPU);
    }

    Streamer.reset(TheTarget->createAsmStreamer(Ctx, stream, /*asmverbose*/true,
                                           /*useLoc*/ true,
                                           /*useCFI*/ true,
                                           /*useDwarfDirectory*/ true,
                                           IP, CE, MAB, ShowInst));
    // Make the MemoryObject wrapper
    FuncMCView memoryObject(Fptr, Fsize);
  
    uint64_t Size;
    uint64_t Index;

    // Do the disassembly
    for (Index = 0; Index < memoryObject.getExtent(); Index += Size) {
        MCInst Inst;

        MCDisassembler::DecodeStatus S;
        S = DisAsm->getInstruction(Inst, Size, memoryObject, Index,
                                  /*REMOVE*/ nulls(), nulls());
        switch (S) {
        case MCDisassembler::Fail:
        SrcMgr.PrintMessage(SMLoc::getFromPointer(memoryObject[Index]),
                            SourceMgr::DK_Warning,
                            "invalid instruction encoding");
        if (Size == 0)
            Size = 1; // skip illegible bytes
        break;

        case MCDisassembler::SoftFail:
        SrcMgr.PrintMessage(SMLoc::getFromPointer(memoryObject[Index]),
                            SourceMgr::DK_Warning,
                            "potentially undefined instruction encoding");
        // Fall through

        case MCDisassembler::Success:
            Streamer->EmitInstruction(Inst);
        break;
        }
    }
}
