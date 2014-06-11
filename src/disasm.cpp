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

#include <string>
#include <iostream>

using namespace llvm;

namespace {
class FuncMCView : public MemoryObject {
private:
    const char* Fptr;
    size_t Fsize;
public:
    FuncMCView(const void* fptr, size_t size) : Fptr((const char*)fptr), Fsize(size) {}

    const char* operator[] (const size_t idx) { return (Fptr+idx); }

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

#ifndef USE_MCJIT
extern "C"
void jl_dump_function_asm(void *Fptr, size_t Fsize,
                          std::vector<JITEvent_EmittedFunctionDetails::LineStart> lineinfo,
                          formatted_raw_ostream &stream) {
#else
extern "C"
void jl_dump_function_asm(void *Fptr, size_t Fsize,
                          object::ObjectFile *objectfile,
                          formatted_raw_ostream &stream) {
#endif

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
    OwningPtr<MCStreamer> Streamer;
    SourceMgr SrcMgr;

#ifdef LLVM34
    llvm::OwningPtr<MCAsmInfo> MAI(TheTarget->createMCAsmInfo(*TheTarget->createMCRegInfo(TripleName),TripleName));
#else
    llvm::OwningPtr<MCAsmInfo> MAI(TheTarget->createMCAsmInfo(TripleName));
#endif
    assert(MAI && "Unable to create target asm info!");

    llvm::OwningPtr<MCRegisterInfo> MRI(TheTarget->createMCRegInfo(TripleName));
    assert(MRI && "Unable to create target register info!");

    OwningPtr<MCObjectFileInfo> MOFI(new MCObjectFileInfo());
#ifdef LLVM34
    MCContext Ctx(MAI.get(), MRI.get(), MOFI.get(), &SrcMgr);
#else
    MCContext Ctx(*MAI, *MRI, MOFI.get(), &SrcMgr);
#endif    
    MOFI->InitMCObjectFileInfo(TripleName, Reloc::Default, CodeModel::Default, Ctx);

    // Set up Subtarget and Disassembler
    OwningPtr<MCSubtargetInfo>
        STI(TheTarget->createMCSubtargetInfo(TripleName, MCPU, Features.getString()));
#ifdef LLVM35
    OwningPtr<const MCDisassembler> DisAsm(TheTarget->createMCDisassembler(*STI, Ctx));
#else
    OwningPtr<const MCDisassembler> DisAsm(TheTarget->createMCDisassembler(*STI));
#endif
    if (!DisAsm) {
        JL_PRINTF(JL_STDERR, "error: no disassembler for target", TripleName.c_str(), "\n");
        return;
    }

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
    Streamer->InitSections();

#ifndef USE_MCJIT
// LLVM33 version
    // Make the MemoryObject wrapper
    FuncMCView memoryObject(Fptr, Fsize);

    uint64_t Size = 0;
    uint64_t Index = 0;
    uint64_t absAddr = 0;

    // Set up the line info
    typedef std::vector<JITEvent_EmittedFunctionDetails::LineStart> LInfoVec;
    LInfoVec::iterator lineIter = lineinfo.begin();
    lineIter = lineinfo.begin();
    uint64_t nextLineAddr = -1;
    DISubprogram debugscope;

    if (lineIter != lineinfo.end()) {
        nextLineAddr = (*lineIter).Address;
        debugscope = DISubprogram((*lineIter).Loc.getScope(jl_LLVMContext));

        stream << "Filename: " << debugscope.getFilename() << "\n";
        stream << "Source line: " << (*lineIter).Loc.getLine() << "\n";
    }
    
    // Do the disassembly
    for (Index = 0, absAddr = (uint64_t)Fptr;
         Index < memoryObject.getExtent(); Index += Size, absAddr += Size) {
        
        if (nextLineAddr != (uint64_t)-1 && absAddr == nextLineAddr) {
            stream << "Source line: " << (*lineIter).Loc.getLine() << "\n";
            nextLineAddr = (*++lineIter).Address;
        }

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
        #ifdef LLVM35
            Streamer->EmitInstruction(Inst, *STI);
        #else
            Streamer->EmitInstruction(Inst);
        #endif
        break;
        }
    }
#else // MCJIT version
    FuncMCView memoryObject(Fptr, Fsize); // MemoryObject wrapper

    if (!objectfile) return;
    DIContext *di_ctx = DIContext::getDWARFContext(objectfile);
    if (di_ctx == NULL) return;
    DILineInfoTable lineinfo = di_ctx->getLineInfoForAddressRange((size_t)Fptr, Fsize);

    // Set up the line info
    DILineInfoTable::iterator lines_iter = lineinfo.begin();
    DILineInfoTable::iterator lines_end = lineinfo.end();

    uint64_t nextLineAddr = -1;

    if (lines_iter != lineinfo.end()) {
        nextLineAddr = lines_iter->first;
        #ifdef LLVM35
        stream << "Filename: " << lines_iter->second.FileName << "\n";
        #else
        stream << "Filename: " << lines_iter->second.getFileName() << "\n";
        #endif
    }

    uint64_t Index = 0;
    uint64_t absAddr = 0;
    uint64_t insSize = 0; 

    // Do the disassembly
    for (Index = 0, absAddr = (uint64_t)Fptr;
         Index < memoryObject.getExtent(); Index += insSize, absAddr += insSize) {
        
        if (nextLineAddr != (uint64_t)-1 && absAddr == nextLineAddr) {
            #ifdef LLVM35
            stream << "Source line: " << lines_iter->second.Line << "\n";
            #else
            stream << "Source line: " << lines_iter->second.getLine() << "\n";
            #endif
            nextLineAddr = (++lines_iter)->first;
        }

        MCInst Inst;
        MCDisassembler::DecodeStatus S;
        S = DisAsm->getInstruction(Inst, insSize, memoryObject, Index,
                                  /*REMOVE*/ nulls(), nulls());
        switch (S) {
        case MCDisassembler::Fail:
        SrcMgr.PrintMessage(SMLoc::getFromPointer(memoryObject[Index]),
                            SourceMgr::DK_Warning,
                            "invalid instruction encoding");
        if (insSize == 0)
            insSize = 1; // skip illegible bytes
        break;

        case MCDisassembler::SoftFail:
        SrcMgr.PrintMessage(SMLoc::getFromPointer(memoryObject[Index]),
                            SourceMgr::DK_Warning,
                            "potentially undefined instruction encoding");
        // Fall through
        case MCDisassembler::Success:
        #ifdef LLVM35
            Streamer->EmitInstruction(Inst, *STI);
        #else
            Streamer->EmitInstruction(Inst);
        #endif
        break;
        }
    }

#endif
}
