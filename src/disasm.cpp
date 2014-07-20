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

static MCInstPrinter *jl_IP;
static MCCodeEmitter *jl_CE;
static MCAsmBackend *jl_MAB;
static const Target *jl_TheTarget;
static MCContext *jl_Ctx;
static SourceMgr *jl_SrcMgr;
static MCDisassembler *jl_DisAsm;
static MCStreamer *jl_Streamer;
static llvm::raw_null_ostream jl_null_ostream;
static llvm::formatted_raw_ostream jl_fstream(jl_null_ostream);

void jl_init_mcctx() {
    const unsigned OutputAsmVariant = 1;
    const bool ShowEncoding = false;
    const bool ShowInst = false;

    // Get the host information
    std::string TripleName;
    if (TripleName.empty())
        TripleName = sys::getDefaultTargetTriple();
    Triple TheTriple(Triple::normalize(TripleName));

    std::string MCPU = sys::getHostCPUName();
    SubtargetFeatures Features;
    Features.getDefaultSubtargetFeatures(TheTriple);

    std::string err;
    jl_TheTarget = TargetRegistry::lookupTarget(TripleName, err);

    // Set up required helpers
    jl_SrcMgr = new SourceMgr();

#if defined(LLVM34)
    MCAsmInfo *MAI = jl_TheTarget->createMCAsmInfo(*jl_TheTarget->createMCRegInfo(TripleName),TripleName);
#else
    MCAsmInfo *MAI = jl_TheTarget->createMCAsmInfo(TripleName);
#endif
    assert(MAI && "Unable to create target asm info!");

    MCRegisterInfo *MRI = jl_TheTarget->createMCRegInfo(TripleName);
    assert(MRI && "Unable to create target register info!");

    MCObjectFileInfo *MOFI = new MCObjectFileInfo();
#ifdef LLVM34
    jl_Ctx = new MCContext(MAI, MRI, MOFI, jl_SrcMgr);
#else
    jl_Ctx = new MCContext(*MAI, *MRI, MOFI, jl_SrcMgr);
#endif
    MOFI->InitMCObjectFileInfo(TripleName, Reloc::Default, CodeModel::Default, *jl_Ctx);

    // Set up Subtarget and Disassembler
    MCSubtargetInfo *STI = jl_TheTarget->createMCSubtargetInfo(TripleName, MCPU, Features.getString());
#ifdef LLVM35
    jl_DisAsm = jl_TheTarget->createMCDisassembler(*STI, *jl_Ctx);
#else
    jl_DisAsm = jl_TheTarget->createMCDisassembler(*STI);
#endif
    if (!jl_DisAsm) {
        JL_PRINTF(JL_STDERR, "warning: no disassembler for target", TripleName.c_str(), ". code_native functionality disabled.\n");
        return;
    }
    MCInstrInfo *MCII = jl_TheTarget->createMCInstrInfo();
    jl_IP = jl_TheTarget->createMCInstPrinter(OutputAsmVariant, *MAI, *MCII, *MRI, *STI);
    if (ShowEncoding) {
        jl_CE = jl_TheTarget->createMCCodeEmitter(*MCII, *MRI, *STI, *jl_Ctx);
#ifdef LLVM34
        jl_MAB = jl_TheTarget->createMCAsmBackend(*MRI, TripleName, MCPU);
#else
        jl_MAB = jl_TheTarget->createMCAsmBackend(TripleName, MCPU);
#endif
    }

    jl_Streamer = jl_TheTarget->createAsmStreamer(*jl_Ctx, jl_fstream, /*asmverbose*/true,
#ifndef LLVM35
                                           /*useLoc*/ true,
                                           /*useCFI*/ true,
#endif
                                           /*useDwarfDirectory*/ true,
                                           jl_IP, jl_CE, jl_MAB, ShowInst);
}

#ifndef USE_MCJIT
extern "C"
void jl_dump_function_asm(void *Fptr, size_t Fsize,
                          std::vector<JITEvent_EmittedFunctionDetails::LineStart> lineinfo,
                          raw_string_ostream &stream) {
#else
extern "C"
void jl_dump_function_asm(void *Fptr, size_t Fsize,
                          object::ObjectFile *objectfile,
                          raw_string_ostream &stream) {
#endif
    if (!jl_DisAsm) {
        JL_PRINTF(JL_STDERR, "error: no disassembler for target\n");
        return;
    }

#ifndef USE_MCJIT
// LLVM33- version
    jl_fstream.setStream(stream);
    jl_Streamer->InitSections();

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

        jl_fstream << "Filename: " << debugscope.getFilename() << "\n";
        jl_fstream << "Source line: " << (*lineIter).Loc.getLine() << "\n";
    }

    // Do the disassembly
    for (Index = 0, absAddr = (uint64_t)Fptr;
         Index < memoryObject.getExtent(); Index += Size, absAddr += Size) {

        if (nextLineAddr != (uint64_t)-1 && absAddr == nextLineAddr) {
            jl_fstream << "Source line: " << (*lineIter).Loc.getLine() << "\n";
            nextLineAddr = (*++lineIter).Address;
        }

        MCInst Inst;

        MCDisassembler::DecodeStatus S;
        S = jl_DisAsm->getInstruction(Inst, Size, memoryObject, Index,
                                  /*REMOVE*/ nulls(), nulls());
        switch (S) {
        case MCDisassembler::Fail:
        jl_SrcMgr->PrintMessage(SMLoc::getFromPointer(memoryObject[Index]),
                            SourceMgr::DK_Warning,
                            "invalid instruction encoding");
        if (Size == 0)
            Size = 1; // skip illegible bytes
        break;

        case MCDisassembler::SoftFail:
        jl_SrcMgr->PrintMessage(SMLoc::getFromPointer(memoryObject[Index]),
                            SourceMgr::DK_Warning,
                            "potentially undefined instruction encoding");
        // Fall through

        case MCDisassembler::Success:
        #ifdef LLVM35
            jl_Streamer->EmitInstruction(Inst, *STI);
        #else
            jl_Streamer->EmitInstruction(Inst);
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
    
    jl_fstream.setStream(stream);
    jl_Streamer->InitSections();

    // Set up the line info
    DILineInfoTable::iterator lines_iter = lineinfo.begin();
    DILineInfoTable::iterator lines_end = lineinfo.end();

    uint64_t nextLineAddr = -1;

    if (lines_iter != lineinfo.end()) {
        nextLineAddr = lines_iter->first;
        #ifdef LLVM35
        jl_fstream << "Filename: " << lines_iter->second.FileName << "\n";
        #else
        jl_fstream << "Filename: " << lines_iter->second.getFileName() << "\n";
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
            jl_fstream << "Source line: " << lines_iter->second.Line << "\n";
            #else
            jl_fstream << "Source line: " << lines_iter->second.getLine() << "\n";
            #endif
            nextLineAddr = (++lines_iter)->first;
        }

        MCInst Inst;
        MCDisassembler::DecodeStatus S;
        S = jl_DisAsm->getInstruction(Inst, insSize, memoryObject, Index,
                                  /*REMOVE*/ nulls(), nulls());
        switch (S) {
        case MCDisassembler::Fail:
        jl_SrcMgr.PrintMessage(SMLoc::getFromPointer(memoryObject[Index]),
                            SourceMgr::DK_Warning,
                            "invalid instruction encoding");
        if (insSize == 0)
            insSize = 1; // skip illegible bytes
        break;

        case MCDisassembler::SoftFail:
        jl_SrcMgr.PrintMessage(SMLoc::getFromPointer(memoryObject[Index]),
                            SourceMgr::DK_Warning,
                            "potentially undefined instruction encoding");
        // Fall through
        case MCDisassembler::Success:
        #ifdef LLVM35
            jl_Streamer->EmitInstruction(Inst, *STI);
        #else
            jl_Streamer->EmitInstruction(Inst);
        #endif
        break;
        }
    }
#endif
    jl_Streamer->Finish();
    jl_fstream.flush();
    jl_fstream.setStream(jl_null_ostream); // release the stream
}
