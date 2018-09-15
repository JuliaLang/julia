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
#include "llvm/Support/TargetSelect.h"
#include <llvm/Support/raw_ostream.h>
#include "llvm/Support/FormattedStream.h"
#include <llvm/ExecutionEngine/JITEventListener.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/DebugInfo/DIContext.h>
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IntrinsicInst.h>
#include "llvm/IR/AssemblyAnnotationWriter.h"

#include "julia.h"
#include "julia_internal.h"
#include "processor.h"

using namespace llvm;
#include "debuginfo.h"
#include "julia_assert.h"

// helper class for tracking inlining context while printing debug info
class DILineInfoPrinter {
    std::vector<DILineInfo> context;
    char LineStart;
    bool bracket_outer;
public:
    DILineInfoPrinter(char LineStart, bool bracket_outer)
        : LineStart(LineStart),
          bracket_outer(bracket_outer) {};
    void emit_finish(raw_ostream &Out);
    void emit_lineinfo(raw_ostream &Out, std::vector<DILineInfo> &DI);

    template<class T>
    void emit_lineinfo(std::string &Out, T &DI)
    {
        raw_string_ostream OS(Out);
        emit_lineinfo(OS, DI);
    }

    void emit_lineinfo(raw_ostream &Out, DILineInfo &DI)
    {
        std::vector<DILineInfo> DIvec(1);
        DIvec[0] = DI;
        emit_lineinfo(Out, DIvec);
    }

    void emit_lineinfo(raw_ostream &Out, DIInliningInfo &DI)
    {
        uint32_t nframes = DI.getNumberOfFrames();
        std::vector<DILineInfo> DIvec(nframes);
        for (uint32_t i = 0; i < DI.getNumberOfFrames(); i++) {
            DIvec[i] = DI.getFrame(i);
        }
        emit_lineinfo(Out, DIvec);
    }

    void emit_finish(std::string &Out)
    {
        raw_string_ostream OS(Out);
        emit_finish(OS);
    }
};

void DILineInfoPrinter::emit_finish(raw_ostream &Out)
{
    uint32_t npops = context.size();
    if (!bracket_outer && npops > 0)
        npops--;
    if (npops) {
        Out << LineStart;
        while (npops--)
            Out << '}';
        Out << '\n';
    }
    context.clear();
}

void DILineInfoPrinter::emit_lineinfo(raw_ostream &Out, std::vector<DILineInfo> &DI)
{
    bool update_line_only = false;
    uint32_t nctx = context.size();
    uint32_t nframes = DI.size();
    if (nframes == 0)
        return; // just skip over lines with no debug info at all
    if (nctx > nframes)
        context.resize(nframes);
    for (uint32_t i = 0; i < nctx && i < nframes; i++) {
        const DILineInfo &CtxLine = context.at(i);
        const DILineInfo &FrameLine = DI.at(nframes - 1 - i);
        if (CtxLine != FrameLine) {
            if (CtxLine.FileName == FrameLine.FileName &&
                    CtxLine.FunctionName == FrameLine.FunctionName) {
                update_line_only = true;
            }
            context.resize(i);
            break;
        }
    }
    uint32_t npops = nctx - context.size() - update_line_only;
    if (npops) {
        Out << LineStart;
        while (npops--)
            Out << '}';
        Out << '\n';
    }
    if (update_line_only) {
        DILineInfo frame = DI.at(nframes - 1 - context.size());
        if (frame.Line != UINT_MAX && frame.Line != 0)
            Out << LineStart << " Location: " << frame.FileName << ":" << frame.Line << '\n';
        context.push_back(frame);
    }
    for (uint32_t i = context.size(); i < nframes; i++) {
        DILineInfo frame = DI.at(nframes - 1 - i);
        context.push_back(frame);
        Out << LineStart << " Function " << frame.FunctionName;
        if (bracket_outer || i != 0)
            Out << " {";
        Out << "\n" << LineStart << " Location: " << frame.FileName;
        if (frame.Line != UINT_MAX && frame.Line != 0)
            Out << ":" << frame.Line;
        Out << "\n";
    }
}


// adaptor class for printing line numbers before llvm IR lines
class LineNumberAnnotatedWriter : public AssemblyAnnotationWriter {
    DILocation *InstrLoc = nullptr;
    DILineInfoPrinter LinePrinter{';', false};
    DenseMap<const Instruction *, DILocation *> DebugLoc;
    DenseMap<const Function *, DISubprogram *> Subprogram;
public:
    LineNumberAnnotatedWriter() {}
    virtual void emitFunctionAnnot(const Function *, formatted_raw_ostream &);
    virtual void emitInstructionAnnot(const Instruction *, formatted_raw_ostream &);
    virtual void emitBasicBlockEndAnnot(const BasicBlock *, formatted_raw_ostream &);
    // virtual void printInfoComment(const Value &, formatted_raw_ostream &) {}

    void addSubprogram(const Function *F, DISubprogram *SP)
    {
        Subprogram[F] = SP;
    }

    void addDebugLoc(const Instruction *I, DILocation *Loc)
    {
        DebugLoc[I] = Loc;
    }
};

void LineNumberAnnotatedWriter::emitFunctionAnnot(
      const Function *F, formatted_raw_ostream &Out)
{
    InstrLoc = nullptr;
    DISubprogram *FuncLoc = F->getSubprogram();
    if (!FuncLoc) {
        auto SP = Subprogram.find(F);
        if (SP != Subprogram.end())
            FuncLoc = SP->second;
    }
    if (!FuncLoc)
        return;
    std::vector<DILineInfo> DIvec(1);
    DILineInfo &DI = DIvec.back();
    DI.FunctionName = FuncLoc->getName();
    DI.FileName = FuncLoc->getFilename();
    DI.Line = FuncLoc->getLine();
    LinePrinter.emit_lineinfo(Out, DIvec);
}

void LineNumberAnnotatedWriter::emitInstructionAnnot(
      const Instruction *I, formatted_raw_ostream &Out)
{
    DILocation *NewInstrLoc = I->getDebugLoc();
    if (!NewInstrLoc) {
        auto Loc = DebugLoc.find(I);
        if (Loc != DebugLoc.end())
            NewInstrLoc = Loc->second;
    }
    if (!NewInstrLoc || NewInstrLoc == InstrLoc)
        return;
    InstrLoc = NewInstrLoc;
    std::vector<DILineInfo> DIvec;
    do {
        DIvec.emplace_back();
        DILineInfo &DI = DIvec.back();
        DIScope *scope = NewInstrLoc->getScope();
        if (scope)
            DI.FunctionName = scope->getName();
        DI.FileName = NewInstrLoc->getFilename();
        DI.Line = NewInstrLoc->getLine();
        NewInstrLoc = NewInstrLoc->getInlinedAt();
    } while (NewInstrLoc);
    LinePrinter.emit_lineinfo(Out, DIvec);
}

void LineNumberAnnotatedWriter::emitBasicBlockEndAnnot(
        const BasicBlock *BB, formatted_raw_ostream &Out)
{
    if (BB == &BB->getParent()->back())
        LinePrinter.emit_finish(Out);
}

// print an llvm IR acquired from jl_get_llvmf
// warning: this takes ownership of, and destroys, f->getParent()
extern "C" JL_DLLEXPORT
jl_value_t *jl_dump_function_ir(void *f, bool strip_ir_metadata, bool dump_module)
{
    std::string code;
    llvm::raw_string_ostream stream(code);

    Function *llvmf = dyn_cast_or_null<Function>((Function*)f);
    if (!llvmf || (!llvmf->isDeclaration() && !llvmf->getParent()))
        jl_error("jl_dump_function_ir: Expected Function* in a temporary Module");

    JL_LOCK(&codegen_lock); // Might GC
    LineNumberAnnotatedWriter AAW;
    if (!llvmf->getParent()) {
        // print the function declaration as-is
        llvmf->print(stream, &AAW);
        delete llvmf;
    }
    else {
        Module *m = llvmf->getParent();
        if (strip_ir_metadata) {
            // strip metadata from all instructions in all functions in the module
            Instruction *deletelast = nullptr; // can't actually delete until the iterator advances
            for (Function &f2 : m->functions()) {
                AAW.addSubprogram(&f2, f2.getSubprogram());
                for (BasicBlock &f2_bb : f2) {
                    for (Instruction &inst : f2_bb) {
                        if (deletelast) {
                            deletelast->eraseFromParent();
                            deletelast = nullptr;
                        }
                        // remove dbg.declare and dbg.value calls
                        if (isa<DbgDeclareInst>(inst) || isa<DbgValueInst>(inst)) {
                            deletelast = &inst;
                            continue;
                        }

                        // iterate over all metadata kinds and set to NULL to remove
                        SmallVector<std::pair<unsigned, MDNode*>, 4> MDForInst;
                        inst.getAllMetadataOtherThanDebugLoc(MDForInst);
                        for (const auto &md_iter : MDForInst) {
                            inst.setMetadata(md_iter.first, NULL);
                        }
                        // record debug location before erasing it
                        AAW.addDebugLoc(&inst, inst.getDebugLoc());
                        inst.setDebugLoc(DebugLoc());
                    }
                    if (deletelast) {
                        deletelast->eraseFromParent();
                        deletelast = nullptr;
                    }
                }
            }
            for (GlobalObject &g2 : m->global_objects()) {
                g2.clearMetadata();
            }
        }
        if (dump_module) {
            m->print(stream, &AAW);
        }
        else {
            llvmf->print(stream, &AAW);
        }
        delete m;
    }
    JL_UNLOCK(&codegen_lock); // Might GC

    return jl_pchar_to_string(stream.str().data(), stream.str().size());
}

static void jl_dump_asm_internal(
        uintptr_t Fptr, size_t Fsize, int64_t slide,
        const object::ObjectFile *object,
        DIContext *di_ctx,
        raw_ostream &rstream,
        const char* asm_variant);

// This isn't particularly fast, but neither is printing assembly, and they're only used for interactive mode
static uint64_t compute_obj_symsize(const object::ObjectFile *obj, uint64_t offset)
{
    // Scan the object file for the closest symbols above and below offset in the .text section
    uint64_t lo = 0;
    uint64_t hi = 0;
    bool setlo = false;
    for (const object::SectionRef &Section : obj->sections()) {
        uint64_t SAddr, SSize;
        if (!Section.isText()) continue;
        SAddr = Section.getAddress();
        SSize = Section.getSize();
        if (offset < SAddr || offset >= SAddr + SSize) continue;
        assert(hi == 0);

        // test for lower and upper symbol bounds relative to other symbols
        hi = SAddr + SSize;
        object::section_iterator ESection = obj->section_end();
        for (const object::SymbolRef &Sym : obj->symbols()) {
            uint64_t Addr;
            object::section_iterator Sect = ESection;
            auto SectOrError = Sym.getSection();
            assert(SectOrError);
            Sect = SectOrError.get();
            if (Sect == ESection) continue;
            if (Sect != Section) continue;
            auto AddrOrError = Sym.getAddress();
            assert(AddrOrError);
            Addr = AddrOrError.get();
            if (Addr <= offset && Addr >= lo) {
                // test for lower bound on symbol
                lo = Addr;
                setlo = true;
            }
            if (Addr > offset && Addr < hi) {
                // test for upper bound on symbol
                hi = Addr;
            }
        }
    }
    if (setlo)
        return hi - lo;
    return 0;
}

// print a native disassembly for the function starting at fptr
extern "C" JL_DLLEXPORT
jl_value_t *jl_dump_fptr_asm(uint64_t fptr, int raw_mc, const char* asm_variant)
{
    assert(fptr != 0);
    jl_ptls_t ptls = jl_get_ptls_states();
    std::string code;
    llvm::raw_string_ostream stream(code);

    // Find debug info (line numbers) to print alongside
    uint64_t symsize = 0;
    int64_t slide = 0, section_slide = 0;
    llvm::DIContext *context = NULL;
    const object::ObjectFile *object = NULL;
    if (!jl_DI_for_fptr(fptr, &symsize, &slide, &section_slide, &object, &context)) {
        if (!jl_dylib_DI_for_fptr(fptr, &object, &context, &slide, &section_slide, false,
            NULL, NULL, NULL, NULL)) {
                jl_printf(JL_STDERR, "WARNING: Unable to find function pointer\n");
                return jl_pchar_to_string("", 0);
        }
    }
    if (symsize == 0 && object != NULL)
        symsize = compute_obj_symsize(object, fptr + slide + section_slide);
    if (symsize == 0) {
        jl_printf(JL_STDERR, "WARNING: Could not determine size of symbol\n");
        return jl_pchar_to_string("", 0);
    }

    if (raw_mc) {
        return (jl_value_t*)jl_pchar_to_array((char*)fptr, symsize);
    }

    // Dump assembly code
    int8_t gc_state = jl_gc_safe_enter(ptls);
    jl_dump_asm_internal(
            fptr, symsize, slide,
            object, context,
            stream,
            asm_variant);
    jl_gc_safe_leave(ptls, gc_state);

    return jl_pchar_to_string(stream.str().data(), stream.str().size());
}


namespace {
#define FuncMCView ArrayRef<uint8_t>

// Look up a symbol, and return a const char* to its name when the
// address matches. We currently just use "L<address>" as name for the
// symbol. We could easily get more fancy, e.g. numbering symbols
// sequentially or encoding the line number, but that doesn't seem
// necessary.
class SymbolTable {
    typedef std::map<uint64_t, std::string> TableType;
    TableType Table;
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
    const char *lookupSymbolName(uint64_t addr);
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
    Table[addr] = "";
}
// Create symbols for all addresses
void SymbolTable::createSymbols()
{
    uintptr_t Fptr = (uintptr_t)MemObj.data();
    uintptr_t Fsize = MemObj.size();
    for (TableType::iterator isymb = Table.begin(), esymb = Table.end();
         isymb != esymb; ++isymb) {
        uintptr_t rel = isymb->first - ip;
        uintptr_t addr = isymb->first;
        if (Fptr <= addr && addr < Fptr + Fsize) {
            std::ostringstream name;
            name << "L" << rel;
            isymb->second = name.str();
        }
        else {
            const char *global = lookupLocalPC(addr);
            if (global)
                isymb->second = global;
        }
    }
}

const char *SymbolTable::lookupSymbolName(uint64_t addr)
{
    TableType::iterator Sym;
    bool insertion;
    std::tie(Sym, insertion) = Table.insert(std::make_pair(addr, std::string()));
    if (insertion) {
        // First time we've seen addr: try to look it up
        StringRef local_name = getSymbolNameAt(addr + slide);
        if (local_name.empty()) {
            const char *global = lookupLocalPC(addr);
            if (global) {
                //std::ostringstream name;
                //name << global << "@0x" << std::hex
                //     << std::setfill('0') << std::setw(2 * sizeof(void*))
                //     << addr;
                //Sym->second = name.str();
                Sym->second = global;
            }
        }
        else {
            Sym->second = local_name;
        }
    }
    return Sym->second.empty() ? NULL : Sym->second.c_str();
}

MCSymbol *SymbolTable::lookupSymbol(uint64_t addr)
{
    TableType::iterator Sym = Table.find(addr);
    if (Sym == Table.end() || Sym->second.empty())
        return NULL;
    MCSymbol *symb = Ctx.getOrCreateSymbol(Sym->second);
    assert(symb->isUndefined());
    return symb;
}

static const char *SymbolLookup(void *DisInfo, uint64_t ReferenceValue, uint64_t *ReferenceType,
                                uint64_t ReferencePC, const char **ReferenceName)
{
    uint64_t RTypeIn = *ReferenceType;
    SymbolTable *SymTab = (SymbolTable*)DisInfo;
    *ReferenceType = LLVMDisassembler_ReferenceType_InOut_None;
    *ReferenceName = NULL;
    if (SymTab->getPass() != 0) {
        if (RTypeIn == LLVMDisassembler_ReferenceType_In_Branch) {
            uint64_t addr = ReferenceValue + SymTab->getIP(); // probably pc-rel
            const char *symbolName = SymTab->lookupSymbolName(addr);
            return symbolName;
        }
        else if (RTypeIn == LLVMDisassembler_ReferenceType_In_PCrel_Load) {
            uint64_t addr = ReferenceValue + SymTab->getIP();
            const char *symbolName = SymTab->lookupSymbolName(addr);
            if (symbolName) {
                *ReferenceType = LLVMDisassembler_ReferenceType_Out_LitPool_SymAddr;
                *ReferenceName = symbolName;
            }
        }
        else if (RTypeIn == LLVMDisassembler_ReferenceType_InOut_None) {
            uint64_t addr = ReferenceValue; // probably not pc-rel
            const char *symbolName = SymTab->lookupSymbolName(addr);
            return symbolName;
        }
    }
    return NULL;
}

static int OpInfoLookup(void *DisInfo, uint64_t PC, uint64_t Offset, uint64_t Size,
                        int TagType, void *TagBuf)
{
    SymbolTable *SymTab = (SymbolTable*)DisInfo;
    LLVMOpInfo1 *info = (LLVMOpInfo1*)TagBuf;
    memset(info, 0, sizeof(*info));
    if (TagType != 1)
        return 0;               // Unknown data format
    PC += SymTab->getIP() - (uint64_t)(uintptr_t)SymTab->getMemoryObject().data(); // add offset from MemoryObject base
    // TODO: see if we knew of a relocation applied at PC
    // info->AddSymbol.Present = 1;
    // info->AddSymbol.Name = name;
    // info->AddSymbol.Value = pointer; // unused by LLVM
    // info->Value = 0;                 // offset
    // return 1;                        // Success
    return 0;
}
} // namespace

static void jl_dump_asm_internal(
        uintptr_t Fptr, size_t Fsize, int64_t slide,
        const object::ObjectFile *object,
        DIContext *di_ctx,
        raw_ostream &rstream,
        const char* asm_variant)
{
    // GC safe
    // Get the host information
    std::string TripleName = sys::getDefaultTargetTriple();
    Triple TheTriple(Triple::normalize(TripleName));

    const auto &target = jl_get_llvm_disasm_target();
    const auto &cpu = target.first;
    const auto &features = target.second;

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
#if JL_LLVM_VERSION >= 60000
    MOFI->InitMCObjectFileInfo(TheTriple, /* PIC */ false, Ctx);
#else
    MOFI->InitMCObjectFileInfo(TheTriple, /* PIC */ false,
                               CodeModel::Default, Ctx);
#endif

    // Set up Subtarget and Disassembler
    std::unique_ptr<MCSubtargetInfo>
        STI(TheTarget->createMCSubtargetInfo(TripleName, cpu, features));
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
#if JL_LLVM_VERSION >= 60000
        MCTargetOptions Options;
        MAB = TheTarget->createMCAsmBackend(*STI, *MRI, Options);
#elif JL_LLVM_VERSION >= 40000
        MCTargetOptions Options;
        MAB = TheTarget->createMCAsmBackend(*MRI, TripleName, cpu, Options);
#else
        MAB = TheTarget->createMCAsmBackend(*MRI, TripleName, cpu);
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
    if (!di_lineinfo.empty()) {
        auto cur_addr = di_lineinfo[0].first;
        auto nlineinfo = di_lineinfo.size();
        // filter out line infos that doesn't contain any instructions
        unsigned j = 0;
        for (unsigned i = 1; i < nlineinfo; i++) {
            auto &info = di_lineinfo[i];
            if (info.first != cur_addr)
                j++;
            cur_addr = info.first;
            if (i != j) {
                di_lineinfo[j] = std::move(info);
            }
        }
        if (j + 1 < nlineinfo) {
            di_lineinfo.resize(j + 1);
        }
    }

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
        DILineInfoPrinter dbgctx{';', true};
        if (pass != 0) {
            if (di_ctx && di_lineIter != di_lineEnd) {
                // Set up the line info
                nextLineAddr = di_lineIter->first;
                if (nextLineAddr != (uint64_t)(Fptr + slide)) {
                    std::string buf;
                    dbgctx.emit_lineinfo(buf, di_lineIter->second);
                    if (!buf.empty()) {
                        Streamer->EmitRawText(buf);
                    }
                }
            }
        }

        uint64_t Index = 0;
        uint64_t insSize = 0;

        // Do the disassembly
        for (Index = 0; Index < Fsize; Index += insSize) {

            if (pass != 0 && nextLineAddr != (uint64_t)-1 && Index + Fptr + slide == nextLineAddr) {
                if (di_ctx) {
                    std::string buf;
                    DILineInfoSpecifier infoSpec(DILineInfoSpecifier::FileLineInfoKind::Default,
                                                 DILineInfoSpecifier::FunctionNameKind::ShortName);
                    DIInliningInfo dbg = di_ctx->getInliningInfoForAddress(Index + Fptr + slide, infoSpec);
                    if (dbg.getNumberOfFrames()) {
                        dbgctx.emit_lineinfo(buf, dbg);
                    }
                    else {
                        dbgctx.emit_lineinfo(buf, di_lineIter->second);
                    }
                    if (!buf.empty())
                        Streamer->EmitRawText(buf);
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
                    // Pass 0: Record all branch target references
                    if (MCIA) {
                        const MCInstrDesc &opcode = MCII->get(Inst.getOpcode());
                        if (opcode.isBranch() || opcode.isCall()) {
                            uint64_t addr;
                            if (MCIA->evaluateBranch(Inst, Fptr + Index, insSize, addr))
                                DisInfo.insertAddress(addr);
                        }
                    }
                }
                else {
                    // Pass 1: Output instruction
                    if (pass != 0) {
                        // attempt to symbolicate any immediate operands
                        const MCInstrDesc &opinfo = MCII->get(Inst.getOpcode());
                        for (unsigned Op = 0; Op < opinfo.NumOperands; Op++) {
                            const MCOperand &OpI = Inst.getOperand(Op);
                            if (OpI.isImm()) {
                                int64_t imm = OpI.getImm();
                                if (opinfo.OpInfo[Op].OperandType == MCOI::OPERAND_PCREL)
                                    imm += Fptr + Index;
                                const char *name = DisInfo.lookupSymbolName(imm);
                                if (name)
                                    Streamer->AddComment(name);
                            }
                        }
                    }
                    Streamer->EmitInstruction(Inst, *STI);
                }
                break;
            }
        }

        DisInfo.setIP(Fptr);
        if (pass == 0)
            DisInfo.createSymbols();

        if (pass != 0 && di_ctx) {
            std::string buf;
            dbgctx.emit_finish(buf);
            if (!buf.empty()) {
                Streamer->EmitRawText(buf);
            }
        }
    }
}

extern "C" JL_DLLEXPORT
LLVMDisasmContextRef jl_LLVMCreateDisasm(
        const char *TripleName, void *DisInfo, int TagType,
        LLVMOpInfoCallback GetOpInfo, LLVMSymbolLookupCallback SymbolLookUp)
{
    return LLVMCreateDisasm(TripleName, DisInfo, TagType, GetOpInfo, SymbolLookUp);
}

extern "C" JL_DLLEXPORT
JL_DLLEXPORT size_t jl_LLVMDisasmInstruction(
        LLVMDisasmContextRef DC, uint8_t *Bytes, uint64_t BytesSize,
        uint64_t PC, char *OutString, size_t OutStringSize)
{
    return LLVMDisasmInstruction(DC, Bytes, BytesSize, PC, OutString, OutStringSize);
}
