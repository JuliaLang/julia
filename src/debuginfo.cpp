// This file is a part of Julia. License is MIT: http://julialang.org/license

#include "platform.h"

#include "llvm-version.h"
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JITEventListener.h>
#include <llvm/DebugInfo/DIContext.h>
#ifdef LLVM37
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#endif
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/IR/Function.h>
#include <llvm/ADT/StringRef.h>
#ifdef LLVM35
#include <llvm/IR/DebugInfo.h>
#else
#include <llvm/DebugInfo.h>
#endif
#ifdef USE_MCJIT
#ifndef LLVM36
#include <llvm/ExecutionEngine/ObjectImage.h>
#endif
#include <llvm/ExecutionEngine/RuntimeDyld.h>
#else
#include <llvm/ExecutionEngine/JITMemoryManager.h>
#endif
#ifdef _OS_DARWIN_
#include <llvm/Object/MachO.h>
#endif
#ifdef _OS_WINDOWS_
#include <llvm/Object/COFF.h>
#   ifdef LLVM37
#       include <llvm/Object/ELFObjectFile.h>
#   endif
#endif

#if defined(USE_MCJIT) && !defined(LLVM36) && defined(_OS_DARWIN_)
#include "../deps/llvm-3.5.0/lib/ExecutionEngine/MCJIT/MCJIT.h"
#endif

#include "julia.h"
#include "julia_internal.h"

#include <string>
#include <sstream>
#include <fstream>
#include <map>
#include <vector>
#include <set>
#include <cstdio>
#include <cassert>
using namespace llvm;

extern DLLEXPORT ExecutionEngine *jl_ExecutionEngine;

#ifdef USE_MCJIT
typedef object::SymbolRef SymRef;
#endif

// --- storing and accessing source location metadata ---

#ifndef USE_MCJIT
struct FuncInfo {
    const Function* func;
    size_t lengthAdr;
    std::string name;
    std::string filename;
    std::vector<JITEvent_EmittedFunctionDetails::LineStart> lines;
};
#else
struct ObjectInfo {
    const object::ObjectFile* object;
    size_t SectionSize;
#ifdef LLVM37
    const llvm::LoadedObjectInfo *L;
#elif defined(LLVM36)
    size_t slide;
#endif
#ifdef _OS_DARWIN_
    const char *name;
#endif
};
#endif

#if defined(_OS_WINDOWS_)
#if defined(_CPU_X86_64_)
extern "C" EXCEPTION_DISPOSITION _seh_exception_handler(PEXCEPTION_RECORD ExceptionRecord,void *EstablisherFrame, PCONTEXT ContextRecord, void *DispatcherContext);
#endif
#include <dbghelp.h>
static void create_PRUNTIME_FUNCTION(uint8_t *Code, size_t Size, StringRef fnname,
        uint8_t *Section, size_t Allocated, uint8_t *UnwindData)
{
    DWORD mod_size = 0;
#if defined(_CPU_X86_64_)
#if !defined(USE_MCJIT)
    uint8_t *catchjmp = Section+Allocated;
    UnwindData = (uint8_t*)(((uintptr_t)catchjmp+12+3)&~(uintptr_t)3);
    if (!catchjmp[0]) {
        catchjmp[0] = 0x48;
        catchjmp[1] = 0xb8; // mov RAX, QWORD PTR [...]
        *(uint64_t*)(&catchjmp[2]) = (uint64_t)&_seh_exception_handler;
        catchjmp[10] = 0xff;
        catchjmp[11] = 0xe0; // jmp RAX
        UnwindData[0] = 0x09; // version info, UNW_FLAG_EHANDLER
        UnwindData[1] = 4;    // size of prolog (bytes)
        UnwindData[2] = 2;    // count of unwind codes (slots)
        UnwindData[3] = 0x05; // frame register (rbp) = rsp
        UnwindData[4] = 4;    // second instruction
        UnwindData[5] = 0x03; // mov RBP, RSP
        UnwindData[6] = 1;    // first instruction
        UnwindData[7] = 0x50; // push RBP
        *(DWORD*)&UnwindData[8] = (DWORD)(catchjmp - Section); // relative location of catchjmp
        mod_size = (DWORD)Allocated+48;
    }
    PRUNTIME_FUNCTION tbl = (PRUNTIME_FUNCTION)(UnwindData+12);
#else
    PRUNTIME_FUNCTION tbl = (PRUNTIME_FUNCTION)malloc(sizeof(RUNTIME_FUNCTION));
#endif
    tbl->BeginAddress = (DWORD)(Code - Section);
    tbl->EndAddress = (DWORD)(Code - Section + Size);
    tbl->UnwindData = (DWORD)(UnwindData - Section);
#else // defined(_CPU_X86_64_)
    Section += (uintptr_t)Code;
    mod_size = Size;
#endif
    if (0) {
        assert(!jl_in_stackwalk);
        jl_in_stackwalk = 1;
        if (mod_size && !SymLoadModuleEx(GetCurrentProcess(), NULL, NULL, NULL, (DWORD64)Section, mod_size, NULL, SLMFLAG_VIRTUAL)) {
#if defined(_CPU_X86_64_) && !defined(USE_MCJIT)
            catchjmp[0] = 0;
#endif
            static int warned = 0;
            if (!warned) {
                jl_printf(JL_STDERR, "WARNING: failed to insert module info for backtrace: %lu\n", GetLastError());
                warned = 1;
            }
        }
        else {
            size_t len = fnname.size()+1;
            if (len > MAX_SYM_NAME)
                len = MAX_SYM_NAME;
            char *name = (char*)alloca(len);
            memcpy(name, fnname.data(), len-1);
            name[len-1] = 0;
            if (!SymAddSymbol(GetCurrentProcess(), (ULONG64)Section, name,
                        (DWORD64)Code, (DWORD)Size, 0)) {
                jl_printf(JL_STDERR, "WARNING: failed to insert function name %s into debug info: %lu\n", name, GetLastError());
            }
        }
        jl_in_stackwalk = 0;
    }
#if defined(_CPU_X86_64_)
    if (!RtlAddFunctionTable(tbl, 1, (DWORD64)Section)) {
        static int warned = 0;
        if (!warned) {
            jl_printf(JL_STDERR, "WARNING: failed to insert function stack unwind info: %lu\n", GetLastError());
            warned = 1;
        }
    }
#endif
}
#endif

struct revcomp {
    bool operator() (const size_t& lhs, const size_t& rhs) const
    { return lhs>rhs; }
};

class JuliaJITEventListener: public JITEventListener
{
#ifndef USE_MCJIT
    std::map<size_t, FuncInfo, revcomp> info;
#else
    std::map<size_t, ObjectInfo, revcomp> objectmap;
#endif

public:
    JuliaJITEventListener(){}
    virtual ~JuliaJITEventListener() {}

#ifndef USE_MCJIT
    virtual void NotifyFunctionEmitted(const Function &F, void *Code,
                                       size_t Size, const EmittedFunctionDetails &Details)
    {
#if defined(_OS_WINDOWS_)
        create_PRUNTIME_FUNCTION((uint8_t*)Code, Size, F.getName(), (uint8_t*)Code, Size, NULL);
#endif
        FuncInfo tmp = {&F, Size, F.getName().str(), std::string(), Details.LineStarts};
        info[(size_t)(Code)] = tmp;
    }

    std::map<size_t, FuncInfo, revcomp>& getMap()
    {
        return info;
    }
#endif // ifndef USE_MCJIT

#ifdef USE_MCJIT
#ifdef LLVM36
    virtual void NotifyObjectEmitted(const object::ObjectFile &obj,
                                     const RuntimeDyld::LoadedObjectInfo &L)
#else
    virtual void NotifyObjectEmitted(const ObjectImage &obj)
#endif
    {
        uint64_t Addr;
        uint64_t Size;
        object::SymbolRef::Type SymbolType;
#ifdef LLVM36
        object::section_iterator Section = obj.section_begin();
        object::section_iterator EndSection = obj.section_end();
        uint64_t SectionAddr = 0;
        StringRef sName;
#else
        object::section_iterator Section = obj.begin_sections();
        object::section_iterator EndSection = obj.end_sections();
        bool isText;
#ifndef _OS_LINUX_
        StringRef sName;
#endif
#endif

#ifndef LLVM36
        uint64_t SectionAddr = 0;
#endif
        uint64_t SectionSize = 0;
        uint64_t SectionAddrCheck = 0; // assert that all of the Sections are at the same location

#if defined(_OS_WINDOWS_)
#if defined(_CPU_X86_64_)
        uint8_t *UnwindData = NULL;
        uint8_t *catchjmp = NULL;
        for (const object::SymbolRef &sym_iter : obj.symbols()) {
#  ifdef LLVM37
            sName = sym_iter.getName().get();
#  else
            sym_iter.getName(sName);
#  endif
            if (sName.equals("__UnwindData")) {
#  ifdef LLVM37
                Addr = sym_iter.getAddress().get();
#  else
                sym_iter.getAddress(Addr);
#  endif
                sym_iter.getSection(Section);
#  ifdef LLVM36
                assert(Section->isText());
#    ifdef LLVM38
                SectionAddr = L.getSectionLoadAddress(*Section);
#    else
                Section->getName(sName);
                SectionAddr = L.getSectionLoadAddress(sName);
#    endif
                Addr += SectionAddr;
#  else
                if (Section->isText(isText) || !isText) assert(0 && "!isText");
                Section->getAddress(SectionAddr);
#  endif
                UnwindData = (uint8_t*)Addr;
                if (SectionAddrCheck)
                    assert(SectionAddrCheck == SectionAddr);
                else
                    SectionAddrCheck = SectionAddr;
            }
            if (sName.equals("__catchjmp")) {
#  ifdef LLVM37
                Addr = sym_iter.getAddress().get();
#  else
                sym_iter.getAddress(Addr);
#  endif
                sym_iter.getSection(Section);
#  ifdef LLVM36
                assert(Section->isText());
#    ifdef LLVM38
                SectionAddr = L.getSectionLoadAddress(*Section);
#    else
                Section->getName(sName);
                SectionAddr = L.getSectionLoadAddress(sName);
#    endif
                Addr += SectionAddr;
#  else
                if (Section->isText(isText) || !isText) assert(0 && "!isText");
                Section->getAddress(SectionAddr);
#  endif
                catchjmp = (uint8_t*)Addr;
                if (SectionAddrCheck)
                    assert(SectionAddrCheck == SectionAddr);
                else
                    SectionAddrCheck = SectionAddr;
            }
        }
        assert(catchjmp);
        assert(UnwindData);
        catchjmp[0] = 0x48;
        catchjmp[1] = 0xb8; // mov RAX, QWORD PTR [&_seh_exception_handle]
        *(uint64_t*)(&catchjmp[2]) = (uint64_t)&_seh_exception_handler;
        catchjmp[10] = 0xff;
        catchjmp[11] = 0xe0; // jmp RAX
        UnwindData[0] = 0x09; // version info, UNW_FLAG_EHANDLER
        UnwindData[1] = 4;    // size of prolog (bytes)
        UnwindData[2] = 2;    // count of unwind codes (slots)
        UnwindData[3] = 0x05; // frame register (rbp) = rsp
        UnwindData[4] = 4;    // second instruction
        UnwindData[5] = 0x03; // mov RBP, RSP
        UnwindData[6] = 1;    // first instruction
        UnwindData[7] = 0x50; // push RBP
        *(DWORD*)&UnwindData[8] = (DWORD)(catchjmp - (uint8_t*)SectionAddr); // relative location of catchjmp
#else // defined(_OS_X86_64_)
        uint8_t *UnwindData = NULL;
#endif // defined(_OS_X86_64_)
#endif // defined(_OS_WINDOWS_)

#ifdef LLVM35
        for (const object::SymbolRef &sym_iter : obj.symbols()) {
#           ifdef LLVM37
                SymbolType = sym_iter.getType();
#           else
                sym_iter.getType(SymbolType);
#           endif
            if (SymbolType != object::SymbolRef::ST_Function) continue;
#           ifdef LLVM37
                Addr = sym_iter.getAddress().get();
#           else
                sym_iter.getAddress(Addr);
#           endif
#           ifdef LLVM38
                Section = sym_iter.getSection().get();
#           else
                sym_iter.getSection(Section);
#           endif
            if (Section == EndSection) continue;
#if defined(LLVM36)
            if (!Section->isText()) continue;
#    ifdef LLVM38
            SectionAddr = L.getSectionLoadAddress(*Section);
#    else
            Section->getName(sName);
            SectionAddr = L.getSectionLoadAddress(sName);
#    endif
            Addr += SectionAddr;
#else
            if (Section->isText(isText) || !isText) continue;
#endif
#if defined(LLVM36)
            SectionSize = Section->getSize();
#else
            Section->getAddress(SectionAddr);
            Section->getSize(SectionSize);
#endif
#ifdef _OS_DARWIN_
#   if defined(LLVM37)
            Size = Section->getSize();
            sName = sym_iter.getName().get();
#   else
            sym_iter.getName(sName);
#   endif
#   if defined(LLVM36)
            if (sName[0] == '_') {
                sName = sName.substr(1);
            }
#   else
            Addr = ((MCJIT*)jl_ExecutionEngine)->getSymbolAddress(sName, true);
            if (!Addr && sName[0] == '_') {
                sName = sName.substr(1);
                Addr = ((MCJIT*)jl_ExecutionEngine)->getSymbolAddress(sName, true);
            }
            if (!Addr) continue;
#   endif
#elif defined(_OS_WINDOWS_)
#   if defined(LLVM37)
            assert(obj.isELF());
            Size = ((llvm::object::ELFSymbolRef)sym_iter).getSize();
            sName = sym_iter.getName().get();
#   else
            sym_iter.getSize(Size);
            sym_iter.getName(sName);
#   endif
#   ifdef _CPU_X86_
            if (sName[0] == '_') sName = sName.substr(1);
#   endif
            if (SectionAddrCheck)
                assert(SectionAddrCheck == SectionAddr);
            else
                SectionAddrCheck = SectionAddr;
            create_PRUNTIME_FUNCTION(
                   (uint8_t*)(intptr_t)Addr, (size_t)Size, sName,
                   (uint8_t*)(intptr_t)SectionAddr, (size_t)SectionSize, UnwindData);
#endif
            const object::ObjectFile *objfile =
#ifdef LLVM36
                &obj;
#else
                obj.getObjectFile();
#endif
            ObjectInfo tmp = {objfile, SectionSize
#ifdef LLVM37
                ,L.clone().release()
#elif defined(LLVM36)
                ,(size_t)SectionAddr
#endif
#ifdef _OS_DARWIN_
                ,strndup(sName.data(), sName.size())
#endif
            };
            objectmap[Addr] = tmp;
        }
#else //LLVM34
        error_code itererr;
        object::symbol_iterator sym_iter = obj.begin_symbols();
        object::symbol_iterator sym_end = obj.end_symbols();
        for (; sym_iter != sym_end; sym_iter.increment(itererr)) {
            sym_iter->getType(SymbolType);
            if (SymbolType != object::SymbolRef::ST_Function) continue;
            sym_iter->getAddress(Addr);
            sym_iter->getSize(Size);

            ObjectInfo tmp = {obj.getObjectFile(), (size_t)Size};
            objectmap[Addr] = tmp;
        }
#endif
    }

    // must implement if we ever start freeing code
    // virtual void NotifyFreeingObject(const ObjectImage &obj) {}
    // virtual void NotifyFreeingObject(const object::ObjectFile &Obj) {}

    std::map<size_t, ObjectInfo, revcomp>& getObjectMap()
    {
        return objectmap;
    }
#endif // USE_MCJIT
};

extern "C"
char *jl_demangle(const char *name)
{
    const char *start = name + 6;
    const char *end = name + strlen(name);
    char *ret;
    if (strncmp(name, "julia_", 6)) goto done;
    if (*start == '\0') goto done;
    while (*(--end) != '_') {
        char c = *end;
        if (c < '0' || c > '9') goto done;
    }
    if (end <= start) goto done;
    ret = (char*)malloc(end-start+1);
    memcpy(ret,start,end-start);
    ret[end-start] = '\0';
    return ret;
 done:
    return strdup(name);
}

JuliaJITEventListener *jl_jit_events;
void RegisterJuliaJITEventListener()
{
    jl_jit_events = new JuliaJITEventListener();
    jl_ExecutionEngine->RegisterJITEventListener(jl_jit_events);
}

// *name and *filename are either NULL or malloc'd pointers
void lookup_pointer(DIContext *context, char **name, size_t *line,
                    char **filename, size_t *inlinedat_line,
                    char **inlinedat_file, size_t pointer,
                    int demangle, int *fromC)
{
    DILineInfo info, topinfo;
    DIInliningInfo inlineinfo;
    if (demangle && *name != NULL) {
        char *oldname = *name;
        *name = jl_demangle(*name);
        free(oldname);
    }
#ifdef LLVM35
    DILineInfoSpecifier infoSpec(DILineInfoSpecifier::FileLineInfoKind::AbsoluteFilePath,
                                 DILineInfoSpecifier::FunctionNameKind::ShortName);
    DILineInfoSpecifier inlineSpec(DILineInfoSpecifier::FileLineInfoKind::AbsoluteFilePath,
                                 DILineInfoSpecifier::FunctionNameKind::ShortName);
#else
    int infoSpec = DILineInfoSpecifier::FileLineInfo |
                   DILineInfoSpecifier::AbsoluteFilePath |
                   DILineInfoSpecifier::FunctionName;
    int inlineSpec = DILineInfoSpecifier::FileLineInfo |
                   DILineInfoSpecifier::AbsoluteFilePath |
                   DILineInfoSpecifier::FunctionName;
#endif

    if (context == NULL) goto done;
    info = context->getLineInfoForAddress(pointer, infoSpec);
    inlineinfo = context->getInliningInfoForAddress(pointer, inlineSpec);

#ifndef LLVM35 // LLVM <= 3.4
    if (strcmp(info.getFunctionName(), "<invalid>") == 0) goto done;
    if (demangle) {
        free(*name);
        *name = jl_demangle(info.getFunctionName());
    }
    else {
        jl_copy_str(name, info.getFunctionName());
    }
    *line = info.getLine();
    jl_copy_str(filename, info.getFileName());

    if (inlineinfo.getNumberOfFrames() > 1) {
        topinfo = inlineinfo.getFrame(inlineinfo.getNumberOfFrames() - 1);
        jl_copy_str(inlinedat_file, topinfo.getFileName());
        *inlinedat_line = topinfo.getLine();
    }
#else
    if (strcmp(info.FunctionName.c_str(), "<invalid>") == 0) goto done;
    jl_copy_str(name, info.FunctionName.c_str());
    *line = info.Line;
    jl_copy_str(filename, info.FileName.c_str());

    if (inlineinfo.getNumberOfFrames() > 1) {
        topinfo = inlineinfo.getFrame(inlineinfo.getNumberOfFrames() - 1);
        jl_copy_str(inlinedat_file, topinfo.FileName.c_str());
        *inlinedat_line = topinfo.getLine();
    }
#endif

done:
    // If this is a jlcall wrapper, set fromC to match JIT behavior
    if (*name == NULL || memcmp(*name, "jlcall_",7) == 0) {
        *fromC = true;
    }
}

#ifdef _OS_DARWIN_
#include <mach-o/dyld.h>
#endif
#ifndef _OS_WINDOWS_
#include <dlfcn.h>
#endif
typedef struct {
    llvm::object::ObjectFile *obj;
    DIContext *ctx;
    int64_t slide;
} objfileentry_t;
typedef std::map<uint64_t, objfileentry_t> obfiletype;
static obfiletype objfilemap;

#ifdef _OS_DARWIN_
bool getObjUUID(llvm::object::MachOObjectFile *obj, uint8_t uuid[16])
{

# ifdef LLVM37
    for (auto Load : obj->load_commands ()) {
# else
#  ifdef LLVM35
    uint32_t LoadCommandCount = obj->getHeader().ncmds;
#  else
    uint32_t LoadCommandCount = obj->getHeader().NumLoadCommands;
#  endif
    llvm::object::MachOObjectFile::LoadCommandInfo Load = obj->getFirstLoadCommandInfo();
    for (unsigned I = 0; ; ++I) {
# endif
        if (
# ifdef LLVM35
            Load.C.cmd == LC_UUID
# else
            Load.C.Type == LC_UUID
# endif
            ) {
            memcpy(uuid,((MachO::uuid_command*)Load.Ptr)->uuid,16);
            return true;
        }
# ifndef LLVM37
        else if (I == LoadCommandCount - 1) {
            return false;
        }
        else {
            Load = obj->getNextLoadCommandInfo(Load);
        }
# endif
    }
    return false;
}
#endif

extern "C" uint64_t jl_sysimage_base;

// *name and *filename should be either NULL or malloc'd pointer
void jl_getDylibFunctionInfo(char **name, char **filename, size_t *line,
                             char** inlinedat_file, size_t *inlinedat_line,
                             size_t pointer, int *fromC, int skipC, int skipInline)
{
#ifdef _OS_WINDOWS_
    IMAGEHLP_MODULE64 ModuleInfo;
    BOOL isvalid;
    if (jl_in_stackwalk) {
        *fromC = 1;
        return;
    }
    ModuleInfo.SizeOfStruct = sizeof(IMAGEHLP_MODULE64);
    jl_in_stackwalk = 1;
    isvalid = SymGetModuleInfo64(GetCurrentProcess(), (DWORD64)pointer, &ModuleInfo);
    jl_in_stackwalk = 0;
    if (isvalid) {
        char *fname = ModuleInfo.LoadedImageName;
        DWORD64 fbase = ModuleInfo.BaseOfImage;
        *fromC = (fbase != jl_sysimage_base);
        if (skipC && *fromC) {
            return;
        }
        static char frame_info_func[
            sizeof(SYMBOL_INFO) +
            MAX_SYM_NAME * sizeof(TCHAR)];
        static IMAGEHLP_LINE64 frame_info_line;
        DWORD dwDisplacement = 0;
        DWORD64 dwDisplacement64 = 0;
        DWORD64 dwAddress = pointer;
        PSYMBOL_INFO pSymbol = (PSYMBOL_INFO)frame_info_func;
        pSymbol->SizeOfStruct = sizeof(SYMBOL_INFO);
        pSymbol->MaxNameLen = MAX_SYM_NAME;
        jl_in_stackwalk = 1;
        if (SymFromAddr(GetCurrentProcess(), dwAddress, &dwDisplacement64,
                        pSymbol)) {
            // SymFromAddr returned success
            jl_copy_str(name, pSymbol->Name);
        }
        else {
            // SymFromAddr failed
            //jl_printf(JL_STDERR,"SymFromAddr returned error : %lu\n", GetLastError());
        }

        frame_info_line.SizeOfStruct = sizeof(IMAGEHLP_LINE64);
        if (SymGetLineFromAddr64(GetCurrentProcess(), dwAddress, &dwDisplacement, &frame_info_line)) {
            // SymGetLineFromAddr64 returned success
            // record source file name and line number
            if (frame_info_line.FileName)
                jl_copy_str(filename, frame_info_line.FileName);
            *line = frame_info_line.LineNumber;
        }
        else if (*fromC) {
            // No debug info, use dll name instead
            jl_copy_str(filename, fname);
        }
        jl_in_stackwalk = 0;
#else // ifdef _OS_WINDOWS_
    Dl_info dlinfo;
    if ((dladdr((void*)pointer, &dlinfo) != 0) && dlinfo.dli_fname) {
        const char *fname;
        uint64_t fbase = (uint64_t)dlinfo.dli_fbase;
#if defined(_OS_DARWIN_)
        size_t msize = (size_t)(((uint64_t)-1)-fbase);
#endif
        *fromC = (fbase != jl_sysimage_base);
        if (skipC && *fromC)
            return;
        // In case we fail with the debug info lookup, we at least still
        // have the function name, even if we don't have line numbers
        jl_copy_str(name, dlinfo.dli_sname);
        jl_copy_str(filename, dlinfo.dli_fname);
        fname = dlinfo.dli_fname;
#endif // ifdef _OS_WINDOWS_
        DIContext *context = NULL;
        int64_t slide = 0;
#if !defined(_OS_WINDOWS_) || defined(LLVM35)
        obfiletype::iterator it = objfilemap.find(fbase);
        llvm::object::ObjectFile *obj = NULL;
        if (it == objfilemap.end()) {
#if defined(_OS_DARWIN_)
#ifdef LLVM36
           std::unique_ptr<MemoryBuffer> membuf = MemoryBuffer::getMemBuffer(
                    StringRef((const char *)fbase, msize), "", false);
           auto origerrorobj = llvm::object::ObjectFile::createObjectFile(
                membuf->getMemBufferRef(), sys::fs::file_magic::unknown);
#elif defined(LLVM35)
            MemoryBuffer *membuf = MemoryBuffer::getMemBuffer(
                StringRef((const char *)fbase, msize), "", false);
            std::unique_ptr<MemoryBuffer> buf(membuf);
            auto origerrorobj = llvm::object::ObjectFile::createObjectFile(
                buf, sys::fs::file_magic::unknown);
#else
            MemoryBuffer *membuf = MemoryBuffer::getMemBuffer(
                StringRef((const char *)fbase, msize), "", false);
            llvm::object::ObjectFile *origerrorobj = llvm::object::ObjectFile::createObjectFile(
                membuf);
#endif
            if (!origerrorobj) {
                objfileentry_t entry = {obj,context,slide};
                objfilemap[fbase] = entry;
                goto lookup;
            }
#ifdef LLVM36
            llvm::object::MachOObjectFile *morigobj = (llvm::object::MachOObjectFile *)origerrorobj.get().release();
#elif LLVM35
            llvm::object::MachOObjectFile *morigobj = (llvm::object::MachOObjectFile *)origerrorobj.get();
#else
            llvm::object::MachOObjectFile *morigobj = (llvm::object::MachOObjectFile *)origerrorobj;
#endif
            // First find the uuid of the object file (we'll use this to make sure we find the
            // correct debug symbol file).
            uint8_t uuid[16], uuid2[16];
            if (!getObjUUID(morigobj,uuid)) {
                objfileentry_t entry = {obj,context,slide};
                objfilemap[fbase] = entry;
                goto lookup;
            }

            // On OS X debug symbols are not contained in the dynamic library and that's why
            // we can't have nice things (easily). For now we only support .dSYM files in the same directory
            // as the shared library. In the future we may use DBGCopyFullDSYMURLForUUID from CoreFoundation to make
            // use of spotlight to find the .dSYM file.
            char dsympath[PATH_MAX];
            strlcpy(dsympath, fname, sizeof(dsympath));
            strlcat(dsympath, ".dSYM/Contents/Resources/DWARF/", sizeof(dsympath));
            strlcat(dsympath, strrchr(fname,'/')+1, sizeof(dsympath));
#ifdef LLVM35
            auto errorobj = llvm::object::ObjectFile::createObjectFile(dsympath);
#else
            llvm::object::ObjectFile *errorobj = llvm::object::ObjectFile::createObjectFile(dsympath);
#endif
#else // ifdef _OS_DARWIN_
            // On Linux systems we need to mmap another copy because of the permissions on the mmap'ed shared library.
#ifdef LLVM35
            auto errorobj = llvm::object::ObjectFile::createObjectFile(fname);
#else
            llvm::object::ObjectFile *errorobj = llvm::object::ObjectFile::createObjectFile(fname);
#endif
#endif // ifdef _OS_DARWIN_
            if (errorobj) {
#if LLVM36
                auto binary = errorobj.get().takeBinary();
                obj = binary.first.release();
                binary.second.release();
#elif LLVM35
                obj = errorobj.get();
#else
                obj = errorobj;
#endif
#ifdef _OS_DARWIN_
                if (getObjUUID(morigobj,uuid2) && memcmp(uuid,uuid2,sizeof(uuid)) == 0) {
#endif
#ifdef LLVM37
                    context = new DWARFContextInMemory(*obj);
#elif LLVM36
                    context = DIContext::getDWARFContext(*obj);
#else
                    context = DIContext::getDWARFContext(obj);
#endif
                    slide = -(uint64_t)fbase;
#ifdef _OS_DARWIN_
                }
#endif
#ifdef _OS_WINDOWS_
#ifdef LLVM35
                assert(obj->isCOFF());
                llvm::object::COFFObjectFile *coffobj = (llvm::object::COFFObjectFile *)obj;
                const llvm::object::pe32plus_header *pe32plus;
                coffobj->getPE32PlusHeader(pe32plus);
                if (pe32plus != NULL) {
                    slide = pe32plus->ImageBase - fbase;
                }
                else {
                    const llvm::object::pe32_header *pe32;
                    coffobj->getPE32Header(pe32);
                    if (pe32 == NULL) {
                        obj = NULL;
                        context = NULL;
                    }
                    else {
                        slide = pe32->ImageBase - fbase;
                    }
                }
#endif
#endif

            }
            objfileentry_t entry = {obj,context,slide};
            objfilemap[fbase] = entry;
        }
        else {
            obj = it->second.obj;
            context = it->second.ctx;
            slide = it->second.slide;
        }
#endif // ifdef _OS_WINDOWS && !LLVM35
#ifdef _OS_DARWIN_
lookup:
#endif
        lookup_pointer(context, name, line, filename, inlinedat_line, inlinedat_file, pointer+slide,
                       fbase == jl_sysimage_base, fromC);
    }
    else {
        *fromC = 1;
    }
}

// Set *name and *filename to either NULL or malloc'd string
void jl_getFunctionInfo(char **name, char **filename, size_t *line,
                        char **inlinedat_file, size_t *inlinedat_line,
                        size_t pointer, int *fromC, int skipC, int skipInline)
{
    *name = NULL;
    *line = -1;
    *filename = NULL;
    *inlinedat_file = NULL;
    *inlinedat_line = -1;
    *fromC = 0;

#ifdef USE_MCJIT
    // With MCJIT we can get function information directly from the ObjectFile
    std::map<size_t, ObjectInfo, revcomp> &objmap =
        jl_jit_events->getObjectMap();
    std::map<size_t, ObjectInfo, revcomp>::iterator it =
        objmap.lower_bound(pointer);

    if (it != objmap.end() &&
        (intptr_t)(*it).first + (*it).second.SectionSize > pointer) {
#if defined(_OS_DARWIN_) && !defined(LLVM37)
        // *name should always be NULL here, free it anyway just to be more
        // robust
        free(*name);
        *name = jl_demangle((*it).second.name);
        DIContext *context = NULL; // versions of MCJIT < 3.7 can't handle MachO relocations
#else
#ifdef LLVM36
#ifdef LLVM37
        DIContext *context = new DWARFContextInMemory(*it->second.object, it->second.L);
#else
        DIContext *context = DIContext::getDWARFContext(*it->second.object);
        pointer -= (*it).second.slide;
#endif
#else
        DIContext *context = DIContext::getDWARFContext(const_cast<object::ObjectFile*>(it->second.object));
#endif
#endif
        lookup_pointer(context, name, line, filename, inlinedat_line, inlinedat_file, pointer, 1, fromC);
        delete context;
        return;
    }

#else // !USE_MCJIT
// Without MCJIT we use the FuncInfo structure containing address maps
    std::map<size_t, FuncInfo, revcomp> &info = jl_jit_events->getMap();
    std::map<size_t, FuncInfo, revcomp>::iterator it = info.lower_bound(pointer);
    if (it != info.end() && (intptr_t)(*it).first + (*it).second.lengthAdr >= pointer) {
        // We do this to hide the jlcall wrappers when getting julia backtraces,
        // but it is still good to have them for regular lookup of C frames.
        if (skipC && (*it).second.lines.empty()) {
            // Technically not true, but we don't want them
            // in julia backtraces, so close enough
            *fromC = 1;
            return;
        }

        jl_copy_str(name, (*it).second.name.c_str());
        jl_copy_str(filename, (*it).second.filename.c_str());

        if ((*it).second.lines.empty()) {
            *fromC = 1;
            return;
        }

        std::vector<JITEvent_EmittedFunctionDetails::LineStart>::iterator vit =
            (*it).second.lines.begin();
        JITEvent_EmittedFunctionDetails::LineStart prev = *vit;

        if ((*it).second.func) {
            DISubprogram debugscope =
                DISubprogram(prev.Loc.getScope((*it).second.func->getContext()));
            jl_copy_str(filename, debugscope.getFilename().str().c_str());
            // the DISubprogram has the un-mangled name, so use that if
            // available. However, if the scope need not be the current
            // subprogram.
            if (debugscope.getName().data() != NULL) {
                jl_copy_str(name, debugscope.getName().str().c_str());
            } else {
                char *oldname = *name;
                *name = jl_demangle(*name);
                free(oldname);
            }
        }

        vit++;

        while (vit != (*it).second.lines.end()) {
            if (pointer <= (*vit).Address) {
                *line = prev.Loc.getLine();
                break;
            }
            prev = *vit;
            vit++;
        }
        if (*line == (size_t) -1) {
            *line = prev.Loc.getLine();
        }

        DILexicalBlockFile locscope = DILexicalBlockFile(prev.Loc.getScope((*it).second.func->getContext()));
        jl_copy_str(filename, locscope.getFilename().str().c_str());

        MDNode *inlinedAt = skipInline ? NULL : prev.Loc.getInlinedAt((*it).second.func->getContext());
        if ((!skipInline) && (inlinedAt != NULL)) {
            DebugLoc inlineloc = DebugLoc::getFromDILocation(inlinedAt);
            DILexicalBlockFile inlinescope = DILexicalBlockFile(inlineloc.getScope((*it).second.func->getContext()));
            jl_copy_str(inlinedat_file, inlinescope.getFilename().str().c_str());
            *inlinedat_line = inlineloc.getLine();
        }

        return;
    }
#endif // USE_MCJIT
    jl_getDylibFunctionInfo(name, filename, line, inlinedat_file, inlinedat_line, pointer, fromC, skipC, skipInline);
}

int jl_get_llvmf_info(uint64_t fptr, uint64_t *symsize, uint64_t *slide,
#ifdef USE_MCJIT
    const object::ObjectFile **object
#else
    std::vector<JITEvent_EmittedFunctionDetails::LineStart> *lines
#endif
    )
{
#ifndef USE_MCJIT
    std::map<size_t, FuncInfo, revcomp> &fmap = jl_jit_events->getMap();
    std::map<size_t, FuncInfo, revcomp>::iterator fit = fmap.find(fptr);

    if (fit != fmap.end()) {
        *symsize = fit->second.lengthAdr;
        *lines = fit->second.lines;
        *slide = 0;
        return 1;
    }
    return 0;
#else // MCJIT version
    std::map<size_t, ObjectInfo, revcomp> &objmap = jl_jit_events->getObjectMap();
    std::map<size_t, ObjectInfo, revcomp>::iterator fit = objmap.find(fptr);

    if (fit != objmap.end()) {
        *symsize = fit->second.SectionSize;
        *object = fit->second.object;
#if defined(LLVM36) && !defined(LLVM37)
        *slide = fit->second.slide;
#else
        *slide = 0;
#endif
        return 1;
    }
    return 0;
#endif
}


#if defined(_OS_DARWIN_) && defined(LLVM37) && defined(LLVM_SHLIB)

/*
 * We use a custom unwinder, so we need to make sure that when registering dynamic
 * frames, we do so with our unwinder rather than with the system one. If LLVM is
 * statically linked everything works out fine, but if it's dynamically linked
 * it would usually pick up the system one, so we need to do the registration
 * ourselves to ensure the right one gets picked.
 */

#include "llvm/ExecutionEngine/SectionMemoryManager.h"
class RTDyldMemoryManagerOSX : public SectionMemoryManager
{
  RTDyldMemoryManagerOSX(const RTDyldMemoryManagerOSX&) = delete;
  void operator=(const RTDyldMemoryManagerOSX&) = delete;

public:
    RTDyldMemoryManagerOSX() {};
    ~RTDyldMemoryManagerOSX() override {};
    void registerEHFrames(uint8_t *Addr, uint64_t LoadAddr, size_t Size) override;
    void deregisterEHFrames(uint8_t *Addr, uint64_t LoadAddr, size_t Size);
};

extern "C" void __register_frame(void*);
extern "C" void __deregister_frame(void*);

static void (*libc_register_frame)(void*)   = NULL;
static void (*libc_deregister_frame)(void*) = NULL;

static const char *processFDE(const char *Entry, bool isDeregister)
{
    const char *P = Entry;
    uint32_t Length = *((const uint32_t *)P);
    P += 4;
    uint32_t Offset = *((const uint32_t *)P);
    if (Offset != 0) {
        if (isDeregister) {
            if (!libc_deregister_frame) {
                libc_deregister_frame = (void(*)(void*))dlsym(RTLD_NEXT,"__deregister_frame");
            }
            assert(libc_deregister_frame);
            libc_deregister_frame(const_cast<char *>(Entry));
            __deregister_frame(const_cast<char *>(Entry));
        }
        else {
            if (!libc_register_frame) {
                libc_register_frame = (void(*)(void*))dlsym(RTLD_NEXT,"__register_frame");
            }
            assert(libc_register_frame);
            libc_register_frame(const_cast<char *>(Entry));
            __register_frame(const_cast<char *>(Entry));
        }
    }
    return P + Length;
}

// This implementation handles frame registration for local targets.
// Memory managers for remote targets should re-implement this function
// and use the LoadAddr parameter.
void RTDyldMemoryManagerOSX::registerEHFrames(uint8_t *Addr, uint64_t LoadAddr, size_t Size)
{
    // On OS X OS X __register_frame takes a single FDE as an argument.
    // See http://lists.cs.uiuc.edu/pipermail/llvmdev/2013-April/061768.html
    const char *P = (const char *)Addr;
    const char *End = P + Size;
    do  {
        P = processFDE(P, false);
    } while(P != End);
}

void RTDyldMemoryManagerOSX::deregisterEHFrames(uint8_t *Addr, uint64_t LoadAddr, size_t Size)
{
    const char *P = (const char *)Addr;
    const char *End = P + Size;
    do  {
        P = processFDE(P, true);
    } while(P != End);
}

RTDyldMemoryManager* createRTDyldMemoryManagerOSX()
{
    return new RTDyldMemoryManagerOSX();
}

#endif

#if defined(_OS_WINDOWS_)
#ifdef USE_MCJIT
extern "C"
DWORD64 jl_getUnwindInfo(ULONG64 dwAddr)
{
    std::map<size_t, ObjectInfo, revcomp> &objmap = jl_jit_events->getObjectMap();
    std::map<size_t, ObjectInfo, revcomp>::iterator it = objmap.lower_bound(dwAddr);
    if (it != objmap.end() && (intptr_t)(*it).first + (*it).second.SectionSize > dwAddr) {
        return (DWORD64)(intptr_t)(*it).first;
    }
    return 0;
}

#else //ifdef USE_MCJIT
#if defined(_CPU_X86_64_)
// Custom memory manager for exception handling on Windows
// we overallocate 48 bytes at the end of each function
// for unwind information (see NotifyFunctionEmitted)
class JITMemoryManagerWin : public JITMemoryManager {
private:
    JITMemoryManager *JMM;
public:
    JITMemoryManagerWin() : JITMemoryManager()
    {
        JMM = JITMemoryManager::CreateDefaultMemManager();
    }
    virtual void setMemoryWritable() { return JMM->setMemoryWritable(); }
    virtual void setMemoryExecutable() { return JMM->setMemoryExecutable(); }
    virtual void setPoisonMemory(bool poison) { return JMM->setPoisonMemory(poison); }
    virtual void AllocateGOT() { JMM->AllocateGOT(); HasGOT = true; }
    virtual uint8_t *getGOTBase() const { return JMM->getGOTBase(); }
    virtual uint8_t *startFunctionBody(const Function *F, uintptr_t &ActualSize)
    {
        if (ActualSize == 0)
            ActualSize += 64;
        ActualSize += 48;
        uint8_t *mem = JMM->startFunctionBody(F,ActualSize);
        ActualSize -= 48;
        return mem;
    }
    virtual uint8_t *allocateStub(const GlobalValue* F, unsigned StubSize, unsigned Alignment)
    {
        return JMM->allocateStub(F,StubSize,Alignment);
    }
    virtual void endFunctionBody(const Function *F, uint8_t *FunctionStart, uint8_t *FunctionEnd)
    {
        FunctionEnd[0] = 0;
        JMM->endFunctionBody(F,FunctionStart,FunctionEnd+48);
    }
    virtual uint8_t *allocateSpace(intptr_t Size, unsigned Alignment) { return JMM->allocateSpace(Size,Alignment); }
    virtual uint8_t *allocateGlobal(uintptr_t Size, unsigned Alignment) { return JMM->allocateGlobal(Size,Alignment); }
    virtual void deallocateFunctionBody(void *Body) { return JMM->deallocateFunctionBody(Body); }
    virtual uint8_t *startExceptionTable(const Function* F,
                                         uintptr_t &ActualSize) { return JMM->startExceptionTable(F,ActualSize); }
    virtual void endExceptionTable(const Function *F, uint8_t *TableStart,
                                   uint8_t *TableEnd, uint8_t *FrameRegister) { return JMM->endExceptionTable(F,TableStart,TableEnd,FrameRegister); }
    virtual void deallocateExceptionTable(void *ET) { return JMM->deallocateExceptionTable(ET); }
    virtual bool CheckInvariants(std::string &str) { return JMM->CheckInvariants(str); }
    virtual size_t GetDefaultCodeSlabSize() { return JMM->GetDefaultCodeSlabSize(); }
    virtual size_t GetDefaultDataSlabSize() { return JMM->GetDefaultDataSlabSize(); }
    virtual size_t GetDefaultStubSlabSize() { return JMM->GetDefaultStubSlabSize(); }
    virtual unsigned GetNumCodeSlabs() { return JMM->GetNumCodeSlabs(); }
    virtual unsigned GetNumDataSlabs() { return JMM->GetNumDataSlabs(); }
    virtual unsigned GetNumStubSlabs() { return JMM->GetNumStubSlabs(); }

#ifdef LLVM35
    virtual uint8_t *allocateCodeSection(uintptr_t Size, unsigned Alignment,
                                         unsigned SectionID, llvm::StringRef SectionName)
    {
        uint8_t *mem = JMM->allocateCodeSection(Size+48, Alignment, SectionID, SectionName);
        mem[Size] = 0;
        return mem;
    }
    virtual uint8_t *allocateDataSection(uintptr_t Size, unsigned Alignment,
                                         unsigned SectionID, llvm::StringRef SectionName, bool IsReadOnly)
    {
        return JMM->allocateDataSection(Size,Alignment,SectionID,SectionName,IsReadOnly);
    }
#else
    virtual uint8_t *allocateCodeSection(uintptr_t Size, unsigned Alignment, unsigned SectionID)
    {
        uint8_t *mem = JMM->allocateCodeSection(Size+48, Alignment, SectionID);
        mem[Size] = 0;
        return mem;
    }
    virtual uint8_t *allocateDataSection(uintptr_t Size, unsigned Alignment,
                                         unsigned SectionID, bool IsReadOnly)
    {
        return JMM->allocateDataSection(Size,Alignment,SectionID,IsReadOnly);
    }
#endif
    virtual void *getPointerToNamedFunction(const std::string &Name, bool AbortOnFailure = true)
    {
        return JMM->getPointerToNamedFunction(Name,AbortOnFailure);
    }
    virtual bool applyPermissions(std::string *ErrMsg = 0) { return JMM->applyPermissions(ErrMsg); }
    virtual void registerEHFrames(StringRef SectionData) { return JMM->registerEHFrames(SectionData); }
};
JITMemoryManager* createJITMemoryManagerWin()
{
    return new JITMemoryManagerWin();
}
#else
extern "C"
DWORD64 jl_getUnwindInfo(ULONG64 dwAddr)
{
    std::map<size_t, FuncInfo, revcomp> &info = jl_jit_events->getMap();
    std::map<size_t, FuncInfo, revcomp>::iterator it = info.lower_bound(dwAddr);
    if (it != info.end() && (intptr_t)(*it).first + (*it).second.lengthAdr > dwAddr) {
        return (DWORD64)(intptr_t)(*it).first;
    }
    return 0;
}
#endif
#endif
#endif
