// This file is a part of Julia. License is MIT: http://julialang.org/license

#include "platform.h"

#include "llvm-version.h"
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JITEventListener.h>
#include <llvm/DebugInfo/DIContext.h>
#ifdef LLVM37
#include <llvm/DebugInfo/DWARF/DWARFContext.h>
#include <llvm/Object/SymbolSize.h>
#endif
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/IR/Function.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/StringMap.h>
#ifdef LLVM35
#include <llvm/IR/DebugInfo.h>
#else
#include <llvm/DebugInfo.h>
#endif
#if defined(USE_MCJIT) || defined(USE_ORCJIT)
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Mangler.h>
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

using namespace llvm;

#include "julia.h"
#include "julia_internal.h"
#include "codegen_internal.h"
#ifdef _OS_LINUX_
#  define UNW_LOCAL_ONLY
#  include <libunwind.h>
#  include <link.h>
#endif

#include <string>
#include <sstream>
#include <fstream>
#include <map>
#include <vector>
#include <set>
#include <cstdio>
#include <cassert>

#if defined(LLVM35) && !defined(LLVM36)
extern ExecutionEngine *jl_ExecutionEngine;
#endif

#ifdef USE_MCJIT
typedef object::SymbolRef SymRef;
#endif

// Any function that acquires this lock must be either a unmanaged thread
// or in the GC safe region and must NOT allocate anything through the GC
// while holding this lock.
// Certain functions in this file might be called from an unmanaged thread
// and cannot have any interaction with the julia runtime
static uv_rwlock_t threadsafe;

extern "C" void jl_init_debuginfo()
{
    uv_rwlock_init(&threadsafe);
}

// --- storing and accessing source location metadata ---

#ifndef USE_MCJIT
struct FuncInfo {
    const Function *func;
    size_t lengthAdr;
    std::vector<JITEvent_EmittedFunctionDetails::LineStart> lines;
    jl_lambda_info_t *linfo;
};
#else
struct ObjectInfo {
    const object::ObjectFile *object;
    size_t SectionSize;
    ptrdiff_t slide;
#ifdef LLVM37
    DIContext *context;
#endif
#if defined(_OS_DARWIN_) && !defined(LLVM37)
    const char *name;
#endif
};
#endif

// Maintain a mapping of unrealized function names -> linfo objects
// so that when we see it get emitted, we can add a link back to the linfo
// that it came from (providing name, type signature, file info, etc.)
static StringMap<jl_lambda_info_t*> linfo_in_flight;
static std::string mangle(const std::string &Name, const DataLayout &DL) {
#if defined(USE_MCJIT) || defined(USE_ORCJIT)
    std::string MangledName;
    {
        raw_string_ostream MangledNameStream(MangledName);
        Mangler::getNameWithPrefix(MangledNameStream, Name, DL);
    }
    return MangledName;
#else
    return Name;
#endif
}
void jl_add_linfo_in_flight(StringRef name, jl_lambda_info_t *linfo, const DataLayout &DL)
{
    linfo_in_flight[mangle(name, DL)] = linfo;
}

#if defined(_OS_WINDOWS_)
#if defined(_CPU_X86_64_)
extern "C" EXCEPTION_DISPOSITION _seh_exception_handler(PEXCEPTION_RECORD ExceptionRecord,void *EstablisherFrame, PCONTEXT ContextRecord, void *DispatcherContext);
#endif
static void create_PRUNTIME_FUNCTION(uint8_t *Code, size_t Size, StringRef fnname,
        uint8_t *Section, size_t Allocated, uint8_t *UnwindData)
{
    // GC safe
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

#ifdef LLVM38
struct strrefcomp {
    bool operator() (const StringRef& lhs, const StringRef& rhs) const
    {
        return lhs.compare(rhs) > 0;
    }
};
#endif

extern "C" tracer_cb jl_linfo_tracer;
static std::vector<jl_lambda_info_t*> triggered_linfos;
void jl_callback_triggered_linfos(void)
{
    if (triggered_linfos.empty())
        return;
    if (jl_linfo_tracer) {
        std::vector<jl_lambda_info_t*> to_process(std::move(triggered_linfos));
        for (jl_lambda_info_t *linfo : to_process)
            jl_call_tracer(jl_linfo_tracer, (jl_value_t*)linfo);
    }
}

class JuliaJITEventListener: public JITEventListener
{
#ifndef USE_MCJIT
    std::map<size_t, FuncInfo, revcomp> info;
#else
    std::map<size_t, ObjectInfo, revcomp> objectmap;
    std::map<size_t, std::pair<size_t, jl_lambda_info_t *>, revcomp> linfomap;
#endif

public:
    JuliaJITEventListener(){}
    virtual ~JuliaJITEventListener() {}

#ifndef USE_MCJIT
    virtual void NotifyFunctionEmitted(const Function &F, void *Code,
                                       size_t Size, const EmittedFunctionDetails &Details)
    {
        // This function modify linfo->fptr in GC safe region.
        // This should be fine since the GC won't scan this field.
        int8_t gc_state = jl_gc_safe_enter();
        uv_rwlock_wrlock(&threadsafe);
        StringRef sName = F.getName();
        StringMap<jl_lambda_info_t*>::iterator linfo_it = linfo_in_flight.find(sName);
        jl_lambda_info_t *linfo = NULL;
        if (linfo_it != linfo_in_flight.end()) {
            linfo = linfo_it->second;
            linfo_in_flight.erase(linfo_it);
            if (((Function*)linfo->functionObjectsDecls.functionObject)->getName().equals(sName))
                linfo->fptr = (jl_fptr_t)(uintptr_t)Code;
        }
#if defined(_OS_WINDOWS_)
        create_PRUNTIME_FUNCTION((uint8_t*)Code, Size, F.getName(), (uint8_t*)Code, Size, NULL);
#endif
        FuncInfo tmp = {&F, Size, Details.LineStarts, linfo};
        info[(size_t)(Code)] = tmp;
#ifndef KEEP_BODIES
        if (!jl_generating_output())
            const_cast<Function*>(&F)->deleteBody();
#endif
        uv_rwlock_wrunlock(&threadsafe);
        jl_gc_safe_leave(gc_state);
    }

    std::map<size_t, FuncInfo, revcomp>& getMap()
    {
        uv_rwlock_rdlock(&threadsafe);
        return info;
    }
#endif // ifndef USE_MCJIT

#ifdef USE_MCJIT
    jl_lambda_info_t *lookupLinfo(size_t pointer)
    {
        auto linfo = linfomap.lower_bound(pointer);
        if (linfo != linfomap.end() && pointer < linfo->first + linfo->second.first)
            return linfo->second.second;
        else
            return NULL;
    }
#ifdef LLVM36

    virtual void NotifyObjectEmitted(const object::ObjectFile &obj,
                                     const RuntimeDyld::LoadedObjectInfo &L)
    {
        return _NotifyObjectEmitted(obj,obj,L);
    }

    virtual void _NotifyObjectEmitted(const object::ObjectFile &obj,
                                     const object::ObjectFile &debugObj,
                                     const RuntimeDyld::LoadedObjectInfo &L)
#else
    virtual void NotifyObjectEmitted(const ObjectImage &obj)
#endif
    {
        // This function modify linfo->fptr in GC safe region.
        // This should be fine since the GC won't scan this field.
        int8_t gc_state = jl_gc_safe_enter();
        uv_rwlock_wrlock(&threadsafe);
#ifdef LLVM36
        object::section_iterator Section = debugObj.section_begin();
        object::section_iterator EndSection = debugObj.section_end();
#else
        object::section_iterator Section = debugObj.begin_sections();
        object::section_iterator EndSection = debugObj.end_sections();
#endif

#ifdef LLVM38
        std::map<StringRef,object::SectionRef,strrefcomp> loadedSections;
        for (const object::SectionRef &lSection: obj.sections()) {
            StringRef sName;
            if (!lSection.getName(sName)) {
                loadedSections[sName] = lSection;
            }
        }
        auto getLoadAddress = [&] (const StringRef &sName) -> uint64_t {
            auto search = loadedSections.find(sName);
            if (search == loadedSections.end())
                return 0;
            return L.getSectionLoadAddress(search->second);
        };
#endif

#if defined(_OS_WINDOWS_)
        uint64_t SectionAddrCheck = 0; // assert that all of the Sections are at the same location
        uint8_t *UnwindData = NULL;
#if defined(_CPU_X86_64_)
        uint8_t *catchjmp = NULL;
        for (const object::SymbolRef &sym_iter : debugObj.symbols()) {
            StringRef sName;
#ifdef LLVM37
            auto sNameOrError = sym_iter.getName();
            assert(sNameOrError);
            sName = sNameOrError.get();
#else
            sym_iter.getName(sName);
#endif
            uint8_t **pAddr = NULL;
            if (sName.equals("__UnwindData")) {
                pAddr = &UnwindData;
            }
            else if (sName.equals("__catchjmp")) {
                pAddr = &catchjmp;
            }
            if (pAddr) {
                uint64_t Addr, SectionAddr, SectionLoadAddr;
#if defined(LLVM38)
                auto AddrOrError = sym_iter.getAddress();
                assert(AddrOrError);
                Addr = AddrOrError.get();
                auto SectionOrError = sym_iter.getSection();
                assert(SectionOrError);
                Section = SectionOrError.get();
                assert(Section != EndSection && Section->isText());
                SectionAddr = Section->getAddress();
                Section->getName(sName);
                SectionLoadAddr = getLoadAddress(sName);
#elif defined(LLVM37)
                auto AddrOrError = sym_iter.getAddress();
                assert(AddrOrError);
                Addr = AddrOrError.get();
                sym_iter.getSection(Section);
                assert(Section != EndSection && Section->isText());
                Section->getName(sName);
                SectionAddr = Section->getAddress();
                SectionLoadAddr = L.getSectionLoadAddress(sName);
#elif defined(LLVM36)
                sym_iter.getAddress(Addr);
                sym_iter.getSection(Section);
                assert(Section != EndSection && Section->isText());
                Section->getName(sName);
                SectionAddr = Section->getAddress();
                SectionLoadAddr = L.getSectionLoadAddress(sName);
#else // LLVM35
                sym_iter.getAddress(Addr);
                sym_iter.getSection(Section);
                assert(Section != EndSection);
                assert(!Section->isText(isText) && isText);
                Section->getAddress(SectionAddr);
                Section->getAddress(SectionLoadAddr);
#endif
                Addr -= SectionAddr - SectionLoadAddr;
                *pAddr = (uint8_t*)Addr;
                if (SectionAddrCheck)
                    assert(SectionAddrCheck == SectionLoadAddr);
                else
                    SectionAddrCheck = SectionLoadAddr;
            }
        }
        assert(catchjmp);
        assert(UnwindData);
        assert(SectionAddrCheck);
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
        *(DWORD*)&UnwindData[8] = (DWORD)(catchjmp - (uint8_t*)SectionAddrCheck); // relative location of catchjmp
#endif // defined(_OS_X86_64_)
#endif // defined(_OS_WINDOWS_)

#ifdef LLVM37
        auto symbols = object::computeSymbolSizes(debugObj);
        bool first = true;
        for(const auto &sym_size : symbols) {
            const object::SymbolRef &sym_iter = sym_size.first;
#ifdef LLVM39
            auto SymbolTypeOrError = sym_iter.getType();
            assert(SymbolTypeOrError);
            object::SymbolRef::Type SymbolType = SymbolTypeOrError.get();
#else
            object::SymbolRef::Type SymbolType = sym_iter.getType();
#endif
            if (SymbolType != object::SymbolRef::ST_Function) continue;
            auto AddrOrError = sym_iter.getAddress();
            assert(AddrOrError);
            uint64_t Addr = AddrOrError.get();
#ifdef LLVM38
            auto SectionOrError = sym_iter.getSection();
            assert(SectionOrError);
            Section = SectionOrError.get();
#else
            sym_iter.getSection(Section);
#endif
            if (Section == EndSection) continue;
            if (!Section->isText()) continue;
            uint64_t SectionAddr = Section->getAddress();
            StringRef secName;
            Section->getName(secName);
#ifdef LLVM38
            uint64_t SectionLoadAddr = getLoadAddress(secName);
#else
            uint64_t SectionLoadAddr = L.getSectionLoadAddress(secName);
#endif
            Addr -= SectionAddr - SectionLoadAddr;
            auto sNameOrError = sym_iter.getName();
            assert(sNameOrError);
            StringRef sName = sNameOrError.get();
            uint64_t SectionSize = Section->getSize();
            size_t Size = sym_size.second;
#if defined(_OS_WINDOWS_)
            if (SectionAddrCheck)
                assert(SectionAddrCheck == SectionLoadAddr);
            else
                SectionAddrCheck = SectionLoadAddr;
            create_PRUNTIME_FUNCTION(
                   (uint8_t*)(intptr_t)Addr, (size_t)Size, sName,
                   (uint8_t*)(intptr_t)SectionLoadAddr, (size_t)SectionSize, UnwindData);
#endif
            StringMap<jl_lambda_info_t*>::iterator linfo_it = linfo_in_flight.find(sName);
            jl_lambda_info_t *linfo = NULL;
            if (linfo_it != linfo_in_flight.end()) {
                linfo = linfo_it->second;
                if (linfo->compile_traced)
                    triggered_linfos.push_back(linfo);
                linfo_in_flight.erase(linfo_it);
                if (((Function*)linfo->functionObjectsDecls.functionObject)->getName().equals(sName))
                    linfo->fptr = (jl_fptr_t)(uintptr_t)Addr;
            }
            if (linfo)
                linfomap[Addr] = std::make_pair(Size, linfo);
            if (first) {
                ObjectInfo tmp = {&debugObj,
                    (size_t)SectionSize,
                    (ptrdiff_t)(SectionAddr - SectionLoadAddr),
                    new DWARFContextInMemory(debugObj, &L),
                    };
                objectmap[SectionLoadAddr] = tmp;
                first = false;
           }
        }

#else // pre-LLVM37
        uint64_t Addr;
        uint64_t Size;
        object::SymbolRef::Type SymbolType;
        StringRef sName;
        uint64_t SectionLoadAddr = 0, SectionAddr = 0;
#ifndef LLVM36
        bool isText;
#endif

#if defined(LLVM35)
        for (const object::SymbolRef &sym_iter : obj.symbols()) {
            sym_iter.getType(SymbolType);
            if (SymbolType != object::SymbolRef::ST_Function) continue;
            sym_iter.getSize(Size);
            sym_iter.getAddress(Addr);
            sym_iter.getSection(Section);
            if (Section == EndSection) continue;
#if defined(LLVM36)
            if (!Section->isText()) continue;
            Section->getName(sName);
            SectionAddr = Section->getAddress();
            SectionLoadAddr = L.getSectionLoadAddress(sName);
            Addr += SectionLoadAddr;
#else
            if (Section->isText(isText) || !isText) continue;
            Section->getAddress(SectionAddr);
            Section->getAddress(SectionLoadAddr);
#endif
            sym_iter.getName(sName);
#ifdef _OS_DARWIN_
#   if !defined(LLVM36)
            Addr = ((MCJIT*)jl_ExecutionEngine)->getSymbolAddress(sName, true);
            if (!Addr && sName[0] == '_') {
                Addr = ((MCJIT*)jl_ExecutionEngine)->getSymbolAddress(sName.substr(1), true);
            }
            if (!Addr) continue;
#   endif
#elif defined(_OS_WINDOWS_)
            uint64_t SectionSize = 0;
#   if defined(LLVM36)
            SectionSize = Section->getSize();
#   else
            Section->getSize(SectionSize);
#   endif
            if (SectionAddrCheck)
                assert(SectionAddrCheck == SectionLoadAddr);
            else
                SectionAddrCheck = SectionLoadAddr;
            create_PRUNTIME_FUNCTION(
                   (uint8_t*)(intptr_t)Addr, (size_t)Size, sName,
                   (uint8_t*)(intptr_t)SectionLoadAddr, (size_t)SectionSize, UnwindData);
#endif
            StringMap<jl_lambda_info_t*>::iterator linfo_it = linfo_in_flight.find(sName);
            jl_lambda_info_t *linfo = NULL;
            if (linfo_it != linfo_in_flight.end()) {
                linfo = linfo_it->second;
                linfo_in_flight.erase(linfo_it);
                if (((Function*)linfo->functionObjectsDecls.functionObject)->getName().equals(sName))
                    linfo->fptr = (jl_fptr_t)(uintptr_t)Addr;
            }
            if (linfo)
                linfomap[Addr] = std::make_pair(Size, linfo);
            const object::ObjectFile *objfile =
#ifdef LLVM36
                &obj;
#else
                obj.getObjectFile();
#endif
            ObjectInfo tmp = {objfile, (size_t)Size,
                (ptrdiff_t)(SectionAddr - SectionLoadAddr),
#ifdef _OS_DARWIN_
                strndup(sName.data(), sName.size()),
#endif
                linfo
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
#endif
        uv_rwlock_wrunlock(&threadsafe);
        jl_gc_safe_leave(gc_state);
    }

    // must implement if we ever start freeing code
    // virtual void NotifyFreeingObject(const ObjectImage &obj) {}
    // virtual void NotifyFreeingObject(const object::ObjectFile &Obj) {}

    std::map<size_t, ObjectInfo, revcomp>& getObjectMap()
    {
        uv_rwlock_rdlock(&threadsafe);
        return objectmap;
    }
#endif // USE_MCJIT
};

#ifdef USE_ORCJIT
JL_DLLEXPORT void ORCNotifyObjectEmitted(JITEventListener *Listener,
                                         const object::ObjectFile &obj,
                                         const object::ObjectFile &debugObj,
                                         const RuntimeDyld::LoadedObjectInfo &L)
{
    ((JuliaJITEventListener*)Listener)->_NotifyObjectEmitted(obj,debugObj,L);
}
#endif

extern "C"
char *jl_demangle(const char *name)
{
    // This function is not allowed to reference any TLS variables since
    // it can be called from an unmanaged thread on OSX.
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

static JuliaJITEventListener *jl_jit_events;
JITEventListener *CreateJuliaJITEventListener()
{
    jl_jit_events = new JuliaJITEventListener();
    return jl_jit_events;
}

// *frames is a one element array containing whatever we could come up
// with for the current frame. here we'll try to expand it using debug info
// func_name and file_name are either NULL or malloc'd pointers
static int lookup_pointer(DIContext *context, jl_frame_t **frames,
                          size_t pointer, int demangle, int noInline)
{
    // This function is not allowed to reference any TLS variables if noInline
    // since it can be called from an unmanaged thread on OSX.
    if (!context) {
        if (demangle && (*frames)[0].func_name != NULL) {
            char *oldname = (*frames)[0].func_name;
            (*frames)[0].func_name = jl_demangle(oldname);
            free(oldname);
        }
        return 1;
    }
#ifdef LLVM35
    DILineInfoSpecifier infoSpec(DILineInfoSpecifier::FileLineInfoKind::AbsoluteFilePath,
                                 DILineInfoSpecifier::FunctionNameKind::ShortName);
#else
    int infoSpec = DILineInfoSpecifier::FileLineInfo |
                   DILineInfoSpecifier::AbsoluteFilePath |
                   DILineInfoSpecifier::FunctionName;
#endif

    auto inlineInfo = context->getInliningInfoForAddress(pointer, infoSpec);

    int fromC = (*frames)[0].fromC;
    int n_frames = inlineInfo.getNumberOfFrames();
    if (noInline)
        n_frames = 1;
    if (n_frames > 1) {
        jl_frame_t *new_frames = (jl_frame_t*)calloc(sizeof(jl_frame_t), n_frames);
        memcpy(&new_frames[n_frames-1], *frames, sizeof(jl_frame_t));
        free(*frames);
        *frames = new_frames;
    }
    jl_lambda_info_t *outer_linfo = (*frames)[n_frames-1].linfo;
    for (int i = 0; i < n_frames; i++) {
        bool inlined_frame = i != n_frames - 1;
        DILineInfo info;
        if (!noInline) {
            info = inlineInfo.getFrame(i);
        }
        else {
            info = context->getLineInfoForAddress(pointer, infoSpec);
        }

        jl_frame_t *frame = &(*frames)[i];
#ifndef LLVM35
        std::string func_name(info.getFunctionName());
#else
        std::string func_name(info.FunctionName);
#endif

        if (inlined_frame) {
            frame->inlined = 1;
            frame->fromC = fromC;
            if (outer_linfo) {
                std::size_t semi_pos = func_name.find(';');
                if (semi_pos != std::string::npos) {
                    func_name = func_name.substr(0, semi_pos);
                    frame->linfo = NULL; // TODO
                }
            }
        }

        if (func_name == "<invalid>")
            frame->func_name = NULL;
        else
            jl_copy_str(&frame->func_name, func_name.c_str());
#ifndef LLVM35
        frame->line = info.getLine();
        std::string file_name(info.getFileName());
#else
        frame->line = info.Line;
        std::string file_name(info.FileName);
#endif
        if (file_name == "<invalid>")
            frame->file_name = NULL;
        else
            jl_copy_str(&frame->file_name, file_name.c_str());

        if (!frame->func_name || !func_name.compare(0, 7, "jlcall_") || !func_name.compare(0, 7, "jlcapi_")) {
            frame->fromC = 1;
        }
    }
    return n_frames;
}

#ifdef _OS_DARWIN_
#include <mach-o/dyld.h>
#endif
#ifndef _OS_WINDOWS_
#include <dlfcn.h>
#endif
typedef struct {
    const llvm::object::ObjectFile *obj;
    DIContext *ctx;
    int64_t slide;
    int64_t section_slide;
} objfileentry_t;
typedef std::map<uint64_t, objfileentry_t, revcomp> obfiletype;
static obfiletype objfilemap;

#ifdef _OS_DARWIN_
static bool getObjUUID(llvm::object::MachOObjectFile *obj, uint8_t uuid[16])
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

static uint64_t jl_sysimage_base;
static void **sysimg_fvars;
static jl_lambda_info_t **sysimg_fvars_linfo;
static size_t sysimg_fvars_n;
extern "C" void jl_register_fptrs(uint64_t sysimage_base, void **fptrs, jl_lambda_info_t **linfos, size_t n)
{
    jl_sysimage_base = (uintptr_t)sysimage_base;
    sysimg_fvars = fptrs;
    sysimg_fvars_linfo = linfos;
    sysimg_fvars_n = n;
}

bool jl_dylib_DI_for_fptr(size_t pointer, const llvm::object::ObjectFile **obj, llvm::DIContext **context, int64_t *slide, int64_t *section_slide,
    bool onlySysImg, bool *isSysImg, void **saddr, char **name, char **filename)
{
    *obj = NULL;
    *context = NULL;
    *slide = 0;
    *section_slide = 0;
// GOAL: Determine containing Library
// Assigning fname, fbase, msize
#ifdef _OS_WINDOWS_
    IMAGEHLP_MODULE64 ModuleInfo;
    bool isvalid;
    ModuleInfo.SizeOfStruct = sizeof(IMAGEHLP_MODULE64);
    jl_in_stackwalk = 1;
    isvalid = SymGetModuleInfo64(GetCurrentProcess(), (DWORD64)pointer, &ModuleInfo);
    jl_in_stackwalk = 0;
    if (isvalid) {
        char *fname = ModuleInfo.LoadedImageName;
        if (!fname[0]) // empirically, LoadedImageName might be missing
            fname = ModuleInfo.ImageName;
        DWORD64 fbase = ModuleInfo.BaseOfImage;
        bool insysimage = (fbase == jl_sysimage_base);
        if (isSysImg)
            *isSysImg = insysimage;
        if (onlySysImg && insysimage) {
            return false;
        }
        static char frame_info_func[
            sizeof(SYMBOL_INFO) +
            MAX_SYM_NAME * sizeof(TCHAR)];
        DWORD64 dwDisplacement64 = 0;
        DWORD64 dwAddress = pointer;
        PSYMBOL_INFO pSymbol = (PSYMBOL_INFO)frame_info_func;
        pSymbol->SizeOfStruct = sizeof(SYMBOL_INFO);
        pSymbol->MaxNameLen = MAX_SYM_NAME;
        jl_in_stackwalk = 1;
        if (SymFromAddr(GetCurrentProcess(), dwAddress, &dwDisplacement64,
                        pSymbol)) {
            // SymFromAddr returned success
            // errors are ignored, but are hopefully patched up by
            // using llvm to read the object (below)
            if (name)
                jl_copy_str(name, pSymbol->Name);
            if (saddr)
                *saddr = (void*)(uintptr_t)pSymbol->Address;
        }

        // If we didn't find the filename before in the debug
        // info, use the dll name
        if (filename && !*filename)
            jl_copy_str(filename, fname);

        jl_in_stackwalk = 0;
#else // ifdef _OS_WINDOWS_
    Dl_info dlinfo;
    int dladdr_success;
    uint64_t fbase;
#ifdef _OS_LINUX_
    struct link_map *extra_info;
    dladdr_success = dladdr1((void*)pointer, &dlinfo, (void**)&extra_info, RTLD_DL_LINKMAP) != 0;
#else
    dladdr_success = dladdr((void*)pointer, &dlinfo) != 0;
#endif

    if (dladdr_success && dlinfo.dli_fname) {
#ifdef _OS_LINUX_
        // dlinfo.dli_fbase is not the right value for the main executable on linux
        fbase = (uintptr_t)extra_info->l_addr;
#else
        fbase = (uintptr_t)dlinfo.dli_fbase;
#endif
        const char *fname;
        if (saddr)
            *saddr = dlinfo.dli_saddr;
#if defined(_OS_DARWIN_)
        size_t msize = (size_t)(((uint64_t)-1)-fbase);
#endif
        bool insysimage = (fbase == jl_sysimage_base);
        if (isSysImg)
            *isSysImg = insysimage;
        if (onlySysImg && insysimage) {
            return false;
        }
        // In case we fail with the debug info lookup, we at least still
        // have the function name, even if we don't have line numbers
        if (name)
            jl_copy_str(name, dlinfo.dli_sname);
        if (filename)
            jl_copy_str(filename, dlinfo.dli_fname);
        fname = dlinfo.dli_fname;
#endif // ifdef _OS_WINDOWS_

// GOAL: Read debuginfo from file
#if !defined(_OS_WINDOWS_) || defined(LLVM35)
        // TODO: need read/write lock here for objfilemap synchronization
        obfiletype::iterator it = objfilemap.find(fbase);
        if (it != objfilemap.end()) {
            // Return cached value
            *obj = it->second.obj;
            *context = it->second.ctx;
            *slide = it->second.slide;
            *section_slide = it->second.section_slide;
        }
        else {
// GOAL: Assign errorobj
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
                objfileentry_t entry = {*obj,*context,*slide,*section_slide};
                objfilemap[fbase] = entry;
                return true;
            }

#ifdef LLVM36
            *obj = (llvm::object::MachOObjectFile *)origerrorobj.get().release();
#elif defined(LLVM35)
            *obj = (llvm::object::MachOObjectFile *)origerrorobj.get();
#else
            *obj = (llvm::object::MachOObjectFile *)origerrorobj;
#endif
            llvm::object::MachOObjectFile *morigobj = (llvm::object::MachOObjectFile *)*obj;


            // First find the uuid of the object file (we'll use this to make sure we find the
            // correct debug symbol file).
            uint8_t uuid[16], uuid2[16];
            if (!getObjUUID(morigobj,uuid)) {
                objfileentry_t entry = {*obj,*context,*slide,*section_slide};
                objfilemap[fbase] = entry;
                return true;
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

#else // ifndef _OS_DARWIN_

            // On Linux systems we need to mmap another copy because of the permissions on the mmap'ed shared library.
            // On Windows we need to mmap another copy since reading the in-memory copy seems to return object_error:unexpected_eof
#ifdef LLVM35
            auto errorobj = llvm::object::ObjectFile::createObjectFile(fname);
#else
            llvm::object::ObjectFile *errorobj = llvm::object::ObjectFile::createObjectFile(fname);
#endif
#endif // ifdef _OS_DARWIN_

// GOAL: Assign *obj, *context, *slide (if above succeeded)
            if (errorobj) {
#ifdef LLVM36
                auto binary = errorobj.get().takeBinary();
                *obj = binary.first.release();
                binary.second.release();
#elif defined(LLVM35)
                *obj = errorobj.get();
#else
                *obj = errorobj;
#endif
#ifdef _OS_DARWIN_
                if (getObjUUID((llvm::object::MachOObjectFile *)*obj,uuid2) &&
                    memcmp(uuid,uuid2,sizeof(uuid)) == 0) {
#endif
#ifdef LLVM37
                    *context = new DWARFContextInMemory(**obj);
#elif defined(LLVM36)
                    *context = DIContext::getDWARFContext(**obj);
#else
                    *context = DIContext::getDWARFContext(const_cast<object::ObjectFile*>(*obj));
#endif
                    *slide = -(int64_t)fbase;
#ifdef _OS_DARWIN_
                } else {
                    // If we're here the, the dsym does not match the dylib. Use the original
                    // object instead. For consistency (and to make sure we get a sensible size
                    // for the memory buffer), we also use a fresh copy mapped from
                    // the file rather than reusing the one in memory. We may want to revisit
                    // that in the future (ideally, once we support fewer LLVM versions).
                    errorobj = llvm::object::ObjectFile::createObjectFile(fname);
                    assert(errorobj);
#ifdef LLVM36
                    auto binary = errorobj.get().takeBinary();
                    *obj = binary.first.release();
                    binary.second.release();
#elif defined(LLVM35)
                    *obj = errorobj.get();
#else
                    *obj = errorobj;
#endif
                    delete morigobj;
                }
#endif
#if defined(_OS_WINDOWS_)
                assert((*obj)->isCOFF());
                const llvm::object::COFFObjectFile *coffobj = (const llvm::object::COFFObjectFile *)*obj;
                const llvm::object::pe32plus_header *pe32plus;
                coffobj->getPE32PlusHeader(pe32plus);
                if (pe32plus != NULL) {
                    *slide = pe32plus->ImageBase - fbase;
                    *section_slide = -(int64_t)pe32plus->ImageBase;
                }
                else {
                    const llvm::object::pe32_header *pe32;
                    coffobj->getPE32Header(pe32);
                    if (pe32 == NULL) {
                        *obj = NULL;
                        *context = NULL;
                        *slide = 0;
                    }
                    else {
                        *slide = pe32->ImageBase - fbase;
                        *section_slide = -(int64_t)pe32->ImageBase;
                    }
                }
#endif
            }
#ifdef LLVM39
            else {
                // TODO: report the error instead of silently consuming it?
                //       jl_error might run into the same error again...
                consumeError(errorobj.takeError());
            }
#endif

            // update cache
            objfileentry_t entry = {*obj,*context,*slide,*section_slide};
            objfilemap[fbase] = entry;
        }
#endif
        return true;
    }
    return false;
}

// *name and *filename should be either NULL or malloc'd pointer
static int jl_getDylibFunctionInfo(jl_frame_t **frames, size_t pointer, int skipC, int noInline)
{
    // This function is not allowed to reference any TLS variables if noInline
    // since it can be called from an unmanaged thread on OSX.
    jl_frame_t *frame0 = *frames;
#ifdef _OS_WINDOWS_
    static IMAGEHLP_LINE64 frame_info_line;
    DWORD dwDisplacement = 0;
    if (jl_in_stackwalk) {
        frame0->fromC = 1;
        return 1;
    }
    jl_in_stackwalk = 1;
    DWORD64 dwAddress = pointer;
    frame_info_line.SizeOfStruct = sizeof(IMAGEHLP_LINE64);
    if (SymGetLineFromAddr64(GetCurrentProcess(), dwAddress, &dwDisplacement, &frame_info_line)) {
        // SymGetLineFromAddr64 returned success
        // record source file name and line number
        if (frame_info_line.FileName)
            jl_copy_str(&frame0->file_name, frame_info_line.FileName);
        frame0->line = frame_info_line.LineNumber;
    }
    jl_in_stackwalk = 0;
#endif
    const object::ObjectFile *object;
    llvm::DIContext *context = NULL;
    bool isSysImg;
    void *saddr;
    int64_t slide, section_slide;
    if (!jl_dylib_DI_for_fptr(pointer, &object, &context, &slide, &section_slide, skipC, &isSysImg, &saddr, &frame0->func_name, &frame0->file_name)) {
        frame0->fromC = 1;
        return 1;
    }
    frame0->fromC = !isSysImg;
    if (isSysImg && sysimg_fvars) {
#ifdef _OS_LINUX_
        unw_proc_info_t pip;
        if (!saddr && unw_get_proc_info_by_ip(unw_local_addr_space,
                                              pointer, &pip, NULL) == 0)
            saddr = (void*)pip.start_ip;
#endif
        if (saddr) {
            for (size_t i = 0; i < sysimg_fvars_n; i++) {
                if (saddr == sysimg_fvars[i]) {
                    frame0->linfo = sysimg_fvars_linfo[i];
                    break;
                }
            }
        }
        return lookup_pointer(context, frames, pointer+slide, isSysImg, noInline);
    }
    return lookup_pointer(context, frames, pointer+slide, isSysImg, noInline);
}

int jl_DI_for_fptr(uint64_t fptr, uint64_t *symsize, int64_t *slide, int64_t *section_slide,
                      const object::ObjectFile **object,
#ifdef USE_MCJIT
                      llvm::DIContext **context
#else
                      std::vector<JITEvent_EmittedFunctionDetails::LineStart> *lines
#endif
                      )
{
    int found = 0;
    *slide = 0;
#ifndef USE_MCJIT
    std::map<size_t, FuncInfo, revcomp> &fmap = jl_jit_events->getMap();
    std::map<size_t, FuncInfo, revcomp>::iterator fit = fmap.lower_bound(fptr);

    if (fit != fmap.end() && fptr < fit->first + fit->second.lengthAdr) {
        if (symsize)
            *symsize = fit->second.lengthAdr;
        *lines = fit->second.lines;
        found = 1;
    }
#else // MCJIT version
    std::map<size_t, ObjectInfo, revcomp> &objmap = jl_jit_events->getObjectMap();
    std::map<size_t, ObjectInfo, revcomp>::iterator fit = objmap.lower_bound(fptr);

    if (fit != objmap.end() && fptr < fit->first + fit->second.SectionSize) {
        if (symsize)
            *symsize = 0;
        if (section_slide)
            *section_slide = fit->second.slide;
        *object = fit->second.object;
        if (context) {
#if defined(LLVM37)
            *context = fit->second.context;
#else
            *context = DIContext::getDWARFContext(*fit->second.object);
#endif
        }
        found = 1;
    }
#endif
    uv_rwlock_rdunlock(&threadsafe);
    return found;
}

#ifdef USE_MCJIT
extern "C"
JL_DLLEXPORT jl_value_t *jl_get_dobj_data(uint64_t fptr)
{
    // Used by Gallium.jl
    const object::ObjectFile *object = NULL;
    DIContext *context;
    int64_t slide, section_slide;
    int8_t gc_state = jl_gc_safe_enter();
    if (!jl_DI_for_fptr(fptr, NULL, &slide, NULL, &object, NULL))
        if (!jl_dylib_DI_for_fptr(fptr, &object, &context, &slide, &section_slide, false, NULL, NULL, NULL, NULL)) {
            jl_gc_safe_leave(gc_state);
            return jl_nothing;
        }
    jl_gc_safe_leave(gc_state);
    if (object == NULL)
        return jl_nothing;
    return (jl_value_t*)jl_ptr_to_array_1d((jl_value_t*)jl_array_uint8_type,
        const_cast<char*>(object->getData().data()),
        object->getData().size(), false);
}

extern "C"
JL_DLLEXPORT uint64_t jl_get_section_start(uint64_t fptr)
{
    // Used by Gallium.jl
    int8_t gc_state = jl_gc_safe_enter();
    std::map<size_t, ObjectInfo, revcomp> &objmap = jl_jit_events->getObjectMap();
    std::map<size_t, ObjectInfo, revcomp>::iterator fit = objmap.lower_bound(fptr);

    uint64_t ret = 0;
    if (fit != objmap.end() && fptr < fit->first + fit->second.SectionSize) {
        ret = fit->first;
    } else {
       obfiletype::iterator objit = objfilemap.lower_bound(fptr);
       // Ideally we'd have a containment check here, but we can't really
       // get the shared library size easily.
       if (objit != objfilemap.end()) {
           ret = objit->first;
       }
    }
    uv_rwlock_rdunlock(&threadsafe);
    jl_gc_safe_leave(gc_state);
    return ret;
}

#endif

// Set *name and *filename to either NULL or malloc'd string
int jl_getFunctionInfo(jl_frame_t **frames_out, size_t pointer, int skipC, int noInline)
{
    // This function is not allowed to reference any TLS variables if noInline
    // since it can be called from an unmanaged thread on OSX.

    jl_frame_t *frames = (jl_frame_t*)calloc(sizeof(jl_frame_t), 1);
    frames[0].line = -1;
    *frames_out = frames;

#ifdef USE_MCJIT
    llvm::DIContext *context;
    const llvm::object::ObjectFile *object;
    uint64_t symsize;
    int64_t slide = 0;
    if (jl_DI_for_fptr(pointer, &symsize, &slide, NULL, &object, &context)) {
        frames[0].linfo = jl_jit_events->lookupLinfo(pointer);
        int nf = lookup_pointer(context, frames_out, pointer+slide, 1, noInline);
        return nf;
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
            frames[0].fromC = 1;
            uv_rwlock_rdunlock(&threadsafe);
            return 1;
        }

        jl_copy_str(&frames[0].func_name, (*it).second.func->getName().str().c_str());
        jl_copy_str(&frames[0].file_name, "");

        if ((*it).second.lines.empty()) {
            frames[0].fromC = 1;
            uv_rwlock_rdunlock(&threadsafe);
            return 1;
        }

        frames[0].linfo = (*it).second.linfo;
        std::vector<JITEvent_EmittedFunctionDetails::LineStart>::iterator vit =
            (*it).second.lines.begin();
        JITEvent_EmittedFunctionDetails::LineStart prev = *vit;

        if ((*it).second.func) {
            DISubprogram debugscope =
                DISubprogram(prev.Loc.getScope((*it).second.func->getContext()));
            jl_copy_str(&frames[0].file_name, debugscope.getFilename().str().c_str());
            // the DISubprogram has the un-mangled name, so use that if
            // available. However, if the scope need not be the current
            // subprogram.
            if (debugscope.getName().data() != NULL) {
                jl_copy_str(&frames[0].func_name, debugscope.getName().str().c_str());
            }
            else {
                char *oldname = frames[0].func_name;
                frames[0].func_name = jl_demangle(frames[0].func_name);
                free(oldname);
            }
        }

        vit++;

        while (vit != (*it).second.lines.end()) {
            if (pointer <= (*vit).Address) {
                frames[0].line = prev.Loc.getLine();
                break;
            }
            prev = *vit;
            vit++;
        }
        if (frames[0].line == -1) {
            frames[0].line = prev.Loc.getLine();
        }

        DILexicalBlockFile locscope = DILexicalBlockFile(prev.Loc.getScope((*it).second.func->getContext()));
        jl_copy_str(&frames[0].file_name, locscope.getFilename().str().c_str());

        /*MDNode *inlinedAt = skipInline ? NULL : prev.Loc.getInlinedAt((*it).second.func->getContext());
        if ((!skipInline) && (inlinedAt != NULL)) {
            DebugLoc inlineloc = DebugLoc::getFromDILocation(inlinedAt);
            DILexicalBlockFile inlinescope = DILexicalBlockFile(inlineloc.getScope((*it).second.func->getContext()));
            jl_copy_str(&frames, inlinescope.getFilename().str().c_str());
            *inlinedat_line = inlineloc.getLine();
            }*/
        uv_rwlock_rdunlock(&threadsafe);
        return 1;
    }
    uv_rwlock_rdunlock(&threadsafe);
#endif // USE_MCJIT
    return jl_getDylibFunctionInfo(frames_out, pointer, skipC, noInline);
}

#if defined(LLVM37) && (defined(_OS_LINUX_) || (defined(_OS_DARWIN_) && defined(LLVM_SHLIB)))
extern "C" void __register_frame(void*);
extern "C" void __deregister_frame(void*);

template <typename callback>
static const char *processFDE(const char *Entry, callback f)
{
    const char *P = Entry;
    uint32_t Length = *((const uint32_t *)P);
    P += 4;
    uint32_t Offset = *((const uint32_t *)P);
    if (Offset != 0) {
        f(Entry);
    }
    return P + Length;
}

template <typename callback>
static void processFDEs(const char *EHFrameAddr, size_t EHFrameSize, callback f)
{
    const char *P = (const char*)EHFrameAddr;
    const char *End = P + EHFrameSize;
    do  {
        P = processFDE(P, f);
    } while(P != End);
}
#endif

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
    void deregisterEHFrames(uint8_t *Addr, uint64_t LoadAddr, size_t Size) override;
};

static void (*libc_register_frame)(void*)   = NULL;
static void (*libc_deregister_frame)(void*) = NULL;

// This implementation handles frame registration for local targets.
// Memory managers for remote targets should re-implement this function
// and use the LoadAddr parameter.
void RTDyldMemoryManagerOSX::registerEHFrames(uint8_t *Addr,
                                              uint64_t LoadAddr,
                                              size_t Size)
{
  // On OS X OS X __register_frame takes a single FDE as an argument.
  // See http://lists.cs.uiuc.edu/pipermail/llvmdev/2013-April/061768.html
  processFDEs((char*)Addr, Size, [](const char *Entry) {
        if (!libc_register_frame) {
          libc_register_frame = (void(*)(void*))dlsym(RTLD_NEXT,"__register_frame");
        }
        assert(libc_register_frame);
        libc_register_frame(const_cast<char *>(Entry));
        __register_frame(const_cast<char *>(Entry));
    });
}

void RTDyldMemoryManagerOSX::deregisterEHFrames(uint8_t *Addr,
                                                uint64_t LoadAddr,
                                                size_t Size)
{
   processFDEs((char*)Addr, Size, [](const char *Entry) {
        if (!libc_deregister_frame) {
          libc_deregister_frame = (void(*)(void*))dlsym(RTLD_NEXT,"__deregister_frame");
        }
        assert(libc_deregister_frame);
        libc_deregister_frame(const_cast<char *>(Entry));
        __deregister_frame(const_cast<char *>(Entry));
    });
}

RTDyldMemoryManager* createRTDyldMemoryManagerOSX()
{
    return new RTDyldMemoryManagerOSX();
}

#endif

#if defined(_OS_LINUX_) && defined(LLVM37) && defined(JL_UNW_HAS_FORMAT_IP)
#include <type_traits>
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
class RTDyldMemoryManagerUnix : public SectionMemoryManager
{
    RTDyldMemoryManagerUnix(const RTDyldMemoryManagerUnix&) = delete;
    void operator=(const RTDyldMemoryManagerUnix&) = delete;

public:
    RTDyldMemoryManagerUnix() {};
    ~RTDyldMemoryManagerUnix() override {};
    void registerEHFrames(uint8_t *Addr, uint64_t LoadAddr, size_t Size) override;
    void deregisterEHFrames(uint8_t *Addr, uint64_t LoadAddr, size_t Size) override;
};

struct unw_table_entry
{
    int32_t start_ip_offset;
    int32_t fde_offset;
};

// Skip over an arbitrary long LEB128 encoding.
// Return the pointer to the first unprocessed byte.
static const uint8_t *consume_leb128(const uint8_t *Addr, const uint8_t *End)
{
    const uint8_t *P = Addr;
    while ((*P >> 7) != 0 && P < End)
        ++P;
    return P + 1;
}

// Parse a LEB128 encoding to a type T. Truncate the result if there's more
// bytes than what there are more bytes than what the type can store.
// Adjust the pointer to the first unprocessed byte.
template<typename T> static T parse_leb128(const uint8_t *&Addr,
                                           const uint8_t *End)
{
    typedef typename std::make_unsigned<T>::type uT;
    uT v = 0;
    for (unsigned i = 0;i < ((sizeof(T) * 8 - 1) / 7 + 1);i++) {
        uint8_t a = *Addr;
        Addr++;
        v |= uT(a & 0x7f) << (i * 7);
        if ((a & 0x80) == 0 || Addr >= End) {
            if (a & 0x40 && std::is_signed<T>::value) {
                int valid_bits = (i + 1) * 7;
                if (valid_bits < 64) {
                    v |= -(uT(1) << valid_bits);
                }
            }
            return T(v);
        }
    }
    Addr = consume_leb128(Addr, End);
    return T(v);
}

template <typename U, typename T>
static U safe_trunc(T t)
{
    assert((t >= static_cast<T>(std::numeric_limits<U>::min()))
           && (t <= static_cast<T>(std::numeric_limits<U>::max())));
    return static_cast<U>(t);
}

// How the address and size in the FDE are encoded.
enum DW_EH_PE : uint8_t {
    DW_EH_PE_absptr = 0x00, /* An absolute pointer. The size is determined by
                             * whether this is a 32-bit or 64-bit address space,
                             * and will be 32 or 64 bits */
    DW_EH_PE_omit = 0xff, // The value is omitted
    DW_EH_PE_uleb128 = 0x01, // The value is an unsigned LEB128
    DW_EH_PE_udata2 = 0x02,
    DW_EH_PE_udata4 = 0x03,
    DW_EH_PE_udata8 = 0x04, /* The value is stored as unsigned data with the
                             * specified number of bytes. */
    DW_EH_PE_signed = 0x08, /* A signed number. The size is determined by
                             * whether this is a 32-bit or 64-bit address space */
    DW_EH_PE_sleb128 = 0x09, /* A signed LEB128. */
    DW_EH_PE_sdata2 = 0x0a,
    DW_EH_PE_sdata4 = 0x0b,
    DW_EH_PE_sdata8 = 0x0c, /* The value is stored as signed data with the
                             * specified number of bytes. */

    // In addition the above basic encodings, there are modifiers.

    DW_EH_PE_pcrel = 0x10, // Value is PC relative.

    // We currently don't support the following once.
    DW_EH_PE_textrel = 0x20, // Value is text relative.
    DW_EH_PE_datarel = 0x30, // Value is data relative.
    DW_EH_PE_funcrel = 0x40, // Value is relative to start of function.
    DW_EH_PE_aligned = 0x50, /* Value is aligned: padding bytes are inserted as
                              * required to make value be naturally aligned. */
    DW_EH_PE_indirect = 0x80 /* This is actually the address of the real value. */
};

// Parse the CIE and return the type of encoding used by FDE
static DW_EH_PE parseCIE(const uint8_t *Addr, const uint8_t *End)
{
    // http://www.airs.com/blog/archives/460
    // Length (4 bytes)
    uint32_t cie_size = *(const uint32_t*)Addr;
    const uint8_t *cie_addr = Addr + 4;
    const uint8_t *p = cie_addr;
    const uint8_t *cie_end = cie_addr + cie_size;
    assert(cie_end <= End);
    // Check this is an CIE record (CIE ID: 4 bytes)
    assert(*(const uint32_t*)cie_addr == 0);
    p += 4;
    // Check CIE version (1 byte)
    uint8_t cie_version = *p;
    assert(cie_version == 1 || cie_version == 3);
    p++;
    // Augmentation String (NUL terminate)
    const char *augmentation = (const char*)p;
    size_t augmentation_len = strlen(augmentation);
    // Assume there's no EH Data field, which exist when the augmentation
    // string has "eh" in it.
    p += augmentation_len + 1;
    // Code Alignment Factor (1 byte)
    // should always be 1 on x86, 4 on PPC, etc.
    // (used for DW_CFA_advance_loc / not used here)
    //assert(*p == 1);
    p++;
    // Data Alignment Factor (LEB128)
    assert(cie_end >= p);
    p = consume_leb128(p, cie_end);
    // return address register
    if (cie_version == 1) {
        p++;
    }
    else {
        p = consume_leb128(p, cie_end);
    }
    // Now it's the augmentation data. which may have the information we
    // are interested in...
    for (const char *augp = augmentation;;augp++) {
        switch (*augp) {
        case 'z':
            // Augmentation Length
            p = consume_leb128(p, cie_end);
            break;
        case 'L':
            // LSDA encoding
            p++;
            break;
        case 'R':
            // .... the only one we care about ....
            return static_cast<DW_EH_PE>(*p);
        case 'P': {
            // Personality data
            // Encoding
            auto encoding = static_cast<DW_EH_PE>(*p);
            p++;
            // Personality function
            switch (encoding & 0xf) {
            case DW_EH_PE_uleb128:
            case DW_EH_PE_sleb128:
                p = consume_leb128(p, cie_end);
                break;
            case DW_EH_PE_udata2:
            case DW_EH_PE_sdata2:
                p += 2;
                break;
            case DW_EH_PE_udata4:
            case DW_EH_PE_sdata4:
                p += 4;
                break;
            case DW_EH_PE_udata8:
            case DW_EH_PE_sdata8:
                p += 8;
                break;
            case DW_EH_PE_signed:
                p += sizeof(void*);
                break;
            default:
                if (encoding == DW_EH_PE_absptr || encoding == DW_EH_PE_omit) {
                    p += sizeof(void*);
                }
                else {
                    assert(0 && "Invalid personality encoding.");
                }
                break;
            }
        }
            break;
        default:
            continue;
        }
        assert(cie_end >= p);
    }
    return DW_EH_PE_absptr;
}

void RTDyldMemoryManagerUnix::registerEHFrames(uint8_t *Addr,
                                               uint64_t LoadAddr,
                                               size_t Size)
{
#ifndef _CPU_ARM_
    // System unwinder
    // Linux uses setjmp/longjmp exception handling on ARM.
    __register_frame(Addr);
#endif
    // Our unwinder
    unw_dyn_info_t *di = new unw_dyn_info_t;
    // In a shared library, this is set to the address of the PLT.
    // For us, just put 0 to emulate a static library. This field does
    // not seem to be used on our supported architectures.
    di->gp = 0;
    // I'm not a great fan of the naming of this constant, but it means the
    // right thing, which is a table of FDEs and ips.
    di->format = UNW_INFO_FORMAT_IP_OFFSET;
    di->u.ti.name_ptr = 0;
    di->u.ti.segbase = (unw_word_t)Addr;
    // Now first count the number of FDEs
    size_t nentries = 0;
    processFDEs((char*)Addr, Size, [&](const char*){ nentries++; });

    uintptr_t start_ip = (uintptr_t)-1;
    uintptr_t end_ip = 0;

    // Then allocate a table and fill in the information
    // While we're at it, also record the start_ip and size,
    // which we fill in the table
    unw_table_entry *table = new unw_table_entry[nentries];
    std::vector<uintptr_t> start_ips(nentries);
    size_t cur_entry = 0;
    // Cache the previously parsed CIE entry so that we can support multiple
    // CIE's (may not happen) without parsing it everytime.
    const uint8_t *cur_cie = nullptr;
    DW_EH_PE encoding = DW_EH_PE_omit;
    processFDEs((char*)Addr, Size, [&](const char *Entry) {
            // Skip Length (4bytes) and CIE offset (4bytes)
            uint32_t fde_size = *(const uint32_t*)Entry;
            uint32_t cie_id = ((const uint32_t*)Entry)[1];
            const uint8_t *cie_addr = (const uint8_t*)(Entry + 4 - cie_id);
            if (cie_addr != cur_cie)
                encoding = parseCIE(cie_addr, Addr + Size);
            const uint8_t *fde_end = (const uint8_t*)(Entry + 4 + fde_size);
            const uint8_t *EntryPtr = (const uint8_t*)(Entry + 8);
            uintptr_t start = 0;
            uintptr_t size = 0;
            // The next two fields are address and size of the PC range
            // covered by this FDE.
            if (encoding == DW_EH_PE_absptr || encoding == DW_EH_PE_omit) {
                assert(fde_size >= 2 * sizeof(void*) + 4);
                start = *(const uintptr_t*)EntryPtr;
                size = *(const uintptr_t*)(EntryPtr + sizeof(void*));
            }
            else {
                uintptr_t baseptr = (uintptr_t)EntryPtr;
                // Only support pcrel for now...
                assert((encoding & 0xf0) == 0x10 &&
                       "Only pcrel mode is supported");
                switch (encoding & 0xf) {
                case DW_EH_PE_uleb128:
                    start = baseptr + parse_leb128<uintptr_t>(EntryPtr, fde_end);
                    size = parse_leb128<uintptr_t>(EntryPtr, fde_end);
                    break;
                case DW_EH_PE_udata2:
                    assert(fde_size >= 2 * 2 + 4);
                    start = baseptr + ((const uint16_t*)EntryPtr)[0];
                    size = ((const uint16_t*)EntryPtr)[1];
                    break;
                case DW_EH_PE_udata4:
                    assert(fde_size >= 2 * 4 + 4);
                    start = baseptr + ((const uint32_t*)EntryPtr)[0];
                    size = ((const uint32_t*)EntryPtr)[1];
                    break;
                case DW_EH_PE_udata8:
                    assert(fde_size >= 2 * 8 + 4);
                    start = uintptr_t(baseptr + ((const uint64_t*)EntryPtr)[0]);
                    size = uintptr_t(((const uint64_t*)EntryPtr)[1]);
                    break;
                case DW_EH_PE_signed:
                    assert(fde_size >= 2 * sizeof(void*) + 4);
                    start = baseptr + ((const intptr_t*)EntryPtr)[0];
                    size = ((const intptr_t*)EntryPtr)[1];
                    break;
                case DW_EH_PE_sleb128:
                    start = baseptr + parse_leb128<intptr_t>(EntryPtr, fde_end);
                    size = parse_leb128<intptr_t>(EntryPtr, fde_end);
                    break;
                case DW_EH_PE_sdata2:
                    assert(fde_size >= 2 * 2 + 4);
                    start = baseptr + ((const int16_t*)EntryPtr)[0];
                    size = ((const int16_t*)EntryPtr)[1];
                    break;
                case DW_EH_PE_sdata4:
                    assert(fde_size >= 2 * 4 + 4);
                    start = baseptr + ((const int32_t*)EntryPtr)[0];
                    size = ((const int32_t*)EntryPtr)[1];
                    break;
                case DW_EH_PE_sdata8:
                    assert(fde_size >= 2 * 8 + 4);
                    start = uintptr_t(baseptr + ((const int64_t*)EntryPtr)[0]);
                    size = uintptr_t(((const int64_t*)EntryPtr)[1]);
                    break;
                default:
                    assert(0 && "Invalid FDE encoding.");
                    break;
                }
            }

            if (start < start_ip)
                start_ip = start;
            if (end_ip < (start + size))
                end_ip = start+size;
            table[cur_entry].fde_offset =
                safe_trunc<int32_t>((intptr_t)Entry - (intptr_t)Addr);
            start_ips[cur_entry] = start;
            cur_entry++;
        });
    for (size_t i = 0;i < nentries;i++) {
        table[i].start_ip_offset =
            safe_trunc<int32_t>((intptr_t)start_ips[i] - (intptr_t)start_ip);
    }
    assert(end_ip != 0);

    di->u.ti.table_len = nentries;
    di->u.ti.table_data = (unw_word_t*)table;
    di->start_ip = start_ip;
    di->end_ip = end_ip;

    _U_dyn_register(di);
}

void RTDyldMemoryManagerUnix::deregisterEHFrames(uint8_t *Addr,
                                           uint64_t LoadAddr,
                                           size_t Size)
{
#ifndef _CPU_ARM_
    __deregister_frame(Addr);
#endif
    // Deregistering with our unwinder requires a lookup table to find the
    // the allocated entry above (or we could look in libunwind's internal
    // data structures).
}

RTDyldMemoryManager* createRTDyldMemoryManagerUnix()
{
    return new RTDyldMemoryManagerUnix();
}

#endif

#ifdef USE_MCJIT
extern "C"
uint64_t jl_getUnwindInfo(uint64_t dwAddr)
{
    // Might be called from unmanaged thread
    std::map<size_t, ObjectInfo, revcomp> &objmap = jl_jit_events->getObjectMap();
    std::map<size_t, ObjectInfo, revcomp>::iterator it = objmap.lower_bound(dwAddr);
    uint64_t ipstart = 0; // ip of the start of the section (if found)
    if (it != objmap.end() && dwAddr < it->first + it->second.SectionSize) {
        ipstart = (uint64_t)(intptr_t)(*it).first;
    }
    uv_rwlock_rdunlock(&threadsafe);
    return ipstart;
}
#else
extern "C"
uint64_t jl_getUnwindInfo(uint64_t dwAddr)
{
    // Might be called from unmanaged thread
    std::map<size_t, FuncInfo, revcomp> &info = jl_jit_events->getMap();
    std::map<size_t, FuncInfo, revcomp>::iterator it = info.lower_bound(dwAddr);
    uint64_t ipstart = 0; // ip of the first instruction in the function (if found)
    if (it != info.end() && (intptr_t)(*it).first + (*it).second.lengthAdr > dwAddr) {
        ipstart = (uint64_t)(intptr_t)(*it).first;
    }
    uv_rwlock_rdunlock(&threadsafe);
    return ipstart;
}
#endif


#if defined(_OS_WINDOWS_) && !defined(USE_MCJIT) && defined(_CPU_X86_64_)
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
    virtual uint8_t *allocateStub(const GlobalValue *F, unsigned StubSize, unsigned Alignment)
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
    virtual uint8_t *startExceptionTable(const Function *F,
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
JITMemoryManager *createJITMemoryManagerWin()
{
    return new JITMemoryManagerWin();
}
#endif
