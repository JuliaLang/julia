// This file is a part of Julia. License is MIT: http://julialang.org/license

#include "platform.h"

#include "llvm-version.h"
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JITEventListener.h>
#include <llvm/DebugInfo/DIContext.h>
#if JL_LLVM_VERSION >= 30700
#include <llvm/DebugInfo/DWARF/DWARFContext.h>
#include <llvm/Object/SymbolSize.h>
#endif
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/IR/Function.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/StringMap.h>
#if JL_LLVM_VERSION >= 30500
#include <llvm/IR/DebugInfo.h>
#else
#include <llvm/DebugInfo.h>
#endif
#if defined(USE_MCJIT) || defined(USE_ORCJIT)
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Mangler.h>
#if JL_LLVM_VERSION < 30600
#include <llvm/ExecutionEngine/ObjectImage.h>
#endif
#include <llvm/ExecutionEngine/RuntimeDyld.h>
#else
#include <llvm/ExecutionEngine/JITMemoryManager.h>
#endif
#include <llvm/Object/MachO.h>
#include <llvm/Object/COFF.h>
#if JL_LLVM_VERSION >= 30700
#  include <llvm/Object/ELFObjectFile.h>
#endif

#if defined(USE_MCJIT) && JL_LLVM_VERSION < 30600 && defined(_OS_DARWIN_)
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

#if JL_LLVM_VERSION >= 30500 && JL_LLVM_VERSION < 30600
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
    jl_method_instance_t *linfo;
};
#else
struct ObjectInfo {
    const object::ObjectFile *object;
    size_t SectionSize;
    ptrdiff_t slide;
#if JL_LLVM_VERSION >= 30700
    DIContext *context;
#endif
#if defined(_OS_DARWIN_) && JL_LLVM_VERSION < 30700
    const char *name;
#endif
};
#endif

// Maintain a mapping of unrealized function names -> linfo objects
// so that when we see it get emitted, we can add a link back to the linfo
// that it came from (providing name, type signature, file info, etc.)
static StringMap<jl_method_instance_t*> linfo_in_flight;
static std::string mangle(const std::string &Name, const DataLayout &DL)
{
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
void jl_add_linfo_in_flight(StringRef name, jl_method_instance_t *linfo, const DataLayout &DL)
{
    linfo_in_flight[mangle(name, DL)] = linfo;
}

#if defined(_OS_WINDOWS_)
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
        *(uint64_t*)(&catchjmp[2]) = (uint64_t)&__julia_personality;
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

#if JL_LLVM_VERSION >= 30800
struct strrefcomp {
    bool operator() (const StringRef& lhs, const StringRef& rhs) const
    {
        return lhs.compare(rhs) > 0;
    }
};
#endif

extern "C" tracer_cb jl_linfo_tracer;
static std::vector<jl_method_instance_t*> triggered_linfos;
void jl_callback_triggered_linfos(void)
{
    if (triggered_linfos.empty())
        return;
    if (jl_linfo_tracer) {
        std::vector<jl_method_instance_t*> to_process(std::move(triggered_linfos));
        for (jl_method_instance_t *linfo : to_process)
            jl_call_tracer(jl_linfo_tracer, (jl_value_t*)linfo);
    }
}

class JuliaJITEventListener: public JITEventListener
{
#ifndef USE_MCJIT
    std::map<size_t, FuncInfo, revcomp> info;
#else
    std::map<size_t, ObjectInfo, revcomp> objectmap;
    std::map<size_t, std::pair<size_t, jl_method_instance_t *>, revcomp> linfomap;
#endif

public:
    JuliaJITEventListener(){}
    virtual ~JuliaJITEventListener() {}

#ifndef USE_MCJIT
    virtual void NotifyFunctionEmitted(const Function &F, void *Code,
                                       size_t Size, const EmittedFunctionDetails &Details)
    {
        jl_ptls_t ptls = jl_get_ptls_states();
        // This function modify linfo->fptr in GC safe region.
        // This should be fine since the GC won't scan this field.
        int8_t gc_state = jl_gc_safe_enter(ptls);
        uv_rwlock_wrlock(&threadsafe);
        StringRef sName = F.getName();
        StringMap<jl_method_instance_t*>::iterator linfo_it = linfo_in_flight.find(sName);
        jl_method_instance_t *linfo = NULL;
        if (linfo_it != linfo_in_flight.end()) {
            linfo = linfo_it->second;
            linfo_in_flight.erase(linfo_it);
            if (!linfo->fptr && linfo->functionObjectsDecls.functionObject &&
                    ((Function*)linfo->functionObjectsDecls.functionObject)->getName().equals(sName)) {
                int jlcall_api = jl_jlcall_api(&F);
                if (linfo->inferred || jlcall_api != 1) {
                    linfo->jlcall_api = jlcall_api;
                    linfo->fptr = (jl_fptr_t)(uintptr_t)Code;
                }
                else {
                    linfo->unspecialized_ducttape = (jl_fptr_t)(uintptr_t)Code;
                }
            }
        }
#if defined(_OS_WINDOWS_)
        create_PRUNTIME_FUNCTION((uint8_t*)Code, Size, F.getName(), (uint8_t*)Code, Size, NULL);
#endif
        FuncInfo tmp = {&F, Size, Details.LineStarts, linfo};
        info[(size_t)(Code)] = tmp;
        uv_rwlock_wrunlock(&threadsafe);
        jl_gc_safe_leave(ptls, gc_state);
    }

    std::map<size_t, FuncInfo, revcomp>& getMap()
    {
        uv_rwlock_rdlock(&threadsafe);
        return info;
    }
#endif // ifndef USE_MCJIT

#ifdef USE_MCJIT
    jl_method_instance_t *lookupLinfo(size_t pointer)
    {
        auto linfo = linfomap.lower_bound(pointer);
        if (linfo != linfomap.end() && pointer < linfo->first + linfo->second.first)
            return linfo->second.second;
        else
            return NULL;
    }
#if JL_LLVM_VERSION >= 30600

    virtual void NotifyObjectEmitted(const object::ObjectFile &obj,
                                     const RuntimeDyld::LoadedObjectInfo &L)
    {
        return _NotifyObjectEmitted(obj,obj,L,nullptr);
    }

    virtual void _NotifyObjectEmitted(const object::ObjectFile &obj,
                                      const object::ObjectFile &debugObj,
                                      const RuntimeDyld::LoadedObjectInfo &L,
                                      RTDyldMemoryManager *memmgr)
#else
    virtual void NotifyObjectEmitted(const ObjectImage &obj)
#endif
    {
        jl_ptls_t ptls = jl_get_ptls_states();
        // This function modify linfo->fptr in GC safe region.
        // This should be fine since the GC won't scan this field.
        int8_t gc_state = jl_gc_safe_enter(ptls);
        uv_rwlock_wrlock(&threadsafe);
#if JL_LLVM_VERSION >= 30600
        object::section_iterator Section = debugObj.section_begin();
        object::section_iterator EndSection = debugObj.section_end();
#else
        object::section_iterator Section = debugObj.begin_sections();
        object::section_iterator EndSection = debugObj.end_sections();
#endif

#if JL_LLVM_VERSION >= 30800
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

#ifdef _CPU_ARM_
        // ARM does not have/use .eh_frame
        uint64_t arm_exidx_addr = 0;
        size_t arm_exidx_len = 0;
        uint64_t arm_text_addr = 0;
        size_t arm_text_len = 0;
        for (auto &section: obj.sections()) {
            bool istext = false;
            if (section.isText()) {
                istext = true;
            }
            else {
                StringRef sName;
                if (section.getName(sName))
                    continue;
                if (sName != ".ARM.exidx") {
                    continue;
                }
            }
#if JL_LLVM_VERSION >= 30800
            uint64_t loadaddr = L.getSectionLoadAddress(section);
#else
            uint64_t loadaddr = L.getSectionLoadAddress(sName);
#endif
            size_t seclen = section.getSize();
            if (istext) {
                arm_text_addr = loadaddr;
                arm_text_len = seclen;
                if (!arm_exidx_addr) {
                    continue;
                }
            }
            else {
                arm_exidx_addr = loadaddr;
                arm_exidx_len = seclen;
                if (!arm_text_addr) {
                    continue;
                }
            }
            unw_dyn_info_t *di = new unw_dyn_info_t;
            di->gp = 0;
            di->format = UNW_INFO_FORMAT_ARM_EXIDX;
            di->start_ip = (uintptr_t)arm_text_addr;
            di->end_ip = (uintptr_t)(arm_text_addr + arm_text_len);
            di->u.rti.name_ptr = 0;
            di->u.rti.table_data = arm_exidx_addr;
            di->u.rti.table_len = arm_exidx_len;
            _U_dyn_register(di);
            break;
        }
#endif

#if defined(_OS_WINDOWS_)
        uint64_t SectionAddrCheck = 0; // assert that all of the Sections are at the same location
        uint8_t *UnwindData = NULL;
#if defined(_CPU_X86_64_)
        uint64_t SectionLoadOffset = 1; // The real offset shouldn't be 1.
        uint8_t *catchjmp = NULL;
        for (const object::SymbolRef &sym_iter : debugObj.symbols()) {
            StringRef sName;
#if JL_LLVM_VERSION >= 30700
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
#if JL_LLVM_VERSION >= 30800
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
#elif JL_LLVM_VERSION >= 30700
                auto AddrOrError = sym_iter.getAddress();
                assert(AddrOrError);
                Addr = AddrOrError.get();
                sym_iter.getSection(Section);
                assert(Section != EndSection && Section->isText());
                Section->getName(sName);
                SectionAddr = Section->getAddress();
                SectionLoadAddr = L.getSectionLoadAddress(sName);
#elif JL_LLVM_VERSION >= 30600
                sym_iter.getAddress(Addr);
                sym_iter.getSection(Section);
                assert(Section != EndSection && Section->isText());
                Section->getName(sName);
                SectionAddr = Section->getAddress();
                SectionLoadAddr = L.getSectionLoadAddress(sName);
#else // JL_LLVM_VERSION >= 30500
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
#ifdef USE_ORCJIT
                if (memmgr)
                    SectionAddr =
                        (uintptr_t)lookupWriteAddressFor(memmgr,
                                                         (void*)SectionLoadAddr);
#endif
                if (SectionLoadOffset != 1)
                    assert(SectionLoadOffset == SectionAddr - SectionLoadAddr);
                else
                    SectionLoadOffset = SectionAddr - SectionLoadAddr;
            }
        }
        assert(catchjmp);
        assert(UnwindData);
        assert(SectionAddrCheck);
        assert(SectionLoadOffset != 1);
        catchjmp[SectionLoadOffset] = 0x48;
        catchjmp[SectionLoadOffset + 1] = 0xb8; // mov RAX, QWORD PTR [&__julia_personality]
        *(uint64_t*)(&catchjmp[SectionLoadOffset + 2]) =
            (uint64_t)&__julia_personality;
        catchjmp[SectionLoadOffset + 10] = 0xff;
        catchjmp[SectionLoadOffset + 11] = 0xe0; // jmp RAX
        UnwindData[SectionLoadOffset] = 0x09; // version info, UNW_FLAG_EHANDLER
        UnwindData[SectionLoadOffset + 1] = 4;    // size of prolog (bytes)
        UnwindData[SectionLoadOffset + 2] = 2;    // count of unwind codes (slots)
        UnwindData[SectionLoadOffset + 3] = 0x05; // frame register (rbp) = rsp
        UnwindData[SectionLoadOffset + 4] = 4;    // second instruction
        UnwindData[SectionLoadOffset + 5] = 0x03; // mov RBP, RSP
        UnwindData[SectionLoadOffset + 6] = 1;    // first instruction
        UnwindData[SectionLoadOffset + 7] = 0x50; // push RBP
        *(DWORD*)&UnwindData[SectionLoadOffset + 8] = (DWORD)(catchjmp - (uint8_t*)SectionAddrCheck); // relative location of catchjmp
#endif // defined(_OS_X86_64_)
#endif // defined(_OS_WINDOWS_)

#if JL_LLVM_VERSION >= 30700
        auto symbols = object::computeSymbolSizes(debugObj);
        bool first = true;
        for(const auto &sym_size : symbols) {
            const object::SymbolRef &sym_iter = sym_size.first;
#if JL_LLVM_VERSION >= 30900
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
#if JL_LLVM_VERSION >= 30800
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
#if JL_LLVM_VERSION >= 30800
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
                   (uint8_t*)(uintptr_t)Addr, (size_t)Size, sName,
                   (uint8_t*)(uintptr_t)SectionLoadAddr, (size_t)SectionSize, UnwindData);
#endif
            StringMap<jl_method_instance_t*>::iterator linfo_it = linfo_in_flight.find(sName);
            jl_method_instance_t *linfo = NULL;
            if (linfo_it != linfo_in_flight.end()) {
                linfo = linfo_it->second;
                if (linfo->compile_traced)
                    triggered_linfos.push_back(linfo);
                linfo_in_flight.erase(linfo_it);
                Function *F = (Function*)linfo->functionObjectsDecls.functionObject;
                if (!linfo->fptr && F && F->getName().equals(sName)) {
                    int jlcall_api = jl_jlcall_api(F);
                    if (linfo->inferred || jlcall_api != 1) {
                        linfo->jlcall_api = jlcall_api;
                        linfo->fptr = (jl_fptr_t)(uintptr_t)Addr;
                    }
                    else {
                        linfo->unspecialized_ducttape = (jl_fptr_t)(uintptr_t)Addr;
                    }
                }
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

#else // pre-LLVM 3.7
        uint64_t Addr;
        uint64_t Size;
        object::SymbolRef::Type SymbolType;
        StringRef sName;
        uint64_t SectionLoadAddr = 0, SectionAddr = 0;
#if JL_LLVM_VERSION < 30600
        bool isText;
#endif

#if JL_LLVM_VERSION >= 30500
        for (const object::SymbolRef &sym_iter : obj.symbols()) {
            sym_iter.getType(SymbolType);
            if (SymbolType != object::SymbolRef::ST_Function) continue;
            sym_iter.getSize(Size);
            sym_iter.getAddress(Addr);
            sym_iter.getSection(Section);
            if (Section == EndSection) continue;
#if JL_LLVM_VERSION >= 30600
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
#   if JL_LLVM_VERSION < 30600
            Addr = ((MCJIT*)jl_ExecutionEngine)->getSymbolAddress(sName, true);
            if (!Addr && sName[0] == '_') {
                Addr = ((MCJIT*)jl_ExecutionEngine)->getSymbolAddress(sName.substr(1), true);
            }
            if (!Addr) continue;
#   endif
#elif defined(_OS_WINDOWS_)
            uint64_t SectionSize = 0;
#   if JL_LLVM_VERSION >= 30600
            SectionSize = Section->getSize();
#   else
            Section->getSize(SectionSize);
#   endif
            if (SectionAddrCheck)
                assert(SectionAddrCheck == SectionLoadAddr);
            else
                SectionAddrCheck = SectionLoadAddr;
            create_PRUNTIME_FUNCTION(
                   (uint8_t*)(uintptr_t)Addr, (size_t)Size, sName,
                   (uint8_t*)(uintptr_t)SectionLoadAddr, (size_t)SectionSize, UnwindData);
#endif
            StringMap<jl_method_instance_t*>::iterator linfo_it = linfo_in_flight.find(sName);
            jl_method_instance_t *linfo = NULL;
            if (linfo_it != linfo_in_flight.end()) {
                linfo = linfo_it->second;
                linfo_in_flight.erase(linfo_it);
                Function *F = (Function*)linfo->functionObjectsDecls.functionObject;
                if (!linfo->fptr && F && F->getName().equals(sName)) {
                    int jlcall_api = jl_jlcall_api(F);
                    if (linfo->inferred || jlcall_api != 1) {
                        linfo->jlcall_api = jlcall_api;
                        linfo->fptr = (jl_fptr_t)(uintptr_t)Addr;
                    }
                    else {
                        linfo->unspecialized_ducttape = (jl_fptr_t)(uintptr_t)Addr;
                    }
                }
            }
            if (linfo)
                linfomap[Addr] = std::make_pair(Size, linfo);
            const object::ObjectFile *objfile =
#if JL_LLVM_VERSION >= 30600
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
#else //JL_LLVM_VERSION >= 30400
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
        jl_gc_safe_leave(ptls, gc_state);
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
                                         const RuntimeDyld::LoadedObjectInfo &L,
                                         RTDyldMemoryManager *memmgr)
{
    ((JuliaJITEventListener*)Listener)->_NotifyObjectEmitted(obj,debugObj,L,memmgr);
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
    if (strncmp(name, "japi1_", 6) &&
        strncmp(name, "japi3_", 6) &&
        strncmp(name, "julia_", 6) &&
        strncmp(name, "jsys1_", 6) &&
        strncmp(name, "jlsys_", 6))
        goto done;
    if (*start == '\0')
        goto done;
    while (*(--end) != '_') {
        char c = *end;
        if (c < '0' || c > '9')
            goto done;
    }
    if (end <= start)
        goto done;
    ret = (char*)malloc(end - start + 1);
    memcpy(ret, start, end - start);
    ret[end - start] = '\0';
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
    // This function is not allowed to reference any TLS variables
    // since it can be called from an unmanaged thread on OSX.
    if (!context) {
        if (demangle) {
            if ((*frames)[0].func_name != NULL) {
                char *oldname = (*frames)[0].func_name;
                (*frames)[0].func_name = jl_demangle(oldname);
                free(oldname);
            }
            else {
                // We do this to hide the jlcall wrappers when getting julia backtraces,
                // but it is still good to have them for regular lookup of C frames.
                // Technically not true, but we don't want them
                // in julia backtraces, so close enough
                (*frames)[0].fromC = 1;
            }
        }
        return 1;
    }
#if JL_LLVM_VERSION >= 30500
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
    if (n_frames == 0)
        // no line number info available in the context, return without the context
        return lookup_pointer(NULL, frames, pointer, demangle, noInline);
    if (noInline)
        n_frames = 1;
    if (n_frames > 1) {
        jl_frame_t *new_frames = (jl_frame_t*)calloc(sizeof(jl_frame_t), n_frames);
        memcpy(&new_frames[n_frames - 1], *frames, sizeof(jl_frame_t));
        free(*frames);
        *frames = new_frames;
    }
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
#if JL_LLVM_VERSION < 30500
        std::string func_name(info.getFunctionName());
#else
        std::string func_name(info.FunctionName);
#endif

        if (inlined_frame) {
            frame->inlined = 1;
            frame->fromC = fromC;
            if ((*frames)[n_frames-1].linfo) {
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
#if JL_LLVM_VERSION < 30500
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

        if (!frame->func_name ||
                func_name.compare(0, 7, "jlsysw_") == 0 ||
                func_name.compare(0, 7, "jlcall_") == 0 ||
                func_name.compare(0, 7, "jlcapi_") == 0) {
            frame->fromC = 1;
        }
    }
    return n_frames;
}

#ifdef _OS_DARWIN_
#include <mach-o/dyld.h>
#else
#define LC_UUID 0
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

static bool getObjUUID(llvm::object::MachOObjectFile *obj, uint8_t uuid[16])
{
# if JL_LLVM_VERSION >= 30700
    for (auto Load : obj->load_commands())
# else
#  if JL_LLVM_VERSION >= 30500
    uint32_t LoadCommandCount = obj->getHeader().ncmds;
#  else
    uint32_t LoadCommandCount = obj->getHeader().NumLoadCommands;
#  endif
    llvm::object::MachOObjectFile::LoadCommandInfo Load = obj->getFirstLoadCommandInfo();
    for (unsigned I = 0; ; ++I)
# endif
    {
        if (
# if JL_LLVM_VERSION >= 30500
            Load.C.cmd == LC_UUID
# else
            Load.C.Type == LC_UUID
# endif
            ) {
            memcpy(uuid, ((const MachO::uuid_command*)Load.Ptr)->uuid, 16);
            return true;
        }
# if JL_LLVM_VERSION < 30700
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

#if JL_LLVM_VERSION >= 30600
struct debug_link_info {
    StringRef filename;
    uint32_t crc32;
};
static debug_link_info getDebuglink(const object::ObjectFile &Obj)
{
    debug_link_info info = {};
    for (const object::SectionRef &Section: Obj.sections()) {
        StringRef sName;
        if (!Section.getName(sName) && sName == ".gnu_debuglink") {
            StringRef Contents;
            if (!Section.getContents(Contents)) {
                size_t length = Contents.find('\0');
                info.filename = Contents.substr(0, length);
                info.crc32 = *(const uint32_t*)Contents.substr(LLT_ALIGN(length + 1, 4), 4).data();
                break;
            }
        }
    }
    return info;
}
/*
 * crc function from http://svnweb.freebsd.org/base/head/sys/libkern/crc32.c (and lldb)
 *
 *   COPYRIGHT (C) 1986 Gary S. Brown. You may use this program, or
 *   code or tables extracted from it, as desired without restriction.
 */
static uint32_t
calc_gnu_debuglink_crc32(const void *buf, size_t size)
{
    static const uint32_t g_crc32_tab[] =
    {
        0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419, 0x706af48f,
        0xe963a535, 0x9e6495a3, 0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988,
        0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91, 0x1db71064, 0x6ab020f2,
        0xf3b97148, 0x84be41de, 0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7,
        0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec, 0x14015c4f, 0x63066cd9,
        0xfa0f3d63, 0x8d080df5, 0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172,
        0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b, 0x35b5a8fa, 0x42b2986c,
        0xdbbbc9d6, 0xacbcf940, 0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59,
        0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423,
        0xcfba9599, 0xb8bda50f, 0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
        0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d, 0x76dc4190, 0x01db7106,
        0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433,
        0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818, 0x7f6a0dbb, 0x086d3d2d,
        0x91646c97, 0xe6635c01, 0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e,
        0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457, 0x65b0d9c6, 0x12b7e950,
        0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65,
        0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2, 0x4adfa541, 0x3dd895d7,
        0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0,
        0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9, 0x5005713c, 0x270241aa,
        0xbe0b1010, 0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
        0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17, 0x2eb40d81,
        0xb7bd5c3b, 0xc0ba6cad, 0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a,
        0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683, 0xe3630b12, 0x94643b84,
        0x0d6d6a3e, 0x7a6a5aa8, 0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1,
        0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb,
        0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc,
        0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5, 0xd6d6a3e8, 0xa1d1937e,
        0x38d8c2c4, 0x4fdff252, 0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b,
        0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55,
        0x316e8eef, 0x4669be79, 0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
        0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f, 0xc5ba3bbe, 0xb2bd0b28,
        0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d,
        0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a, 0x9c0906a9, 0xeb0e363f,
        0x72076785, 0x05005713, 0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38,
        0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21, 0x86d3d2d4, 0xf1d4e242,
        0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777,
        0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c, 0x8f659eff, 0xf862ae69,
        0x616bffd3, 0x166ccf45, 0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2,
        0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db, 0xaed16a4a, 0xd9d65adc,
        0x40df0b66, 0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
        0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605, 0xcdd70693,
        0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94,
        0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d
    };
    const uint8_t *p = (const uint8_t *)buf;
    uint32_t crc;

    crc = ~0U;
    while (size--)
        crc = g_crc32_tab[(crc ^ *p++) & 0xFF] ^ (crc >> 8);
    return crc ^ ~0U;
}

#if JL_LLVM_VERSION >= 30900
static Expected<object::OwningBinary<object::ObjectFile>>
#else
static ErrorOr<object::OwningBinary<object::ObjectFile>>
#endif
openDebugInfo(StringRef debuginfopath, const debug_link_info &info)
{
    auto SplitFile = MemoryBuffer::getFile(debuginfopath);
    if (std::error_code EC = SplitFile.getError()) {
#if JL_LLVM_VERSION >= 30900
        return errorCodeToError(EC);
#else
        return EC;
#endif
    }

    uint32_t crc32 = calc_gnu_debuglink_crc32(
            SplitFile.get()->getBufferStart(),
            SplitFile.get()->getBufferSize());
    if (crc32 != info.crc32) {
#if JL_LLVM_VERSION >= 30900
        return errorCodeToError(object::object_error::arch_not_found);
#else
        return object::object_error::arch_not_found;
#endif
    }

    auto error_splitobj = object::ObjectFile::createObjectFile(
            SplitFile.get().get()->getMemBufferRef(),
            sys::fs::file_magic::unknown);
    if (!error_splitobj) {
#if JL_LLVM_VERSION >= 30900
        return error_splitobj.takeError();
#else
        return error_splitobj.getError();
#endif
    }

    // successfully validated and loaded split debug info file
    return object::OwningBinary<object::ObjectFile>(
            std::move(error_splitobj.get()),
            std::move(SplitFile.get()));
}
#endif

static uint64_t jl_sysimage_base;
static void **sysimg_fvars;
static jl_method_instance_t **sysimg_fvars_linfo;
static size_t sysimg_fvars_n;
extern "C" void jl_register_fptrs(uint64_t sysimage_base, void **fptrs, jl_method_instance_t **linfos, size_t n)
{
    jl_sysimage_base = (uintptr_t)sysimage_base;
    sysimg_fvars = fptrs;
    sysimg_fvars_linfo = linfos;
    sysimg_fvars_n = n;
}

template<typename T>
static inline void ignoreError(T &err)
{
#if JL_LLVM_VERSION >= 30900 && !defined(NDEBUG)
    consumeError(err.takeError());
#endif
}

extern "C" void jl_refresh_dbg_module_list(void);
bool jl_dylib_DI_for_fptr(size_t pointer, const llvm::object::ObjectFile **obj, llvm::DIContext **context, int64_t *slide, int64_t *section_slide,
    bool onlySysImg, bool *isSysImg, void **saddr, char **name, char **filename)
{
    *obj = NULL;
    *context = NULL;
    *slide = 0;
    *section_slide = 0;

// GOAL: Determine containing Library
// Assigning fname, fbase
#ifdef _OS_WINDOWS_
    IMAGEHLP_MODULE64 ModuleInfo;
    ModuleInfo.SizeOfStruct = sizeof(IMAGEHLP_MODULE64);
    jl_refresh_dbg_module_list();
    jl_in_stackwalk = 1;
    bool isvalid = SymGetModuleInfo64(GetCurrentProcess(), (DWORD64)pointer, &ModuleInfo);
    jl_in_stackwalk = 0;
    if (!isvalid) return false;

    StringRef fname = ModuleInfo.LoadedImageName;
    if (fname.empty()) // empirically, LoadedImageName might be missing
        fname = ModuleInfo.ImageName;
    DWORD64 fbase = ModuleInfo.BaseOfImage;
    bool insysimage = (fbase == jl_sysimage_base);
    if (isSysImg)
        *isSysImg = insysimage;
    if (onlySysImg && !insysimage) {
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
    else if (saddr) {
        *saddr = NULL;
    }

    // If we didn't find the filename before in the debug
    // info, use the dll name
    if (filename && !*filename)
        jl_copy_str(filename, fname.data());

    jl_in_stackwalk = 0;

#else // ifdef _OS_WINDOWS_
    Dl_info dlinfo;
    int dladdr_success;
    uint64_t fbase;
#ifdef __GLIBC__
    struct link_map *extra_info;
    dladdr_success = dladdr1((void*)pointer, &dlinfo, (void**)&extra_info, RTLD_DL_LINKMAP) != 0;
#else
    dladdr_success = dladdr((void*)pointer, &dlinfo) != 0;
#endif
    if (!dladdr_success || !dlinfo.dli_fname)
        return false;

#ifdef __GLIBC__
    // dlinfo.dli_fbase is not the right value for the main executable on linux
    fbase = (uintptr_t)extra_info->l_addr;
#else
    fbase = (uintptr_t)dlinfo.dli_fbase;
#endif
    StringRef fname;
    if (saddr)
        *saddr = dlinfo.dli_saddr;
    bool insysimage = (fbase == jl_sysimage_base);
    if (isSysImg)
        *isSysImg = insysimage;
    if (onlySysImg && !insysimage) {
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

    int isdarwin = 0, islinux = 0, iswindows = 0;
#if defined(_OS_DARWIN_)
    isdarwin = 1;
#elif defined(_OS_LINUX_) || defined(_OS_FREEBSD_)
    islinux = 1;
#elif defined(_OS_WINDOWS_)
    iswindows = 1;
#endif
    (void)iswindows;

#if JL_LLVM_VERSION < 30500
    if (iswindows) {
        return true;
    }
#endif

// GOAL: Read debuginfo from file
    // TODO: need read/write lock here for objfilemap synchronization
    obfiletype::iterator it = objfilemap.find(fbase);
    if (it != objfilemap.end()) {
        // Return cached value
        *obj = it->second.obj;
        *context = it->second.ctx;
        *slide = it->second.slide;
        *section_slide = it->second.section_slide;
        return true;
    }

// GOAL: Assign errorobj
    StringRef objpath;
    std::string debuginfopath;
    uint8_t uuid[16], uuid2[16];
    if (isdarwin) {
        size_t msize = (size_t)(((uint64_t)-1) - fbase);
#if JL_LLVM_VERSION >= 30600
        std::unique_ptr<MemoryBuffer> membuf = MemoryBuffer::getMemBuffer(
                StringRef((const char *)fbase, msize), "", false);
        auto origerrorobj = llvm::object::ObjectFile::createObjectFile(
            membuf->getMemBufferRef(), sys::fs::file_magic::unknown);
#elif JL_LLVM_VERSION >= 30500
        MemoryBuffer *membuf = MemoryBuffer::getMemBuffer(
            StringRef((const char *)fbase, msize), "", false);
        std::unique_ptr<MemoryBuffer> buf(membuf);
        auto origerrorobj = llvm::object::ObjectFile::createObjectFile(
            buf, sys::fs::file_magic::unknown);
#else
        MemoryBuffer *membuf = MemoryBuffer::getMemBuffer(
            StringRef((const char *)fbase, msize), "", false);
        std::unique_ptr<llvm::object::ObjectFile> origerrorobj(llvm::object::ObjectFile::createObjectFile(
            membuf));
#endif
        if (!origerrorobj) {
            objfileentry_t entry = {};
            objfilemap[fbase] = entry;
            return true;
        }

        llvm::object::MachOObjectFile *morigobj = (llvm::object::MachOObjectFile*)
#if JL_LLVM_VERSION >= 30600
            origerrorobj.get().get();
#else
            origerrorobj.get();
#endif

        // First find the uuid of the object file (we'll use this to make sure we find the
        // correct debug symbol file).
        if (!getObjUUID(morigobj, uuid)) {
            objfileentry_t entry = {};
            objfilemap[fbase] = entry;
            return true;
        }

        // On OS X debug symbols are not contained in the dynamic library.
        // For now we only support .dSYM files in the same directory
        // as the shared library. In the future we may use DBGCopyFullDSYMURLForUUID from CoreFoundation to make
        // use of spotlight to find the .dSYM file.
        size_t sep = fname.rfind('/');
        debuginfopath = fname;
        debuginfopath += ".dSYM/Contents/Resources/DWARF/";
        debuginfopath += fname.substr(sep + 1);
        objpath = debuginfopath;
    }
    else {
        // On Linux systems we need to mmap another copy because of the permissions on the mmap'ed shared library.
        // On Windows we need to mmap another copy since reading the in-memory copy seems to return object_error:unexpected_eof
        objpath = fname;
    }
#if JL_LLVM_VERSION >= 30500
    auto errorobj = llvm::object::ObjectFile::createObjectFile(objpath);
#else
    std::unique_ptr<llvm::object::ObjectFile> errorobj(llvm::object::ObjectFile::createObjectFile(objpath));
#endif

// GOAL: Assign *obj, *context, *slide (if above succeeded)
    if (errorobj) {
#if JL_LLVM_VERSION >= 30600
        auto *debugobj = errorobj->getBinary();
#else
        auto *debugobj = errorobj.get();
#endif

        if (islinux) {
#if JL_LLVM_VERSION >= 30600
            // if the file has a .gnu_debuglink section,
            // try to load its companion file instead
            // in the expected locations
            // for now, we don't support the build-id method
            debug_link_info info = getDebuglink(*debugobj);
            if (!info.filename.empty()) {
                size_t sep = fname.rfind('/');
#if JL_LLVM_VERSION >= 30900
                Expected<object::OwningBinary<object::ObjectFile>>
                    DebugInfo(errorCodeToError(std::make_error_code(std::errc::no_such_file_or_directory)));
                // Can't find a way to construct an empty Expected object
                // that can be ignored.
                ignoreError(DebugInfo);
#else
                ErrorOr<object::OwningBinary<object::ObjectFile>>
                    DebugInfo(std::errc::no_such_file_or_directory);
#endif
                if (fname.substr(sep + 1) != info.filename) {
                    debuginfopath = fname.substr(0, sep + 1);
                    debuginfopath += info.filename;
                    DebugInfo = openDebugInfo(debuginfopath, info);
                }
                if (!DebugInfo) {
                    debuginfopath = fname.substr(0, sep + 1);
                    debuginfopath += ".debug/";
                    debuginfopath += info.filename;
                    ignoreError(DebugInfo);
                    DebugInfo = openDebugInfo(debuginfopath, info);
                }
                if (!DebugInfo) {
                    debuginfopath = "/usr/lib/debug/";
                    debuginfopath += fname.substr(0, sep + 1);
                    debuginfopath += info.filename;
                    ignoreError(DebugInfo);
                    DebugInfo = openDebugInfo(debuginfopath, info);
                }
                if (DebugInfo) {
                    errorobj = std::move(DebugInfo);
                    // Yes, we've checked, and yes LLVM want us to check again.
                    assert(errorobj);
                    debugobj = errorobj->getBinary();
                }
                else {
                    ignoreError(DebugInfo);
                }
            }
#endif
        }

        if (isdarwin) {
            // verify the UUID matches
            if (!getObjUUID((llvm::object::MachOObjectFile*)debugobj, uuid2) ||
                    memcmp(uuid, uuid2, sizeof(uuid)) != 0) {
                objfileentry_t entry = {};
                objfilemap[fbase] = entry;
                return true;
            }
        }

        if (auto *OF = dyn_cast<const object::COFFObjectFile>(debugobj)) {
            assert(iswindows);
#if JL_LLVM_VERSION >= 30800
            *slide = OF->getImageBase() - fbase;
            *section_slide = 0; // Since LLVM 3.8+ addresses are adjusted correctly
#elif JL_LLVM_VERSION >= 30500
            const llvm::object::pe32plus_header *pe32plus;
            OF->getPE32PlusHeader(pe32plus);
            if (pe32plus != NULL) {
                *slide = pe32plus->ImageBase - fbase;
                *section_slide = -(int64_t)pe32plus->ImageBase;
            }
            else {
                const llvm::object::pe32_header *pe32;
                OF->getPE32Header(pe32);
                if (pe32 == NULL) {
                    objfileentry_t entry = {};
                    objfilemap[fbase] = entry;
                    return true;
                }
                else {
                    *slide = pe32->ImageBase - fbase;
                    *section_slide = -(int64_t)pe32->ImageBase;
                }
            }
#endif
        }
        else {
            *slide = -(int64_t)fbase;
        }

#if JL_LLVM_VERSION >= 30700
        *context = new DWARFContextInMemory(*debugobj);
#elif JL_LLVM_VERSION >= 30600
        *context = DIContext::getDWARFContext(*debugobj);
#else
        *context = DIContext::getDWARFContext(debugobj);
#endif
        *obj = debugobj;
#if JL_LLVM_VERSION >= 30600
        auto binary = errorobj->takeBinary();
        binary.first.release();
        binary.second.release();
#else
        errorobj.release();
#endif
    }
    else {
        // TODO: report the error instead of silently consuming it?
        //       jl_error might run into the same error again...
        ignoreError(errorobj);
    }

    // update cache
    objfileentry_t entry = {*obj, *context, *slide, *section_slide};
    objfilemap[fbase] = entry;
    return true;
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
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
        if (!saddr) {
            DWORD64 ImageBase;
            PRUNTIME_FUNCTION fn = RtlLookupFunctionEntry(pointer, &ImageBase, NULL);
            if (fn)
                saddr = (void*)(ImageBase + fn->BeginAddress);
        }
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
#if JL_LLVM_VERSION >= 30700
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
    jl_ptls_t ptls = jl_get_ptls_states();
    // Used by Gallium.jl
    const object::ObjectFile *object = NULL;
    DIContext *context;
    int64_t slide, section_slide;
    int8_t gc_state = jl_gc_safe_enter(ptls);
    if (!jl_DI_for_fptr(fptr, NULL, &slide, NULL, &object, NULL))
        if (!jl_dylib_DI_for_fptr(fptr, &object, &context, &slide, &section_slide, false, NULL, NULL, NULL, NULL)) {
            jl_gc_safe_leave(ptls, gc_state);
            return jl_nothing;
        }
    jl_gc_safe_leave(ptls, gc_state);
    if (object == NULL)
        return jl_nothing;
    return (jl_value_t*)jl_ptr_to_array_1d((jl_value_t*)jl_array_uint8_type,
        const_cast<char*>(object->getData().data()),
        object->getData().size(), false);
}

extern "C"
JL_DLLEXPORT uint64_t jl_get_section_start(uint64_t fptr)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    // Used by Gallium.jl
    int8_t gc_state = jl_gc_safe_enter(ptls);
    std::map<size_t, ObjectInfo, revcomp> &objmap = jl_jit_events->getObjectMap();
    std::map<size_t, ObjectInfo, revcomp>::iterator fit = objmap.lower_bound(fptr);

    uint64_t ret = 0;
    if (fit != objmap.end() && fptr < fit->first + fit->second.SectionSize) {
        ret = fit->first;
    }
    else {
       obfiletype::iterator objit = objfilemap.lower_bound(fptr);
       // Ideally we'd have a containment check here, but we can't really
       // get the shared library size easily.
       if (objit != objfilemap.end()) {
           ret = objit->first;
       }
    }
    uv_rwlock_rdunlock(&threadsafe);
    jl_gc_safe_leave(ptls, gc_state);
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
    if (it != info.end() && (uintptr_t)(*it).first + (*it).second.lengthAdr >= pointer) {
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
        LLVMContext &Ctx = (*it).second.func->getContext();

        DISubprogram debugscope(prev.Loc.getScope(Ctx));
        jl_copy_str(&frames[0].file_name, debugscope.getFilename().str().c_str());
        // The DISubprogram has the un-mangled name, so use that if
        // available. However, the scope need not be the current subprogram.
        if (debugscope.getName().data() != NULL) {
            jl_copy_str(&frames[0].func_name, debugscope.getName().str().c_str());
        }
        else {
            char *oldname = frames[0].func_name;
            frames[0].func_name = jl_demangle(frames[0].func_name);
            free(oldname);
        }

        // find nearest line info
        ++vit;
        while (vit != (*it).second.lines.end()) {
            if (pointer <= (*vit).Address) {
                break;
            }
            prev = *vit;
            ++vit;
        }

        // read out inlining and line number information
        int n_frames = 1;
        if (!noInline) {
            MDNode *inlinedAt = prev.Loc.getInlinedAt(Ctx);
            while (inlinedAt != NULL) {
                DebugLoc inlineloc = DebugLoc::getFromDILocation(inlinedAt);
                inlinedAt = inlineloc.getInlinedAt(Ctx);
                n_frames++;
            }
            if (n_frames > 1) {
                frames = (jl_frame_t*)calloc(sizeof(jl_frame_t), n_frames);
                memcpy(&frames[n_frames - 1], *frames_out, sizeof(jl_frame_t));
                free(*frames_out);
                *frames_out = frames;
            }
        }
        DebugLoc inlineloc = prev.Loc;
        for (int i = 0; i < n_frames; i++) {
            frames[i].inlined = i != n_frames - 1;
            frames[i].line = inlineloc.getLine();
            DISubprogram locscope(inlineloc.getScope(Ctx));
            jl_copy_str(&frames[i].file_name, locscope.getFilename().str().c_str());
            jl_copy_str(&frames[i].func_name, locscope.getName().str().c_str());
            MDNode *inlinedAt = inlineloc.getInlinedAt(Ctx);
            inlineloc = DebugLoc::getFromDILocation(inlinedAt);
        }

        uv_rwlock_rdunlock(&threadsafe);
        return n_frames;
    }
    uv_rwlock_rdunlock(&threadsafe);
#endif // USE_MCJIT
    return jl_getDylibFunctionInfo(frames_out, pointer, skipC, noInline);
}

extern "C" jl_method_instance_t *jl_gdblookuplinfo(void *p)
{
#ifndef USE_MCJIT
    std::map<size_t, FuncInfo, revcomp> &info = jl_jit_events->getMap();
    std::map<size_t, FuncInfo, revcomp>::iterator it = info.lower_bound((size_t)p);
    jl_method_instance_t *li = NULL;
    if (it != info.end() && (uintptr_t)(*it).first + (*it).second.lengthAdr >= (uintptr_t)p)
        li = (*it).second.linfo;
    uv_rwlock_rdunlock(&threadsafe);
    return li;
#else
    return jl_jit_events->lookupLinfo((size_t)p);
#endif
}

#if JL_LLVM_VERSION >= 30700 && (defined(_OS_LINUX_) || (defined(_OS_DARWIN_) && defined(LLVM_SHLIB)))
extern "C" void __register_frame(void*);
extern "C" void __deregister_frame(void*);

template <typename callback>
static void processFDEs(const char *EHFrameAddr, size_t EHFrameSize, callback f)
{
    const char *P = EHFrameAddr;
    const char *End = P + EHFrameSize;
    do {
        const char *Entry = P;
        P += 4;
        assert(P <= End);
        uint32_t Length = *(const uint32_t*)Entry;
        // Length == 0: Terminator
        if (Length == 0)
            break;
        assert(P + Length <= End);
        uint32_t Offset = *(const uint32_t*)P;
        // Offset == 0: CIE
        if (Offset != 0)
            f(Entry);
        P += Length;
    } while (P != End);
}
#endif

#if defined(_OS_DARWIN_) && JL_LLVM_VERSION >= 30700 && defined(LLVM_SHLIB)

/*
 * We use a custom unwinder, so we need to make sure that when registering dynamic
 * frames, we do so with our unwinder rather than with the system one. If LLVM is
 * statically linked everything works out fine, but if it's dynamically linked
 * it would usually pick up the system one, so we need to do the registration
 * ourselves to ensure the right one gets picked.
 */

static void (*libc_register_frame)(void*)   = NULL;
static void (*libc_deregister_frame)(void*) = NULL;

// This implementation handles frame registration for local targets.
void register_eh_frames(uint8_t *Addr, size_t Size)
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

void deregister_eh_frames(uint8_t *Addr, size_t Size)
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

#elif defined(_OS_LINUX_) && JL_LLVM_VERSION >= 30700 && \
    defined(JL_UNW_HAS_FORMAT_IP) && !defined(_CPU_ARM_)
#include <type_traits>

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

void register_eh_frames(uint8_t *Addr, size_t Size)
{
    // System unwinder
    __register_frame(Addr);
    // Our unwinder
    unw_dyn_info_t *di = new unw_dyn_info_t;
    // In a shared library, this is set to the address of the PLT.
    // For us, just put 0 to emulate a static library. This field does
    // not seem to be used on our supported architectures.
    di->gp = 0;
    // I'm not a great fan of the naming of this constant, but it means the
    // right thing, which is a table of FDEs and ips.
    di->format = UNW_INFO_FORMAT_IP_OFFSET;
    di->u.rti.name_ptr = 0;
    di->u.rti.segbase = (unw_word_t)Addr;
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
                end_ip = start + size;
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

    di->u.rti.table_len = nentries * sizeof(*table) / sizeof(unw_word_t);
    di->u.rti.table_data = (unw_word_t)table;
    di->start_ip = start_ip;
    di->end_ip = end_ip;

    _U_dyn_register(di);
}

void deregister_eh_frames(uint8_t *Addr, size_t Size)
{
    __deregister_frame(Addr);
    // Deregistering with our unwinder requires a lookup table to find the
    // the allocated entry above (or we could look in libunwind's internal
    // data structures).
}

#elif defined(_CPU_ARM_)

void register_eh_frames(uint8_t *Addr, size_t Size)
{
}

void deregister_eh_frames(uint8_t *Addr, size_t Size)
{
}

#else

void register_eh_frames(uint8_t *Addr, size_t Size)
{
}

void deregister_eh_frames(uint8_t *Addr, size_t Size)
{
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
        ipstart = (uint64_t)(uintptr_t)(*it).first;
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
    if (it != info.end() && (uintptr_t)(*it).first + (*it).second.lengthAdr > dwAddr) {
        ipstart = (uint64_t)(uintptr_t)(*it).first;
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

#if JL_LLVM_VERSION >= 30500
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
