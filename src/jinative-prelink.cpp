// This file is a part of Julia. License is MIT: https://julialang.org/license

// Links the objects of an image into the native code section of a .ji file
// (see jinative.h), with JITLink used as a static linker.
//
// Every object becomes one LinkGraph. All graphs are laid out in one address
// range that starts at a made-up base, and symbols that no graph defines get
// made-up addresses far away from it. After the fixups are applied, anything
// that depends on where the image or an import is loaded is a 64-bit pointer
// slot, which is recorded as a rebase or a bind. Everything else is relative
// and was resolved here.

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/ExecutionEngine/JITLink/JITLink.h>
#include <llvm/ExecutionEngine/JITLink/JITLinkMemoryManager.h>
#include <llvm/ExecutionEngine/JITLink/aarch64.h>
#include <llvm/ExecutionEngine/JITLink/x86_64.h>
#include <llvm/Object/Archive.h>
#include <llvm/Support/Endian.h>
#include <llvm/Support/EndianStream.h>
#include <llvm/Support/LEB128.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/SHA256.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/Triple.h>

#include <map>
#include <string>
#include <vector>

#include "jinative.h"

using namespace llvm;
using namespace llvm::jitlink;

namespace {

static const uint64_t VBase = 0x1000000000ULL; // above 4 GiB so that no address fits 32 bits
static const uint64_t ImportBase = 0x7000000000000000ULL;
static const uint64_t ImportStride = 0x10000;

enum Region { RX, RO, RELRO, RW, NRegions };

// BasicLayout groups sections by protection, so a protection that no section
// has otherwise marks the ones that are read-only once the loader fixed them up.
static const orc::MemProt RelroProt = orc::MemProt::Read | orc::MemProt::Write | orc::MemProt::Exec;
static bool isExec(orc::MemProt Prot)
{
    return Prot != RelroProt && (Prot & orc::MemProt::Exec) != orc::MemProt::None;
}

struct Prelinker;

struct PendingAlloc {
    LinkGraph *G;
    std::unique_ptr<BasicLayout> BL;
    JITLinkMemoryManager::OnAllocatedFunction OnAllocated;
};

struct PendingLookup {
    JITLinkContext::LookupMap Symbols;
    std::unique_ptr<JITLinkAsyncLookupContinuation> LC;
};

class ImageAlloc : public JITLinkMemoryManager::InFlightAlloc {
public:
    void abandon(OnAbandonedFunction OnAbandoned) override { OnAbandoned(Error::success()); }
    void finalize(OnFinalizedFunction OnFinalized) override
    {
        OnFinalized(JITLinkMemoryManager::FinalizedAlloc(orc::ExecutorAddr(VBase)));
    }
};

class ImageMemoryManager : public JITLinkMemoryManager {
public:
    ImageMemoryManager(Prelinker &P) : P(P) {}
    void allocate(const JITLinkDylib *JD, LinkGraph &G, OnAllocatedFunction OnAllocated) override;
    void deallocate(std::vector<FinalizedAlloc> Allocs, OnDeallocatedFunction OnDeallocated) override
    {
        for (auto &A : Allocs)
            A.release();
        OnDeallocated(Error::success());
    }
    Prelinker &P;
};

struct Prelinker {
    Triple TT;
    uint64_t PageSize;
    std::string Err;
    ImageMemoryManager MemMgr{*this};
    std::vector<PendingAlloc> Allocs;
    std::vector<PendingLookup> Lookups;
    std::vector<char> Image; // header page and the three segments
    uint64_t RegionOff[NRegions] = {}, RegionSize[NRegions] = {};

    StringMap<uint64_t> Exports; // symbols visible to the other graphs
    StringMap<uint32_t> ImportIdx;
    std::vector<std::string> Imports;
    std::vector<uint64_t> Rebases;
    std::vector<jl_native_bind_t> Binds;
    struct Sym { uint64_t Addr; uint32_t Size; std::string Name; };
    std::vector<Sym> Syms;
    std::vector<jl_native_object_t> Objs;
    struct DebugSec { std::string Name; std::vector<char> Data; std::vector<uint32_t> Slides; };
    std::vector<DebugSec> Debug;
    unsigned NRelaxed = 0, NStubCalls = 0, NDead = 0;
    uint64_t ZeroFill = 0;

    void fail(const Twine &Msg)
    {
        if (Err.empty())
            Err = Msg.str();
    }
    bool inImage(uint64_t Addr) const { return Addr >= VBase && Addr < VBase + Image.size(); }
    StringRef demangle(StringRef Name) const
    {
        if (TT.isOSBinFormatMachO())
            Name.consume_front("_");
        return Name;
    }

    void layout();
    void resolve();
    Error markRelocatedSectionsWritable(LinkGraph &G);
    Error relaxStubCalls(LinkGraph &G);
    Error recordFixups(LinkGraph &G);
    bool isPointer64(Edge::Kind K) const;
    bool isAbsolute(Edge::Kind K) const;
    void write(SmallVectorImpl<char> &Out);
};

class Context : public JITLinkContext {
public:
    Context(Prelinker &P) : JITLinkContext(nullptr), P(P) {}
    JITLinkMemoryManager &getMemoryManager() override { return P.MemMgr; }
    void notifyFailed(Error E) override { P.fail(toString(std::move(E))); }
    void lookup(const LookupMap &Symbols, std::unique_ptr<JITLinkAsyncLookupContinuation> LC) override
    {
        P.Lookups.push_back({Symbols, std::move(LC)});
    }
    Error notifyResolved(LinkGraph &G) override
    {
        for (auto *Sym : G.defined_symbols()) {
            if (!Sym->hasName())
                continue;
            if (Sym->getScope() != Scope::Local) {
                auto R = P.Exports.try_emplace(*Sym->getName(), Sym->getAddress().getValue());
                if (!R.second && Sym->getLinkage() == Linkage::Strong)
                    return make_error<StringError>("duplicate symbol " + *Sym->getName(),
                                                   inconvertibleErrorCode());
            }
            if (Sym->isCallable() && Sym->getSize() != 0)
                P.Syms.push_back({Sym->getAddress().getValue(), (uint32_t)Sym->getSize(),
                                  P.demangle(*Sym->getName()).str()});
        }
        return Error::success();
    }
    void notifyFinalized(JITLinkMemoryManager::FinalizedAlloc Alloc) override { Alloc.release(); }
    Error modifyPassConfig(LinkGraph &G, PassConfiguration &Config) override
    {
        Config.PostPrunePasses.push_back([this](LinkGraph &G) { return P.markRelocatedSectionsWritable(G); });
        Config.PreFixupPasses.push_back([this](LinkGraph &G) { return P.relaxStubCalls(G); });
        Config.PostFixupPasses.push_back([this](LinkGraph &G) { return P.recordFixups(G); });
        return Error::success();
    }
    Prelinker &P;
};

void ImageMemoryManager::allocate(const JITLinkDylib *JD, LinkGraph &G, OnAllocatedFunction OnAllocated)
{
    P.Allocs.push_back({&G, std::make_unique<BasicLayout>(G), std::move(OnAllocated)});
}

bool Prelinker::isPointer64(Edge::Kind K) const
{
    if (TT.isAArch64())
        return K == aarch64::Pointer64;
    return K == x86_64::Pointer64;
}

// Kinds whose value depends on the load address but cannot be fixed up at load time
bool Prelinker::isAbsolute(Edge::Kind K) const
{
    if (TT.isAArch64())
        return K == aarch64::Pointer64Authenticated || K == aarch64::Pointer32 ||
               K == aarch64::MoveWide16;
    return K == x86_64::Pointer32 || K == x86_64::Pointer32Signed || K == x86_64::Pointer16 ||
           K == x86_64::Pointer8;
}

// A slot that is fixed up at load time has to be writable then. Sections that
// are otherwise read-only go to a segment of their own.
Error Prelinker::markRelocatedSectionsWritable(LinkGraph &G)
{
    for (auto &Sec : G.sections()) {
        if (Sec.getMemLifetime() == orc::MemLifetime::NoAlloc)
            continue;
        auto Prot = Sec.getMemProt();
        if ((Prot & orc::MemProt::Write) != orc::MemProt::None)
            continue;
        bool HasPointer = false;
        for (auto *B : Sec.blocks()) {
            for (auto &E : B->edges()) {
                if (isPointer64(E.getKind())) {
                    HasPointer = true;
                    break;
                }
            }
            if (HasPointer)
                break;
        }
        if (!HasPointer)
            continue;
        // JITLink puts the GOT of aarch64 next to the code
        if ((Prot & orc::MemProt::Exec) != orc::MemProt::None && Sec.getName() != "$__GOT")
            return make_error<StringError>("pointer in executable section " + Sec.getName() +
                                           " of " + G.getName(), inconvertibleErrorCode());
        Sec.setMemProt(RelroProt);
    }
    return Error::success();
}

// References to a symbol of another object go through the GOT, and calls through
// a stub, since the other graph was not laid out when this one was built. Now
// that it is, refer to the symbol directly, as a linker that sees all objects
// at once does.
Error Prelinker::relaxStubCalls(LinkGraph &G)
{
    if (!TT.isAArch64())
        return Error::success(); // x86_64 has this as a default pass
    Section *Stubs = G.findSectionByName(aarch64::PLTTableManager::getSectionName());
    Section *GOT = G.findSectionByName(aarch64::GOTTableManager::getSectionName());
    if (!GOT)
        return Error::success();
    // The in-image address that a GOT entry holds, if any
    auto gotTarget = [&](Symbol &Ptr, Symbol *&Target, int64_t &Addend) {
        if (!Ptr.isDefined() || &Ptr.getBlock().getSection() != GOT || Ptr.getBlock().edges_size() != 1)
            return false;
        Edge &PtrEdge = *Ptr.getBlock().edges().begin();
        Target = &PtrEdge.getTarget();
        Addend = PtrEdge.getAddend();
        return isPointer64(PtrEdge.getKind()) && inImage(Target->getAddress().getValue() + Addend);
    };
    auto instr = [&](Block &B, Edge &E) {
        return support::endian::read32le(B.getContent().data() + E.getOffset());
    };
    // GOT entries that are only loaded with `adrp` + `ldr` can be computed with `adrp` + `add`
    DenseMap<Block*, bool> Direct;
    for (auto &Sec : G.sections()) {
        if (Sec.getMemLifetime() == orc::MemLifetime::NoAlloc || &Sec == Stubs)
            continue;
        for (auto *B : Sec.blocks()) {
            for (auto &E : B->edges()) {
                Symbol &T = E.getTarget();
                if (E.getKind() < Edge::FirstRelocation || !T.isDefined() || &T.getBlock().getSection() != GOT)
                    continue;
                bool OK = E.getAddend() == 0 && T.getOffset() == 0 &&
                    ((E.getKind() == aarch64::Page21 && (instr(*B, E) & 0x9f000000) == 0x90000000) ||
                     (E.getKind() == aarch64::PageOffset12 && (instr(*B, E) & 0xffc00000) == 0xf9400000));
                auto R = Direct.try_emplace(&T.getBlock(), OK);
                if (!OK)
                    R.first->second = false;
            }
        }
    }
    for (auto &Sec : G.sections()) {
        if (Sec.getMemLifetime() == orc::MemLifetime::NoAlloc || &Sec == Stubs)
            continue;
        bool Exec = isExec(Sec.getMemProt());
        for (auto *B : Sec.blocks()) {
            for (auto &E : B->edges()) {
                if (E.getKind() < Edge::FirstRelocation || !E.getTarget().isDefined())
                    continue;
                Block &TB = E.getTarget().getBlock();
                Symbol *Target;
                int64_t Addend;
                if (&TB.getSection() == GOT) {
                    if (!Direct.lookup(&TB) || !gotTarget(E.getTarget(), Target, Addend))
                        continue;
                    if (E.getKind() == aarch64::PageOffset12) {
                        // ldr xD, [xN, #off] -> add xD, xN, #off
                        uint32_t I = instr(*B, E);
                        support::endian::write32le(B->getMutableContent(G).data() + E.getOffset(),
                                                   0x91000000 | (I & 0x3ff));
                    }
                    E.setTarget(*Target);
                    E.setAddend(Addend);
                    NRelaxed++;
                }
                else if (Exec && Stubs && &TB.getSection() == Stubs && E.getKind() == aarch64::Branch26PCRel) {
                    // the stub is `adrp` + `ldr` of a GOT entry, then `br`
                    if (TB.edges_size() != 2)
                        continue;
                    Symbol &Ptr = TB.edges().begin()->getTarget();
                    if (&std::next(TB.edges().begin())->getTarget() != &Ptr || !gotTarget(Ptr, Target, Addend)) {
                        NStubCalls++;
                        continue;
                    }
                    int64_t Delta = Target->getAddress().getValue() + Addend -
                                    (B->getAddress().getValue() + E.getOffset());
                    if (!isInt<28>(Delta)) {
                        NStubCalls++;
                        continue;
                    }
                    E.setTarget(*Target);
                    E.setAddend(E.getAddend() + Addend);
                    NRelaxed++;
                }
            }
        }
    }
    return Error::success();
}

Error Prelinker::recordFixups(LinkGraph &G)
{
    auto error = [&](const Twine &Msg, Block &B, Edge &E) {
        return make_error<StringError>(Msg + ": " + G.getEdgeKindName(E.getKind()) + " edge in " +
                                       B.getSection().getName() + " of " + G.getName() + " to " +
                                       (E.getTarget().hasName() ? *E.getTarget().getName() : "<anonymous>"),
                                       inconvertibleErrorCode());
    };
    jl_native_object_t UW = {UINT64_MAX, 0, 0, 0, 0, 0, (uint32_t)Debug.size(), 0};
    // Stubs and GOT entries that nothing refers to any more need no fixup
    Section *Stubs = G.findSectionByName("$__STUBS");
    Section *GOT = G.findSectionByName("$__GOT");
    DenseSet<Block*> Used;
    for (int Pass = 0; Pass < 2; Pass++) { // stubs first, then the GOT entries of the used ones
        for (auto &Sec : G.sections()) {
            if (Sec.getMemLifetime() == orc::MemLifetime::NoAlloc || (&Sec == Stubs) != (Pass == 1))
                continue;
            for (auto *B : Sec.blocks()) {
                if (Pass == 1 && !Used.count(B))
                    continue;
                for (auto &E : B->edges())
                    if (E.getTarget().isDefined())
                        Used.insert(&E.getTarget().getBlock());
            }
        }
    }
    for (auto &Sec : G.sections()) {
        if (Sec.getMemLifetime() == orc::MemLifetime::NoAlloc)
            continue;
        bool Exec = isExec(Sec.getMemProt());
        bool Writable = (Sec.getMemProt() & orc::MemProt::Write) != orc::MemProt::None;
        if (Exec && !Sec.empty()) {
            SectionRange R(Sec);
            UW.text_start = std::min(UW.text_start, R.getStart().getValue() - VBase);
            UW.text_end = std::max(UW.text_end, R.getEnd().getValue() - VBase);
        }
        for (auto *B : Sec.blocks()) {
            if ((&Sec == GOT || &Sec == Stubs) && !Used.count(B)) {
                NDead++;
                continue;
            }
            for (auto &E : B->edges()) {
                if (E.getKind() < Edge::FirstRelocation)
                    continue;
                Symbol &T = E.getTarget();
                uint64_t TAddr = T.getAddress().getValue();
                bool Import = !inImage(TAddr);
                if (!isPointer64(E.getKind())) {
                    // relative to the image or a constant, as long as the target is in it
                    if (isAbsolute(E.getKind()))
                        return error("absolute relocation that the loader cannot apply", *B, E);
                    if (Import && !(T.isAbsolute() && TAddr == VBase))
                        return error("relative relocation against an import", *B, E);
                    continue;
                }
                if (!Writable)
                    return error("pointer outside of the data segment", *B, E);
                uint64_t Slot = B->getAddress().getValue() + E.getOffset() - VBase;
                char *P = Image.data() + Slot;
                if (Import) {
                    if (!T.hasName())
                        return error("pointer to an unnamed absolute address", *B, E);
                    uint32_t Idx = ImportIdx.lookup(*T.getName());
                    if (Idx == 0)
                        return error("pointer to an unresolved import", *B, E);
                    Binds.push_back({Slot, E.getAddend(), Idx - 1, 0});
                    support::endian::write64le(P, 0);
                }
                else {
                    Rebases.push_back(Slot);
                    support::endian::write64le(P, TAddr + E.getAddend() - VBase);
                }
            }
        }
    }
    auto section = [&](StringRef Name, uint64_t &Off, uint64_t &Size) {
        Section *Sec = G.findSectionByName(Name);
        if (!Sec || Sec->empty())
            return;
        SectionRange R(*Sec);
        Off = R.getStart().getValue() - VBase;
        Size = R.getSize();
    };
    if (TT.isOSBinFormatMachO()) {
        section("__TEXT,__eh_frame", UW.eh_frame, UW.eh_frame_size);
        section("__TEXT,__unwind_info", UW.unwind_info, UW.unwind_info_size);
    }
    else {
        section(".eh_frame", UW.eh_frame, UW.eh_frame_size);
    }
    // The debug info was fixed up where it is. Addresses in it are those of the
    // image at VBase, and the sections refer to each other by offset, so the
    // sections of an object are kept together and apart from other objects.
    for (auto &Sec : G.sections()) {
        if (Sec.getMemLifetime() != orc::MemLifetime::NoAlloc || Sec.empty())
            continue;
        StringRef Name = Sec.getName();
        if (!Name.consume_front("__DWARF,__") && !Name.consume_front("."))
            continue;
        if (!Name.starts_with("debug_") || Name.contains("pub"))
            continue;
        SectionRange R(Sec);
        DebugSec DS;
        DS.Name = Name == "debug_str_offs" ? "debug_str_offsets" : Name.str();
        DS.Data.assign(R.getSize(), 0);
        for (auto *B : Sec.blocks()) {
            if (B->isZeroFill())
                continue;
            uint64_t BOff = B->getAddress() - R.getStart();
            memcpy(DS.Data.data() + BOff, B->getContent().data(), B->getSize());
            for (auto &E : B->edges()) {
                if (isPointer64(E.getKind()) && inImage(E.getTarget().getAddress().getValue()))
                    DS.Slides.push_back(BOff + E.getOffset());
            }
        }
        Debug.push_back(std::move(DS));
    }
    UW.ndebug = Debug.size() - UW.debug;
    if (UW.text_end != 0)
        Objs.push_back(UW);
    return Error::success();
}

// Assign every segment of every graph a place in the image
void Prelinker::layout()
{
    struct Placement { BasicLayout::Segment *Seg; Region R; uint64_t Off; };
    std::vector<Placement> Placements;
    for (auto &A : Allocs) {
        for (auto &KV : A.BL->segments()) {
            auto &AG = KV.first;
            auto &Seg = KV.second;
            if (AG.getMemLifetime() != orc::MemLifetime::Standard) {
                fail("segment that only lives until the image is finalized in " + A.G->getName());
                return;
            }
            auto Prot = AG.getMemProt();
            Region R = Prot == RelroProt ? RELRO : isExec(Prot) ? RX :
                       (Prot & orc::MemProt::Write) != orc::MemProt::None ? RW : RO;
            ZeroFill += Seg.ZeroFillSize;
            uint64_t Off = alignTo(RegionSize[R], Seg.Alignment);
            RegionSize[R] = Off + Seg.ContentSize + Seg.ZeroFillSize;
            Placements.push_back({&Seg, R, Off});
        }
    }
    uint64_t Off = PageSize; // the header gets the first page
    for (int R = 0; R < NRegions; R++) {
        RegionOff[R] = Off;
        Off = alignTo(Off + RegionSize[R], PageSize);
    }
    Image.assign(Off, 0);
    for (auto &Pl : Placements) {
        uint64_t SegOff = RegionOff[Pl.R] + Pl.Off;
        Pl.Seg->Addr = orc::ExecutorAddr(VBase + SegOff);
        Pl.Seg->WorkingMem = Image.data() + SegOff;
    }
}

// Give every symbol that the graphs asked for an address
void Prelinker::resolve()
{
    auto Pending = std::move(Lookups);
    Lookups.clear();
    for (auto &L : Pending) {
        AsyncLookupResult Result;
        for (auto &KV : L.Symbols) {
            StringRef Name = *KV.first;
            auto It = Exports.find(Name);
            uint64_t Addr;
            if (It != Exports.end()) {
                Addr = It->second;
            }
            else {
                uint32_t &Idx = ImportIdx[Name];
                if (Idx == 0) {
                    Imports.push_back(demangle(Name).str());
                    Idx = Imports.size();
                }
                Addr = ImportBase + Idx * ImportStride;
            }
            Result[KV.first] = orc::ExecutorSymbolDef(orc::ExecutorAddr(Addr), JITSymbolFlags::Exported);
        }
        L.LC->run(std::move(Result));
    }
}

static const char Ident[] = "julia.pkgimage";
static const uint32_t CodeSignatureHeaderSize = 12 + 8 + 88 + sizeof(Ident);

// Ad-hoc code signature (an embedded signature superblob that holds only a code
// directory) over `Data`. macOS only maps a file executable if it has one.
static void writeCodeSignature(ArrayRef<char> Data, SmallVectorImpl<char> &Out)
{
    const unsigned PageShift = 12, HashSize = 32;
    uint64_t NPages = (Data.size() + (1 << PageShift) - 1) >> PageShift;
    uint32_t HashOff = 88 + sizeof(Ident);
    uint32_t CDSize = HashOff + NPages * HashSize;
    raw_svector_ostream OS(Out);
    support::endian::Writer W(OS, endianness::big);
    auto w32 = [&](uint32_t V) { W.write<uint32_t>(V); };
    auto w64 = [&](uint64_t V) { W.write<uint64_t>(V); };
    w32(0xfade0cc0); w32(12 + 8 + CDSize); w32(1); // superblob with one blob
    w32(0); w32(20); // the code directory
    w32(0xfade0c02); w32(CDSize); w32(0x20400);
    w32(0x2); // adhoc
    w32(HashOff); w32(88); // hashOffset, identOffset
    w32(0); w32(NPages); // nSpecialSlots, nCodeSlots
    w32(Data.size()); // codeLimit
    OS << (char)HashSize << (char)2 /* SHA-256 */ << (char)0 << (char)PageShift;
    w32(0); w32(0); w32(0); w32(0); // spare2, scatterOffset, teamOffset, spare3
    w64(0); // codeLimit64
    w64(0); w64(Data.size()); w64(0); // execSegBase, execSegLimit, execSegFlags
    OS.write(Ident, sizeof(Ident));
    for (uint64_t I = 0; I < NPages; I++) {
        size_t Start = I << PageShift;
        size_t N = std::min<size_t>(1 << PageShift, Data.size() - Start);
        auto H = SHA256::hash(ArrayRef<uint8_t>((const uint8_t*)Data.data() + Start, N));
        OS.write((const char*)H.data(), H.size());
    }
}

void Prelinker::write(SmallVectorImpl<char> &Out)
{
    jl_native_header_t H = {};
    memcpy(H.magic, JL_NATIVE_MAGIC, sizeof(H.magic));
    H.version = JL_NATIVE_VERSION;
    H.page_size = PageSize;
    H.vm_size = Image.size();
    H.link_base = VBase;
    H.text_off = RegionOff[RX]; H.text_size = RegionSize[RX];
    H.rodata_off = RegionOff[RO]; H.rodata_size = RegionSize[RO];
    H.relro_off = RegionOff[RELRO]; H.relro_size = RegionSize[RELRO];
    H.data_off = RegionOff[RW]; H.data_size = RegionSize[RW];

    Out.assign(Image.begin(), Image.end());
    raw_svector_ostream OS(Out);
    auto align = [&]() { OS.write_zeros(offsetToAlignment(Out.size(), Align(8))); };

    std::string Strtab(1, '\0');
    auto str = [&](StringRef S) { uint32_t O = Strtab.size(); Strtab.append(S.begin(), S.end()); Strtab.push_back('\0'); return O; };

    llvm::sort(Rebases);
    H.rebase_off = Out.size();
    H.nrebase = Rebases.size();
    uint64_t Prev = 0;
    for (uint64_t Slot : Rebases) {
        encodeULEB128(Slot - Prev, OS);
        Prev = Slot;
    }
    align();
    llvm::sort(Binds, [](const jl_native_bind_t &A, const jl_native_bind_t &B) { return A.slot < B.slot; });
    H.bind_off = Out.size();
    H.nbind = Binds.size();
    OS.write((const char*)Binds.data(), Binds.size() * sizeof(jl_native_bind_t));
    H.imports_off = Out.size();
    H.nimports = Imports.size();
    for (auto &Name : Imports)
        support::endian::Writer(OS, endianness::little).write<uint32_t>(str(Name));
    align();
    llvm::sort(Syms, [](const Sym &A, const Sym &B) { return A.Addr < B.Addr; });
    H.syms_off = Out.size();
    H.nsyms = Syms.size();
    for (auto &S : Syms) {
        jl_native_sym_t NS = {S.Addr - VBase, S.Size, str(S.Name)};
        OS.write((const char*)&NS, sizeof(NS));
    }
    // objects are not reordered, since they index the debug sections
    H.objects_off = Out.size();
    H.nobjects = Objs.size();
    OS.write((const char*)Objs.data(), Objs.size() * sizeof(jl_native_object_t));
    H.debug_off = Out.size();
    H.ndebug = Debug.size();
    size_t DebugTable = Out.size();
    OS.write_zeros(Debug.size() * sizeof(jl_native_debug_section_t));
    for (size_t I = 0; I < Debug.size(); I++) {
        jl_native_debug_section_t DS = {Out.size(), Debug[I].Data.size(), 0, str(Debug[I].Name),
                                        (uint32_t)Debug[I].Slides.size()};
        OS.write(Debug[I].Data.data(), Debug[I].Data.size());
        align();
        DS.slides_off = Out.size();
        OS.write((const char*)Debug[I].Slides.data(), Debug[I].Slides.size() * sizeof(uint32_t));
        memcpy(Out.data() + DebugTable + I * sizeof(DS), &DS, sizeof(DS));
    }
    align();
    H.strtab_off = Out.size();
    H.strtab_size = Strtab.size();
    OS << Strtab;

    auto exported = [&](StringRef Name) -> uint64_t {
        auto It = Exports.find(TT.isOSBinFormatMachO() ? ("_" + Name).str() : Name.str());
        return It == Exports.end() ? 0 : It->second - VBase;
    };
    H.image_pointers = exported("jl_image_pointers");
    H.image_coverage = exported("jl_image_coverage");

    OS.write_zeros(offsetToAlignment(Out.size(), Align(4096)));
    H.codesig_off = Out.size();
    if (TT.isOSDarwin()) {
        // the header is part of what is signed, so its size has to be known first
        uint64_t NPages = (Out.size() + 4095) >> 12;
        H.codesig_size = CodeSignatureHeaderSize + NPages * 32;
        H.file_size = H.codesig_off + H.codesig_size;
        memcpy(Out.data(), &H, sizeof(H));
        SmallVector<char, 0> Sig;
        writeCodeSignature(ArrayRef<char>(Out.data(), Out.size()), Sig);
        assert(Sig.size() == H.codesig_size);
        OS.write(Sig.data(), Sig.size());
    }
    else {
        H.file_size = Out.size();
        memcpy(Out.data(), &H, sizeof(H));
    }
}

} // namespace

// Link `Objects` (object files of the triple `TT`) into `Out`. Returns false, with
// the reason in `Err`, if they cannot be represented.
bool jl_prelink_native_image(ArrayRef<MemoryBufferRef> Objects, const Triple &TT, uint64_t PageSize,
                             SmallVectorImpl<char> &Out, std::string &Err, std::string *Stats)
{
    if (!(TT.isAArch64() || TT.getArch() == Triple::x86_64)) {
        Err = "unsupported architecture";
        return false;
    }
    Prelinker P;
    P.TT = TT;
    P.PageSize = PageSize;
    auto SSP = std::make_shared<orc::SymbolStringPool>();
    for (auto &Obj : Objects) {
        auto G = createLinkGraphFromObject(Obj, SSP);
        if (!G) {
            Err = toString(G.takeError());
            return false;
        }
        // the address that the compact unwind tables of Mach-O are relative to
        (*G)->addAbsoluteSymbol("__jitlink$libunwind_dso_base", orc::ExecutorAddr(VBase), 0,
                                Linkage::Strong, Scope::Local, true);
        link(std::move(*G), std::make_unique<Context>(P));
        if (!P.Err.empty())
            break;
    }
    if (P.Err.empty() && P.Allocs.size() != Objects.size())
        P.fail("object without content");
    if (P.Err.empty())
        P.layout();
    if (P.Err.empty()) {
        // runs each graph up to its lookup of the symbols it does not define
        auto Allocs = std::move(P.Allocs);
        for (auto &A : Allocs) {
            if (auto E = A.BL->apply()) {
                P.fail(toString(std::move(E)));
                break;
            }
            A.OnAllocated(std::make_unique<ImageAlloc>());
            if (!P.Err.empty())
                break;
        }
    }
    if (P.Err.empty())
        P.resolve(); // applies the fixups
    if (P.Err.empty() && P.Exports.count(TT.isOSBinFormatMachO() ? "_jl_image_pointers" : "jl_image_pointers") == 0)
        P.fail("jl_image_pointers is not defined");
    if (!P.Err.empty()) {
        Err = P.Err;
        return false;
    }
    P.write(Out);
    if (Stats) {
        raw_string_ostream OS(*Stats);
        OS << "objects " << Objects.size() << " text " << P.RegionSize[RX] << " rodata " << P.RegionSize[RO]
           << " relro " << P.RegionSize[RELRO] << " data " << P.RegionSize[RW] << " zerofill " << P.ZeroFill << " rebases " << P.Rebases.size() << " binds " << P.Binds.size()
           << " imports " << P.Imports.size() << " syms " << P.Syms.size() << " linked " << P.Objs.size() << " debug " << P.Debug.size()
           << " relaxed " << P.NRelaxed << " stubcalls " << P.NStubCalls << " dead " << P.NDead << " file " << Out.size();
    }
    return true;
}

#ifdef JL_PRELINK_STANDALONE
// jinative-prelink out.jln in.a|in.o...
int main(int argc, char **argv)
{
    std::vector<std::unique_ptr<MemoryBuffer>> Bufs;
    std::vector<std::unique_ptr<object::Archive>> Archives;
    std::vector<MemoryBufferRef> Objects;
    ExitOnError ExitOnErr("jinative-prelink: ");
    for (int I = 2; I < argc; I++) {
        Bufs.push_back(ExitOnErr(errorOrToExpected(MemoryBuffer::getFile(argv[I]))));
        MemoryBufferRef Ref = Bufs.back()->getMemBufferRef();
        if (Ref.getBuffer().starts_with("!<arch>\n")) {
            Archives.push_back(ExitOnErr(object::Archive::create(Ref)));
            Error E = Error::success();
            for (auto &C : Archives.back()->children(E))
                Objects.push_back(ExitOnErr(C.getMemoryBufferRef()));
            ExitOnErr(std::move(E));
        }
        else {
            Objects.push_back(Ref);
        }
    }
    Triple TT(sys::getProcessTriple());
    SmallVector<char, 0> Out;
    std::string Err, Stats;
    if (!jl_prelink_native_image(Objects, TT, TT.isAArch64() ? 16384 : 4096, Out, Err, &Stats)) {
        errs() << "error: " << Err << "\n";
        return 1;
    }
    outs() << Stats << "\n";
    std::error_code EC;
    raw_fd_ostream OS(argv[1], EC);
    OS.write(Out.data(), Out.size());
    return 0;
}
#endif
