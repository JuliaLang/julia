#include "llvm-version.h"
#include <llvm/ExecutionEngine/JITLink/JITLinkMemoryManager.h>
#include <llvm/ExecutionEngine/JITLink/JITLink.h>
#include <llvm/ADT/IntervalMap.h>
#include <llvm/Support/Process.h>
#ifdef _OS_LINUX_
#  include <sys/syscall.h>
#  include <sys/utsname.h>
#  include <sys/resource.h>
#endif
#ifndef _OS_WINDOWS_
#  include <sys/mman.h>
#  include <sys/stat.h>
#  include <fcntl.h>
#  include <unistd.h>
#  if defined(_OS_DARWIN_) && !defined(MAP_ANONYMOUS)
#    define MAP_ANONYMOUS MAP_ANON
#  endif
#endif
#ifdef _OS_FREEBSD_
#  include <sys/types.h>
#  include <sys/resource.h>
#endif

using namespace llvm;

namespace {

    static int getPosixProtectionFlags(unsigned Flags) {
    switch (Flags & llvm::sys::Memory::MF_RWE_MASK) {
    case llvm::sys::Memory::MF_READ:
        return PROT_READ;
    case llvm::sys::Memory::MF_WRITE:
        return PROT_WRITE;
    case llvm::sys::Memory::MF_READ|llvm::sys::Memory::MF_WRITE:
        return PROT_READ | PROT_WRITE;
    case llvm::sys::Memory::MF_READ|llvm::sys::Memory::MF_EXEC:
        return PROT_READ | PROT_EXEC;
    case llvm::sys::Memory::MF_READ | llvm::sys::Memory::MF_WRITE |
        llvm::sys::Memory::MF_EXEC:
        return PROT_READ | PROT_WRITE | PROT_EXEC;
    case llvm::sys::Memory::MF_EXEC:
    #if (defined(__FreeBSD__) || defined(__POWERPC__) || defined (__ppc__) || \
        defined(_POWER) || defined(_ARCH_PPC))
        // On PowerPC, having an executable page that has no read permission
        // can have unintended consequences.  The function InvalidateInstruction-
        // Cache uses instructions dcbf and icbi, both of which are treated by
        // the processor as loads.  If the page has no read permissions,
        // executing these instructions will result in a segmentation fault.
        return PROT_READ | PROT_EXEC;
    #else
        return PROT_EXEC;
    #endif
    default:
        llvm_unreachable("Illegal memory protection flag specified!");
    }
    // Provide a default return value as required by some compilers.
    return PROT_NONE;
    }

    static Expected<void *> reserve_address_space(uint64_t size) {
        auto ptr = mmap64(nullptr, size, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        if (ptr == MAP_FAILED) {
            perror("reserve_address_space");
            abort();
            return errorCodeToError(std::make_error_code(std::errc::not_enough_memory));
        }
        return ptr;
    }

    static Error unreserve_address_space(void *addr, uint64_t size) {
        auto result = munmap(addr, size);
        if (result == -1) {
            perror("unreserve_address_space");
            abort();
            return errorCodeToError(std::make_error_code(std::errc::bad_address));
        }
        return Error::success();
    }

    static Error commit(void *addr, uint64_t size, llvm::sys::Memory::ProtectionFlags flags) {
        auto result = mprotect(addr, size, getPosixProtectionFlags(flags));
        if (result == -1) {
            perror("commit");
            abort();
            return errorCodeToError(std::make_error_code(std::errc::invalid_argument));
        }
        return Error::success();
    }

    static Error uncommit(void *addr, uint64_t size) {
        auto result = mprotect(addr, size, PROT_NONE);
        if (result == -1) {
            perror("uncommit");
            abort();
            return errorCodeToError(std::make_error_code(std::errc::invalid_argument));
        }
        return Error::success();
    }

    static constexpr uint64_t RESERVED_PAGE_SIZE = 1ull << 32; // 4GB address space

    llvm::sys::Memory::ProtectionFlags prot() {
        return llvm::sys::Memory::ProtectionFlags(0);
    }

    template<typename... Flags>
    llvm::sys::Memory::ProtectionFlags prot(llvm::sys::Memory::ProtectionFlags flag, Flags... flags) {
        return llvm::sys::Memory::ProtectionFlags(flag | prot(flags...));
    }

    class Allocator {
    public:
        struct Allocation {
            sys::MemoryBlock StandardSegsMem;
            sys::MemoryBlock FinalizeSegsMem;
        };

        virtual Expected<Allocation> allocate(const jitlink::JITLinkDylib *JD, uint64_t standard, uint64_t finalize, sys::Memory::ProtectionFlags flags) = 0;
        virtual Error deallocate(sys::MemoryBlock &block) = 0;
    };

    class DefaultAllocator : public Allocator { // Stateless allocator that delegates directly to the OS
        public:
        Expected<Allocation> allocate(const jitlink::JITLinkDylib *JD, uint64_t standard, uint64_t finalize, sys::Memory::ProtectionFlags flags) override {
            (void) JD;
            std::error_code EC;
            auto Slab = sys::Memory::allocateMappedMemory(standard + finalize, nullptr,
                                                    flags, EC);
        
            if (EC) {
                return errorCodeToError(EC);
            }
            Allocation allocation;
        
            // Zero-fill the whole slab up-front.
            memset(Slab.base(), 0, Slab.allocatedSize());
        
            allocation.StandardSegsMem = {Slab.base(),
                                static_cast<size_t>(standard)};
            allocation.FinalizeSegsMem = {(void *)((char *)Slab.base() + standard),
                                static_cast<size_t>(finalize)};
            return allocation;
        }

        Error deallocate(sys::MemoryBlock &block) override {
            return errorCodeToError(sys::Memory::releaseMappedMemory(block));
        }
    };

    class SlabAllocator : public Allocator {
        typedef IntervalMap<void*, const jitlink::JITLinkDylib *> Reserver;
        class Slab {
            private:
                typedef IntervalMap<void*, bool> AllocationTracker;
            public:
                Slab(uint64_t PageSize, sys::MemoryBlock reserved) : PageSize(PageSize), reserved_block(reserved), allocation_mutex(), allocator(std::make_unique<AllocationTracker::Allocator>()), allocated(*allocator) {
                    allocated.insert(reserved.base(), static_cast<char*>(reserved.base()) + reserved.allocatedSize() - 1, false);
                }

                static Expected<std::unique_ptr<Slab>> Create(uint64_t PageSize, uint64_t SlabSize) {
                    auto addr = reserve_address_space(SlabSize);
                    if (!addr) {
                        return addr.takeError();
                    }
                    return std::make_unique<Slab>(PageSize, sys::MemoryBlock(*addr, SlabSize));
                }
                sys::MemoryBlock reserved() {
                    return reserved_block;
                }

                ~Slab() {
                    cantFail(unreserve_address_space(reserved_block.base(), reserved_block.allocatedSize()));
                }

                Expected<sys::MemoryBlock> allocate(uint64_t size, sys::Memory::ProtectionFlags flags) {
                    std::lock_guard<std::mutex> lock(allocation_mutex);
                    size = std::max(size, uint64_t(1));
                    size = ((size + PageSize - 1) / PageSize) * PageSize;
                    for (auto it = allocated.begin(), end = allocated.end(); it != end; ++it) {
                        if (!*it) {
                            //We found an unallocated block
                            //Is it big enough?
                            auto start = static_cast<char*>(it.start());
                            //IntervalMap is [start, end]
                            if (static_cast<uint64_t>((static_cast<char*>(it.stop()) + 1) - start) >= size) {
                                auto end = it.stop();
                                it.erase();
                                allocated.insert(start, start + size - 1, true);
                                if (start + size <= end) {
                                    allocated.insert(start + size, end, false);
                                }
                                if (auto err = commit(start, size, flags)) {
                                    return std::move(err);
                                }
                                return sys::MemoryBlock(start, size);
                            }
                        }
                    }
                    //We didn't find a big enough unallocated block :(
                    return errorCodeToError(std::make_error_code(std::errc::not_enough_memory));
                }

                Error deallocate(sys::MemoryBlock block) {
                    std::lock_guard<std::mutex> lock(allocation_mutex);
                    void *block_end = static_cast<char*>(block.base()) + block.allocatedSize() - 1;
                    auto it = allocated.find(block.base());
                    auto start = it.start(), end = it.stop();
                    assert(block.base() >= start && block_end <= end && "Deallocated block was not a contiguous block!");
                    assert(*it && "Attempted to deallocate an already-freed block!");
                    it.erase();
                    if (start != block.base()) {
                        allocated.insert(start, static_cast<char*>(block.base()) - 1, true);
                    }
                    allocated.insert(block.base(), block_end, false);
                    if (block_end != end) {
                        allocated.insert(static_cast<char*>(block_end) + 1, end, true);
                    }
                    if (auto err = uncommit(block.base(), block.allocatedSize())) {
                        return std::move(err);
                    }
                    return Error::success();
                }
            private:
                uint64_t PageSize;
                sys::MemoryBlock reserved_block;
                std::mutex allocation_mutex;
                std::unique_ptr<AllocationTracker::Allocator> allocator;
                AllocationTracker allocated;
        };
        public:

        SlabAllocator(uint64_t PageSize, uint64_t SlabSize) : PageSize(PageSize), SlabSize(SlabSize), allocations(), reserve_allocator(), reserved(reserve_allocator) {}

        Expected<Slab*> reserve(const jitlink::JITLinkDylib *JD) {
            std::lock_guard<std::mutex> lock(allocation_mutex);
            auto it = allocations.find(JD);
            if (it == allocations.end()) {
                auto slab = Slab::Create(PageSize, SlabSize);
                if (!slab) {
                    return slab.takeError();
                }
                auto reserved = (**slab).reserved();
                allocations[JD] = std::move(*slab);
                this->reserved.insert(reserved.base(), static_cast<char*>(reserved.base()) + reserved.allocatedSize() - 1, JD);
                it = allocations.find(JD);
                assert(it != allocations.end());
            }
            return it->second.get();
        }

        Expected<Allocation> allocate(const jitlink::JITLinkDylib *JD, uint64_t standard, uint64_t finalize, sys::Memory::ProtectionFlags flags) override {
            auto maybe_slab = reserve(JD);
            if (!maybe_slab) {
                return maybe_slab.takeError();
            }
            Allocation allocation;
            auto StandardSlab = (**maybe_slab).allocate(standard, flags);
            if (!StandardSlab) {
                return StandardSlab.takeError();
            }
            allocation.StandardSegsMem = *StandardSlab;
            memset(StandardSlab->base(), 0, StandardSlab->allocatedSize());
            auto FinalizeSlab = (**maybe_slab).allocate(finalize, flags);
            if (!FinalizeSlab) {
                return joinErrors(FinalizeSlab.takeError(), (**maybe_slab).deallocate(allocation.StandardSegsMem));
            }
            allocation.FinalizeSegsMem = *FinalizeSlab;
            memset(FinalizeSlab->base(), 0, FinalizeSlab->allocatedSize());
            return allocation;
        }

        Error deallocate(sys::MemoryBlock &block) override {
            std::lock_guard<std::mutex> lock(allocation_mutex);
            auto it = reserved.find(block.base());
            assert(it != reserved.end());
            return allocations[*it]->deallocate(std::move(block));
        }

        private:
            uint64_t PageSize;
            uint64_t SlabSize;
            std::mutex allocation_mutex;
            std::map<const jitlink::JITLinkDylib *, std::unique_ptr<Slab>> allocations;
            Reserver::Allocator reserve_allocator;
            Reserver reserved;
    };

    class JuliaJITLinkMemoryManager : public jitlink::JITLinkMemoryManager {
        class JuliaInFlightAlloc : public jitlink::JITLinkMemoryManager::InFlightAlloc {
        public:
            JuliaInFlightAlloc(JuliaJITLinkMemoryManager &MemMgr, jitlink::LinkGraph &G,
                jitlink::BasicLayout BL, sys::MemoryBlock StandardSegments, sys::MemoryBlock FinalizationSegments)
                : MemMgr(MemMgr), G(G), BL(std::move(BL)), StandardSegments(StandardSegments),
                    FinalizationSegments(FinalizationSegments) {}

            /// Called prior to finalization if the allocation should be abandoned.
            void abandon(OnAbandonedFunction OnAbandoned) override {
                OnAbandoned(joinErrors(MemMgr.allocator->deallocate(FinalizationSegments), MemMgr.allocator->deallocate(StandardSegments)));
            }

            /// Called to transfer working memory to the target and apply finalization.
            void finalize(OnFinalizedFunction OnFinalized) override {
                // Apply memory protections to all segments.
                if (auto Err = applyProtections()) {
                    OnFinalized(std::move(Err));
                    return;
                }

                // Run finalization actions.
                auto DeallocActions = orc::shared::runFinalizeActions(G.allocActions());
                if (!DeallocActions) {
                    OnFinalized(DeallocActions.takeError());
                    return;
                }

                // Release the finalize segments slab.
                if (auto err = MemMgr.allocator->deallocate(FinalizationSegments)) {
                    OnFinalized(std::move(err));
                    return;
                }

                // Continue with finalized allocation.
                OnFinalized(MemMgr.createFinalizedAlloc(std::move(StandardSegments),
                                                        std::move(*DeallocActions)));
            }
        private:
            Error applyProtections() {
                for (auto &KV : BL.segments()) {
                const auto &AG = KV.first;
                auto &Seg = KV.second;

                auto Prot = toSysMemoryProtectionFlags(AG.getMemProt());

                uint64_t SegSize =
                    alignTo(Seg.ContentSize + Seg.ZeroFillSize, MemMgr.PageSize);
                sys::MemoryBlock MB(Seg.WorkingMem, SegSize);
                if (auto EC = sys::Memory::protectMappedMemory(MB, Prot))
                    return errorCodeToError(EC);
                if (Prot & sys::Memory::MF_EXEC)
                    sys::Memory::InvalidateInstructionCache(MB.base(), MB.allocatedSize());
                }
                return Error::success();
            }
            JuliaJITLinkMemoryManager &MemMgr;
            jitlink::LinkGraph &G;
            jitlink::BasicLayout BL;
            sys::MemoryBlock StandardSegments;
            sys::MemoryBlock FinalizationSegments;
        };
    public:

        /// Attempts to auto-detect the host page size.
        /// Will preallocate and dispatch slabs of size MaxSize
        static Expected<std::unique_ptr<JITLinkMemoryManager>> Create(uint64_t MaxSize);

        /// Create an instance using the given page size.
        JuliaJITLinkMemoryManager(uint64_t PageSize, std::unique_ptr<Allocator> allocator) : PageSize(PageSize), allocator(std::move(allocator)) {}

        void allocate(const jitlink::JITLinkDylib *JD, jitlink::LinkGraph &G,
                        OnAllocatedFunction OnAllocated) override {

            // FIXME: Just check this once on startup.
            if (!isPowerOf2_64((uint64_t)PageSize)) {
                OnAllocated(make_error<StringError>("Page size is not a power of 2",
                                                    inconvertibleErrorCode()));
                return;
            }

            jitlink::BasicLayout BL(G);

            /// Scan the request and calculate the group and total sizes.
            /// Check that segment size is no larger than a page.
            auto SegsSizes = BL.getContiguousPageBasedLayoutSizes(PageSize);
            if (!SegsSizes) {
                OnAllocated(SegsSizes.takeError());
                return;
            }

            /// Check that the total size requested (including zero fill) is not larger
            /// than a size_t.
            if (SegsSizes->total() > std::numeric_limits<size_t>::max()) {
                OnAllocated(make_error<jitlink::JITLinkError>(
                    "Total requested size " + formatv("{0:x}", SegsSizes->total()) +
                    " for graph " + G.getName() + " exceeds address space"));
                return;
            }

            // Allocate one slab for the whole thing (to make sure everything is
            // in-range), then partition into standard and finalization blocks.
            //
            // FIXME: Make two separate allocations in the future to reduce
            // fragmentation: finalization segments will usually be a single page, and
            // standard segments are likely to be more than one page. Where multiple
            // allocations are in-flight at once (likely) the current approach will leave
            // a lot of single-page holes.
            auto allocation = allocator->allocate(JD, SegsSizes->StandardSegs, SegsSizes->FinalizeSegs, prot(sys::Memory::MF_READ, sys::Memory::MF_WRITE));
            if (!allocation) {
                OnAllocated(allocation.takeError());
            }

            auto NextStandardSegAddr = orc::ExecutorAddr::fromPtr(allocation->StandardSegsMem.base());
            auto NextFinalizeSegAddr = orc::ExecutorAddr::fromPtr(allocation->FinalizeSegsMem.base());

            // LLVM_DEBUG({
            //     dbgs() << "InProcessMemoryManager allocated:\n";
            //     if (SegsSizes->StandardSegs)
            //     dbgs() << formatv("  [ {0:x16} -- {1:x16} ]", NextStandardSegAddr,
            //                         NextStandardSegAddr + StandardSegsMem.allocatedSize())
            //             << " to stardard segs\n";
            //     else
            //     dbgs() << "  no standard segs\n";
            //     if (SegsSizes->FinalizeSegs)
            //     dbgs() << formatv("  [ {0:x16} -- {1:x16} ]", NextFinalizeSegAddr,
            //                         NextFinalizeSegAddr + FinalizeSegsMem.allocatedSize())
            //             << " to finalize segs\n";
            //     else
            //     dbgs() << "  no finalize segs\n";
            // });

            // Build ProtMap, assign addresses.
            for (auto &KV : BL.segments()) {
                auto &AG = KV.first;
                auto &Seg = KV.second;

                auto &SegAddr = (AG.getMemDeallocPolicy() == jitlink::MemDeallocPolicy::Standard)
                                    ? NextStandardSegAddr
                                    : NextFinalizeSegAddr;

                Seg.WorkingMem = SegAddr.toPtr<char *>();
                Seg.Addr = SegAddr;

                SegAddr += alignTo(Seg.ContentSize + Seg.ZeroFillSize, PageSize);
            }

            if (auto Err = BL.apply()) {
                OnAllocated(std::move(Err));
                return;
            }

            OnAllocated(std::make_unique<JuliaInFlightAlloc>(*this, G, std::move(BL),
                                                            std::move(allocation->StandardSegsMem),
                                                            std::move(allocation->FinalizeSegsMem)));
        }

        // Use overloads from base class.
        using JITLinkMemoryManager::allocate;

        void deallocate(std::vector<FinalizedAlloc> Allocs,
                        OnDeallocatedFunction OnDeallocated) override {
            std::vector<sys::MemoryBlock> StandardSegmentsList;
            std::vector<std::vector<orc::shared::WrapperFunctionCall>> DeallocActionsList;

            {
                std::lock_guard<std::mutex> Lock(FinalizedAllocsMutex);
                for (auto &Alloc : Allocs) {
                    auto *FA = Alloc.release().toPtr<FinalizedAllocInfo *>();
                    StandardSegmentsList.push_back(std::move(FA->StandardSegments));
                    if (!FA->DeallocActions.empty())
                        DeallocActionsList.push_back(std::move(FA->DeallocActions));
                    FA->~FinalizedAllocInfo();
                    FinalizedAllocInfos.Deallocate(FA);
                }
            }

            Error DeallocErr = Error::success();

            while (!DeallocActionsList.empty()) {
                auto &DeallocActions = DeallocActionsList.back();
                auto &StandardSegments = StandardSegmentsList.back();

                /// Run any deallocate calls.
                while (!DeallocActions.empty()) {
                if (auto Err = DeallocActions.back().runWithSPSRetErrorMerged())
                    DeallocErr = joinErrors(std::move(DeallocErr), std::move(Err));
                DeallocActions.pop_back();
                }

                /// Release the standard segments slab.
                if (auto EC = sys::Memory::releaseMappedMemory(StandardSegments))
                DeallocErr = joinErrors(std::move(DeallocErr), errorCodeToError(EC));

                DeallocActionsList.pop_back();
                StandardSegmentsList.pop_back();
            }

            OnDeallocated(std::move(DeallocErr));
        }

        // Use overloads from base class.
        using JITLinkMemoryManager::deallocate;

    private:
        struct FinalizedAllocInfo {
            sys::MemoryBlock StandardSegments;
            std::vector<orc::shared::WrapperFunctionCall> DeallocActions;
        };

        FinalizedAlloc createFinalizedAlloc(
            sys::MemoryBlock StandardSegments,
            std::vector<orc::shared::WrapperFunctionCall> DeallocActions) {
                std::lock_guard<std::mutex> Lock(FinalizedAllocsMutex);
                auto *FA = FinalizedAllocInfos.Allocate<FinalizedAllocInfo>();
                new (FA) FinalizedAllocInfo(
                    {std::move(StandardSegments), std::move(DeallocActions)});
                return FinalizedAlloc(orc::ExecutorAddr::fromPtr(FA));
            }

        uint64_t PageSize;
        std::unique_ptr<Allocator> allocator;
        std::mutex FinalizedAllocsMutex;
        RecyclingAllocator<BumpPtrAllocator, FinalizedAllocInfo> FinalizedAllocInfos;
    };

    Expected<std::unique_ptr<jitlink::JITLinkMemoryManager>> JuliaJITLinkMemoryManager::Create(uint64_t MaxSize) {
        if (auto PageSize = sys::Process::getPageSize()) {
            return std::make_unique<JuliaJITLinkMemoryManager>(*PageSize, std::make_unique<SlabAllocator>(*PageSize, MaxSize));
        } else {
            return PageSize.takeError();
        }
    }
} // namespace

Expected<std::unique_ptr<jitlink::JITLinkMemoryManager>> CreateJuliaJITLinkMemMgr() {
    return JuliaJITLinkMemoryManager::Create(RESERVED_PAGE_SIZE);
}
