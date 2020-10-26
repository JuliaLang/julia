// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "platform.h"

#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include "julia.h"
#include "julia_internal.h"

#ifdef _OS_LINUX_
#  include <sys/syscall.h>
#  include <sys/utsname.h>
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
#endif
#include "julia_assert.h"

namespace {

static size_t get_block_size(size_t size)
{
    return (size > jl_page_size * 256 ? LLT_ALIGN(size, jl_page_size) :
            jl_page_size * 256);
}

// Wrapper function to mmap/munmap/mprotect pages...
static void *map_anon_page(size_t size)
{
#ifdef _OS_WINDOWS_
    char *mem = (char*)VirtualAlloc(NULL, size + jl_page_size,
                                    MEM_COMMIT, PAGE_READWRITE);
    assert(mem && "Cannot allocate RW memory");
    mem = (char*)LLT_ALIGN(uintptr_t(mem), jl_page_size);
#else // _OS_WINDOWS_
    void *mem = mmap(nullptr, size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    assert(mem != MAP_FAILED && "Cannot allocate RW memory");
#endif // _OS_WINDOWS_
    return mem;
}

static void unmap_page(void *ptr, size_t size)
{
#ifdef _OS_WINDOWS_
    VirtualFree(ptr, size, MEM_DECOMMIT);
#else // _OS_WINDOWS_
    munmap(ptr, size);
#endif // _OS_WINDOWS_
}

#ifdef _OS_WINDOWS_
enum class Prot : int {
    RW = PAGE_READWRITE,
    RX = PAGE_EXECUTE,
    RO = PAGE_READONLY
};

static void protect_page(void *ptr, size_t size, Prot flags)
{
    DWORD old_prot;
    if (!VirtualProtect(ptr, size, (DWORD)flags, &old_prot)) {
        jl_safe_printf("Cannot protect page @%p of size %u to 0x%x (err 0x%x)\n",
                       ptr, (unsigned)size, (unsigned)flags,
                       (unsigned)GetLastError());
        abort();
    }
}
#else // _OS_WINDOWS_
enum class Prot : int {
    RW = PROT_READ | PROT_WRITE,
    RX = PROT_READ | PROT_EXEC,
    RO = PROT_READ
};

static void protect_page(void *ptr, size_t size, Prot flags)
{
    int ret = mprotect(ptr, size, (int)flags);
    if (ret != 0) {
        perror(__func__);
        abort();
    }
}

static bool check_fd_or_close(int fd)
{
    if (fd == -1)
        return false;
    int err = fcntl(fd, F_SETFD, FD_CLOEXEC);
    assert(err == 0);
    (void)err; // prevent compiler warning
    if (fchmod(fd, S_IRWXU) != 0 ||
        ftruncate(fd, jl_page_size) != 0) {
        close(fd);
        return false;
    }
    // This can fail due to `noexec` mount option ....
    void *ptr = mmap(nullptr, jl_page_size, PROT_READ | PROT_EXEC,
                     MAP_SHARED, fd, 0);
    if (ptr == MAP_FAILED) {
        close(fd);
        return false;
    }
    munmap(ptr, jl_page_size);
    return true;
}
#endif // _OS_WINDOWS_

static intptr_t anon_hdl = -1;

#ifdef _OS_WINDOWS_
// As far as I can tell `CreateFileMapping` cannot be resized on windows.
// Also, creating big file mapping and then map pieces of it seems to
// consume too much global resources. Therefore, we use each file mapping
// as a block on windows
static void *create_shared_map(size_t size, size_t id)
{
    void *addr = MapViewOfFile((HANDLE)id, FILE_MAP_ALL_ACCESS,
                               0, 0, size);
    assert(addr && "Cannot map RW view");
    return addr;
}

static intptr_t init_shared_map()
{
    anon_hdl = 0;
    return 0;
}

static void *alloc_shared_page(size_t size, size_t *id, bool exec)
{
    assert(size % jl_page_size == 0);
    DWORD file_mode = exec ? PAGE_EXECUTE_READWRITE : PAGE_READWRITE;
    HANDLE hdl = CreateFileMapping(INVALID_HANDLE_VALUE, NULL,
                                   file_mode, 0, size, NULL);
    *id = (size_t)hdl;
    // We set the maximum permissions for this to the maximum for this file, and then
    // VirtualProtect, such that the debugger can still access these
    // pages and set breakpoints if it wants to.
    DWORD map_mode = FILE_MAP_ALL_ACCESS | (exec ? FILE_MAP_EXECUTE : 0);
    void *addr = MapViewOfFile(hdl, map_mode, 0, 0, size);
    assert(addr && "Cannot map RO view");
    DWORD protect_mode = exec ? PAGE_EXECUTE_READ : PAGE_READONLY;
    VirtualProtect(addr, size, protect_mode, &file_mode);
    return addr;
}
#else // _OS_WINDOWS_
// For shared mapped region
static intptr_t get_anon_hdl(void)
{
    int fd = -1;

    // Linux and FreeBSD can create an anonymous fd without touching the
    // file system.
#  ifdef __NR_memfd_create
    fd = syscall(__NR_memfd_create, "julia-codegen", 0);
    if (check_fd_or_close(fd))
        return fd;
#  endif
#  ifdef _OS_FREEBSD_
    fd = shm_open(SHM_ANON, O_RDWR, S_IRWXU);
    if (check_fd_or_close(fd))
        return fd;
#  endif
    char shm_name[] = "julia-codegen-0123456789-0123456789/tmp///";
    pid_t pid = getpid();
    // `shm_open` can't be mapped exec on mac
#  ifndef _OS_DARWIN_
    do {
        snprintf(shm_name, sizeof(shm_name),
                 "julia-codegen-%d-%d", (int)pid, rand());
        fd = shm_open(shm_name, O_RDWR | O_CREAT | O_EXCL, S_IRWXU);
        if (check_fd_or_close(fd)) {
            shm_unlink(shm_name);
            return fd;
        }
    } while (errno == EEXIST);
#  endif
    FILE *tmpf = tmpfile();
    if (tmpf) {
        fd = dup(fileno(tmpf));
        fclose(tmpf);
        if (check_fd_or_close(fd)) {
            return fd;
        }
    }
    snprintf(shm_name, sizeof(shm_name),
             "/tmp/julia-codegen-%d-XXXXXX", (int)pid);
    fd = mkstemp(shm_name);
    if (check_fd_or_close(fd)) {
        unlink(shm_name);
        return fd;
    }
    return -1;
}

static size_t map_offset = 0;
// Multiple of 128MB.
// Hopefully no one will set a ulimit for this to be a problem...
static constexpr size_t map_size_inc = 128 * 1024 * 1024;
static size_t map_size = 0;
static jl_mutex_t shared_map_lock;

static void *create_shared_map(size_t size, size_t id)
{
    void *addr = mmap(nullptr, size, PROT_READ | PROT_WRITE, MAP_SHARED,
                      anon_hdl, id);
    assert(addr != MAP_FAILED && "Cannot map RW view");
    return addr;
}

static intptr_t init_shared_map()
{
    anon_hdl = get_anon_hdl();
    if (anon_hdl == -1)
        return -1;
    map_offset = 0;
    map_size = map_size_inc;
    int ret = ftruncate(anon_hdl, map_size);
    if (ret != 0) {
        perror(__func__);
        abort();
    }
    return anon_hdl;
}

static void *alloc_shared_page(size_t size, size_t *id, bool exec)
{
    assert(size % jl_page_size == 0);
    size_t off = jl_atomic_fetch_add(&map_offset, size);
    *id = off;
    if (__unlikely(off + size > map_size)) {
        JL_LOCK_NOGC(&shared_map_lock);
        size_t old_size = map_size;
        while (off + size > map_size)
            map_size += map_size_inc;
        if (old_size != map_size) {
            int ret = ftruncate(anon_hdl, map_size);
            if (ret != 0) {
                perror(__func__);
                abort();
            }
        }
        JL_UNLOCK_NOGC(&shared_map_lock);
    }
    return create_shared_map(size, off);
}
#endif // _OS_WINDOWS_

#ifdef _OS_LINUX_
// Using `/proc/self/mem`, A.K.A. Keno's remote memory manager.

ssize_t pwrite_addr(int fd, const void *buf, size_t nbyte, uintptr_t addr)
{
    static_assert(sizeof(off_t) >= 8, "off_t is smaller than 64bits");
#ifdef _P64
    const uintptr_t sign_bit = uintptr_t(1) << 63;
    if (__unlikely(sign_bit & addr)) {
        // This case should not happen with default kernel on 64bit since the address belongs
        // to kernel space (linear mapping).
        // However, it seems possible to change this at kernel compile time.

        // pwrite doesn't support offset with sign bit set but lseek does.
        // This is obviously not thread safe but none of the mem manager does anyway...
        // From the kernel code, `lseek` with `SEEK_SET` can't fail.
        // However, this can possibly confuse the glibc wrapper to think that
        // we have invalid input value. Use syscall directly to be sure.
        syscall(SYS_lseek, (long)fd, addr, (long)SEEK_SET);
        // The return value can be -1 when the glibc syscall function
        // think we have an error return with and `addr` that's too large.
        // Ignore the return value for now.
        return write(fd, buf, nbyte);
    }
#endif
    return pwrite(fd, buf, nbyte, (off_t)addr);
}

// Do not call this directly.
// Use `get_self_mem_fd` which has a guard to call this only once.
static int _init_self_mem()
{
    struct utsname kernel;
    uname(&kernel);
    int major, minor;
    if (-1 == sscanf(kernel.release, "%d.%d", &major, &minor))
        return -1;
    // Can't risk getting a memory block backed by transparent huge pages,
    // which cause the kernel to freeze on systems that have the DirtyCOW
    // mitigation patch, but are < 4.10.
    if (!(major > 4 || (major == 4 && minor >= 10)))
        return -1;
#ifdef O_CLOEXEC
    int fd = open("/proc/self/mem", O_RDWR | O_SYNC | O_CLOEXEC);
    if (fd == -1)
        return -1;
#else
    int fd = open("/proc/self/mem", O_RDWR | O_SYNC);
    if (fd == -1)
        return -1;
    fcntl(fd, F_SETFD, FD_CLOEXEC);
#endif

    // Check if we can write to a RX page
    void *test_pg = mmap(nullptr, jl_page_size, PROT_READ | PROT_EXEC,
                         MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    // We can ignore this though failure to allocate executable memory would be a bigger problem.
    assert(test_pg != MAP_FAILED && "Cannot allocate executable memory");

    const uint64_t v = 0xffff000012345678u;
    int ret = pwrite_addr(fd, (const void*)&v, sizeof(uint64_t), (uintptr_t)test_pg);
    if (ret != sizeof(uint64_t) || *(volatile uint64_t*)test_pg != v) {
        munmap(test_pg, jl_page_size);
        close(fd);
        return -1;
    }
    munmap(test_pg, jl_page_size);
    return fd;
}

static int get_self_mem_fd()
{
    static int fd = _init_self_mem();
    return fd;
}

static void write_self_mem(void *dest, void *ptr, size_t size)
{
    while (size > 0) {
        ssize_t ret = pwrite_addr(get_self_mem_fd(), ptr, size, (uintptr_t)dest);
        if ((size_t)ret == size)
            return;
        if (ret == -1 && (errno == EAGAIN || errno == EINTR))
            continue;
        assert((size_t)ret < size);
        size -= ret;
        ptr = (char*)ptr + ret;
        dest = (char*)dest + ret;
    }
}
#endif // _OS_LINUX_

using namespace llvm;

// Allocation strategies
// * For RW data, no memory protection needed, use plain memory pool.
// * For RO data or code,
//
//   The first allocation in the page always has write address equals to
//   runtime address.
//
//   1. shared dual map
//
//       Map an (unlinked) anonymous file as memory pool.
//       After first allocation, write address points to the second map.
//       The second map is set to unreadable and unwritable in finalization.
//
//   2. private dual map
//
//       Same as above but use anonymous memory map as memory pool,
//       and use low level OS api to set up the second map.
//
//   3. copying data into RO page bypassing page protection
//
//       After first allocation, write address points to a temporary buffer.
//       Requires copying data out of the temporary buffer in finalization.

// Allocates at least 256 pages per block and keep up to 8 blocks in the free
// list. The block with the least free space is discarded when we need to
// allocate a new page.
// Unused full pages are free'd from the block before discarding so at most
// one page is wasted on each discarded blocks. There should be at most one
// block with more than 128 pages available so the discarded one must have
// less than 128 pages available and therefore at least 128 pages used.
// (Apart from fragmentation) this guarantees less than 1% of memory is wasted.

// the `shared` type parameter is for Windows only....
struct Block {
    // runtime address
    char *ptr{nullptr};
    size_t total{0};
    size_t avail{0};

    Block(const Block&) = delete;
    Block &operator=(const Block&) = delete;
    Block(Block &&other)
        : ptr(other.ptr),
          total(other.total),
          avail(other.avail)
    {
        other.ptr = nullptr;
        other.total = other.avail = 0;
    }

    Block() = default;

    void *alloc(size_t size, size_t align)
    {
        size_t aligned_avail = avail & (-align);
        if (aligned_avail < size)
            return nullptr;
        char *p = ptr + total - aligned_avail;
        avail = aligned_avail - size;
        return p;
    }
    void reset(void *addr, size_t size)
    {
        if (avail >= jl_page_size) {
            uintptr_t end = uintptr_t(ptr) + total;
            uintptr_t first_free = end - avail;
            first_free = LLT_ALIGN(first_free, jl_page_size);
            assert(first_free < end);
            unmap_page((void*)first_free, end - first_free);
        }
        ptr = (char*)addr;
        total = avail = size;
    }
};

class RWAllocator {
    static constexpr int nblocks = 8;
    Block blocks[nblocks]{};
public:
    void *alloc(size_t size, size_t align)
    {
        size_t min_size = (size_t)-1;
        int min_id = 0;
        for (int i = 0;i < nblocks && blocks[i].ptr;i++) {
            if (void *ptr = blocks[i].alloc(size, align))
                return ptr;
            if (blocks[i].avail < min_size) {
                min_size = blocks[i].avail;
                min_id = i;
            }
        }
        size_t block_size = get_block_size(size);
        blocks[min_id].reset(map_anon_page(block_size), block_size);
        return blocks[min_id].alloc(size, align);
    }
};

struct SplitPtrBlock : public Block {
    // Possible states
    // Allocation:
    // * Initial allocation: `state & InitAlloc`
    // * Followup allocation: `(state & Alloc) && !(state & InitAlloc)`
    enum State {
        // This block has no page protection set yet
        InitAlloc = (1 << 0),
        // There is at least one allocation in this page since last finalization
        Alloc = (1 << 1),
        // `wr_ptr` can be directly used as write address.
        WRInit = (1 << 2),
        // With `WRInit` set, whether `wr_ptr` has write permission enabled.
        WRReady = (1 << 3),
    };

    uintptr_t wr_ptr{0};
    uint32_t state{0};
    SplitPtrBlock() = default;

    void swap(SplitPtrBlock &other)
    {
        std::swap(ptr, other.ptr);
        std::swap(total, other.total);
        std::swap(avail, other.avail);
        std::swap(wr_ptr, other.wr_ptr);
        std::swap(state, other.state);
    }

    SplitPtrBlock(SplitPtrBlock &&other)
        : SplitPtrBlock()
    {
        swap(other);
    }
};

struct Allocation {
    // Address to write to (the one returned by the allocation function)
    void *wr_addr;
    // Runtime address
    void *rt_addr;
    size_t sz;
    bool relocated;
};

template<bool exec>
class ROAllocator {
protected:
    static constexpr int nblocks = 8;
    SplitPtrBlock blocks[nblocks];
    // Blocks that are done allocating (removed from `blocks`)
    // but might not have all the permissions set or data copied yet.
    SmallVector<SplitPtrBlock, 16> completed;
    virtual void *get_wr_ptr(SplitPtrBlock &block, void *rt_ptr,
                             size_t size, size_t align) = 0;
    virtual SplitPtrBlock alloc_block(size_t size) = 0;
public:
    virtual ~ROAllocator() {}
    virtual void finalize()
    {
        for (auto &alloc: allocations) {
            // ensure the mapped pages are consistent
            sys::Memory::InvalidateInstructionCache(alloc.wr_addr,
                                                    alloc.sz);
            sys::Memory::InvalidateInstructionCache(alloc.rt_addr,
                                                    alloc.sz);
        }
        completed.clear();
        allocations.clear();
    }
    // Allocations that have not been finalized yet.
    SmallVector<Allocation, 16> allocations;
    void *alloc(size_t size, size_t align)
    {
        size_t min_size = (size_t)-1;
        int min_id = 0;
        for (int i = 0;i < nblocks && blocks[i].ptr;i++) {
            auto &block = blocks[i];
            void *ptr = block.alloc(size, align);
            if (ptr) {
                void *wr_ptr;
                if (block.state & SplitPtrBlock::InitAlloc) {
                    wr_ptr = ptr;
                }
                else {
                    wr_ptr = get_wr_ptr(block, ptr, size, align);
                }
                block.state |= SplitPtrBlock::Alloc;
                allocations.push_back(Allocation{wr_ptr, ptr, size, false});
                return wr_ptr;
            }
            if (block.avail < min_size) {
                min_size = block.avail;
                min_id = i;
            }
        }
        size_t block_size = get_block_size(size);
        auto &block = blocks[min_id];
        auto new_block = alloc_block(block_size);
        block.swap(new_block);
        if (new_block.state) {
            completed.push_back(std::move(new_block));
        }
        else {
            new_block.reset(nullptr, 0);
        }
        void *ptr = block.alloc(size, align);
#ifdef _OS_WINDOWS_
        block.state = SplitPtrBlock::Alloc;
        void *wr_ptr = get_wr_ptr(block, ptr, size, align);
        allocations.push_back(Allocation{wr_ptr, ptr, size, false});
        ptr = wr_ptr;
#else
        block.state = SplitPtrBlock::Alloc | SplitPtrBlock::InitAlloc;
        allocations.push_back(Allocation{ptr, ptr, size, false});
#endif
        return ptr;
    }
};

template<bool exec>
class DualMapAllocator : public ROAllocator<exec> {
protected:
    void *get_wr_ptr(SplitPtrBlock &block, void *rt_ptr, size_t, size_t) override
    {
        assert((char*)rt_ptr >= block.ptr &&
               (char*)rt_ptr < (block.ptr + block.total));
        if (!(block.state & SplitPtrBlock::WRInit)) {
            block.wr_ptr = (uintptr_t)create_shared_map(block.total,
                                                        block.wr_ptr);
            block.state |= SplitPtrBlock::WRInit;
        }
        if (!(block.state & SplitPtrBlock::WRReady)) {
            protect_page((void*)block.wr_ptr, block.total, Prot::RW);
            block.state |= SplitPtrBlock::WRReady;
        }
        return (char*)rt_ptr + (block.wr_ptr - uintptr_t(block.ptr));
    }
    SplitPtrBlock alloc_block(size_t size) override
    {
        SplitPtrBlock new_block;
        // use `wr_ptr` to record the id initially
        auto ptr = alloc_shared_page(size, (size_t*)&new_block.wr_ptr, exec);
        new_block.reset(ptr, size);
        return new_block;
    }
    void finalize_block(SplitPtrBlock &block, bool reset)
    {
        // This function handles setting the block to the right mode
        // and free'ing maps that are not needed anymore.
        // If `reset` is `true`, we won't allocate in this block anymore and
        // we should free up resources that is not needed at runtime.
        if (!(block.state & SplitPtrBlock::Alloc)) {
            // A block that is not used this time, check if we need to free it.
            if ((block.state & SplitPtrBlock::WRInit) && reset)
                unmap_page((void*)block.wr_ptr, block.total);
            return;
        }
        // For a block we used this time
        if (block.state & SplitPtrBlock::InitAlloc) {
            // For an initial block, we have a single RW map.
            // Need to map it to RO or RX.
            assert(!(block.state & (SplitPtrBlock::WRReady |
                                    SplitPtrBlock::WRInit)));
            protect_page(block.ptr, block.total, exec ? Prot::RX : Prot::RO);
            block.state = 0;
        }
        else {
            // For other ones, the runtime address has the correct mode.
            // Need to map the write address to RO.
            assert(block.state & SplitPtrBlock::WRInit);
            assert(block.state & SplitPtrBlock::WRReady);
            if (reset) {
                unmap_page((void*)block.wr_ptr, block.total);
            }
            else {
                protect_page((void*)block.wr_ptr, block.total, Prot::RO);
                block.state = SplitPtrBlock::WRInit;
            }
        }
    }
public:
    DualMapAllocator()
    {
        assert(anon_hdl != -1);
    }
    void finalize() override
    {
        for (auto &block : this->blocks) {
            finalize_block(block, false);
        }
        for (auto &block : this->completed) {
            finalize_block(block, true);
            block.reset(nullptr, 0);
        }
        ROAllocator<exec>::finalize();
    }
};

#ifdef _OS_LINUX_
template<bool exec>
class SelfMemAllocator : public ROAllocator<exec> {
    SmallVector<Block, 16> temp_buff;
protected:
    void *get_wr_ptr(SplitPtrBlock &block, void *rt_ptr,
                     size_t size, size_t align) override
    {
        assert(!(block.state & SplitPtrBlock::InitAlloc));
        for (auto &wr_block: temp_buff) {
            if (void *ptr = wr_block.alloc(size, align)) {
                return ptr;
            }
        }
        temp_buff.emplace_back();
        Block &new_block = temp_buff.back();
        size_t block_size = get_block_size(size);
        new_block.reset(map_anon_page(block_size), block_size);
        return new_block.alloc(size, align);
    }
    SplitPtrBlock alloc_block(size_t size) override
    {
        SplitPtrBlock new_block;
        new_block.reset(map_anon_page(size), size);
        return new_block;
    }
    void finalize_block(SplitPtrBlock &block, bool reset)
    {
        if (!(block.state & SplitPtrBlock::Alloc))
            return;
        if (block.state & SplitPtrBlock::InitAlloc) {
            // for an initial block, we need to map it to ro or rx
            assert(!(block.state & (SplitPtrBlock::WRReady |
                                    SplitPtrBlock::WRInit)));
            protect_page(block.ptr, block.total, exec ? Prot::RX : Prot::RO);
            block.state = 0;
        }
    }
public:
    SelfMemAllocator()
        : ROAllocator<exec>(),
          temp_buff()
    {
        assert(get_self_mem_fd() != -1);
    }
    void finalize() override
    {
        for (auto &block : this->blocks) {
            finalize_block(block, false);
        }
        for (auto &block : this->completed) {
            finalize_block(block, true);
            block.reset(nullptr, 0);
        }
        for (auto &alloc : this->allocations) {
            if (alloc.rt_addr == alloc.wr_addr)
                continue;
            write_self_mem(alloc.rt_addr, alloc.wr_addr, alloc.sz);
        }
        // clear all the temp buffers except the first one
        // (we expect only one)
        bool cached = false;
        for (auto &block : temp_buff) {
            if (cached) {
                munmap(block.ptr, block.total);
                block.ptr = nullptr;
                block.total = block.avail = 0;
            }
            else {
                block.avail = block.total;
                cached = true;
            }
        }
        if (cached)
            temp_buff.resize(1);
        ROAllocator<exec>::finalize();
    }
};
#endif // _OS_LINUX_

class RTDyldMemoryManagerJL : public SectionMemoryManager {
    struct EHFrame {
        uint8_t *addr;
        size_t size;
    };
    RTDyldMemoryManagerJL(const RTDyldMemoryManagerJL&) = delete;
    void operator=(const RTDyldMemoryManagerJL&) = delete;
    SmallVector<EHFrame, 16> pending_eh;
    RWAllocator rw_alloc;
    std::unique_ptr<ROAllocator<false>> ro_alloc;
    std::unique_ptr<ROAllocator<true>> exe_alloc;
    bool code_allocated;

public:
    RTDyldMemoryManagerJL()
        : SectionMemoryManager(),
          pending_eh(),
          rw_alloc(),
          ro_alloc(),
          exe_alloc(),
          code_allocated(false)
    {
#ifdef _OS_LINUX_
        if (!ro_alloc && get_self_mem_fd() != -1) {
            ro_alloc.reset(new SelfMemAllocator<false>());
            exe_alloc.reset(new SelfMemAllocator<true>());
        }
#endif
        if (!ro_alloc && init_shared_map() != -1) {
            ro_alloc.reset(new DualMapAllocator<false>());
            exe_alloc.reset(new DualMapAllocator<true>());
        }
    }
    ~RTDyldMemoryManagerJL() override
    {
    }
    void registerEHFrames(uint8_t *Addr, uint64_t LoadAddr,
                          size_t Size) override;
#if 0
    // Disable for now since we are not actually using this.
    void deregisterEHFrames(uint8_t *Addr, uint64_t LoadAddr,
                            size_t Size) override;
#endif
    uint8_t *allocateCodeSection(uintptr_t Size, unsigned Alignment,
                                 unsigned SectionID,
                                 StringRef SectionName) override;
    uint8_t *allocateDataSection(uintptr_t Size, unsigned Alignment,
                                 unsigned SectionID, StringRef SectionName,
                                 bool isReadOnly) override;
    using SectionMemoryManager::notifyObjectLoaded;
    void notifyObjectLoaded(RuntimeDyld &Dyld,
                            const object::ObjectFile &Obj) override;
    bool finalizeMemory(std::string *ErrMsg = nullptr) override;
    template <typename DL, typename Alloc>
    void mapAddresses(DL &Dyld, Alloc &&allocator)
    {
        for (auto &alloc: allocator->allocations) {
            if (alloc.rt_addr == alloc.wr_addr || alloc.relocated)
                continue;
            alloc.relocated = true;
            Dyld.mapSectionAddress(alloc.wr_addr, (uintptr_t)alloc.rt_addr);
        }
    }
    template <typename DL>
    void mapAddresses(DL &Dyld)
    {
        if (!ro_alloc)
            return;
        mapAddresses(Dyld, ro_alloc);
        mapAddresses(Dyld, exe_alloc);
    }
#ifdef _OS_WINDOWS_
    template <typename Alloc>
    void *lookupWriteAddressFor(void *rt_addr, Alloc &&allocator)
    {
        for (auto &alloc: allocator->allocations) {
            if (alloc.rt_addr == rt_addr) {
                return alloc.wr_addr;
            }
        }
        return nullptr;
    }
    void *lookupWriteAddressFor(void *rt_addr)
    {
        if (!ro_alloc)
            return rt_addr;
        if (void *ptr = lookupWriteAddressFor(rt_addr, ro_alloc))
            return ptr;
        if (void *ptr = lookupWriteAddressFor(rt_addr, exe_alloc))
            return ptr;
        return rt_addr;
    }
#endif // _OS_WINDOWS_
};

uint8_t *RTDyldMemoryManagerJL::allocateCodeSection(uintptr_t Size,
                                                    unsigned Alignment,
                                                    unsigned SectionID,
                                                    StringRef SectionName)
{
    // allocating more than one code section can confuse libunwind.
    assert(!code_allocated);
    code_allocated = true;
    if (exe_alloc)
        return (uint8_t*)exe_alloc->alloc(Size, Alignment);
    return SectionMemoryManager::allocateCodeSection(Size, Alignment, SectionID,
                                                     SectionName);
}

uint8_t *RTDyldMemoryManagerJL::allocateDataSection(uintptr_t Size,
                                                    unsigned Alignment,
                                                    unsigned SectionID,
                                                    StringRef SectionName,
                                                    bool isReadOnly)
{
    if (!isReadOnly)
        return (uint8_t*)rw_alloc.alloc(Size, Alignment);
    if (ro_alloc)
        return (uint8_t*)ro_alloc->alloc(Size, Alignment);
    return SectionMemoryManager::allocateDataSection(Size, Alignment, SectionID,
                                                     SectionName, isReadOnly);
}

void RTDyldMemoryManagerJL::notifyObjectLoaded(RuntimeDyld &Dyld,
                                               const object::ObjectFile &Obj)
{
    if (!ro_alloc) {
        assert(!exe_alloc);
        SectionMemoryManager::notifyObjectLoaded(Dyld, Obj);
        return;
    }
    assert(exe_alloc);
    mapAddresses(Dyld);
}

bool RTDyldMemoryManagerJL::finalizeMemory(std::string *ErrMsg)
{
    code_allocated = false;
    if (ro_alloc) {
        ro_alloc->finalize();
        assert(exe_alloc);
        exe_alloc->finalize();
        for (auto &frame: pending_eh)
            register_eh_frames(frame.addr, frame.size);
        pending_eh.clear();
        return false;
    }
    else {
        assert(!exe_alloc);
        return SectionMemoryManager::finalizeMemory(ErrMsg);
    }
}

void RTDyldMemoryManagerJL::registerEHFrames(uint8_t *Addr,
                                             uint64_t LoadAddr,
                                             size_t Size)
{
    if (uintptr_t(Addr) == LoadAddr) {
        register_eh_frames(Addr, Size);
    }
    else {
        pending_eh.push_back(EHFrame{(uint8_t*)(uintptr_t)LoadAddr, Size});
    }
}

#if 0
void RTDyldMemoryManagerJL::deregisterEHFrames(uint8_t *Addr,
                                               uint64_t LoadAddr,
                                               size_t Size)
{
    deregister_eh_frames((uint8_t*)LoadAddr, Size);
}
#endif

}

#ifdef _OS_WINDOWS_
void *lookupWriteAddressFor(RTDyldMemoryManager *memmgr, void *rt_addr)
{
    return ((RTDyldMemoryManagerJL*)memmgr)->lookupWriteAddressFor(rt_addr);
}
#endif

RTDyldMemoryManager* createRTDyldMemoryManager()
{
    return new RTDyldMemoryManagerJL();
}
