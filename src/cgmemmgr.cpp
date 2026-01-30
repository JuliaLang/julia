// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "platform.h"

#include <llvm/ExecutionEngine/JITLink/JITLink.h>
#include <llvm/ExecutionEngine/JITLink/JITLinkMemoryManager.h>
#include <llvm/ExecutionEngine/Orc/MapperJITLinkMemoryManager.h>

#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#ifdef _OS_LINUX_
#  include <sys/syscall.h>
#  include <sys/utsname.h>
#  include <sys/resource.h>
#endif
#ifdef _OS_WINDOWS_
#  include <memoryapi.h>
#else
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
#ifdef _OS_OPENBSD_
#  include <sys/resource.h>
#endif

#define DEBUG_TYPE "cgmemmgr"

namespace {

using namespace llvm;

#ifdef _P64
constexpr int DEFAULT_BLOCK_SIZE = 20;
constexpr int DEFAULT_TEXT_DATA_RATIO = 7;
#else
constexpr int DEFAULT_BLOCK_SIZE = 17;
constexpr int DEFAULT_TEXT_DATA_RATIO = 7;
#endif

/*
 * CALL_RANGE is the width of the (signed) integer difference for code/data
 * relocations on the host architecture with the "preferred" code model (usually
 * small).  CALL_RANGE only imposes a restriction on the total size of the
 * executable segment in a module, but it also has an optimization role; if one
 * module's text is within range of another, the GOT load in the PLT thunk can
 * be optimized into a direct jump.
 */

#if defined(_CPU_X86_)
const int CALL_RANGE = 32;
#elif defined(_CPU_X86_64_)
const int CALL_RANGE = 32;
#elif defined(_CPU_AARCH64_)
const int CALL_RANGE = 28;
#elif defined(_CPU_ARM_)
const int CALL_RANGE = 26;
#elif defined(_CPU_RISCV64_)
const int CALL_RANGE = 32;
#elif defined(_CPU_PPC64_)
const int CALL_RANGE = 26;
#elif defined(_CPU_PPC_)
const int CALL_RANGE = 26;
#else
#error "Architecture unsupported"
#endif

#ifndef _OS_WINDOWS_
std::optional<uintptr_t> map_reserve(uintptr_t hint, size_t size)
{
    assert(size % jl_page_size == 0);
    int flags = MAP_PRIVATE | MAP_ANONYMOUS;
# ifdef MAP_GUARD
    flags |= MAP_GUARD;
# endif
# ifdef MAP_NORESERVE
    flags |= MAP_NORESERVE;
# endif
    void *p = mmap((void *)hint, size, 0, flags, -1, 0);
    if (p == MAP_FAILED)
        return {};
    else
        return (uintptr_t)p;
}
#endif

uintptr_t map_reserve(size_t size)
{
#ifdef _OS_WINDOWS_
    // Noop: we can do better when we bump the minimum OS version to Windows 10,
    // where we can use VirtualAlloc2 with MEM_RESERVE_PLACEHOLDER.
    return 0;
#else
    if (auto p = map_reserve(0, size))
        return *p;
#endif
    abort();
}

void map_remove(uintptr_t addr, size_t size)
{
#ifdef _OS_WINDOWS_
    VirtualFree((void *)addr, size, MEM_DECOMMIT);
#else
    if (munmap((void *)addr, size) != -1)
        return;
    perror(__func__);
    abort();
#endif
}

uintptr_t map_rw(uintptr_t addr, size_t size)
{
#ifdef _OS_WINDOWS_
    // Until Win10+, we'll put the allocation whereever.
    (void)addr;
    void *ret = VirtualAlloc(nullptr, size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
    if (!ret)
        abort();
    return (uintptr_t)ret;

#else
    if (mmap((void *)addr, size, PROT_READ | PROT_WRITE,
             MAP_ANONYMOUS | MAP_PRIVATE | MAP_FIXED, -1, 0) == MAP_FAILED) {
        perror(__func__);
        abort();
    }
    return addr;
#endif
}

/*
 * Search and reserve a free region in the address space of the given size, such
 * that every byte in the region is within 2^(range-1) of every byte in the
 * target region.
 */
uintptr_t map_reserve_around(size_t size, int range_bits, uintptr_t target,
                             size_t target_size)
{
    assert(target % jl_page_size == 0);
    assert(target_size % jl_page_size == 0);

#ifdef _OS_WINDOWS_
    // Noop: we can do better when we bump the minimum OS version to Windows 10,
    // where we can use VirtualAlloc2 with MEM_RESERVE_PLACEHOLDER and
    // MemExtendedParameterAddressRequirements.
    return 0;
#else
    size_t half_range = 1u << (range_bits - 1);
    uintptr_t lo = target + target_size - half_range;
    uintptr_t hi = target + half_range - size;
    // The target region is too big.
    if (lo >= target || hi < target + target_size)
        return map_reserve(size);

    auto inrange = [=](uintptr_t addr) { return addr >= lo && addr < hi; };
    uint64_t seed = target ^ 0xdeadbeefdeadbeef;
    uintptr_t hint = target + target_size;
    for (int i = 0; i < 10; ++i) {
        if (auto addr = map_reserve(hint, size)) {
            if (inrange(*addr))
                return *addr;
            map_remove(*addr, size);
        }
        do {
            intptr_t r = cong(1u << range_bits, &seed) - half_range;
            hint = LLT_ALIGN(target + target_size + r, jl_page_size);
        } while (!inrange(hint));
    }
#endif

    // Failed to get a desirable address; return anything
    return map_reserve(size);
}

#ifndef _OS_WINDOWS_
static bool check_fd_or_close(int fd) JL_NOTSAFEPOINT
{
    if (fd == -1)
        return false;
    int err = fcntl(fd, F_SETFD, FD_CLOEXEC);
    assert(err == 0);
    (void)err; // prevent compiler warning
    if (fchmod(fd, S_IRWXU) != 0 || ftruncate(fd, jl_page_size) != 0) {
        close(fd);
        return false;
    }
    // This can fail due to `noexec` mount option ....
    void *ptr = mmap(nullptr, jl_page_size, PROT_NONE, MAP_SHARED, fd, 0);
    if (ptr == MAP_FAILED) {
        close(fd);
        return false;
    }
    // macOS allows us to MAP_SHARED, but not with R-X permissions.  We need to
    // mprotect in a second step.
    if (mprotect(ptr, jl_page_size, PROT_EXEC | PROT_READ) == -1) {
        close(fd);
        return false;
    }
    munmap(ptr, jl_page_size);
    return true;
}

static intptr_t init_anon_hdl(void) JL_NOTSAFEPOINT
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
    char shm_name[JL_PATH_MAX] = "julia-codegen-0123456789-0123456789/tmp///";
    pid_t pid = getpid();
    // `shm_open` can't be mapped exec on mac
#  ifndef _OS_DARWIN_
    int shm_open_errno;
    do {
        snprintf(shm_name, sizeof(shm_name),
                 "julia-codegen-%d-%d", (int)pid, rand());
        fd = shm_open(shm_name, O_RDWR | O_CREAT | O_EXCL, S_IRWXU);
        shm_open_errno = errno; // check_fd_or_close trashes errno, so save beforehand
        if (check_fd_or_close(fd)) {
            shm_unlink(shm_name);
            return fd;
        }
    } while (shm_open_errno == EEXIST);
#  endif
    FILE *tmpf = tmpfile();
    if (tmpf) {
        fd = dup(fileno(tmpf));
        fclose(tmpf);
        if (check_fd_or_close(fd)) {
            return fd;
        }
    }
    size_t len = sizeof(shm_name);
    if (uv_os_tmpdir(shm_name, &len) != 0) {
        // Unknown error; default to `/tmp`
        snprintf(shm_name, sizeof(shm_name), "/tmp");
        len = 4;
    }
    snprintf(shm_name + len, sizeof(shm_name) - len,
             "/julia-codegen-%d-XXXXXX", (int)pid);
    fd = mkstemp(shm_name);
    if (check_fd_or_close(fd)) {
        unlink(shm_name);
        return fd;
    }
    return -1;
}

// Multiple of 128MB.
// Hopefully no one will set a ulimit for this to be a problem...
static constexpr size_t map_size_inc_default = 128 * 1024 * 1024;

static size_t get_map_size_inc() JL_NOTSAFEPOINT
{
    rlimit rl;
    if (getrlimit(RLIMIT_FSIZE, &rl) != -1) {
        if (rl.rlim_cur != RLIM_INFINITY) {
            return std::min<size_t>(map_size_inc_default, rl.rlim_cur);
        }
        if (rl.rlim_max != RLIM_INFINITY) {
            return std::min<size_t>(map_size_inc_default, rl.rlim_max);
        }
    }
    return map_size_inc_default;
}
#endif

#ifdef _OS_LINUX_
// Using `/proc/self/mem`, A.K.A. Keno's remote memory manager.

ssize_t pwrite_addr(int fd, const void *buf, size_t nbyte, uintptr_t addr) JL_NOTSAFEPOINT
{
    static_assert(sizeof(off_t) >= 8, "off_t is smaller than 64bits");
#ifdef _P64
    const uintptr_t sign_bit = uintptr_t(1) << 63;
    if (__unlikely(sign_bit & addr)) {
        // This case should not happen with default kernel on 64bit since the address belongs
        // to kernel space (linear mapping).
        // However, it seems possible to change this at kernel compile time.

        // pwrite doesn't support offset with sign bit set but lseek does.
        // This is obviously not thread-safe but none of the mem manager does anyway...
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
static int _init_self_mem() JL_NOTSAFEPOINT
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

static int get_self_mem_fd() JL_NOTSAFEPOINT
{
    static int fd = _init_self_mem();
    return fd;
}

static void write_self_mem(void *dest, void *ptr, size_t size) JL_NOTSAFEPOINT
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

struct Allocation {
    // Address to write to (the one returned by the allocation function)
    char *wr_addr;
    // Runtime address
    char *rt_addr;
    size_t sz;
};

struct Block {
    char *ptr{nullptr};
    size_t total{0};
    size_t avail{0};

    bool can_alloc(size_t size, size_t align) {
        return (avail & (-align)) >= size;
    }

    Allocation alloc(size_t size, size_t align)
    {
        assert(can_alloc(size, align));
        if (size == 0)
            return {nullptr, nullptr, 0};
        size_t aligned_avail = avail & (-align);
        char *p = ptr + total - aligned_avail;
        avail = aligned_avail - size;
        return {p, p, size};
    }

    static Block map(uintptr_t addr, size_t size) {
        if (size == 0)
            return {};
        Block b;
        b.ptr = (char *)map_rw(addr, size);
        b.total = b.avail = size;
        return b;
    }

    void unmap_extra()
    {
        size_t used = LLT_ALIGN(total - avail, jl_page_size);
        if (total - used > 0)
            map_remove((uintptr_t)ptr + used, total - used);
    }

    bool in_block(const Allocation &a)
    {
        return a.rt_addr >= ptr && a.rt_addr < ptr + total;
    }
};

struct SplitPtrBlock : public Block {
    char *wr_ptr;
    int in_flight{0};
};

class ROBlockMapper {
public:
    // Map a block at the given (runtime) address.
    virtual SplitPtrBlock map(uintptr_t addr, size_t size) = 0;
    virtual Allocation alloc(SplitPtrBlock &block, size_t size, size_t align) = 0;
    virtual void finalize(SplitPtrBlock &block, const Allocation &alloc) = 0;
    virtual void cleanup(SplitPtrBlock &block) = 0;
    virtual ~ROBlockMapper() = default;
};

class DualBlockMapper : public ROBlockMapper {
public:
    static std::unique_ptr<ROBlockMapper> Create()
    {
#ifdef _OS_WINDOWS_
        return std::unique_ptr<ROBlockMapper>(new DualBlockMapper());
#else
        intptr_t hdl = init_anon_hdl();
        if (hdl == -1)
            return {};
        return std::unique_ptr<ROBlockMapper>(new DualBlockMapper(hdl));
#endif
    }

    SplitPtrBlock map(uintptr_t addr, size_t size) override
    {
        SplitPtrBlock b;
        assert(size % jl_page_size == 0);
#ifdef _OS_WINDOWS_
        HANDLE hdl = CreateFileMapping(INVALID_HANDLE_VALUE, nullptr,
                                       PAGE_EXECUTE_READWRITE, 0, size, nullptr);
        if (!hdl)
            abort();
        // We set the maximum permissions for this to the maximum for this file, and then
        // VirtualProtect, such that the debugger can still access these
        // pages and set breakpoints if it wants to.
        b.total = b.avail = size;
        b.ptr = (char *)MapViewOfFile(hdl, FILE_MAP_ALL_ACCESS | FILE_MAP_EXECUTE, 0, 0, size);
        assert(b.ptr && "Cannot map RX view");
        DWORD file_mode;
        VirtualProtect(b.ptr, size, PAGE_EXECUTE_READ | FILE_MAP_EXECUTE, &file_mode);

        b.wr_ptr = (char *)MapViewOfFile(hdl, FILE_MAP_ALL_ACCESS, 0, 0, size);
        assert(b.wr_ptr && "Cannot map RW view");

        return b;
#else
        if (__unlikely(map_offset + size > map_size))
            grow_map(size);

        if (mmap((void *)addr, size, PROT_NONE, MAP_SHARED | MAP_FIXED, anon_hdl,
                 map_offset) == MAP_FAILED)
            goto fail;
        if (mprotect((void *)addr, size, PROT_READ | PROT_EXEC) == -1)
            goto fail;

        b.wr_ptr = (char *)mmap(nullptr, size, PROT_READ | PROT_WRITE, MAP_SHARED, anon_hdl,
                                map_offset);
        if (b.wr_ptr == MAP_FAILED)
            goto fail;

        map_offset += size;

        b.avail = b.total = size;
        b.ptr = (char *)addr;

        return b;

fail:
        perror(__func__);
        abort();
#endif
    }

    Allocation alloc(SplitPtrBlock &block, size_t size, size_t align) override
    {
        Allocation a = block.alloc(size, align);
        a.wr_addr = (a.rt_addr - block.ptr) + block.wr_ptr;
        ++block.in_flight;
        return a;
    }

    void finalize(SplitPtrBlock &block, const Allocation &alloc) override
    {
        --block.in_flight;
        assert(block.in_flight >= 0);
    }

    void cleanup(SplitPtrBlock &block) override
    {
        if (block.total > 0)
            map_remove((uintptr_t)block.wr_ptr, block.total);
    }

    ~DualBlockMapper()
    {
#ifndef _OS_WINDOWS_
        close(anon_hdl);
#endif
    }

protected:
#ifndef _OS_WINDOWS_
    DualBlockMapper(intptr_t anon_hdl) : anon_hdl(anon_hdl) {}
#endif

#ifndef _OS_WINDOWS_
    void grow_map(size_t size)
    {
        size = std::max(LLT_ALIGN(size, jl_page_size), get_map_size_inc());
        map_size += size;
        int ret = ftruncate(anon_hdl, map_size);
        if (ret != 0) {
            perror(__func__);
            abort();
        }
    }
#endif

private:
#ifndef _OS_WINDOWS_
    intptr_t anon_hdl;
    size_t map_offset = 0;
    size_t map_size = 0;
#endif
};

#ifdef _OS_LINUX_
class SelfMemMapper : public ROBlockMapper {
public:
    static std::unique_ptr<ROBlockMapper> Create()
    {
        int fd = get_self_mem_fd();
        if (fd == -1)
            return {};
        return std::unique_ptr<ROBlockMapper>(new SelfMemMapper(fd));
    }

    SplitPtrBlock map(uintptr_t addr, size_t size) override
    {
        SplitPtrBlock b{};
        assert(size % jl_page_size == 0);
        b.ptr = (char *)addr;
        b.avail = b.total = size;

        if (mmap((void *)addr, size, PROT_READ | PROT_EXEC,
                 MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0) == MAP_FAILED) {
            perror(__func__);
            abort();
        }

        return b;
    }

    Allocation alloc(SplitPtrBlock &block, size_t size, size_t align) override
    {
        Allocation a = block.alloc(size, align);
        a.wr_addr = size > 0 ? (char *)malloc(size) : nullptr;
        ++block.in_flight;
        return a;
    }

    void finalize(SplitPtrBlock &block, const Allocation &alloc) override
    {
        --block.in_flight;
        assert(block.in_flight >= 0);
        if (alloc.wr_addr != nullptr) {
            write_self_mem(alloc.rt_addr, alloc.wr_addr, alloc.sz);
            free(alloc.wr_addr);
        }
    }

    void cleanup(SplitPtrBlock &block) override {}

protected:
    SelfMemMapper(int self_fd) : self_fd(self_fd) {}

private:
    int self_fd;
};
#endif // _OS_LINUX_

// Some environment variables to enable testing
static long getenv_int(const char *name, long def)
{
    char *data = getenv(name);
    if (!data)
        return def;
    char *end;
    long result = strtol(data, &end, 10);
    if (end != data + strlen(data))
        jl_errorf("Bad value for %s", name);
    return result;
}

class CodeAllocator {
public:
    CodeAllocator()
    {
        block_size_bits = getenv_int("JULIA_CGMEMMGR_BLOCK_SIZE", DEFAULT_BLOCK_SIZE);
        if (block_size_bits > (CALL_RANGE - 1))
            jl_error("JULIA_CGMEMMGR_BLOCK_SIZE is too big for call relocations");
        else if ((1u << block_size_bits) <= jl_page_size)
            jl_error("JULIA_CGMEMMGR_BLOCK_SIZE is smaller than page size");

        int pages = (1u << block_size_bits) / jl_page_size;
        int rx_pages = pages * DEFAULT_TEXT_DATA_RATIO / (1 + DEFAULT_TEXT_DATA_RATIO);
        int rw_pages = pages - rx_pages;
        assert(rw_pages > 0 && rx_pages > 0);
        rx_block_size = rx_pages * jl_page_size;
        rw_block_size = rw_pages * jl_page_size;

        using MapFn = std::unique_ptr<ROBlockMapper> (*)();
        std::pair<const char *, MapFn> mappers[] = {
            {"dual", &DualBlockMapper::Create},
#ifdef _OS_LINUX_
            {"selfmem", &SelfMemMapper::Create},
#endif
        };

        char *mapper_type = getenv("JULIA_CGMEMMGR_MAP_TYPE");
        for (auto [name, mapfn] : mappers)
            if ((mapper_type && !strcmp(mapper_type, name)) || !mapper_type)
                rx_mapper = mapfn();
        if (!rx_mapper && mapper_type)
            jl_errorf("Unsupported JULIA_CGMEMMGR_MAP_TYPE: %s", mapper_type);

        if ((rx_mapper = DualBlockMapper::Create()))
            return;
#ifdef _OS_LINUX_
        if ((rx_mapper = SelfMemMapper::Create()))
            return;
#endif

        if (!rx_mapper)
            abort();
    }

    std::pair<Allocation, Allocation> alloc(size_t size_rx, size_t align_rx, size_t size_rw,
                                            size_t align_rw)
    {
        SplitPtrBlock *rx = &rx_block;
        Block *rw = &rw_block;
        std::optional<SplitPtrBlock> new_rx;
        std::optional<Block> new_rw;

        if (size_rx > rx_block_size || size_rw > rw_block_size) {
            // If this is an unusually large allocation, we can allocate an
            // entire block for it (though it will not benefit from being near
            // other code).
            size_rx = LLT_ALIGN(LLT_ALIGN(size_rx, align_rx), jl_page_size);
            size_rw = LLT_ALIGN(LLT_ALIGN(size_rw, align_rw), jl_page_size);
            uintptr_t base = map_reserve(size_rx + size_rw);
            new_rx = rx_mapper->map(base, size_rx);
            new_rw = Block::map(base + size_rx, size_rw);
            rx = &*new_rx;
            rw = &*new_rw;
        }
        else if (!rx_block.can_alloc(size_rx, align_rx) ||
                 !rw_block.can_alloc(size_rw, align_rw)) {
            alloc_blocks();
        }

        std::pair<Allocation, Allocation> ret{rx_mapper->alloc(*rx, size_rx, align_rx),
                                              rw->alloc(size_rw, align_rw)};
        if (new_rx)
            completed.push_back(std::move(*new_rx));

        return ret;
    }

    // Only RX allocations must be finalized.
    void finalize(const Allocation &alloc)
    {
        if (alloc.sz == 0)
            return;

        // Note: on some aarch64 platforms, like Apple CPUs, we need read
        // permission in order to invalidate instruction cache lines.  We are
        // not guaranteed to have read permission on the wr_addr when using
        // DualMapAllocator.
        sys::Memory::InvalidateInstructionCache(alloc.rt_addr, alloc.sz);

        if (rx_block.in_block(alloc)) {
            rx_mapper->finalize(rx_block, alloc);
        }
        else {
            auto it = std::find_if(completed.begin(), completed.end(),
                                   [&](SplitPtrBlock &b) { return b.in_block(alloc); });
            assert(it != completed.end());
            rx_mapper->finalize(*it, alloc);
            if (it->in_flight == 0) {
                rx_mapper->cleanup(*it);
                completed.erase(it);
            }
        }
    }

protected:
    void alloc_blocks()
    {
        uintptr_t block;
#ifdef _OS_WINDOWS_
        // Without Win10+, we'll just map the RW and RX sections right after
        // each other and use the large code model.
        block = 0;
        (void)map_reserve_around;
#else
        size_t size = 1u << block_size_bits;
        if (code_lo == 0 && code_hi == 0) {
            block = map_reserve(size);
            code_lo = block;
            code_hi = block + size;
        }
        else {
            block = map_reserve_around(size, CALL_RANGE, code_lo, code_hi - code_lo);
            code_lo = std::min(code_lo, block);
            code_hi = std::max(code_hi, block + size);
        }
#endif

        SplitPtrBlock rx = rx_mapper->map(block, rx_block_size);
        Block rw = Block::map(block + rx.total, rw_block_size);

        std::swap(rx_block, rx);
        std::swap(rw_block, rw);

        rx.unmap_extra();
        if (rx.in_flight == 0)
            rx_mapper->cleanup(rx);
        else
            completed.push_back(std::move(rx));
        rw.unmap_extra();
    }

private:
    std::unique_ptr<ROBlockMapper> rx_mapper;

    SplitPtrBlock rx_block;
    Block rw_block;

    SmallVector<SplitPtrBlock> completed;

    size_t block_size_bits;
    size_t rx_block_size;
    size_t rw_block_size;

    uintptr_t code_lo{0};
    uintptr_t code_hi{0};
};

class JLJITLinkMemoryManager : public jitlink::JITLinkMemoryManager {
public:
    class InFlightAlloc;

    void allocate(const jitlink::JITLinkDylib *JD, jitlink::LinkGraph &G,
                  OnAllocatedFunction OnAllocated) override;

    void deallocate(std::vector<FinalizedAlloc> Allocs,
                    OnDeallocatedFunction OnDeallocated) override
    {
        // This shouldn't be reachable, but we will get a better error message
        // from JITLink if we leak this allocation and fail elsewhere.
    }

protected:
    void finalize(const Allocation &A)
    {
        std::unique_lock Lock{Mutex};
        Alloc.finalize(A);
    }

private:
    std::mutex Mutex;
    CodeAllocator Alloc;
};

class JLJITLinkMemoryManager::InFlightAlloc : public JITLinkMemoryManager::InFlightAlloc {
public:
    InFlightAlloc(JLJITLinkMemoryManager &MM, jitlink::LinkGraph &G, Allocation RX)
      : MM(MM), G(G), RX(RX)
    {
    }

    void abandon(OnAbandonedFunction OnAbandoned) override
    {
        // This shouldn't be reachable, but we will get a better error message
        // from JITLink if we leak this allocation and fail elsewhere.
    }

    void finalize(OnFinalizedFunction OnFinalized) override
    {
        MM.finalize(RX);

        // Need to handle dealloc actions when we GC code
#if JL_LLVM_VERSION >= 210000 && JL_LLVM_VERSION < 220000
        // This change was reverted before llvm 22 is branched off
        shared::runFinalizeActions(G.allocActions(), [&](auto E) {
            if (!E)
                return OnFinalized(E.takeError());
            OnFinalized(FinalizedAlloc{});
        });
#else
        auto E = orc::shared::runFinalizeActions(G.allocActions());
        if (!E)
            return OnFinalized(E.takeError());
        OnFinalized(FinalizedAlloc{});
#endif
    }

private:
    JLJITLinkMemoryManager &MM;
    jitlink::LinkGraph &G;
    Allocation RX;
};

using orc::MemLifetime;
using orc::MemProt;

struct Segment {
    Align Alignment;
    size_t Size = 0;
    SmallVector<jitlink::Block *> Blocks;

    template<typename Func>
    void visitBlocks(Func F)
    {
        uintptr_t Pos = 0;
        for (auto B : Blocks) {
            Pos += (B->getAlignmentOffset() - Pos) % B->getAlignment();
            F(Pos, B);
            Pos += B->getSize();
            Alignment = std::max(Alignment, Align(B->getAlignment()));
        }
        Size = Pos;
    }

    void sort() { llvm::sort(Blocks, compareBlock); }

    static bool compareBlock(const jitlink::Block *LHS, const jitlink::Block *RHS)
    {
        if (LHS->getSection().getOrdinal() != RHS->getSection().getOrdinal())
            return LHS->getSection().getOrdinal() < RHS->getSection().getOrdinal();
        if (LHS->getAddress() != RHS->getAddress())
            return LHS->getAddress() < RHS->getAddress();
        return LHS->getSize() < RHS->getSize();
    }
};

void JLJITLinkMemoryManager::allocate(const jitlink::JITLinkDylib *JD,
                                      jitlink::LinkGraph &G,
                                      OnAllocatedFunction OnAllocated)
{
    Segment RX, RW;
    Segment *Segments[] = {&RX, &RW};
    for (auto &Sec : G.sections()) {
        if (Sec.getMemLifetime() == MemLifetime::NoAlloc)
            continue;
        MemProt P = Sec.getMemProt();
        assert(bool(P & MemProt::Exec) ? !bool(P & MemProt::Write) : true);
        Segment &Seg = bool(P & MemProt::Exec) ? RX : bool(P & MemProt::Write) ? RW : RX;
        Seg.Blocks.append(Sec.blocks().begin(), Sec.blocks().end());
    }

    // Make a first pass over each segment to compute the size.
    for (auto S : Segments) {
        S->sort();
        S->visitBlocks([](size_t Pos, jitlink::Block *B) {});
    }

    Allocation Allocs[2];
    {
        std::unique_lock Lock{Mutex};
        std::tie(Allocs[0], Allocs[1]) =
            Alloc.alloc(RX.Size, RX.Alignment.value(), RW.Size, RW.Alignment.value());
    }

    for (auto [S, A] : llvm::zip(Segments, Allocs)) {
        // Silly capture list because using A directly is C++20
        S->visitBlocks([A = A](size_t Pos, jitlink::Block *B) {
            B->setAddress(orc::ExecutorAddr::fromPtr(A.rt_addr + Pos));
            if (B->isZeroFill()) {
                memset(A.wr_addr + Pos, 0, B->getSize());
            }
            else {
                memcpy(A.wr_addr + Pos, B->getContent().data(), B->getSize());
                B->setMutableContent({Pos + A.wr_addr, B->getSize()});
            }
        });
    }

    OnAllocated(std::make_unique<InFlightAlloc>(*this, G, Allocs[0]));
}

} // namespace

std::unique_ptr<jitlink::JITLinkMemoryManager> createJITLinkMemoryManager()
{
    return std::make_unique<JLJITLinkMemoryManager>();
}
