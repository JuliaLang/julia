#include <llvm/ADT/StringMap.h>
#include <llvm/DebugInfo/DIContext.h>
#include <llvm/IR/DataLayout.h>

#include "julia.h"

#include <map>
#include <mutex>
#include <type_traits>

typedef struct {
    const llvm::object::ObjectFile *obj;
    llvm::DIContext *ctx;
    int64_t slide;
} objfileentry_t;

// Central registry for resolving function addresses to `jl_method_instance_t`s and
// originating `ObjectFile`s (for the DWARF debug info).
//
// A global singleton instance is notified by the JIT whenever a new object is emitted,
// and later queried by the various function info APIs. We also use the chance to handle
// some platform-specific unwind info registration (which is unrelated to the query
// functionality).
class JITDebugInfoRegistry
{
public:
    template<typename ResourceT>
    struct Locked {

        template<typename CResourceT>
        struct Lock {
            std::unique_lock<std::mutex> lock;
            CResourceT &resource;

            Lock(std::mutex &mutex, CResourceT &resource) JL_NOTSAFEPOINT : lock(mutex), resource(resource) {}
            Lock(Lock &&) JL_NOTSAFEPOINT = default;
            Lock &operator=(Lock &&) JL_NOTSAFEPOINT = default;

            CResourceT &operator*() JL_NOTSAFEPOINT {
                return resource;
            }

            const CResourceT &operator*() const JL_NOTSAFEPOINT {
                return resource;
            }

            CResourceT *operator->() JL_NOTSAFEPOINT {
                return &**this;
            }

            const CResourceT *operator->() const JL_NOTSAFEPOINT {
                return &**this;
            }

            operator const CResourceT &() const JL_NOTSAFEPOINT {
                return resource;
            }

            ~Lock() JL_NOTSAFEPOINT = default;
        };
    private:

        mutable std::mutex mutex;
        ResourceT resource;
    public:
        typedef Lock<ResourceT> LockT;
        typedef Lock<const ResourceT> ConstLockT;

        Locked(ResourceT resource = ResourceT()) JL_NOTSAFEPOINT : mutex(), resource(std::move(resource)) {}

        LockT operator*() JL_NOTSAFEPOINT {
            return LockT(mutex, resource);
        }

        ConstLockT operator*() const JL_NOTSAFEPOINT {
            return ConstLockT(mutex, resource);
        }

        ~Locked() JL_NOTSAFEPOINT = default;
    };

    struct image_info_t {
        uint64_t base;
        jl_image_fptrs_t fptrs;
        jl_method_instance_t **fvars_linfo;
        size_t fvars_n;
    };

    struct libc_frames_t {
#if defined(_OS_DARWIN_) && defined(LLVM_SHLIB)
        std::atomic<void(*)(void*)> libc_register_frame_{nullptr};
        std::atomic<void(*)(void*)> libc_deregister_frame_{nullptr};

        void libc_register_frame(const char *Entry) JL_NOTSAFEPOINT;

        void libc_deregister_frame(const char *Entry) JL_NOTSAFEPOINT;
#endif
    };
private:

    struct ObjectInfo {
        const llvm::object::ObjectFile *object = nullptr;
        size_t SectionSize = 0;
        ptrdiff_t slide = 0;
        llvm::object::SectionRef Section{};
        llvm::DIContext *context = nullptr;
    };

    template<typename KeyT, typename ValT>
    using rev_map = std::map<KeyT, ValT, std::greater<KeyT>>;

    typedef rev_map<size_t, ObjectInfo> objectmap_t;
    typedef rev_map<uint64_t, objfileentry_t> objfilemap_t;

    objectmap_t objectmap{};
    rev_map<size_t, std::pair<size_t, jl_method_instance_t *>> linfomap{};

    // Maintain a mapping of unrealized function names -> linfo objects
    // so that when we see it get emitted, we can add a link back to the linfo
    // that it came from (providing name, type signature, file info, etc.)
    Locked<llvm::StringMap<jl_code_instance_t*>> codeinst_in_flight{};

    Locked<llvm::DenseMap<uint64_t, image_info_t>> image_info{};

    Locked<objfilemap_t> objfilemap{};

    static std::string mangle(llvm::StringRef Name, const llvm::DataLayout &DL) JL_NOTSAFEPOINT;

public:

    JITDebugInfoRegistry() JL_NOTSAFEPOINT;
    ~JITDebugInfoRegistry() JL_NOTSAFEPOINT = default;

    libc_frames_t libc_frames{};

    void add_code_in_flight(llvm::StringRef name, jl_code_instance_t *codeinst, const llvm::DataLayout &DL) JL_NOTSAFEPOINT;
    jl_method_instance_t *lookupLinfo(size_t pointer) JL_NOTSAFEPOINT;
    void registerJITObject(const llvm::object::ObjectFile &Object,
                        std::function<uint64_t(const llvm::StringRef &)> getLoadAddress,
                        std::function<void*(void*)> lookupWriteAddress);
    objectmap_t& getObjectMap() JL_NOTSAFEPOINT;
    void add_image_info(image_info_t info) JL_NOTSAFEPOINT;
    bool get_image_info(uint64_t base, image_info_t *info) const JL_NOTSAFEPOINT;
    Locked<objfilemap_t>::LockT get_objfile_map() JL_NOTSAFEPOINT;
};
