#ifndef JL_CONCURRENT_UTILS_H
#define JL_CONCURRENT_UTILS_H

#include <condition_variable>
#include <functional>
#include <type_traits>
#include <mutex>

#include "julia.h"

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

template
<typename ResourceT, size_t max = 0,
    typename BackingT = std::stack<ResourceT,
        std::conditional_t<max == 0,
            llvm::SmallVector<ResourceT>,
            llvm::SmallVector<ResourceT, max>
        >
    >
>
struct ResourcePool {
    public:
    typedef ResourceT value_type;
    ResourcePool(std::function<ResourceT()> creator) : creator(std::move(creator)) {}
    class OwningResource {
        public:
        OwningResource(ResourcePool &pool, ResourceT resource) : pool(pool), resource(std::move(resource)) {}
        OwningResource(const OwningResource &) = delete;
        OwningResource &operator=(const OwningResource &) = delete;
        OwningResource(OwningResource &&) = default;
        OwningResource &operator=(OwningResource &&) = default;
        ~OwningResource() {
            if (resource) pool.release(std::move(*resource));
        }
        ResourceT release() {
            ResourceT res(std::move(*resource));
            resource.reset();
            return res;
        }
        void reset(ResourceT res) {
            *resource = std::move(res);
        }
        ResourceT &operator*() {
            return *resource;
        }
        ResourceT *operator->() {
            return get();
        }
        ResourceT *get() {
            return resource.getPointer();
        }
        const ResourceT &operator*() const {
            return *resource;
        }
        const ResourceT *operator->() const {
            return get();
        }
        const ResourceT *get() const {
            return resource.getPointer();
        }
        explicit operator bool() const {
            return resource;
        }
        private:
        ResourcePool &pool;
        llvm::Optional<ResourceT> resource;
    };

    OwningResource operator*() {
        return OwningResource(*this, acquire());
    }

    OwningResource get() {
        return **this;
    }

    ResourceT acquire() {
        std::unique_lock<std::mutex> lock(mutex);
        if (!pool.empty()) {
            return pop(pool);
        }
        if (!max || created < max) {
            created++;
            return creator();
        }
        empty.wait(lock, [&](){ return !pool.empty(); });
        assert(!pool.empty() && "Expected resource pool to have a value!");
        return pop(pool);
    }
    void release(ResourceT &&resource) {
        std::lock_guard<std::mutex> lock(mutex);
        pool.push(std::move(resource));
        empty.notify_one();
    }
    private:
    template<typename T, typename Container>
    static ResourceT pop(std::queue<T, Container> &pool) {
        ResourceT top = std::move(pool.front());
        pool.pop();
        return top;
    }
    template<typename PoolT>
    static ResourceT pop(PoolT &pool) {
        ResourceT top = std::move(pool.top());
        pool.pop();
        return top;
    }
    std::function<ResourceT()> creator;
    size_t created = 0;
    BackingT pool;
    std::mutex mutex;
    std::condition_variable empty;
};

#endif
