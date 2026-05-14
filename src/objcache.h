// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_OBJCACHE_H
#define JL_OBJCACHE_H

#include <atomic>
#include <condition_variable>

#include <llvm/ADT/FunctionExtras.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/MemoryBuffer.h>
#include <lmdb.h>
#include <uv.h>

#include "analyzer_annotations.h"

/*
 * Environment variable knobs:
 *
 * JULIA_OBJCACHE       Set to 0 to disable the objcache.
 * JULIA_OBJCACHE_LOG   When set, logs cache hits/misses to the provided path.
 * JULIA_OBJCACHE_PATH  When unset, the cache is stored in the depot, under
 *                      /cache/<Julia version>.  It is useful to set this when
 *                      bootstrapping Julia, since the depot path is not yet
 *                      available.
 */

using CompileFn = llvm::unique_function<std::unique_ptr<llvm::MemoryBuffer>()>;

class ObjCache {
public:
    ObjCache() = default;
    ~ObjCache() JL_NOTSAFEPOINT = default;
    std::unique_ptr<llvm::MemoryBuffer> get(llvm::Module &M, CompileFn Compile);
    bool isEnabled() JL_NOTSAFEPOINT;
    void shutdown() JL_NOTSAFEPOINT;

    using Hash = std::array<uint8_t, 20>;

protected:
    void writerThread();
    void initDB();

private:
    std::atomic<bool> Initialized = false;
    MDB_env *Env = nullptr;
    MDB_dbi ObjCacheDbi;
    uv_thread_t WriterThread;
    bool Started = false;
    bool Exiting = false;
    std::vector<std::pair<Hash, std::unique_ptr<llvm::MemoryBuffer>>> ObjQueue;
    std::mutex Mutex;
    std::condition_variable QueueCond;
};

#endif // JL_OBJCACHE_H
