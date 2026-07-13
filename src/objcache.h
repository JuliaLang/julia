// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_OBJCACHE_H
#define JL_OBJCACHE_H

#include <memory>

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

// The argument is true if the result may be stored in the cache, in which case
// the compiled object must not depend on the state of this process.
using CompileFn = llvm::unique_function<std::unique_ptr<llvm::MemoryBuffer>(bool)>;

struct ObjCacheState;

class ObjCache {
public:
    ObjCache();
    ~ObjCache() JL_NOTSAFEPOINT;
    std::unique_ptr<llvm::MemoryBuffer>
    get(llvm::Module &M, CompileFn Compile) JL_NOTSAFEPOINT_ENTER JL_NOTSAFEPOINT_LEAVE;
    // Whether the cache can possibly be used by this process.  Unlike the
    // asynchronous database initialization, this is known at construction
    // time, so it can be used to configure the optimization pipeline.
    bool isGloballyEnabled() const JL_NOTSAFEPOINT { return GloballyEnabled; }
    void shutdown() JL_NOTSAFEPOINT;

    using Hash = std::array<uint8_t, 20>;

protected:
    void initDB() JL_NOTSAFEPOINT_ENTER JL_NOTSAFEPOINT_LEAVE;

private:
    const bool GloballyEnabled;
    // Shared with the detached writer thread and any outstanding cache-hit
    // buffers, both of which may outlive this object.
    std::shared_ptr<ObjCacheState> State;
};

#endif // JL_OBJCACHE_H
