// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_OBJCACHE_H
#define JL_OBJCACHE_H

#include <condition_variable>

#include <llvm/ADT/FunctionExtras.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/MemoryBuffer.h>
#include <lmdb.h>
#include <uv.h>

using CompileFn = llvm::unique_function<std::unique_ptr<llvm::MemoryBuffer>()>;

class ObjCache {
public:
    ObjCache() = default;
    std::unique_ptr<llvm::MemoryBuffer> get(llvm::Module &M, CompileFn Compile);

protected:
    void writerThread();
    void initDB();

private:
    std::atomic<bool> Initialized = false;
    MDB_env *Env = nullptr;
    uv_thread_t WriterThread;
    std::vector<std::pair<llvm::ModuleHash, std::unique_ptr<llvm::MemoryBuffer>>> ObjQueue;
    std::mutex Mutex;
    std::condition_variable QueueCond;
};

#endif // JL_OBJCACHE_H
