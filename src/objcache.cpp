// This file is a part of Julia. License is MIT: https://julialang.org/license
#include "objcache.h"

#include <chrono>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/SHA1.h>

#include "jl_codegen_hash.inc"
#include "julia.h"

static FILE *getLogFile()
{
    const char *Path = getenv("JULIA_OBJCACHE_LOG");
    if (!Path)
        return nullptr;
    FILE *F = fopen(Path, "a");
    if (!F) {
        jl_printf(JL_STDERR, "objcache: failed to open log file %s\n", Path);
        return nullptr;
    }
    return F;
}

static FILE *LogFile = getLogFile();

static std::optional<std::string> getCachePath()
{
    // Useful to be able to override the objcache path for testing, or to use
    // the cache during bootstrapping.
    if (const char *P = getenv("JULIA_OBJCACHE_PATH"))
        return {P};
    if (jl_base_module == nullptr)
        return {};
    jl_value_t *DepotPath = jl_get_global(jl_base_module, jl_symbol("DEPOT_PATH"));
    if (!DepotPath || !jl_is_array(DepotPath) || jl_array_len(DepotPath) < 1)
        return {};
    jl_value_t *DepotStr = jl_array_ptr_ref(DepotPath, 0);
    if (!jl_is_string(DepotStr))
        return {};

    return (llvm::Twine(jl_string_ptr(DepotStr)) + "/cache/v" +
            llvm::Twine(JULIA_VERSION_MAJOR) + "." + llvm::Twine(JULIA_VERSION_MINOR))
        .str();
}

class MDBMemoryBuffer : public llvm::MemoryBuffer {
public:
    MDBMemoryBuffer(MDB_txn *Txn, llvm::StringRef Data) : Txn(Txn)
    {
        init(Data.begin(), Data.end(), false);
    }
    ~MDBMemoryBuffer() override { mdb_txn_abort(Txn); }
    BufferKind getBufferKind() const override { return MemoryBuffer_MMap; }

private:
    MDB_txn *Txn;
};

static int checkMDB(int Err)
{
    if (Err == 0)
        return Err;
    jl_printf(JL_STDERR, "objcache error: %s\n", mdb_strerror(Err));
    return Err;
}

void ObjCache::initDB()
{
    std::unique_lock<std::mutex> Lock{Mutex};
    std::string Path;
    MDB_txn *Txn;

    if (Initialized.load(memory_order_acquire))
        return;

    const char *Enable = getenv("JULIA_OBJCACHE");
    auto CachePath = getCachePath();
    if (!CachePath || !Enable || !strcmp(Enable, "0"))
        goto cleanup;

    if (checkMDB(mdb_env_create(&Env))) {
        Env = nullptr;
        goto cleanup;
    }
    checkMDB(mdb_env_set_maxreaders(Env, 510));
    checkMDB(mdb_env_set_maxdbs(Env, 128));
    checkMDB(mdb_env_set_mapsize(Env, (size_t)1 << 30)); // 1 GiB maximum
    llvm::sys::fs::create_directories(*CachePath);
    if (checkMDB(mdb_env_open(Env, CachePath->c_str(), MDB_NOSYNC | MDB_NOTLS, 0640))) {
        mdb_env_close(Env);
        Env = nullptr;
        goto cleanup;
    }

    if (checkMDB(mdb_txn_begin(Env, nullptr, 0, &Txn)))
        goto cleanup;
    if (checkMDB(mdb_dbi_open(Txn, "objcache", MDB_CREATE, &ObjCacheDbi)))
        goto cleanup_txn;
    if (checkMDB(mdb_txn_commit(Txn)))
        goto cleanup;

    uv_thread_create(
        &WriterThread, [](void *arg) { static_cast<ObjCache *>(arg)->writerThread(); },
        this);
    Started = true;
    goto cleanup;

cleanup_txn:
    mdb_txn_abort(Txn);
cleanup:
    Initialized.store(true, memory_order_release);
}

static std::atomic<size_t> NWrite = 0, NRead = 0, NMiss = 0, NHit = 0;

static ObjCache::Hash hashModule(const llvm::Module &M)
{
    llvm::raw_null_ostream OS;
    llvm::BitcodeWriter BW{OS};
    llvm::ModuleHash ModHash;
    llvm::SHA1 Hasher;

    BW.writeModule(M, false, nullptr, true, &ModHash);
    BW.writeSymtab();
    BW.writeStrtab();

    Hasher.update(LLVM_VERSION_STRING);
    Hasher.update(JL_CODEGEN_SRC_HASH);
    Hasher.update({(uint8_t *)&ModHash[0], sizeof ModHash});
    return Hasher.final();
}

static MDB_val hashToKey(ObjCache::Hash H, llvm::SmallVectorImpl<char> &Out)
{
    llvm::StringRef Prefix = "obj-";
    Out.append(Prefix.begin(), Prefix.end());
    llvm::toHex(H, true, Out);
    Out.push_back(0);
    // Add null terminator for convenient printing, but don't include it in the key.
    return {Out.size_in_bytes() - 1, Out.data()};
}

std::unique_ptr<llvm::MemoryBuffer> ObjCache::get(llvm::Module &M, CompileFn Compile)
{
    if (!Initialized.load(memory_order_acquire))
        initDB();

    if (!Env)
        return Compile();

    size_t Weight = 0;
    for (auto &F : M.functions())
        for (auto &BB : F)
            Weight += BB.size();

    using Clock = std::chrono::steady_clock;

    auto LookupStart = Clock::now();

    Hash H = hashModule(M);
    llvm::SmallVector<char, 4 + 2 * sizeof(llvm::ModuleHash) + 1> KeyBuf;
    MDB_val Key = hashToKey(H, KeyBuf);

    MDB_txn *Txn;
    if (int Err = mdb_txn_begin(Env, nullptr, MDB_RDONLY, &Txn)) {
        checkMDB(Err);
        return Compile();
    }

    MDB_val Data;
    if (int Err = mdb_get(Txn, ObjCacheDbi, &Key, &Data)) {
        if (Err != MDB_NOTFOUND)
            checkMDB(Err);
        mdb_txn_abort(Txn);

        double LookupMs =
            std::chrono::duration<double, std::milli>(Clock::now() - LookupStart).count();

        NMiss.fetch_add(1, memory_order_relaxed);
        auto CompileStart = Clock::now();
        auto Obj = Compile();
        double CompileMs =
            std::chrono::duration<double, std::milli>(Clock::now() - CompileStart).count();
        if (!Obj)
            return nullptr;

        if (LogFile)
            fprintf(LogFile, "lookup,%s,%.3f,miss,%.3f,%zu,%zu\n", KeyBuf.begin(), LookupMs,
                    CompileMs, Obj->getBufferSize(), Weight);

        auto ObjCopy = llvm::MemoryBuffer::getMemBufferCopy(Obj->getBuffer());
        {
            std::unique_lock<std::mutex> Lock{Mutex};
            ObjQueue.push_back({H, std::move(ObjCopy)});
        }
        QueueCond.notify_one();

        return Obj;
    }

    auto Buf = std::make_unique<MDBMemoryBuffer>(
        Txn, llvm::StringRef{(const char *)Data.mv_data, Data.mv_size});
    NHit.fetch_add(1, memory_order_relaxed);
    NRead.fetch_add(Buf->getBufferSize(), memory_order_relaxed);

    double LookupMs =
        std::chrono::duration<double, std::milli>(Clock::now() - LookupStart).count();
    if (LogFile)
        fprintf(LogFile, "lookup,%s,%.3f,hit,%zu,%zu\n", KeyBuf.begin(), LookupMs,
                Buf->getBufferSize(), Weight);

    return Buf;
}

bool ObjCache::isEnabled() const
{
    return Env;
}

void ObjCache::shutdown()
{
    if (Started) {
        {
            std::unique_lock<std::mutex> Lock{Mutex};
            Exiting = true;
        }
        QueueCond.notify_one();
        uv_thread_join(&WriterThread);
    }

    if (LogFile)
        jl_safe_printf(
            "cache read : %zu\ncache write: %zu\ncache hit  : %zu\ncache miss : %zu\n",
            NRead.load(memory_order_relaxed), NWrite.load(memory_order_relaxed),
            NHit.load(memory_order_relaxed), NMiss.load(memory_order_relaxed));
}

void ObjCache::writerThread()
{
    std::vector<std::pair<Hash, std::unique_ptr<llvm::MemoryBuffer>>> LocalQueue;
    while (1) {
        LocalQueue.clear();
        {
            std::unique_lock Lock{Mutex};
            QueueCond.wait(Lock, [this]() { return Exiting || !ObjQueue.empty(); });
            std::swap(LocalQueue, ObjQueue);
        }
        if (LocalQueue.empty())
            return;

        MDB_txn *Txn;
        if (int Err = mdb_txn_begin(Env, nullptr, 0, &Txn)) {
            checkMDB(Err);
            continue;
        }

        using Clock = std::chrono::steady_clock;

        bool Abort = false;
        for (auto &[H, Obj] : LocalQueue) {
            llvm::SmallVector<char, 4 + 2 * sizeof(llvm::ModuleHash) + 1> KeyBuf;
            MDB_val Key = hashToKey(H, KeyBuf);
            MDB_val Data{Obj->getBufferSize(), (void *)Obj->getBufferStart()};
            auto WriteStart = Clock::now();
            if (int Err = mdb_put(Txn, ObjCacheDbi, &Key, &Data, 0)) {
                checkMDB(Err);
                Abort = true;
                break;
            }
            NWrite.fetch_add(Obj->getBufferSize(), memory_order_relaxed);
            double WriteMs =
                std::chrono::duration<double, std::milli>(Clock::now() - WriteStart)
                    .count();
            if (LogFile)
                fprintf(LogFile, "write,%s,%.3f,%zu\n", KeyBuf.begin(), WriteMs,
                        Obj->getBufferSize());
            auto _ = std::move(Obj);
        }

        if (Abort)
            mdb_txn_abort(Txn);
        else
            checkMDB(mdb_txn_commit(Txn));
    }
}
