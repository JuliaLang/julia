// This file is a part of Julia. License is MIT: https://julialang.org/license
#include "objcache.h"

#include <chrono>
#include <llvm/Support/FileSystem.h>

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
    MDBMemoryBuffer(MDB_txn *Txn, llvm::StringRef Data) : Txn(Txn), Data(Data) {}
    ~MDBMemoryBuffer() override { mdb_txn_abort(Txn); }

private:
    MDB_txn *Txn;
    llvm::StringRef Data;
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
    if (Initialized.load(memory_order_acquire))
        return;

    const char *Enable = getenv("JULIA_OBJCACHE");
    auto CachePath = getCachePath();
    if (!CachePath || !Enable || strcmp(Enable, "1"))
        goto done;

    if (checkMDB(mdb_env_create(&Env))) {
        Env = nullptr;
        goto done;
    }
    checkMDB(mdb_env_set_maxreaders(Env, 510));
    checkMDB(mdb_env_set_maxdbs(Env, 128));
    llvm::sys::fs::create_directories(*CachePath);
    if (checkMDB(mdb_env_open(Env, CachePath->c_str(), 0, 0640))) {
        mdb_env_close(Env);
        Env = nullptr;
        goto done;
    }
    checkMDB(mdb_env_set_mapsize(Env, (size_t)1 << 30)); // 1 GiB maximum

    MDB_txn *Txn;
    MDB_dbi Dbi;
    if (checkMDB(mdb_txn_begin(Env, nullptr, 0, &Txn)))
        goto done;
    if (checkMDB(mdb_dbi_open(Txn, "objcache", MDB_CREATE, &Dbi)))
        goto done;
    if (checkMDB(mdb_txn_commit(Txn)))
        goto done;

    uv_thread_create(
        &WriterThread, [](void *arg) { static_cast<ObjCache *>(arg)->writerThread(); },
        this);

done:
    Initialized.store(true, memory_order_release);
    return;
}

static size_t NWrite = 0, NRead = 0, NMiss = 0, NHit = 0;

__attribute__((destructor)) static void dump_stats()
{
    if (LogFile)
        jl_printf(
            JL_STDERR,
            "cache read : %zu\ncache write: %zu\ncache hit  : %zu\ncache miss : %zu\n",
            NRead, NWrite, NHit, NMiss);
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

    llvm::raw_null_ostream OS;
    llvm::BitcodeWriter BW{OS};
    llvm::ModuleHash ModHash;
    llvm::SmallVector<char, 2 * sizeof ModHash + 1> KeyBuf;
    BW.writeModule(M, false, nullptr, true, &ModHash);
    BW.writeSymtab();
    BW.writeStrtab();
    llvm::toHex({(uint8_t *)&ModHash[0], sizeof ModHash}, true, KeyBuf);
    KeyBuf[KeyBuf.size() - 1] = 0;

    MDB_txn *Txn;
    if (int Err = mdb_txn_begin(Env, nullptr, MDB_RDONLY | MDB_NOSYNC, &Txn)) {
        checkMDB(Err);
        return Compile();
    }

    MDB_dbi Dbi;
    if (int Err = mdb_dbi_open(Txn, "objcache", 0, &Dbi)) {
        checkMDB(Err);
        mdb_txn_abort(Txn);
        return Compile();
    }

    MDB_val Key{sizeof ModHash, ModHash.data()};
    MDB_val Data;
    if (int Err = mdb_get(Txn, Dbi, &Key, &Data)) {
        if (Err != MDB_NOTFOUND)
            checkMDB(Err);
        mdb_txn_abort(Txn);

        double LookupMs =
            std::chrono::duration<double, std::milli>(Clock::now() - LookupStart).count();

        ++NMiss;
        auto CompileStart = Clock::now();
        auto Obj = Compile();
        double CompileMs =
            std::chrono::duration<double, std::milli>(Clock::now() - CompileStart).count();

        if (LogFile)
            fprintf(LogFile, "lookup,%s,%.3f,miss,%.3f,%zu,%zu\n", KeyBuf.begin(), LookupMs,
                    CompileMs, Obj->getBufferSize(), Weight);

        auto ObjCopy = llvm::MemoryBuffer::getMemBufferCopy(Obj->getBuffer());
        {
            std::unique_lock<std::mutex> Lock{Mutex};
            ObjQueue.push_back({ModHash, std::move(ObjCopy)});
        }
        QueueCond.notify_one();

        return Obj;
    }

    auto Buf = llvm::MemoryBuffer::getMemBuffer(
        llvm::StringRef((const char *)Data.mv_data, Data.mv_size), "", false);
    ++NHit;
    NRead += Buf->getBufferSize();

    mdb_txn_abort(Txn);

    double LookupMs =
        std::chrono::duration<double, std::milli>(Clock::now() - LookupStart).count();
    if (LogFile)
        fprintf(LogFile, "lookup,%s,%.3f,hit,%zu,%zu\n", KeyBuf.begin(), LookupMs,
                Buf->getBufferSize(), Weight);

    return Buf;
}

void ObjCache::writerThread()
{
    std::vector<std::pair<llvm::ModuleHash, std::unique_ptr<llvm::MemoryBuffer>>>
        LocalQueue;
    while (1) {
        LocalQueue.clear();
        {
            std::unique_lock Lock{Mutex};
            QueueCond.wait(Lock, [this]() { return !ObjQueue.empty(); });
            std::swap(LocalQueue, ObjQueue);
        }

        MDB_txn *Txn;
        if (int Err = mdb_txn_begin(Env, nullptr, 0, &Txn)) {
            checkMDB(Err);
            continue;
        }

        MDB_dbi Dbi;
        if (int Err = mdb_dbi_open(Txn, "objcache", 0, &Dbi)) {
            checkMDB(Err);
            mdb_txn_abort(Txn);
            continue;
        }

        using Clock = std::chrono::steady_clock;

        for (auto &[ModHash, Obj] : LocalQueue) {
            llvm::SmallVector<char, 2 * sizeof ModHash + 1> KeyBuf;
            llvm::toHex({(uint8_t *)&ModHash[0], sizeof ModHash}, true, KeyBuf);
            KeyBuf[KeyBuf.size() - 1] = 0;

            MDB_val Key{sizeof ModHash, ModHash.data()};
            MDB_val Data{Obj->getBufferSize(), (void *)Obj->getBufferStart()};
            auto WriteStart = Clock::now();
            if (int Err = mdb_put(Txn, Dbi, &Key, &Data, 0))
                checkMDB(Err);
            NWrite += Obj->getBufferSize();
            double WriteMs =
                std::chrono::duration<double, std::milli>(Clock::now() - WriteStart)
                    .count();
            if (LogFile)
                fprintf(LogFile, "write,%s,%.3f,%zu\n", KeyBuf.begin(), WriteMs,
                        Obj->getBufferSize());
            auto _ = std::move(Obj);
        }

        checkMDB(mdb_txn_commit(Txn));
    }
}
