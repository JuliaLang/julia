#include "objcache.h"

#include <chrono>

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

class MDBMemoryBuffer : public llvm::MemoryBuffer {
public:
    MDBMemoryBuffer(MDB_txn *Txn, llvm::StringRef Data) : Txn(Txn), Data(Data) {}
    ~MDBMemoryBuffer() override { mdb_txn_abort(Txn); }

private:
    MDB_txn *Txn;
    llvm::StringRef Data;
};

static void checkMDB(int Err)
{
    if (Err == 0)
        return;
    jl_printf(JL_STDERR, "objcache error: %s\n", mdb_strerror(Err));
}

ObjCache::ObjCache()
{
    const char *Enable = getenv("JULIA_OBJCACHE");
    if (!Enable || strcmp(Enable, "1"))
        return;

    if (int Err = mdb_env_create(&Env)) {
        checkMDB(Err);
        Env = nullptr;
        return;
    }
    checkMDB(mdb_env_set_maxreaders(Env, 510));
    if (int Err = mdb_env_open(Env, "objcache", 0, 0640)) {
        checkMDB(Err);
        mdb_env_close(Env);
        Env = nullptr;
        return;
    }
    checkMDB(mdb_env_set_mapsize(Env, 4ULL << 30));

    uv_thread_create(
        &WriterThread, [](void *arg) { static_cast<ObjCache *>(arg)->writerThread(); },
        this);
}

static size_t NWrite = 0, NRead = 0, NMiss = 0, NHit = 0;

__attribute__((destructor)) static void dump_stats()
{
    jl_printf(JL_STDERR,
              "cache read : %zu\ncache write: %zu\ncache hit  : %zu\ncache miss : %zu\n",
              NRead, NWrite, NHit, NMiss);
}

std::unique_ptr<llvm::MemoryBuffer> ObjCache::get(llvm::Module &M, CompileFn Compile)
{
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
    if (int Err = mdb_dbi_open(Txn, nullptr, 0, &Dbi)) {
        checkMDB(Err);
        mdb_txn_abort(Txn);
        return Compile();
    }

    MDB_val Key{ModHash.size() * sizeof ModHash[0], ModHash.data()};
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
            fprintf(LogFile, "lookup,%s,%.3f,miss,%.3f,%zu,%zu\n", KeyBuf.begin(),
                    LookupMs, CompileMs, Obj->getBufferSize(), Weight);

        auto ObjCopy = llvm::MemoryBuffer::getMemBufferCopy(Obj->getBuffer());
        {
            std::unique_lock<std::mutex> Lock{QueueMutex};
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
            std::unique_lock Lock{QueueMutex};
            QueueCond.wait(Lock, [this]() { return !ObjQueue.empty(); });
            std::swap(LocalQueue, ObjQueue);
        }

        MDB_txn *Txn;
        if (int Err = mdb_txn_begin(Env, nullptr, 0, &Txn)) {
            checkMDB(Err);
            continue;
        }

        MDB_dbi Dbi;
        if (int Err = mdb_dbi_open(Txn, nullptr, 0, &Dbi)) {
            checkMDB(Err);
            mdb_txn_abort(Txn);
            continue;
        }

        using Clock = std::chrono::steady_clock;

        for (auto &[ModHash, Obj] : LocalQueue) {
            llvm::SmallVector<char, 2 * sizeof ModHash + 1> KeyBuf;
            llvm::toHex({(uint8_t *)&ModHash[0], sizeof ModHash}, true, KeyBuf);
            KeyBuf[KeyBuf.size() - 1] = 0;

            MDB_val Key{ModHash.size(), ModHash.data()};
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
