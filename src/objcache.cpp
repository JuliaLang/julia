// This file is a part of Julia. License is MIT: https://julialang.org/license
#include "objcache.h"

#include <llvm/Support/Endian.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/SHA1.h>

#include "jl_codegen_hash.inc"
#include "julia.h"
#include "julia_internal.h"

namespace endian = llvm::support::endian;
using endianness = llvm::endianness;

static constexpr int OBJCACHE_SCHEMA = 1;

// Skip atime refreshes when the existing access time is within this many
// nanoseconds of the new one, to avoid excessive LRU bookkeeping writes.
static constexpr int64_t OBJCACHE_ATIME_GRANULARITY = 300;

static uint64_t parseEnvU64(const char *Name, uint64_t Default)
{
    const char *S = getenv(Name);
    if (!S || !*S)
        return Default;
    char *End;
    unsigned long long V = strtoull(S, &End, 0);
    if (*End != '\0') {
        jl_printf(JL_STDERR, "objcache: invalid value for %s: %s\n", Name, S);
        return Default;
    }
    return (uint64_t)V;
}

// Parse an env var as a real number in [0, 1] and convert to a fixed-point
// fraction of 2^31.
static uint32_t parseEnvFrac31(const char *Name, double Default)
{
    const char *S = getenv(Name);
    double V = Default;
    if (S && *S) {
        char *End;
        V = strtod(S, &End);
        if (*End != '\0') {
            jl_printf(JL_STDERR, "objcache: invalid value for %s: %s\n", Name, S);
            V = Default;
        }
    }
    if (V < 0.0)
        V = 0.0;
    if (V > 1.0)
        V = 1.0;
    return (uint32_t)(V * (double)(1ULL << 31));
}

// We'll use a smaller default cache size on 32 bit, since we have a lot less
// address space to spare.
#ifdef _P64
static constexpr size_t OBJCACHE_DEFAULT_CAPACITY = 512 << 20;
#else
static constexpr size_t OBJCACHE_DEFAULT_CAPACITY = 32 << 20;
#endif
static const size_t OBJCACHE_CAPACITY =
    parseEnvU64("JULIA_OBJCACHE_CAPACITY", OBJCACHE_DEFAULT_CAPACITY);

// When the map is full, evict down to OBJCACHE_EVICT_TO/2^31 capacity.
static const uint32_t OBJCACHE_EVICT_TO = parseEnvFrac31("JULIA_OBJCACHE_EVICT_TO", 0.5);
// Evict when we reach OBJCACHE_EVICT_FROM/2^31 of capacity.
static const uint32_t OBJCACHE_EVICT_FROM =
    parseEnvFrac31("JULIA_OBJCACHE_EVICT_FROM", 0.875);

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

#define checkMDB(Err) (checkMDB_(Err, __LINE__))

static int checkMDB_(int Err, int Line)
{
    if (Err == 0)
        return Err;
    jl_printf(JL_STDERR, "objcache error (%d): %s\n", Line, mdb_strerror(Err));
    return Err;
}

class MDBTxn {
public:
    MDBTxn(MDB_env *Env, unsigned Flags = 0)
    {
        if (checkMDB(mdb_txn_begin(Env, nullptr, Flags, &Txn)))
            Txn = nullptr;
    }
    ~MDBTxn()
    {
        if (Txn)
            mdb_txn_abort(Txn);
    }
    MDBTxn(const MDBTxn &) = delete;
    MDBTxn &operator=(const MDBTxn &) = delete;
    MDBTxn(MDBTxn &&RHS) : Txn(std::exchange(RHS.Txn, nullptr)) {}
    MDBTxn &operator=(MDBTxn &&RHS)
    {
        std::swap(Txn, RHS.Txn);
        return *this;
    }
    void abort()
    {
        mdb_txn_abort(Txn);
        Txn = nullptr;
    }
    int commit()
    {
        int Ret = mdb_txn_commit(Txn);
        Txn = nullptr;
        return Ret;
    }
    MDB_txn *Txn{};
};

template<typename T>
MDB_val mdbVal(T &x)
{
    return {sizeof x, (void *)&x};
}

class MDBMemoryBuffer : public llvm::MemoryBuffer {
public:
    MDBMemoryBuffer(MDBTxn Txn, llvm::StringRef Data) : Txn(std::move(Txn))
    {
        init(Data.begin(), Data.end(), false);
    }
    BufferKind getBufferKind() const override { return MemoryBuffer_MMap; }

private:
    MDBTxn Txn;
};

void ObjCache::initDB()
{
    std::unique_lock<std::mutex> Lock{Mutex};

    if (Initialized.load(memory_order_acquire))
        return;

    const char *Enable = getenv("JULIA_OBJCACHE");
    auto CachePath = getCachePath();
    if (!CachePath || (Enable && !strcmp(Enable, "0")))
        goto done;

    if (checkMDB(mdb_env_create(&Env))) {
        Env = nullptr;
        goto done;
    }
    checkMDB(mdb_env_set_maxreaders(Env, 510));
    checkMDB(mdb_env_set_maxdbs(Env, 128));
    checkMDB(mdb_env_set_mapsize(Env, OBJCACHE_CAPACITY));
    llvm::sys::fs::create_directories(*CachePath);
    if (checkMDB(mdb_env_open(Env, CachePath->c_str(), MDB_NOSYNC | MDB_NOTLS, 0640))) {
        mdb_env_close(Env);
        goto cleanup;
    }

    {
        MDBTxn Txn{Env};
        if (!Txn.Txn)
            goto cleanup_env;
        if (checkMDB(mdb_dbi_open(Txn.Txn, "objcache", MDB_CREATE, &ObjCacheDbi)))
            goto cleanup_env;
        if (checkMDB(mdb_dbi_open(Txn.Txn, "objmeta", MDB_CREATE, &ObjMetaDbi)))
            goto cleanup_env;

        int Version = OBJCACHE_SCHEMA;
        MDB_val Key = mdbVal("schema");
        MDB_val Ver = mdbVal(Version);
        int Err = mdb_put(Txn.Txn, ObjMetaDbi, &Key, &Ver, MDB_NOOVERWRITE);
        if (Err == MDB_KEYEXIST && *static_cast<int *>(Ver.mv_data) != OBJCACHE_SCHEMA)
            goto cleanup_env;

        checkMDB(Txn.commit());
    }

    uv_thread_create(
        &WriterThread, [](void *arg) { static_cast<ObjCache *>(arg)->writerThread(); },
        this);
    Started = true;
    goto done;

cleanup_env:
    mdb_env_close(Env);
cleanup:
    Env = nullptr;
done:
    Initialized.store(true, memory_order_release);
}

ObjCache::~ObjCache()
{
    if (!Env)
        return;
    mdb_dbi_close(Env, ObjCacheDbi);
    mdb_dbi_close(Env, ObjMetaDbi);
    mdb_env_close(Env);
}

static std::atomic<size_t> NWrite = 0, NRead = 0, NMiss = 0, NHit = 0, NEvicted = 0;

static ObjCache::Hash hashModule(const llvm::Module &M)
{
    llvm::raw_null_ostream OS;
    llvm::BitcodeWriter BW{OS};
    llvm::ModuleHash ModHash;
    llvm::SHA1 Hasher;

    BW.writeModule(M, false, nullptr, true, &ModHash);
    // These are mandatory to get a valid hash.
    BW.writeSymtab();
    BW.writeStrtab();

    Hasher.update(LLVM_VERSION_STRING);
    Hasher.update(JL_CODEGEN_SRC_HASH);
    Hasher.update({(uint8_t *)&ModHash[0], sizeof ModHash});
    return Hasher.final();
}

constexpr size_t OBJKEY_SIZE = 2 + sizeof(ObjCache::Hash);
constexpr size_t METAKEY_SIZE = 2 + sizeof(int64_t) + sizeof(ObjCache::Hash);

constexpr char OBJKEY_TAG = 'O';
constexpr char METAKEY_TAG = 'M';

std::array<uint8_t, OBJKEY_SIZE> toObjKey(const ObjCache::Hash &Hash)
{
    std::array<uint8_t, OBJKEY_SIZE> Ret;
    Ret[0] = OBJKEY_TAG;
    Ret[1] = 0;
    memcpy(Ret.begin() + 2, Hash.begin(), Hash.size());
    return Ret;
}

std::array<uint8_t, METAKEY_SIZE> toMetaKey(int64_t Time, const ObjCache::Hash &Hash)
{
    std::array<uint8_t, METAKEY_SIZE> Ret;
    Ret[0] = METAKEY_TAG;
    Ret[1] = 0;
    endian::write(Ret.begin() + 2, Time, endianness::big);
    memcpy(Ret.begin() + 2 + sizeof Time, Hash.begin(), Hash.size());
    return Ret;
}

std::pair<int64_t, ObjCache::Hash> fromMetaKey(const char *Key)
{
    assert(Key[0] == METAKEY_TAG && Key[1] == 0);
    ObjCache::Hash Hash;
    auto Time = endian::read<int64_t>(Key + 2, endianness::big);
    memcpy(Hash.begin(), Key + 2 + sizeof Time, sizeof Hash);
    return {Time, Hash};
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

    uint64_t LookupStart = jl_hrtime();

    auto Hash = hashModule(M);
    auto ObjKey = toObjKey(Hash);

    MDBTxn Txn{Env, MDB_RDONLY};
    if (!Txn.Txn)
        return Compile();

    MDB_val Data;
    MDB_val Key = mdbVal(ObjKey);
    if (int Err = mdb_get(Txn.Txn, ObjCacheDbi, &Key, &Data)) {
        if (Err != MDB_NOTFOUND) {
            checkMDB(Err);
            return Compile();
        }
        Txn.abort();

        double LookupMs = (jl_hrtime() - LookupStart) / 1.0e6;

        NMiss.fetch_add(1, memory_order_relaxed);
        uint64_t CompileStart = jl_hrtime();
        auto Obj = Compile();
        double CompileMs = (jl_hrtime() - CompileStart) / 1.0e6;
        if (!Obj)
            return nullptr;

        if (LogFile) {
            std::unique_lock<std::mutex> Lock{LogMutex};
            fprintf(LogFile, "lookup,%s,%.3f,miss,%.3f,%zu,%zu\n",
                    llvm::toHex(Hash, true).c_str(), LookupMs, CompileMs,
                    Obj->getBufferSize(), Weight);
        }

        auto ObjCopy = llvm::MemoryBuffer::getMemBufferCopy(Obj->getBuffer());
        {
            std::unique_lock<std::mutex> Lock{Mutex};
            ObjQueue.push_back({Hash, std::move(ObjCopy)});
        }
        QueueCond.notify_one();

        return Obj;
    }

    {
        std::unique_lock<std::mutex> Lock{Mutex};
        ObjQueue.push_back({Hash, nullptr});
    }
    QueueCond.notify_one();

    auto Buf = std::make_unique<MDBMemoryBuffer>(
        std::move(Txn), llvm::StringRef{(const char *)Data.mv_data, Data.mv_size});
    NHit.fetch_add(1, memory_order_relaxed);
    NRead.fetch_add(Buf->getBufferSize(), memory_order_relaxed);

    double LookupMs = (jl_hrtime() - LookupStart) / 1.0e6;
    if (LogFile) {
        std::unique_lock<std::mutex> Lock{LogMutex};
        fprintf(LogFile, "lookup,%s,%.3f,hit,%zu,%zu\n", llvm::toHex(Hash, true).c_str(),
                LookupMs, Buf->getBufferSize(), Weight);
    }

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

    if (LogFile) {
        std::unique_lock<std::mutex> Lock{LogMutex};
        jl_safe_printf(
            "cache read:  %zu\ncache write: %zu\ncache hit:   %zu\ncache miss:  %zu\ncache evict: %zu\n",
            NRead.load(memory_order_relaxed), NWrite.load(memory_order_relaxed),
            NHit.load(memory_order_relaxed), NMiss.load(memory_order_relaxed),
            NEvicted.load(memory_order_relaxed));
    }
}

void ObjCache::writerThread()
{
    std::vector<std::pair<Hash, std::unique_ptr<llvm::MemoryBuffer>>> LocalQueue;
    bool Evict = false;
    while (1) {
        LocalQueue.clear();
        {
            std::unique_lock Lock{Mutex};
            QueueCond.wait(Lock, [this]() { return Exiting || !ObjQueue.empty(); });
            std::swap(LocalQueue, ObjQueue);
        }
        if (LocalQueue.empty())
            return;

        MDBTxn Txn{Env};
        if (!Txn.Txn)
            continue;

        if (!maybeEvictLRU(Txn, Evict))
            goto abort;
        Evict = false;

        uv_timeval_t Tv;
        uv_gettimeofday(&Tv);
        for (auto &[H, Obj] : LocalQueue) {
            auto ObjKey = toObjKey(H);
            MDB_val Key = mdbVal(ObjKey);
            if (Obj) {
                // Cache miss - write object
                MDB_val Data{Obj->getBufferSize(), (void *)Obj->getBufferStart()};
                if (int Err = mdb_put(Txn.Txn, ObjCacheDbi, &Key, &Data, 0)) {
                    // If this fails because of MDB_MAP_FULL, we can't find
                    // enough contiguous pages in the database.  Abort this
                    // cache write, but evict on the next iteration.
                    if (Err == MDB_MAP_FULL)
                        Evict = true;
                    else
                        checkMDB(Err);
                    goto abort;
                }
                NWrite.fetch_add(Obj->getBufferSize(), memory_order_relaxed);
                auto _ = std::move(Obj);
                if (!updateATime(Txn, H, Tv.tv_sec, true))
                    goto abort;
            }
            else {
                // Cache hit - update use time.  We set bit 62 to sort entries
                // that have been hit at least once after entries that have only
                // been written, so never-read entries will always be evicted
                // first.
                if (!updateATime(Txn, H, Tv.tv_sec | (1LL << 62), false))
                    goto abort;
            }
        }
        Txn.commit();
abort:;
    }
}

bool ObjCache::updateATime(MDBTxn &Txn, const Hash &Hash, int64_t Time, bool Fresh)
{
    auto ObjKey = toObjKey(Hash);
    MDB_val Key = mdbVal(ObjKey);
    if (!Fresh) {
        MDB_val OldData;
        if (int Err = mdb_get(Txn.Txn, ObjMetaDbi, &Key, &OldData)) {
            // This is possible if the atime update was queued, but we have
            // evicted the cache entry in the meantime.  We should not abort the
            // transaction in that case.
            if (Err == MDB_NOTFOUND)
                return true;
            checkMDB(Err);
            return false;
        }
        assert(OldData.mv_size == sizeof(int64_t));
        int64_t OldTime;
        memcpy(&OldTime, OldData.mv_data, sizeof OldTime);
        if (Time < OldTime + OBJCACHE_ATIME_GRANULARITY)
            return false;

        auto MetaKey = toMetaKey(OldTime, Hash);
        MDB_val Key2 = mdbVal(MetaKey);
        if (int Err = mdb_del(Txn.Txn, ObjMetaDbi, &Key2, nullptr)) {
            if (Err != MDB_MAP_FULL)
                checkMDB(Err);
            return false;
        }
    }

    MDB_val TimeData{sizeof Time, &Time};
    if (int Err = mdb_put(Txn.Txn, ObjMetaDbi, &Key, &TimeData, 0)) {
        if (Err != MDB_MAP_FULL)
            checkMDB(Err);
        return false;
    }

    auto MetaKey = toMetaKey(Time, Hash);
    MDB_val Key2 = mdbVal(MetaKey);
    MDB_val EmptyData{0, nullptr};
    if (int Err = mdb_put(Txn.Txn, ObjMetaDbi, &Key2, &EmptyData, 0)) {
        if (Err != MDB_MAP_FULL)
            checkMDB(Err);
        return false;
    }
    return true;
}

bool ObjCache::maybeEvictLRU(MDBTxn &Txn, bool Force)
{
    size_t EvictFromSize = (OBJCACHE_CAPACITY * OBJCACHE_EVICT_FROM) >> 31;
    auto ShouldEvict = [&]() {
        return Force ||
               dbiSize(Txn, ObjCacheDbi) + dbiSize(Txn, ObjMetaDbi) >= EvictFromSize;
    };
    if (!ShouldEvict())
        return true;

    MDB_cursor *MetaCur;
    checkMDB(mdb_cursor_open(Txn.Txn, ObjMetaDbi, &MetaCur));

    auto LowMeta = toMetaKey(0, {});
    MDB_val MetaKey = mdbVal(LowMeta);
    int Ret = mdb_cursor_get(MetaCur, &MetaKey, nullptr, MDB_SET_RANGE);
    while (!Ret && ShouldEvict() && ((const char *)MetaKey.mv_data)[0] == METAKEY_TAG) {
        Force = false;
        auto [Time, Hash] = fromMetaKey((const char *)MetaKey.mv_data);
        NEvicted.fetch_add(1, memory_order_relaxed);
        if (LogFile) {
            std::unique_lock<std::mutex> Lock{LogMutex};
            fprintf(LogFile, "evict,%s,,,,,\n", llvm::toHex(Hash, true).c_str());
        }

        auto ObjKey = toObjKey(Hash);
        MDB_val Key = mdbVal(ObjKey);
        checkMDB(mdb_del(Txn.Txn, ObjCacheDbi, &Key, nullptr));
        Key = mdbVal(ObjKey);
        checkMDB(mdb_del(Txn.Txn, ObjMetaDbi, &Key, nullptr));
        checkMDB(mdb_cursor_del(MetaCur, 0));
        Ret = mdb_cursor_get(MetaCur, &MetaKey, nullptr, MDB_NEXT);
        if (Ret != MDB_NOTFOUND)
            checkMDB(Ret);
    }

    // Start a new transaction to release our lock on all the pages that
    // are now free.
    Txn.commit();
    Txn = MDBTxn{Env};

    return true;
}

size_t ObjCache::dbiSize(MDBTxn &Txn, MDB_dbi Dbi)
{
    MDB_stat Stat;
    mdb_stat(Txn.Txn, ObjCacheDbi, &Stat);
    return (Stat.ms_leaf_pages + Stat.ms_branch_pages + Stat.ms_overflow_pages) *
           Stat.ms_psize;
}
