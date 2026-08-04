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
        fprintf(stderr, "objcache: invalid value for %s: %s\n", Name, S);
        return Default;
    }
    return (uint64_t)V;
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

static FILE *getLogFile()
{
    const char *Path = getenv("JULIA_OBJCACHE_LOG");
    if (!Path)
        return nullptr;
    FILE *F = fopen(Path, "a");
    if (!F) {
        jl_safe_printf("objcache: failed to open log file %s\n", Path);
        return nullptr;
    }
    return F;
}

static FILE *LogFile = getLogFile();

static std::optional<std::string> getCachePath() JL_CANSAFEPOINT
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

    // LMDB 1.0 cannot open data files created by LMDB 0.9, so use a
    // different directory than the LMDB 0.9 based versions of this code.
    return (llvm::Twine(jl_string_ptr(DepotStr)) + "/cache/v" +
            llvm::Twine(JULIA_VERSION_MAJOR) + "." + llvm::Twine(JULIA_VERSION_MINOR) +
            "/objcache-lmdb1")
        .str();
}

#define checkMDB(Err) (checkMDB_(Err, __LINE__))

static int checkMDB_(int Err, int Line) JL_NOTSAFEPOINT
{
    if (Err == 0)
        return Err;
    jl_safe_printf("objcache error (%d): %s\n", Line, mdb_strerror(Err));
    return Err;
}

class MDBTxn {
public:
    MDBTxn(MDB_env *Env, unsigned Flags = 0) JL_NOTSAFEPOINT
    {
        if (checkMDB(mdb_txn_begin(Env, nullptr, Flags, &Txn)))
            Txn = nullptr;
    }
    ~MDBTxn() JL_NOTSAFEPOINT
    {
        if (Txn)
            mdb_txn_abort(Txn);
    }
    MDBTxn(const MDBTxn &) = delete;
    MDBTxn &operator=(const MDBTxn &) = delete;
    MDBTxn(MDBTxn &&RHS) JL_NOTSAFEPOINT : Txn(std::exchange(RHS.Txn, nullptr)) {}
    MDBTxn &operator=(MDBTxn &&RHS) JL_NOTSAFEPOINT
    {
        std::swap(Txn, RHS.Txn);
        return *this;
    }
    void abort() JL_NOTSAFEPOINT
    {
        mdb_txn_abort(Txn);
        Txn = nullptr;
    }
    int commit() JL_NOTSAFEPOINT
    {
        int Ret = mdb_txn_commit(Txn);
        Txn = nullptr;
        return Ret;
    }
    MDB_txn *Txn{};
};

template<typename T>
MDB_val mdbVal(T &x) JL_NOTSAFEPOINT
{
    return {sizeof x, (void *)&x};
}

namespace {
class MDBMemoryBuffer : public llvm::MemoryBuffer {
public:
    MDBMemoryBuffer(MDBTxn Txn, llvm::StringRef Data) JL_NOTSAFEPOINT : Txn(std::move(Txn))
    {
        init(Data.begin(), Data.end(), false);
    }
    BufferKind getBufferKind() const override { return MemoryBuffer_MMap; }

private:
    MDBTxn Txn;
};
} // anonymous namespace

void ObjCache::initDB()
{
    // Read DEPOT_PATH before taking ObjCache::Lock so we can enter a GC-unsafe
    // region to read the global.
    jl_task_t *ct = jl_current_task;
    int8_t gc_state = jl_gc_unsafe_enter(ct->ptls);
    const char *Enable = getenv("JULIA_OBJCACHE");
    auto CachePath = getCachePath();
    jl_gc_unsafe_leave(ct->ptls, gc_state);

    std::unique_lock<std::mutex> Lock{Mutex};

    if (Initialized.load(memory_order_acquire))
        return;

    if (!CachePath || (Enable && !strcmp(Enable, "0")))
        goto done;

    // Exiting processes with no live tasks can have mmap()ped files, which
    // triggers an assertion in rr if another process does a writev() to the fd.
    if (jl_running_under_rr(0))
        goto done;

    if (checkMDB(mdb_env_create(&Env))) {
        Env = nullptr;
        goto done;
    }
    checkMDB(mdb_env_set_maxreaders(Env, 510));
    checkMDB(mdb_env_set_maxdbs(Env, 128));
    checkMDB(mdb_env_set_mapsize(Env, OBJCACHE_CAPACITY * 2));
    llvm::sys::fs::create_directories(*CachePath);
    if (int Err = mdb_env_open(Env, CachePath->c_str(), MDB_NOSYNC | MDB_NOTLS, 0640)) {
        // These two are expected conditions, not errors: record them so the
        // REPL banner can explain why the cache is disabled.
        if (Err == MDB_REMOTE_FS)
            DisabledNotice = "the cache directory is on a network filesystem";
        else if (Err == MDB_PIDNS_MISMATCH)
            DisabledNotice = "it is in use by a process in a different pid namespace";
        // EPERM/EACCES: sandboxes (e.g. sandbox-exec on macOS CI) may deny
        // access to the SysV semaphores LMDB uses on Apple platforms; the
        // cache cannot work in such environments, so disable it quietly.
        else if (Err != ENOENT && Err != EPERM && Err != EACCES)
            checkMDB(Err);
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

        MDB_stat Stat;
        checkMDB(mdb_stat(Txn.Txn, ObjCacheDbi, &Stat));
        PageSize = Stat.ms_psize;

        int Version = OBJCACHE_SCHEMA;
        MDB_val Key = mdbVal("schema");
        MDB_val Ver = mdbVal(Version);
        int Err = mdb_put(Txn.Txn, ObjMetaDbi, &Key, &Ver, MDB_NOOVERWRITE);
        if (Err == MDB_KEYEXIST && *static_cast<int *>(Ver.mv_data) != OBJCACHE_SCHEMA)
            goto cleanup_env;

        checkMDB(Txn.commit());
    }


#ifndef __clang_gcanalyzer__
    uv_thread_create(
        &WriterThread, [](void *arg) { static_cast<ObjCache *>(arg)->writerThread(); },
        this);
#endif
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

static ObjCache::Hash hashModule(const llvm::Module &M) JL_NOTSAFEPOINT
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

/*
 * The objcache is stored using two LMDB databases.  The "objcache" database
 * contains an entry for every cached object, with the key being an objkey
 * (O\0<hash>) and the value being the object file contents.  The "objmeta"
 * database contains two entries for every cached object, an objkey with the
 * access time (up-to-date to within OBJCACHE_ATIME_GRANULARITY seconds), and a
 * metakey (M\0<big endian time><hash>) with an empty value.  The purpose of
 * having two types of keys in the objmeta database is to make the two
 * fundamental operations fast:
 * - Given an hash, update or delete the access time.
 * - Retrieve the hashes of the N least recently used cache entries.
 *
 * objcache
 *   ObjKey(Hash1) => <data>
 *   ObjKey(Hash2) => <data>
 *
 * objmeta
 *   MetaKey(ATime2, Hash2)
 *   MetaKey(ATime1, Hash1)
 *   ObjKey(Hash1)          => ATime1
 *   ObjKey(Hash2)          => ATime2
 */

constexpr size_t OBJKEY_SIZE = 2 + sizeof(ObjCache::Hash);
constexpr size_t METAKEY_SIZE = 2 + sizeof(int64_t) + sizeof(ObjCache::Hash);

constexpr char OBJKEY_TAG = 'O';
constexpr char METAKEY_TAG = 'M';

static std::array<uint8_t, OBJKEY_SIZE> toObjKey(const ObjCache::Hash &Hash) JL_NOTSAFEPOINT
{
    std::array<uint8_t, OBJKEY_SIZE> Ret;
    Ret[0] = OBJKEY_TAG;
    Ret[1] = 0;
    memcpy(Ret.begin() + 2, Hash.begin(), Hash.size());
    return Ret;
}

static std::array<uint8_t, METAKEY_SIZE> toMetaKey(int64_t Time,
                                                   const ObjCache::Hash &Hash) JL_NOTSAFEPOINT
{
    std::array<uint8_t, METAKEY_SIZE> Ret;
    Ret[0] = METAKEY_TAG;
    Ret[1] = 0;
    endian::write(Ret.begin() + 2, Time, endianness::big);
    memcpy(Ret.begin() + 2 + sizeof Time, Hash.begin(), Hash.size());
    return Ret;
}

static std::pair<int64_t, ObjCache::Hash> fromMetaKey(const char *Key) JL_NOTSAFEPOINT
{
    assert(Key[0] == METAKEY_TAG && Key[1] == 0);
    ObjCache::Hash Hash;
    auto Time = endian::read<int64_t>(Key + 2, endianness::big);
    memcpy(Hash.begin(), Key + 2 + sizeof Time, sizeof Hash);
    return {Time, Hash};
}

std::unique_ptr<llvm::MemoryBuffer> ObjCache::get(llvm::Module &M, CompileFn Compile) JL_CANSAFEPOINT_ENTER_LEAVE
{
    auto doCompile = [&]() JL_CANSAFEPOINT_ENTER_LEAVE
        -> std::unique_ptr<llvm::MemoryBuffer> {
#ifndef __clang_gcanalyzer__
        return Compile();
#else
        return nullptr;
#endif
    };

    if (!Initialized.load(memory_order_acquire))
        initDB();

    if (!Env)
        return doCompile();

    size_t Weight = 0;
    if (LogFile) {
        for (auto &F : M.functions())
            for (auto &BB : F)
                Weight += BB.size();
    }

    uint64_t LookupStart = jl_hrtime();

    auto Hash = hashModule(M);
    auto ObjKey = toObjKey(Hash);

    MDBTxn Txn{Env, MDB_RDONLY};
    if (!Txn.Txn)
        return doCompile();

    MDB_val Data;
    MDB_val Key = mdbVal(ObjKey);
    if (int Err = mdb_get(Txn.Txn, ObjCacheDbi, &Key, &Data)) {
        if (Err != MDB_NOTFOUND) {
            checkMDB(Err);
            return doCompile();
        }
        Txn.abort();

        double LookupMs = (jl_hrtime() - LookupStart) / 1.0e6;

        NMiss.fetch_add(1, memory_order_relaxed);
        uint64_t CompileStart = jl_hrtime();
        auto Obj = doCompile();
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

const char *ObjCache::disabledNotice()
{
    if (!Initialized.load(memory_order_acquire))
        initDB();
    return DisabledNotice;
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
    while (1) {
        bool IsExiting;
        LocalQueue.clear();
        {
            std::unique_lock Lock{Mutex};
            QueueCond.wait(Lock, [this]() { return Exiting || !ObjQueue.empty(); });
            std::swap(LocalQueue, ObjQueue);
            IsExiting = Exiting;
        }
        if (LocalQueue.empty())
            return;

        MDBTxn Txn{Env};
        if (!Txn.Txn)
            continue;

        uv_timeval_t Tv;
        uv_gettimeofday(&Tv);
        auto I = LocalQueue.begin();
        for (; I < LocalQueue.end(); ++I) {
            auto &[H, Obj] = *I;
            auto ObjKey = toObjKey(H);
            MDB_val Key = mdbVal(ObjKey);
            if (Obj) {
                // Cache miss - write object. Once shutdown has been
                // requested, don't start eviction work just to make room for
                // new entries — a full cache would make exit arbitrarily
                // slow. Drop the write instead; it only costs a future miss.
                if (!maybeEvictLRU(Txn, Obj->getBufferSize(), /*AllowEvict*/ !IsExiting)) {
                    if (IsExiting)
                        continue;
                    goto abort;
                }
                MDB_val Data{Obj->getBufferSize(), (void *)Obj->getBufferStart()};
                if (int Err = mdb_put(Txn.Txn, ObjCacheDbi, &Key, &Data, 0)) {
                    // If this fails because of MDB_MAP_FULL, we can't find
                    // enough contiguous pages in the database.  Skip it.
                    if (Err != MDB_MAP_FULL)
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
        continue;
abort:;
        std::unique_lock Lock{Mutex};
        std::move(++I, LocalQueue.end(), std::back_inserter(ObjQueue));
    }
}

bool ObjCache::updateATime(MDBTxn &Txn, const Hash &Hash, int64_t Time, bool Fresh)
{
    auto ObjKey = toObjKey(Hash);
    MDB_val Key = mdbVal(ObjKey);
    MDB_val OldData;
    if (int Err = mdb_get(Txn.Txn, ObjMetaDbi, &Key, &OldData)) {
        if (Err != MDB_NOTFOUND) {
            checkMDB(Err);
            return false;
        }
        // This is possible if the atime update was queued, but we have
        // evicted the cache entry in the meantime.  We should not abort the
        // transaction in that case.
        if (!Fresh)
            return true;
    }
    else {
        assert(OldData.mv_size == sizeof(int64_t));
        int64_t OldTime;
        memcpy(&OldTime, OldData.mv_data, sizeof OldTime);
        if (Time < OldTime + OBJCACHE_ATIME_GRANULARITY)
            return true;

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

bool ObjCache::maybeEvictLRU(MDBTxn &Txn, size_t RoomFor, bool AllowEvict)
{
    RoomFor = LLT_ALIGN(RoomFor, PageSize);
    auto Used = [&]() {
        return dbiSize(Txn, ObjCacheDbi) + dbiSize(Txn, ObjMetaDbi) + RoomFor;
    };
    auto ShouldEvict = [&]() {
        size_t Threshold = OBJCACHE_CAPACITY * 3 / 4;
        return Used() > Threshold;
    };

    if (Used() <= OBJCACHE_CAPACITY)
        return true;
    if (!AllowEvict)
        return false;

    // Evict in bounded batches, committing between them. Freeing an
    // unbounded number of pages in one transaction makes the commit's
    // freelist bookkeeping (mdb_freelist_save) pathologically slow on a
    // full map: it needs a large contiguous overflow run for the free-page
    // record, and searching a huge fragmented freelist for it is
    // effectively quadratic.
    static const size_t EVICT_BATCH = 64;
    bool More = true;
    while (More) {
        More = false;
        MDB_cursor *MetaCur;
        if (checkMDB(mdb_cursor_open(Txn.Txn, ObjMetaDbi, &MetaCur)))
            return false;

        auto LowMeta = toMetaKey(0, {});
        MDB_val MetaKey = mdbVal(LowMeta);
        int Ret = mdb_cursor_get(MetaCur, &MetaKey, nullptr, MDB_SET_RANGE);
        size_t Batch = 0;
        while (!Ret && ShouldEvict() && ((const char *)MetaKey.mv_data)[0] == METAKEY_TAG) {
            if (++Batch > EVICT_BATCH) {
                More = true;
                break;
            }
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

        // Start a new transaction to release our lock on all the pages
        // that are now free.
        if (Txn.commit())
            return false;
        Txn = MDBTxn{Env};
        if (!Txn.Txn)
            return false;
    }

    return true;
}

// Test support: synthesize cache entries directly so tests can construct
// specific cache shapes (e.g. a full map with a large fragmented freelist)
// without compiling the corresponding volume of real code. Writes up to
// NEntries entries of EntrySize bytes each, stopping early once the map
// fills, then, if PunchPeriod is nonzero, deletes every PunchPeriod-th
// written entry to scatter free pages into the freelist. Both phases commit
// in bounded batches (the punch phase shrinks its batch when a commit does
// not fit into the remaining space). Returns the number of entries left in
// the cache, or -1 if the cache is disabled or an unexpected error occurred.
int64_t ObjCache::testPopulate(uint64_t NEntries, uint64_t EntrySize, uint64_t PunchPeriod)
{
    disabledNotice(); // force initialization
    if (!isEnabled())
        return -1;

    std::vector<uint8_t> Data(EntrySize, 0xab);
    auto NthHash = [](uint64_t I) JL_NOTSAFEPOINT {
        Hash H{};
        memcpy(H.data(), &I, sizeof I);
        return H;
    };

    const uint64_t BATCH = 256;
    uint64_t Written = 0;
    bool Full = false;
    while (Written < NEntries && !Full) {
        MDBTxn Txn{Env};
        if (!Txn.Txn)
            return -1;
        uint64_t Base = Written;
        for (uint64_t J = 0; J < BATCH && Base + J < NEntries; J++) {
            uint64_t I = Base + J;
            auto H = NthHash(I);
            auto ObjKey = toObjKey(H);
            MDB_val Key = mdbVal(ObjKey);
            MDB_val Val{Data.size(), Data.data()};
            int Err = mdb_put(Txn.Txn, ObjCacheDbi, &Key, &Val, 0);
            int64_t Time = (int64_t)I;
            if (!Err) {
                MDB_val TimeVal{sizeof Time, &Time};
                Key = mdbVal(ObjKey);
                Err = mdb_put(Txn.Txn, ObjMetaDbi, &Key, &TimeVal, 0);
            }
            if (!Err) {
                auto MetaKey = toMetaKey(Time, H);
                MDB_val Key2 = mdbVal(MetaKey);
                MDB_val Empty{0, nullptr};
                Err = mdb_put(Txn.Txn, ObjMetaDbi, &Key2, &Empty, 0);
            }
            if (Err) {
                if (Err != MDB_MAP_FULL)
                    checkMDB(Err);
                Full = true;
                break;
            }
        }
        uint64_t InBatch = std::min(BATCH, NEntries - Base);
        if (Full)
            InBatch = 0; // partially applied batch aborts with the txn below
        if (InBatch == 0) {
            // Nothing (fully) written; drop the transaction.
            break;
        }
        if (Txn.commit()) {
            Full = true;
            break;
        }
        Written += InBatch;
    }

    uint64_t Deleted = 0;
    if (PunchPeriod) {
        uint64_t I = 0;
        uint64_t PunchBatch = BATCH;
        while (I < Written && PunchBatch) {
            MDBTxn Txn{Env};
            if (!Txn.Txn)
                return -1;
            uint64_t Start = I, InBatch = 0;
            for (uint64_t J = 0; J < PunchBatch && I < Written; I += PunchPeriod, J++) {
                auto H = NthHash(I);
                auto ObjKey = toObjKey(H);
                MDB_val Key = mdbVal(ObjKey);
                int Err = mdb_del(Txn.Txn, ObjCacheDbi, &Key, nullptr);
                if (!Err) {
                    Key = mdbVal(ObjKey);
                    mdb_del(Txn.Txn, ObjMetaDbi, &Key, nullptr);
                    auto MetaKey = toMetaKey((int64_t)I, H);
                    MDB_val Key2 = mdbVal(MetaKey);
                    mdb_del(Txn.Txn, ObjMetaDbi, &Key2, nullptr);
                    InBatch++;
                }
                else if (Err != MDB_NOTFOUND && Err != MDB_MAP_FULL) {
                    checkMDB(Err);
                    return -1;
                }
            }
            if (Txn.commit()) {
                // The commit itself did not fit (copy-on-write pages); retry
                // this stretch with a smaller batch.
                I = Start;
                PunchBatch /= 2;
                continue;
            }
            Deleted += InBatch;
        }
    }
    return (int64_t)(Written - Deleted);
}

size_t ObjCache::dbiSize(MDBTxn &Txn, MDB_dbi Dbi)
{
    MDB_stat Stat;
    mdb_stat(Txn.Txn, Dbi, &Stat);
    return (Stat.ms_leaf_pages + Stat.ms_branch_pages + Stat.ms_overflow_pages) *
           Stat.ms_psize;
}
