// This file is a part of Julia. License is MIT: https://julialang.org/license
#include "objcache.h"

#include <atomic>
#include <condition_variable>
#include <mutex>
#include <vector>

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

// Cache writes are opportunistic: if the writer thread cannot keep up (e.g.
// because it is blocked on a lock held by a misbehaving process), drop writes
// rather than queueing them without bound.
static constexpr size_t OBJCACHE_MAX_QUEUE_ENTRIES = 1024;
static constexpr size_t OBJCACHE_MAX_QUEUE_BYTES = 64 << 20;

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

    // LMDB 1.0 cannot open 0.9 data files, so keep their cache directories
    // distinct even within the same Julia minor-version depot.
    return (llvm::Twine(jl_string_ptr(DepotStr)) + "/cache/v" +
            llvm::Twine(JULIA_VERSION_MAJOR) + "." + llvm::Twine(JULIA_VERSION_MINOR) +
            "/objcache-lmdb1")
        .str();
}

// A process-wide switch, decided before the first module is compiled: modules
// compiled while this is true must never embed process-specific state (see
// optimizeModule), even if cache initialization subsequently fails.
static bool objCacheGloballyEnabled() JL_NOTSAFEPOINT
{
    const char *Enable = getenv("JULIA_OBJCACHE");
    if (Enable && !strcmp(Enable, "0"))
        return false;
#ifdef __HAIKU__
    // LMDB has no robust lock support on Haiku, so a process dying at the
    // wrong moment (which the detached writer thread makes more likely) could
    // leave the cache permanently locked.
    return false;
#else
    // Exiting processes with no live tasks can have mmap()ped files, which
    // triggers an assertion in rr if another process does a writev() to the fd.
    return !jl_running_under_rr(0);
#endif
}

#define checkMDB(Err) (checkMDB_(Err, __LINE__))

static int checkMDB_(int Err, int Line) JL_NOTSAFEPOINT
{
    if (Err == 0)
        return Err;
    jl_safe_printf("objcache error (%d): %s\n", Line, mdb_strerror(Err));
    return Err;
}

#define checkUV(Err) (checkUV_(Err, __LINE__))

static int checkUV_(int Err, int Line) JL_NOTSAFEPOINT
{
    if (Err == 0)
        return Err;
    jl_safe_printf("objcache thread error (%d): %s\n", Line, uv_strerror(Err));
    return Err;
}

namespace {
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
    int commit() JL_NOTSAFEPOINT
    {
        int Ret = mdb_txn_commit(Txn);
        Txn = nullptr;
        return Ret;
    }
    MDB_txn *Txn{};
};
} // anonymous namespace

template<typename T>
MDB_val mdbVal(T &x) JL_NOTSAFEPOINT
{
    return {sizeof x, (void *)&x};
}

enum class ObjCachePhase : uint8_t {
    Uninitialized,
    Initializing, // the writer thread is opening the database
    Ready,
    Failed,
    Exiting,
};

// All cache state is shared with the writer thread, which is detached (it may
// block indefinitely on locks shared with other, possibly misbehaving,
// processes, so it must never be joined) and can therefore outlive the JIT.
struct ObjCacheState {
    ~ObjCacheState() JL_NOTSAFEPOINT
    {
        if (ReadTxn)
            mdb_txn_abort(ReadTxn);
        if (Env)
            mdb_env_close(Env);
    }

    std::atomic<ObjCachePhase> Phase{ObjCachePhase::Uninitialized};
    std::string CachePath;

    // Written by the writer thread before Phase becomes Ready, constant after.
    MDB_env *Env = nullptr;
    MDB_dbi ObjCacheDbi{};
    MDB_dbi ObjMetaDbi{};
    size_t PageSize{};

    // A single pre-reserved read transaction, renewed and reset on each use so
    // that lookups never have to take LMDB's reader-table lock.  ReadInUse is
    // its lease.
    MDB_txn *ReadTxn = nullptr;
    std::atomic_flag ReadInUse = ATOMIC_FLAG_INIT;

    // Non-null MemoryBuffer -> cache miss, want to write new entry
    // Null MemoryBuffer     -> cache hit, want to update atime
    std::vector<std::pair<ObjCache::Hash, std::unique_ptr<llvm::MemoryBuffer>>> ObjQueue;
    size_t QueuedBytes = 0;
    std::mutex Mutex;
    std::mutex LogMutex;
    std::condition_variable QueueCond;
};

static void writerThreadEntry(void *Opaque) JL_NOTSAFEPOINT;

ObjCache::ObjCache()
    : GloballyEnabled(objCacheGloballyEnabled()), State(std::make_shared<ObjCacheState>())
{
}

ObjCache::~ObjCache()
{
    shutdown();
}

void ObjCache::initDB()
{
    // Read DEPOT_PATH before taking the mutex so we can enter a GC-unsafe
    // region to read the global.
    std::optional<std::string> CachePath;
    if (GloballyEnabled) {
        jl_task_t *ct = jl_current_task;
        int8_t gc_state = jl_gc_unsafe_enter(ct->ptls);
        CachePath = getCachePath();
        jl_gc_unsafe_leave(ct->ptls, gc_state);
    }

    std::unique_lock<std::mutex> Lock{State->Mutex};

    if (State->Phase.load(memory_order_acquire) != ObjCachePhase::Uninitialized)
        return;

    // All LMDB calls that can block on locks happen on the writer thread, so
    // that another (possibly misbehaving or dead) process sharing the cache
    // can never wedge this one.
    bool ThreadStarted = false;
#ifndef __clang_gcanalyzer__
    if (GloballyEnabled && CachePath) {
        State->CachePath = std::move(*CachePath);
        auto *Arg = new std::shared_ptr<ObjCacheState>(State);
        uv_thread_t WriterThread;
        if (checkUV(uv_thread_create(&WriterThread, writerThreadEntry, Arg)) == 0) {
            // Never joined: shutdown() must not block behind a writer that is
            // itself blocked on another process.  The thread holds a reference
            // to State and exits on its own once it observes Exiting.
            checkUV(uv_thread_detach(&WriterThread));
            ThreadStarted = true;
        }
        else {
            delete Arg;
        }
    }
#endif
    State->Phase.store(ThreadStarted ? ObjCachePhase::Initializing :
                                       ObjCachePhase::Failed,
                       memory_order_release);
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

static void enqueue(ObjCacheState &State, const ObjCache::Hash &Hash,
                    std::unique_ptr<llvm::MemoryBuffer> Obj) JL_NOTSAFEPOINT
{
    size_t Bytes = Obj ? Obj->getBufferSize() : 0;
    {
        std::unique_lock<std::mutex> Lock{State.Mutex};
        ObjCachePhase Phase = State.Phase.load(memory_order_relaxed);
        if (Phase != ObjCachePhase::Initializing && Phase != ObjCachePhase::Ready)
            return;
        // An empty queue always accepts, so that objects larger than the byte
        // limit can still be cached (one at a time).
        if (!State.ObjQueue.empty() &&
            (State.ObjQueue.size() >= OBJCACHE_MAX_QUEUE_ENTRIES ||
             State.QueuedBytes + Bytes > OBJCACHE_MAX_QUEUE_BYTES))
            return;
        State.QueuedBytes += Bytes;
        State.ObjQueue.push_back({Hash, std::move(Obj)});
    }
    State.QueueCond.notify_one();
}

std::unique_ptr<llvm::MemoryBuffer> ObjCache::get(llvm::Module &M, CompileFn Compile)
{
    auto doCompile = [&](bool Cacheable) JL_NOTSAFEPOINT_ENTER JL_NOTSAFEPOINT_LEAVE
        -> std::unique_ptr<llvm::MemoryBuffer> {
#ifndef __clang_gcanalyzer__
        return Compile(Cacheable);
#else
        return nullptr;
#endif
    };

    if (State->Phase.load(memory_order_acquire) == ObjCachePhase::Uninitialized)
        initDB();

    // Freeze the cacheability decision for this materialization: if the writer
    // thread finishes initializing concurrently, the module must not end up
    // mixing cache-portable and process-specific transforms.
    ObjCachePhase Phase = State->Phase.load(memory_order_acquire);
    bool Cacheable = Phase == ObjCachePhase::Initializing || Phase == ObjCachePhase::Ready;
    if (!Cacheable)
        return doCompile(false);

    size_t Weight = 0;
    if (LogFile) {
        for (auto &F : M.functions())
            for (auto &BB : F)
                Weight += BB.size();
    }

    uint64_t LookupStart = jl_hrtime();

    auto Hash = hashModule(M);
    auto ObjKey = toObjKey(Hash);

    // While Initializing, lookups always miss; the compiled objects are queued
    // and written in case the writer thread comes up successfully.
    std::unique_ptr<llvm::MemoryBuffer> Buf;
    if (Phase == ObjCachePhase::Ready &&
        !State->ReadInUse.test_and_set(memory_order_acquire)) {
        if (checkMDB(mdb_txn_renew(State->ReadTxn))) {
            // A transaction whose renewal failed may not be used again; leave
            // the lease taken so that no lookup ever touches it (it is aborted
            // by ~ObjCacheState).
            return doCompile(true);
        }
        MDB_val Data;
        MDB_val Key = mdbVal(ObjKey);
        int Err = mdb_get(State->ReadTxn, State->ObjCacheDbi, &Key, &Data);
        if (Err == 0)
            // Copy the object out so the lease is only ever held briefly.
            Buf = llvm::MemoryBuffer::getMemBufferCopy(
                llvm::StringRef{(const char *)Data.mv_data, Data.mv_size});
        else if (Err != MDB_NOTFOUND)
            checkMDB(Err);
        mdb_txn_reset(State->ReadTxn);
        State->ReadInUse.clear(memory_order_release);
    }

    if (!Buf) {
        double LookupMs = (jl_hrtime() - LookupStart) / 1.0e6;

        NMiss.fetch_add(1, memory_order_relaxed);
        uint64_t CompileStart = jl_hrtime();
        auto Obj = doCompile(true);
        double CompileMs = (jl_hrtime() - CompileStart) / 1.0e6;
        if (!Obj)
            return nullptr;

        if (LogFile) {
            std::unique_lock<std::mutex> Lock{State->LogMutex};
            fprintf(LogFile, "lookup,%s,%.3f,miss,%.3f,%zu,%zu\n",
                    llvm::toHex(Hash, true).c_str(), LookupMs, CompileMs,
                    Obj->getBufferSize(), Weight);
        }

        enqueue(*State, Hash, llvm::MemoryBuffer::getMemBufferCopy(Obj->getBuffer()));

        return Obj;
    }

    enqueue(*State, Hash, nullptr);

    NHit.fetch_add(1, memory_order_relaxed);
    NRead.fetch_add(Buf->getBufferSize(), memory_order_relaxed);

    double LookupMs = (jl_hrtime() - LookupStart) / 1.0e6;
    if (LogFile) {
        std::unique_lock<std::mutex> Lock{State->LogMutex};
        fprintf(LogFile, "lookup,%s,%.3f,hit,%zu,%zu\n", llvm::toHex(Hash, true).c_str(),
                LookupMs, Buf->getBufferSize(), Weight);
    }

    return Buf;
}

void ObjCache::shutdown()
{
    ObjCachePhase OldPhase;
    {
        std::unique_lock<std::mutex> Lock{State->Mutex};
        OldPhase = State->Phase.exchange(ObjCachePhase::Exiting, memory_order_acq_rel);
        // Pending writes are abandoned; the writer thread exits once it
        // observes the phase change (or immediately, if it was wedged and
        // is unblocked later).
        State->ObjQueue.clear();
        State->QueuedBytes = 0;
    }
    State->QueueCond.notify_all();

    if (LogFile && OldPhase != ObjCachePhase::Exiting) {
        std::unique_lock<std::mutex> Lock{State->LogMutex};
        jl_safe_printf(
            "cache read:  %zu\ncache write: %zu\ncache hit:   %zu\ncache miss:  %zu\ncache evict: %zu\n",
            NRead.load(memory_order_relaxed), NWrite.load(memory_order_relaxed),
            NHit.load(memory_order_relaxed), NMiss.load(memory_order_relaxed),
            NEvicted.load(memory_order_relaxed));
    }
}

static bool updateATime(ObjCacheState &State, MDBTxn &Txn, const ObjCache::Hash &Hash,
                        int64_t Time, bool Fresh) JL_NOTSAFEPOINT;
static bool maybeEvictLRU(ObjCacheState &State, MDBTxn &Txn,
                          size_t RoomFor) JL_NOTSAFEPOINT;

enum class OpenResult {
    Ready,
    Missing,
    Failed,
};

// Opens (Create == false) or creates (Create == true) the databases and, on
// success, publishes the handles and the reserved read transaction, moving
// Phase to Ready.
static OpenResult openDB(ObjCacheState &State, bool Create) JL_NOTSAFEPOINT
{
    MDBTxn Txn{State.Env, Create ? 0 : unsigned(MDB_RDONLY)};
    if (!Txn.Txn)
        return OpenResult::Failed;

    unsigned Flags = Create ? MDB_CREATE : 0;
    MDB_dbi ObjCacheDbi, ObjMetaDbi;
    int Err = mdb_dbi_open(Txn.Txn, "objcache", Flags, &ObjCacheDbi);
    if (Err == MDB_NOTFOUND)
        return OpenResult::Missing;
    if (checkMDB(Err))
        return OpenResult::Failed;
    Err = mdb_dbi_open(Txn.Txn, "objmeta", Flags, &ObjMetaDbi);
    if (Err == MDB_NOTFOUND)
        return OpenResult::Missing;
    if (checkMDB(Err))
        return OpenResult::Failed;

    int Version = OBJCACHE_SCHEMA;
    MDB_val Key = mdbVal("schema");
    MDB_val Ver = mdbVal(Version);
    if (Create) {
        Err = mdb_put(Txn.Txn, ObjMetaDbi, &Key, &Ver, MDB_NOOVERWRITE);
        if (Err != MDB_KEYEXIST && checkMDB(Err))
            return OpenResult::Failed;
    }
    else {
        Err = mdb_get(Txn.Txn, ObjMetaDbi, &Key, &Ver);
        if (Err == MDB_NOTFOUND)
            return OpenResult::Missing;
        if (checkMDB(Err))
            return OpenResult::Failed;
    }
    if (Ver.mv_size != sizeof Version ||
        memcmp(Ver.mv_data, &Version, sizeof Version) != 0)
        return OpenResult::Failed;

    MDB_stat Stat;
    if (checkMDB(mdb_stat(Txn.Txn, ObjCacheDbi, &Stat)))
        return OpenResult::Failed;
    if (checkMDB(Txn.commit()))
        return OpenResult::Failed;

    // Reserve one MDB_NOTLS reader slot up front; renewing a reset read
    // transaction reuses the slot without taking LMDB's reader-table lock, so
    // the lookup path in ObjCache::get never blocks on other processes.
    MDB_txn *ReadTxn;
    if (checkMDB(mdb_txn_begin(State.Env, nullptr, MDB_RDONLY, &ReadTxn)))
        return OpenResult::Failed;
    mdb_txn_reset(ReadTxn);

    {
        std::unique_lock<std::mutex> Lock{State.Mutex};
        if (State.Phase.load(memory_order_relaxed) != ObjCachePhase::Initializing) {
            // shutdown() got there first
            mdb_txn_abort(ReadTxn);
            return OpenResult::Failed;
        }
        State.ObjCacheDbi = ObjCacheDbi;
        State.ObjMetaDbi = ObjMetaDbi;
        State.PageSize = Stat.ms_psize;
        State.ReadTxn = ReadTxn;
        State.Phase.store(ObjCachePhase::Ready, memory_order_release);
    }
    return OpenResult::Ready;
}

static void writerThread(ObjCacheState &State) JL_NOTSAFEPOINT
{
    OpenResult Result = OpenResult::Failed;
    if (!checkMDB(mdb_env_create(&State.Env))) {
        checkMDB(mdb_env_set_maxreaders(State.Env, 510));
        checkMDB(mdb_env_set_maxdbs(State.Env, 128));
        checkMDB(mdb_env_set_mapsize(State.Env, OBJCACHE_CAPACITY * 2));
        llvm::sys::fs::create_directories(State.CachePath);
        int Err = mdb_env_open(State.Env, State.CachePath.c_str(),
                               MDB_NOSYNC | MDB_NOTLS, 0640);
        if (Err) {
            if (Err != ENOENT)
                checkMDB(Err);
        }
        else {
            // Recover locks held by processes that died holding them.
            int Dead;
            checkMDB(mdb_reader_check(State.Env, &Dead));
            // Prefer opening the databases read-only: if another process is
            // wedged holding the write lock, existing cache contents remain
            // usable (only the queued writes will never be committed).
            Result = openDB(State, false);
            if (Result == OpenResult::Missing)
                Result = openDB(State, true);
        }
    }
    if (Result != OpenResult::Ready) {
        if (State.Env) {
            mdb_env_close(State.Env);
            State.Env = nullptr;
        }
        std::unique_lock<std::mutex> Lock{State.Mutex};
        ObjCachePhase Expected = ObjCachePhase::Initializing;
        State.Phase.compare_exchange_strong(Expected, ObjCachePhase::Failed,
                                            memory_order_acq_rel);
        State.ObjQueue.clear();
        State.QueuedBytes = 0;
        return;
    }

    std::vector<std::pair<ObjCache::Hash, std::unique_ptr<llvm::MemoryBuffer>>> LocalQueue;
    while (1) {
        LocalQueue.clear();
        {
            std::unique_lock Lock{State.Mutex};
            State.QueueCond.wait(Lock, [&]() {
                return State.Phase.load(memory_order_relaxed) == ObjCachePhase::Exiting ||
                       !State.ObjQueue.empty();
            });
            if (State.Phase.load(memory_order_relaxed) == ObjCachePhase::Exiting)
                return;
            std::swap(LocalQueue, State.ObjQueue);
            State.QueuedBytes = 0;
        }

        MDBTxn Txn{State.Env};
        if (!Txn.Txn)
            continue;

        uv_timeval_t Tv;
        uv_gettimeofday(&Tv);
        bool OK = true, Exiting = false;
        for (auto &[H, Obj] : LocalQueue) {
            if ((Exiting = State.Phase.load(memory_order_relaxed) ==
                           ObjCachePhase::Exiting))
                break;
            auto ObjKey = toObjKey(H);
            MDB_val Key = mdbVal(ObjKey);
            if (Obj) {
                // Cache miss - write object
                if (!(OK = maybeEvictLRU(State, Txn, Obj->getBufferSize())))
                    break;
                MDB_val Data{Obj->getBufferSize(), (void *)Obj->getBufferStart()};
                if (int Err = mdb_put(Txn.Txn, State.ObjCacheDbi, &Key, &Data, 0)) {
                    // If this fails because of MDB_MAP_FULL, we can't find
                    // enough contiguous pages in the database.  Skip it.
                    if (Err != MDB_MAP_FULL)
                        checkMDB(Err);
                    OK = false;
                    break;
                }
                NWrite.fetch_add(Obj->getBufferSize(), memory_order_relaxed);
                auto _ = std::move(Obj);
                if (!(OK = updateATime(State, Txn, H, Tv.tv_sec, true)))
                    break;
            }
            else {
                // Cache hit - update use time.  We set bit 62 to sort entries
                // that have been hit at least once after entries that have only
                // been written, so never-read entries will always be evicted
                // first.
                if (!(OK = updateATime(State, Txn, H, Tv.tv_sec | (1LL << 62), false)))
                    break;
            }
        }
        // On failure the transaction is aborted (by ~MDBTxn) and the remaining
        // entries are dropped; cache writes are only ever best-effort.
        if (OK)
            checkMDB(Txn.commit());
        if (Exiting)
            return;
    }
}

static void writerThreadEntry(void *Opaque)
{
    auto *Arg = static_cast<std::shared_ptr<ObjCacheState> *>(Opaque);
    std::shared_ptr<ObjCacheState> State = std::move(*Arg);
    delete Arg;
    writerThread(*State);
}

static bool updateATime(ObjCacheState &State, MDBTxn &Txn, const ObjCache::Hash &Hash,
                        int64_t Time, bool Fresh)
{
    auto ObjKey = toObjKey(Hash);
    MDB_val Key = mdbVal(ObjKey);
    MDB_val OldData;
    if (int Err = mdb_get(Txn.Txn, State.ObjMetaDbi, &Key, &OldData)) {
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
    else if (OldData.mv_size != sizeof(int64_t)) {
        // Corrupt entry: overwrite it below.  Its metakey, which we can no
        // longer identify, is eventually cleaned up by eviction.
    }
    else {
        int64_t OldTime;
        memcpy(&OldTime, OldData.mv_data, sizeof OldTime);
        if (Time < OldTime + OBJCACHE_ATIME_GRANULARITY)
            return true;

        auto MetaKey = toMetaKey(OldTime, Hash);
        MDB_val Key2 = mdbVal(MetaKey);
        if (int Err = mdb_del(Txn.Txn, State.ObjMetaDbi, &Key2, nullptr)) {
            if (Err != MDB_MAP_FULL)
                checkMDB(Err);
            return false;
        }
    }

    MDB_val TimeData{sizeof Time, &Time};
    if (int Err = mdb_put(Txn.Txn, State.ObjMetaDbi, &Key, &TimeData, 0)) {
        if (Err != MDB_MAP_FULL)
            checkMDB(Err);
        return false;
    }

    auto MetaKey = toMetaKey(Time, Hash);
    MDB_val Key2 = mdbVal(MetaKey);
    MDB_val EmptyData{0, nullptr};
    if (int Err = mdb_put(Txn.Txn, State.ObjMetaDbi, &Key2, &EmptyData, 0)) {
        if (Err != MDB_MAP_FULL)
            checkMDB(Err);
        return false;
    }
    return true;
}

static size_t dbiSize(MDBTxn &Txn, MDB_dbi Dbi) JL_NOTSAFEPOINT
{
    MDB_stat Stat;
    mdb_stat(Txn.Txn, Dbi, &Stat);
    return (Stat.ms_leaf_pages + Stat.ms_branch_pages + Stat.ms_overflow_pages) *
           Stat.ms_psize;
}

static bool maybeEvictLRU(ObjCacheState &State, MDBTxn &Txn, size_t RoomFor)
{
    RoomFor = LLT_ALIGN(RoomFor, State.PageSize);
    auto Used = [&]() JL_NOTSAFEPOINT {
        return dbiSize(Txn, State.ObjCacheDbi) + dbiSize(Txn, State.ObjMetaDbi) + RoomFor;
    };
    auto ShouldEvict = [&]() JL_NOTSAFEPOINT {
        size_t Threshold = OBJCACHE_CAPACITY * 3 / 4;
        return Used() > Threshold;
    };

    if (Used() <= OBJCACHE_CAPACITY)
        return true;

    MDB_cursor *MetaCur;
    if (checkMDB(mdb_cursor_open(Txn.Txn, State.ObjMetaDbi, &MetaCur)))
        return false;

    auto LowMeta = toMetaKey(0, {});
    MDB_val MetaKey = mdbVal(LowMeta);
    int Ret = mdb_cursor_get(MetaCur, &MetaKey, nullptr, MDB_SET_RANGE);
    while (!Ret && ShouldEvict() && MetaKey.mv_size == METAKEY_SIZE &&
           ((const char *)MetaKey.mv_data)[0] == METAKEY_TAG) {
        auto [Time, Hash] = fromMetaKey((const char *)MetaKey.mv_data);
        // (Not logged to LogFile: the detached writer could race stdio
        // teardown when the process exits.)
        NEvicted.fetch_add(1, memory_order_relaxed);

        auto ObjKey = toObjKey(Hash);
        MDB_val Key = mdbVal(ObjKey);
        checkMDB(mdb_del(Txn.Txn, State.ObjCacheDbi, &Key, nullptr));
        Key = mdbVal(ObjKey);
        checkMDB(mdb_del(Txn.Txn, State.ObjMetaDbi, &Key, nullptr));
        checkMDB(mdb_cursor_del(MetaCur, 0));
        Ret = mdb_cursor_get(MetaCur, &MetaKey, nullptr, MDB_NEXT);
        if (Ret != MDB_NOTFOUND)
            checkMDB(Ret);
    }

    // Start a new transaction to release our lock on all the pages that
    // are now free.
    checkMDB(Txn.commit());
    Txn = MDBTxn{State.Env};

    return Txn.Txn != nullptr;
}
