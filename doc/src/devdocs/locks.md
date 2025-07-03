# [Proper maintenance and care of multi-threading locks](@id Proper-maintenance-and-care-of-multi-threading-locks)

The following strategies are used to ensure that the code is dead-lock free (generally by addressing
the 4th Coffman condition: circular wait).

1. structure code such that only one lock will need to be acquired at a time
2. always acquire shared locks in the same order, as given by the table below
3. avoid constructs that expect to need unrestricted recursion

## Types of locks

`uv_mutex_t` (or `std::mutex`) is a wrapper around platform-specific locks
(`pthread_mutex_t` on Unix, `CRITICAL_SECTION` on Windows).  It may cause the
current OS thread to block, is not reentrant, and is not a safepoint.

`jl_mutex_t` is a reentrant spinlock.  `jl_mutex_t`s acquired in a `try` block
will be unlocked when we leave the block, either by reaching the end or catching
an exception.  `JL_LOCK`/`JL_UNLOCK` are safepoints, while
`JL_LOCK_NOGC`/`JL_UNLOCK_NOGC` are not.  `jl_mutex_t` must not be held across
task switches.

## Lock hierarchy

Below are all of the locks that exist in the system and the mechanisms for using
them that avoid the potential for deadlocks (no Ostrich algorithm allowed here).
Except in the special cases where a rule for avoiding deadlock is given, no lock
of a lower level may acquire a lock at a higher level.

### Level 1

No other lock may be acquired when one of these locks is held.  As a result, the
code must not do any allocation or hit any safepoints. Note that there are
safepoints when doing allocation, enabling/disabling GC, entering/restoring
exception frames, and taking/releasing locks.

* `safepoint_lock` (`uv_mutex_t`)
  !!! danger

      This lock is acquired implicitly by `JL_LOCK` and `JL_UNLOCK`. Use the
      `_NOGC` variants to avoid that for level 1 locks.

* `shared_map_lock.mtx` (`uv_mutex_t`)
* `finalizers_lock` (`jl_mutex_t`)
* `gc_pages_lock` (`uv_mutex_t`)
* `gc_perm_lock` (`uv_mutex_t`)
* `gc_queue_observer_lock` (`uv_mutex_t`)
* `gc_threads_lock` (`uv_mutex_t`)
* `flisp_lock` (`uv_mutex_t`)
  !!! note
      flisp itself is already threadsafe; this lock only protects the
      `jl_ast_context_list_t` pool.  Likewise, the `ResourcePool<?>::mutexes`
      just protect the associated resource pool.

* `jl_in_stackwalk` (`uv_mutex_t`, Win32 only)
* `ResourcePool<?>.mutex` (`std::mutex`)
* `RLST_mutex` (`std::mutex`)
* `llvm_printing_mutex` (`std::mutex`)
* `jl_locked_stream.mutex` (`std::mutex`)
* `debuginfo_asyncsafe` (`uv_rwlock_t`)
* `profile_show_peek_cond_lock` (`jl_mutex_t`)
* `trampoline_lock` (`uv_mutex_t`)
* `bt_data_prof_lock` (`uv_mutex_t`)
* `jl_ptls_t.sleep_lock` (`uv_mutex_t`)
* `tls_lock` (`uv_mutex_t`)
* `page_profile_lock` (`uv_mutex_t`)
* `symtab_lock` (`uv_mutex_t`)
* `engine_lock` (`std::mutex`)

### Level 2

* `global_roots_lock`
* `jl_module_t.lock`
* `newly_inferred_mutex`
* `JLDebuginfoPlugin.PluginMutex` (`std::mutex`)
* `precompile_field_replace_lock`
* `live_tasks_lock` (`uv_mutex_t`)
* `heapsnapshot_lock`
* `jitlock`

### Level 3

* `jl_method_t.writelock`
* `typecache_lock`
* `libmap_lock`

### Level 4

* `jl_methcache_t.writelock`

### Level 5

* `jl_methtable_t.writelock`

### Level 6

* `JuliaOJIT::EmissionMutex` (`std::recursive_mutex`)

* `jl_modules_mutex`

* `jl_uv_mutex` (known as `iolock` from Julia)
  !!! danger
      Doing any I/O (for example, printing warning messages or debug information)
      while holding any other lock listed above may result in pernicious and
      hard-to-find deadlocks.

The following is a level 7 lock, which can only be acquired when not holding any other locks:

>   * world_counter_lock


* Individual `ThreadSynchronizer` locks
  !!! danger
      This may continue to be held after releasing the iolock, or acquired
      without it, but be very careful to never attempt to acquire the iolock
      while holding it.

* `Libdl.LazyLibrary.lock` (`ReentrantLock`)

* `orc::ThreadSafeContext`
  !!! note
      Acquiring a TSCtx should only be done from the JIT's pool of TSCtx's, and
      all locks on that TSCtx should be released prior to returning it to the
      pool. If multiple TSCtx locks must be acquired at the same time (due to
      recursive compilation), then locks should be acquired in the order that
      the TSCtxs were borrowed from the pool.

* `cfun_lock`

### Level 7
* `world_counter_lock`
* `precomp_statement_out_lock`
* `dispatch_statement_out_lock`

## Exceptions to the lock hierarchy

Ordinarily, it is forbidden to acquire locks of equal level to a lock already
held.  In these specific cases we use a special protocol for acquiring locks at
the same level:

- `jl_method_t.writelock`

  Invalidation acquires the lock for every method during its depth-first search
  for backedges.  To avoid deadlocks, we must already hold `world_counter_lock`
  before acquiring multiple `jl_method_t.writelock`s.
