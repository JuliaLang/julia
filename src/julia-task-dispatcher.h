// This file is a part of Julia. License is MIT: https://julialang.org/license

namespace {

using namespace llvm::orc;

template <typename U> struct future_value_storage {
  // Union disables default construction/destruction semantics, allowing us to
  // use placement new/delete for precise control over value lifetime
  union {
    U value_;
  };

  future_value_storage() {}
  ~future_value_storage() {}
};

template <> struct future_value_storage<void> {
  // No value_ member for void
};

struct JuliaTaskDispatcher : public TaskDispatcher {
    /// Forward declarations
    class future_base;
    void dispatch(std::unique_ptr<Task> T) override;
    void shutdown() override;
    void work_until(future_base &F);

protected:
  void process_tasks(jl_unique_gcsafe_lock &Lock) JL_NOTSAFEPOINT_ENTER JL_NOTSAFEPOINT_LEAVE;

private:
  /// C++ does not support non-static thread_local variables, so this needs to
  /// store both the task and the associated dispatcher queue so that shutdown
  /// can wait for the correct tasks to finish.
  SmallVector<std::unique_ptr<Task>> TaskQueue;
  std::mutex DispatchMutex;
  std::condition_variable WorkFinishedCV;

public:

/// @name ORC Promise/Future Classes
///
/// ORC-aware promise/future implementation that integrates with the
/// TaskDispatcher system to allow efficient cooperative multitasking while
/// waiting for results (with certain limitations on what can be awaited).
/// Together they provide building blocks for a full async/await-like runtime
/// for llvm that supports multiple threads.
///
/// Unlike std::promise/std::future alone, these classes can help dispatch other
/// tasks while waiting, preventing deadlocks and improving overall system
/// throughput. They have a similar API, though with some important differences
/// and some features simply not currently implemented.
///
/// @{

template <typename T> class promise;
template <typename T> class future;

/// Status for future/promise state
enum class FutureStatus : uint8_t { NotReady = 0, Ready = 1 };

/// @}

/// Type-erased base class for futures, generally for scheduler use to avoid
/// needing virtual dispatches
class future_base {
public:
  /// Check if the future is now ready with a value (precondition: get_promise()
  /// must have been called)
  bool ready() const JL_NOTSAFEPOINT {
    if (!valid())
      report_fatal_error("ready() called before get_promise()");
    return state_->status_.load(std::memory_order_acquire) == FutureStatus::Ready;
  }

  /// Check if the future is in a valid state (not moved-from and get_promise() called)
  bool valid() const JL_NOTSAFEPOINT { return state_ != nullptr; }

  /// Wait for the future to be ready, helping with task dispatch
  void wait(JuliaTaskDispatcher &D) {
    // Keep helping with task dispatch until our future is ready
    if (!ready()) {
      D.work_until(*this);
      if (state_->status_.load(std::memory_order_relaxed) != FutureStatus::Ready)
        report_fatal_error(
            "work_until() returned without this future being ready");
    }
  }

protected:
  struct state_base {
    std::atomic<FutureStatus> status_{FutureStatus::NotReady};
  };

  future_base(state_base *state) : state_(state) {}
  future_base() = default;

  /// Only allow deleting the future once it is invalid
  ~future_base() {
    if (state_)
      report_fatal_error("get() must be called before future destruction (ensuring promise::set_value memory is valid)");
  }

  // Move constructor and assignment
  future_base(future_base &&other) noexcept : state_(other.state_) {
    other.state_ = nullptr;
  }

  future_base &operator=(future_base &&other) noexcept {
    if (this != &other) {
      this->~future_base();
      state_ = other.state_;
      other.state_ = nullptr;
    }
    return *this;
  }

  state_base *state_;
};

/// TaskDispatcher-aware future class for cooperative await.
///
/// @tparam T The type of value this future will provide. Use void for futures
/// that
///           signal completion without providing a value.
///
/// This future implementation is similar to `std::future`, so most code can
/// transition to it easily. However, it differs from `std::future` in a few
/// key ways to be aware of:
/// - No exception support (or the overhead for it).
/// - The future is created before the promise, then the promise is created
///   from the future.
/// - The future is in an invalid state until get_promise() has been called.
/// - Waiting operations (get(&D), wait(&D)) help dispatch other tasks while
///   blocked, requiring an additional argument of which TaskDispatcher object
///   of where all associated work will be scheduled.
/// - While `wait` may be called multiple times and on multiple threads, all of
///   them must have returned before calling `get` on exactly one thread.
/// - Must call get() exactly once before destruction (enforced with
///   `report_fatal_error`) after each call to `get_promise`. Internal state is
///   freed when `get` returns, and allocated when `get_promise` is called.
///
/// Other notable features, in common with `std::future`:
/// - Supports both value types and void specialization through the same
/// interface.
/// - Thread-safe through atomic operations.
/// - Provides acquire-release ordering with `std::promise::set_value()`.
/// - Concurrent access to any method (including to `ready`) on multiple threads
///   is not allowed.
/// - Holding any locks while calling `get()` is likely to lead to deadlock.
///
/// @warning Users should avoid borrowing references to futures. References may
/// go out of scope and break the uniqueness contract, which may break the
/// soundness of the types. Always use move semantics or pass by value.

template <typename T> class future : public future_base {
public:
  future() : future_base(nullptr) {}
  future(const future &) = delete;
  future &operator=(const future &) = delete;
  future(future &&) = default;
  future &operator=(future &&) = default;

  /// Get the value, helping with task dispatch while waiting.
  /// This will destroy the underlying value, so this must be called exactly
  /// once, which returns the future to the initial state.
  T get(JuliaTaskDispatcher &D) {
    if (!valid())
      report_fatal_error("get() must only be called once, after get_promise()");
    wait(D);
    auto state_ = static_cast<state*>(this->state_);
    this->state_ = nullptr;
    return take_value(state_);
  }

  /// Get the associated promise (must only be called once)
  promise<T> get_promise() {
    if (valid())
      report_fatal_error("get_promise() can only be called once");
    auto state_ = new state();
    this->state_ = state_;
    return promise<T>(state_);
  }

private:
  friend class promise<T>;

  // Template the state struct with EBCO so that future<void> has no wasted
  // overhead for the value. The declaration of future_value_storage is far
  // above here since GCC doesn't implement it properly when nested.
  struct state : future_base::state_base, future_value_storage<T> {};

  template <typename U = T>
  typename std::enable_if<!std::is_void<U>::value, U>::type take_value(state *state_) {
    T result = std::move(state_->value_);
    state_->value_.~T();
    delete state_;
    return result;
  }

  template <typename U = T>
  typename std::enable_if<std::is_void<U>::value, U>::type take_value(state *state_) {
    delete state_;
  }
};

/// TaskDispatcher-aware promise class that provides values to associated
/// futures.
///
/// @tparam T The type of value this promise will provide. Use void for promises
/// that
///           signal completion without providing a value.
///
/// This promise implementation provides the value-setting side of the
/// promise/future pair and integrates with the ORC TaskDispatcher system. Key
/// characteristics:
/// - Created from a future via get_promise() rather than creating the future from the promise.
/// - Must call get_future() on the thread that created it (it can be passed to another thread, but do not borrow a reference and use that to mutate it from another thread).
/// - Must call set_value() exactly once per `get_promise()` call to provide the result.
/// - Thread-safe from set_value to get.
/// - Move-only semantics to prevent accidental copying.
///
/// The `promise` can usually be passed to another thread in one of two ways:
/// - With move semantics:
///     * `[P = F.get_promise()] () { P.set_value(); }`
///     * `[P = std::move(P)] () { P.set_value(); }`
///     * Advantages: clearer where `P` is owned, automatic deadlock detection
///     on destruction,
///       easier memory management if the future is returned from the function.
/// - By reference:
///     * `[&P] () { P.set_value(); }`
///     * Advantages: simpler memory management if the future is consumed in the
///     same function.
///     * Disadvantages: more difficult memory management if the future is
///     returned from the function, no deadlock detection.
///
/// @warning Users should avoid borrowing references to promises. References may
/// go out of scope and break the uniqueness contract, which may break the
/// soundness of the types. Always use move semantics or pass by value.
///
/// @par Error Handling:
/// The promise/future system uses report_fatal_error() for misuse:
/// - Calling set_value() more than once.
/// - Destroying a future without calling get().
/// - Calling get() more than once on a future.
///
/// @par Thread Safety:
/// - Each promise/future must only be accessed by one thread, as concurrent
///   calls to the API functions may result in crashes.
/// - Multiple threads can safely access different promise/future pairs.
/// - set_value() and get() operations are atomic and thread-safe.
/// - Move operations should only be performed by a single thread.
template <typename T> class promise {
  friend class future<T>;

public:
  promise() : state_(nullptr) {}

  ~promise() {
    // Assert proper promise lifecycle: ensure set_value was called if promise was valid.
    // This can catch deadlocks where a promise is created but set_value() is
    // never called, though only if the promise is moved from instead of
    // borrowed from the frame with the future.
    // Empty promises (state_ == nullptr) are allowed to be destroyed without calling set_value.
  }

  promise(const promise &) = delete;
  promise &operator=(const promise &) = delete;

  promise(promise &&other) noexcept
      : state_(other.state_) {
    other.state_ = nullptr;
  }

  promise &operator=(promise &&other) noexcept {
    if (this != &other) {
      this->~promise();
      state_ = other.state_;
      other.state_ = nullptr;
    }
    return *this;
  }


  /// Set the value (must only be called once)
  // In C++20, this std::conditional weirdness can probably be replaced just
  // with requires. It ensures that we don't try to define a method for `void&`,
  // but that if the user calls set_value(v) for any value v that they get a
  // member function error, instead of no member named 'value_'.
  template <typename U = T>
  void
  set_value(const typename std::conditional<std::is_void<T>::value,
                                            std::nullopt_t, T>::type &value) const {
    assert(state_ && "set_value() can only be called once");
    new (&state_->value_) T(value);
    state_->status_.store(FutureStatus::Ready, std::memory_order_release);
    state_ = nullptr;
  }

  template <typename U = T>
  void set_value(typename std::conditional<std::is_void<T>::value,
                                           std::nullopt_t, T>::type &&value) const {
    assert(state_ && "set_value() can only be called once");
    new (&state_->value_) T(std::move(value));
    state_->status_.store(FutureStatus::Ready, std::memory_order_release);
    state_ = nullptr;
  }

  template <typename U = T>
  typename std::enable_if<std::is_void<U>::value, void>::type
  set_value(const std::nullopt_t &value) = delete;

  template <typename U = T>
  typename std::enable_if<std::is_void<U>::value, void>::type
  set_value(std::nullopt_t &&value) = delete;

  template <typename U = T>
  typename std::enable_if<std::is_void<U>::value, void>::type set_value() const {
    assert(state_ && "set_value() can only be called once");
    state_->status_.store(FutureStatus::Ready, std::memory_order_release);
    state_ = nullptr;
  }

  /// Swap with another promise
  void swap(promise &other) noexcept {
    using std::swap;
    swap(state_, other.state_);
  }

private:
  explicit promise(typename future<T>::state *state)
      : state_(state) {}

  mutable typename future<T>::state *state_;
};

}; // class JuliaTaskDispatcher

void JuliaTaskDispatcher::dispatch(std::unique_ptr<Task> T) {
  std::unique_lock Lock{DispatchMutex};
  TaskQueue.push_back(std::move(T));
}

void JuliaTaskDispatcher::shutdown() {
  abort();
}

void JuliaTaskDispatcher::work_until(future_base &F) {
  jl_unique_gcsafe_lock Lock{DispatchMutex};
  while (!F.ready()) {
    process_tasks(Lock);

    // Check if our future is now ready
    if (F.ready())
      return;

    // If we get here, our queue is empty but the future isn't ready
    // We need to wait for other threads to finish work that should complete our
    // future
    Lock.wait(WorkFinishedCV);
  }
}

void JuliaTaskDispatcher::process_tasks(jl_unique_gcsafe_lock &Lock) {
    while (!TaskQueue.empty()) {
        auto T = TaskQueue.pop_back_val();

        Lock.native.unlock();
        T->run();
        Lock.native.lock();

        WorkFinishedCV.notify_all();
    }
}

} // End namespace

namespace std {
template <typename T>
void swap(::JuliaTaskDispatcher::promise<T> &lhs, ::JuliaTaskDispatcher::promise<T> &rhs) noexcept {
  lhs.swap(rhs);
}
} // End namespace std

// n.b. this actually is sometimes a safepoint
Expected<SymbolMap>
safelookup(ExecutionSession &ES,
           const JITDylibSearchOrder &SearchOrder,
           SymbolLookupSet Symbols, LookupKind K = LookupKind::Static,
           SymbolState RequiredState = SymbolState::Ready,
           RegisterDependenciesFunction RegisterDependencies = NoDependenciesToRegister) {
  JuliaTaskDispatcher::future<MSVCPExpected<SymbolMap>> PromisedFuture;
  auto NotifyComplete = [PromisedResult = PromisedFuture.get_promise()](Expected<SymbolMap> R) {
    PromisedResult.set_value(std::move(R));
  };
  ES.lookup(K, SearchOrder, std::move(Symbols), RequiredState,
        std::move(NotifyComplete), RegisterDependencies);
  return PromisedFuture.get(static_cast<JuliaTaskDispatcher&>(ES.getExecutorProcessControl().getDispatcher()));
}

Expected<ExecutorSymbolDef>
safelookup(ExecutionSession &ES,
           const JITDylibSearchOrder &SearchOrder,
           SymbolStringPtr Name,
           SymbolState RequiredState = SymbolState::Ready) {
  SymbolLookupSet Names({Name});

  if (auto ResultMap = safelookup(ES, SearchOrder, std::move(Names), LookupKind::Static,
                                  RequiredState, NoDependenciesToRegister)) {
    assert(ResultMap->size() == 1 && "Unexpected number of results");
    assert(ResultMap->count(Name) && "Missing result for symbol");
    return std::move(ResultMap->begin()->second);
  } else
    return ResultMap.takeError();
}

Expected<ExecutorSymbolDef>
safelookup(ExecutionSession &ES,
           ArrayRef<JITDylib *> SearchOrder, SymbolStringPtr Name,
           SymbolState RequiredState = SymbolState::Ready) {
  return safelookup(ES, makeJITDylibSearchOrder(SearchOrder), Name, RequiredState);
}

Expected<ExecutorSymbolDef>
safelookup(ExecutionSession &ES,
           ArrayRef<JITDylib *> SearchOrder, StringRef Name,
           SymbolState RequiredState = SymbolState::Ready) {
  return safelookup(ES, SearchOrder, ES.intern(Name), RequiredState);
}
