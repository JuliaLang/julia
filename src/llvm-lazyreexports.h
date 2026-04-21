//===------------------------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// This file contains vendored versions of RedirectableSymbolManager and
// LazyReexportsManager with some modifications:
// - Symbol lookups are done using the asynchronous API.
// - We assume that we JIT in-process.
// - Symbol resolution is done via an arbitrary callback.  The callback can
//   decide throw a Julia exception instead of returning a resolved symbol.

#ifndef JL_LLVM_REDIRECTABLESYMBOLS_H
#define JL_LLVM_REDIRECTABLESYMBOLS_H

#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>

namespace julia {

using namespace llvm;

using orc::ResourceManager, orc::MaterializationUnit, orc::JITDylibSP,
    orc::SymbolAliasMapEntry, orc::SymbolStringPtr, orc::JITDylib, orc::ResourceKey,
    orc::ExecutorSymbolDef, orc::ResourceTrackerSP, orc::MaterializationResponsibility,
    orc::ExecutorAddr, orc::ExecutionSession, orc::SymbolMap, orc::SymbolFlagsMap,
    orc::ObjectLinkingLayer;

class RedirectionManager {
public:
  virtual ~RedirectionManager() = default;

  /// Change the redirection destination of given symbols to new destination
  /// symbols.
  virtual void redirect(JITDylib &JD, SymbolMap NewDests,
                        unique_function<void(Error)> OnComplete) = 0;

  /// Blocking version of redirect.
  Error redirect(JITDylib &JD, SymbolMap NewDests);

  /// Change the redirection destination of given symbol to new destination
  /// symbol.
  Error redirect(JITDylib &JD, SymbolStringPtr Symbol,
                 ExecutorSymbolDef NewDest) {
    return redirect(JD, {{std::move(Symbol), NewDest}});
  }

private:
  virtual void anchor();
};

/// Base class for managing redirectable symbols in which a call
/// gets redirected to another symbol in runtime.
class RedirectableSymbolManager : public RedirectionManager {
public:
  /// Create redirectable symbols with given symbol names and initial
  /// desitnation symbol addresses.
  Error createRedirectableSymbols(ResourceTrackerSP RT,
                                  SymbolMap InitialDests);

  /// Create a single redirectable symbol with given symbol name and initial
  /// desitnation symbol address.
  Error createRedirectableSymbol(ResourceTrackerSP RT, SymbolStringPtr Symbol,
                                 ExecutorSymbolDef InitialDest) {
    return createRedirectableSymbols(RT, {{std::move(Symbol), InitialDest}});
  }

  /// Emit redirectable symbol
  virtual void
  emitRedirectableSymbols(std::unique_ptr<MaterializationResponsibility> MR,
                          SymbolMap InitialDests) = 0;
};

/// RedirectableMaterializationUnit materializes redirectable symbol
/// by invoking RedirectableSymbolManager::emitRedirectableSymbols
class RedirectableMaterializationUnit : public MaterializationUnit {
public:
  RedirectableMaterializationUnit(RedirectableSymbolManager &RM,
                                  SymbolMap InitialDests)
      : MaterializationUnit(convertToFlags(InitialDests)), RM(RM),
        InitialDests(std::move(InitialDests)) {}

  StringRef getName() const override {
    return "RedirectableSymbolMaterializationUnit";
  }

  void materialize(std::unique_ptr<MaterializationResponsibility> R) override {
    RM.emitRedirectableSymbols(std::move(R), std::move(InitialDests));
  }

  void discard(const JITDylib &JD, const SymbolStringPtr &Name) override {
    InitialDests.erase(Name);
  }

private:
  static MaterializationUnit::Interface
  convertToFlags(const SymbolMap &InitialDests) {
    SymbolFlagsMap Flags;
    for (auto [K, V] : InitialDests)
      Flags[K] = V.getFlags();
    return MaterializationUnit::Interface(Flags, {});
  }

  RedirectableSymbolManager &RM;
  SymbolMap InitialDests;
};

class JITLinkRedirectableSymbolManager
    : public RedirectableSymbolManager {
public:
  /// Create redirection manager that uses JITLink based implementaion.
  static Expected<std::unique_ptr<RedirectableSymbolManager>>
  Create(ObjectLinkingLayer &ObjLinkingLayer) {
    auto AnonymousPtrCreator(jitlink::getAnonymousPointerCreator(
        ObjLinkingLayer.getExecutionSession().getTargetTriple()));
    auto PtrJumpStubCreator(jitlink::getPointerJumpStubCreator(
        ObjLinkingLayer.getExecutionSession().getTargetTriple()));
    if (!AnonymousPtrCreator || !PtrJumpStubCreator)
      return make_error<StringError>("Architecture not supported",
                                     inconvertibleErrorCode());
    return std::unique_ptr<RedirectableSymbolManager>(
        new JITLinkRedirectableSymbolManager(
            ObjLinkingLayer, AnonymousPtrCreator, PtrJumpStubCreator));
  }

  JITLinkRedirectableSymbolManager(
      ObjectLinkingLayer &ObjLinkingLayer,
      jitlink::AnonymousPointerCreator &AnonymousPtrCreator,
      jitlink::PointerJumpStubCreator &PtrJumpStubCreator)
      : ObjLinkingLayer(ObjLinkingLayer),
        AnonymousPtrCreator(std::move(AnonymousPtrCreator)),
        PtrJumpStubCreator(std::move(PtrJumpStubCreator)) {}

  ObjectLinkingLayer &getObjectLinkingLayer() const { return ObjLinkingLayer; }

  void emitRedirectableSymbols(std::unique_ptr<MaterializationResponsibility> R,
                               SymbolMap InitialDests) override;

  void redirect(JITDylib &JD, SymbolMap NewDests,
                unique_function<void(Error)> OnComplete) override;

private:
  ObjectLinkingLayer &ObjLinkingLayer;
  jitlink::AnonymousPointerCreator AnonymousPtrCreator;
  jitlink::PointerJumpStubCreator PtrJumpStubCreator;
  std::atomic_size_t StubGraphIdx{0};
};

using SymbolResolverMap =
    DenseMap<SymbolStringPtr, std::pair<JITSymbolFlags, std::function<SymbolStringPtr()>>>;

class LazyReexportsManager : public ResourceManager {

  friend std::unique_ptr<MaterializationUnit>
  lazyReexports(LazyReexportsManager &, SymbolResolverMap);

public:
  struct CallThroughInfo {
    JITDylibSP JD;
    SymbolStringPtr Name;
    std::function<SymbolStringPtr()> Resolver;
  };

  using OnTrampolinesReadyFn = unique_function<void(
                                                    Expected<std::vector<ExecutorSymbolDef>> EntryAddrs)>;
  using EmitTrampolinesFn =
      unique_function<void(ResourceTrackerSP RT, size_t NumTrampolines,
                           OnTrampolinesReadyFn OnTrampolinesReady)>;

  /// Create a LazyReexportsManager that uses the ORC runtime for reentry.
  /// This will work both in-process and out-of-process.
  static Expected<std::unique_ptr<LazyReexportsManager>>
  Create(EmitTrampolinesFn EmitTrampolines, RedirectableSymbolManager &RSMgr,
         JITDylib &PlatformJD);

  LazyReexportsManager(LazyReexportsManager &&) = delete;
  LazyReexportsManager &operator=(LazyReexportsManager &&) = delete;

  Error handleRemoveResources(JITDylib &JD, ResourceKey K) override;
  void handleTransferResources(JITDylib &JD, ResourceKey DstK,
                               ResourceKey SrcK) override;

private:
  class MU;
  class Plugin;

  using ResolveSendResultFn =
      unique_function<void(Expected<ExecutorSymbolDef>)>;

  LazyReexportsManager(EmitTrampolinesFn EmitTrampolines, RedirectableSymbolManager &RSMgr,
                       JITDylib &PlatformJD, Error &Err);

  std::unique_ptr<MaterializationUnit>
  createLazyReexports(SymbolResolverMap Reexports);

  void emitReentryTrampolines(std::unique_ptr<MaterializationResponsibility> MR,
                              SymbolResolverMap Reexports);
  void emitRedirectableSymbols(
                               std::unique_ptr<MaterializationResponsibility> MR,
                               SymbolResolverMap Reexports,
                               Expected<std::vector<ExecutorSymbolDef>> ReentryPoints);
  void resolve(ResolveSendResultFn SendResult, ExecutorAddr ReentryStubAddr);

  ExecutionSession &ES;
  EmitTrampolinesFn EmitTrampolines;
  RedirectableSymbolManager &RSMgr;

  DenseMap<ResourceKey, std::vector<ExecutorAddr>> KeyToReentryAddrs;
  DenseMap<ExecutorAddr, CallThroughInfo> CallThroughs;
};

Expected<std::unique_ptr<LazyReexportsManager>>
createJITLinkLazyReexportsManager(ObjectLinkingLayer &ObjLinkingLayer,
                                  RedirectableSymbolManager &RSMgr,
                                  JITDylib &PlatformJD);

/// Define lazy-reexports based on the given SymbolResolverMap. Each lazy re-export
/// is a callable symbol that will look up and dispatch to the given aliasee on
/// first call. All subsequent calls will go directly to the aliasee.
inline std::unique_ptr<MaterializationUnit>
lazyReexports(LazyReexportsManager &LRM, SymbolResolverMap Reexports) {
  return LRM.createLazyReexports(std::move(Reexports));
}

} // End namespace julia

#endif // JL_LLVM_REDIRECTABLESYMBOLS_H
