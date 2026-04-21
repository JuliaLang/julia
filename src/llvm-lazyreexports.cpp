//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/JITLinkReentryTrampolines.h>

#include "llvm-lazyreexports.h"

#define DEBUG_TYPE "orc"

using namespace julia;
using orc::SymbolInstance, orc::MemProt, orc::SymbolLookupSet,
    orc::NonOwningSymbolStringPtr, orc::LookupKind, orc::JITDylibLookupFlags,
    orc::SymbolState, orc::NoDependenciesToRegister, orc::shared::WrapperFunction,
    orc::shared::SPSExecutorSymbolDef, orc::shared::SPSExpected,
    orc::shared::SPSExecutorAddr;
namespace tpctypes = orc::tpctypes;

Error RedirectionManager::redirect(JITDylib &JD, SymbolMap NewDests) {
  std::promise<MSVCPError> P;
  redirect(JD, std::move(NewDests),
           [&P](Error E) { P.set_value(std::move(E)); });
  auto F = P.get_future();
  return F.get();
}

void RedirectionManager::anchor() {}

Error RedirectableSymbolManager::createRedirectableSymbols(
    ResourceTrackerSP RT, SymbolMap InitialDests) {
  auto &JD = RT->getJITDylib();
  return JD.define(std::make_unique<RedirectableMaterializationUnit>(
                       *this, std::move(InitialDests)),
                   RT);
}

namespace {
constexpr StringRef JumpStubSectionName = "__orc_stubs";
constexpr StringRef StubPtrSectionName = "__orc_stub_ptrs";
constexpr StringRef StubSuffix = "$__stub_ptr";
} // namespace

void JITLinkRedirectableSymbolManager::emitRedirectableSymbols(
    std::unique_ptr<MaterializationResponsibility> R, SymbolMap InitialDests) {

  auto &ES = ObjLinkingLayer.getExecutionSession();
  auto G = std::make_unique<jitlink::LinkGraph>(
      ("<indirect stubs graph #" + Twine(++StubGraphIdx) + ">").str(),
      ES.getSymbolStringPool(), ES.getTargetTriple(), SubtargetFeatures(),
      jitlink::getGenericEdgeKindName);
  auto &PointerSection =
      G->createSection(StubPtrSectionName, MemProt::Write | MemProt::Read);
  auto &StubsSection =
      G->createSection(JumpStubSectionName, MemProt::Exec | MemProt::Read);

  SymbolFlagsMap NewSymbols;
  for (auto &[Name, Def] : InitialDests) {
    jitlink::Symbol *TargetSym = nullptr;
    if (Def.getAddress())
      TargetSym = &G->addAbsoluteSymbol(
          G->allocateName(*Name + "$__init_tgt"), Def.getAddress(), 0,
          jitlink::Linkage::Strong, jitlink::Scope::Local, false);

    auto PtrName = ES.intern((*Name + StubSuffix).str());
    auto &Ptr = AnonymousPtrCreator(*G, PointerSection, TargetSym, 0);
    Ptr.setName(PtrName);
    Ptr.setScope(jitlink::Scope::Hidden);
    auto &Stub = PtrJumpStubCreator(*G, StubsSection, Ptr);
    Stub.setName(Name);
    Stub.setScope(Def.getFlags().isExported() ? jitlink::Scope::Default
                                              : jitlink::Scope::Hidden);
    Stub.setLinkage(!Def.getFlags().isWeak() ? jitlink::Linkage::Strong
                                             : jitlink::Linkage::Weak);
    NewSymbols[std::move(PtrName)] = JITSymbolFlags();
  }

  // Try to claim responsibility for the new stub symbols.
  if (auto Err = R->defineMaterializing(std::move(NewSymbols))) {
    ES.reportError(std::move(Err));
    return R->failMaterialization();
  }

  ObjLinkingLayer.emit(std::move(R), std::move(G));
}

void JITLinkRedirectableSymbolManager::redirect(
    JITDylib &JD, SymbolMap NewDests, unique_function<void(Error)> OnComplete) {
  auto &ES = ObjLinkingLayer.getExecutionSession();
  SymbolLookupSet LS;
  DenseMap<NonOwningSymbolStringPtr, SymbolStringPtr> PtrToStub;
  for (auto &[StubName, Sym] : NewDests) {
    auto PtrName = ES.intern((*StubName + StubSuffix).str());
    PtrToStub[NonOwningSymbolStringPtr(PtrName)] = StubName;
    LS.add(std::move(PtrName));
  }

  auto LookupComplete = [this, NewDests = std::move(NewDests),
                         PtrToStub = std::move(PtrToStub),
                         OnComplete = std::move(OnComplete)](
                            Expected<SymbolMap> PtrSyms) mutable {
    if (!PtrSyms)
      return OnComplete(PtrSyms.takeError());

    std::vector<tpctypes::PointerWrite> PtrWrites;
    for (auto &[PtrName, PtrSym] : *PtrSyms) {
      auto DestSymI =
          NewDests.find(PtrToStub.at(NonOwningSymbolStringPtr(PtrName)));
      assert(DestSymI != NewDests.end() && "Bad ptr -> stub mapping");
      auto &DestSym = DestSymI->second;
      PtrWrites.push_back({PtrSym.getAddress(), DestSym.getAddress()});
    }

    OnComplete(ObjLinkingLayer.getExecutionSession()
                   .getExecutorProcessControl()
                   .getMemoryAccess()
                   .writePointers(PtrWrites));
  };

  ES.lookup(LookupKind::Static, {{&JD, JITDylibLookupFlags::MatchAllSymbols}},
            std::move(LS), SymbolState::Ready, std::move(LookupComplete),
            NoDependenciesToRegister);
}

class LazyReexportsManager::MU : public MaterializationUnit {
public:
  MU(LazyReexportsManager &LRMgr, SymbolResolverMap Reexports)
      : MaterializationUnit(getInterface(Reexports)), LRMgr(LRMgr),
        Reexports(std::move(Reexports)) {}

private:
  Interface getInterface(const SymbolResolverMap &Reexports) {
    SymbolFlagsMap SF;
    for (auto &[Alias, AI] : Reexports)
      SF[Alias] = AI.first;
    return {std::move(SF), nullptr};
  }

  StringRef getName() const override { return "LazyReexportsManager::MU"; }

  void materialize(std::unique_ptr<MaterializationResponsibility> R) override {
    LRMgr.emitReentryTrampolines(std::move(R), std::move(Reexports));
  }

  void discard(const JITDylib &JD, const SymbolStringPtr &Name) override {
    Reexports.erase(Name);
  }

  LazyReexportsManager &LRMgr;
  SymbolResolverMap Reexports;
};

class LazyReexportsManager::Plugin : public ObjectLinkingLayer::Plugin {
public:
  void modifyPassConfig(MaterializationResponsibility &MR,
                        jitlink::LinkGraph &G,
                        jitlink::PassConfiguration &Config) override {}

  Error notifyFailed(MaterializationResponsibility &MR) override {
    return Error::success();
  }

  Error notifyRemovingResources(JITDylib &JD, ResourceKey K) override {
    return Error::success();
  }

  void notifyTransferringResources(JITDylib &JD, ResourceKey DstKey,
                                   ResourceKey SrcKey) override {}

private:
  std::mutex M;
};

Expected<std::unique_ptr<LazyReexportsManager>>
LazyReexportsManager::Create(EmitTrampolinesFn EmitTrampolines,
                             RedirectableSymbolManager &RSMgr,
                             JITDylib &PlatformJD) {
  Error Err = Error::success();
  std::unique_ptr<LazyReexportsManager> LRM(new LazyReexportsManager(
      std::move(EmitTrampolines), RSMgr, PlatformJD, Err));
  if (Err)
    return std::move(Err);
  return std::move(LRM);
}

Error LazyReexportsManager::handleRemoveResources(JITDylib &JD, ResourceKey K) {
  return JD.getExecutionSession().runSessionLocked([&]() -> Error {
    auto I = KeyToReentryAddrs.find(K);
    if (I == KeyToReentryAddrs.end())
      return Error::success();

    auto &ReentryAddrs = I->second;
    for (auto &ReentryAddr : ReentryAddrs) {
      assert(CallThroughs.count(ReentryAddr) && "CallTrhough missing");
      CallThroughs.erase(ReentryAddr);
    }
    KeyToReentryAddrs.erase(I);
    return Error::success();
  });
}

void LazyReexportsManager::handleTransferResources(JITDylib &JD,
                                                   ResourceKey DstK,
                                                   ResourceKey SrcK) {
  auto I = KeyToReentryAddrs.find(SrcK);
  if (I != KeyToReentryAddrs.end()) {
    auto J = KeyToReentryAddrs.find(DstK);
    if (J == KeyToReentryAddrs.end()) {
      auto Tmp = std::move(I->second);
      KeyToReentryAddrs.erase(I);
      KeyToReentryAddrs[DstK] = std::move(Tmp);
    } else {
      auto &SrcAddrs = I->second;
      auto &DstAddrs = J->second;
      llvm::append_range(DstAddrs, SrcAddrs);
      KeyToReentryAddrs.erase(I);
    }
  }
}

LazyReexportsManager::LazyReexportsManager(EmitTrampolinesFn EmitTrampolines,
                                           RedirectableSymbolManager &RSMgr,
                                           JITDylib &PlatformJD,
                                           Error &Err)
  : ES(PlatformJD.getExecutionSession()),
    EmitTrampolines(std::move(EmitTrampolines)), RSMgr(RSMgr)
{
    using namespace orc::shared;

    ErrorAsOutParameter _(&Err);

    ExecutionSession::JITDispatchHandlerAssociationMap WFs;
    WFs[ES.intern("__orc_rt_jl_resolve_tag")] =
        ES.wrapAsyncWithSPS<SPSExpected<SPSExecutorSymbolDef>(SPSExecutorAddr)>(
            this, &LazyReexportsManager::resolve);

    Err = ES.registerJITDispatchHandlers(PlatformJD, std::move(WFs));
}

std::unique_ptr<MaterializationUnit>
LazyReexportsManager::createLazyReexports(SymbolResolverMap Reexports) {
  return std::make_unique<MU>(*this, std::move(Reexports));
}

void LazyReexportsManager::emitReentryTrampolines(
    std::unique_ptr<MaterializationResponsibility> MR,
    SymbolResolverMap Reexports) {
  size_t NumTrampolines = Reexports.size();
  auto RT = MR->getResourceTracker();
  EmitTrampolines(
      std::move(RT), NumTrampolines,
      [this, MR = std::move(MR), Reexports = std::move(Reexports)](
          Expected<std::vector<ExecutorSymbolDef>> ReentryPoints) mutable {
        emitRedirectableSymbols(std::move(MR), std::move(Reexports),
                                std::move(ReentryPoints));
      });
}

void LazyReexportsManager::emitRedirectableSymbols(
    std::unique_ptr<MaterializationResponsibility> MR, SymbolResolverMap Reexports,
    Expected<std::vector<ExecutorSymbolDef>> ReentryPoints) {

  if (!ReentryPoints) {
    MR->getExecutionSession().reportError(ReentryPoints.takeError());
    MR->failMaterialization();
    return;
  }

  assert(Reexports.size() == ReentryPoints->size() &&
         "Number of reentry points doesn't match number of reexports");

  // Bind entry points to names.
  SymbolMap Redirs;
  size_t I = 0;
  for (auto &[Name, AI] : Reexports)
    Redirs[Name] = {(*ReentryPoints)[I++].getAddress(), AI.first};

  I = 0;
  if (!Reexports.empty()) {
    if (auto Err = MR->withResourceKeyDo([&](ResourceKey K) {
          auto &JD = MR->getTargetJITDylib();
          auto &ReentryAddrsForK = KeyToReentryAddrs[K];
          for (auto &[Name, AI] : Reexports) {
            const auto &ReentryPoint = (*ReentryPoints)[I++];
            CallThroughs[ReentryPoint.getAddress()] = {&JD, Name, std::move(AI.second)};
            ReentryAddrsForK.push_back(ReentryPoint.getAddress());
          }
        })) {
      MR->getExecutionSession().reportError(std::move(Err));
      MR->failMaterialization();
      return;
    }
  }

  RSMgr.emitRedirectableSymbols(std::move(MR), std::move(Redirs));
}

void LazyReexportsManager::resolve(ResolveSendResultFn SendResult,
                                   ExecutorAddr ReentryStubAddr) {

  CallThroughInfo LandingInfo;

  ES.runSessionLocked([&]() {
    auto I = CallThroughs.find(ReentryStubAddr);
    if (I == CallThroughs.end())
      return SendResult(make_error<StringError>(
          "Reentry address " + formatv("{0:x}", ReentryStubAddr) +
              " not registered",
          inconvertibleErrorCode()));
    LandingInfo = I->second;
  });

  // This can throw a Julia exception, so we can't be holding any locks.
  SymbolStringPtr Dest = LandingInfo.Resolver();
  SymbolInstance LandingSym(LandingInfo.JD, std::move(Dest));
  LandingSym.lookupAsync([this, JD = std::move(LandingInfo.JD),
                          ReentryName = std::move(LandingInfo.Name),
                          SendResult = std::move(SendResult)](
                             Expected<ExecutorSymbolDef> Result) mutable {
    if (Result) {
      RSMgr.redirect(*JD, {{ReentryName, *Result}},
                     [Result = std::move(Result),
                      SendResult = std::move(SendResult)](Error Err) mutable {
                       if (Err)
                         SendResult(std::move(Err));
                       else
                         SendResult(std::move(Result));
                     });
    } else
      SendResult(std::move(Result));
  });
}

Expected<std::unique_ptr<LazyReexportsManager>>
julia::createJITLinkLazyReexportsManager(ObjectLinkingLayer &ObjLinkingLayer,
                                         RedirectableSymbolManager &RSMgr,
                                         JITDylib &PlatformJD)
{
    auto JLT = orc::JITLinkReentryTrampolines::Create(ObjLinkingLayer);
    if (!JLT)
        return JLT.takeError();

    return LazyReexportsManager::Create(
        [JLT = std::move(*JLT)](
            ResourceTrackerSP RT, size_t NumTrampolines,
            LazyReexportsManager::OnTrampolinesReadyFn OnTrampolinesReady) mutable {
            JLT->emit(std::move(RT), NumTrampolines, std::move(OnTrampolinesReady));
        },
        RSMgr, PlatformJD);
}

orc::shared::CWrapperFunctionResult jl_orc_jitdispatch(const void *FnTag, const char *Data,
                                                       size_t Size);

static void __orc_rt_resolve_fail(void *Caller, const char *ErrMsg) {
  fprintf(stderr, "error resolving implementation for stub %p: %s\n", Caller,
          ErrMsg);
  abort();
}

char __orc_rt_jl_resolve_tag;
extern "C" void *__orc_rt_jl_resolve(void *Caller)
{
    Expected<ExecutorSymbolDef> Result((ExecutorSymbolDef()));
    if (auto Err =
            WrapperFunction<SPSExpected<SPSExecutorSymbolDef>(SPSExecutorAddr)>::call(
                [](const char *Data, size_t Size) {
                    return jl_orc_jitdispatch(&__orc_rt_jl_resolve_tag, Data, Size);
                },
                Result, ExecutorAddr::fromPtr(Caller))) {
        __orc_rt_resolve_fail(Caller, toString(std::move(Err)).c_str());
    }

    if (!Result) {
        __orc_rt_resolve_fail(Caller, toString(Result.takeError()).c_str());
    }

    return Result->getAddress().toPtr<void *>();
}
