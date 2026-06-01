// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "GCChecker.h"

#include "llvm/ADT/SmallPtrSet.h"

// GCChecker program-state definitions and graph/state manipulation helpers.

namespace clang {
namespace ento {

#define JL_GC_DEFINE_PROGRAMSTATE_TRAIT(Name)                                  \
  void *ProgramStateTrait<jl_gc_checker::Name>::GDMIndex() {                   \
    static int Index;                                                          \
    return &Index;                                                             \
  }

JL_GC_DEFINE_PROGRAMSTATE_TRAIT(GCDepth)
JL_GC_DEFINE_PROGRAMSTATE_TRAIT(GCDisabledAt)
JL_GC_DEFINE_PROGRAMSTATE_TRAIT(SafepointDisabledAt)
JL_GC_DEFINE_PROGRAMSTATE_TRAIT(MayCallSafepoint)
JL_GC_DEFINE_PROGRAMSTATE_TRAIT(GCRootMap)
JL_GC_DEFINE_PROGRAMSTATE_TRAIT(GCObjectStateMap)
JL_GC_DEFINE_PROGRAMSTATE_TRAIT(GCRegionObjectMap)
JL_GC_DEFINE_PROGRAMSTATE_TRAIT(GCObjectRegionMap)
JL_GC_DEFINE_PROGRAMSTATE_TRAIT(GCDerivedRegionOwnerMap)
JL_GC_DEFINE_PROGRAMSTATE_TRAIT(GCObjectOwnershipMap)
JL_GC_DEFINE_PROGRAMSTATE_TRAIT(GCRootFrameMap)
JL_GC_DEFINE_PROGRAMSTATE_TRAIT(GCPermanentRootRegions)
JL_GC_DEFINE_PROGRAMSTATE_TRAIT(GCConservativeRootRegions)

#undef JL_GC_DEFINE_PROGRAMSTATE_TRAIT

#define JL_GC_DEFINE_FACTORY_TRAIT(Type)                                       \
  void *ProgramStateTrait<Type>::GDMIndex() {                                  \
    static int Index;                                                          \
    return &Index;                                                             \
  }

JL_GC_DEFINE_FACTORY_TRAIT(jl_gc_checker::GCChecker::GCObjectSet)
JL_GC_DEFINE_FACTORY_TRAIT(jl_gc_checker::GCChecker::GCRegionSet)
JL_GC_DEFINE_FACTORY_TRAIT(jl_gc_checker::GCChecker::GCRootList)

#undef JL_GC_DEFINE_FACTORY_TRAIT

} // namespace ento
} // namespace clang

namespace jl_gc_checker {

namespace Helpers {
const VarRegion *walk_back_to_global_VR(const MemRegion *Region) {
  if (!Region)
    return nullptr;
  while (true) {
    const VarRegion *VR = Region->getAs<VarRegion>();
    if (VR && VR->getDecl()->hasGlobalStorage()) {
      return VR;
    }
    const SymbolicRegion *SymR = Region->getAs<SymbolicRegion>();
    if (SymR) {
      const SymbolRegionValue *SymRV =
          dyn_cast<SymbolRegionValue>(SymR->getSymbol());
      if (!SymRV) {
        const SymbolDerived *SD = dyn_cast<SymbolDerived>(SymR->getSymbol());
        if (SD) {
          Region = SD->getRegion();
          continue;
        }
        break;
      }
      Region = SymRV->getRegion();
      continue;
    }
    const SubRegion *SR = Region->getAs<SubRegion>();
    if (!SR)
      break;
    Region = SR->getSuperRegion();
  }
  return nullptr;
}
} // namespace Helpers

GCChecker::LivenessState
GCChecker::LivenessState::getForArgument(const FunctionDecl *FD,
                                         const ParmVarDecl *PVD,
                                         bool isFunctionSafepoint) {
  bool maybeUnrooted = declHasAnnotation(PVD, "julia_maybe_unrooted");
  LivenessState VS = getAllocated();
  if (!isFunctionSafepoint || maybeUnrooted) {
    VS.PVD = PVD;
    VS.FD = FD;
  }
  return VS;
}

GCChecker::GCObjectSet
GCChecker::emptyObjectSet(const ProgramStateRef &State) {
  return State->get_context<GCObjectSet>().getEmptySet();
}

GCChecker::GCRegionSet
GCChecker::emptyRegionSet(const ProgramStateRef &State) {
  return State->get_context<GCRegionSet>().getEmptySet();
}

GCChecker::GCRootList
GCChecker::emptyRootList(const ProgramStateRef &State) {
  return State->get_context<GCRootList>().getEmptyList();
}

bool GCChecker::objectSetContains(GCObjectSet Objects, GCObject Object) {
  return Object && Objects.contains(Object);
}

bool GCChecker::objectSetHasMultiple(GCObjectSet Objects) {
  unsigned Count = 0;
  for (GCObject Object : Objects) {
    (void)Object;
    if (++Count > 1)
      return true;
  }
  return false;
}

GCChecker::GCObjectSet
GCChecker::singletonObjectSet(const ProgramStateRef &State, GCObject Object) {
  GCObjectSet Objects = emptyObjectSet(State);
  if (Object)
    Objects = State->get_context<GCObjectSet>().add(Objects, Object);
  return Objects;
}

bool GCChecker::isAllocaExpr(const Stmt *S) {
  const auto *E = dyn_cast_or_null<Expr>(S);
  if (!E)
    return false;
  E = E->IgnoreParenCasts();
  const auto *CE = dyn_cast<CallExpr>(E);
  if (!CE)
    return false;
  const FunctionDecl *FD = CE->getDirectCallee();
  if (!FD)
    FD = dyn_cast_or_null<FunctionDecl>(CE->getCalleeDecl());
  StringRef FDName =
      FD && FD->getDeclName().isIdentifier() ? FD->getName() : "";
  return FDName == "alloca" || FDName == "__builtin_alloca";
}

bool GCChecker::isAllocaDerivedRegion(const ProgramStateRef &State,
                                      const MemRegion *Region,
                                      unsigned Depth) const {
  if (!Region || Depth > 8)
    return false;
  Region = Region->StripCasts();
  if (Region->getAs<AllocaRegion>())
    return true;
  if (const auto *SR = Region->getAs<SubRegion>()) {
    if (isAllocaDerivedRegion(State, SR->getSuperRegion(), Depth + 1))
      return true;
  }
  if (const auto *SymR = Region->getAs<SymbolicRegion>())
    return isAllocaDerivedSymbol(State, SymR->getSymbol(), Depth + 1);
  if (const auto *TVR = Region->getAs<TypedValueRegion>())
    return isAllocaDerivedSVal(State, State->getSVal(TVR), Depth + 1);
  return false;
}

bool GCChecker::isAllocaDerivedSymbol(const ProgramStateRef &State,
                                      SymbolRef Sym, unsigned Depth) const {
  if (!Sym || Depth > 8)
    return false;
  if (const auto *SCast = dyn_cast<SymbolCast>(Sym))
    return isAllocaDerivedSymbol(State, SCast->getOperand(), Depth + 1);
  if (const auto *SConjured = dyn_cast<SymbolConjured>(Sym))
    return isAllocaExpr(SConjured->getStmt());
  if (isGCObjectType(Sym->getType()))
    return false;
  if (const auto *SDerived = dyn_cast<SymbolDerived>(Sym)) {
    return isAllocaDerivedSymbol(State, SDerived->getParentSymbol(),
                                 Depth + 1);
  }
  return isAllocaDerivedRegion(State, Sym->getOriginRegion(), Depth + 1);
}

bool GCChecker::isAllocaDerivedSVal(const ProgramStateRef &State, SVal V,
                                    unsigned Depth) const {
  if (Depth > 8)
    return false;
  if (const MemRegion *Region = V.getAsRegion())
    return isAllocaDerivedRegion(State, Region, Depth + 1);
  if (SymbolRef Sym = V.getAsSymbol(true))
    return isAllocaDerivedSymbol(State, Sym, Depth + 1);
  return false;
}

GCChecker::GCObject
GCChecker::ensureObjectForSymbol(SymbolRef Sym, ProgramStateRef &State,
                                 CheckerContext &C,
                                 LivenessState InitialState,
                                 QualType ObjectType) const {
  if (!Sym)
    return nullptr;
  QualType Type = ObjectType.isNull() ? Sym->getType() : ObjectType;
  if (isAllocaDerivedSymbol(State, Sym))
    return nullptr;
  if (!isGCObjectType(Type))
    return nullptr;
  if (std::optional<LivenessState> ExistingSymbolState =
          getStateForSymbol(State, Sym))
    InitialState = *ExistingSymbolState;
  GCObjectSet ExistingObjects = getObjectsForSymbol(State, Sym);
  if (!ExistingObjects.isEmpty()) {
    for (GCObject ExistingObject : ExistingObjects) {
      const LivenessState *ExistingState =
          State->get<GCObjectStateMap>(ExistingObject);
      bool ShouldUpdate =
          !ExistingState ||
          (InitialState.isPotentiallyFreed() &&
           !ExistingState->isPotentiallyFreed()) ||
          (InitialState.isUntracked() && ExistingState->isJustAllocated());
      if (ShouldUpdate)
        State = State->set<GCObjectStateMap>(ExistingObject, InitialState);
      if (const MemRegion *ObjectRegion = getObjectRegion(ExistingObject, C))
        State = setObjectsForRegion(State, ObjectRegion,
                                    singletonObjectSet(State, ExistingObject),
                                    C);
    }
    State = setObjectsForSymbol(State, Sym, ExistingObjects, C);
    return *ExistingObjects.begin();
  }
  GCObject Object = dyn_cast<SymbolConjured>(Sym);
  if (!Object) {
    if (const SymbolCast *SCast = dyn_cast<SymbolCast>(Sym))
      Object = ensureObjectForSymbol(SCast->getOperand(), State, C,
                                     InitialState, ObjectType);
    if (Object) {
      State = setObjectsForSymbol(State, Sym,
                                  singletonObjectSet(State, Object), C);
      return Object;
    }
  }
  if (!Object) {
    SVal S = C.getSValBuilder().conjureSymbolVal(
        C.getCFGElementRef(), C.getLocationContext(), Type,
        C.blockCount());
    Object = dyn_cast_or_null<SymbolConjured>(S.getAsSymbol());
  }
  if (!Object)
    return nullptr;
  const LivenessState *ExistingState = State->get<GCObjectStateMap>(Object);
  bool ShouldUpdate =
      !ExistingState ||
      (InitialState.isPotentiallyFreed() &&
       !ExistingState->isPotentiallyFreed()) ||
      (InitialState.isUntracked() && ExistingState->isJustAllocated());
  if (ShouldUpdate)
    State = State->set<GCObjectStateMap>(Object, InitialState);
  State = setObjectsForSymbol(State, Sym, singletonObjectSet(State, Object), C);
  const MemRegion *ObjectRegion = getObjectRegion(Object, C);
  if (ObjectRegion)
    State = setObjectsForRegion(State, ObjectRegion,
                                singletonObjectSet(State, Object), C);
  return Object;
}

GCChecker::GCObjectSet
GCChecker::getObjectsForSymbol(const ProgramStateRef &State,
                               SymbolRef Sym) {
  if (!Sym)
    return emptyObjectSet(State);
  if (const SymbolCast *SCast = dyn_cast<SymbolCast>(Sym))
    return getObjectsForSymbol(State, SCast->getOperand());
  auto getMappedRegionObjects = [&State](const MemRegion *Region) {
    if (!Region)
      return emptyObjectSet(State);
    Region = Region->StripCasts();
    if (const GCObjectSet *Objects = State->get<GCRegionObjectMap>(Region))
      return *Objects;
    return emptyObjectSet(State);
  };
  if (const SymbolRegionValue *SRV = dyn_cast<SymbolRegionValue>(Sym)) {
    GCObjectSet Objects = getMappedRegionObjects(SRV->getRegion());
    if (!Objects.isEmpty())
      return Objects;
  }
  if (GCObject Object = dyn_cast<SymbolConjured>(Sym)) {
    if (!isGCObjectType(getAtomicValueType(Sym->getType())))
      return emptyObjectSet(State);
    return singletonObjectSet(State, Object);
  }
  if (const MemRegion *Origin = Sym->getOriginRegion()) {
    GCObjectSet Objects = getMappedRegionObjects(Origin);
    if (!Objects.isEmpty())
      return Objects;
  }
  return emptyObjectSet(State);
}

std::optional<GCChecker::LivenessState>
GCChecker::getStateForSymbol(const ProgramStateRef &State, SymbolRef Sym) {
  GCObjectSet Objects = getObjectsForSymbol(State, Sym);
  if (Objects.isEmpty())
    return std::nullopt;
  bool SawTrackedObject = false;
  for (GCObject Object : Objects) {
    if (State->get<GCObjectStateMap>(Object)) {
      SawTrackedObject = true;
      break;
    }
  }
  if (!SawTrackedObject)
    return std::nullopt;
  return aggregateObjectState(State, Objects);
}

GCChecker::GCObjectSet
GCChecker::getDerivedParentObjectsForSymbol(const ProgramStateRef &State,
                                            SymbolRef Sym) const {
  if (!Sym)
    return emptyObjectSet(State);
  if (const SymbolCast *SCast = dyn_cast<SymbolCast>(Sym))
    return getDerivedParentObjectsForSymbol(State, SCast->getOperand());
  if (const SymbolDerived *SDerived = dyn_cast<SymbolDerived>(Sym)) {
    GCObjectSet Objects =
        getObjectsForSymbol(State, SDerived->getParentSymbol());
    if (Objects.isEmpty())
      Objects = getDerivedParentObjectsForSymbol(State,
                                                 SDerived->getParentSymbol());
    if (Objects.isEmpty())
      Objects = getObjectsForRegion(State, SDerived->getRegion());
    if (Objects.isEmpty())
      Objects = getTrackedParentObjects(State, SDerived->getRegion());
    return Objects;
  }
  return emptyObjectSet(State);
}

GCChecker::GCObjectSet
GCChecker::getObjectsForSVal(const ProgramStateRef &State, SVal V) {
  if (SymbolRef Sym = V.getAsSymbol())
    return getObjectsForSymbol(State, Sym);
  if (const MemRegion *Region = V.getAsRegion())
    return getObjectsForRegion(State, Region);
  if (auto LCV = V.getAs<nonloc::LazyCompoundVal>())
    return getObjectsForRegion(State, LCV->getRegion());
  if (SymbolRef Sym = V.getAsSymbol(true))
    return getObjectsForSymbol(State, Sym);
  return emptyObjectSet(State);
}

GCChecker::GCObjectSet
GCChecker::getObjectsForRegion(const ProgramStateRef &State,
                               const MemRegion *Region) {
  if (!Region)
    return emptyObjectSet(State);
  Region = Region->StripCasts();
  if (const GCObjectSet *Objects = State->get<GCRegionObjectMap>(Region))
    return *Objects;
  if (const SymbolicRegion *SR = Region->getAs<SymbolicRegion>())
    return getObjectsForSymbol(State, SR->getSymbol());
  return emptyObjectSet(State);
}

GCChecker::GCObjectSet
GCChecker::getObjectsOwningRegion(const ProgramStateRef &State,
                                  const MemRegion *Region) {
  GCObjectSet Objects = emptyObjectSet(State);
  if (!Region)
    return Objects;
  Region = Region->StripCasts();
  if (const GCObjectSet *OwnerObjects =
          State->get<GCDerivedRegionOwnerMap>(Region)) {
    for (GCObject Object : *OwnerObjects)
      Objects = State->get_context<GCObjectSet>().add(Objects, Object);
  }
  if (!Objects.isEmpty())
    return Objects;
  GCObjectRegionMapTy ObjectRegions = State->get<GCObjectRegionMap>();
  for (auto I = ObjectRegions.begin(), E = ObjectRegions.end(); I != E; ++I) {
    for (const MemRegion *OwnedRegion : I.getData()) {
      if (OwnedRegion && OwnedRegion->StripCasts() == Region)
        Objects = State->get_context<GCObjectSet>().add(Objects, I.getKey());
    }
  }
  return Objects;
}

GCChecker::GCObjectSet
GCChecker::getTrackedParentObjects(const ProgramStateRef &State,
                                   const MemRegion *Region) {
  GCObjectSet Objects = emptyObjectSet(State);
  if (!Region)
    return Objects;
  bool IsInitialRegion = true;
  for (const MemRegion *Cur = Region->StripCasts(); Cur;) {
    if (!IsInitialRegion || Cur->getAs<SymbolicRegion>()) {
      GCObjectSet CurObjects = getObjectsForRegion(State, Cur);
      for (GCObject Object : CurObjects)
        Objects = State->get_context<GCObjectSet>().add(Objects, Object);
      if (!Objects.isEmpty())
        return Objects;
    }

    GCObjectSet OwnerObjects = getObjectsOwningRegion(State, Cur);
    for (GCObject Object : OwnerObjects)
      Objects = State->get_context<GCObjectSet>().add(Objects, Object);
    if (!Objects.isEmpty())
      return Objects;

    if (const auto *SR = Cur->getAs<SubRegion>()) {
      const MemRegion *ParentRegion = SR->getSuperRegion()->StripCasts();
      GCObjectSet ParentObjects = getObjectsForRegion(State, ParentRegion);
      for (GCObject Object : ParentObjects)
        Objects = State->get_context<GCObjectSet>().add(Objects, Object);
      ParentObjects = getObjectsOwningRegion(State, ParentRegion);
      for (GCObject Object : ParentObjects)
        Objects = State->get_context<GCObjectSet>().add(Objects, Object);
      if (!Objects.isEmpty())
        return Objects;
    }

    const auto *SR = Cur->getAs<SubRegion>();
    if (!SR)
      break;
    Cur = SR->getSuperRegion()->StripCasts();
    IsInitialRegion = false;
  }
  SymbolRef Root = walkToRoot(
      [](SymbolRef Sym, const LivenessState *OldVState) { return !OldVState; },
      State, Region);
  if (Root) {
    GCObjectSet RootObjects = getObjectsForSymbol(State, Root);
    for (GCObject Object : RootObjects)
      Objects = State->get_context<GCObjectSet>().add(Objects, Object);
  }
  return Objects;
}

GCChecker::GCObjectSet
GCChecker::getObjectsForRegionOrParents(const ProgramStateRef &State,
                                         const MemRegion *Region) {
  GCObjectSet Objects = getObjectsForRegion(State, Region);
  if (Objects.isEmpty())
    Objects = getTrackedParentObjects(State, Region);
  return Objects;
}

GCChecker::GCObjectSet
GCChecker::getObjectsForExprValue(const Expr *E, SVal V,
                                  ProgramStateRef State,
                                  CheckerContext &C) const {
  GCObjectSet Objects = getObjectsForSVal(State, V);
  if (Objects.isEmpty()) {
    if (SymbolRef Sym = V.getAsSymbol(true))
      Objects = getDerivedParentObjectsForSymbol(State, Sym);
  }
  if (Objects.isEmpty()) {
    if (const MemRegion *Region = V.getAsRegion())
      Objects = getTrackedParentObjects(State, Region);
  }
  if (Objects.isEmpty() && E) {
    if (const MemRegion *StorageRegion =
            getStorageRegionForExpr(ignoreOuterPointerCasts(E), State, C))
      Objects = getObjectsForRegionOrParents(State, StorageRegion);
  }
  return Objects;
}

ProgramStateRef
GCChecker::setObjectsForSymbol(ProgramStateRef State, SymbolRef Sym,
                               GCObjectSet Objects,
                               CheckerContext &C) const {
  (void)C;
  if (!Sym)
    return State;
  if (const SymbolCast *SCast = dyn_cast<SymbolCast>(Sym))
    return setObjectsForSymbol(State, SCast->getOperand(), Objects, C);
  if (const SymbolRegionValue *SRV = dyn_cast<SymbolRegionValue>(Sym))
    return setObjectsForRegion(State, SRV->getRegion(), Objects, C);
  const MemRegion *Origin = Sym->getOriginRegion();
  if (!Origin)
    return State;
  return setObjectsForRegion(State, Origin, Objects, C);
}

ProgramStateRef
GCChecker::setObjectsForRegion(ProgramStateRef State, const MemRegion *Region,
                               GCObjectSet Objects,
                               CheckerContext &C) const {
  (void)C;
  if (!Region)
    return State;
  Region = Region->StripCasts();
  return State->set<GCRegionObjectMap>(Region, Objects);
}

bool GCChecker::regionBindingMayAliasMultipleLocations(
    const ProgramStateRef &State, const MemRegion *Region) const {
  if (!Region)
    return false;
  Region = Region->StripCasts();
  if (const auto *ER = Region->getAs<ElementRegion>())
    return !ER->getIndex().getAs<nonloc::ConcreteInt>();
  if (const auto *SR = Region->getAs<SubRegion>()) {
    const MemRegion *Super = SR->getSuperRegion()->StripCasts();
    if (objectSetHasMultiple(getObjectsForRegion(State, Super)) ||
        objectSetHasMultiple(getObjectsOwningRegion(State, Super)) ||
        objectSetHasMultiple(getTrackedParentObjects(State, Super)))
      return true;
  }
  return false;
}

bool GCChecker::isMutableStorageCarrierRegion(const MemRegion *Region) {
  if (!Region)
    return false;
  Region = Region->StripCasts();
  if (Region->getAs<ElementRegion>() || Region->getAs<SymbolicRegion>())
    return false;
  while (Region) {
    Region = Region->StripCasts();
    if (Region->getAs<VarRegion>() || Region->getAs<AllocaRegion>())
      return true;
    const auto *SR = Region->getAs<SubRegion>();
    if (!SR)
      return false;
    Region = SR->getSuperRegion();
  }
  return false;
}

ProgramStateRef
GCChecker::addObjectRegionEdge(ProgramStateRef State, GCObject Object,
                               const MemRegion *Region) const {
  if (!Object || !Region)
    return State;
  Region = Region->StripCasts();
  if (isMutableStorageCarrierRegion(Region)) {
    GCObjectSet Owners = emptyObjectSet(State);
    if (const GCObjectSet *OldOwners =
            State->get<GCDerivedRegionOwnerMap>(Region))
      Owners = *OldOwners;
    Owners = State->get_context<GCObjectSet>().add(Owners, Object);
    return State->set<GCDerivedRegionOwnerMap>(Region, Owners);
  }
  GCRegionSet Regions = emptyRegionSet(State);
  if (const GCRegionSet *OldRegions = State->get<GCObjectRegionMap>(Object))
    Regions = *OldRegions;
  Regions = State->get_context<GCRegionSet>().add(Regions, Region);
  return State->set<GCObjectRegionMap>(Object, Regions);
}

ProgramStateRef
GCChecker::addObjectRegionEdges(ProgramStateRef State, GCObjectSet Objects,
                                const MemRegion *Region) const {
  for (GCObject Object : Objects)
    State = addObjectRegionEdge(State, Object, Region);
  return State;
}

ProgramStateRef GCChecker::addObjectOwnershipEdge(ProgramStateRef State,
                                                  GCObject Owner,
                                                  GCObjectSet OwnedObjects) const {
  if (!Owner || OwnedObjects.isEmpty())
    return State;
  if (const GCObjectSet *Existing = State->get<GCObjectOwnershipMap>(Owner)) {
    for (GCObject Object : *Existing)
      OwnedObjects = State->get_context<GCObjectSet>().add(OwnedObjects,
                                                           Object);
  }
  return State->set<GCObjectOwnershipMap>(Owner, OwnedObjects);
}

ProgramStateRef GCChecker::addObjectOwnershipEdges(ProgramStateRef State,
                                                   GCObjectSet Owners,
                                                   GCObjectSet OwnedObjects) const {
  if (OwnedObjects.isEmpty())
    return State;
  for (GCObject Owner : Owners)
    State = addObjectOwnershipEdge(State, Owner, OwnedObjects);
  return State;
}

ProgramStateRef
GCChecker::bindRegionToObjects(ProgramStateRef State, const MemRegion *Region,
                               GCObjectSet Objects,
                               CheckerContext &C) const {
  if (!Region)
    return State;
  Region = Region->StripCasts();
  GCObjectSet OldObjects = emptyObjectSet(State);
  if (const GCObjectSet *Existing = State->get<GCRegionObjectMap>(Region))
    OldObjects = *Existing;
  if (State->contains<GCConservativeRootRegions>(Region) ||
      regionBindingMayAliasMultipleLocations(State, Region)) {
    for (GCObject Object : OldObjects)
      Objects = State->get_context<GCObjectSet>().add(Objects, Object);
  }
  if (const auto *SR = Region->getAs<SymbolicRegion>()) {
    if (GCObject RegionObject = dyn_cast<SymbolConjured>(SR->getSymbol()))
      Objects = State->get_context<GCObjectSet>().add(Objects, RegionObject);
  }
  State = setObjectsForRegion(State, Region, Objects, C);
  return State;
}

ProgramStateRef
GCChecker::bindRegionToSVal(ProgramStateRef State, const MemRegion *Region,
                            SVal V, CheckerContext &C) const {
  GCObjectSet Objects = getObjectsForSVal(State, V);
  if (Objects.isEmpty()) {
    if (SymbolRef Sym = V.getAsSymbol(true)) {
      if (GCObject Object = ensureObjectForSymbol(Sym, State, C))
        Objects = singletonObjectSet(State, Object);
    }
  }
  return bindRegionToObjects(State, Region, Objects, C);
}

ProgramStateRef GCChecker::bindRootRegionToCurrentValue(
    ProgramStateRef State, const MemRegion *Region, QualType ValueType,
    CheckerContext &C, RootValueBinding Binding) const {
  if (!Region)
    return State;
  if (ValueType.isNull()) {
    if (const auto *TVR = Region->getAs<TypedValueRegion>())
      ValueType = TVR->getValueType();
  }
  SVal Value = State->getSVal(Region);
  GCObjectSet ValueObjects = getObjectsForSVal(State, Value);
  bool HasGCObjectValue = !ValueObjects.isEmpty();
  bool MayBeUnknownObject =
      Value.isUnknown() || Value.getAsSymbol(true) || Value.getAsRegion();
  bool ShouldConjure =
      (Binding == RootValueBinding::ConjureUnknownOnly &&
       MayBeUnknownObject && !Value.isZeroConstant() && !HasGCObjectValue) ||
      (Binding == RootValueBinding::ConjurePossibleOutValue &&
       !HasGCObjectValue);
  if (ShouldConjure && !ValueType.isNull() &&
      isGCTrackedType(ValueType)) {
    Value = C.getSValBuilder().conjureSymbolVal(
        nullptr, C.getCFGElementRef(), C.getLocationContext(), ValueType,
        C.blockCount());
    State = State->bindLoc(loc::MemRegionVal(Region), Value,
                           C.getLocationContext());
    ValueObjects = getObjectsForSVal(State, Value);
  }
  GCObjectSet Objects = ValueObjects;
  if (Objects.isEmpty())
    Objects = getTrackedParentObjects(State, Region);
  if (!Objects.isEmpty())
    return bindRegionToObjects(State, Region, Objects, C);
  return bindRegionToSVal(State, Region, Value, C);
}

const MemRegion *GCChecker::getStorageRegionForExpr(const Expr *E,
                                                    ProgramStateRef State,
                                                    CheckerContext &C) const {
  if (!E)
    return nullptr;
  const Expr *Inner = E->IgnoreParenCasts();
  const auto *LCtx = C.getLocationContext();
  if (const auto *UO = dyn_cast<UnaryOperator>(Inner)) {
    if (UO->getOpcode() == UO_Deref) {
      if (const MemRegion *Region =
              getRegionForPointerValue(State->getSVal(UO->getSubExpr(), LCtx),
                                       C))
        return Region->StripCasts();
    }
  }
  if (const auto *ME = dyn_cast<MemberExpr>(Inner)) {
    if (const auto *FD = dyn_cast<FieldDecl>(ME->getMemberDecl())) {
      SVal Base = State->getSVal(ME->getBase(), LCtx);
      if (!Base.getAsRegion()) {
        if (const MemRegion *BaseRegion =
                getStorageRegionForExpr(ME->getBase(), State, C))
          Base = loc::MemRegionVal(BaseRegion);
      }
      if (const MemRegion *Region = State->getLValue(FD, Base).getAsRegion())
        return Region->StripCasts();
    }
  }
  if (const auto *DRE = dyn_cast<DeclRefExpr>(Inner)) {
    if (const auto *VD = dyn_cast<VarDecl>(DRE->getDecl())) {
      if (const MemRegion *Region = State->getLValue(VD, LCtx).getAsRegion())
        return Region->StripCasts();
    }
  }
  SVal InnerValue = State->getSVal(Inner, LCtx);
  if (const MemRegion *Region = InnerValue.getAsRegion())
    return Region->StripCasts();
  if (auto LCV = InnerValue.getAs<nonloc::LazyCompoundVal>())
    return LCV->getRegion()->StripCasts();
  return nullptr;
}

const MemRegion *GCChecker::getBindingRegionForExprResult(
    const Expr *E, ProgramStateRef State, CheckerContext &C) const {
  if (!E)
    return nullptr;
  const Expr *Result = E->IgnoreParenCasts();
  const Stmt *Cur = E;
  while (Cur) {
    auto Parents = C.getASTContext().getParents(*Cur);
    if (Parents.empty())
      return nullptr;
    if (const auto *VD = Parents[0].get<VarDecl>()) {
      if (VD->getInit() && VD->getInit()->IgnoreParenCasts() == Result) {
        if (const MemRegion *Region =
                State->getLValue(VD, C.getLocationContext()).getAsRegion())
          return Region->StripCasts();
      }
      return nullptr;
    }
    if (const auto *BO = Parents[0].get<BinaryOperator>()) {
      if (BO->isAssignmentOp() && BO->getRHS()->IgnoreParenCasts() == Result)
        return getStorageRegionForExpr(BO->getLHS(), State, C);
      return nullptr;
    }
    if (const auto *ParentExpr = Parents[0].get<Expr>()) {
      Cur = ParentExpr;
      continue;
    }
    return nullptr;
  }
  return nullptr;
}

const MemRegion *GCChecker::getRootingRegionForExpr(const Expr *E,
                                                    ProgramStateRef State,
                                                    CheckerContext &C) const {
  if (!E)
    return nullptr;
  const Expr *Inner = E->IgnoreParenCasts();
  const auto *LCtx = C.getLocationContext();
  if (const auto *UO = dyn_cast<UnaryOperator>(Inner)) {
    if (UO->getOpcode() == UO_Deref) {
      if (const MemRegion *Region =
              getRegionForPointerValue(State->getSVal(UO->getSubExpr(), LCtx),
                                       C)) {
        Region = Region->StripCasts();
        if (isRootingRegion(State, Region) || State->get<GCRootMap>(Region))
          return Region;
      }
    }
  }
  if (const auto *ME = dyn_cast<MemberExpr>(Inner)) {
    if (const auto *FD = dyn_cast<FieldDecl>(ME->getMemberDecl())) {
      SVal Base = State->getSVal(ME->getBase(), LCtx);
      if (!Base.getAsRegion()) {
        if (const MemRegion *BaseRegion =
                getStorageRegionForExpr(ME->getBase(), State, C))
          Base = loc::MemRegionVal(BaseRegion);
      }
      if (const MemRegion *Region = State->getLValue(FD, Base).getAsRegion()) {
        Region = Region->StripCasts();
        if (isRootingRegion(State, Region) || State->get<GCRootMap>(Region))
          return Region;
      }
    }
  }
  if (const MemRegion *Region = State->getSVal(Inner, LCtx).getAsRegion()) {
    Region = Region->StripCasts();
    if (isRootingRegion(State, Region) || State->get<GCRootMap>(Region))
      return Region;
  }
  return nullptr;
}

SVal GCChecker::getOrConjurePointerValue(Loc LValue, QualType Type,
                                         ProgramStateRef &State,
                                         CheckerContext &C) const {
  SVal Value = State->getSVal(LValue);
  if (!Value.isUnknown() && (Value.getAsRegion() || Value.getAsSymbol(true)))
    return Value;
  if (Type.isNull() || !Type->isPointerType())
    return Value;
  Value = C.getSValBuilder().conjureSymbolVal(
      nullptr, C.getCFGElementRef(), C.getLocationContext(), Type,
      C.blockCount());
  State = State->bindLoc(LValue, Value, C.getLocationContext());
  return Value;
}

GCChecker::GCObjectSet GCChecker::getObjectsForRootPropagatingArgument(
  const CallEvent &Call, unsigned ArgIdx, ProgramStateRef &State,
  CheckerContext &C) const {
  const Expr *ArgExpr = Call.getArgExpr(ArgIdx);
  const Expr *ValueExpr = ignoreOuterPointerCasts(ArgExpr);
  SVal Arg = Call.getArgSVal(ArgIdx);
  GCObjectSet Objects = getObjectsForSVal(State, Arg);
  const MemRegion *RootRegion = getRootingRegionForExpr(ValueExpr, State, C);
  SymbolRef ArgSym = Arg.getAsSymbol(true);
  if (Objects.isEmpty() && ValueExpr) {
    if (const MemRegion *StorageRegion =
            getStorageRegionForExpr(ValueExpr, State, C))
      Objects = getObjectsForRegionOrParents(State, StorageRegion);
  }
  if (!RootRegion) {
    if (ArgSym) {
      if (const MemRegion *Origin = ArgSym->getOriginRegion()) {
        Origin = Origin->StripCasts();
        if (isRootingRegion(State, Origin) || State->get<GCRootMap>(Origin))
          RootRegion = Origin;
      }
    }
  }
  if (Objects.isEmpty() && ArgSym)
    Objects = getDerivedParentObjectsForSymbol(State, ArgSym);
  if (Objects.isEmpty() && RootRegion) {
    State = bindRootRegionToCurrentValue(
        State, RootRegion, ValueExpr ? ValueExpr->getType() : QualType(), C);
    Objects = getObjectsForRegion(State, RootRegion);
  }
  if (Objects.isEmpty()) {
    if (ArgSym) {
      if (GCObject Object = ensureObjectForSymbol(ArgSym, State, C))
        Objects = singletonObjectSet(State, Object);
    }
  }
  if (!Objects.isEmpty() && RootRegion)
    State = bindRegionToObjects(State, RootRegion, Objects, C);
  return Objects;
}

GCChecker::GCObjectSet GCChecker::getObjectsForGenericMemoryRef(
    const Expr *E, SVal V, QualType Type, ProgramStateRef &State,
    CheckerContext &C) const {
  GCObjectSet Objects = emptyObjectSet(State);
  const FieldDecl *MemField = getGenericMemoryRefMemField(Type);
  if (!MemField)
    return Objects;

  const MemRegion *BaseRegion = nullptr;
  if (auto LCV = V.getAs<nonloc::LazyCompoundVal>())
    BaseRegion = LCV->getRegion();
  if (!BaseRegion)
    BaseRegion = V.getAsRegion();
  if (!BaseRegion)
    BaseRegion = getStorageRegionForExpr(E, State, C);

  if (!BaseRegion)
    return Objects;
  BaseRegion = BaseRegion->StripCasts();

  SVal Base = loc::MemRegionVal(BaseRegion);
  const MemRegion *MemFieldRegion =
      State->getLValue(MemField, Base).getAsRegion();
  GCObjectSet CarrierObjects = getObjectsForRegionOrParents(State, BaseRegion);
  if (MemFieldRegion) {
    MemFieldRegion = MemFieldRegion->StripCasts();
    SVal MemValue = State->getSVal(MemFieldRegion);
    Objects = getObjectsForSVal(State, MemValue);
    if (Objects.isEmpty()) {
      if (const MemRegion *MemValueRegion = MemValue.getAsRegion())
        Objects = getObjectsForRegionOrParents(State, MemValueRegion);
    }
    if (Objects.isEmpty())
      Objects = CarrierObjects;
    if (!Objects.isEmpty()) {
      State = bindRegionToObjects(State, MemFieldRegion, Objects, C);
      return Objects;
    }
  }

  return CarrierObjects;
}

const MemRegion *GCChecker::getRegionForPointerValue(SVal V,
                                                     CheckerContext &C) const {
  if (const MemRegion *Region = V.getAsRegion())
    return Region->StripCasts();
  if (SymbolRef Sym = V.getAsSymbol(true))
    return C.getSValBuilder().getRegionManager().getSymbolicRegion(Sym);
  return nullptr;
}

ProgramStateRef
GCChecker::addFrameRoot(ProgramStateRef State, unsigned Depth,
                        const MemRegion *Region, CheckerContext &C) const {
  if (!Region)
    return State;
  Region = Region->StripCasts();
  GCRootList Roots = emptyRootList(State);
  if (const GCRootList *OldRoots = State->get<GCRootFrameMap>(Depth))
    Roots = *OldRoots;
  Roots = State->get_context<GCRootList>().add(Region, Roots);
  State = State->set<GCRootFrameMap>(Depth, Roots);
  return bindRootRegionToCurrentValue(State, Region, QualType(), C);
}

ProgramStateRef
GCChecker::removeFrameRoots(ProgramStateRef State, unsigned Depth) const {
  return State->remove<GCRootFrameMap>(Depth);
}

bool GCChecker::isRootingRegion(const ProgramStateRef &State,
                                const MemRegion *Region) {
  if (!Region)
    return false;
  Region = Region->StripCasts();
  if (State->contains<GCPermanentRootRegions>(Region))
    return true;
  if (State->get<GCRootMap>(Region))
    return true;
  if (const auto *ER = Region->getAs<ElementRegion>()) {
    if (isRootingRegion(State, ER->getSuperRegion()))
      return true;
  }
  return false;
}

ProgramStateRef GCChecker::addPermanentRoot(ProgramStateRef State,
                                            const MemRegion *Region) const {
  if (!Region)
    return State;
  Region = Region->StripCasts();
  return State->add<GCPermanentRootRegions>(Region);
}

ProgramStateRef GCChecker::promiseRootedSVal(ProgramStateRef State, SVal V,
                                             CheckerContext &C,
                                             QualType ObjectType) const {
  GCObjectSet Objects = getObjectsForSVal(State, V);
  SymbolRef Sym = V.getAsSymbol(true);
  if (Sym && Objects.isEmpty()) {
    if (GCObject Object = ensureObjectForSymbol(
            Sym, State, C, LivenessState::getAllocated(), ObjectType))
      Objects = singletonObjectSet(State, Object);
  }
  if (const MemRegion *Region = V.getAsRegion()) {
    State = addPermanentRoot(State, Region);
    if (!Objects.isEmpty())
      State = setObjectsForRegion(State, Region, Objects, C);
  }
  for (GCObject Object : Objects)
    State = addPermanentRoot(State, getObjectRegion(Object, C));
  return State;
}

const MemRegion *GCChecker::getObjectRegion(GCObject Object,
                                            CheckerContext &C) const {
  if (!Object)
    return nullptr;
  return C.getSValBuilder().getRegionManager().getSymbolicRegion(Object);
}

const MemRegion *GCChecker::getIndexedObjectRegion(GCObject Object,
                                                   NonLoc Index,
                                                   CheckerContext &C) const {
  const auto *Base = dyn_cast_or_null<SubRegion>(getObjectRegion(Object, C));
  if (!Base)
    return nullptr;
  return C.getSValBuilder().getRegionManager().getElementRegion(
      C.getASTContext().VoidPtrTy, Index, Base, C.getASTContext());
}

const MemRegion *GCChecker::getIndexedObjectRegion(GCObject Object,
                                                   uint64_t Index,
                                                   CheckerContext &C) const {
  return getIndexedObjectRegion(Object, C.getSValBuilder().makeArrayIndex(Index),
                                C);
}

const MemRegion *
GCChecker::getTrackedParentRegion(const MemRegion *Region) const {
  if (!Region)
    return nullptr;
  for (const MemRegion *Cur = Region->StripCasts(); Cur;) {
    if (const auto *TVR = Cur->getAs<TypedValueRegion>()) {
      if (isGCTrackedType(TVR->getValueType())) {
        if (const auto *SR = Cur->getAs<SubRegion>())
          return SR->getSuperRegion();
      }
    }
    const auto *SR = Cur->getAs<SubRegion>();
    if (!SR)
      break;
    Cur = SR->getSuperRegion()->StripCasts();
  }
  return nullptr;
}

GCChecker::GCObjectSet
GCChecker::computeReachableObjects(const ProgramStateRef &State) {
  GCObjectSet Reachable = emptyObjectSet(State);
  SmallVector<const MemRegion *, 32> RegionWorklist;
  llvm::SmallPtrSet<const MemRegion *, 32> SeenRegions;
  SmallVector<GCObject, 32> ObjectWorklist;
  llvm::SmallPtrSet<GCObject, 32> SeenObjects;

  for (const MemRegion *Region : State->get<GCPermanentRootRegions>())
    RegionWorklist.push_back(Region);
  GCRootFrameMapTy Frames = State->get<GCRootFrameMap>();
  for (auto I = Frames.begin(), E = Frames.end(); I != E; ++I) {
    for (const MemRegion *Region : I.getData())
      RegionWorklist.push_back(Region);
  }
  GCRegionObjectMapTy RegionObjects = State->get<GCRegionObjectMap>();
  for (auto I = RegionObjects.begin(), E = RegionObjects.end(); I != E; ++I) {
    if (isRootingRegion(State, I.getKey()))
      RegionWorklist.push_back(I.getKey());
  }

  while (!RegionWorklist.empty() || !ObjectWorklist.empty()) {
    while (!RegionWorklist.empty()) {
      const MemRegion *Region = RegionWorklist.pop_back_val();
      if (!Region || !SeenRegions.insert(Region).second)
        continue;
      GCObjectSet Objects = getObjectsForRegion(State, Region);
      for (GCObject Object : Objects)
        ObjectWorklist.push_back(Object);
    }
    if (ObjectWorklist.empty())
      break;
    GCObject Object = ObjectWorklist.pop_back_val();
    if (!Object || !SeenObjects.insert(Object).second)
      continue;
    Reachable = State->get_context<GCObjectSet>().add(Reachable, Object);
    RegionWorklist.push_back(
        State->getStateManager().getRegionManager().getSymbolicRegion(Object));
    if (const GCRegionSet *Regions = State->get<GCObjectRegionMap>(Object)) {
      for (const MemRegion *Region : *Regions)
        if (!isMutableStorageCarrierRegion(Region))
          RegionWorklist.push_back(Region);
    }
    if (const GCObjectSet *OwnedObjects =
            State->get<GCObjectOwnershipMap>(Object)) {
      for (GCObject OwnedObject : *OwnedObjects)
        ObjectWorklist.push_back(OwnedObject);
    }
  }
  return Reachable;
}

bool GCChecker::objectsAreReachable(const ProgramStateRef &State,
                                    GCObjectSet Objects) {
  if (Objects.isEmpty())
    return false;
  GCObjectSet Reachable = computeReachableObjects(State);
  return objectsAreReachable(Objects, Reachable);
}

bool GCChecker::objectsAreReachable(GCObjectSet Objects,
                                    GCObjectSet Reachable) {
  if (Objects.isEmpty())
    return false;
  for (GCObject Object : Objects) {
    if (!Reachable.contains(Object))
      return false;
  }
  return true;
}

bool GCChecker::objectsMayBeFreed(const ProgramStateRef &State,
                                  GCObjectSet Objects,
                                  GCObjectSet Reachable) {
  if (!Objects.isEmpty() && objectsAreReachable(Objects, Reachable))
    return false;
  for (GCObject Object : Objects) {
    const LivenessState *VS = State->get<GCObjectStateMap>(Object);
    if (VS && VS->isPotentiallyFreed())
      return true;
  }
  return false;
}

bool GCChecker::objectsMayBeFreed(const ProgramStateRef &State,
                                  GCObjectSet Objects) {
  GCObjectSet Reachable = computeReachableObjects(State);
  return objectsMayBeFreed(State, Objects, Reachable);
}

GCChecker::LivenessState
GCChecker::aggregateObjectState(const ProgramStateRef &State,
                                GCObjectSet Objects) {
  std::optional<LivenessState> AllocatedState;
  std::optional<LivenessState> UntrackedState;
  for (GCObject Object : Objects) {
    const LivenessState *VS = State->get<GCObjectStateMap>(Object);
    if (!VS)
      continue;
    if (VS->isPotentiallyFreed())
      return *VS;
    if (VS->isJustAllocated())
      AllocatedState = *VS;
    else if (VS->isUntracked())
      UntrackedState = *VS;
  }
  if (AllocatedState)
    return *AllocatedState;
  if (UntrackedState)
    return *UntrackedState;
  return LivenessState::getUntracked();
}

} // namespace jl_gc_checker
