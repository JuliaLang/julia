// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JULIA_CLANGSA_GCCHECKER_H
#define JULIA_CLANGSA_GCCHECKER_H

#include "clang/AST/ParentMapContext.h"
#include "clang/AST/Type.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugType.h"
#include "clang/StaticAnalyzer/Core/Checker.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CallEvent.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramStateTrait.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SVals.h"

#include "llvm/ADT/ImmutableList.h"
#include "llvm/ADT/ImmutableMap.h"
#include "llvm/ADT/ImmutableSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"

#include "HelpersCommon.hpp"

#include <memory>
#include <optional>
#include <utility>

#if defined(__GNUC__)
#define USED_FUNC __attribute__((used))
#else
#define USED_FUNC
#endif

namespace jl_gc_checker {
using namespace clang;
using namespace ento;
using std::make_unique;

typedef std::shared_ptr<PathDiagnosticPiece> PDP;

inline const Stmt *getStmtForDiagnostics(const ExplodedNode *N)
{
    return N->getStmtForDiagnostics();
}

inline unsigned getStackFrameHeight(const LocationContext *stack)
{
    // TODO: or use getID ?
    unsigned depth = 0;
    while (stack) {
        depth++;
        stack = stack->getParent();
    }
    return depth;
}

namespace Helpers {
const VarRegion *walk_back_to_global_VR(const MemRegion *Region);
}

class GCChecker
    : public Checker<
          eval::Call,
          check::BeginFunction,
          check::EndFunction,
          check::PostCall,
          check::PreCall,
          check::PostStmt<CStyleCastExpr>,
          check::PostStmt<ArraySubscriptExpr>,
          check::PostStmt<MemberExpr>,
          check::PostStmt<UnaryOperator>,
          check::Bind,
          check::Location> {
  mutable std::unique_ptr<BugType> BT;
  template <typename callback>
  void report_error(callback f, CheckerContext &C, StringRef message) const;
  void report_error(CheckerContext &C, StringRef message) const {
    return report_error([](PathSensitiveBugReport *) {}, C, message);
  }
  void
  report_value_error(CheckerContext &C, SymbolRef Sym, const char *message,
                     clang::SourceRange range = clang::SourceRange()) const;

public:
  using GCObject = const SymbolConjured *;
  using GCObjectSet = llvm::ImmutableSet<GCObject>;
  using GCRegionSet = llvm::ImmutableSet<const MemRegion *>;
  using GCRootList = llvm::ImmutableList<const MemRegion *>;
  enum class RootValueBinding {
    ConjureUnknownOnly,
    ConjurePossibleOutValue,
  };

  struct LivenessState {
    enum State { Allocated, PotentiallyFreed, Untracked } S;

    // Optional Metadata (for error messages)
    const FunctionDecl *FD;
    const ParmVarDecl *PVD;

    LivenessState(State InS) : S(InS), FD(nullptr), PVD(nullptr) {}
    LivenessState() : S(Untracked), FD(nullptr), PVD(nullptr) {}

    USED_FUNC void dump() const {
      llvm::dbgs() << ((S == Allocated) ? "Allocated"
                     : (S == PotentiallyFreed) ? "PotentiallyFreed"
                     : (S == Untracked) ? "Untracked"
                     : "Error");
      llvm::dbgs() << "\n";
    }

    bool operator==(const LivenessState &VS) const {
      return S == VS.S && FD == VS.FD && PVD == VS.PVD;
    }
    bool operator!=(const LivenessState &VS) const {
      return S != VS.S || FD != VS.FD || PVD != VS.PVD;
    }

    void Profile(llvm::FoldingSetNodeID &ID) const {
      ID.AddInteger(S);
      ID.AddPointer(FD);
      ID.AddPointer(PVD);
    }

    bool isPotentiallyFreed() const { return S == PotentiallyFreed; }
    bool isJustAllocated() const { return S == Allocated; }
    bool isUntracked() const { return S == Untracked; }

    static LivenessState getAllocated() {
      return LivenessState(Allocated);
    }
    static LivenessState getFreed() {
      return LivenessState(PotentiallyFreed);
    }
    static LivenessState getUntracked() {
      return LivenessState(Untracked);
    }
    static LivenessState getForArgument(const FunctionDecl *FD,
                                        const ParmVarDecl *PVD,
                                        bool isFunctionSafepoint);
  };

private:
  template <typename callback>
  static bool isJuliaType(callback f, QualType QT) {
    if (QT->isReferenceType())
      return isJuliaType(f, QT->getPointeeType().getUnqualifiedType());
    if (const auto *AT = QT->getAs<AtomicType>())
      return isJuliaType(f, AT->getValueType().getUnqualifiedType());
    if (QT->isPointerType() || QT->isArrayType())
      return isJuliaType(
          f, clang::QualType(QT->getPointeeOrArrayElementType(), 0));
    const TypedefType *TT = QT->getAs<TypedefType>();
    if (TT) {
      if (f(TT->getDecl()->getName()))
        return true;
    }
    const TagDecl *TD = QT->getUnqualifiedDesugaredType()->getAsTagDecl();
    if (!TD) {
      return false;
    }
    return f(TD->getName());
  }
  template <typename callback>
  static SymbolRef walkToRoot(callback f, const ProgramStateRef &State,
                              const MemRegion *Region);
  static GCObjectSet emptyObjectSet(const ProgramStateRef &State);
  static GCRegionSet emptyRegionSet(const ProgramStateRef &State);
  static GCRootList emptyRootList(const ProgramStateRef &State);
  static bool objectSetContains(GCObjectSet Objects, GCObject Object);
  static bool objectSetHasMultiple(GCObjectSet Objects);
  static GCObjectSet singletonObjectSet(const ProgramStateRef &State,
                                        GCObject Object);
  static bool isAllocaExpr(const Stmt *S);
  bool isAllocaDerivedRegion(const ProgramStateRef &State,
                             const MemRegion *Region,
                             unsigned Depth = 0) const;
  bool isAllocaDerivedSymbol(const ProgramStateRef &State, SymbolRef Sym,
                             unsigned Depth = 0) const;
  bool isAllocaDerivedSVal(const ProgramStateRef &State, SVal V,
                           unsigned Depth = 0) const;
  GCObject ensureObjectForSymbol(SymbolRef Sym, ProgramStateRef &State,
                                 CheckerContext &C,
                                 LivenessState InitialState =
                                     LivenessState::getAllocated(),
                                 QualType ObjectType = QualType()) const;
  static GCObjectSet getObjectsForSymbol(const ProgramStateRef &State,
                                         SymbolRef Sym);
  static std::optional<LivenessState>
  getStateForSymbol(const ProgramStateRef &State, SymbolRef Sym);
  GCObjectSet getDerivedParentObjectsForSymbol(const ProgramStateRef &State,
                                               SymbolRef Sym) const;
  static GCObjectSet getObjectsForSVal(const ProgramStateRef &State, SVal V);
  static GCObjectSet getObjectsForRegion(const ProgramStateRef &State,
                                         const MemRegion *Region);
  static GCObjectSet getObjectsOwningRegion(const ProgramStateRef &State,
                                            const MemRegion *Region);
  static GCObjectSet getTrackedParentObjects(const ProgramStateRef &State,
                                             const MemRegion *Region);
  static GCObjectSet
  getObjectsForRegionOrParents(const ProgramStateRef &State,
                               const MemRegion *Region);
  GCObjectSet getObjectsForExprValue(const Expr *E, SVal V,
                                     ProgramStateRef State,
                                     CheckerContext &C) const;
  ProgramStateRef setObjectsForSymbol(ProgramStateRef State, SymbolRef Sym,
                                      GCObjectSet Objects,
                                      CheckerContext &C) const;
  ProgramStateRef setObjectsForRegion(ProgramStateRef State,
                                       const MemRegion *Region,
                                       GCObjectSet Objects,
                                       CheckerContext &C) const;
  bool regionBindingMayAliasMultipleLocations(const ProgramStateRef &State,
                                              const MemRegion *Region) const;
  static bool isMutableStorageCarrierRegion(const MemRegion *Region);
  ProgramStateRef addObjectRegionEdge(ProgramStateRef State, GCObject Object,
                                      const MemRegion *Region) const;
  ProgramStateRef addObjectRegionEdges(ProgramStateRef State,
                                       GCObjectSet Objects,
                                       const MemRegion *Region) const;
  ProgramStateRef addObjectOwnershipEdge(ProgramStateRef State, GCObject Owner,
                                         GCObjectSet OwnedObjects) const;
  ProgramStateRef addObjectOwnershipEdges(ProgramStateRef State,
                                          GCObjectSet Owners,
                                          GCObjectSet OwnedObjects) const;
  ProgramStateRef bindRegionToObjects(ProgramStateRef State,
                                      const MemRegion *Region,
                                      GCObjectSet Objects,
                                      CheckerContext &C) const;
  ProgramStateRef bindRegionToSVal(ProgramStateRef State,
                                   const MemRegion *Region, SVal V,
                                   CheckerContext &C) const;
  ProgramStateRef bindRootRegionToCurrentValue(ProgramStateRef State,
                                               const MemRegion *Region,
                                               QualType ValueType,
                                               CheckerContext &C,
                                               RootValueBinding Binding =
                                                   RootValueBinding::
                                                       ConjureUnknownOnly) const;
  const MemRegion *getStorageRegionForExpr(const Expr *E,
                                           ProgramStateRef State,
                                           CheckerContext &C) const;
  const MemRegion *getBindingRegionForExprResult(const Expr *E,
                                                 ProgramStateRef State,
                                                 CheckerContext &C) const;
  const MemRegion *getRootingRegionForExpr(const Expr *E,
                                           ProgramStateRef State,
                                           CheckerContext &C) const;
  SVal getOrConjurePointerValue(Loc LValue, QualType Type,
                                ProgramStateRef &State,
                                CheckerContext &C) const;
  GCObjectSet getObjectsForRootPropagatingArgument(const CallEvent &Call,
                                                   unsigned ArgIdx,
                                                   ProgramStateRef &State,
                                                   CheckerContext &C) const;
  GCObjectSet getObjectsForGenericMemoryRef(const Expr *E, SVal V,
                                            QualType Type,
                                            ProgramStateRef &State,
                                            CheckerContext &C) const;
  const MemRegion *getRegionForPointerValue(SVal V, CheckerContext &C) const;
  ProgramStateRef addFrameRoot(ProgramStateRef State, unsigned Depth,
                               const MemRegion *Region,
                               CheckerContext &C) const;
  ProgramStateRef removeFrameRoots(ProgramStateRef State, unsigned Depth) const;
  static bool isRootingRegion(const ProgramStateRef &State,
                              const MemRegion *Region);
  ProgramStateRef addPermanentRoot(ProgramStateRef State,
                                   const MemRegion *Region) const;
  ProgramStateRef promiseRootedSVal(ProgramStateRef State, SVal V,
                                    CheckerContext &C,
                                    QualType ObjectType = QualType()) const;
  const MemRegion *getObjectRegion(GCObject Object, CheckerContext &C) const;
  const MemRegion *getIndexedObjectRegion(GCObject Object, NonLoc Index,
                                          CheckerContext &C) const;
  const MemRegion *getIndexedObjectRegion(GCObject Object, uint64_t Index,
                                          CheckerContext &C) const;
  const MemRegion *getTrackedParentRegion(const MemRegion *Region) const;
  static GCObjectSet computeReachableObjects(const ProgramStateRef &State);
  static bool objectsAreReachable(GCObjectSet Objects, GCObjectSet Reachable);
  static bool objectsAreReachable(const ProgramStateRef &State,
                                  GCObjectSet Objects);
  static bool objectsMayBeFreed(const ProgramStateRef &State,
                                GCObjectSet Objects,
                                GCObjectSet Reachable);
  static bool objectsMayBeFreed(const ProgramStateRef &State,
                                GCObjectSet Objects);
  static LivenessState aggregateObjectState(const ProgramStateRef &State,
                                            GCObjectSet Objects);

  static bool isGCTrackedType(QualType Type);
  static bool isGenericMemoryRefType(QualType Type);
  static const FieldDecl *getGenericMemoryRefMemField(QualType Type);
  static bool isGCObjectType(QualType Type);
  static bool isGCTracked(const Expr *E);
  static QualType getAtomicValueType(QualType Type);
  static const Expr *ignoreOuterPointerCasts(const Expr *E);
  static bool isAssignmentLHS(const Expr *E, CheckerContext &C);
  bool isGloballyRootedType(QualType Type) const;
  static void dumpState(const ProgramStateRef &State);
  static const AnnotateAttr *declHasAnnotation(const clang::Decl *D, const char *which);
  static std::optional<unsigned> declHasIndexedAnnotation(const clang::Decl *D,
                                                          StringRef Prefix);
  static std::optional<std::pair<unsigned, unsigned>>
  declHasIndexedPairAnnotation(const clang::Decl *D, StringRef Prefix);
  static bool isFDAnnotatedNotSafepoint(const clang::FunctionDecl *FD,
                                        const SourceManager &SM);
  static const SourceManager &getSM(CheckerContext &C) { return C.getSourceManager(); }
  bool isSafepoint(const CallEvent &Call, CheckerContext &C) const;
  bool processPotentialSafepoint(const CallEvent &Call, CheckerContext &C,
                                 ProgramStateRef &State) const;
  bool processRootPropagatingRegionResult(const CallEvent &Call,
                                          CheckerContext &C,
                                          ProgramStateRef &State) const;
  bool processAllocationOfResult(const CallEvent &Call, CheckerContext &C,
                                 ProgramStateRef &State) const;
  bool processArgumentRooting(const CallEvent &Call, CheckerContext &C,
                              ProgramStateRef &State) const;
  bool rootRegionIfGlobal(const MemRegion *R, ProgramStateRef &,
                          CheckerContext &C) const;
  bool gcEnabledHere(CheckerContext &C) const;
  bool gcEnabledHere(ProgramStateRef State) const;
  bool safepointEnabledHere(CheckerContext &C) const;
  bool safepointEnabledHere(ProgramStateRef State) const;
  bool propagateArgumentRootedness(CheckerContext &C,
                                   ProgramStateRef &State) const;
  SymbolRef getSymbolForResult(const Expr *Result, bool ShouldConjure,
                               ProgramStateRef &State, CheckerContext &C) const;

public:
  void checkBeginFunction(CheckerContext &Ctx) const;
  void checkEndFunction(const clang::ReturnStmt *RS, CheckerContext &Ctx) const;
  bool evalCall(const CallEvent &Call, CheckerContext &C) const;
  void checkPreCall(const CallEvent &Call, CheckerContext &C) const;
  void checkPostCall(const CallEvent &Call, CheckerContext &C) const;
  void checkPostStmt(const CStyleCastExpr *CE, CheckerContext &C) const;
  void checkPostStmt(const ArraySubscriptExpr *CE, CheckerContext &C) const;
  void checkPostStmt(const MemberExpr *ME, CheckerContext &C) const;
  void checkPostStmt(const UnaryOperator *UO, CheckerContext &C) const;
  void checkDerivingExpr(const Expr *Result, const Expr *Parent,
                         CheckerContext &C) const;
  void checkBind(SVal Loc, SVal Val, const Stmt *S, CheckerContext &) const;
  void checkLocation(SVal Loc, bool IsLoad, const Stmt *S,
                     CheckerContext &) const;

  class GCBugVisitor : public BugReporterVisitor {
  public:
    GCBugVisitor() {}

    void Profile(llvm::FoldingSetNodeID &ID) const override {
      static int X = 0;
      ID.AddPointer(&X);
    }

    PDP VisitNode(const ExplodedNode *N, BugReporterContext &BRC,
                  PathSensitiveBugReport &BR) override;
  };

  class SafepointBugVisitor : public BugReporterVisitor {
  public:
    SafepointBugVisitor() {}

    void Profile(llvm::FoldingSetNodeID &ID) const override {
      static int X = 0;
      ID.AddPointer(&X);
    }

    PDP VisitNode(const ExplodedNode *N, BugReporterContext &BRC,
                  PathSensitiveBugReport &BR) override;
  };

  class GCValueBugVisitor : public BugReporterVisitor {
  protected:
    SymbolRef Sym;

  public:
    GCValueBugVisitor(SymbolRef S) : Sym(S) {}

    void Profile(llvm::FoldingSetNodeID &ID) const override {
      static int X = 0;
      ID.AddPointer(&X);
      ID.AddPointer(Sym);
    }

    PDP ExplainNoPropagation(const ExplodedNode *N, PathDiagnosticLocation Pos,
                             BugReporterContext &BRC,
                             PathSensitiveBugReport &BR);
    PDP ExplainNoPropagationFromExpr(const clang::Expr *FromWhere,
                                     const ExplodedNode *N,
                                     PathDiagnosticLocation Pos,
                                     BugReporterContext &BRC,
                                     PathSensitiveBugReport &BR);

    PDP VisitNode(const ExplodedNode *N, BugReporterContext &BRC,
                  PathSensitiveBugReport &BR) override;
  };
};

// These ProgramState traits are declared by hand because Clang's
// REGISTER_*_WITH_PROGRAMSTATE macros place trait keys in an anonymous
// namespace and are not usable across multiple translation units.

// Current explicit JL_GC_PUSH nesting depth in the active stack frame.
class GCDepth {};
using GCDepthTy = unsigned;
// Stack-frame height where GC was disabled, or -1 when GC is enabled.
class GCDisabledAt {};
using GCDisabledAtTy = unsigned;
// Stack-frame height where safepoints were disabled, or -1 when enabled.
class SafepointDisabledAt {};
using SafepointDisabledAtTy = unsigned;
// Tracks whether the current call path may execute a safepoint.
class MayCallSafepoint {};
using MayCallSafepointTy = bool;
// Explicit stack root slots and the GCDepth at which each slot was pushed.
class GCRootMap {};
using GCRootMapTy = llvm::ImmutableMap<const MemRegion *, int>;
// Liveness of each modeled Julia object. Julia objects are SymbolConjureds.
class GCObjectStateMap {};
using GCObjectStateMapTy =
    llvm::ImmutableMap<GCChecker::GCObject, GCChecker::LivenessState>;
// Region-to-object bindings. A region may conservatively root multiple objects.
class GCRegionObjectMap {};
using GCRegionObjectMapTy =
    llvm::ImmutableMap<const MemRegion *, GCChecker::GCObjectSet>;
// Structural regions that are semantically part of a Julia object.
class GCObjectRegionMap {};
using GCObjectRegionMapTy =
    llvm::ImmutableMap<GCChecker::GCObject, GCChecker::GCRegionSet>;
// Non-structural carrier regions used to recover parent objects for derived
// values such as by-value jl_genericmemoryref_t temporaries. These are not root
// edges: concrete field/element regions reached through a carrier, such as
// jl_svec_data(parent)[0], remain structural GCObjectRegionMap entries.
class GCDerivedRegionOwnerMap {};
using GCDerivedRegionOwnerMapTy =
    llvm::ImmutableMap<const MemRegion *, GCChecker::GCObjectSet>;
// Unknown object-to-object ownership edges, such as propagation through a
// result where the specific owned field is not modeled.
class GCObjectOwnershipMap {};
using GCObjectOwnershipMapTy =
    llvm::ImmutableMap<GCChecker::GCObject, GCChecker::GCObjectSet>;
// Rooting regions introduced at each stack frame height.
class GCRootFrameMap {};
using GCRootFrameMapTy = llvm::ImmutableMap<unsigned, GCChecker::GCRootList>;
// Regions that root their bindings independently of JL_GC_PUSH/POP depth.
class GCPermanentRootRegions {};
using GCPermanentRootRegionsTy = llvm::ImmutableSet<const MemRegion *>;
// Root regions whose updates must keep old bindings as an over-approximation.
class GCConservativeRootRegions {};
using GCConservativeRootRegionsTy = llvm::ImmutableSet<const MemRegion *>;

} // namespace jl_gc_checker

namespace clang {
namespace ento {

#define JL_GC_DECLARE_PROGRAMSTATE_TRAIT(Name)                                 \
  template <>                                                                  \
  struct ProgramStateTrait<jl_gc_checker::Name>                                \
      : public ProgramStatePartialTrait<jl_gc_checker::Name##Ty> {             \
    static void *GDMIndex();                                                   \
  };

JL_GC_DECLARE_PROGRAMSTATE_TRAIT(GCDepth)
JL_GC_DECLARE_PROGRAMSTATE_TRAIT(GCDisabledAt)
JL_GC_DECLARE_PROGRAMSTATE_TRAIT(SafepointDisabledAt)
JL_GC_DECLARE_PROGRAMSTATE_TRAIT(MayCallSafepoint)
JL_GC_DECLARE_PROGRAMSTATE_TRAIT(GCRootMap)
JL_GC_DECLARE_PROGRAMSTATE_TRAIT(GCObjectStateMap)
JL_GC_DECLARE_PROGRAMSTATE_TRAIT(GCRegionObjectMap)
JL_GC_DECLARE_PROGRAMSTATE_TRAIT(GCObjectRegionMap)
JL_GC_DECLARE_PROGRAMSTATE_TRAIT(GCDerivedRegionOwnerMap)
JL_GC_DECLARE_PROGRAMSTATE_TRAIT(GCObjectOwnershipMap)
JL_GC_DECLARE_PROGRAMSTATE_TRAIT(GCRootFrameMap)
JL_GC_DECLARE_PROGRAMSTATE_TRAIT(GCPermanentRootRegions)
JL_GC_DECLARE_PROGRAMSTATE_TRAIT(GCConservativeRootRegions)

#undef JL_GC_DECLARE_PROGRAMSTATE_TRAIT

#define JL_GC_DECLARE_FACTORY_TRAIT(Type)                                      \
  template <>                                                                  \
  struct ProgramStateTrait<Type> : public ProgramStatePartialTrait<Type> {      \
    static void *GDMIndex();                                                   \
  };

JL_GC_DECLARE_FACTORY_TRAIT(jl_gc_checker::GCChecker::GCObjectSet)
JL_GC_DECLARE_FACTORY_TRAIT(jl_gc_checker::GCChecker::GCRegionSet)
JL_GC_DECLARE_FACTORY_TRAIT(jl_gc_checker::GCChecker::GCRootList)

#undef JL_GC_DECLARE_FACTORY_TRAIT

} // namespace ento
} // namespace clang

namespace jl_gc_checker {

template <typename callback>
void GCChecker::report_error(callback f, CheckerContext &C,
                             StringRef message) const {
  // Generate an error node.
  ExplodedNode *N = C.generateErrorNode();
  if (!N)
    return;

  if (!BT)
    BT.reset(new BugType(this, "Invalid GC thingy", categories::LogicError));
  auto Report = make_unique<PathSensitiveBugReport>(*BT, message, N);
  Report->addVisitor(make_unique<GCBugVisitor>());
  f(Report.get());
  C.emitReport(std::move(Report));
}

template <typename callback>
SymbolRef GCChecker::walkToRoot(callback f, const ProgramStateRef &State,
                                const MemRegion *Region) {
  if (!Region)
    return nullptr;
  while (true) {
    const SymbolicRegion *SR = Region->getSymbolicBase();
    if (!SR) {
      return nullptr;
    }
    SymbolRef Sym = SR->getSymbol();
    std::optional<LivenessState> OldVState = getStateForSymbol(State, Sym);
    if (f(Sym, OldVState ? &*OldVState : nullptr)) {
      if (const SymbolRegionValue *SRV = dyn_cast<SymbolRegionValue>(Sym)) {
        Region = SRV->getRegion();
        continue;
      } else if (const SymbolDerived *SD = dyn_cast<SymbolDerived>(Sym)) {
        Region = SD->getRegion();
        continue;
      }
      return nullptr;
    }
    return Sym;
  }
}

} // namespace jl_gc_checker

#endif
