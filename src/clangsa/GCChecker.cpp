// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "clang/AST/Type.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/StaticAnalyzer/Checkers/SValExplainer.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugType.h"
#include "clang/StaticAnalyzer/Core/BugReporter/CommonBugCategories.h"
#include "clang/StaticAnalyzer/Core/Checker.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CallEvent.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SVals.h"
#include "clang/StaticAnalyzer/Frontend/AnalysisConsumer.h"
#include "clang/StaticAnalyzer/Frontend/FrontendActions.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"
#include "clang/StaticAnalyzer/Frontend/CheckerRegistry.h"
#include "clang/Lex/Lexer.h"

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/Debug.h"
#include <iostream>
#include <memory>
#include <optional>

#if defined(__GNUC__)
#define USED_FUNC __attribute__((used))
#else
#define USED_FUNC
#endif

using std::make_unique;

namespace {
using namespace clang;
using namespace ento;

typedef std::shared_ptr<PathDiagnosticPiece> PDP;
#define MakePDP make_unique<PathDiagnosticEventPiece>

static const Stmt *getStmtForDiagnostics(const ExplodedNode *N)
{
    return N->getStmtForDiagnostics();
}

static unsigned getStackFrameHeight(const LocationContext *stack)
{
    // TODO: or use getID ?
    unsigned depth = 0;
    while (stack) {
        depth++;
        stack = stack->getParent();
    }
    return depth;
}

static bool isImmediateMacroExpansionNamed(CheckerContext &C,
                                           SourceLocation Loc,
                                           StringRef Name) {
  if (!Loc.isMacroID())
    return false;
  return Lexer::getImmediateMacroName(Loc, C.getSourceManager(),
                                      C.getASTContext().getLangOpts()) == Name;
}

static const Expr *getPointerArithmeticBase(const Expr *E) {
  E = E->IgnoreParens();
  const auto *BO = dyn_cast<BinaryOperator>(E);
  if (!BO || !BO->isAdditiveOp())
    return nullptr;
  const Expr *LHS = BO->getLHS()->IgnoreParenCasts();
  if (LHS->getType()->isPointerType())
    return LHS;
  const Expr *RHS = BO->getRHS()->IgnoreParenCasts();
  if (RHS->getType()->isPointerType())
    return RHS;
  return nullptr;
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
  struct ValueState {
    enum State { Allocated, PotentiallyFreed, Untracked } S;

    // Optional Metadata (for error messages)
    const FunctionDecl *FD;
    const ParmVarDecl *PVD;

    ValueState(State InS) : S(InS), FD(nullptr), PVD(nullptr) {}
    ValueState() : S(Untracked), FD(nullptr), PVD(nullptr) {}

    USED_FUNC void dump() const {
      llvm::dbgs() << ((S == Allocated) ? "Allocated"
                     : (S == PotentiallyFreed) ? "PotentiallyFreed"
                     : (S == Untracked) ? "Untracked"
                     : "Error");
      llvm::dbgs() << "\n";
    }

    bool operator==(const ValueState &VS) const {
      return S == VS.S;
    }
    bool operator!=(const ValueState &VS) const {
      return S != VS.S;
    }

    void Profile(llvm::FoldingSetNodeID &ID) const {
      ID.AddInteger(S);
    }

    bool isPotentiallyFreed() const { return S == PotentiallyFreed; }
    bool isJustAllocated() const { return S == Allocated; }
    bool isUntracked() const { return S == Untracked; }

    static ValueState getAllocated() {
      return ValueState(Allocated);
    }
    static ValueState getFreed() {
      return ValueState(PotentiallyFreed);
    }
    static ValueState getUntracked() {
      return ValueState(Untracked);
    }
    static ValueState getForArgument(const FunctionDecl *FD,
                                     const ParmVarDecl *PVD,
                                     bool isFunctionSafepoint) {
      bool maybeUnrooted = declHasAnnotation(PVD, "julia_maybe_unrooted");
      ValueState VS = getAllocated();
      if (!isFunctionSafepoint || maybeUnrooted) {
        VS.PVD = PVD;
        VS.FD = FD;
      }
      return VS;
    }
  };

  struct RootLifetime {
    enum Kind { Permanent, GCFrame } K;
    unsigned Depth;

    RootLifetime(Kind InK, unsigned InDepth = 0) : K(InK), Depth(InDepth) {}

    bool operator==(const RootLifetime &Other) const {
      return K == Other.K && Depth == Other.Depth;
    }
    bool operator!=(const RootLifetime &Other) const {
      return K != Other.K || Depth != Other.Depth;
    }

    bool shouldPopAtDepth(unsigned CurrentDepth) const {
      return K == GCFrame && CurrentDepth == Depth;
    }
    bool outlives(const RootLifetime &Other) const {
      if (K != Other.K)
        return K == Permanent;
      return K == GCFrame && Depth < Other.Depth;
    }
    int diagnosticDepth() const {
      return K == Permanent ? -1 : static_cast<int>(Depth);
    }

    void Profile(llvm::FoldingSetNodeID &ID) const {
      ID.AddInteger(K);
      ID.AddInteger(Depth);
    }

    static RootLifetime getPermanent() {
      return RootLifetime(Permanent);
    }
    static RootLifetime getGCFrame(unsigned Depth) {
      return RootLifetime(GCFrame, Depth);
    }
  };

  struct RootState {
    enum Kind { Root, RootArray } K;
    RootLifetime Lifetime;

    RootState(Kind InK, RootLifetime InLifetime)
        : K(InK), Lifetime(InLifetime) {}

    bool operator==(const RootState &VS) const {
      return K == VS.K && Lifetime == VS.Lifetime;
    }
    bool operator!=(const RootState &VS) const {
      return K != VS.K || Lifetime != VS.Lifetime;
    }

    bool shouldPopAtDepth(unsigned Depth) const {
      return Lifetime.shouldPopAtDepth(Depth);
    }
    bool isRootArray() const { return K == RootArray; }

    void Profile(llvm::FoldingSetNodeID &ID) const {
      ID.AddInteger(K);
      Lifetime.Profile(ID);
    }

    static RootState getRoot(unsigned Depth) {
      return RootState(Root, RootLifetime::getGCFrame(Depth));
    }
    static RootState getPermanentRoot() {
      return RootState(Root, RootLifetime::getPermanent());
    }
    static RootState getPermanentRootArray() {
      return RootState(RootArray, RootLifetime::getPermanent());
    }
    static RootState getRootArray(unsigned Depth) {
      return RootState(RootArray, RootLifetime::getGCFrame(Depth));
    }
  };

  struct GCCell {
    enum Kind { RegionCell, HeapField, HeapArrayElement } K;

    const MemRegion *Region;
    SymbolRef Owner;
    const FieldDecl *Field;
    int64_t Index;
    bool Summary;

    GCCell()
        : K(RegionCell), Region(nullptr), Owner(nullptr), Field(nullptr),
          Index(0), Summary(false) {}

    bool operator==(const GCCell &Other) const {
      return K == Other.K && Region == Other.Region && Owner == Other.Owner &&
             Field == Other.Field && Index == Other.Index &&
             Summary == Other.Summary;
    }
    bool operator!=(const GCCell &Other) const { return !(*this == Other); }
    bool operator<(const GCCell &Other) const {
      std::less<const void *> Less;
      if (K != Other.K)
        return K < Other.K;
      if (Region != Other.Region)
        return Less(Region, Other.Region);
      if (Owner != Other.Owner)
        return Less(Owner, Other.Owner);
      if (Field != Other.Field)
        return Less(Field, Other.Field);
      if (Index != Other.Index)
        return Index < Other.Index;
      return Summary < Other.Summary;
    }

    bool isHeapCell() const { return K == HeapField || K == HeapArrayElement; }
    bool isRegionCell() const { return K == RegionCell; }
    bool isSummaryCell() const { return K == HeapArrayElement && Summary; }

    void Profile(llvm::FoldingSetNodeID &ID) const {
      ID.AddInteger(K);
      ID.AddPointer(Region);
      ID.AddPointer(Owner);
      ID.AddPointer(Field);
      ID.AddInteger(Index);
      ID.AddBoolean(Summary);
    }

    static GCCell forRegion(const MemRegion *Region) {
      GCCell Cell;
      Cell.K = RegionCell;
      Cell.Region = Region;
      return Cell;
    }
    static GCCell forHeapField(SymbolRef Owner, const FieldDecl *Field) {
      GCCell Cell;
      Cell.K = HeapField;
      Cell.Owner = Owner;
      Cell.Field = Field;
      return Cell;
    }
    static GCCell forHeapArrayElement(SymbolRef Owner, int64_t Index) {
      GCCell Cell;
      Cell.K = HeapArrayElement;
      Cell.Owner = Owner;
      Cell.Index = Index;
      Cell.Summary = false;
      return Cell;
    }
    static GCCell forHeapArraySummary(SymbolRef Owner) {
      GCCell Cell;
      Cell.K = HeapArrayElement;
      Cell.Owner = Owner;
      Cell.Summary = true;
      return Cell;
    }
  };

private:
  struct RootLookupResult {
    enum Provenance { ExactRoot, RootArrayElement, AliasToRootSlot };

    const RootState *RS;
    const MemRegion *RootRegion;
    const MemRegion *SlotRegion;
    Provenance P;

    RootLookupResult()
        : RS(nullptr), RootRegion(nullptr), SlotRegion(nullptr), P(ExactRoot) {}
    RootLookupResult(const RootState *InRS, const MemRegion *InRootRegion,
                     const MemRegion *InSlotRegion, Provenance InP)
        : RS(InRS), RootRegion(InRootRegion), SlotRegion(InSlotRegion),
          P(InP) {}
  };

  template <typename callback>
  static bool isJuliaType(callback f, QualType QT) {
    if (QT->isReferenceType())
      return isJuliaType(f, QT->getPointeeType().getUnqualifiedType());
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

  static bool isGCTrackedType(QualType Type);
  static bool isGCTracked(const Expr *E);
  bool isGloballyRootedType(QualType Type) const;
  static void dumpState(const ProgramStateRef &State);
  static const AnnotateAttr *declHasAnnotation(const clang::Decl *D, const char *which);
  static bool isFDAnnotatedNotSafepoint(const clang::FunctionDecl *FD, const SourceManager &SM);
  static const SourceManager &getSM(CheckerContext &C) { return C.getSourceManager(); }
  bool isSafepoint(const CallEvent &Call, CheckerContext &C) const;
  bool processPotentialSafepoint(const CallEvent &Call, CheckerContext &C,
                                 ProgramStateRef &State) const;
  bool processAllocationOfResult(const CallEvent &Call, CheckerContext &C,
                                 ProgramStateRef &State) const;
  bool processArgumentRooting(const CallEvent &Call, CheckerContext &C,
                              ProgramStateRef &State) const;
  bool processHeapEffects(const CallEvent &Call, CheckerContext &C,
                          ProgramStateRef &State) const;
  bool rootRegionIfGlobal(const MemRegion *R, ProgramStateRef &,
                          CheckerContext &C, ValueState *ValS = nullptr,
                          bool *IsRooted = nullptr) const;
  static const MemRegion *getOriginRegion(SymbolRef Sym);
  static SymbolRef getTrackedSymbolForSVal(const ProgramStateRef &State,
                                           SVal Val);
  static bool lookupRootRegion(const ProgramStateRef &State,
                               const MemRegion *R,
                               RootLookupResult *Result);
  static std::optional<GCCell> resolveCell(const ProgramStateRef &State,
                                           const MemRegion *R);
  static std::optional<GCCell> resolveCell(const ProgramStateRef &State,
                                           SVal V);
  static bool lookupRootCell(const ProgramStateRef &State, const GCCell &Cell,
                             RootLookupResult *Result = nullptr);
  static ProgramStateRef setCellContent(ProgramStateRef State,
                                        const GCCell &Cell, SymbolRef Sym);
  static ProgramStateRef addObjectChild(ProgramStateRef State, SymbolRef Owner,
                                        const GCCell &Child);
  static ProgramStateRef addObjectSymbolChild(ProgramStateRef State,
                                              SymbolRef Owner,
                                              SymbolRef Child);
  static ProgramStateRef setObjectChildContent(ProgramStateRef State,
                                               SymbolRef Owner,
                                               const GCCell &Child,
                                               SymbolRef ChildSym);
  static bool getConcreteIndex(SVal V, int64_t &Index);
  static bool traceReachableSymbols(const ProgramStateRef &State,
                                    llvm::SmallPtrSetImpl<SymbolRef> &Reachable,
                                    SymbolRef Target = nullptr,
                                    const MemRegion **RootRegion = nullptr,
                                    int *RootDepth = nullptr);
  static ProgramStateRef runGCSafepoint(ProgramStateRef State,
                                        SymbolRef SpeciallyRootedSymbol,
                                        SymbolRef RetSym,
                                        bool &DidChange);
  static bool getBestRoot(const ProgramStateRef &State, SymbolRef Sym,
                          const MemRegion **RootRegion = nullptr,
                          int *RootDepth = nullptr);
  static bool isSymbolRooted(const ProgramStateRef &State, SymbolRef Sym);
  static bool isSymbolOrOriginRooted(const ProgramStateRef &State,
                                     SymbolRef Sym);
  static ProgramStateRef markSymbolRooted(ProgramStateRef State, SymbolRef Sym,
                                          const MemRegion *SlotRegion);
  static ProgramStateRef addSymbolRoots(ProgramStateRef State, SymbolRef To,
                                        SymbolRef From);
  static ProgramStateRef copySymbolRoots(ProgramStateRef State, SymbolRef To,
                                         SymbolRef From);
  static ProgramStateRef removeRootSlotFromValues(ProgramStateRef State,
                                                  const MemRegion *SlotRegion);
  static ProgramStateRef removeRootSlotsForRoot(ProgramStateRef State,
                                                const MemRegion *RootRegion);
  static ProgramStateRef setRootSlotContent(ProgramStateRef State,
                                            const MemRegion *SlotRegion,
                                            SymbolRef Sym);
  static const ValueState *getValStateForRegion(ASTContext &AstC,
                                                const ProgramStateRef &State,
                                                const MemRegion *R,
                                                bool Debug = false);
  static SymbolRef getRootedSymbolForRegion(const ProgramStateRef &State,
                                            const MemRegion *R);
  bool gcEnabledHere(CheckerContext &C) const;
  bool gcEnabledHere(ProgramStateRef State) const;
  bool safepointEnabledHere(CheckerContext &C) const;
  bool safepointEnabledHere(ProgramStateRef State) const;
  bool propagateArgumentRootedness(CheckerContext &C,
                                   ProgramStateRef &State) const;
  SymbolRef getSymbolForResult(const Expr *Result, const ValueState *OldValS,
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
                         bool ParentIsLoc, CheckerContext &C) const;
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

    PDP VisitNode(const ExplodedNode *N, BugReporterContext &BRC, PathSensitiveBugReport &BR) override;
  };

  class SafepointBugVisitor : public BugReporterVisitor {
  public:
    SafepointBugVisitor() {}

    void Profile(llvm::FoldingSetNodeID &ID) const override {
      static int X = 0;
      ID.AddPointer(&X);
    }

    PDP VisitNode(const ExplodedNode *N, BugReporterContext &BRC, PathSensitiveBugReport &BR) override;
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
                             BugReporterContext &BRC, PathSensitiveBugReport &BR);
    PDP ExplainNoPropagationFromExpr(const clang::Expr *FromWhere,
                                     const ExplodedNode *N,
                                     PathDiagnosticLocation Pos,
                                     BugReporterContext &BRC, PathSensitiveBugReport &BR);

    PDP VisitNode(const ExplodedNode *N, BugReporterContext &BRC, PathSensitiveBugReport &BR) override;
  }; // namespace
};

} // namespace

REGISTER_TRAIT_WITH_PROGRAMSTATE(GCDepth, unsigned)
REGISTER_TRAIT_WITH_PROGRAMSTATE(GCDisabledAt, unsigned)
REGISTER_TRAIT_WITH_PROGRAMSTATE(SafepointDisabledAt, unsigned)
REGISTER_TRAIT_WITH_PROGRAMSTATE(MayCallSafepoint, bool)
REGISTER_MAP_WITH_PROGRAMSTATE(GCValueMap, SymbolRef, GCChecker::ValueState)
REGISTER_MAP_WITH_PROGRAMSTATE(GCRootMap, const MemRegion *,
                               GCChecker::RootState)
REGISTER_SET_WITH_PROGRAMSTATE(GCValuePermanentRoots, SymbolRef)
REGISTER_SET_FACTORY_WITH_PROGRAMSTATE(GCValueRootSet, const MemRegion *)
REGISTER_MAP_WITH_PROGRAMSTATE(GCValueRootMap, SymbolRef, GCValueRootSet)
// If GCRootContentMap[Slot] is Sym, GCValueRootMap[Sym] must contain Slot.
// Root slot writes should use setRootSlotContent; direct updates must maintain
// both maps.
REGISTER_MAP_WITH_PROGRAMSTATE(GCRootContentMap, const MemRegion *, SymbolRef)
REGISTER_MAP_WITH_PROGRAMSTATE(GCRootRegistry, GCChecker::GCCell,
                               GCChecker::RootState)
REGISTER_MAP_WITH_PROGRAMSTATE(GCCellContentMap, GCChecker::GCCell, SymbolRef)
REGISTER_SET_FACTORY_WITH_PROGRAMSTATE(GCCellSet, GCChecker::GCCell)
REGISTER_MAP_WITH_PROGRAMSTATE(GCObjectChildrenMap, SymbolRef, GCCellSet)
REGISTER_SET_FACTORY_WITH_PROGRAMSTATE(GCSymbolSet, SymbolRef)
REGISTER_MAP_WITH_PROGRAMSTATE(GCObjectSymbolChildrenMap, SymbolRef,
                               GCSymbolSet)

const MemRegion *GCChecker::getOriginRegion(SymbolRef Sym) {
  if (const SymbolCast *SC = dyn_cast<SymbolCast>(Sym))
    return getOriginRegion(SC->getOperand());
  if (const SymbolRegionValue *SRV = dyn_cast<SymbolRegionValue>(Sym))
    return SRV->getRegion();
  if (const SymbolDerived *SD = dyn_cast<SymbolDerived>(Sym))
    return SD->getRegion();
  return nullptr;
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
    const ValueState *OldVState = State->get<GCValueMap>(Sym);
    if (f(Sym, OldVState)) {
      if (const MemRegion *Origin = getOriginRegion(Sym)) {
        Region = Origin;
        continue;
      }
      return nullptr;
    }
    return Sym;
  }
}

SymbolRef GCChecker::getTrackedSymbolForSVal(const ProgramStateRef &State,
                                             SVal Val) {
  if (SymbolRef Sym = Val.getAsSymbol()) {
    if (State->get<GCValueMap>(Sym) && isSymbolOrOriginRooted(State, Sym))
      return Sym;
  }
  if (SymbolRef Sym = getRootedSymbolForRegion(State, Val.getAsRegion()))
    return Sym;
  if (SymbolRef Sym = Val.getAsSymbol()) {
    if (State->get<GCValueMap>(Sym))
      return Sym;
  }
  return walkToRoot(
      [](SymbolRef Sym, const ValueState *OldVState) { return !OldVState; },
      State, Val.getAsRegion());
}

bool GCChecker::lookupRootRegion(const ProgramStateRef &State,
                                 const MemRegion *Region,
                                 RootLookupResult *Result) {
  if (Result)
    *Result = RootLookupResult();
  bool SawAlias = false;
  for (unsigned I = 0; I < 16 && Region; ++I) {
    Region = Region->StripCasts();
    if (const RootState *RS = State->get<GCRootMap>(Region)) {
      if (Result)
        *Result = RootLookupResult(
            RS, Region, RS->isRootArray() ? nullptr : Region,
            SawAlias ? RootLookupResult::AliasToRootSlot
                     : RootLookupResult::ExactRoot);
      return true;
    }

    if (const FieldRegion *FR = Region->getAs<FieldRegion>()) {
      const MemRegion *Super = FR->getSuperRegion()->StripCasts();
      if (const auto *SR = Super->getAs<SymbolicRegion>()) {
        if (const MemRegion *Origin = getOriginRegion(SR->getSymbol())) {
          if (const auto *OriginSuper =
                  dyn_cast<SubRegion>(Origin->StripCasts())) {
            Region =
                Region->getMemRegionManager().getFieldRegionWithSuper(FR,
                                                                       OriginSuper);
            SawAlias = true;
            continue;
          }
        }
      }
    }

    if (const ElementRegion *ER = Region->getAs<ElementRegion>()) {
      const MemRegion *Base = ER->getBaseRegion()->StripCasts();
      if (const RootState *RS = State->get<GCRootMap>(Base)) {
        if (RS->isRootArray()) {
          if (Result)
            *Result = RootLookupResult(
                RS, Base, Region,
                SawAlias ? RootLookupResult::AliasToRootSlot
                         : RootLookupResult::RootArrayElement);
          return true;
        }
      }
      if (const auto *SR = Base->getAs<SymbolicRegion>()) {
        if (const MemRegion *Origin = getOriginRegion(SR->getSymbol())) {
          if (const auto *OriginBase =
                  dyn_cast<SubRegion>(Origin->StripCasts())) {
            Region =
                Region->getMemRegionManager().getElementRegionWithSuper(ER,
                                                                         OriginBase);
            SawAlias = true;
            continue;
          }
        }
      }
      const auto Index = ER->getIndex().getAs<nonloc::ConcreteInt>();
      if (!Index || !Index->getValue()->isZero())
        break;
      Region = ER->getSuperRegion();
      continue;
    }

    if (const SymbolicRegion *SR = Region->getAs<SymbolicRegion>()) {
      if (const MemRegion *Origin = getOriginRegion(SR->getSymbol())) {
        Region = Origin;
        SawAlias = true;
        continue;
      }
    }

    break;
  }
  return false;
}

bool GCChecker::getConcreteIndex(SVal V, int64_t &Index) {
  auto CI = V.getAs<nonloc::ConcreteInt>();
  if (!CI)
    return false;
  const llvm::APSInt &Value = CI->getValue();
  if (!Value.isSigned() && Value.getActiveBits() > 63)
    return false;
  Index = Value.getSExtValue();
  return true;
}

std::optional<GCChecker::GCCell>
GCChecker::resolveCell(const ProgramStateRef &State, SVal V) {
  return resolveCell(State, V.getAsRegion());
}

std::optional<GCChecker::GCCell>
GCChecker::resolveCell(const ProgramStateRef &State, const MemRegion *Region) {
  if (!Region)
    return std::nullopt;

  for (unsigned I = 0; I < 16 && Region; ++I) {
    Region = Region->StripCasts();

    RootLookupResult Lookup;
    if (lookupRootRegion(State, Region, &Lookup) && Lookup.SlotRegion)
      return GCCell::forRegion(Lookup.SlotRegion);

    if (const SymbolicRegion *SR = Region->getAs<SymbolicRegion>()) {
      if (const MemRegion *Origin = getOriginRegion(SR->getSymbol())) {
        Region = Origin;
        continue;
      }
    }

    if (const FieldRegion *FR = Region->getAs<FieldRegion>()) {
      const MemRegion *Super = FR->getSuperRegion()->StripCasts();
      SymbolRef Owner = walkToRoot(
          [](SymbolRef Sym, const ValueState *OldVState) {
            return !OldVState;
          },
          State, Super);
      if (Owner)
        return GCCell::forHeapField(Owner, FR->getDecl());
      return GCCell::forRegion(Region);
    }

    if (const ElementRegion *ER = Region->getAs<ElementRegion>()) {
      const MemRegion *Base = ER->getBaseRegion()->StripCasts();
      RootLookupResult BaseLookup;
      if (lookupRootRegion(State, Base, &BaseLookup))
        return GCCell::forRegion(Region);

      SymbolRef Owner = walkToRoot(
          [](SymbolRef Sym, const ValueState *OldVState) {
            return !OldVState;
          },
          State, Base);
      if (Owner) {
        int64_t Index = 0;
        if (getConcreteIndex(ER->getIndex(), Index))
          return GCCell::forHeapArrayElement(Owner, Index);
        return GCCell::forHeapArraySummary(Owner);
      }

      // Do not collapse non-root array element zero to its super-region here:
      // cell identity should describe the actual location being assigned.
      return GCCell::forRegion(Region);
    }

    return GCCell::forRegion(Region);
  }

  return std::nullopt;
}

bool GCChecker::lookupRootCell(const ProgramStateRef &State,
                               const GCCell &Cell,
                               RootLookupResult *Result) {
  if (Result)
    *Result = RootLookupResult();
  if (!Cell.isRegionCell())
    return false;

  if (const RootState *RS = State->get<GCRootRegistry>(Cell)) {
    if (Result)
      *Result =
          RootLookupResult(RS, Cell.Region, RS->isRootArray() ? nullptr
                                                              : Cell.Region,
                           RootLookupResult::ExactRoot);
    return true;
  }

  return lookupRootRegion(State, Cell.Region, Result);
}

ProgramStateRef GCChecker::setCellContent(ProgramStateRef State,
                                          const GCCell &Cell, SymbolRef Sym) {
  if (!Sym)
    return State->remove<GCCellContentMap>(Cell);
  return State->set<GCCellContentMap>(Cell, Sym);
}

ProgramStateRef GCChecker::addObjectChild(ProgramStateRef State,
                                          SymbolRef Owner,
                                          const GCCell &Child) {
  if (!Owner || !Child.isHeapCell())
    return State;

  auto &Factory = State->get_context<GCCellSet>();
  const GCCellSet *CurrentChildren = State->get<GCObjectChildrenMap>(Owner);
  GCCellSet Children =
      CurrentChildren ? *CurrentChildren : Factory.getEmptySet();
  Children = Factory.add(Children, Child);
  return State->set<GCObjectChildrenMap>(Owner, Children);
}

ProgramStateRef GCChecker::addObjectSymbolChild(ProgramStateRef State,
                                                SymbolRef Owner,
                                                SymbolRef Child) {
  if (!Owner || !Child)
    return State;

  auto &Factory = State->get_context<GCSymbolSet>();
  const GCSymbolSet *CurrentChildren =
      State->get<GCObjectSymbolChildrenMap>(Owner);
  GCSymbolSet Children =
      CurrentChildren ? *CurrentChildren : Factory.getEmptySet();
  Children = Factory.add(Children, Child);
  return State->set<GCObjectSymbolChildrenMap>(Owner, Children);
}

ProgramStateRef GCChecker::setObjectChildContent(ProgramStateRef State,
                                                 SymbolRef Owner,
                                                 const GCCell &Child,
                                                 SymbolRef ChildSym) {
  State = addObjectChild(State, Owner, Child);
  return setCellContent(State, Child, ChildSym);
}

bool GCChecker::traceReachableSymbols(
    const ProgramStateRef &State, llvm::SmallPtrSetImpl<SymbolRef> &Reachable,
    SymbolRef Target, const MemRegion **RootRegion, int *RootDepth) {
  if (RootRegion)
    *RootRegion = nullptr;
  if (RootDepth)
    *RootDepth = 0;

  SmallVector<GCCell, 32> CellWorklist;
  SmallVector<SymbolRef, 32> ObjectWorklist;

  for (SymbolRef Sym : State->get<GCValuePermanentRoots>()) {
    if (Reachable.insert(Sym).second)
      ObjectWorklist.push_back(Sym);
    if (Target && Sym == Target) {
      if (RootDepth)
        *RootDepth = -1;
      return true;
    }
  }

  GCCellContentMapTy ContentMap = State->get<GCCellContentMap>();
  for (auto I = ContentMap.begin(), E = ContentMap.end(); I != E; ++I) {
    RootLookupResult Lookup;
    if (!lookupRootCell(State, I.getKey(), &Lookup))
      continue;
    if (Lookup.RS && Lookup.RS->isRootArray() && !Lookup.SlotRegion)
      continue;
    CellWorklist.push_back(I.getKey());
  }

  // Keep the older root-slot content map as a compatibility cache while the
  // checker is migrated to GCCellContentMap.
  GCRootContentMapTy RootContentMap = State->get<GCRootContentMap>();
  for (auto I = RootContentMap.begin(), E = RootContentMap.end(); I != E; ++I) {
    RootLookupResult Lookup;
    if (!lookupRootRegion(State, I.getKey(), &Lookup) || !Lookup.SlotRegion)
      continue;
    CellWorklist.push_back(GCCell::forRegion(Lookup.SlotRegion));
  }

  // Transitional compatibility: legacy propagation records root slots directly
  // on derived symbols. Treat those entries as a derived reachability cache
  // while exact cell provenance is still being expanded.
  GCValueRootMapTy LegacyRootMap = State->get<GCValueRootMap>();
  for (auto I = LegacyRootMap.begin(), E = LegacyRootMap.end(); I != E; ++I) {
    const GCValueRootSet &Roots = I.getData();
    for (auto RI = Roots.begin(), RE = Roots.end(); RI != RE; ++RI) {
      RootLookupResult Lookup;
      if (!lookupRootRegion(State, *RI, &Lookup) || !Lookup.RootRegion)
        continue;
      if (Reachable.insert(I.getKey()).second)
        ObjectWorklist.push_back(I.getKey());
      if (Target && I.getKey() == Target) {
        if (RootRegion)
          *RootRegion = Lookup.RootRegion;
        if (RootDepth && Lookup.RS)
          *RootDepth = Lookup.RS->Lifetime.diagnosticDepth();
        return true;
      }
      break;
    }
  }

  while (!CellWorklist.empty() || !ObjectWorklist.empty()) {
    while (!CellWorklist.empty()) {
      GCCell Cell = CellWorklist.pop_back_val();
      const SymbolRef *Sym = State->get<GCCellContentMap>(Cell);
      if (!Sym && Cell.isRegionCell())
        Sym = State->get<GCRootContentMap>(Cell.Region);
      if (!Sym)
        continue;

      if (Reachable.insert(*Sym).second)
        ObjectWorklist.push_back(*Sym);
      if (Target && *Sym == Target) {
        RootLookupResult Lookup;
        if (RootRegion && lookupRootCell(State, Cell, &Lookup))
          *RootRegion = Lookup.RootRegion;
        if (RootDepth && lookupRootCell(State, Cell, &Lookup) && Lookup.RS)
          *RootDepth = Lookup.RS->Lifetime.diagnosticDepth();
        return true;
      }
    }

    while (!ObjectWorklist.empty()) {
      SymbolRef Obj = ObjectWorklist.pop_back_val();
      if (const GCCellSet *Children = State->get<GCObjectChildrenMap>(Obj)) {
        for (auto I = Children->begin(), E = Children->end(); I != E; ++I)
          CellWorklist.push_back(*I);
      }
      if (const GCSymbolSet *Children =
              State->get<GCObjectSymbolChildrenMap>(Obj)) {
        for (auto I = Children->begin(), E = Children->end(); I != E; ++I) {
          if (Reachable.insert(*I).second)
            ObjectWorklist.push_back(*I);
          if (Target && *I == Target)
            return true;
        }
      }
    }
  }

  return Target ? Reachable.count(Target) : false;
}

ProgramStateRef GCChecker::runGCSafepoint(ProgramStateRef State,
                                          SymbolRef SpeciallyRootedSymbol,
                                          SymbolRef RetSym,
                                          bool &DidChange) {
  llvm::SmallPtrSet<SymbolRef, 32> Reachable;
  traceReachableSymbols(State, Reachable);
  if (SpeciallyRootedSymbol)
    Reachable.insert(SpeciallyRootedSymbol);
  if (RetSym)
    Reachable.insert(RetSym);

  GCValueMapTy AMap = State->get<GCValueMap>();
  for (auto I = AMap.begin(), E = AMap.end(); I != E; ++I) {
    if (!I.getData().isJustAllocated())
      continue;
    if (Reachable.count(I.getKey()))
      continue;
    if (isSymbolOrOriginRooted(State, I.getKey()))
      continue;
    State = State->set<GCValueMap>(I.getKey(), ValueState::getFreed());
    DidChange = true;
  }
  return State;
}

bool GCChecker::getBestRoot(const ProgramStateRef &State, SymbolRef Sym,
                            const MemRegion **RootRegion, int *RootDepth) {
  if (RootRegion)
    *RootRegion = nullptr;
  if (RootDepth)
    *RootDepth = 0;
  if (!Sym)
    return false;
  if (State->contains<GCValuePermanentRoots>(Sym)) {
    if (RootDepth)
      *RootDepth = -1;
    return true;
  }
  const RootState *BestRS = nullptr;
  const MemRegion *BestRootRegion = nullptr;
  if (const GCValueRootSet *Roots = State->get<GCValueRootMap>(Sym)) {
    for (auto I = Roots->begin(), E = Roots->end(); I != E; ++I) {
      RootLookupResult Lookup;
      if (!lookupRootRegion(State, *I, &Lookup) || !Lookup.RootRegion)
        continue;
      if (!BestRS || Lookup.RS->Lifetime.outlives(BestRS->Lifetime)) {
        BestRS = Lookup.RS;
        BestRootRegion = Lookup.RootRegion;
      }
    }
  }

  if (!BestRS) {
    llvm::SmallPtrSet<SymbolRef, 32> Reachable;
    return traceReachableSymbols(State, Reachable, Sym, RootRegion, RootDepth);
  }
  if (RootRegion)
    *RootRegion = BestRootRegion;
  if (RootDepth)
    *RootDepth = BestRS->Lifetime.diagnosticDepth();
  return true;
}

bool GCChecker::isSymbolRooted(const ProgramStateRef &State, SymbolRef Sym) {
  return getBestRoot(State, Sym);
}

bool GCChecker::isSymbolOrOriginRooted(const ProgramStateRef &State,
                                       SymbolRef Sym) {
  if (isSymbolRooted(State, Sym))
    return true;
  llvm::SmallPtrSet<SymbolRef, 32> Reachable;
  if (traceReachableSymbols(State, Reachable, Sym))
    return true;
  const MemRegion *Origin = getOriginRegion(Sym);
  return Origin && getRootedSymbolForRegion(State, Origin);
}

ProgramStateRef GCChecker::markSymbolRooted(ProgramStateRef State,
                                            SymbolRef Sym,
                                            const MemRegion *SlotRegion) {
  if (!Sym)
    return State;
  if (const ValueState *VS = State->get<GCValueMap>(Sym)) {
    if (VS->isPotentiallyFreed())
      State = State->set<GCValueMap>(Sym, ValueState::getAllocated());
  }
  if (!SlotRegion)
    return State->add<GCValuePermanentRoots>(Sym);

  auto &Factory = State->get_context<GCValueRootSet>();
  const GCValueRootSet *CurrentRoots = State->get<GCValueRootMap>(Sym);
  GCValueRootSet Roots =
      CurrentRoots ? *CurrentRoots : Factory.getEmptySet();
  Roots = Factory.add(Roots, SlotRegion);
  return State->set<GCValueRootMap>(Sym, Roots);
}

ProgramStateRef GCChecker::addSymbolRoots(ProgramStateRef State, SymbolRef To,
                                          SymbolRef From) {
  if (!To || !From)
    return State;
  if (State->contains<GCValuePermanentRoots>(From))
    State = State->add<GCValuePermanentRoots>(To);

  const GCValueRootSet *FromRoots = State->get<GCValueRootMap>(From);
  if (!FromRoots)
    return State;

  auto &Factory = State->get_context<GCValueRootSet>();
  const GCValueRootSet *CurrentRoots = State->get<GCValueRootMap>(To);
  GCValueRootSet Roots =
      CurrentRoots ? *CurrentRoots : Factory.getEmptySet();
  for (auto I = FromRoots->begin(), E = FromRoots->end(); I != E; ++I)
    Roots = Factory.add(Roots, *I);
  return State->set<GCValueRootMap>(To, Roots);
}

ProgramStateRef GCChecker::copySymbolRoots(ProgramStateRef State, SymbolRef To,
                                           SymbolRef From) {
  if (!To || !From)
    return State;
  if (State->contains<GCValuePermanentRoots>(From))
    State = State->add<GCValuePermanentRoots>(To);
  else
    State = State->remove<GCValuePermanentRoots>(To);
  if (const GCValueRootSet *Roots = State->get<GCValueRootMap>(From)) {
    return State->set<GCValueRootMap>(To, *Roots);
  }
  return State->remove<GCValueRootMap>(To);
}

ProgramStateRef
GCChecker::removeRootSlotFromValues(ProgramStateRef State,
                                    const MemRegion *SlotRegion) {
  if (!SlotRegion)
    return State;

  auto &Factory = State->get_context<GCValueRootSet>();
  GCValueRootMapTy RootMap = State->get<GCValueRootMap>();
  for (auto I = RootMap.begin(), E = RootMap.end(); I != E; ++I) {
    GCValueRootSet Roots = I.getData();
    if (!Roots.contains(SlotRegion))
      continue;
    Roots = Factory.remove(Roots, SlotRegion);
    if (Roots.isEmpty())
      State = State->remove<GCValueRootMap>(I.getKey());
    else
      State = State->set<GCValueRootMap>(I.getKey(), Roots);
  }

  return State;
}

ProgramStateRef
GCChecker::removeRootSlotsForRoot(ProgramStateRef State,
                                  const MemRegion *RootRegion) {
  if (!RootRegion)
    return State;

  SmallVector<const MemRegion *, 8> Slots;
  GCRootContentMapTy ContentMap = State->get<GCRootContentMap>();
  for (auto I = ContentMap.begin(), E = ContentMap.end(); I != E; ++I) {
    RootLookupResult Lookup;
    if (lookupRootRegion(State, I.getKey(), &Lookup) &&
        Lookup.RootRegion == RootRegion && Lookup.SlotRegion)
      Slots.push_back(Lookup.SlotRegion);
  }

  GCValueRootMapTy RootMap = State->get<GCValueRootMap>();
  for (auto I = RootMap.begin(), E = RootMap.end(); I != E; ++I) {
    const GCValueRootSet &Roots = I.getData();
    for (auto RI = Roots.begin(), RE = Roots.end(); RI != RE; ++RI) {
      RootLookupResult Lookup;
      if (lookupRootRegion(State, *RI, &Lookup) &&
          Lookup.RootRegion == RootRegion)
        Slots.push_back(*RI);
    }
  }

  for (const MemRegion *Slot : Slots) {
    State = removeRootSlotFromValues(State, Slot);
    State = State->remove<GCRootContentMap>(Slot);
  }
  return State;
}

ProgramStateRef GCChecker::setRootSlotContent(ProgramStateRef State,
                                              const MemRegion *SlotRegion,
                                              SymbolRef Sym) {
  RootLookupResult Lookup;
  if (!lookupRootRegion(State, SlotRegion, &Lookup) || !Lookup.SlotRegion)
    return State;

  const SymbolRef *Current = State->get<GCRootContentMap>(Lookup.SlotRegion);
  if (!Sym) {
    State = setCellContent(State, GCCell::forRegion(Lookup.SlotRegion),
                           nullptr);
    if (!Current)
      return State;
    State = removeRootSlotFromValues(State, Lookup.SlotRegion);
    return State->remove<GCRootContentMap>(Lookup.SlotRegion);
  }
  if (!Current) {
    State = setCellContent(State, GCCell::forRegion(Lookup.SlotRegion), Sym);
    State = State->set<GCRootContentMap>(Lookup.SlotRegion, Sym);
    return markSymbolRooted(State, Sym, Lookup.SlotRegion);
  }
  if (Current && *Current == Sym) {
    State = setCellContent(State, GCCell::forRegion(Lookup.SlotRegion), Sym);
    if (const GCValueRootSet *Roots = State->get<GCValueRootMap>(Sym)) {
      if (Roots->contains(Lookup.SlotRegion))
        return State;
    }
    return markSymbolRooted(State, Sym, Lookup.SlotRegion);
  }

  State = removeRootSlotFromValues(State, Lookup.SlotRegion);
  State = setCellContent(State, GCCell::forRegion(Lookup.SlotRegion), Sym);
  State = State->set<GCRootContentMap>(Lookup.SlotRegion, Sym);
  return markSymbolRooted(State, Sym, Lookup.SlotRegion);
}

namespace Helpers {
static const VarRegion *walk_back_to_global_VR(const MemRegion *Region) {
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

PDP GCChecker::GCBugVisitor::VisitNode(const ExplodedNode *N,
                                       BugReporterContext &BRC, PathSensitiveBugReport &BR) {
  const ExplodedNode *PrevN = N->getFirstPred();
  unsigned NewGCDepth = N->getState()->get<GCDepth>();
  unsigned OldGCDepth = PrevN->getState()->get<GCDepth>();
  if (NewGCDepth != OldGCDepth) {
    PathDiagnosticLocation Pos(getStmtForDiagnostics(N),
                               BRC.getSourceManager(), N->getLocationContext());
    return MakePDP(Pos, "GC frame changed here.");
  }
  unsigned NewGCState = N->getState()->get<GCDisabledAt>();
  unsigned OldGCState = PrevN->getState()->get<GCDisabledAt>();
  if (false /*NewGCState != OldGCState*/) {
    PathDiagnosticLocation Pos(getStmtForDiagnostics(N),
                               BRC.getSourceManager(), N->getLocationContext());
    return MakePDP(Pos, "GC enabledness changed here.");
  }
  return nullptr;
}

PDP GCChecker::SafepointBugVisitor::VisitNode(const ExplodedNode *N,
                                       BugReporterContext &BRC, PathSensitiveBugReport &BR) {
  const ExplodedNode *PrevN = N->getFirstPred();
  unsigned NewSafepointDisabled = N->getState()->get<SafepointDisabledAt>();
  unsigned OldSafepointDisabled = PrevN->getState()->get<SafepointDisabledAt>();
  if (NewSafepointDisabled != OldSafepointDisabled) {
    const Decl *D = &N->getCodeDecl();
    const AnnotateAttr *Ann = declHasAnnotation(D, "julia_not_safepoint");
    PathDiagnosticLocation Pos;
    if (OldSafepointDisabled == (unsigned)-1) {
      if (Ann) {
        Pos = PathDiagnosticLocation{Ann->getLoc(), BRC.getSourceManager()};
        return MakePDP(Pos, "Tracking JL_NOTSAFEPOINT annotation here.");
      } else {
        PathDiagnosticLocation Pos = PathDiagnosticLocation::createDeclBegin(
            N->getLocationContext(), BRC.getSourceManager());
        if (Pos.isValid())
          return MakePDP(Pos, "Tracking JL_NOTSAFEPOINT annotation here.");
        //N->getLocation().dump();
      }
    } else if (NewSafepointDisabled == (unsigned)-1) {
      PathDiagnosticLocation Pos = PathDiagnosticLocation::createDeclBegin(
          N->getLocationContext(), BRC.getSourceManager());
      if (Pos.isValid())
        return MakePDP(Pos, "Safepoints re-enabled here");
      //N->getLocation().dump();
    }
    // n.b. there may be no position here to report if they were disabled by julia_notsafepoint_enter/leave
  }
  return nullptr;
}

PDP GCChecker::GCValueBugVisitor::ExplainNoPropagationFromExpr(
    const clang::Expr *FromWhere, const ExplodedNode *N,
    PathDiagnosticLocation Pos, BugReporterContext &BRC, PathSensitiveBugReport &BR) {
  const MemRegion *Region =
      N->getState()->getSVal(FromWhere, N->getLocationContext()).getAsRegion();
  SymbolRef Parent = walkToRoot(
      [&](SymbolRef Sym, const ValueState *OldVState) { return !OldVState; },
      N->getState(), Region);
  if (!Parent && Region) {
    Parent = walkToRoot(
        [&](SymbolRef Sym, const ValueState *OldVState) { return !OldVState; },
        N->getState(), N->getState()->getSVal(Region).getAsRegion());
  }
  if (!Parent) {
    // May have been derived from a global. Check that
    const VarRegion *VR = Helpers::walk_back_to_global_VR(Region);
    if (VR) {
      BR.addNote("Derivation root was here",
                 PathDiagnosticLocation::create(VR->getDecl(),
                                                BRC.getSourceManager()));
      const VarDecl *VD = VR->getDecl();
      if (VD) {
        if (!declHasAnnotation(VD, "julia_globally_rooted")) {
          return MakePDP(Pos, "Argument value was derived from unrooted "
                              "global. May need GLOBALLY_ROOTED annotation.");
        } else if (!isGCTrackedType(VD->getType())) {
          return MakePDP(
              Pos, "Argument value was derived global with untracked type. You "
                   "may want to update the checker's type list");
        }
      }
      return MakePDP(Pos,
                     "Argument value was derived from global, but the checker "
                     "did not propagate the root. This may be a bug");
    }
    return MakePDP(Pos,
                   "Could not propagate root. Argument value was untracked.");
  }
  const ValueState *ValS = N->getState()->get<GCValueMap>(Parent);
  assert(ValS);
  if (ValS->isPotentiallyFreed()) {
    BR.addVisitor(make_unique<GCValueBugVisitor>(Parent));
    return MakePDP(
        Pos, "Root not propagated because it may have been freed. Tracking.");
  } else if (isSymbolRooted(N->getState(), Parent)) {
    BR.addVisitor(make_unique<GCValueBugVisitor>(Parent));
    return MakePDP(
        Pos, "Root was not propagated due to a bug. Tracking base value.");
  } else {
    BR.addVisitor(make_unique<GCValueBugVisitor>(Parent));
    return MakePDP(Pos, "No Root to propagate. Tracking.");
  }
}

PDP GCChecker::GCValueBugVisitor::ExplainNoPropagation(
    const ExplodedNode *N, PathDiagnosticLocation Pos, BugReporterContext &BRC,
    PathSensitiveBugReport &BR) {
  if (N->getLocation().getAs<StmtPoint>()) {
    const clang::Stmt *TheS = N->getLocation().castAs<StmtPoint>().getStmt();
    const clang::CallExpr *CE = dyn_cast<CallExpr>(TheS);
    const clang::MemberExpr *ME = dyn_cast<MemberExpr>(TheS);
    if (ME)
      return ExplainNoPropagationFromExpr(ME->getBase(), N, Pos, BRC, BR);
    const clang::ArraySubscriptExpr *ASE = dyn_cast<ArraySubscriptExpr>(TheS);
    if (ASE)
      return ExplainNoPropagationFromExpr(ASE->getLHS(), N, Pos, BRC, BR);
    if (!CE)
      return nullptr;
    const clang::FunctionDecl *FD = CE->getDirectCallee();
    if (!FD)
      return nullptr;
    for (unsigned i = 0; i < FD->getNumParams(); ++i) {
      if (!declHasAnnotation(FD->getParamDecl(i), "julia_propagates_root"))
        continue;
      return ExplainNoPropagationFromExpr(CE->getArg(i), N, Pos, BRC, BR);
    }
    return nullptr;
  }
  return nullptr;
}

PDP GCChecker::GCValueBugVisitor::VisitNode(const ExplodedNode *N,
                                            BugReporterContext &BRC, PathSensitiveBugReport &BR) {
  const ExplodedNode *PrevN = N->getFirstPred();
  const ValueState *NewValueState = N->getState()->get<GCValueMap>(Sym);
  const ValueState *OldValueState = PrevN->getState()->get<GCValueMap>(Sym);
  bool NewRooted = isSymbolOrOriginRooted(N->getState(), Sym);
  bool OldRooted = isSymbolOrOriginRooted(PrevN->getState(), Sym);
  int NewRootDepth = 0;
  int OldRootDepth = 0;
  getBestRoot(N->getState(), Sym, nullptr, &NewRootDepth);
  getBestRoot(PrevN->getState(), Sym, nullptr, &OldRootDepth);
  const Stmt *Stmt = getStmtForDiagnostics(N);

  PathDiagnosticLocation Pos;
  if (Stmt)
    Pos = PathDiagnosticLocation{Stmt, BRC.getSourceManager(),
                                 N->getLocationContext()};
  else
    Pos = PathDiagnosticLocation::createDeclEnd(N->getLocationContext(),
                                                BRC.getSourceManager());
  if (!NewValueState)
    return nullptr;
  if (!OldValueState) {
    if (NewRooted) {
      return MakePDP(Pos, "Started tracking value here (root was inherited).");
    } else {
      if (NewValueState->FD) {
        bool isFunctionSafepoint =
            !isFDAnnotatedNotSafepoint(NewValueState->FD, BRC.getSourceManager());
        bool maybeUnrooted =
            declHasAnnotation(NewValueState->PVD, "julia_maybe_unrooted");
        assert(isFunctionSafepoint || maybeUnrooted);
        (void)maybeUnrooted;
        Pos =
            PathDiagnosticLocation{NewValueState->PVD, BRC.getSourceManager()};
        if (!isFunctionSafepoint)
          return MakePDP(Pos, "Argument not rooted, because function was "
                              "annotated as not a safepoint");
        else
          return MakePDP(Pos, "Argument was annotated as MAYBE_UNROOTED.");
      } else {
        PDP Diag = ExplainNoPropagation(N, Pos, BRC, BR);
        if (Diag)
          return Diag;
        return MakePDP(Pos, "Started tracking value here.");
      }
    }
  }
  if (!OldValueState->isUntracked() && NewValueState->isUntracked()) {
    PDP Diag = ExplainNoPropagation(N, Pos, BRC, BR);
    if (Diag)
      return Diag;
    return MakePDP(Pos, "Created untracked derivative.");
  } else if (NewValueState->isPotentiallyFreed() &&
             OldValueState->isJustAllocated()) {
    // std::make_shared< in later LLVM
    return MakePDP(Pos, "Value may have been GCed here.");
  } else if (NewValueState->isPotentiallyFreed() &&
             !OldValueState->isPotentiallyFreed()) {
    // std::make_shared< in later LLVM
    return MakePDP(Pos,
                   "Value may have been GCed here (though I don't know why).");
  } else if (NewRooted && !OldRooted) {
    return MakePDP(Pos, "Value was rooted here.");
  } else if (!NewRooted && OldRooted) {
    return MakePDP(Pos, "Root was released here.");
  } else if (NewRooted && OldRooted && NewRootDepth != OldRootDepth) {
    return MakePDP(Pos, "Rooting Depth changed here.");
  }
  return nullptr;
}

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

void GCChecker::report_value_error(CheckerContext &C, SymbolRef Sym,
                                   const char *message,
                                   SourceRange range) const {
  // Generate an error node.
  ExplodedNode *N = C.generateErrorNode();
  if (!N)
    return;

  if (!BT)
    BT.reset(new BugType(this, "Invalid GC thingy", categories::LogicError));
  auto Report = make_unique<PathSensitiveBugReport>(*BT, message, N);
  Report->addVisitor(make_unique<GCValueBugVisitor>(Sym));
  Report->addVisitor(make_unique<GCBugVisitor>());
  Report->addVisitor(make_unique<ConditionBRVisitor>());
  if (!range.isInvalid()) {
    Report->addRange(range);
  }
  C.emitReport(std::move(Report));
}

bool GCChecker::gcEnabledHere(CheckerContext &C) const {
  return gcEnabledHere(C.getState());
}

bool GCChecker::gcEnabledHere(ProgramStateRef State) const {
  unsigned disabledAt = State->get<GCDisabledAt>();
  return disabledAt == (unsigned)-1;
}

bool GCChecker::safepointEnabledHere(CheckerContext &C) const {
    return safepointEnabledHere(C.getState());
}

bool GCChecker::safepointEnabledHere(ProgramStateRef State) const {
  unsigned disabledAt = State->get<SafepointDisabledAt>();
  return disabledAt == (unsigned)-1;
}

bool GCChecker::propagateArgumentRootedness(CheckerContext &C,
                                            ProgramStateRef &State) const {
  const auto *LCtx = C.getLocationContext();

  const auto *Site = cast<StackFrameContext>(LCtx)->getCallSite();
  if (!Site)
    return false;

  const auto *FD = dyn_cast<FunctionDecl>(LCtx->getDecl());
  if (!FD)
    return false;

  const auto *CE = dyn_cast<CallExpr>(Site);
  if (!CE)
    return false;

  // FD->dump();

  bool Change = false;
  int idx = 0;
  auto hasCopyableRoots = [&](SymbolRef Sym) {
    return Sym && (State->contains<GCValuePermanentRoots>(Sym) ||
                   State->get<GCValueRootMap>(Sym));
  };
  auto stripCasts = [](const Expr *E) {
    while (E) {
      E = E->IgnoreParens();
      if (const auto *Cast = dyn_cast<CastExpr>(E)) {
        E = Cast->getSubExpr();
        continue;
      }
      return E;
    }
    return E;
  };
  auto symbolForExprValue = [&](const Expr *E,
                                const LocationContext *ArgLCtx) -> SymbolRef {
    E = stripCasts(E);
    SVal V = State->getSVal(E, ArgLCtx);
    if (SymbolRef Sym = V.getAsSymbol())
      return Sym;
    if (const MemRegion *R = V.getAsRegion())
      return State->getSVal(R).getAsSymbol();
    if (const auto *DRE = dyn_cast_or_null<DeclRefExpr>(E)) {
      if (const auto *VD = dyn_cast<VarDecl>(DRE->getDecl()))
        return State->getSVal(State->getLValue(VD, ArgLCtx)).getAsSymbol();
    }
    return nullptr;
  };
  auto rootedBaseFromArgExpr = [&](const Expr *ArgExpr,
                                   const LocationContext *ArgLCtx) -> SymbolRef {
    ArgExpr = stripCasts(ArgExpr);
    if (const auto *ME = dyn_cast_or_null<MemberExpr>(ArgExpr)) {
      SymbolRef BaseSym = symbolForExprValue(ME->getBase(), ArgLCtx);
      if (hasCopyableRoots(BaseSym))
        return BaseSym;
    }
    return nullptr;
  };
  for (const auto P : FD->parameters()) {
    if (!isGCTrackedType(P->getType())) {
      continue;
    }
    const Expr *ArgExpr = CE->getArg(idx++);
    const LocationContext *ArgLCtx = LCtx->getParent();
    auto Arg = State->getSVal(ArgExpr, ArgLCtx);
    SymbolRef ArgSym = getTrackedSymbolForSVal(State, Arg);
    SymbolRef BaseRootSym = rootedBaseFromArgExpr(ArgExpr, ArgLCtx);
    if (!ArgSym && BaseRootSym)
      ArgSym = BaseRootSym;
    if (!ArgSym) {
      continue;
    }
    SymbolRef RootSourceSym = ArgSym;
    if (!hasCopyableRoots(RootSourceSym) && BaseRootSym)
      RootSourceSym = BaseRootSym;
    const ValueState *ValS = State->get<GCValueMap>(ArgSym);
    if (!ValS) {
      report_error(
          [&](PathSensitiveBugReport *Report) {
            Report->addNote(
                "Tried to find root for this parameter in inlined call",
                PathDiagnosticLocation::create(P, C.getSourceManager()));
          },
          C, "Missed allocation of parameter");
      continue;
    }
    auto Param = State->getLValue(P, LCtx);
    SymbolRef ParamSym = State->getSVal(Param).getAsSymbol();
    if (!ParamSym) {
      continue;
    }
    if (isGloballyRootedType(P->getType())) {
      State = State->set<GCValueMap>(ParamSym, ValueState::getAllocated());
      State = markSymbolRooted(State, ParamSym, nullptr);
      Change = true;
      continue;
    }
    State = State->set<GCValueMap>(ParamSym, *ValS);
    bool RootSourceReachable = isSymbolOrOriginRooted(State, RootSourceSym);
    State = copySymbolRoots(State, ParamSym, RootSourceSym);
    if (RootSourceReachable && !isSymbolOrOriginRooted(State, ParamSym)) {
      State = setObjectChildContent(State, RootSourceSym,
                                    GCCell::forHeapArraySummary(RootSourceSym),
                                    ParamSym);
    }
    Change = true;
  }
  return Change;
}

void GCChecker::checkBeginFunction(CheckerContext &C) const {
  // Consider top-level argument values rooted, unless an annotation says
  // otherwise
  const auto *LCtx = C.getLocationContext();
  const auto *FD = dyn_cast<FunctionDecl>(LCtx->getDecl());
  assert(FD);
  unsigned CurrentHeight = getStackFrameHeight(C.getStackFrame());
  ProgramStateRef State = C.getState();
  bool Change = false;
  if (C.inTopFrame()) {
    State = State->set<GCDisabledAt>((unsigned)-1);
    State = State->set<SafepointDisabledAt>((unsigned)-1);
    Change = true;
  }
  if (gcEnabledHere(State) && declHasAnnotation(FD, "julia_gc_disabled")) {
    State = State->set<GCDisabledAt>(CurrentHeight);
    Change = true;
  }
  bool isFunctionSafepoint = !isFDAnnotatedNotSafepoint(FD, getSM(C));
  if (safepointEnabledHere(State) &&
      (!isFunctionSafepoint || declHasAnnotation(FD, "julia_notsafepoint_leave"))) {
    State = State->set<SafepointDisabledAt>(CurrentHeight);
    Change = true;
  }
  if (!C.inTopFrame()) {
    if (propagateArgumentRootedness(C, State) || Change)
      C.addTransition(State);
    return;
  }
  for (const auto P : FD->parameters()) {
    if (declHasAnnotation(P, "julia_require_rooted_slot")) {
      auto Param = State->getLValue(P, LCtx);
      const MemRegion *Root = State->getSVal(Param).getAsRegion();
      State = State->set<GCRootMap>(Root, RootState::getPermanentRoot());
      State = State->set<GCRootRegistry>(GCCell::forRegion(Root),
                                         RootState::getPermanentRoot());
    } else if (P->getName() == "args" && P->getType()->isPointerType() &&
               P->getType()->getPointeeType()->isPointerType() &&
               isGCTrackedType(P->getType()->getPointeeType())) {
      auto Param = State->getLValue(P, LCtx);
      const MemRegion *Root = State->getSVal(Param).getAsRegion();
      State = State->set<GCRootMap>(Root, RootState::getPermanentRootArray());
      State = State->set<GCRootRegistry>(GCCell::forRegion(Root),
                                         RootState::getPermanentRootArray());
      Change = true;
    } else if (isGCTrackedType(P->getType())) {
      auto Param = State->getLValue(P, LCtx);
      SymbolRef AssignedSym = State->getSVal(Param).getAsSymbol();
      if (!AssignedSym)
        continue;
      assert(AssignedSym);
      State = State->set<GCValueMap>(AssignedSym,
                                     ValueState::getForArgument(FD, P, isFunctionSafepoint));
      if (isFunctionSafepoint &&
          !declHasAnnotation(P, "julia_maybe_unrooted"))
        State = markSymbolRooted(State, AssignedSym, nullptr);
      Change = true;
    }
  }
  if (Change) {
    C.addTransition(State);
  }
}

void GCChecker::checkEndFunction(const clang::ReturnStmt *RS,
                                 CheckerContext &C) const {
  ProgramStateRef State = C.getState();
  const auto *LCtx = C.getLocationContext();
  const auto *FD = dyn_cast<FunctionDecl>(LCtx->getDecl());

  if (RS && gcEnabledHere(State) && RS->getRetValue() && isGCTracked(RS->getRetValue())) {
    auto ResultVal = C.getSVal(RS->getRetValue());
    SymbolRef Sym = ResultVal.getAsSymbol(true);
    const ValueState *ValS = Sym ? State->get<GCValueMap>(Sym) : nullptr;
    if (ValS && ValS->isPotentiallyFreed()) {
      report_value_error(C, Sym, "Return value may have been GCed", RS->getSourceRange());
    }
  }

  unsigned CurrentHeight = getStackFrameHeight(C.getStackFrame());
  bool Changed = false;
  if (State->get<GCDisabledAt>() == CurrentHeight) {
    State = State->set<GCDisabledAt>((unsigned)-1);
    Changed = true;
  }
  if (State->get<SafepointDisabledAt>() == CurrentHeight) {
    if (!isFDAnnotatedNotSafepoint(FD, getSM(C)) && !(FD && declHasAnnotation(FD, "julia_notsafepoint_enter"))) {
      report_error(C, "Safepoints disabled at end of function");
    }
    State = State->set<SafepointDisabledAt>((unsigned)-1);
    Changed = true;
  }
  if (Changed)
    C.addTransition(State);
  if (!C.inTopFrame())
    return;
  unsigned CurrentDepth = C.getState()->get<GCDepth>();
  if (CurrentDepth != 0) {
    report_error(C, "Non-popped GC frame present at end of function");
  }
}

const AnnotateAttr *GCChecker::declHasAnnotation(const clang::Decl *D, const char *which) {
  for (const auto *Ann : D->specific_attrs<AnnotateAttr>()) {
    if (Ann->getAnnotation() == which)
      return Ann;
  }
  return nullptr;
}

bool GCChecker::isFDAnnotatedNotSafepoint(const clang::FunctionDecl *FD, const SourceManager &SM) {
  if (declHasAnnotation(FD, "julia_not_safepoint"))
      return true;
  SourceLocation Loc = FD->getLocation();
  StringRef Name = SM.getFilename(Loc);
  Name = llvm::sys::path::filename(Name);
  if (Name.starts_with("llvm-"))
      return true;
  return false;
}

static bool isMutexLock(StringRef name) {
    return name == "uv_mutex_lock" ||
           name == "uv_mutex_trylock" ||
           name == "pthread_mutex_lock" ||
           name == "pthread_mutex_trylock" ||
           name == "__gthread_mutex_lock" ||
           name == "__gthread_mutex_trylock" ||
           name == "__gthread_recursive_mutex_lock" ||
           name == "__gthread_recursive_mutex_trylock" ||
           name == "pthread_spin_lock" ||
           name == "pthread_spin_trylock" ||
           name == "uv_rwlock_rdlock" ||
           name == "uv_rwlock_tryrdlock" ||
           name == "uv_rwlock_wrlock" ||
           name == "uv_rwlock_trywrlock" ||
           false;
}

static bool isMutexUnlock(StringRef name) {
    return name == "uv_mutex_unlock" ||
           name == "pthread_mutex_unlock" ||
           name == "__gthread_mutex_unlock" ||
           name == "__gthread_recursive_mutex_unlock" ||
           name == "pthread_spin_unlock" ||
           name == "uv_rwlock_rdunlock" ||
           name == "uv_rwlock_wrunlock" ||
           false;
}


bool GCChecker::isGCTrackedType(QualType QT) {
  return isJuliaType(
             [](StringRef Name) {
               if (Name.ends_with_insensitive("jl_value_t") ||
                   Name.ends_with_insensitive("jl_svec_t") ||
                   Name.ends_with_insensitive("jl_sym_t") ||
                   Name.ends_with_insensitive("jl_expr_t") ||
                   Name.ends_with_insensitive("jl_code_info_t") ||
                   Name.ends_with_insensitive("jl_array_t") ||
                   Name.ends_with_insensitive("jl_genericmemory_t") ||
                   //Name.ends_with_insensitive("jl_genericmemoryref_t") ||
                   Name.ends_with_insensitive("jl_method_t") ||
                   Name.ends_with_insensitive("jl_method_instance_t") ||
                   Name.ends_with_insensitive("jl_debuginfo_t") ||
                   Name.ends_with_insensitive("jl_tupletype_t") ||
                   Name.ends_with_insensitive("jl_datatype_t") ||
                   Name.ends_with_insensitive("jl_typemap_entry_t") ||
                   Name.ends_with_insensitive("jl_typemap_level_t") ||
                   Name.ends_with_insensitive("jl_typename_t") ||
                   Name.ends_with_insensitive("jl_module_t") ||
                   Name.ends_with_insensitive("jl_tupletype_t") ||
                   Name.ends_with_insensitive("jl_gc_tracked_buffer_t") ||
                   Name.ends_with_insensitive("jl_binding_t") ||
                   Name.ends_with_insensitive("jl_binding_partition_t") ||
                   Name.ends_with_insensitive("jl_ordereddict_t") ||
                   Name.ends_with_insensitive("jl_tvar_t") ||
                   Name.ends_with_insensitive("jl_typemap_t") ||
                   Name.ends_with_insensitive("jl_unionall_t") ||
                   Name.ends_with_insensitive("jl_methtable_t") ||
                   Name.ends_with_insensitive("jl_methcache_t") ||
                   Name.ends_with_insensitive("jl_cgval_t") ||
                   Name.ends_with_insensitive("jl_codectx_t") ||
                   Name.ends_with_insensitive("jl_ast_context_t") ||
                   Name.ends_with_insensitive("jl_code_instance_t") ||
                   Name.ends_with_insensitive("jl_excstack_t") ||
                   Name.ends_with_insensitive("jl_task_t") ||
                   Name.ends_with_insensitive("jl_uniontype_t") ||
                   Name.ends_with_insensitive("jl_method_match_t") ||
                   Name.ends_with_insensitive("jl_vararg_t") ||
                   Name.ends_with_insensitive("jl_opaque_closure_t") ||
                   Name.ends_with_insensitive("jl_globalref_t") ||
                   Name.ends_with_insensitive("jl_abi_override_t") ||
                   // Probably not technically true for these, but let's allow it as a root
                   Name.ends_with_insensitive("jl_ircode_state") ||
                   Name.ends_with_insensitive("typemap_intersection_env") ||
                   Name.ends_with_insensitive("interpreter_state") ||
                   Name.ends_with_insensitive("jl_typeenv_t") ||
                   Name.ends_with_insensitive("jl_stenv_t") ||
                   Name.ends_with_insensitive("jl_varbinding_t") ||
                   Name.ends_with_insensitive("set_world") ||
                   Name.ends_with_insensitive("jl_codectx_t") ||
                   Name.ends_with_insensitive("jl_codegen_params_t") ||
                   Name.ends_with_insensitive("egal_set")) {
                 return true;
               }
               return false;
             },
             QT);
}

bool GCChecker::isGCTracked(const Expr *E) {
  while (1) {
    if (isGCTrackedType(E->getType()))
      return true;
    if (auto ICE = dyn_cast<ImplicitCastExpr>(E))
      E = ICE->getSubExpr();
    else if (auto CE = dyn_cast<CastExpr>(E))
      E = CE->getSubExpr();
    else
      return false;
  }
}

bool GCChecker::isGloballyRootedType(QualType QT) const {
  return isJuliaType(
      [](StringRef Name) { return Name.ends_with("jl_sym_t"); }, QT);
}

bool GCChecker::isSafepoint(const CallEvent &Call, CheckerContext &C) const {
  bool isCalleeSafepoint = true;
  if (Call.isInSystemHeader()) {
    // defined by -isystem per
    // https://clang.llvm.org/docs/UsersManual.html#controlling-diagnostics-in-system-headers
    isCalleeSafepoint = false;
  } else {
    const clang::Decl *Decl = Call.getDecl(); // we might not have a simple call, or we might have an SVal
    const clang::Expr *Callee = nullptr;
    if (auto CE = dyn_cast_or_null<CallExpr>(Call.getOriginExpr())) {
      Callee = CE->getCallee();
      if (Decl == nullptr)
          Decl = CE->getCalleeDecl(); // ignores dyn_cast<FunctionDecl>, so it could also be a MemberDecl, etc.
    }
    const DeclContext *DC = Decl ? Decl->getDeclContext() : nullptr;
    while (DC) {
      // Anything in llvm or std is not a safepoint
      if (const NamespaceDecl *NDC = dyn_cast<NamespaceDecl>(DC))
        if (NDC->getName() == "llvm" || NDC->getName() == "std" || NDC->getName() == "tp")
          return false;
      DC = DC->getParent();
    }
    const FunctionDecl *FD = Decl ? Decl->getAsFunction() : nullptr;
    if (!Decl || !FD) {
      if (Callee == nullptr) {
        isCalleeSafepoint = true;
      } else if (const ElaboratedType *ET = dyn_cast<ElaboratedType>(Callee->getType())){
        if (const TypedefType *TDT = dyn_cast<TypedefType>(ET->getNamedType())) {
          isCalleeSafepoint =
              !declHasAnnotation(TDT->getDecl(), "julia_not_safepoint");
        }
      } else if (const CXXPseudoDestructorExpr *PDE =
                     dyn_cast<CXXPseudoDestructorExpr>(Callee)) {
        // A pseudo-destructor is an expression that looks like a member
        // access to a destructor of a scalar type. A pseudo-destructor
        // expression has no run-time semantics beyond evaluating the base
        // expression (which would have its own CallEvent, if applicable).
        isCalleeSafepoint = false;
      }
    } else if (FD) {
      if (FD->getBuiltinID() != 0 || FD->isTrivial())
        isCalleeSafepoint = false;
      else if (FD->getDeclName().isIdentifier() &&
               (FD->getName().starts_with("uv_") ||
                FD->getName().starts_with("unw_") ||
                FD->getName().starts_with("_U")) &&
               FD->getName() != "uv_run")
        isCalleeSafepoint = false;
      else
        isCalleeSafepoint = !isFDAnnotatedNotSafepoint(FD, getSM(C));
    }
  }
  return isCalleeSafepoint;
}

bool GCChecker::processPotentialSafepoint(const CallEvent &Call,
                                          CheckerContext &C,
                                          ProgramStateRef &State) const {
  if (!isSafepoint(Call, C))
    return false;
  bool DidChange = false;
  if (!gcEnabledHere(C))
    return false;
  const Decl *D = Call.getDecl();
  const FunctionDecl *FD = D ? D->getAsFunction() : nullptr;
  SymbolRef SpeciallyRootedSymbol = nullptr;
  if (FD) {
    for (unsigned i = 0; i < FD->getNumParams(); ++i) {
      QualType ParmType = FD->getParamDecl(i)->getType();
      if (declHasAnnotation(FD->getParamDecl(i), "julia_temporarily_roots")) {
        if (ParmType->isPointerType() &&
            ParmType->getPointeeType()->isPointerType() &&
            isGCTrackedType(ParmType->getPointeeType())) {
          // This is probably an out parameter. Find the value it refers to now.
          SVal Loaded =
              State->getSVal(*(Call.getArgSVal(i).getAs<Loc>()));
          SpeciallyRootedSymbol = Loaded.getAsSymbol();
          continue;
        }
        SVal Test = Call.getArgSVal(i);
        // Walk backwards to find the symbol that we're tracking for this
        // value
        const MemRegion *Region = Test.getAsRegion();
        SpeciallyRootedSymbol =
            walkToRoot([&](SymbolRef Sym,
                           const ValueState *OldVState) { return !OldVState; },
                       State, Region);
        break;
      }
    }
  }

  // Don't free the return value
  SymbolRef RetSym = Call.getReturnValue().getAsSymbol();

  State = runGCSafepoint(State, SpeciallyRootedSymbol, RetSym, DidChange);
  return DidChange;
}

const GCChecker::ValueState *
GCChecker::getValStateForRegion(ASTContext &AstC, const ProgramStateRef &State,
                                 const MemRegion *Region, bool Debug) {
  SymbolRef Sym = getRootedSymbolForRegion(State, Region);
  if (!Sym)
    return nullptr;
  return State->get<GCValueMap>(Sym);
}

SymbolRef GCChecker::getRootedSymbolForRegion(const ProgramStateRef &State,
                                              const MemRegion *Region) {
  if (!Region)
    return nullptr;
  return walkToRoot(
      [&](SymbolRef Sym, const ValueState *OldVState) {
        return !OldVState || !isSymbolRooted(State, Sym);
      },
      State, Region);
}

bool GCChecker::processArgumentRooting(const CallEvent &Call, CheckerContext &C,
                                       ProgramStateRef &State) const {
  auto *Decl = Call.getDecl();
  const FunctionDecl *FD = Decl ? Decl->getAsFunction() : nullptr;
  if (!FD)
    return false;
  if (FD->getDeclName().isIdentifier()) {
    StringRef Name = FD->getName();
    if (Name == "jl_svecset" || Name == "jl_array_ptr_set")
      return false;
  }
  SVal RootingVal;
  bool SawRootingVal = false;
  SymbolRef RootedSymbol = nullptr;
  for (unsigned i = 0; i < FD->getNumParams(); ++i) {
    if (declHasAnnotation(FD->getParamDecl(i), "julia_rooting_argument")) {
      RootingVal = Call.getArgSVal(i);
      SawRootingVal = true;
    } else if (declHasAnnotation(FD->getParamDecl(i),
                                 "julia_rooted_argument")) {
      RootedSymbol = Call.getArgSVal(i).getAsSymbol();
    }
  }
  if (!SawRootingVal || !RootedSymbol)
    return false;
  SymbolRef RootingSym = getTrackedSymbolForSVal(State, RootingVal);
  const ValueState *OldVState =
      RootingSym ? State->get<GCValueMap>(RootingSym) : nullptr;
  if (!OldVState)
    return false;
  if (!State->get<GCValueMap>(RootedSymbol)) {
    State = State->set<GCValueMap>(RootedSymbol, ValueState::getAllocated());
  }
  State = setObjectChildContent(State, RootingSym,
                                GCCell::forHeapArraySummary(RootingSym),
                                RootedSymbol);
  State = addSymbolRoots(State, RootedSymbol, RootingSym);
  return true;
}

bool GCChecker::processHeapEffects(const CallEvent &Call, CheckerContext &C,
                                   ProgramStateRef &State) const {
  auto *Decl = Call.getDecl();
  const FunctionDecl *FD = Decl ? Decl->getAsFunction() : nullptr;
  if (!FD || !FD->getDeclName().isIdentifier())
    return false;

  StringRef Name = FD->getName();
  bool Changed = false;

  auto indexedChild = [&](SymbolRef Owner, unsigned IndexArg) {
    int64_t Index = 0;
    if (IndexArg < Call.getNumArgs() &&
        getConcreteIndex(Call.getArgSVal(IndexArg), Index))
      return GCCell::forHeapArrayElement(Owner, Index);
    return GCCell::forHeapArraySummary(Owner);
  };

  auto ensureTracked = [&](SymbolRef Sym) {
    if (Sym && !State->get<GCValueMap>(Sym))
      State = State->set<GCValueMap>(Sym, ValueState::getAllocated());
  };

  auto hasCopyableRoots = [&](SymbolRef Sym) {
    return Sym && (State->contains<GCValuePermanentRoots>(Sym) ||
                   State->get<GCValueRootMap>(Sym));
  };

  auto stripCasts = [](const Expr *E) {
    while (E) {
      E = E->IgnoreParens();
      if (const auto *Cast = dyn_cast<CastExpr>(E)) {
        E = Cast->getSubExpr();
        continue;
      }
      return E;
    }
    return E;
  };

  auto symbolForExprValue = [&](const Expr *E) -> SymbolRef {
    E = stripCasts(E);
    SVal V = C.getSVal(E);
    if (SymbolRef Sym = V.getAsSymbol())
      return Sym;
    if (const MemRegion *R = V.getAsRegion())
      return State->getSVal(R).getAsSymbol();
    if (const auto *DRE = dyn_cast_or_null<DeclRefExpr>(E)) {
      if (const auto *VD = dyn_cast<VarDecl>(DRE->getDecl()))
        return State->getSVal(State->getLValue(VD, C.getLocationContext()))
            .getAsSymbol();
    }
    return nullptr;
  };

  auto rootedBaseFromArgExpr = [&](unsigned ArgIdx) -> SymbolRef {
    if (ArgIdx >= Call.getNumArgs())
      return nullptr;
    const Expr *ArgExpr = stripCasts(Call.getArgExpr(ArgIdx));
    const auto *ME = dyn_cast_or_null<MemberExpr>(ArgExpr);
    if (!ME)
      return nullptr;
    SymbolRef BaseSym = symbolForExprValue(ME->getBase());
    return hasCopyableRoots(BaseSym) ? BaseSym : nullptr;
  };

  auto canonicalizeSymbol = [&](SymbolRef Sym) -> SymbolRef {
    if (!Sym)
      return nullptr;
    if (const MemRegion *Origin = getOriginRegion(Sym)) {
      SymbolRef OriginSym = State->getSVal(Origin).getAsSymbol();
      if (OriginSym && State->get<GCValueMap>(OriginSym))
        return OriginSym;
    }
    return Sym;
  };

  auto rootSourceForOrigin = [&](SymbolRef Sym) -> SymbolRef {
    if (!Sym)
      return nullptr;
    const MemRegion *Origin = getOriginRegion(Sym);
    if (!Origin)
      return nullptr;
    if (SymbolRef RootSource = getRootedSymbolForRegion(State, Origin))
      return RootSource;
    SymbolRef OriginSym = State->getSVal(Origin).getAsSymbol();
    if (OriginSym && OriginSym != Sym && isSymbolOrOriginRooted(State, OriginSym))
      return OriginSym;
    return nullptr;
  };

  auto materializeOriginReachability = [&](SymbolRef Sym) {
    for (unsigned I = 0; I < 4 && Sym && !isSymbolRooted(State, Sym); ++I) {
      SymbolRef RootSource = rootSourceForOrigin(Sym);
      if (!RootSource || RootSource == Sym)
        return;
      State = addObjectSymbolChild(State, RootSource, Sym);
      State = addSymbolRoots(State, Sym, RootSource);
      Sym = RootSource;
    }
  };

  if (Name == "jl_gc_wb" || Name == "jl_gc_wb_fresh" ||
      Name == "jl_gc_wb_current_task" || Name == "jl_gc_wb_knownold") {
    if (Call.getNumArgs() < 2)
      return false;
    SymbolRef Owner =
        canonicalizeSymbol(getTrackedSymbolForSVal(State, Call.getArgSVal(0)));
    SymbolRef ChildSym = Call.getArgSVal(1).getAsSymbol();
    if (!ChildSym)
      ChildSym = getTrackedSymbolForSVal(State, Call.getArgSVal(1));
    ChildSym = canonicalizeSymbol(ChildSym);
    if (!Owner || !ChildSym)
      return false;
    ensureTracked(ChildSym);
    materializeOriginReachability(Owner);
    State = addObjectSymbolChild(State, Owner, ChildSym);
    State = setObjectChildContent(State, Owner,
                                  GCCell::forHeapArraySummary(Owner),
                                  ChildSym);
    if (hasCopyableRoots(Owner))
      State = addSymbolRoots(State, ChildSym, Owner);
    return true;
  }

  if (Name == "jl_svecset" || Name == "jl_array_ptr_set") {
    if (Call.getNumArgs() < 3)
      return false;
    SymbolRef Owner =
        canonicalizeSymbol(getTrackedSymbolForSVal(State, Call.getArgSVal(0)));
    if (!Owner)
      return false;
    materializeOriginReachability(Owner);
    SymbolRef ChildSym = Call.getArgSVal(2).getAsSymbol();
    if (!ChildSym)
      ChildSym = getTrackedSymbolForSVal(State, Call.getArgSVal(2));
    ChildSym = canonicalizeSymbol(ChildSym);
    ensureTracked(ChildSym);
    State = setObjectChildContent(State, Owner, indexedChild(Owner, 1),
                                  ChildSym);
    SymbolRef RootingOwner = hasCopyableRoots(Owner) ? Owner
                                                     : rootedBaseFromArgExpr(0);
    if (RootingOwner) {
      State = setObjectChildContent(State, RootingOwner,
                                    GCCell::forHeapArraySummary(RootingOwner),
                                    ChildSym);
      State = addSymbolRoots(State, ChildSym, RootingOwner);
    }
    return true;
  }

  if (Name == "jl_svecref" || Name == "jl_array_ptr_ref" ||
      Name == "jl_get_nth_field_checked") {
    if (Call.getNumArgs() < 2)
      return false;
    SymbolRef Owner =
        canonicalizeSymbol(getTrackedSymbolForSVal(State, Call.getArgSVal(0)));
    SymbolRef RetSym = Call.getReturnValue().getAsSymbol();
    if (!Owner || !RetSym)
      return false;
    materializeOriginReachability(Owner);
    SymbolRef RootingOwner = hasCopyableRoots(Owner) ? Owner
                                                     : rootedBaseFromArgExpr(0);
    auto markReadFromRootedOwner = [&](SymbolRef Sym) {
      if (!Sym)
        return;
      ensureTracked(Sym);
      if (!RootingOwner)
        return;
      if (const ValueState *VS = State->get<GCValueMap>(Sym)) {
        if (VS->isPotentiallyFreed())
          State = State->set<GCValueMap>(Sym, ValueState::getAllocated());
      }
      State = addSymbolRoots(State, Sym, RootingOwner);
    };

    GCCell Child = indexedChild(Owner, 1);
    State = addObjectChild(State, Owner, Child);
    if (!Child.isSummaryCell() &&
        State->get<GCCellContentMap>(Child)) {
      const SymbolRef *Existing = State->get<GCCellContentMap>(Child);
      markReadFromRootedOwner(*Existing);
      if (Call.getOriginExpr())
        State = State->BindExpr(Call.getOriginExpr(), C.getLocationContext(),
                                C.getSValBuilder().makeSymbolVal(*Existing));
      Changed = true;
    } else {
      markReadFromRootedOwner(RetSym);
      if (!Child.isSummaryCell())
        State = setCellContent(State, Child, RetSym);
      Changed = true;
    }
    return Changed;
  }

  bool IsSVecConstructor =
      Name == "jl_svec1" || Name == "jl_svec2" || Name == "jl_svec3" ||
      Name == "jl_svec" || Name == "ijl_svec";
  if (!IsSVecConstructor)
    return false;

  SymbolRef Owner = Call.getReturnValue().getAsSymbol();
  if (!Owner)
    return false;

  unsigned FirstValueArg =
      (Name == "jl_svec" || Name == "ijl_svec") ? 1 : 0;
  for (unsigned I = FirstValueArg; I < Call.getNumArgs(); ++I) {
    SymbolRef ChildSym = Call.getArgSVal(I).getAsSymbol();
    if (!ChildSym || !State->get<GCValueMap>(ChildSym))
      continue;
    GCCell Child = GCCell::forHeapArrayElement(Owner, I - FirstValueArg);
    State = setObjectChildContent(State, Owner, Child, ChildSym);
    Changed = true;
  }

  return Changed;
}

bool GCChecker::processAllocationOfResult(const CallEvent &Call,
                                          CheckerContext &C,
                                          ProgramStateRef &State) const {
  QualType QT = Call.getResultType();
  if (!isGCTrackedType(QT))
    return false;
  if (!Call.getOriginExpr()) {
    return false;
  }
  SymbolRef Sym = Call.getReturnValue().getAsSymbol();
  if (!Sym) {
    SVal S = C.getSValBuilder().conjureSymbolVal(
        C.getCFGElementRef(), C.getLocationContext(), QT, C.blockCount());
    State = State->BindExpr(Call.getOriginExpr(), C.getLocationContext(), S);
    Sym = S.getAsSymbol();
  }
  if (isGloballyRootedType(QT)) {
    State = State->set<GCValueMap>(Sym, ValueState::getAllocated());
    State = markSymbolRooted(State, Sym, nullptr);
  } else {
    const ValueState *ValS = State->get<GCValueMap>(Sym);
    ValueState NewVState = ValS ? *ValS : ValueState::getAllocated();
    bool NewSymbolIsRooted = false;
    auto *Decl = Call.getDecl();
    const FunctionDecl *FD = Decl ? Decl->getAsFunction() : nullptr;
    if (FD) {
      if (declHasAnnotation(FD, "julia_globally_rooted")) {
        NewSymbolIsRooted = true;
      } else {
        // Special case for jl_box_ functions which have value-dependent
        // global roots.
        StringRef FDName =
            FD->getDeclName().isIdentifier() ? FD->getName() : "";
        if (FDName.starts_with("jl_box_") || FDName.starts_with("ijl_box_")) {
          SVal Arg = Call.getArgSVal(0);
          if (auto CI = Arg.getAs<nonloc::ConcreteInt>()) {
            const llvm::APSInt &Value = CI->getValue();
            bool GloballyRooted = false;
            const int64_t NBOX_C = 1024;
            if (FDName.starts_with("jl_box_u") || FDName.starts_with("ijl_box_u")) {
              if (Value < NBOX_C) {
                GloballyRooted = true;
              }
            } else {
              if (-NBOX_C / 2 < Value && Value < (NBOX_C - NBOX_C / 2)) {
                GloballyRooted = true;
              }
            }
            if (GloballyRooted) {
              NewSymbolIsRooted = true;
            }
          }
        } else {
          for (unsigned i = 0; i < FD->getNumParams(); ++i) {
            if (declHasAnnotation(FD->getParamDecl(i),
                                  "julia_propagates_root")) {
              SVal Test = Call.getArgSVal(i);
              // Walk backwards to find the region that roots this value
              const MemRegion *Region = Test.getAsRegion();
              SymbolRef OldSym = getRootedSymbolForRegion(State, Region);
              const ValueState *OldVState =
                  OldSym ? State->get<GCValueMap>(OldSym) : nullptr;
              if (OldVState) {
                NewVState = *OldVState;
                State = addSymbolRoots(State, Sym, OldSym);
              }
              break;
            }
          }
        }
      }
    }
    State = State->set<GCValueMap>(Sym, NewVState);
    if (NewSymbolIsRooted)
      State = markSymbolRooted(State, Sym, nullptr);
  }
  return true;
}

void GCChecker::checkPostCall(const CallEvent &Call, CheckerContext &C) const {
  ProgramStateRef State = C.getState();
  bool didChange = processArgumentRooting(Call, C, State);
  if (!C.wasInlined)
    didChange |= processPotentialSafepoint(Call, C, State);
  didChange |= processAllocationOfResult(Call, C, State);
  didChange |= processHeapEffects(Call, C, State);
  if (didChange)
    C.addTransition(State);
}

// Implicitly root values that were casted to globally rooted values
void GCChecker::checkPostStmt(const CStyleCastExpr *CE,
                              CheckerContext &C) const {
  if (!isGloballyRootedType(CE->getTypeAsWritten())) {
    if (isImmediateMacroExpansionNamed(C, CE->getExprLoc(), "jl_svec_data")) {
      if (const Expr *Base = getPointerArithmeticBase(CE->getSubExpr()))
        checkDerivingExpr(CE, Base, false, C);
    }
    return;
  }
  SymbolRef Sym = C.getSVal(CE).getAsSymbol();
  if (!Sym)
    return;
  ProgramStateRef State = C.getState();
  if (!State->get<GCValueMap>(Sym))
    State = State->set<GCValueMap>(Sym, ValueState::getAllocated());
  C.addTransition(markSymbolRooted(State, Sym, nullptr));
}

SymbolRef GCChecker::getSymbolForResult(const Expr *Result,
                                        const ValueState *OldValS,
                                        ProgramStateRef &State,
                                        CheckerContext &C) const {
  QualType QT = Result->getType();
  if (!QT->isPointerType() || QT->getPointeeType()->isVoidType())
    return nullptr;
  auto ValLoc = State->getSVal(Result, C.getLocationContext()).getAs<Loc>();
  if (!ValLoc) {
    return nullptr;
  }
  SVal Loaded = State->getSVal(*ValLoc);
  if (Loaded.isUnknown() || !Loaded.getAsSymbol()) {
    if (OldValS || GCChecker::isGCTracked(Result)) {
      Loaded = C.getSValBuilder().conjureSymbolVal(
          nullptr, C.getCFGElementRef(), C.getLocationContext(),
          Result->getType(), C.blockCount());
      State = State->bindLoc(*ValLoc, Loaded, C.getLocationContext());
      // State = State->BindExpr(Result, C.getLocationContext(),
      // State->getSVal(*ValLoc));
    }
  }
  return Loaded.getAsSymbol();
}

void GCChecker::checkDerivingExpr(const Expr *Result, const Expr *Parent,
                                  bool ParentIsLoc, CheckerContext &C) const {
  if (auto PE = dyn_cast<ParenExpr>(Parent)) {
    Parent = PE->getSubExpr();
  }
  if (auto UO = dyn_cast<UnaryOperator>(Parent)) {
    if (UO->getOpcode() == UO_AddrOf) {
      Parent = UO->getSubExpr();
    }
  }
  bool ResultTracked = true;
  ProgramStateRef State = C.getState();
  if (isGloballyRootedType(Result->getType())) {
    SymbolRef NewSym = getSymbolForResult(Result, nullptr, State, C);
    if (!NewSym) {
      return;
    }
    if (State->contains<GCValuePermanentRoots>(NewSym)) {
      return;
    }
    if (!State->get<GCValueMap>(NewSym))
      State = State->set<GCValueMap>(NewSym, ValueState::getAllocated());
    C.addTransition(markSymbolRooted(State, NewSym, nullptr));
    return;
  }
  if (!isGCTracked(Result)) {
    // TODO: We may want to refine this. This is to track pointers through the
    // array list in jl_module_t.
    bool ParentIsModule = isJuliaType(
        [](StringRef Name) { return Name.ends_with("jl_module_t"); },
        Parent->getType());
    bool ResultIsArrayList = isJuliaType(
        [](StringRef Name) { return Name.ends_with("arraylist_t"); },
        Result->getType());
    if (!(ParentIsModule && ResultIsArrayList) && isGCTracked(Parent)) {
      ResultTracked = false;
    }
  }
  // This is the pointer
  auto ResultVal = C.getSVal(Result);
  if (ResultVal.isUnknown()) {
    if (!Result->getType()->isPointerType()) {
      return;
    }
    ResultVal = C.getSValBuilder().conjureSymbolVal(
        C.getCFGElementRef(), C.getLocationContext(), Result->getType(),
        C.blockCount());
    State = State->BindExpr(Result, C.getLocationContext(), ResultVal);
  }
  auto ValLoc = ResultVal.getAs<Loc>();
  if (!ValLoc)
    return;
  SVal ParentVal = C.getSVal(Parent);
  SymbolRef OldSym = ParentVal.getAsSymbol(true);
  const MemRegion *Region = C.getSVal(Parent).getAsRegion();
  const ValueState *OldValS = OldSym ? State->get<GCValueMap>(OldSym) : nullptr;
  SymbolRef NewSym = getSymbolForResult(Result, OldValS, State, C);
  if (!NewSym) {
    return;
  }
  // NewSym might already have a better root
  const ValueState *NewValS = State->get<GCValueMap>(NewSym);
  if (ParentIsLoc) {
    if (std::optional<GCCell> SourceCell = resolveCell(State, Region)) {
      if (SourceCell->isHeapCell()) {
        SymbolRef Owner = SourceCell->Owner;
        const ValueState *OwnerState =
            Owner ? State->get<GCValueMap>(Owner) : nullptr;
        if (OwnerState && isSymbolOrOriginRooted(State, Owner)) {
          State = State->set<GCValueMap>(NewSym, ValueState::getAllocated());
          State = setObjectChildContent(State, Owner, *SourceCell, NewSym);
          State = addSymbolRoots(State, NewSym, Owner);
          C.addTransition(State);
          return;
        }
      }
    }
  }
  if (Region) {
    const VarRegion *VR = Region->getAs<VarRegion>();
    bool inheritedState = false;
    bool inheritedRoot = false;
    ValueState Updated = ValueState::getAllocated();
    if (VR && isa<ParmVarDecl>(VR->getDecl())) {
      // This works around us not being able to track symbols for struct/union
      // parameters very well.
      const auto *FD =
          dyn_cast<FunctionDecl>(C.getLocationContext()->getDecl());
      if (FD) {
        inheritedState = true;
        bool isFunctionSafepoint = !isFDAnnotatedNotSafepoint(FD, getSM(C));
        inheritedRoot =
            isFunctionSafepoint &&
            !declHasAnnotation(cast<ParmVarDecl>(VR->getDecl()),
                               "julia_maybe_unrooted");
        Updated =
            ValueState::getForArgument(FD, cast<ParmVarDecl>(VR->getDecl()), isFunctionSafepoint);
      }
    } else {
      VR = Helpers::walk_back_to_global_VR(Region);
      if (VR) {
        if (VR && rootRegionIfGlobal(VR, State, C, &Updated, &inheritedRoot)) {
          inheritedState = true;
        }
      }
    }
    if (inheritedState && ResultTracked) {
      State = State->set<GCValueMap>(NewSym, Updated);
      if (inheritedRoot) {
        SymbolRef SourceSym = State->getSVal(VR).getAsSymbol();
        if (SourceSym && isSymbolRooted(State, SourceSym))
          State = copySymbolRoots(State, NewSym, SourceSym);
        else
          State = markSymbolRooted(State, NewSym, nullptr);
      }
      C.addTransition(State);
      return;
    }
  }
  if (isSymbolOrOriginRooted(State, NewSym)) {
    if (!NewValS && ResultTracked) {
      C.addTransition(
          State->set<GCValueMap>(NewSym, ValueState::getAllocated()));
    }
    return;
  }
  if (!OldValS) {
    // This way we'll get better diagnostics
    if (isGCTracked(Result)) {
      C.addTransition(
          State->set<GCValueMap>(NewSym, ValueState::getUntracked()));
    }
    return;
  }
  if (OldValS->isPotentiallyFreed()) {
    report_value_error(C, OldSym,
                       "Creating derivative of value that may have been GCed");
  } else if (ResultTracked) {
    State = State->set<GCValueMap>(NewSym, *OldValS);
    GCCell Child = GCCell::forHeapArraySummary(OldSym);
    const Expr *CellExpr = ParentIsLoc ? Parent : Result;
    if (const auto *ME = dyn_cast<MemberExpr>(CellExpr->IgnoreParenCasts())) {
      if (const auto *FD = dyn_cast<FieldDecl>(ME->getMemberDecl()))
        Child = GCCell::forHeapField(OldSym, FD);
    } else if (const auto *ASE =
                   dyn_cast<ArraySubscriptExpr>(CellExpr->IgnoreParenCasts())) {
      int64_t Index = 0;
      if (getConcreteIndex(C.getSVal(ASE->getIdx()), Index))
        Child = GCCell::forHeapArrayElement(OldSym, Index);
    }
    State = setObjectChildContent(State, OldSym, Child, NewSym);
    State = addSymbolRoots(State, NewSym, OldSym);
    C.addTransition(State);
    return;
  }
}

// Propagate rootedness through subscript
void GCChecker::checkPostStmt(const ArraySubscriptExpr *ASE,
                              CheckerContext &C) const {
  // Could be a root array, in which case this should be considered rooted
  // by that array.
  const MemRegion *Region = C.getSVal(ASE).getAsRegion();
  ProgramStateRef State = C.getState();
  RootLookupResult Lookup;
  if (Region && isGCTracked(ASE) &&
      lookupRootRegion(State, Region, &Lookup)) {
    ValueState ValS = ValueState::getAllocated();
    SymbolRef NewSym = getSymbolForResult(ASE, &ValS, State, C);
    if (!NewSym)
      return;
    if (!State->get<GCValueMap>(NewSym))
      State = State->set<GCValueMap>(NewSym, ValS);
    if (Lookup.SlotRegion)
      State = setRootSlotContent(State, Lookup.SlotRegion, NewSym);
    else
      State = markSymbolRooted(State, NewSym, Lookup.RootRegion);
    C.addTransition(State);
    return;
  }
  checkDerivingExpr(ASE, ASE->getLHS(), true, C);
}

void GCChecker::checkPostStmt(const MemberExpr *ME, CheckerContext &C) const {
  // It is possible for the member itself to be gcrooted, so check that first
  const MemRegion *Region = C.getSVal(ME).getAsRegion();
  ProgramStateRef State = C.getState();
  if (Region && isGCTracked(ME)) {
    RootLookupResult Lookup;
    if (lookupRootRegion(State, Region, &Lookup)) {
      ValueState ValS = ValueState::getAllocated();
      SymbolRef NewSym = getSymbolForResult(ME, &ValS, State, C);
      if (!NewSym)
        return;
      if (!State->get<GCValueMap>(NewSym))
        State = State->set<GCValueMap>(NewSym, ValS);
      if (Lookup.SlotRegion)
        State = setRootSlotContent(State, Lookup.SlotRegion, NewSym);
      else
        State = markSymbolRooted(State, NewSym, Lookup.RootRegion);
      C.addTransition(State);
      return;
    }
  }
  if (!ME->getType()->isPointerType())
    return;
  clang::Expr *Base = ME->getBase();
  checkDerivingExpr(ME, Base, true, C);
}

void GCChecker::checkPostStmt(const UnaryOperator *UO,
                              CheckerContext &C) const {
  if (UO->getOpcode() == UO_Deref) {
    checkDerivingExpr(UO, UO->getSubExpr(), true, C);
  }
}

USED_FUNC void GCChecker::dumpState(const ProgramStateRef &State) {
  GCValueMapTy AMap = State->get<GCValueMap>();
  llvm::raw_ostream &Out = llvm::outs();
  Out << "State: "
      << "\n";
  for (auto I = AMap.begin(), E = AMap.end(); I != E; ++I) {
    I.getKey()->dumpToStream(Out);
  }
}

void GCChecker::checkPreCall(const CallEvent &Call, CheckerContext &C) const {
  if (!gcEnabledHere(C))
    return;
  unsigned NumArgs = Call.getNumArgs();
  ProgramStateRef State = C.getState();
  bool isCalleeSafepoint = isSafepoint(Call, C);
  auto *Decl = Call.getDecl();
  const FunctionDecl *FD = Decl ? Decl->getAsFunction() : nullptr;
  StringRef FDName =
      FD && FD->getDeclName().isIdentifier() ? FD->getName() : "";
  if (isMutexUnlock(FDName) || (FD && declHasAnnotation(FD, "julia_notsafepoint_leave"))) {
    const auto *LCtx = C.getLocationContext();
    const auto *FD = dyn_cast<FunctionDecl>(LCtx->getDecl());
    if (State->get<SafepointDisabledAt>() == getStackFrameHeight(C.getStackFrame()) &&
        !isFDAnnotatedNotSafepoint(FD, getSM(C))) {
      State = State->set<SafepointDisabledAt>((unsigned)-1);
      C.addTransition(State);
    }
  }
  if (!safepointEnabledHere(State) && isCalleeSafepoint) {
    // Suppress this warning if the function is noreturn.
    // We could separate out "not safepoint, except for noreturn functions",
    // but that seems like a lot of effort with little benefit.
    if (!FD || !FD->isNoReturn()) {
      report_error(
          [&](PathSensitiveBugReport *Report) {
            if (FD)
              Report->addNote(
                  "Tried to call method defined here",
                  PathDiagnosticLocation::create(FD, C.getSourceManager()));
            Report->addVisitor(make_unique<SafepointBugVisitor>());
          },
          C, ("Calling potential safepoint as " +
              Call.getKindAsString() + " from function annotated JL_NOTSAFEPOINT").str());
      return;
    }
  }
  if (FD && FD->getDeclName().isIdentifier() &&
      FD->getName() == "JL_GC_PROMISE_ROOTED")
    return;
  for (unsigned idx = 0; idx < NumArgs; ++idx) {
    SVal Arg = Call.getArgSVal(idx);
    SymbolRef Sym = Arg.getAsSymbol();
    // Hack to work around passing unions/structs by value.
    if (auto LCV = Arg.getAs<nonloc::LazyCompoundVal>()) {
      const MemRegion *R = LCV->getRegion();
      if (R) {
        if (const SubRegion *SR = R->getAs<SubRegion>()) {
          if (const SymbolicRegion *SSR =
                  SR->getSuperRegion()->getAs<SymbolicRegion>()) {
            Sym = SSR->getSymbol();
          }
        }
      }
    }
    if (!Sym)
      continue;
    auto *ValState = State->get<GCValueMap>(Sym);
    if (!ValState)
      continue;
    SourceRange range;
    if (const Expr *E = Call.getArgExpr(idx)) {
      range = E->getSourceRange();
      if (!isGCTracked(E))
        continue;
    }
    if (ValState->isPotentiallyFreed()) {
      report_value_error(C, Sym, "Argument value may have been GCed", range);
    }
    if (isSymbolOrOriginRooted(State, Sym))
      continue;
    bool MaybeUnrooted = false;
    if (FD) {
      if (idx < FD->getNumParams()) {
        MaybeUnrooted =
            declHasAnnotation(FD->getParamDecl(idx), "julia_maybe_unrooted");
      }
    }
    if (!MaybeUnrooted && isCalleeSafepoint) {
      report_value_error(
          C, Sym,
          "Passing non-rooted value as argument to function that may GC",
          range);
    }
  }
}

bool GCChecker::evalCall(const CallEvent &Call, CheckerContext &C) const {
  // These checks should have no effect on the surrounding environment
  // (globals should not be invalidated, etc), hence the use of evalCall.
  const CallExpr *CE = dyn_cast<CallExpr>(Call.getOriginExpr());
  unsigned CurrentDepth = C.getState()->get<GCDepth>();
  auto name = CE ? C.getCalleeName(CE) : "";
  if (name == "JL_GC_POP") {
    if (CurrentDepth == 0) {
      report_error(C, "JL_GC_POP without corresponding push");
      return true;
    }
    CurrentDepth -= 1;
    // Go through all roots, see which ones are no longer with us.
    // The go through the values and unroot those for which those were our
    // roots.
    ProgramStateRef State = C.getState()->set<GCDepth>(CurrentDepth);
    GCRootMapTy AMap = State->get<GCRootMap>();
    SmallVector<const MemRegion *, 5> PoppedRoots;
    for (auto I = AMap.begin(), E = AMap.end(); I != E; ++I) {
      if (I.getData().shouldPopAtDepth(CurrentDepth)) {
        PoppedRoots.push_back(I.getKey());
      }
    }
    for (const MemRegion *R : PoppedRoots) {
      State = State->remove<GCRootRegistry>(GCCell::forRegion(R));
      State = State->remove<GCRootMap>(R);
    }
    C.addTransition(State);
    return true;
  } else if (name == "JL_GC_PUSH1" || name == "JL_GC_PUSH2" ||
             name == "JL_GC_PUSH3" || name == "JL_GC_PUSH4" ||
             name == "JL_GC_PUSH5" || name == "JL_GC_PUSH6" ||
             name == "JL_GC_PUSH7" || name == "JL_GC_PUSH8" ||
             name == "JL_GC_PUSH9") {
    ProgramStateRef State = C.getState();
    // Transform slots to roots, transform values to rooted
    unsigned NumArgs = CE->getNumArgs();
    for (unsigned i = 0; i < NumArgs; ++i) {
      SVal V = C.getSVal(CE->getArg(i));
      auto MRV = V.getAs<loc::MemRegionVal>();
      if (!MRV) {
        report_error(C, "JL_GC_PUSH with something other than a local variable");
        return true;
      }
      const MemRegion *Region = MRV->getRegion();
      State = State->set<GCRootMap>(Region, RootState::getRoot(CurrentDepth));
      State = State->set<GCRootRegistry>(GCCell::forRegion(Region),
                                         RootState::getRoot(CurrentDepth));
      // Now for the value
      SVal Value = State->getSVal(Region);
      SymbolRef Sym = Value.getAsSymbol();
      if (!Sym) {
        State = setRootSlotContent(State, Region, nullptr);
        continue;
      }
      const ValueState *ValState = State->get<GCValueMap>(Sym);
      if (!ValState)
        continue;
      if (ValState->isPotentiallyFreed())
        report_value_error(C, Sym,
                           "Trying to root value which may have been GCed");
      State = setRootSlotContent(State, Region, Sym);
    }
    CurrentDepth += 1;
    State = State->set<GCDepth>(CurrentDepth);
    C.addTransition(State);
    return true;
  } else if (name == "_JL_GC_PUSHARGS") {
    ProgramStateRef State = C.getState();
    SVal ArgArray = C.getSVal(CE->getArg(0));
    auto MRV = ArgArray.getAs<loc::MemRegionVal>();
    if (!MRV) {
      report_error(C, "JL_GC_PUSH with something other than an args array");
      return true;
    }
    const MemRegion *Region = MRV->getRegion()->StripCasts();
    State =
        State->set<GCRootMap>(Region, RootState::getRootArray(CurrentDepth));
    State = State->set<GCRootRegistry>(GCCell::forRegion(Region),
                                       RootState::getRootArray(CurrentDepth));
    CurrentDepth += 1;
    State = State->set<GCDepth>(CurrentDepth);
    C.addTransition(State);
    return true;
  } else if (name == "JL_GC_PROMISE_ROOTED") {
    SVal Arg = C.getSVal(CE->getArg(0));
    SymbolRef Sym = Arg.getAsSymbol();
    if (!Sym)
      Sym = getTrackedSymbolForSVal(C.getState(), Arg);
    if (!Sym) {
      if (const MemRegion *Region = Arg.getAsRegion())
        Sym = C.getState()->getSVal(Region).getAsSymbol();
    }
    if (!Sym) {
      report_error(C, "Can not understand this promise.");
      return true;
    }
    ProgramStateRef State = C.getState();
    if (!State->get<GCValueMap>(Sym))
      State = State->set<GCValueMap>(Sym, ValueState::getAllocated());
    C.addTransition(markSymbolRooted(State, Sym, nullptr));
    return true;
  } else if (name == "jl_gc_push_arraylist") {
    CurrentDepth += 1;
    ProgramStateRef State = C.getState()->set<GCDepth>(CurrentDepth);
    SVal ArrayList = C.getSVal(CE->getArg(1));
    // Try to find the items field
    FieldDecl *FD = NULL;
    RecordDecl *RD = dyn_cast_or_null<RecordDecl>(
        CE->getArg(1)->getType()->getPointeeType()->getAsTagDecl());
    if (RD) {
      for (FieldDecl *X : RD->fields()) {
        if (X->getName() == "items") {
          FD = X;
          break;
        }
      }
    }
    if (FD) {
      Loc ItemsLoc = *(State->getLValue(FD, ArrayList).getAs<Loc>());
      SVal Items = State->getSVal(ItemsLoc);
      if (Items.isUnknown()) {
        Items = C.getSValBuilder().conjureSymbolVal(
            C.getCFGElementRef(), C.getLocationContext(), FD->getType(),
            C.blockCount());
        State = State->bindLoc(ItemsLoc, Items, C.getLocationContext());
      }
      assert(Items.getAsRegion());
      // The items list is now rooted
      State = State->set<GCRootMap>(Items.getAsRegion(),
                                    RootState::getRootArray(CurrentDepth));
      State = State->set<GCRootRegistry>(GCCell::forRegion(Items.getAsRegion()),
                                         RootState::getRootArray(CurrentDepth));
    }
    C.addTransition(State);
    return true;
  } else if (name == "jl_ast_preserve") {
    // TODO: Maybe bind the rooting to the context. For now, the second
    //       argument gets unconditionally rooted
    ProgramStateRef State = C.getState();
    SymbolRef Sym = C.getSVal(CE->getArg(1)).getAsSymbol();
    if (!Sym)
      return true;
    if (!State->get<GCValueMap>(Sym))
      State = State->set<GCValueMap>(Sym, ValueState::getAllocated());
    C.addTransition(markSymbolRooted(State, Sym, nullptr));
    return true;
  } else if (name == "jl_gc_enable" || name == "ijl_gc_enable") {
    ProgramStateRef State = C.getState();
    // Check for a literal argument
    SVal Arg = C.getSVal(CE->getArg(0));
    auto CI = Arg.getAs<nonloc::ConcreteInt>();
    bool EnabledAfter = true;
    if (CI) {
      const llvm::APSInt &Val = CI->getValue();
      EnabledAfter = Val != 0;
    } else {
      cast<SymbolConjured>(Arg.getAsSymbol())->getStmt()->dump();
    }
    bool EnabledNow = gcEnabledHere(State);
    if (!EnabledAfter) {
      State = State->set<GCDisabledAt>((unsigned)-2);
    } else {
      State = State->set<GCDisabledAt>((unsigned)-1);
    }
    // GC State is explicitly modeled, so let's make sure
    // the execution matches our model
    SVal Result = C.getSValBuilder().makeTruthVal(EnabledNow, CE->getType());
    C.addTransition(State->BindExpr(CE, C.getLocationContext(), Result));
    return true;
  }
  {
      auto *Decl = Call.getDecl();
      const FunctionDecl *FD = Decl ? Decl->getAsFunction() : nullptr;
      if (isMutexLock(name) || (FD && declHasAnnotation(FD, "julia_notsafepoint_enter"))) {
        ProgramStateRef State = C.getState();
        if (State->get<SafepointDisabledAt>() == (unsigned)-1) {
          C.addTransition(State->set<SafepointDisabledAt>(getStackFrameHeight(C.getStackFrame())));
          return true;
        }
      }
  }
  return false;
}

void GCChecker::checkBind(SVal LVal, SVal RVal, const clang::Stmt *S,
                          CheckerContext &C) const {
  auto State = C.getState();
  const MemRegion *R = LVal.getAsRegion();
  if (!R) {
    return;
  }
  std::optional<GCCell> Cell = resolveCell(State, R);
  RootLookupResult RootLookup;
  bool shouldBeRootArray = R->getAs<ElementRegion>();
  bool HasRoot = lookupRootRegion(State, R, &RootLookup);
  SymbolRef Sym = RVal.getAsSymbol();
  const auto *TrackedRValState = Sym ? State->get<GCValueMap>(Sym) : nullptr;
  auto hasCopyableRoots = [&](SymbolRef RootSym) {
    return RootSym && (State->contains<GCValuePermanentRoots>(RootSym) ||
                       State->get<GCValueRootMap>(RootSym));
  };
  if (!TrackedRValState && Sym) {
    bool LHSCanHoldGC = false;
    if (const auto *TVR = R->getAs<TypedValueRegion>())
      LHSCanHoldGC = isGCTrackedType(TVR->getValueType());
    if (LHSCanHoldGC) {
      if (const MemRegion *Origin = getOriginRegion(Sym)) {
        std::optional<GCCell> SourceCell = resolveCell(State, Origin);
        if (SourceCell && SourceCell->isHeapCell()) {
          SymbolRef Owner = SourceCell->Owner;
          const ValueState *OwnerState =
              Owner ? State->get<GCValueMap>(Owner) : nullptr;
          if (OwnerState && isSymbolOrOriginRooted(State, Owner)) {
            State = State->set<GCValueMap>(Sym, *OwnerState);
            State = setObjectChildContent(State, Owner, *SourceCell, Sym);
            if (hasCopyableRoots(Owner))
              State = addSymbolRoots(State, Sym, Owner);
            TrackedRValState = State->get<GCValueMap>(Sym);
          }
        }
      }
    }
  }
  bool DidCellUpdate = false;
  if (Cell && (TrackedRValState || State->get<GCCellContentMap>(*Cell))) {
    State = setCellContent(State, *Cell, TrackedRValState ? Sym : nullptr);
    DidCellUpdate = true;
  }
  if (Cell && Cell->isHeapCell()) {
    if (TrackedRValState) {
      if (TrackedRValState->isPotentiallyFreed())
        report_value_error(C, Sym,
                           "Trying to store value which may have been GCed");
      State = setObjectChildContent(State, Cell->Owner, *Cell, Sym);
      if (hasCopyableRoots(Cell->Owner))
        State = addSymbolRoots(State, Sym, Cell->Owner);
      C.addTransition(State);
      return;
    }
    if (DidCellUpdate)
      C.addTransition(State);
    return;
  }
  if (!Sym && !HasRoot) {
    if (DidCellUpdate)
      C.addTransition(State);
    return;
  }
  if (!HasRoot) {
    if (const ElementRegion *ER = R->getAs<ElementRegion>()) {
      R = ER->getBaseRegion()->StripCasts();
      shouldBeRootArray = true;
    }
    ValueState ValS;
    bool LHSRooted = false;
    SymbolRef RootSource = nullptr;
    bool IsGlobalRoot = false;
    const MemRegion *BaseRegion = R->getBaseRegion();
    if (rootRegionIfGlobal(BaseRegion, State, C, &ValS, &IsGlobalRoot)) {
      LHSRooted = IsGlobalRoot;
      RootSource = BaseRegion ? State->getSVal(BaseRegion).getAsSymbol() : nullptr;
    } else {
      RootSource = getRootedSymbolForRegion(State, R);
      LHSRooted = RootSource != nullptr;
      if (const ValueState *RootSourceState =
              RootSource ? State->get<GCValueMap>(RootSource) : nullptr)
        ValS = *RootSourceState;
    }
    if (!LHSRooted) {
      if (DidCellUpdate)
        C.addTransition(State);
      return;
    }

    State = State->set<GCValueMap>(Sym, ValS);
    if (RootSource)
      State = setObjectChildContent(State, RootSource,
                                    GCCell::forHeapArraySummary(RootSource),
                                    Sym);
    if (RootSource)
      State = addSymbolRoots(State, Sym, RootSource);
    else
      State = markSymbolRooted(State, Sym, nullptr);
    C.addTransition(State);
    return;
  }
  if (shouldBeRootArray && !RootLookup.RS->isRootArray()) {
    report_error(
        C, "This assignment looks weird. Expected a root array on the LHS.");
    return;
  }
  if (!Sym) {
    if (RootLookup.SlotRegion) {
      State = setRootSlotContent(State, RootLookup.SlotRegion, nullptr);
      C.addTransition(State);
    }
    return;
  }
  const auto *RValState = State->get<GCValueMap>(Sym);
  if (!RValState) {
    if (rootRegionIfGlobal(Sym->getOriginRegion(), State, C)) {
      C.addTransition(State);
      return;
    }
    Sym->dump();
    if (auto *SC = dyn_cast<SymbolConjured>(Sym)) {
      SC->getStmt()->dump();
    }
    report_value_error(C, Sym,
                       "Saw assignment to root, but missed the allocation");
    return;
  }
  if (RValState->isPotentiallyFreed())
    report_value_error(C, Sym, "Trying to root value which may have been GCed");
  if (RootLookup.SlotRegion)
    State = setRootSlotContent(State, RootLookup.SlotRegion, Sym);
  else
    State = markSymbolRooted(State, Sym, RootLookup.RootRegion);
  C.addTransition(State);
}

bool GCChecker::rootRegionIfGlobal(const MemRegion *R, ProgramStateRef &State,
                                   CheckerContext &C, ValueState *ValS,
                                   bool *IsRooted) const {
  if (IsRooted)
    *IsRooted = false;
  if (!R)
    return false;
  const VarRegion *VR = R->getAs<VarRegion>();
  if (!VR)
    return false;
  const VarDecl *VD = VR->getDecl();
  assert(VD);
  if (!VD->hasGlobalStorage())
    return false;
  if (!isGCTrackedType(VD->getType()))
    return false;
  bool isGlobalRoot = false;
  if (declHasAnnotation(VD, "julia_globally_rooted") ||
      isGloballyRootedType(VD->getType())) {
    State = State->set<GCRootMap>(R, RootState::getPermanentRoot());
    State = State->set<GCRootRegistry>(GCCell::forRegion(R),
                                       RootState::getPermanentRoot());
    isGlobalRoot = true;
    if (IsRooted)
      *IsRooted = true;
  }
  SVal TheVal = State->getSVal(R);
  SymbolRef Sym = TheVal.getAsSymbol();
  ValueState TheValS = ValueState::getAllocated();
  if (ValS)
    *ValS = TheValS;
  if (Sym) {
    const ValueState *GVState = C.getState()->get<GCValueMap>(Sym);
    if (!GVState)
      State = State->set<GCValueMap>(Sym, TheValS);
    if (isGlobalRoot) {
      State = setCellContent(State, GCCell::forRegion(R), Sym);
      State = State->set<GCRootContentMap>(R, Sym);
      State = markSymbolRooted(State, Sym, R);
    }
  }
  return true;
}

void GCChecker::checkLocation(SVal SLoc, bool IsLoad, const Stmt *S,
                              CheckerContext &C) const {
  ProgramStateRef State = C.getState();
  bool DidChange = false;
  RootLookupResult Lookup;
  // Loading from a root produces a rooted symbol. TODO: Can we do something
  // better than this.
  if (IsLoad &&
      lookupRootRegion(State, SLoc.getAsRegion(), &Lookup)) {
    SymbolRef LoadedSym =
        State->getSVal(*SLoc.getAs<Loc>()).getAsSymbol();
    if (LoadedSym) {
      if (!State->get<GCValueMap>(LoadedSym))
        State = State->set<GCValueMap>(LoadedSym, ValueState::getAllocated());
      if (Lookup.SlotRegion) {
        DidChange = true;
        if (!State->get<GCRootContentMap>(Lookup.SlotRegion)) {
          State = setCellContent(State, GCCell::forRegion(Lookup.SlotRegion),
                                 LoadedSym);
          State = State->set<GCRootContentMap>(Lookup.SlotRegion, LoadedSym);
        }
        State = markSymbolRooted(State, LoadedSym, Lookup.SlotRegion);
      } else if (!isSymbolOrOriginRooted(State, LoadedSym)) {
        DidChange = true;
        State = markSymbolRooted(State, LoadedSym, Lookup.RootRegion);
      }
    }
  }
  // If it's just the symbol by itself, let it be. We allow dead pointers to be
  // passed around, so long as they're not accessed. However, we do want to
  // start tracking any globals that may have been accessed.
  if (rootRegionIfGlobal(SLoc.getAsRegion(), State, C)) {
    C.addTransition(State);
    return;
  }
  SymbolRef SymByItself = SLoc.getAsSymbol(false);
  if (SymByItself) {
    DidChange &&C.addTransition(State);
    return;
  }
  // This will walk backwards until it finds the base symbol
  SymbolRef Sym = SLoc.getAsSymbol(true);
  if (!Sym) {
    DidChange &&C.addTransition(State);
    return;
  }
  const ValueState *VState = State->get<GCValueMap>(Sym);
  if (!VState) {
    DidChange &&C.addTransition(State);
    return;
  }
  if (VState->isPotentiallyFreed()) {
    report_value_error(C, Sym,
                       "Trying to access value which may have been GCed");
  }
  DidChange &&C.addTransition(State);
}

namespace clang {
namespace ento {
void registerGCChecker(CheckerManager &mgr) {
  mgr.registerChecker<GCChecker>();
}
} // namespace ento
} // namespace clang

#ifdef CLANG_PLUGIN
extern "C" const char clang_analyzerAPIVersionString[] =
    CLANG_ANALYZER_API_VERSION_STRING;
extern "C" void clang_registerCheckers(CheckerRegistry &registry) {
  registry.addChecker<GCChecker>(
      "julia.GCChecker", "Validates julia gc invariants",
      "https://docs.julialang.org/en/v1/devdocs/gc-sa/"
  );
}
#endif
