// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "GCChecker.h"

// GCChecker diagnostic visitors and report construction.

namespace jl_gc_checker {

static PDP makePDP(PathDiagnosticLocation Pos, StringRef Message) {
  return std::make_unique<PathDiagnosticEventPiece>(Pos, Message);
}

PDP GCChecker::GCBugVisitor::VisitNode(const ExplodedNode *N,
                                       BugReporterContext &BRC, PathSensitiveBugReport &BR) {
  const ExplodedNode *PrevN = N->getFirstPred();
  unsigned NewGCDepth = N->getState()->get<GCDepth>();
  unsigned OldGCDepth = PrevN->getState()->get<GCDepth>();
  if (NewGCDepth != OldGCDepth) {
    PathDiagnosticLocation Pos(getStmtForDiagnostics(N),
                               BRC.getSourceManager(), N->getLocationContext());
    return makePDP(Pos, "GC frame changed here.");
  }
  unsigned NewGCState = N->getState()->get<GCDisabledAt>();
  unsigned OldGCState = PrevN->getState()->get<GCDisabledAt>();
  if (false /*NewGCState != OldGCState*/) {
    PathDiagnosticLocation Pos(getStmtForDiagnostics(N),
                               BRC.getSourceManager(), N->getLocationContext());
    return makePDP(Pos, "GC enabledness changed here.");
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
    PathDiagnosticLocation Pos;
    if (OldSafepointDisabled == (unsigned)-1) {
      const AnnotateAttr *Ann = declHasAnnotation(D, "julia_not_safepoint");
      if (!Ann)
        Ann = declHasAnnotation(D, "julia_notsafepoint_leave");
      if (Ann) {
        Pos = PathDiagnosticLocation{Ann->getLoc(), BRC.getSourceManager()};
        return makePDP(Pos, "Tracking JL_NOTSAFEPOINT annotation here.");
      }
      PathDiagnosticLocation Pos = PathDiagnosticLocation::createDeclBegin(
          N->getLocationContext(), BRC.getSourceManager());
      if (Pos.isValid())
        return makePDP(Pos, "Tracking JL_NOTSAFEPOINT annotation here.");
    } else if (NewSafepointDisabled == (unsigned)-1) {
      const AnnotateAttr *Ann = declHasAnnotation(D, "julia_safepoint_enter");
      if (Ann) {
        Pos = PathDiagnosticLocation{Ann->getLoc(), BRC.getSourceManager()};
        return makePDP(Pos, "Tracking JL_NOTSAFEPOINT annotation here.");
      }
      PathDiagnosticLocation Pos = PathDiagnosticLocation::createDeclBegin(
          N->getLocationContext(), BRC.getSourceManager());
      if (Pos.isValid())
        return makePDP(Pos, "Safepoints re-enabled here");
    }
    // n.b. there may be no position here to report if they were disabled by julia_notsafepoint_enter/leave
    // N->getLocation().dump();
  }
  return nullptr;
}

PDP GCChecker::GCValueBugVisitor::ExplainNoPropagationFromExpr(
    const clang::Expr *FromWhere, const ExplodedNode *N,
    PathDiagnosticLocation Pos, BugReporterContext &BRC, PathSensitiveBugReport &BR) {
  const MemRegion *Region =
      N->getState()->getSVal(FromWhere, N->getLocationContext()).getAsRegion();
  SymbolRef Parent = walkToRoot(
      [&](SymbolRef Sym, const LivenessState *OldVState) { return !OldVState; },
      N->getState(), Region);
  if (!Parent && Region) {
    Parent = walkToRoot(
        [&](SymbolRef Sym, const LivenessState *OldVState) { return !OldVState; },
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
          return makePDP(Pos, "Argument value was derived from unrooted "
                              "global. May need GLOBALLY_ROOTED annotation.");
        } else if (!isGCTrackedType(VD->getType())) {
          return makePDP(
              Pos, "Argument value was derived global with untracked type. You "
                   "may want to update the checker's type list");
        }
      }
      return makePDP(Pos,
                     "Argument value was derived from global, but the checker "
                     "did not propagate the root. This may be a bug");
    }
    return makePDP(Pos,
                   "Could not propagate root. Argument value was untracked.");
  }
  std::optional<LivenessState> ValS = getStateForSymbol(N->getState(), Parent);
  assert(ValS);
  if (ValS->isPotentiallyFreed()) {
    BR.addVisitor(make_unique<GCValueBugVisitor>(Parent));
    return makePDP(
        Pos, "Root not propagated because it may have been freed. Tracking.");
  } else if (GCChecker::objectsAreReachable(
                 GCChecker::getObjectsForSymbol(N->getState(), Parent),
                 GCChecker::computeReachableObjects(N->getState()))) {
    BR.addVisitor(make_unique<GCValueBugVisitor>(Parent));
    return makePDP(
        Pos, "Root was not propagated due to a bug. Tracking base value.");
  } else {
    BR.addVisitor(make_unique<GCValueBugVisitor>(Parent));
    return makePDP(Pos, "No Root to propagate. Tracking.");
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
  std::optional<LivenessState> NewSymbolState =
      getStateForSymbol(N->getState(), Sym);
  std::optional<LivenessState> OldSymbolState =
      getStateForSymbol(PrevN->getState(), Sym);
  GCObjectSet NewObjects = GCChecker::getObjectsForSymbol(N->getState(), Sym);
  GCObjectSet OldObjects = GCChecker::getObjectsForSymbol(PrevN->getState(), Sym);
  GCObjectSet NewReachableObjects =
      GCChecker::computeReachableObjects(N->getState());
  GCObjectSet OldReachableObjects =
      GCChecker::computeReachableObjects(PrevN->getState());
  bool NewReachable =
      !NewObjects.isEmpty() &&
      GCChecker::objectsAreReachable(NewObjects, NewReachableObjects);
  bool OldReachable =
      !OldObjects.isEmpty() &&
      GCChecker::objectsAreReachable(OldObjects, OldReachableObjects);
  const Stmt *Stmt = getStmtForDiagnostics(N);

  PathDiagnosticLocation Pos;
  if (Stmt)
    Pos = PathDiagnosticLocation{Stmt, BRC.getSourceManager(),
                                 N->getLocationContext()};
  else
    Pos = PathDiagnosticLocation::createDeclEnd(N->getLocationContext(),
                                                BRC.getSourceManager());
  if (!NewSymbolState)
    return nullptr;
  if (!OldSymbolState) {
    if (NewReachable) {
      return makePDP(Pos, "Started tracking value here (root was inherited).");
    } else {
      if (NewSymbolState->FD) {
        bool isFunctionSafepoint =
            !isFDAnnotatedNotSafepoint(NewSymbolState->FD, BRC.getSourceManager());
        bool maybeUnrooted =
            declHasAnnotation(NewSymbolState->PVD, "julia_maybe_unrooted");
        assert(isFunctionSafepoint || maybeUnrooted);
        (void)maybeUnrooted;
        Pos =
            PathDiagnosticLocation{NewSymbolState->PVD, BRC.getSourceManager()};
        if (!isFunctionSafepoint)
          return makePDP(Pos, "Argument not rooted, because function was "
                              "annotated as not a safepoint");
        else
          return makePDP(Pos, "Argument was annotated as MAYBE_UNROOTED.");
      } else {
        PDP Diag = ExplainNoPropagation(N, Pos, BRC, BR);
        if (Diag)
          return Diag;
        return makePDP(Pos, "Started tracking value here.");
      }
    }
  }
  if (!OldSymbolState->isUntracked() && NewSymbolState->isUntracked()) {
    PDP Diag = ExplainNoPropagation(N, Pos, BRC, BR);
    if (Diag)
      return Diag;
    return makePDP(Pos, "Created untracked derivative.");
  } else if (NewSymbolState->isPotentiallyFreed() &&
             OldSymbolState->isJustAllocated()) {
    // std::make_shared< in later LLVM
    return makePDP(Pos, "Value may have been GCed here.");
  } else if (NewSymbolState->isPotentiallyFreed() &&
             !OldSymbolState->isPotentiallyFreed()) {
    // std::make_shared< in later LLVM
    return makePDP(Pos,
                   "Value may have been GCed here (though I don't know why).");
  } else if (!OldReachable && NewReachable && !OldObjects.isEmpty() &&
             !NewObjects.isEmpty()) {
    return makePDP(Pos, "Value was rooted here.");
  } else if (OldReachable && !NewReachable && !OldObjects.isEmpty() &&
             !NewObjects.isEmpty()) {
    return makePDP(Pos, "Root was released here.");
  }
  return nullptr;
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

USED_FUNC void GCChecker::dumpState(const ProgramStateRef &State) {
  GCObjectStateMapTy AMap = State->get<GCObjectStateMap>();
  llvm::raw_ostream &Out = llvm::outs();
  Out << "State: "
      << "\n";
  for (auto I = AMap.begin(), E = AMap.end(); I != E; ++I) {
    I.getKey()->dumpToStream(Out);
  }
}

} // namespace jl_gc_checker
