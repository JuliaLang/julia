//===-- ImplicitAtomicsChecker.cpp - Null dereference checker -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This defines NullDerefChecker, a builtin check in ExprEngine that performs
// checks for null pointers at loads and stores.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ExprObjC.h"
#include "clang/AST/ExprOpenMP.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugType.h"
#include "clang/StaticAnalyzer/Core/Checker.h"
#include "clang/StaticAnalyzer/Core/CheckerManager.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CallEvent.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerHelpers.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/StaticAnalyzer/Frontend/CheckerRegistry.h"


using namespace clang;
using namespace ento;

namespace {
class ImplicitAtomicsChecker
    : public Checker< check::PreStmt<CastExpr>,
                      check::PreStmt<BinaryOperator>,
                      check::PreStmt<UnaryOperator>,
                      check::PreCall> {
                      //check::Bind
                      //check::Location
  BugType ImplicitAtomicsBugType{this, "Implicit Atomic seq_cst synchronization", "Atomics"};

  void reportBug(const Stmt *S, CheckerContext &C) const;
  void reportBug(const Stmt *S, CheckerContext &C, StringRef desc) const;
  void reportBug(const CallEvent &S, CheckerContext &C, StringRef desc="") const;

public:
  //void checkLocation(SVal location, bool isLoad, const Stmt* S,
  //                   CheckerContext &C) const;
  //void checkBind(SVal L, SVal V, const Stmt *S, CheckerContext &C) const;
  void checkPreStmt(const CastExpr *CE, CheckerContext &C) const;
  void checkPreStmt(const UnaryOperator *UOp, CheckerContext &C) const;
  void checkPreStmt(const BinaryOperator *BOp, CheckerContext &C) const;
  void checkPreCall(const CallEvent &Call, CheckerContext &C) const;
};
} // end anonymous namespace

// Checks if RD has name in Names and is in std namespace
static bool hasStdClassWithName(const CXXRecordDecl *RD,
                                ArrayRef<llvm::StringLiteral> Names) {
  // or could check ASTContext::getQualifiedTemplateName()->isDerivedFrom() ?
  if (!RD || !RD->getDeclContext()->isStdNamespace())
    return false;
  if (RD->getDeclName().isIdentifier()) {
    StringRef Name = RD->getName();
    return llvm::any_of(Names, [&Name](StringRef GivenName) -> bool {
      return Name == GivenName;
    });
  }
  return false;
}

constexpr llvm::StringLiteral STD_PTR_NAMES[] = {"atomic", "atomic_ref"};

static bool isStdAtomic(const CXXRecordDecl *RD) {
  return hasStdClassWithName(RD, STD_PTR_NAMES);
}

static bool isStdAtomicCall(const Expr *E) {
  return E && isStdAtomic(E->IgnoreImplicit()->getType()->getAsCXXRecordDecl());
}

static bool isStdAtomic(const Expr *E) {
  return E->getType()->isAtomicType();
}

void ImplicitAtomicsChecker::reportBug(const CallEvent &S, CheckerContext &C, StringRef desc) const {
    reportBug(S.getOriginExpr(), C, desc);
}

// try to find the "best" node to attach this to, so we generate fewer duplicate reports
void ImplicitAtomicsChecker::reportBug(const Stmt *S, CheckerContext &C) const {
  while (1) {
    const auto *expr = dyn_cast<Expr>(S);
    if (!expr)
      break;
    expr = expr->IgnoreParenCasts();
    if (const auto *UO = dyn_cast<UnaryOperator>(expr))
      S = UO->getSubExpr();
    else if (const auto *BO = dyn_cast<BinaryOperator>(expr))
      S = isStdAtomic(BO->getLHS()) ? BO->getLHS() :
             isStdAtomic(BO->getRHS()) ? BO->getRHS() :
             BO->getLHS();
    else
      break;
  }
  reportBug(S, C, "");
}

void ImplicitAtomicsChecker::reportBug(const Stmt *S, CheckerContext &C, StringRef desc) const {
  SmallString<100> buf;
  llvm::raw_svector_ostream os(buf);
  os << ImplicitAtomicsBugType.getDescription() << desc;
  PathDiagnosticLocation N = PathDiagnosticLocation::createBegin(
    S, C.getSourceManager(), C.getLocationContext());
  auto report = std::make_unique<BasicBugReport>(ImplicitAtomicsBugType, buf.str(), N);
  C.emitReport(std::move(report));
}

void ImplicitAtomicsChecker::checkPreStmt(const CastExpr *CE, CheckerContext &C) const {
  //if (isStdAtomic(CE) != isStdAtomic(CE->getSubExpr())) { // AtomicToNonAtomic or NonAtomicToAtomic CastExpr
  if (CE->getCastKind() == CK_AtomicToNonAtomic) {
    reportBug(CE, C);
  }
}

void ImplicitAtomicsChecker::checkPreStmt(const UnaryOperator *UOp,
                                          CheckerContext &C) const {
  if (UOp->getOpcode() == UO_AddrOf)
    return;
  const Expr *Sub = UOp->getSubExpr();
  if (isStdAtomic(UOp) || isStdAtomic(Sub))
    reportBug(UOp, C);
}

void ImplicitAtomicsChecker::checkPreStmt(const BinaryOperator *BOp,
                                          CheckerContext &C) const {
  const Expr *Lhs = BOp->getLHS();
  const Expr *Rhs = BOp->getRHS();
  if (isStdAtomic(Lhs) || isStdAtomic(Rhs) || isStdAtomic(BOp))
    reportBug(BOp, C);
}

void ImplicitAtomicsChecker::checkPreCall(const CallEvent &Call,
                                          CheckerContext &C) const {
  const auto *MC = dyn_cast<CXXInstanceCall>(&Call);
  if (!MC || !isStdAtomicCall(MC->getCXXThisExpr()))
    return;
  if (const auto *OC = dyn_cast<CXXMemberOperatorCall>(&Call)) {
    OverloadedOperatorKind OOK = OC->getOverloadedOperator();
    if (CXXOperatorCallExpr::isAssignmentOp(OOK) || OOK == OO_PlusPlus || OOK == OO_MinusMinus) {
      reportBug(Call, C, " (std::atomic)");
    }
  }
  else if (const auto *Convert = dyn_cast<CXXConversionDecl>(MC->getDecl())) {
    reportBug(Call, C, " (std::atomic)");
  }
}


//// These seem probably unnecessary:
//
//static const Expr *getDereferenceExpr(const Stmt *S, bool IsBind=false) {
//  const Expr *E = nullptr;
//
//  // Walk through lvalue casts to get the original expression
//  // that syntactically caused the load.
//  if (const Expr *expr = dyn_cast<Expr>(S))
//    E = expr->IgnoreParenLValueCasts();
//
//  if (IsBind) {
//    const VarDecl *VD;
//    const Expr *Init;
//    std::tie(VD, Init) = parseAssignment(S);
//    if (VD && Init)
//      E = Init;
//  }
//  return E;
//}
//
//// load or bare symbol
//void ImplicitAtomicsChecker::checkLocation(SVal l, bool isLoad, const Stmt* S,
//                                           CheckerContext &C) const {
//  const Expr *expr = getDereferenceExpr(S);
//  assert(expr);
//  if (isStdAtomic(expr))
//    reportBug(S, C);
//}
//
//// auto &r = *l, or store
//void ImplicitAtomicsChecker::checkBind(SVal L, SVal V, const Stmt *S,
//                                       CheckerContext &C) const {
//  const Expr *expr = getDereferenceExpr(S, /*IsBind=*/true);
//  assert(expr);
//  if (isStdAtomic(expr))
//    reportBug(S, C, " (bind)");
//}

namespace clang {
namespace ento {
void registerImplicitAtomicsChecker(CheckerManager &mgr) {
  mgr.registerChecker<ImplicitAtomicsChecker>();
}
bool shouldRegisterImplicitAtomicsChecker(const CheckerManager &mgr) {
  return true;
}
} // namespace ento
} // namespace clang

#ifdef CLANG_PLUGIN
extern "C" const char clang_analyzerAPIVersionString[] =
    CLANG_ANALYZER_API_VERSION_STRING;
extern "C" void clang_registerCheckers(CheckerRegistry &registry) {
  registry.addChecker<ImplicitAtomicsChecker>(
      "julia.ImplicitAtomics", "Flags implicit atomic operations", ""
  );
}
#endif
