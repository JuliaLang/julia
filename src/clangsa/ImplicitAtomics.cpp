// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang-tidy/ClangTidy.h"
#include "clang-tidy/ClangTidyCheck.h"
#include "clang-tidy/ClangTidyModule.h"
#include "clang-tidy/ClangTidyModuleRegistry.h"

using namespace clang;
using namespace clang::tidy;
using namespace clang::ast_matchers;

class ImplicitAtomicsChecker : public ClangTidyCheck {
  void reportBug(const Stmt *S, StringRef desc="");

public:
  ImplicitAtomicsChecker(StringRef Name, ClangTidyContext *Context);
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;

private:
};

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

void ImplicitAtomicsChecker::reportBug(const Stmt *S, StringRef desc) {
  // try to find the "best" node to attach this to, so we generate fewer duplicate reports
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
  SmallString<100> buf;
  llvm::raw_svector_ostream os(buf);
  os << "Implicit Atomic seq_cst synchronization" << desc;
  diag(S->getBeginLoc(), buf.str());
}


ImplicitAtomicsChecker::
    ImplicitAtomicsChecker(StringRef Name, ClangTidyContext *Context)
    : ClangTidyCheck(Name, Context) {
}

void ImplicitAtomicsChecker::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(castExpr(hasCastKind(CK_AtomicToNonAtomic))
                         .bind("cast"),
                     this);
  Finder->addMatcher(unaryOperator(unless(hasAnyOperatorName("&")))
                         .bind("unary-op"),
                     this);
  Finder->addMatcher(binaryOperator()
                         .bind("binary-op"),
                     this);
  Finder->addMatcher(cxxOperatorCallExpr()
                         .bind("cxxcall"),
                     this);
  Finder->addMatcher(cxxMemberCallExpr()
                         .bind("cxxcall"),
                     this);
}

void ImplicitAtomicsChecker::check(const MatchFinder::MatchResult &Result) {
  if (const auto *UOp = Result.Nodes.getNodeAs<UnaryOperator>("unary-op")) {
    const Expr *Sub = UOp->getSubExpr();
    if (isStdAtomic(UOp) || isStdAtomic(Sub))
      reportBug(UOp);
  }
  if (const auto *BOp = Result.Nodes.getNodeAs<BinaryOperator>("binary-op")) {
    const Expr *Lhs = BOp->getLHS();
    const Expr *Rhs = BOp->getRHS();
    if (isStdAtomic(Lhs) || isStdAtomic(Rhs) || isStdAtomic(BOp))
      reportBug(BOp);
  }
  if (const auto *CE = Result.Nodes.getNodeAs<CastExpr>("cast")) {
    reportBug(CE);
  }
  if (const auto *Call = Result.Nodes.getNodeAs<CallExpr>("cxxcall")) {
    if (const auto *OC = dyn_cast<CXXOperatorCallExpr>(Call)) {
      const auto *CXXThisExpr = OC->getArg(0);
      if (isStdAtomicCall(CXXThisExpr)) {
        OverloadedOperatorKind OOK = OC->getOperator();
        if (CXXOperatorCallExpr::isAssignmentOp(OOK) || OOK == OO_PlusPlus || OOK == OO_MinusMinus) {
          reportBug(CXXThisExpr, " (std::atomic operator)");
        }
      }
    }
    else if (const auto *OC = dyn_cast<CXXMemberCallExpr>(Call)) {
      const auto *CXXThisExpr = OC->getImplicitObjectArgument();
      if (isStdAtomicCall(CXXThisExpr)) {
        if (isa<CXXConversionDecl>(OC->getMethodDecl())) {
          reportBug(CXXThisExpr, " (std::atomic cast)");
        }
      }
    }
  }
}

class ImplicitAtomicsCheckerModule : public ClangTidyModule {
public:
  void addCheckFactories(ClangTidyCheckFactories &CheckFactories) override {
    CheckFactories.registerCheck<ImplicitAtomicsChecker>("concurrency-implicit-atomics");
  }
};

namespace clang {
namespace tidy {

// Register the ImplicitAtomicsCheckerModule using this statically initialized variable.
static ClangTidyModuleRegistry::Add<::ImplicitAtomicsCheckerModule>
    X("concurrency-module", "Adds my concurrency checks.");

// This anchor is used to force the linker to link in the generated object file
// and thus register the ImplicitAtomicsCheckerModule.
volatile int ImplicitAtomicsCheckerModuleAnchorSource = 0;

} // namespace tidy
} // namespace clang
