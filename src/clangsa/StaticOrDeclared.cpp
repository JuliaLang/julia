// This file is a part of Julia. License is MIT: https://julialang.org/license

// StaticOrDeclared: a clang-tidy check that enforces that every function with
// external linkage defined in a source file is either
//
//   (a) declared in some header (i.e. it has a non-defining redeclaration that
//       comes from an #included file, so the function is part of an API that
//       other translation units can call), or
//   (b) declared `static` (internal linkage), so it is private to its
//       translation unit, or
//
//   (c) explicitly exported with `JL_DLLEXPORT` -- i.e. it carries an explicit
//       `__declspec(dllexport)` (on Windows) and/or
//       `__attribute__((visibility("default")))` (elsewhere). Such a function
//       is deliberately part of the public ABI even without a prototype, so it
//       is permitted. Which attribute is present depends on the platform the
//       analysis runs on, so both are accepted, or
//
//   (d) explicitly annotated with the `extern` keyword in this file. Functions
//       are external by default, so spelling out `extern` is a deliberate
//       statement that the external linkage is intended, which overrides the
//       warning. (This is the storage class `extern`, not the `extern "C"`
//       language-linkage specifier.)
//
// A function definition that is neither is potentially a mistake: it pollutes
// the global namespace with a symbol nobody can correctly call (there is no
// prototype to call it against) and prevents the compiler from optimizing it as
// a file-local function. The fix is to either add a prototype to the relevant
// header or to mark the definition `static`.
//
// The same expectation is imposed on C++ class/struct/union definitions: a type
// defined in a source file must either be declared in a header (so it is a real
// API type), be explicitly exported, or -- since a type cannot be `static` --
// live in an anonymous namespace, which is how C++ marks a type as local to its
// translation unit. A type with external linkage that is defined only in a .cpp
// has the same problems as such a function (ODR hazards across translation
// units, an emitted vtable/typeinfo nobody can name).
//
// Usage (see src/Makefile):
//   clang-tidy foo.c --quiet \
//       -load libStaticOrDeclaredPlugin.so \
//       --checks='-*,julia-static-or-declared' \
//       -- <compiler flags>

#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Lexer.h"
#include "clang-tidy/ClangTidyCheck.h"
#include "clang-tidy/ClangTidyModule.h"
#include "clang-tidy/ClangTidyModuleRegistry.h"
#include "llvm/ADT/SmallVector.h"

#include <optional>
#include <utility>

using namespace clang;
using namespace clang::tidy;
using namespace clang::ast_matchers;

namespace {

class StaticOrDeclaredCheck : public ClangTidyCheck {
public:
    StaticOrDeclaredCheck(StringRef Name, ClangTidyContext *Context)
        : ClangTidyCheck(Name, Context) {}

    void registerMatchers(MatchFinder *Finder) override {
        Finder->addMatcher(functionDecl().bind("fn"), this);
        Finder->addMatcher(cxxRecordDecl(isDefinition()).bind("rec"), this);
    }

    void check(const MatchFinder::MatchResult &Result) override {
        SourceManager &SM = Result.Context->getSourceManager();

        if (const auto *FD = Result.Nodes.getNodeAs<FunctionDecl>("fn")) {
            if (!shouldCheck(FD))
                return;
            // Report each function once, at its anchor declaration, and skip
            // functions with no declaration in this file. Every redeclaration
            // is matched, so all but the anchor are dropped here.
            const FunctionDecl *Anchor = anchorDecl(FD, SM);
            if (!Anchor || FD != Anchor)
                return;
            // Explicit `extern` is the deliberate opt-out (option (d)); it is
            // also the temporary escape hatch for forward declarations that
            // have not yet been migrated into a header.
            if (isExplicitlyExtern(FD))
                return;
            if (Anchor->isThisDeclarationADefinition()) {
                // The definition lives in this file: it is fine if declared in
                // a header (a) or explicitly exported (c).
                if (!isDeclaredInHeader(FD, SM) && !isExplicitlyExported(FD)) {
                    auto Diag =
                        diag(Anchor->getLocation(),
                             "function %0 has external linkage but is not "
                             "declared in any header; declare it in a header or "
                             "make it 'static'")
                        << Anchor;
                    addStaticFixIt(Diag, FD, SM,
                                   Result.Context->getLangOpts());
                }
            } else {
                // Only a forward declaration lives in this file; the definition
                // is in another translation unit.
                if (isExplicitlyExported(FD) && FD->getDefinition())
                    return;
                diag(Anchor->getLocation(),
                     "function %0 is forward-declared in a source file rather "
                     "than a header; move the declaration to a header or mark "
                     "it 'extern'")
                    << Anchor;
            }
        }
        if (const auto *RD = Result.Nodes.getNodeAs<CXXRecordDecl>("rec")) {
            if (shouldCheck(RD, SM) && !isDeclaredInHeader(RD, SM) &&
                !isExplicitlyExported(RD)) {
                auto Diag = diag(RD->getLocation(),
                                 "type '%0' has external linkage but is not "
                                 "declared in any header; declare it in a "
                                 "header or move it into an anonymous namespace")
                            << RD->getName();
                addAnonNamespaceFixIt(Diag, RD, SM,
                                      Result.Context->getLangOpts());
            }
        }
    }

private:
    // Decide whether FD's function is one we hold to the rule. This is a
    // property of the function rather than of a single declaration: kind and
    // linkage are shared across all of its redeclarations.
    static bool shouldCheck(const FunctionDecl *FD) {
        if (FD->isImplicit())
            return false;
        // Only free functions: methods are declared in their class body and
        // templates, specializations and instantiations are skipped.
        if (isa<CXXMethodDecl>(FD) ||
            FD->getTemplatedKind() != FunctionDecl::TK_NonTemplate)
            return false;
        if (FD->isMain())
            return false;
        // Check for a `static` function or one in an anonymous namespace.
        if (FD->getFormalLinkage() != Linkage::External)
            return false;
        return true;
    }

    // Pick the single declaration to anchor the diagnostic on, so each function
    // is reported at most once even though every redeclaration is matched.
    // Prefer the definition when it is written in the main file (the natural
    // place to point, and the one that can be made `static`); otherwise the
    // first forward declaration in the main file.
    static const FunctionDecl *anchorDecl(const FunctionDecl *FD,
                                          SourceManager &SM) {
        FileID MainID = SM.getMainFileID();
        const FunctionDecl *First = nullptr;
        for (const auto *R : FD->redecls()) {
            // Step out of any `extern "C"` specifier to find the real lexical
            // context, then keep only declarations written at file scope.
            const DeclContext *LDC = R->getLexicalDeclContext();
            while (const auto *LSD = dyn_cast<LinkageSpecDecl>(LDC))
                LDC = LSD->getLexicalDeclContext();
            if (!LDC->isFileContext())
                continue;
            SourceLocation Loc = SM.getExpansionLoc(R->getLocation());
            if (Loc.isInvalid() || SM.getFileID(Loc) != MainID)
                continue;
            if (R->isThisDeclarationADefinition())
                return R;
            if (!First)
                First = R;
        }
        return First;
    }

    // Decide whether RD is a class/struct/union definition we should hold to the rule.
    static bool shouldCheck(const CXXRecordDecl *RD, SourceManager &SM) {
        // Skip compiler-synthesized records, lambda closures and the injected
        // class name (`Foo` inside `class Foo`).
        if (RD->isImplicit() || RD->isLambda() || RD->isInjectedClassName())
            return false;
        // Anonymous structs/unions have no name to collide on.
        if (!RD->getIdentifier())
            return false;
        // Member types are governed by their enclosing class.
        if (RD->getDeclContext()->isRecord())
            return false;
        // Templates, specializations and instantiations can be ignored.
        if (RD->getDescribedClassTemplate() ||
            isa<ClassTemplateSpecializationDecl>(RD))
            return false;
        // A type in an anonymous namespace (or a local class) has internal/no
        // linkage: that is the C++ way to mark it local, so it is correct.
        if (RD->getFormalLinkage() != Linkage::External)
            return false;
        // Only flag definitions written in the main source file.
        SourceLocation Loc = SM.getExpansionLoc(RD->getLocation());
        return Loc.isValid() && SM.getFileID(Loc) == SM.getMainFileID();
    }

    // Compute the textual range of a single standalone declaration, from its
    // begin location to just past the terminating `;`, if possible.
    static bool getWrapRange(const Decl *D, const SourceManager &SM,
                             const LangOptions &LangOpts, SourceLocation &Begin,
                             SourceLocation &AfterSemi) {
        Begin = D->getBeginLoc();
        SourceLocation End = D->getEndLoc();
        if (Begin.isInvalid() || End.isInvalid() || Begin.isMacroID() ||
            End.isMacroID())
            return false;
        std::optional<Token> Semi = Lexer::findNextToken(End, SM, LangOpts);
        if (!Semi || !Semi->is(tok::semi) || Semi->getLocation().isMacroID())
            return false;
        AfterSemi =
            Lexer::getLocForEndOfToken(Semi->getLocation(), 0, SM, LangOpts);
        return AfterSemi.isValid();
    }

    // Offer a fix-it that makes a function internal by prefixing `static`:
    //     static void foo(void) { ... }
    static void addStaticFixIt(DiagnosticBuilder &Diag, const FunctionDecl *FD,
                               const SourceManager &SM,
                               const LangOptions &LangOpts) {
        FileID MainID = SM.getMainFileID();
        llvm::SmallVector<SourceLocation, 4> Inserts;
        for (const auto *R : FD->redecls()) {
            SourceLocation Loc = SM.getExpansionLoc(R->getLocation());
            if (Loc.isInvalid() || SM.getFileID(Loc) != MainID)
                continue;
            if (R->getStorageClass() != SC_None)
                return;
            // `extern "C" void f();` -- a linkage-specification directly
            // prefixing the declaration with no braces -- cannot be prefixed
            // with `static`: the resulting `extern "C" static` is ill-formed. A
            // braced `extern "C" { ... }` block is fine, since `static` then
            // lands inside the braces.
            if (const auto *LSD =
                    dyn_cast<LinkageSpecDecl>(R->getLexicalDeclContext()))
                if (!LSD->hasBraces())
                    return;
            SourceLocation Begin = R->getBeginLoc();
            if (Begin.isInvalid() || Begin.isMacroID())
                return;
            Inserts.push_back(Begin);
        }
        for (SourceLocation L : Inserts)
            Diag << FixItHint::CreateInsertion(L, "static ");
    }

    // Offer a fix-it that wraps the type in an anonymous namespace:
    //
    //     namespace {
    //     struct Foo { ... };
    //     }  // anonymous namespace
    //
    // The fix is all-or-nothing: if any one of those declarations is not a
    // simple standalone `struct Foo ...;` (or is macro-spelled), no fix is
    // offered at all, rather than emitting a partial wrap that would change the
    // program's meaning.
    static void addAnonNamespaceFixIt(DiagnosticBuilder &Diag,
                                      const CXXRecordDecl *RD,
                                      const SourceManager &SM,
                                      const LangOptions &LangOpts) {
        FileID MainID = SM.getMainFileID();
        llvm::SmallVector<std::pair<SourceLocation, SourceLocation>, 4> Ranges;
        for (const auto *R : RD->redecls()) {
            SourceLocation Loc = SM.getExpansionLoc(R->getLocation());
            if (Loc.isInvalid() || SM.getFileID(Loc) != MainID)
                continue;
            SourceLocation Begin, AfterSemi;
            if (!getWrapRange(R, SM, LangOpts, Begin, AfterSemi))
                return;
            Ranges.emplace_back(Begin, AfterSemi);
        }
        for (const auto &R : Ranges)
            Diag << FixItHint::CreateInsertion(R.first, "namespace {\n")
                 << FixItHint::CreateInsertion(R.second,
                                               "\n}  // anonymous namespace");
    }

    // An entity is "declared in a header" when it has any non-defining
    // redeclaration whose location is in a file other than the main source
    // file -- i.e. a prototype (or forward declaration) that was #included
    // rather than written inline in the source next to the definition.
    template <typename DeclT>
    static bool isDeclaredInHeader(const DeclT *D, SourceManager &SM) {
        FileID MainID = SM.getMainFileID();
        for (auto *R : D->redecls()) {
            if (R->isThisDeclarationADefinition())
                continue;
            SourceLocation Loc = SM.getExpansionLoc(R->getLocation());
            if (Loc.isValid() && SM.getFileID(Loc) != MainID)
                return true;
        }
        return false;
    }

    // An entity is explicitly exported when any of its declarations carries an
    // explicit `__declspec(dllexport)` or `__attribute__((visibility("default")))`.
    template <typename DeclT>
    static bool isExplicitlyExported(const DeclT *D) {
        for (auto *R : D->redecls()) {
            if (R->template hasAttr<DLLExportAttr>())
                return true;
            if (const auto *VA = R->template getAttr<VisibilityAttr>())
                if (VA->getVisibility() == VisibilityAttr::Default)
                    return true;
        }
        return false;
    }

    // A function whose definition or any of its declarations in this file is
    // explicitly annotated with the `extern` keyword. Functions have external
    // linkage by default, so a plain definition needs no `extern` -- writing it
    // out is a deliberate statement that the external linkage is intended, which
    // overrides the warning.
    static bool isExplicitlyExtern(const FunctionDecl *FD) {
        for (const auto *R : FD->redecls())
            if (R->getStorageClass() == SC_Extern)
                return true;
        return false;
    }
};

class StaticOrDeclaredModule : public ClangTidyModule {
public:
    void addCheckFactories(ClangTidyCheckFactories &CheckFactories) override {
        CheckFactories.registerCheck<StaticOrDeclaredCheck>(
            "julia-static-or-declared");
    }
};

} // namespace

namespace clang {
namespace tidy {

// Register the StaticOrDeclaredModule using this statically initialized
// variable.
static ClangTidyModuleRegistry::Add<::StaticOrDeclaredModule>
    X("julia-static-or-declared-module",
      "Adds the julia-static-or-declared check.");

// This anchor is used to force the linker to link in the generated object file
// and thus register the StaticOrDeclaredModule.
volatile int StaticOrDeclaredModuleAnchorSource = 0;

} // namespace tidy
} // namespace clang
