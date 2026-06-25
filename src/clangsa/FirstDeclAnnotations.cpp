// This file is a part of Julia. License is MIT: https://julialang.org/license

// FirstDeclAnnotations: a clang-tidy check that enforces that every Julia
// attribute that callers must see -- the `JL_*` macros that expand to
// `__attribute__((annotate("julia_...")))`, e.g. JL_NOTSAFEPOINT,
// JL_PROPAGATES_ROOT, JL_GLOBALLY_ROOTED, JL_ROOTED_BY_ARG(n), as well as the
// visibility/linkage macros JL_DLLEXPORT, JL_DLLIMPORT and JL_HIDDEN (which
// expand to `__attribute__((visibility(...)))` or `__declspec(dllexport/
// dllimport)`) -- is written on the first declaration of a function, which is
// normally the prototype in a header, rather than only on a later
// redeclaration such as the definition in a .c/.cpp file.
//
// Why: these attributes are consumed per-declaration -- the analyzer
// annotations by the static analyzer, and the visibility/linkage attributes by
// the compiler when emitting references. Callers in other translation units
// only ever see the header prototype, so an attribute that appears only on the
// definition is invisible to them and to the analyzer at those call sites. The
// attribute belongs on the first (header) declaration so that every caller
// sees it. While often the compiler may be able to flag some of these (like
// dllexport), in practice that has seemed to be platform-specific and poorly
// worded and doesn't have auto-fix.
//
// Two cases are exempt:
//
//   * A block-scope prototype (a local extern declaration written inside a
//   function body) may add an annotation that is valid only for that one
//   caller -- e.g. jl_read_codeinst_invoke is redeclared JL_NOTSAFEPOINT
//   inside a caller that passes waitcompile = 0. That narrower annotation
//   deliberately does not belong on the global first declaration, so such
//   declarations are not flagged. This applies only when the function also has
//   a global prototype that the block-scope declaration is narrowing; a
//   block-scope annotation on a function with no global prototype is still
//   flagged.
//
//   * A first declaration in a system header (e.g. an upstream LLVM header
//   reached via --system-header-prefix, as for llvmGetPassPluginInfo). That
//   header is not ours to edit, so the annotation cannot be hoisted onto it
//   and the later declaration is not flagged.
//
// Each diagnostic carries a fix-it that moves the attribute: it copies the
// original macro spelling (e.g. JL_NOTSAFEPOINT, JL_ROOTED_BY_ARG(1),
// JL_DLLEXPORT) onto the first declaration and removes the misplaced copy from
// the later declaration. The fix is only offered when the attribute is written
// through such a macro -- a raw `__attribute__((...))` is still diagnosed but
// without a fix, since clang only records its inner-argument range (not the
// enclosing `__attribute__((...))` syntax, which a sibling attribute may
// share), so the spelling cannot be moved safely. Pass `--fix` (or
// `--fix-notes`) to clang-tidy to apply the fixes automatically.
//
// Usage (see src/Makefile): clang-tidy foo.c --quiet \ -load
// libFirstDeclAnnotationsPlugin.so \
// --checks='-*,julia-first-decl-annotations' [--fix] \ -- <compiler flags>

#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/TypeLoc.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Lexer.h"
#include "clang-tidy/ClangTidyCheck.h"
#include "clang-tidy/ClangTidyModule.h"
#include "clang-tidy/ClangTidyModuleRegistry.h"

using namespace clang;
using namespace clang::tidy;
using namespace clang::ast_matchers;

namespace {

class FirstDeclAnnotationsCheck : public ClangTidyCheck {
public:
    FirstDeclAnnotationsCheck(StringRef Name, ClangTidyContext *Context)
        : ClangTidyCheck(Name, Context) {}

    void registerMatchers(MatchFinder *Finder) override {
        // Match every function declaration; the matcher fires once per
        // declaration node (each prototype, plus the definition), and the
        // comparison against the first declaration happens in check().
        Finder->addMatcher(functionDecl().bind("fn"), this);
    }

    void check(const MatchFinder::MatchResult &Result) override {
        const auto *FD = Result.Nodes.getNodeAs<FunctionDecl>("fn");
        if (!FD || FD->isImplicit())
            return;

        const FunctionDecl *First = FD->getFirstDecl();
        if (First == FD)
            return;

        SourceManager &SM = Result.Context->getSourceManager();

        // If the first declaration lives in a system header (e.g. an upstream
        // LLVM header reached via --system-header-prefix, as for
        // llvmGetPassPluginInfo), it is not ours to edit: the annotation cannot
        // be hoisted onto it. Flagging a later declaration here would be a
        // false positive with no applicable fix, so skip it.
        if (SM.isInSystemHeader(First->getLocation()))
            return;

        // A block-scope redeclaration (a prototype written inside a function
        // body, i.e. a local extern declaration) is intentionally allowed to
        // add an annotation that holds only for that specific caller -- e.g.
        // jl_read_codeinst_invoke is redeclared JL_NOTSAFEPOINT inside a caller
        // that passes waitcompile = 0. Such a narrower annotation must NOT be
        // hoisted onto the global first declaration, so do not flag it.
        // This exemption only applies when the function also has a global
        // prototype: the block-scope declaration is then understood to be
        // narrowing that shared declaration. A block-scope annotation on a
        // function with no global prototype is still flagged.
        if (FD->isLocalExternDecl() && hasGlobalPrototype(FD))
            return;

        const LangOptions &LO = getLangOpts();

        // Function-level annotations (e.g. JL_NOTSAFEPOINT) are written after
        // the parameter list, so the fix inserts them just after the first
        // declaration's closing `)`.
        checkDecl(FD, First, FD, /*ParamIndex=*/-1,
                  endOfToken(functionRParenLoc(First), SM, LO), SM, LO);

        // Parameter-level annotations (e.g. JL_PROPAGATES_ROOT,
        // JL_ROOTED_BY_ARG). Compare by position; redeclarations of the same
        // function have the same parameters. The fix inserts them just after
        // the matching parameter on the first declaration.
        unsigned NumParams =
            std::min(FD->getNumParams(), First->getNumParams());
        for (unsigned I = 0; I < NumParams; ++I) {
            const ParmVarDecl *FirstParam = First->getParamDecl(I);
            checkDecl(FD->getParamDecl(I), FirstParam, FD,
                      static_cast<int>(I),
                      endOfToken(FirstParam->getEndLoc(), SM, LO), SM, LO);
        }
    }

private:
    // Report any governed Julia attribute that is written on `Later` (a
    // parameter or function from a non-first declaration) but missing from
    // `First` (the corresponding declaration on the function's first
    // declaration). `FD`/`ParamIndex` are used only to phrase the diagnostic;
    // ParamIndex < 0 means the function itself. `InsertLoc` is where on the
    // first declaration an attribute should be inserted to fix the problem.
    void checkDecl(const Decl *Later, const Decl *First,
                   const FunctionDecl *FD, int ParamIndex,
                   SourceLocation InsertLoc, SourceManager &SM,
                   const LangOptions &LO) {
        for (const auto *A : Later->attrs()) {
            if (A->isInherited())
                continue;
            std::string Key = attrKey(A);
            if (Key.empty())
                continue;
            if (hasAttrLike(First, Key))
                continue;
            std::string Name = attrName(A);

            SourceLocation Loc = A->getLocation();
            if (Loc.isInvalid())
                Loc = Later->getLocation();

            // Build a fix that moves the attribute to the first declaration:
            // copy the exact macro spelling (e.g. "JL_ROOTED_BY_ARG(1)",
            // "JL_DLLEXPORT") onto the first declaration and delete it where it
            // is misplaced.
            //
            // Only do this when the attribute was written through a macro: its
            // expansion range is then the single, self-contained macro
            // invocation token, which is exactly what we want to move. A raw
            // `__attribute__((visibility("default")))` instead reports a range
            // covering only the inner `visibility("default")`, with no record of
            // the enclosing `__attribute__((...))`/`[[...]]`/`__declspec(...)`
            // syntax (which a sibling attribute may share), so copying or
            // deleting just that inner part would corrupt the source. For raw
            // attributes we still report the problem but offer no fix-it; Julia
            // sources use the macros, so the fix applies in practice. Only offer
            // the fix when both edits can be expressed against real file
            // locations.
            llvm::SmallVector<FixItHint, 2> Fixes;
            bool FromMacro = A->getRange().getBegin().isMacroID();
            CharSourceRange Spelling = SM.getExpansionRange(A->getRange());
            StringRef Text =
                Spelling.getBegin().isFileID() && Spelling.getEnd().isFileID()
                    ? Lexer::getSourceText(Spelling, SM, LO)
                    : StringRef();
            if (FromMacro && InsertLoc.isValid() && !Text.empty()) {
                Fixes.push_back(
                    FixItHint::CreateInsertion(InsertLoc, (" " + Text).str()));
                // Also drop one preceding space along with the misplaced
                // annotation so the source does not keep a double space.
                CharSourceRange Removal = Spelling;
                SourceLocation B = Spelling.getBegin();
                if (B.isFileID()) {
                    bool Invalid = false;
                    const char *Prev =
                        SM.getCharacterData(B.getLocWithOffset(-1), &Invalid);
                    if (!Invalid && Prev && (*Prev == ' ' || *Prev == '\t'))
                        Removal.setBegin(B.getLocWithOffset(-1));
                }
                Fixes.push_back(FixItHint::CreateRemoval(Removal));
            }

            if (ParamIndex < 0) {
                auto Diag = diag(Loc,
                     "Julia annotation \"%0\" is on this declaration of %1 but "
                     "missing from its first declaration; move it to the first "
                     "declaration so callers and the analyzer see it");
                Diag << Name << FD;
                for (const FixItHint &F : Fixes)
                    Diag << F;
            } else {
                auto Diag = diag(Loc,
                     "Julia annotation \"%0\" is on parameter %1 of this "
                     "declaration of %2 but missing from its first "
                     "declaration; move it to the first declaration so callers "
                     "and the analyzer see it");
                Diag << Name << (ParamIndex + 1) << FD;
                for (const FixItHint &F : Fixes)
                    Diag << F;
            }
            diag(First->getLocation(), "first declaration is here",
                 DiagnosticIDs::Note);
        }
    }

    // True if the function has a global prototype: some declaration other
    // than FD that is a file-scope declaration.
    static bool hasGlobalPrototype(const FunctionDecl *FD) {
        for (const FunctionDecl *R : FD->redecls()) {
            if (R == FD)
                continue;
            if (!R->isLocalExternDecl())
                return true;
        }
        return false;
    }

    // Location just past the end of the token at `Loc` (where a following
    // annotation would be inserted), or an invalid location if unavailable.
    static SourceLocation endOfToken(SourceLocation Loc, SourceManager &SM,
                                     const LangOptions &LO) {
        if (Loc.isInvalid())
            return SourceLocation();
        return Lexer::getLocForEndOfToken(Loc, 0, SM, LO);
    }

    // The closing `)` of a function declaration's parameter list, or an invalid
    // location if it cannot be determined.
    static SourceLocation functionRParenLoc(const FunctionDecl *FD) {
        if (TypeSourceInfo *TSI = FD->getTypeSourceInfo()) {
            TypeLoc TL = TSI->getTypeLoc().IgnoreParens();
            if (auto FTL = TL.getAs<FunctionTypeLoc>())
                return FTL.getRParenLoc();
        }
        return SourceLocation();
    }

    // Identify which attributes this check governs and how to compare them
    // across declarations. Returns a key that is equal for two attributes that
    // represent the same Julia attribute, or an empty string for attributes we
    // do not check. Two declarations "have the same attribute" iff their keys
    // match -- this is what lets us tell a missing attribute from a present one.
    static std::string attrKey(const Attr *A) {
        if (const auto *Ann = dyn_cast<AnnotateAttr>(A))
            return ("annotate:" + Ann->getAnnotation()).str();
        if (const auto *Vis = dyn_cast<VisibilityAttr>(A))
            return "visibility:" + std::to_string(Vis->getVisibility());
        if (isa<DLLExportAttr>(A))
            return "dllexport";
        if (isa<DLLImportAttr>(A))
            return "dllimport";
        return std::string();
    }

    // Human-readable name of a governed attribute, used for the diagnostic's
    // "%0". For analyzer annotations this is the `julia_...` string; for the
    // visibility/linkage attributes it is their spelling.
    static std::string attrName(const Attr *A) {
        if (const auto *Ann = dyn_cast<AnnotateAttr>(A))
            return Ann->getAnnotation().str();
        if (const auto *Vis = dyn_cast<VisibilityAttr>(A)) {
            switch (Vis->getVisibility()) {
            case VisibilityAttr::Default:
                return "visibility(\"default\")";
            case VisibilityAttr::Hidden:
                return "visibility(\"hidden\")";
            case VisibilityAttr::Protected:
                return "visibility(\"protected\")";
            }
        }
        if (isa<DLLExportAttr>(A))
            return "dllexport";
        if (isa<DLLImportAttr>(A))
            return "dllimport";
        return std::string();
    }

    // True if `D` carries a governed attribute whose key matches `Key`.
    static bool hasAttrLike(const Decl *D, StringRef Key) {
        for (const auto *A : D->attrs())
            if (attrKey(A) == Key)
                return true;
        return false;
    }
};

class FirstDeclAnnotationsModule : public ClangTidyModule {
public:
    void addCheckFactories(ClangTidyCheckFactories &CheckFactories) override {
        CheckFactories.registerCheck<FirstDeclAnnotationsCheck>(
            "julia-first-decl-annotations");
    }
};

} // namespace

namespace clang {
namespace tidy {

// Register the FirstDeclAnnotationsModule using this statically initialized
// variable.
static ClangTidyModuleRegistry::Add<::FirstDeclAnnotationsModule>
    X("julia-first-decl-annotations-module",
      "Adds the julia-first-decl-annotations check.");

// This anchor is used to force the linker to link in the generated object file
// and thus register the FirstDeclAnnotationsModule.
volatile int FirstDeclAnnotationsModuleAnchorSource = 0;

} // namespace tidy
} // namespace clang
