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
// This check additionally enforces a second, related rule: whenever a function
// is converted to a function pointer (assignment, initialization, a call
// argument, a return, or an aggregate initializer), the function must carry at
// least the Julia analyzer annotations that the target function-pointer type
// requires. The annotations on a function-pointer type are written on the
// typedef, e.g.
//
//   typedef void (*jl_gc_cb_pre_gc_t)(int full) JL_NOTSAFEPOINT;
//
// and the analyzer assumes that any call made through such a pointer carries
// that annotation (here, that the callback does not hit a safepoint). If a
// function that is *not* JL_NOTSAFEPOINT is assigned to such a pointer, callers
// through the pointer would be analyzed unsoundly. So the converted function
// must have a superset of the target's annotations -- the declaration must
// "have more than the target". A function with fewer annotations than the
// function-pointer type it is assigned to is diagnosed (without a fix-it: the
// right resolution -- annotate the function, or relax the typedef, or correct
// the assignment -- is a judgement call). Functions the analyzer already treats as
// non-safepoints without an annotation (those declared in system or `llvm-*`
// headers, in the `llvm`/`std`/`tp` namespaces, compiler builtins, or named
// like the `uv_`/`unw_`/`_U` runtime helpers) are exempt, mirroring
// GCChecker's own reasoning so sound conversions are not flagged.
//
// This check enforces a third, closely related rule for C++ virtual methods:
// an overriding method must carry at least the Julia analyzer annotations of
// every method it overrides -- it must be "stronger than" the original. The
// analyzer assumes a virtual call made through a base-class reference carries
// the overridden method's annotations (e.g. that an overridden JL_NOTSAFEPOINT
// method does not hit a safepoint); if an override drops that annotation, calls
// dispatched dynamically to it would be analyzed unsoundly. So an override that
// provides fewer annotations than a method it overrides is diagnosed (without a
// fix-it, for the same reason as the function-pointer case: the right
// resolution is a judgement call). The same exemptions as the function-pointer
// case apply -- a method the analyzer already treats as a non-safepoint without
// an annotation is not flagged.
//
// Usage (see src/Makefile): clang-tidy foo.c --quiet \ -load
// libFirstDeclAnnotationsPlugin.so \
// --checks='-*,julia-first-decl-annotations' [--fix] \ -- <compiler flags>

#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ParentMapContext.h"
#include "clang/AST/Type.h"
#include "clang/AST/TypeLoc.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Lexer.h"
#include "clang-tidy/ClangTidyCheck.h"
#include "clang-tidy/ClangTidyModule.h"
#include "clang-tidy/ClangTidyModuleRegistry.h"
#include "llvm/ADT/StringSet.h"

#include "HelpersCommon.hpp"

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
        // comparison against the first declaration happens in checkFirstDecl().
        Finder->addMatcher(functionDecl().bind("fn"), this);

        // Match a function being converted to a function pointer.
        Finder->addMatcher(
            implicitCastExpr(
                hasCastKind(CK_FunctionToPointerDecay),
                hasSourceExpression(
                    ignoringParens(declRefExpr(to(functionDecl())))))
                .bind("conv"),
            this);
        Finder->addMatcher(
            unaryOperator(
                hasOperatorName("&"),
                hasUnaryOperand(
                    ignoringParens(declRefExpr(to(functionDecl())))))
                .bind("conv"),
            this);

        // Match a C++ method that overrides one or more virtual methods; the
        // annotations on the override are compared against the overridden
        // methods in checkOverride().
        Finder->addMatcher(cxxMethodDecl(isOverride()).bind("override"), this);
    }

    void check(const MatchFinder::MatchResult &Result) override {
        if (const auto *FD = Result.Nodes.getNodeAs<FunctionDecl>("fn"))
            checkFirstDecl(FD, Result);
        else if (const auto *Conv = Result.Nodes.getNodeAs<Expr>("conv"))
            checkFnPtrConversion(Conv, Result);
        else if (const auto *MD =
                     Result.Nodes.getNodeAs<CXXMethodDecl>("override"))
            checkOverride(MD, Result);
    }

    void checkFirstDecl(const FunctionDecl *FD,
                        const MatchFinder::MatchResult &Result) {
        if (!FD || FD->isImplicit())
            return;

        // Never act on a template instantiation: its declarations are generated
        // from the pattern and their locations map back to it, so a fix here
        // would edit (and could duplicate at) the pattern's source. The pattern
        // itself and explicit specializations are not instantiations and are
        // still handled.
        if (FD->isTemplateInstantiation())
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
        // the parameter list and any trailing qualifiers, so the fix inserts
        // them just after the first declaration's declarator suffix.
        checkDecl(FD, First, FD, /*ParamIndex=*/-1,
                  endOfToken(declaratorSuffixEnd(First, SM), SM, LO), SM, LO);

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

    // A function `Conv` (a function-to-pointer decay or `&f`) is being
    // converted to a function pointer. Diagnose if the function lacks a Julia
    // annotation that the target function-pointer type requires: the analyzer
    // assumes calls through that pointer carry the annotation, so the function
    // must carry at least the annotations of the type it is assigned to.
    void checkFnPtrConversion(const Expr *Conv,
                              const MatchFinder::MatchResult &Result) {
        ASTContext &Ctx = *Result.Context;
        const FunctionDecl *FD = referencedFunction(Conv);
        if (!FD)
            return;

        // The target function-pointer type (with its typedef sugar, which is
        // where the annotations live) is the declared type of the surrounding
        // context, not the type of the decayed-pointer node itself.
        QualType Target = targetType(Conv, Ctx);
        if (Target.isNull() || !Target->isFunctionPointerType())
            return;

        SourceManager &SM = Ctx.getSourceManager();

        // Collect the annotations the target type requires, then report each
        // one the function does not provide.
        llvm::SmallVector<std::pair<std::string, const TypedefNameDecl *>, 2>
            Required;
        collectTypedefAnnotations(Target, Ctx, Required);
        for (const auto &R : Required) {
            StringRef Key = R.first;
            if (functionProvidesAnnotation(FD, Key, SM))
                continue;
            // Strip the "annotate:" prefix added by attrKey() for the message.
            StringRef Name = Key;
            Name.consume_front("annotate:");
            auto Diag = diag(Conv->getExprLoc(),
                 "%0 is converted to a function pointer of type %1 that "
                 "requires Julia annotation \"%2\", but %0 is not annotated "
                 "\"%2\"; annotate the function so callers through the pointer "
                 "are analyzed soundly");
            Diag << FD << Target << Name;
            diag(R.second->getLocation(),
                 "function-pointer type %0 requires the annotation here",
                 DiagnosticIDs::Note)
                << R.second;
            diag(FD->getFirstDecl()->getLocation(),
                 "%0 is declared here", DiagnosticIDs::Note)
                << FD;
        }
    }

    // A C++ method `MD` overrides one or more virtual methods. The analyzer
    // assumes a virtual call dispatched through a base-class reference carries
    // the overridden method's annotations, so the override must carry at least
    // every annotation of the methods it overrides -- it must be "stronger
    // than" the original.
    void checkOverride(const CXXMethodDecl *MD,
                       const MatchFinder::MatchResult &Result) {
        SourceManager &SM = Result.Context->getSourceManager();

        llvm::SmallVector<std::pair<std::string, const CXXMethodDecl *>, 2> Required;
        llvm::StringSet<> Seen;
        for (const CXXMethodDecl *Base : MD->overridden_methods())
            for (const FunctionDecl *R : Base->redecls())
                for (const auto *A : R->attrs()) {
                    std::string Key = attrKey(A);
                    if (!StringRef(Key).starts_with("annotate:"))
                        continue;
                    if (Seen.insert(Key).second)
                        Required.emplace_back(std::move(Key), Base);
                }

        for (const auto &R : Required) {
            StringRef Key = R.first;
            if (functionProvidesAnnotation(MD, Key, SM))
                continue;
            // Strip the "annotate:" prefix added by attrKey() for the message.
            StringRef Name = Key;
            Name.consume_front("annotate:");
            auto Diag = diag(MD->getLocation(),
                 "%0 overrides a method that requires Julia annotation \"%1\", "
                 "but %0 is not annotated \"%1\"; annotate the override so "
                 "virtual calls through the base class are analyzed soundly");
            Diag << MD << Name;
            diag(R.second->getLocation(),
                 "overridden method %0 requires the annotation here",
                 DiagnosticIDs::Note)
                << R.second;
        }
    }

    // The function referred to by a conversion node (a function-to-pointer
    // decay `ImplicitCastExpr` or an address-of `UnaryOperator`), or null.
    static const FunctionDecl *referencedFunction(const Expr *Conv) {
        const Expr *Sub = nullptr;
        if (const auto *ICE = dyn_cast<ImplicitCastExpr>(Conv))
            Sub = ICE->getSubExpr();
        else if (const auto *UO = dyn_cast<UnaryOperator>(Conv))
            Sub = UO->getSubExpr();
        if (!Sub)
            return nullptr;
        const auto *DRE = dyn_cast<DeclRefExpr>(Sub->IgnoreParens());
        return DRE ? dyn_cast<FunctionDecl>(DRE->getDecl()) : nullptr;
    }

    // Recover the declared type the conversion node `Conv` is being assigned
    // to, preserving typedef sugar (which is where function-pointer annotations
    // live). The decayed-pointer node itself has the canonical, unsugared
    // pointer type, so the sugared target is read from the parent context:
    // the initialized variable, the assignment LHS, the called function's
    // parameter, the enclosing function's return type, or the field/element of
    // an aggregate initializer. Contexts that are not one of these (or that we
    // cannot resolve precisely) yield a null type and are not diagnosed.
    static QualType targetType(const Expr *Conv, ASTContext &Ctx) {
        DynTypedNodeList Parents = Ctx.getParents(*Conv);
        if (Parents.empty())
            return QualType();
        const DynTypedNode &P = Parents[0];

        if (const auto *VD = P.get<VarDecl>())
            return VD->getType();
        if (const auto *FD = P.get<FieldDecl>())
            return FD->getType();
        if (const auto *BO = P.get<BinaryOperator>()) {
            if (BO->getOpcode() == BO_Assign && BO->getRHS() == Conv)
                return BO->getLHS()->getType();
            return QualType();
        }
        if (P.get<ReturnStmt>())
            return enclosingFunctionReturnType(Conv, Ctx);
        if (const auto *CE = P.get<CallExpr>())
            return callArgType(CE, Conv);
        if (const auto *ILE = P.get<InitListExpr>())
            return initListElementType(ILE, Conv);
        return QualType();
    }

    // The parameter type (with sugar) that argument `Conv` of call `CE` is
    // passed to, or null for the callee position, varargs, or an unresolved
    // callee.
    static QualType callArgType(const CallExpr *CE, const Expr *Conv) {
        if (CE->getCallee() == Conv)
            return QualType();
        const FunctionDecl *Callee = CE->getDirectCallee();
        if (!Callee)
            return QualType();
        for (unsigned I = 0, N = CE->getNumArgs(); I < N; ++I) {
            if (CE->getArg(I) != Conv)
                continue;
            if (I < Callee->getNumParams())
                return Callee->getParamDecl(I)->getType();
            return QualType(); // variadic argument: no declared parameter type
        }
        return QualType();
    }

    // The return type (with sugar) of the function enclosing `Conv`, found by
    // walking up the parent chain to the nearest function declaration.
    static QualType enclosingFunctionReturnType(const Expr *Conv,
                                                ASTContext &Ctx) {
        DynTypedNode Node = DynTypedNode::create(*Conv);
        for (;;) {
            DynTypedNodeList Parents = Ctx.getParents(Node);
            if (Parents.empty())
                return QualType();
            Node = Parents[0];
            if (const auto *FD = Node.get<FunctionDecl>())
                return FD->getReturnType();
        }
    }

    // The field or element type (with sugar) that `Conv` initializes within an
    // aggregate initializer `ILE`, matched by position. Designated and nested
    // forms we cannot match precisely yield a null type.
    static QualType initListElementType(const InitListExpr *ILE,
                                        const Expr *Conv) {
        if (const auto *RT = ILE->getType()->getAs<RecordType>()) {
            unsigned I = 0;
            for (const FieldDecl *Field : RT->getDecl()->fields()) {
                if (I >= ILE->getNumInits())
                    break;
                if (ILE->getInit(I) == Conv)
                    return Field->getType();
                ++I;
            }
            return QualType();
        }
        if (const auto *AT = ILE->getType()->getAsArrayTypeUnsafe()) {
            for (unsigned I = 0, N = ILE->getNumInits(); I < N; ++I)
                if (ILE->getInit(I) == Conv)
                    return AT->getElementType();
        }
        return QualType();
    }

    // Append every governed annotation carried by a typedef in the sugar chain
    // of `T`, paired with the typedef that declares it. Walking the chain (not
    // just the outermost typedef) handles annotations layered across nested
    // typedefs.
    static void collectTypedefAnnotations(
        QualType T, ASTContext &Ctx,
        llvm::SmallVectorImpl<std::pair<std::string, const TypedefNameDecl *>>
            &Out) {
        while (!T.isNull()) {
            const Type *TP = T.getTypePtr();
            if (const auto *TT = dyn_cast<TypedefType>(TP)) {
                const TypedefNameDecl *TD = TT->getDecl();
                for (const auto *A : TD->attrs()) {
                    std::string Key = attrKey(A);
                    if (StringRef(Key).starts_with("annotate:"))
                        Out.emplace_back(std::move(Key), TD);
                }
            }
            QualType Next = T.getSingleStepDesugaredType(Ctx);
            if (Next.getTypePtr() == TP)
                break;
            T = Next;
        }
    }

    // True if the function provides the annotation identified by `Key` (an
    // attrKey(), e.g. "annotate:julia_not_safepoint"), either by carrying it on
    // some declaration or -- for julia_not_safepoint -- by being a function the
    // GC analyzer already treats as a non-safepoint without an explicit
    // annotation. The exemptions mirror GCChecker so that conversions the
    // analyzer considers sound are not flagged here.
    static bool functionProvidesAnnotation(const FunctionDecl *FD,
                                           StringRef Key, SourceManager &SM) {
        for (const FunctionDecl *R : FD->redecls())
            if (hasAttrLike(R, Key))
                return true;
        if (Key == "annotate:julia_not_safepoint")
            return analyzerTreatsAsNotSafepoint(FD, SM);
        return false;
    }

    // Mirror of GCChecker's reasoning for callees that are non-safepoints
    // without an explicit JL_NOTSAFEPOINT: external declarations we cannot
    // annotate (system or `llvm-*` headers, the `llvm`/`std`/`tp` namespaces),
    // compiler builtins, and the `uv_`/`unw_`/`_U` runtime helpers.
    static bool analyzerTreatsAsNotSafepoint(const FunctionDecl *FD,
                                             SourceManager &SM) {
        const FunctionDecl *First = FD->getFirstDecl();
        if (SM.isInSystemHeader(First->getLocation()))
            return true;
        if (jl_clangsa::isInLLVMHeaderFile(First->getLocation(), SM))
            return true;
        if (jl_clangsa::isInNonSafepointNamespace(FD->getDeclContext()))
            return true;
        if (FD->getBuiltinID() != 0)
            return true;
        StringRef Name = FD->getDeclName().isIdentifier() ? FD->getName() : "";
        return jl_clangsa::nameIsNonSafepointRuntimeHelper(Name);
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

    // The later of two locations in the translation unit (ignoring invalid
    // ones), used to find the last token of a function's declarator suffix.
    static SourceLocation later(SourceLocation A, SourceLocation B,
                                SourceManager &SM) {
        if (A.isInvalid())
            return B;
        if (B.isInvalid())
            return A;
        return SM.isBeforeInTranslationUnit(A, B) ? B : A;
    }

    // The start of the last token of the function declarator's suffix -- the
    // parameter list's `)` plus any trailing qualifiers that a function-level
    // annotation must follow: cv-/ref-qualifiers and the exception specification
    // (carried by the FunctionTypeLoc), and the `override`/`final` virt
    // specifiers (carried as attributes). A member function `f() const` thus
    // gets its annotation after `const` (`f() const JL_NOTSAFEPOINT`) rather
    // than the ill-formed `f() JL_NOTSAFEPOINT const`. Reading this from the AST
    // avoids re-lexing the source, which could be confused by macros.
    static SourceLocation declaratorSuffixEnd(const FunctionDecl *FD,
                                              SourceManager &SM) {
        TypeSourceInfo *TSI = FD->getTypeSourceInfo();
        if (!TSI)
            return SourceLocation();
        auto FTL = TSI->getTypeLoc().IgnoreParens().getAs<FunctionTypeLoc>();
        if (!FTL)
            return SourceLocation();
        SourceLocation End = later(FTL.getRParenLoc(), FTL.getLocalRangeEnd(), SM);
        if (auto FPTL = FTL.getAs<FunctionProtoTypeLoc>())
            End = later(End, FPTL.getExceptionSpecRange().getEnd(), SM);
        if (const auto *A = FD->getAttr<OverrideAttr>())
            End = later(End, A->getLocation(), SM);
        if (const auto *A = FD->getAttr<FinalAttr>())
            End = later(End, A->getLocation(), SM);
        return End;
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
