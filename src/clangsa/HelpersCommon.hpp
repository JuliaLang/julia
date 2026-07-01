// This file is a part of Julia. License is MIT: https://julialang.org/license

// HelpersCommon: small predicates shared between the Julia clang plugins.
//
// This is deliberately a `.hpp` of `inline` definitions rather than a library:
// each plugin (libGCCheckerPlugin, libFirstDeclAnnotationsPlugin, ...) is built
// and loaded separately, so there is no common object file to link against.
// Including the definitions here compiles a private copy into each plugin.

#ifndef JULIA_CLANGSA_HELPERSCOMMON_HPP
#define JULIA_CLANGSA_HELPERSCOMMON_HPP

#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclCXX.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Path.h"

namespace jl_clangsa {
using namespace clang;

// True if any enclosing namespace of `DC` is one the analyzer treats as never
// reaching a safepoint: `llvm`, `std`, or `tp`. A null `DC` (e.g. a decl with
// no resolved context) is simply not in such a namespace.
inline bool isInNonSafepointNamespace(const DeclContext *DC) {
  for (; DC; DC = DC->getParent())
    if (const auto *NS = dyn_cast<NamespaceDecl>(DC))
      if (NS->getName() == "llvm" || NS->getName() == "std" ||
          NS->getName() == "tp")
        return true;
  return false;
}

// True if `Loc` is in a file named like an upstream LLVM header (`llvm-*`),
// which the analyzer treats as external and therefore never a safepoint.
inline bool isInLLVMHeaderFile(SourceLocation Loc, const SourceManager &SM) {
  StringRef Name = llvm::sys::path::filename(SM.getFilename(Loc));
  return Name.starts_with("llvm-");
}

// True if `Name` is one of the `uv_`/`unw_`/`_U` runtime helpers the analyzer
// treats as a non-safepoint. `uv_run` is the sole exception: it drives the
// event loop and can reach arbitrary safepoints.
inline bool nameIsNonSafepointRuntimeHelper(StringRef Name) {
  return (Name.starts_with("uv_") || Name.starts_with("unw_") ||
          Name.starts_with("_U")) &&
         Name != "uv_run";
}

} // namespace jl_clangsa

#endif
