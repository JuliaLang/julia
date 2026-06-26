// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "GCChecker.h"

#include "clang/StaticAnalyzer/Frontend/CheckerRegistry.h"

namespace clang {
namespace ento {
void registerGCChecker(CheckerManager &mgr) {
  mgr.registerChecker<jl_gc_checker::GCChecker>();
}
} // namespace ento
} // namespace clang

#ifdef CLANG_PLUGIN
extern "C" const char clang_analyzerAPIVersionString[] =
    CLANG_ANALYZER_API_VERSION_STRING;
extern "C" void clang_registerCheckers(clang::ento::CheckerRegistry &registry) {
  registry.addChecker<jl_gc_checker::GCChecker>(
      "julia.GCChecker", "Validates julia gc invariants",
      "https://docs.julialang.org/en/v1/devdocs/gc-sa/"
  );
}
#endif
