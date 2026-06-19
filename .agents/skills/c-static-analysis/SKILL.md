---
name: c-static-analysis
description: Run Clang static analysis on Julia's C/C++ runtime and codegen, and satisfy the GC-rooting checker. Use after modifying any .c/.cpp file under src/ (excluding headers), before opening a PR.
---

# C/C++ static analysis (src/)

Use this after you modify a C/C++ source file under `src/` (not headers). Note
that runtime changes also require a rebuild (`make -j`).

Run static analysis checks:

- First run `make -C src install-analysis-deps` to initialize dependencies (only needed once the first time).
- Run `make -C src analyze-<filename> --output-sync -j8` (replace `<filename>` with the basename of any C or C++ file you modified, excluding headers).
- Tests can also be rerun individually with `clang-sa-<filename>`, `clang-sagc-<filename>` or `clang-tidy-<filename>`.

## Fixing the GC-rooting checker (clang-sagc)

If `clang-sagc-<filename>` fails, it may require adding `JL_GC_PUSH` statements,
or `JL_GC_PROMISE_ROOTED` statements, or require fixing locks.

- Remember arguments are assumed rooted, so check the callers to make sure that
  is handled.
- If the value is being temporarily moved around in a struct or arraylist,
  `JL_GC_PROMISE_ROOTED(struct->field)` may be needed as a statement (it returns
  void) immediately after reloading the struct before any use of struct.
- Put that promise as early in the code as is legal, near the definition not the
  use.
