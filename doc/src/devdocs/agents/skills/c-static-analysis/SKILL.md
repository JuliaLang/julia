---
name: c-static-analysis
description: Run Clang static analysis on Julia's C/C++ runtime and codegen, and satisfy the GC-rooting checker. Use after modifying runtime/codegen .c/.cpp files under src/ (excluding headers), before opening a PR.
---

# C/C++ static analysis (src/)

Use this after you modify a runtime/codegen C/C++ source file under `src/` (not
headers). Note that runtime changes also require a rebuild (`make -j`).

Run static analysis checks:

- First run `make -C src install-analysis-deps` to initialize dependencies (only
  needed once the first time, or after the LLVM/Clang toolchain dependencies
  change). This may download and install LLVM/Clang artifacts.
- For a source file such as `src/jloptions.c` or `src/codegen.cpp`, use the file
  stem without the `.c`/`.cpp` extension:
  ```sh
  make -C src analyze-<file-stem> -j8 [--output-sync]
  ```
  For example, to analyze `src/jloptions.c`, run:
  ```sh
  make -C src analyze-jloptions -j8
  ```
  Add `--output-sync` when your `make` supports it to keep parallel output
  grouped; otherwise omit it.
- Checks can also be rerun individually with `clang-sa-<file-stem>`,
  `clang-sagc-<file-stem>` or `clang-tidy-<file-stem>`.

## Fixing the GC-rooting checker (clang-sagc)

If `clang-sagc-<file-stem>` fails, it may require adding `JL_GC_PUSH` statements,
or `JL_GC_PROMISE_ROOTED` statements, or require fixing locks.

- Remember arguments are assumed rooted, so check the callers to make sure that
  is handled.
- If the value is being temporarily moved around in a struct or arraylist,
  `JL_GC_PROMISE_ROOTED(struct->field)` may be needed as a statement (it returns
  void) immediately after reloading the struct before any use of struct.
- Put that promise as early in the code as is legal, near the definition not the
  use.
