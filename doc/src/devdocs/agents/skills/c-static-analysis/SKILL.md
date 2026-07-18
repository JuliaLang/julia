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

If `clang-sagc-<file-stem>` fails, first look for fixes that establish real
rooting, such as adding appropriate `JL_GC_PUSH`/`JL_GC_POP` scopes, or for
lock/control-flow fixes.

Do not add `JL_GC_PROMISE_ROOTED` without explicit user or maintainer
confirmation. `JL_GC_PROMISE_ROOTED` asserts that a value is already rooted; it
does not root the value. If it appears necessary, stop and ask for confirmation,
showing the exact expression, the existing root that makes it safe, and the
safepoints considered.

- Remember arguments are assumed rooted, so check the callers to make sure that
  is handled.
- As a diagnostic hint when asking for confirmation: if the value is temporarily
  moved through a struct or arraylist and then reloaded, the promised expression
  may need to refer to the reloaded field, such as
  `JL_GC_PROMISE_ROOTED(struct->field)`, immediately after the reload and before
  any use of that field.
- If confirmed, put the promise as early in the code as is legal, near the
  definition or reload rather than the use.
