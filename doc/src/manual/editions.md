# [Editions](@id editions)

Julia's edition mechanism, inspired by the [Rust edition
mechanism](https://doc.rust-lang.org/edition-guide), enables improvements to the
Julia language while maintaining backwards-compatibility for existing code.  In
short, you may specify an edition for any piece of Julia code you write (usually
at the project level), so even if a new edition changes something, the code's
behaviour won't change until you choose to update its edition.

This mechanism is still under development as of 1.14:
- You may hear the term "syntax versions" being used to refer to editions.  This
  is because the original implementation was limited to parser evolution.
  Both terms refer to the same thing.
- Unlike Rust, there's no automated tooling for migration yet (fortunately,
  there is also a lack of migrations to make).
- The points on this page about editions stored in the AST, or being inspected
  by macros, requires hygienic macros, which are not present in edition 1.14.
  Reading these is still useful for understanding the goals of the system.

## What do edition numbers mean?

Each edition is named after the first Julia minor version it's supported in
(which is unique per edition), with the exception of `EXPERIMENTAL_EDITION`.
The same isn't true in reverse; not every new minor version will necessarily
support a new edition.  Julia version <=1.13 only supports edition 1.13, v1.14
only supports editions 1.13 and 1.14, v1.15 supports editions 1.13-1.15, and so
on.

### Unstable editions

Since an edition is only considered stable once its Julia version is released,
features may appear and disappear if, for example, you use regularly use edition
1.15 on a nightly build with `VERSION == v"1.15.0-DEV"`.

`EXPERIMENTAL_EDITION` is always unstable, is defined by whatever copy of Julia
is running, and shouldn't be used outside of developing Julia itself.

## Specifying edition

### In projects
Projects may specify an edition for the entire project's code.

The edition of a package is determined by the loading mechanism in the following
order of precedence:

1. If a `syntax.julia_version` field is present in the project file, it is used
   directly:
   ```toml
   name = "MyPackage"
   uuid = "..."
   syntax.julia_version = "1.14" # edition 1.14
   ```

2. Otherwise, if a `[compat]` section specifies a Julia version constraint, the
   minimum compatible version is used:
   ```toml
   [compat]
   julia = "1.13 - 2"  # implies edition 1.13
   ```

3. Otherwise, the oldest edition is used.

### In scripts and the REPL

Scripts and the REPL also default to the active project's edition, but fall back
to the newest edition instead of the oldest.  This determination happens:

1. At startup after processing `--project`
2. Before parsing any REPL input, once for each prompt

In particular, a manual `set_active_project` in a script will not change the
edition used for the rest of the script. However, doing so at the REPL
(explicitly or implicitly via the Pkg REPL mode) will affect the edition used to
parse the *next* REPL input.

`Core.set_edition(Main, v"1.234")` can be used in interactive settings.

## Macros

A Julia program may have a different edition than its dependencies.  Usually
it's simple to tell what code belongs to what project, but when macros can mix
caller and macro-provided syntax arbitrarily, "the edition of some code" needs a
more precise definition.

The edition of code is known by parse time and attached to each AST node.  A
node might have a different meaning in another edition, so it's unsupported to
blindly change the edition of parsed code, but this can be done correctly for
specific node kinds and edition numbers.  However, this allows macros to
correctly read the edition of any syntax they're given.

If you call a macro with a different edition than that of the macrocall, you are
responsible for ensuring the syntax passed to it is acceptable (possibly by
transforming it to equivalent syntax in an older edition).  Macro writers should
also make an effort to reject unsupported syntax.

## List of julia editions

### 1.13

This is the oldest edition, and is the only one supported by Julia versions 1.13
or lower.

1.13 is the default edition in projects that don't specify one.

### 1.14

The only changes in edition 1.14 are parsing changes.

- The `typegroup` form is now supported
- Labeled and value-carrying `break` and `continue` are now supported
- Wrapping arithmetic operators (e.g. `+%`) are now supported
- Space-separated macrocall arguments may now span multiple lines when the
  macrocall is wrapped with parentheses
