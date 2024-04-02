# JuliaLowering

[![Build Status](https://github.com/c42f/JuliaLowering.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/c42f/JuliaLowering.jl/actions/workflows/CI.yml?query=branch%3Amain)

Experimental port of Julia's code "lowering" compiler passes into Julia.

Lowering comprises four symbolic simplification steps
* Syntax desugaring - simplifying the rich surface syntax down to a small
  number of forms.
* Scope analysis - analyzing identifier names used in the code to discover
  local variables, closure captures, and associate global variables to the
  appropriate module.
* Closure conversion - convert closures to types and deal with captured
  variables efficiently where possible.
* Flattening to linear IR - convert code in hierarchical tree form to a
  flat array of statements and control flow into gotos.

## Goals

This work is intended to
* Bring precise code provenance to Julia's lowered form (and eventually
  downstream in type inference, stack traces, etc). This has many benefits
    - Talk to users precisely about their code via character-precise error and
      diagnostic messages from lowering 
    - Greatly simplify the implementation of critical tools like Revise.jl
      which rely on analyzing how the user's source maps to the compiler's data
      structures
    - Allow tools like JuliaInterpreter to use type-inferred and optimized
      code, with the potential for huge speed improvements.
* Bring improvements for macro authors
    - Prototype "automatic hygiene" (no more need for `esc()`!)
    - Precise author-defined error reporting from macros
    - Sketch better interfaces for syntax trees (hopefully!)

# Design Notes

A disorganized collection of design notes :)

## Syntax trees

Want something something better than `JuliaSyntax.SyntaxNode`! `SyntaxTree` and
`SyntaxGraph` provide this. (These will probably end up in `JuliaSyntax`.)

We want to allow arbitrary attributes to be attached to tree nodes by analysis
passes. This separates the analysis pass implementation from the data
structure, allowing passes which don't know about each other to act on a shared
data structure.

Design and implementation inspiration comes in several analogies:

Analogy 1: the ECS (Entity-Component-System) pattern for computer game design.
This pattern is highly successful because it separates game logic (systems)
from game objects (entities) by providing flexible storage
* Compiler passes are "systems"
* AST tree nodes are "entities"
* Node attributes are "components"

Analogy 2: The AoS to SoA transformation. But here we've got a kind of
tree-of-structs-with-optional-attributes to struct-of-Dicts transformation.
The data alignment / packing efficiency and concrete type safe storage benefits
are similar.

Analogy 3: Graph algorithms which represent graphs as a compact array of node
ids and edges with integer indices, rather than using a linked data structure.

## Julia's existing lowering implementation

### How does macro expansion work?

`macroexpand(m::Module, x)` calls `jl_macroexpand` in ast.c:

```
jl_value_t *jl_macroexpand(jl_value_t *expr, jl_module_t *inmodule)
{
    expr = jl_copy_ast(expr);
    expr = jl_expand_macros(expr, inmodule, NULL, 0, jl_world_counter, 0);
    expr = jl_call_scm_on_ast("jl-expand-macroscope", expr, inmodule);
    return expr;
}
```

First we copy the AST here. This is mostly a trivial deep copy of `Expr`s and
shallow copy of their non-`Expr` children, except for when they contain
embedded `CodeInfo/phi/phic` nodes which are also deep copied.

Second we expand macros recursively by calling 

`jl_expand_macros(expr, inmodule, macroctx, onelevel, world, throw_load_error)`

This relies on state indexed by `inmodule` and `world`, which gives it some
funny properties:
* `module` expressions can't be expanded: macro expansion depends on macro
  lookup within the module, but we can't do that without `eval`.

Expansion proceeds from the outermost to innermost macros. So macros see any
macro calls or quasiquote (`quote/$`) in their children as unexpanded forms.

Things which are expanded:
* `quote` is expanded using flisp code in `julia-bq-macro`
  - symbol / ssavalue -> `QuoteNode` (inert)
  - atom -> itself
  - at depth zero, `$` expands to its content
  - Expressions `x` without `$` expand to `(copyast (inert x))`
  - Other expressions containing a `$` expand to a call to `_expr` with all the
    args mapped through `julia-bq-expand-`. Roughly!
  - Special handling exists for multi-splatting arguments as in `quote quote $$(x...) end end`
* `macrocall` proceeds with
  - Expand with `jl_invoke_julia_macro`
    - Call `eval` on the macro name (!!) to get the macro function. Look up
      the method.
    - Set up arguments for the macro calling convention 
    - Wraps errors in macro invocation in `LoadError`
    - Returns the expression, as well as the module at
      which that method of that macro was defined and `LineNumberNode` where
      the macro was invoked in the source.
  - Deep copy the AST
  - Recursively expand child macros in the context of the module where the
    macrocall method was defined
  - Wrap the result in `(hygienic-scope ,result ,newctx.m ,lineinfo)` (except
    for special case optimizations)
* `hygenic-scope` expands `args[1]` with `jl_expand_macros`, with the module
  of expansion set to `args[2]`.  Ie, it's the `Expr` representation of the
  module and expression arguments to `macroexpand`. The way this returns
  either `hygenic-scope` or unwraps is a bit confusing.
* "`do` macrocalls" have their own special handling because the macrocall is
  the child of the `do`. This seems like a mess!!


### Scope resolution

Scopes are documented in the Juila documentation on [Scope of Variables](https://docs.julialang.org/en/v1/manual/variables-and-scoping/)

This pass disambiguates variables which have the same name in different scopes
and fills in the list of local variables within each lambda.

#### Which data is needed to define a scope?

As scope is a collection of variable names by category:
* `argument` - arguments to a lambda
* `local` - variables declared local (at top level) or implicitly local (in lambdas) or desugared to local-def
* `global` - variables declared global (in lambdas) or implicitly global (at top level)
* `static-parameter` - lambda type arguments from `where` clauses

#### How does scope resolution work?

We traverse the AST starting at the root paying attention to certian nodes:
* Nodes representing identifiers (Identifier, operators, var)
    - If a variable exists in the table, it's *replaced* with the value in the table.
    - If it doesn't exist, it becomes an `outerref`
* Variable scoping constructs: `local`, `local-def`
    - collected by scope-block
    - removed during traversal
* Scope metadata `softscope`, `hardscope` - just removed
* New scopes
    - `lambda` creates a new scope containing itself and its arguments,
      otherwise copying the parent scope. It resolves the body with that new scope.
    - `scope-block` is really complicated - see below
* Scope queries `islocal`, `locals`
    - `islocal` - statically expand to true/false based on whether var name is a local var
    - `locals` - return list of locals - see `@locals`
    - `require-existing-local` - somewhat like `islocal`, but allows globals
      too (whaa?! naming) and produces a lowering error immediately if variable
      is not known.  Should be called `require-in-scope` ??
* `break-block`, `symbolicgoto`, `symboliclabel` need special handling because
  one of their arguments is a non-quoted symbol.
* Add static parameters for generated functions `with-static-parameters`
* `method` - special handling for static params

`scope-block` is the complicated bit. It's processed by
* Searching the expressions within the block for any `local`, `local-def`,
  `global` and assigned vars. Searching doesn't recurse into `lambda`,
  `scope-block`, `module` and `toplevel`
* Building lists of implicit locals or globals (depending on whether we're in a
  top level thunk)
* Figuring out which local variables need to be renamed. This is any local variable
  with a name which has already occurred in processing one of the previous scope blocks
* Check any conflicting local/global decls and soft/hard scope
* Build new scope with table of renames
* Resolve the body with the new scope, applying the renames

### Intermediate forms used in lowering

* `local-def` - flisp code explains this as
  - "a local that we know has an assignment that dominates all usages"
  - "local declaration of a defined variable"

There's also this comment in https://github.com/JuliaLang/julia/issues/22314:

> mark the [...] variable as local-def, which would prevent it from getting Core.Boxed during the closure conversion it'll be detected as known-SSA

But maybe that's confusing. It seems like `local-def` is a local which lowering
asserts is "always defined" / "definitely initialized before use". But it's not
necessarily single-assign, so not SSA.

### Lowered IR

See https://docs.julialang.org/en/v1/devdocs/ast/#Lowered-form

#### CodeInfo

```julia
mutable struct CodeInfo
    code::Vector{Any}             # IR statements
    codelocs::Vector{Int32}       # `length(code)` Vector of indices into `linetable`
    ssavaluetypes::Any            # `length(code)` or Vector of inferred types after opt
    ssaflags::Vector{UInt32}      # flag for every statement in `code`
                                  #   0 if meta statement
                                  #   inbounds_flag - 1 bit (LSB)
                                  #   inline_flag   - 1 bit
                                  #   noinline_flag - 1 bit
                                  #   ... other 8 flags which are defined in compiler/optimize.jl
                                  #   effects_flags - 9 bits
    method_for_inference_limit_heuristics::Any
    linetable::Any
    slotnames::Vector{Symbol}     # names of parameters and local vars used in the code
    slotflags::Vector{UInt8}      # vinfo flags from flisp
    slottypes::Any                # nothing (used by typeinf)
    rettype::Any                  # Any (used by typeinf)
    parent::Any                   # nothing (used by typeinf)
    edges::Any
    min_world::UInt64
    max_world::UInt64
    inferred::Bool
    propagate_inbounds::Bool
    has_fcall::Bool
    nospecializeinfer::Bool
    inlining::UInt8
    constprop::UInt8
    purity::UInt16
    inlining_cost::UInt16
end
```

