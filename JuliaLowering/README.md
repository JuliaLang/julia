# JuliaLowering

[![Build Status](https://github.com/c42f/JuliaLowering.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/c42f/JuliaLowering.jl/actions/workflows/CI.yml?query=branch%3Amain)

JuliaLowering.jl is an experimental port of Julia's code lowering compiler
passes, written in Julia itself. "Code lowering" is the set of compiler passes
which *symbolically* transform and simplify Julia's syntax prior to type
inference.

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

## Trying it out

Note this is a work in progress; many types of syntax are not yet handled.

1. You need a 1.12-DEV build of Julia: At least 1.12.0-DEV.512. Commit `263928f9ad4` is currentl known to work. Note that JuliaLowering relies on Julia internals and may be broken on the latest Julia dev version from time to time. (In fact it is currently broken on the latest `1.12-DEV`.)
2. Check out the main branch of [JuliaSyntax](https://github.com/JuliaLang/JuliaSyntax.jl)
3. Get the latest version of [JuliaSyntaxFormatter](https://github.com/c42f/JuliaSyntaxFormatter.jl)
4. Run the demo `include("test/demo.jl")`

# Design notes

## Syntax trees

Want something something better than `JuliaSyntax.SyntaxNode`! `SyntaxTree` and
`SyntaxGraph` provide this. Some future version of these should end up in
`JuliaSyntax`.

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

### References

Sander Mertens, the author of the Flecs ECS has a blog post series discussing
ECS data structures and the many things that may be done with them. We may want
to use some of these tricks to make `SyntaxTree` faster, eventually. See, for
example,
[Building Games in ECS with Entity Relationships](https://ajmmertens.medium.com/building-games-in-ecs-with-entity-relationships-657275ba2c6c)

### Structural assertions / checking validity of syntax trees

Syntax trees in Julia `Expr` form are very close to lisp lists: a symbol at the
`head` of the list which specifies the syntactic form, and a sequence of
children in the syntax tree. This is a representation which `JuliaSyntax` and
`JuliaLowering` follow but it does come with certain disadvantages. One of the
most problematic is that the number of children affects the validity (and
sometimes semantics) of an AST node, as much as the `head` symbol does.

In `JuliaSyntax` we've greatly reduced the overloading of `head` in order to
simplify the interpretation of child structures in the tree. For example,
broadcast calls like `f.(x,y)` use the `K"dotcall"` kind rather than being a
node with `head == Symbol(".")` and a tuple as children.

However, there's still many ways for lowering to encounter invalid expressions
of type `SyntaxTree` and these must be checked. In JuliaSyntax we have several
levels of effort corresponding to the type of errors conditions we desire to
check and report:

* For invalid syntax which is accepted by the `JuliaSyntax`
  parser but is invalid in lowering we use manual `if` blocks followed by
  throwing a `LoweringError`. This is more programming effort but allows for
  the highest quality error messages for the typical end user.
* For invalid syntax which can only be produced by macros (ie, not by the
  parser) we mostly use the `@chk` macro. This is a quick tool for validating
  input but gives lesser quality error messages.
* For JuliaLowering's internal invariants we just use `@assert` - these should
  never be hit and can be compiled out in principle.

## Provenance tracking

Expression provenance is tracked through lowering by attaching provenance
information in the `source` attribute to every expression as it is generated.
For example when parsing a source file we have

```julia
julia> ex = parsestmt(SyntaxTree, "a + b", filename="foo.jl")
SyntaxTree with attributes kind,value,name_val,syntax_flags,source
[call-i]                                │ 
  a                                     │ 
  +                                     │ 
  b                                     │ 

julia> ex[3].source
a + b
#   ╙ ── these are the bytes you're looking for 😊
```

The `provenance` function should be used to look up the `source` attribute and
the `showprov` function used to inspect the content (this is preferred because
the encoding of `source` is an implementation detail). For example:

```julia
julia> showprov(ex[3])
a + b
#   ╙ ── in source
# @ foo.jl:1
```

During macro expansion and lowering provenance gets more complicated because an
expression can arise from multiple sources. For example, we want to keep track
of the entire stack of macro expansions an expression was generated by, while
also recording where it occurred in the original source file.

For this, we use a tree data structure. Let's look at the following pair of
macros

```julia
julia> JuliaLowering.include_string(Main, raw"""
       module M
           macro inner()
               :(2)
           end

           macro outer()
               :((1, @inner))
           end
       end
       """, "some_macros.jl")
```

The tree which arises from macro expanding this is pretty simple:

```julia
julia> expanded = JuliaLowering.macroexpand(Main, parsestmt(SyntaxTree, "M.@outer()"))
SyntaxTree with attributes scope_layer,kind,value,var_id,name_val,syntax_flags,source
[tuple-p]                               │ 
  1                                     │ 
  2                                     │ 
```

but the provenance information recorded for the second element `2` of this
tuple is not trivial; it includes the macro call expressions for `@inner` and
`@outer`. We can show this in tree form:

```julia
julia> showprov(expanded[2], tree=true)
2
├─ 2
│  └─ @ some_macros.jl:3
└─ (macrocall @inner)
   ├─ (macrocall @inner)
   │  └─ @ some_macros.jl:7
   └─ (macrocall-p (. M @outer))
      └─ @ foo.jl:1
```

or as a more human readable flattened list highlighting of source ranges:

```julia
module M
    macro inner()
        :(2)
#         ╙ ── in source
    end

# @ some_macros.jl:3


    macro outer()
        :((1, @inner))
#             └────┘ ── in macro expansion
    end
end
# @ some_macros.jl:7

M.@outer()
└────────┘ ── in macro expansion
# @ foo.jl:1
```

## Problems with Hygiene in Julia's exiting macro system

To write correct hygienic macros in Julia (as of 2024), macro authors must use
`esc()` on any any syntax passed to the macro so that passed identifiers escape
to the macro caller scope. However

* This is not automatic and the correct use of `esc()` is one of the things
  that new macro authors find most confusing. (My impression, based on various
  people complaining about how confusing `esc()` is.)
* `esc()` wraps expressions in `Expr(:escape)`, but this doesn't work well when
  macros pass such escaped syntax to an inner macro call. As discussed in
  [Julia issue #37691](https://github.com/JuliaLang/julia/issues/37691), macros
  in Julia's existing system are not composable by default. Writing
  composable macros in the existing system would require preserving the escape
  nesting depth when recursing into any macro argument nested expressions.
  Almost no macro author knows how to do this and is prepared to pay for the
  complexity of getting it right.

The requirement to use `esc()` stems from Julia's pervasive use of the simple
`Expr` data structure which represents a unadorned AST in which names are plain
symbols. For example, a macro call `@foo x` gets passed the  symbol `:x`
which is just a name without any information attached to indicate that it came
from the scope where `@foo` was called.

### Hygiene References

* [Toward Fearless Macros](https://lambdaland.org/posts/2023-10-17_fearless_macros) -
  a blog post by Ashton Wiersdorf
* [Towards the Essence of Hygiene](https://michaeldadams.org/papers/hygiene/hygiene-2015-popl-authors-copy.pdf) - a paper by Michael Adams
* [Bindings as sets of scopes](https://www-old.cs.utah.edu/plt/scope-sets/) - a description of Racket's scope set mechanism by Matthew Flatt

# Overview of lowering passes

JuliaLowering uses six symbolic transformation passes:

1. Macro expansion - expanding user-defined syntactic constructs by running the
   user's macros. This pass also includes a small amount of other symbolic
   simplification.
2. Syntax desugaring - simplifying Julia's rich surface syntax down to a small
   number of syntactic forms.
3. Scope analysis - analyzing identifier names used in the code to discover
   local variables, closure captures, and associate global variables to the
   appropriate module. Transform all names (kind `K"Identifier"`) into binding
   IDs (kind `K"BindingId"`) which can be looked up in a table of bindings.
4. Closure conversion - convert closures to types and deal with captured
   variables efficiently where possible.
5. Flattening to untyped IR - convert code in hierarchical tree form to a
   flat array of statements; convert control flow into gotos.
6. Convert untyped IR to `CodeInfo` form for integration with the Julia runtime.

## Pass 1: Macro expansion

This pass expands macros and quoted syntax, and does some very light conversion
of a few syntax `Kind`s in preparation for syntax desugaring.

### Hygiene in JuliaLowering

In JuliaLowering we make hygiene automatic and remove `esc()` by combining names
with scope information. In the language of the paper [*Towards the Essence of
Hygiene*](https://michaeldadams.org/papers/hygiene/hygiene-2015-popl-authors-copy.pdf)
by Michael Adams, this combination is called a "syntax object". In
JuliaLowering our representation is the tuple `(name,scope_layer)`, also called
`VarId` in the scope resolution pass.

JuliaLowering's macro expander attaches a unique *scope layer* to each
identifier in a piece of syntax. A "scope layer" is an integer identifer
combined with the module in which the syntax was created.

When expanding macros,

* Any identifiers passed to the macro are tagged with the scope layer they were
  defined within.
* A new unique scope layer is generated for the macro invocation, and any names
  in the syntax produced by the macro are tagged with this layer.

Subsequently, the `(name,scope_layer)` pairs are used when resolving bindings.
This ensures that, by default, we satisfy the basic rules for hygenic macros
discussed in Adams' paper:

1. A macro can't insert a binding that can capture references other than those
   inserted by the macro.
2. A macro can't insert a reference that can be captured by bindings other than
   those inserted by the macro.

TODO: Write more here...

## Pass 2: Syntax desugaring

This pass recursively converts many special surface syntax forms to a smaller
set of syntax `Kind`s, following the AST's hierarchical tree structure. Some
such as `K"scope_block"` are internal to lowering and removed during later
passes. See `kinds.jl` for a list of these internal forms.

This pass is implemented in `desugaring.jl`. It's quite large because Julia has
many special syntax features.

## Pass 3: Scope analysis / binding resolution

This pass replaces variables with bindings of kind `K"BindingId"`,
disambiguating variables when the same name is used in different scopes. It
also fills in the list of non-global bindings within each lambda and metadata
about such bindings as will be used later during closure conversion.

Scopes are documented in the Juila documentation on
[Scope of Variables](https://docs.julialang.org/en/v1/manual/variables-and-scoping/)

During scope resolution, we maintain a stack of `ScopeInfo` data structures.

When a new `lambda` or `scope_block` is discovered, we create a new `ScopeInfo` by
1. Find all identifiers bound or used within a scope. New *bindings* may be
   introduced by one of the `local`, `global` keywords, implicitly by
   assignment, as function arguments to a `lambda`, or as type arguments in a
   method ("static parameters"). Identifiers are *used* when they are
   referenced.
2. Infer which bindings are newly introduced local or global variables (and
   thus require a distinct identity from names already in the stack)
3. Assign a `BindingId` (unique integer) to each new binding

We then push this `ScopeInfo` onto the stack and traverse the expressions
within the scope translating each `K"Identifier"` into the associated
`K"BindingId"`. While we're doing this we also resolve some special forms like
`islocal` by making use of the scope stack.

The detailed rules for whether assignment introduces a new variable depend on
the `scope_block`'s `scope_type` attribute when we are processing top-level
code.
* `scope_type == :hard` (as for bindings inside a `let` block) means an
  assignment always introduces a new binding
* `scope_type == :neutral` - inherit soft or hard scope from the parent scope.
* `scope_type == :soft` - assignments are to globals if the variable
  exists in global module scope. Soft scope doesn't have surface syntax and is
  introduced for top-level code by REPL-like environments.

## Pass 4: Closure conversion / lower bindings

The main goal of this pass is closure conversion, but it's also used for
lowering typed bindings and global assignments. Roughly, this is passes 3 and 4
in the original `julia-syntax.scm`. In JuliaLowering it also comes in two steps:

The first step (part of `scope_resolution.jl`) is to compute metadata related
to bindings, both per-binding and per-binding-per-closure-scope.

Properties which are computed per-binding which can help with symbolic
optimizations include:
* Type is declared (`x::T` syntax in a statement): type conversions must be
  inserted at every assignment of `x`.
* Never undefined: value is always assigned to the binding before being read
  hence this binding doesn't require the use of `Core.NewvarNode`.
* Single assignment: (TODO how is this defined, what is it for and does it go
  here or below?)

Properties of non-globals which are computed per-binding-per-closure include:
* Read: the value of the binding is used.
* Write: the binding is asssigned to.
* Captured: Bindings defined outside the closure which are either Read or Write
  within the closure are "captured" and need to be one of the closure's fields.
* Called: the binding is called as a function, ie, `x()`. (TODO - what is this
  for?)

The second step uses this metadata to
* Convert closures into `struct` types
* Lower bindings captured by closures into references to boxes as necessary
* Deal with typed bindings (`K"decl"`) and their assignments
* Lower const and non-const global assignments
* TODO: probably more here.


### Q&A

#### When does `function` introduce a closure?

Closures are just functions where the name of the function is *local* in scope.
How does the function name become a local? The `function` keyword acts like an
assignment to the function name for the purposes of scope resolution. Thus
`function f() body end` is rather like `f = ()->body` and may result in the
symbol `f` being either `local` or `global`. Like other assignments, `f` may be
declared global or local explicitly, but if not `f` is subject to the usual
rules for assignments inside scopes. For example, inside a `let` scope
`function f() ...` would result in the symbol `f` being local.

Examples:

```julia
begin
    # f is global because `begin ... end` does not introduce a scope
    function f()
        body
    end

    # g is a closure because `g` is explicitly declared local
    local g
    function g()
        body
    end
end

let
    # f is local so this is a closure becuase `let ... end` introduces a scope
    function f()
        body
    end

    # g is not a closure because `g` is declared global
    global g
    function g()
        body
    end
end
```

#### How do captures work with non-closures?

Yes it's true, you can capture local variables into global methods. For example:

```julia
begin
    local x = 1
    function f(y)
        x + y
    end
    x = 2
end
```

The way this works is to put `x` in a `Box` and interpolate it into the AST of
`f` (the `Box` can be eliminated in some cases, but not here). Essentially this
lowers to code which is almost-equivalent to the following:

```julia
begin
    local x = Core.Box(1)
    @eval function f(y)
        $(x.contents) + y
    end
    x.contents = 2
end
```

#### How do captures work with closures with multiple methods?

Sometimes you might want a closure with multiple methods, but those methods
might capture different local variables. For example,

```julia
let
    x = 1
    y = 1.5
    function f(xx::Int)
        xx + x
    end
    function f(yy::Float64)
        yy + y
    end

    f(42)
end
```

In this case, the closure type must capture both `x` and `y` and the generated
code looks rather like this:

```julia
struct TheClosureType
    x
    y
end

let
    x = 1
    y = 1.5
    f = TheClosureType(x,y)
    function (self::TheClosureType)(xx::Int)
        xx + self.x
    end
    function (self::TheClosureType)(yy::Int)
        yy + self.y
    end

    f(42)
end
```

#### When are `method` defs lifted to top level?

Closure method definitions must be lifted to top level whenever the definitions
appear inside a function. This is allow efficient compilation and avoid world
age issues.

Conversely, when method defs appear in top level code, they are executed
inline.

## Pass 5: Convert to untyped IR

This pass is implemented in `linear_ir.jl`.

### Untyped IR (JuliaLowering form)

JuliaLowering's untyped IR is very close to the runtime's `CodeInfo` form (see
below), but is more concretely typed as `JuliaLowering.SyntaxTree`.

Metadata is generally represented differently:
* The statements retain full code provenance information as `SyntaxTree`
  objects. See `kinds.jl` for a list of which `Kind`s occur in the output IR
  but not in surface syntax.
* The list of slots is `Vector{Slot}`, including `@nospecialize` metadata

### Lowering of exception handlers

Exception handling involves a careful interplay between lowering and the Julia
runtime. The forms `enter`, `leave` and `pop_exception` dynamically modify the
exception-related state on the `Task`; lowering and the runtime work together
to maintain correct invariants for this state.

Lowering of exception handling must ensure that

* Each `enter` is matched with a `leave` on every possible non-exceptional
  program path (including implicit returns generated in tail position).
* Each `catch` block which is entered and handles the exception - by exiting
  via a non-exceptional program path - is matched with a `pop_exception`
* Each `finally` block runs, regardless of the way it's entered - either by
  normal program flow, an exception, early `return` or a jump out of an inner
  context via `break`/`continue`/`goto` etc.

The following special forms are emitted into the IR:

* `(= tok (enter catch_label dynscope))` -
  push exception handler with catch block at `catch_label` and dynamic
  scope `dynscope`, yielding a token which is used by `leave` and
  `pop_exception`. `dynscope` is only used in the special `tryfinally` form
  without associated source level syntax (see the `@with` macro)
* `(leave tok)` -
    pop exception handler back to the state of the `tok` from the associated
    `enter`. Multiple tokens can be supplied to pop multiple handlers using
    `(leave tok1 tok2 ...)`.
* `(pop_exception tok)` - pop exception stack back to state of associated enter

When an `enter` is encountered, the runtime pushes a new handler onto the
`Task`'s exception handler stack which will jump to `catch_label` when an
exception occurs.

There are two ways that the exception-related task state can be restored

1. By encountering a `leave` which will restore the handler state with `tok`.
2. By throwing an exception. In this case the runtime will pop one handler
   automatically and jump to the catch label with the new exception pushed
   onto the exception stack. On this path the exception stack state must be
   restored back to the associated `enter` by encountering `pop_exception`.

Note that the handler and exception stack represent two distinct types of
exception-related state restoration which need to happen. Note also that the
"handler state restoration" actually includes several pieces of runtime state
including GC flags - see `jl_eh_restore_state` in the runtime for that.

#### Lowering finally code paths

When lowering `finally` blocks we want to emit the user's finally code once but
multiple code paths may traverse the finally block. For example, consider the
code

```julia
function foo(x)
    while true
        try
            if x == 1
                return f(x)
            elseif x == 2
                g(x)
                continue
            else
                break
            end
        finally
            h()
        end
    end
end
```

In this situation there's four distinct code paths through the finally block:
1. `return f(x)` needs to call `val = f(x)`, leave the `try` block, run `h()` then
   return `val`.
2. `continue` needs to call `h()` then jump to the start of the while loop
3. `break` needs to call `h()` then jump to the exit of the while loop
4. If an exception occurs in `f(x)` or `g(x)`, we need to call `h()` before
   falling back into the while loop.

To deal with these we create a `finally_tag` variable to dynamically track
which action to take after the finally block exits. Before jumping to the block
we set this variable to a unique integer tag identifying the incoming code
path. At the exit of the user's code (`h()` in this case) we perform the jump
appropriate to the `break`, `continue` or `return` as necessary based on the tag.

(TODO - these are the only four cases which can occur, but, for example,
multiple `return`s create multiple tags rather than assigning to a single
variable. Collapsing these into a single case might be worth considering? But
also might be worse for type inference in some cases?)

## Pass 6: Convert IR to `CodeInfo` representation

This pass convert's JuliaLowering's internal representation of untyped IR into
a form the Julia runtime understands. This is a necessary decoupling which
separates the development of JuliaLowering.jl from the evolution of the Julia
runtime itself.

### Untyped IR (`CodeInfo` form)

The final lowered IR is expressed as `CodeInfo` objects which are a sequence of
`code` statments containing
* Literals
* Restricted forms of `Expr` (with semantics different from surface syntax,
  even for the same `head`! for example the arguments to `Expr(:call)` in IR
  must be "simple" and aren't evaluated in order)
* `Core.SlotNumber` 
* Other special forms from `Core` like `Core.ReturnNode`, `Core.EnterNode`, etc.
* `Core.SSAValue`, indexing any value generated from a statement in the `code`
  array.
* Etc (todo)

The IR obeys certain invariants which are checked by the downstream code in
base/compiler/validation.jl.

See also https://docs.julialang.org/en/v1/devdocs/ast/#Lowered-form

CodeInfo layout (as of early 1.12-DEV):

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

## Notes on toplevel-only forms and eval-related functions

In the current Julia runtime,

`Base.eval()`
- Uses `jl_toplevel_eval_in` which calls `jl_toplevel_eval_flex`

`jl_toplevel_eval_flex(mod, ex)`
- Lowers if necessay
- Evaluates certain blessed top level forms
  * `:.`
  * `:module`
  * `:using`
  * `:import`
  * `:public`
  * `:export`
  * `:global`
  * `:const`
  * `:toplevel`
  * `:error`
  * `:incomplete`
  * Identifier and literals
- Otherwise expects `Expr(:thunk)`
  * Use codegen "where necessary/profitable" (eg ccall, has_loops etc)
  * Otherwise interpret via `jl_interpret_toplevel_thunk`

Should we lower the above blessed top level forms to julia runtime calls?
Pros:
- Semantically sound. Lowering should do syntax checking in things like
  `Expr(:using)` rather than doing this in the runtime support functions.
- Precise lowering error messages
- Replaces more Expr usage
- Replaces a whole pile of C code with significantly less Julia code
- Lowering output becomes more consistently imperative
Cons: 
- Lots more code to write
- May need to invent intermediate data structures to replace `Expr`
- Bootstrap?
- Some forms require creating toplevel thunks

In general, we'd be replacing current *declarative* lowering targets like
`Expr(:using)` with an *imperative* call to a `Core` API instead. The call and
the setup of its arguments would need to go in a thunk. We've currently got an
odd mixture of imperative and declarative lowered code.

## Bugs in Julia's lowering

List of bugs which should be fixed upstream in flisp implementation
* `f()[begin]` has the side effect `f()` twice.
* `a[(begin=1; a=2)]` gives a weird error
* `function A.ccall() ; end` allows `ccall` as a name but it's not allowed without the `A.`

## Notes on Racket's hygiene

People look at [Racket](https://racket-lang.org/) as an example of a very
complete system of hygienic macros. We should learn from them, but keeping in
mind that Racket's macro system is inherently more complicated. Racket's
current approach to hygiene is described in an [accessible talk](https://www.youtube.com/watch?v=Or_yKiI3Ha4)
and in more depth in [a paper](https://www-old.cs.utah.edu/plt/publications/popl16-f.pdf).

Some differences which makes Racket's macro expander different from Julia:

* Racket allows *local* definitions of macros. Macro code can be embedded in an
  inner lexical scope and capture locals from that scope, but still needs to be
  executed at compile time. Julia supports macros at top level scope only.
* Racket goes to great lengths to execute the minimal package code necessary to
  expand macros; the "pass system". Julia just executes all top level
  statements in order when precompiling a package.
* As a lisp, Racket's surface syntax is dramatically simpler and more uniform

