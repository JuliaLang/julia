# Collecting a restricted subtyping corpus

Set `JULIA_SUBTYPE_CORPUS_DIR` before starting Julia. The runtime creates that
leaf directory (its parent must exist) and saves one self-contained TOML file
per distinct completed public `jl_subtype_env` query. `jl_subtype`, including
ordinary `<:` calls that reach the runtime, delegates to this entry point.
Compiler-folded queries do not reach the logger. Internal decisions that depend
on an algorithm environment are not recorded as standalone queries.

```sh
mkdir -p /tmp/subtype-data
JULIA_SUBTYPE_CORPUS_DIR=/tmp/subtype-data/queries ./julia --startup-file=no workload.jl
```

Collection works in ordinary builds; `JL_SUBTYPE_TRACE` is not required. It can
coexist with the explicit decision transcripts from `jl_subtype_trace`. The
environment variable is sampled once, on the first query. The directory is
resolved to an absolute path at that point, so later `cd()` calls do not redirect
collection. Changing `ENV`
after startup does not toggle logging. With collection disabled, the added
cost is a cached atomic flag read at the public entry point.

To capture bootstrap, set the variable in the environment of a build that
actually regenerates the system image, or run the bootstrap command printed by
`make` with the variable set. A no-op `make` produces no new queries. Multiple
build processes may use the same directory.

## Accepted grammar

`JULIA_SUBTYPE_CORPUS_GRAMMAR` selects the input fragment:

| Value | Accepted inputs |
| --- | --- |
| `nominal` (default) | Original Boolean/nominal fragment below |
| `tuples` | Also fixed-length tuples, including empty and nested tuples |
| `unionall` | `tuples` plus closed `UnionAll` forms and bound variable occurrences |
| `datatype` | Fixed tuples and invariant datatypes (including structs and value parameters), plus `Type{T}`; no quantified input expressions |
| `linear` | `datatype` plus closed `UnionAll` forms whose variable occurs exactly once |
| `full` | Every completed query, without grammar, scope, depth, or node-count filtering |

For example:

```sh
JULIA_SUBTYPE_CORPUS_GRAMMAR=unionall JULIA_SUBTYPE_CORPUS_DIR=/tmp/subtype-data/quantified ./julia --startup-file=no workload.jl
```

The mode is read at startup alongside the directory. Unknown mode names disable
collection with a diagnostic. Existing nominal-mode records retain their exact
v1 format and keys.

The original fragment accepts:

* `Any` and `Union{}`.
* Nonparametric abstract and primitive `DataType`s whose entire supertype chain
  also belongs to this grammar.
* Native binary `Union` and `Core.Intersect` nodes, and unary `Core.Not` nodes,
  recursively containing accepted types.

The `nominal`, `tuples`, and `unionall` modes exclude structs, non-tuple parametric types, `Vararg`, unbounded
`Tuple`, `TypeEq`, `TypeEgal`, and partially initialized bootstrap types. A fixed
`Vararg` that Julia has already expanded into ordinary tuple elements is accepted
as that fixed tuple. Nominal ancestors remain nonparametric.

In `unionall` mode both bounds and the body must belong to the fragment. Bounds
may reference enclosing variables. Every variable occurrence must resolve to an
active binder; free variables, including occurrences escaping a shared expression,
are rejected. Each side of a query is validated as a closed expression. No
assumption about a variable's diagonal status is used to filter queries.

`datatype` and `linear` accept non-tuple datatype parameters invariantly, including
literal value parameters and this fork's `TypeEq` representation of `Type{T}`.
They still exclude `Vararg`, unbounded `Tuple`, `TypeEgal`, `TypeApp`, and free
variables. In `linear`, every binder must occur **exactly once syntactically**
in its body, including datatype parameters and bounds of nested binders. Repeated
uses in different union branches count separately. Inherited supertype metadata
is checked for admissibility but does not multiply these occurrence counts.
This is a syntactic subset, not an attempt to predict the legacy algorithm's
path-dependent variable use. Datatype family wrappers recorded as metadata may
contain `UnionAll` even in a `datatype` record; they are not part of its input
expression. Both modes use the v3 graph archive described below.

For restricted modes, depth is bounded at 256 and nodes/tuple edges at 4096. In extended modes the
4096 input limit counts visits rather than distinct pointers, since shared
expressions must be revisited in their lexical scope. Cyclic or larger inputs
are skipped. This bounds instrumentation work without eagerly expanding unions.

Recognizing `Core.Not` does **not** add complement semantics to Julia's native
subtyping algorithm. Only completed native queries are recorded; these results
are observations to compare with the experimental solver, not a specification
of its set semantics.

## Format (version 1)

A file starts with `result = true` or `result = false`, followed by:

```toml
version = 1
kind = "subtype"
lhs = 2
rhs = 1

[[nodes]]
id = 1
kind = "Any"

[[nodes]]
id = 2
kind = "Abstract"
name = "Example"
module = ["Main", "ExampleModule"]
module_uuid = "00000000000000000000000000000000"
module_build_id = "00000000000000000000000000000001"
nominal_id = "opaque nominal identity"
super = 1
```

IDs are one-based, local to the file, and assigned in deterministic postorder.
All operands and ancestors are included. The node variants are:

| `kind` | Additional fields |
| --- | --- |
| `Any`, `Bottom` | none |
| `Union`, `Intersect` | `a`, `b` (node IDs) |
| `Not` | `child` (node ID) |
| `Abstract` | `name`, `module`, `module_uuid`, `module_build_id`, `nominal_id`, `super` |
| `Primitive` | the `Abstract` fields plus `bits` |

Consumers can reconstruct fresh anonymous nominal hierarchies directly from
these records; the original modules and package dependencies are unnecessary.
Treat names and identities as data, never executable source.

## Extended format (version 2)

Queries containing tuples or binders use `version = 2` with the same envelope,
nominal nodes, and result line. Queries wholly in the old fragment still use v1.
Additional node variants are:

| `kind` | Additional fields |
| --- | --- |
| `Tuple` | `elements`: ordered array of node IDs (possibly empty) |
| `TypeVar` | `lb`, `ub`: bound node IDs; `binder`: unique query-local integer |
| `UnionAll` | `var`: its `TypeVar` node ID; `body`: body node ID |

Bounds precede the variable node in postorder. Repeated occurrences refer to the
same `TypeVar` ID; distinct binders have distinct IDs even when their bounds and
printed names agree. Binder numbers follow deterministic traversal, not memory
addresses or variable names. Alpha-renamed queries deduplicate. A shared binder
expression visited twice is recorded as two lexical binding occurrences, just as
two independently allocated copies would be. Scope-sensitive traversal also
prevents shared input pointers from hiding out-of-scope variables.

This is a capture/reconstruction extension; it does not implement quantified or
tuple subtyping in the experimental solver. A native replay checks that the
reconstructed query reproduces the recorded answer, not that the new solver
agrees or that Julia's diagonal semantics are set-theoretically complete.

## Identity, safety, and persistence

Uniqueness means the ordered pair of structural expression trees, including
nominal identity and hierarchy. Repeated structural subexpressions have one node
even when their original runtime allocations differ. Operand order and raw
unnormalized syntax are preserved: `Union{A,B}` and `Union{B,A}` may therefore
have separate files. Semantically equivalent but syntactically different queries
are intentionally not collapsed. Results are excluded from the deduplication
key.

Nominal identity includes the module build ID, module path, type name, and an
occurrence discriminator for distinct type objects with the same metadata.
Module build IDs and UUIDs are frozen when a nominal type is first encountered,
so later bootstrap metadata updates do not change its recorded identity.
Encountered nominal types are retained in a GC-scanned registry for the life of
the process, so reclaimed addresses cannot be mistaken for old types. This also
preserves distinct unbound types created with the same name in the same module.
Only image types and Core types that have canonical bindings when first
encountered use stable cross-process identities. Unbound, ambiguous, and other runtime-created
types additionally receive a process-session discriminator, so two processes
encountering same-name types in different orders cannot accidentally merge them.
The occurrence ordinal follows encounter order within that session. Fresh
modules, bootstrap-created types, and different image builds need not deduplicate
across runs. This avoids treating unrelated nominal definitions as the same type
merely because their names match.

Files use a 64-bit content hash with a collision suffix. Equality is always
checked against complete canonical key bytes; hash collisions cannot discard
queries. Complete temporary files are published by an atomic hard link, so
parallel processes cannot expose a half-written `.toml` or overwrite another
query. A filesystem supporting hard links is required. A killed process can
leave a hidden `.query-*` temporary file, which can safely be removed when no
collectors are running. Publication is atomic but does not promise persistence
across a machine power failure.

A conflicting result for an existing query emits a diagnostic and writes the
new observation to `<original>.conflict`, preserving the original `.toml`.
Filesystem failures produce a diagnostic without changing the subtype result.
An in-memory cache avoids repeated disk access for duplicate queries. Memory
usage grows with unique queries and nominal types while collection is enabled;
there is no persistent cross-file type heap and no eviction in this prototype.
Collection never calls Julia code, invokes a safepoint, or allocates Julia heap
objects while inspecting inputs. A mutex protects process-local state; separate
processes coordinate through atomic publication and byte comparisons.

Run the subtype suite, including corpus instrumentation tests, with:

```sh
JULIA_TEST_FAILFAST=1 make test-revise-subtype
```

## General graph format (version 3)

The `datatype`, `linear`, and `full` modes use a general graph archive, `version = 3`.
The envelope still contains `kind = "subtype"`, `lhs`, `rhs`, and the separately
recorded Boolean result. It additionally records `pointer_bytes` and `byte_order`
for native-layout value payloads. This format captures information for future
replay; it is not the v1/v2 solver replay format.

`datatype` admits fixed tuples and invariant datatype applications, including
struct types and literal value parameters, with no quantified or free variables.
`linear` additionally admits `UnionAll` when each bound variable has exactly one
syntactic occurrence, including occurrences in nested bounds. `full` bypasses
these grammar checks entirely. Its graph walk is iterative, has dynamically grown
storage, and has no fixed depth or node cap. It records cycles and sharing directly,
including the cycle between a datatype family and its wrapper.

IDs are one-based, assigned in deterministic discovery order, and may refer
forward or backward. An edge value of `0` means an absent runtime field. Unlike
v2, v3 preserves TypeVar objects and their names, including free variables and
shared raw expression objects; it does not perform alpha-renaming or collapse
independently allocated equivalent graphs. The `var` edge of each `UnionAll`
identifies its binding variable. Consumers must resolve lexical scope using these
edges, not printed names. Sharing differences can therefore produce separate
v3 records even when the expressions denote equivalent types.

| Node kind | Fields |
| --- | --- |
| `Any`, `Bottom` | none |
| `Union`, `Intersect` | `a`, `b` |
| `Not` | `child` |
| `TypeVar` | `name_hex`, `lb`, `ub` |
| `UnionAll` | `var`, `body` |
| `Vararg` | `T`, `N` (either can be absent) |
| `TypeEq`, `TypeEgal` | `T` |
| `TypeApp` | `head`, `param` |
| `DataType` | `typename`, `params`, `super`, `abstract`, `primitive`, `concrete`, `typename_available`; `bits` for initialized primitives |
| `TypeName` | `name_hex`, `module`, `wrapper`, `field_names`, `abstract`, `mutable`, `identity` |
| `Module` | `name_hex`, `parent`, `uuid`, `build_id`, `identity` |
| `SimpleVector` | `items` |
| `Symbol`, `String` | `bytes` |
| `Value` | `type`, `bytes`; primitives also have `bits`; ordinary immutable values have `pointer_offsets`, `pointer_refs`, `encoding` |
| `Opaque` | `type`, `identity`, `reason` |

Names, symbols, strings, and value bytes are lowercase hexadecimal, preserving
non-UTF8 symbol bytes and embedded NULs in strings without invalid TOML. `params`,
`items`, and `pointer_refs` contain ordered node IDs. Other graph-edge fields are
single node IDs. A missing typename during bootstrap is represented by a zero
edge and `typename_available = false`; consumers must not interpret the placeholder
`abstract = false` as a statement about a completed definition.

The archive includes datatype parameters, supertypes, and family wrappers, but
omits datatype caches, module bindings, and field-type graphs. These are unrelated
to nominal/invariant subtyping and can expand into much of the runtime. Lazily
computed datatype layouts are also omitted from datatype nodes to avoid changing
query identity merely because a layout has subsequently been computed.

Primitive values preserve their bytes. Ordinary immutable values without padding
preserve native payload bytes with GC pointer slots zeroed; the referenced objects
are recorded separately with byte offsets. Their `encoding` is
`"native-layout-pointer-masked"`. Mutable objects, padded values, and foreign or
special layouts are explicitly `Opaque`, with reason
`"mutable-padded-or-special-layout"`. Values containing native pointers are opaque
with reason `"native-pointer-value"`. The collector never dereferences native
pointers or walks arbitrary mutable runtime state. Opaque records identify the
object but do not reconstruct its contents; full capture is therefore broader
than fully portable replay.

Opaque and runtime-created nominal identities use a process-session prefix and a
monotonic counter, with a GC-rooted registry preventing address reuse. Raw heap
addresses are not identity keys. Canonically bound image/Core datatype families
use their module and family name identity; image modules carry build metadata.
Family identity classification is frozen at first encounter. A later replacement
of a canonical family receives a distinct process identity, while the original
family keeps its recorded identity.
As with other modes, different builds or runtime-created definitions can remain
separate across processes. Full records are compared in their entirety when
publishing; the file hash is only an index, never proof of equality.
