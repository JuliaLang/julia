# Kind registry: namespaced statement kinds (§3.4) — THE registry for the
# whole stack (Level 2 step 1): syntax kinds (JuliaSyntax/JuliaLowering) and
# IR kinds share one numbering space through this single instance.
#
# `Kind` is a 16-bit primitive whose bits split into `dialect id | opcode`.
# The split is a registry parameter. The core dialect has fixed dialect id 0
# and the bootstrap syntax stack claims statically reserved dialect ids
# (1..FIRST_DYNAMIC_DIALECT-1), so their `K"…"` literals are compile-time
# constants safe to bake into pkgimages; genuinely external dialects get
# session-local ids ≥ FIRST_DYNAMIC_DIALECT and their literals read a
# registration-populated binding. Identity is symbolic (`dialect.opname`).

"""
    K"name"
    Kind(namestr)

`Kind` is a type tag for the kind of nodes/statements on the shared substrate
(§3.7): tokens and interior nodes of syntax trees, and IR statement kinds —
one nominal type end to end, one numbering space (syntax `K"call"` and core
`K"call"` are DISTINCT kinds in different dialects).
"""
primitive type Kind 16 end

kind_uint(k::Kind) = reinterpret(UInt16, k)

function Kind(x::Integer)
    if x < 0 || x > typemax(UInt16)
        throw(ArgumentError("Kind out of range: $x"))
    end
    return Base.bitcast(Kind, convert(UInt16, x))
end

Base.isless(x::Kind, y::Kind) = kind_uint(x) < kind_uint(y)

# ---------------------------------------------------------------------------
# Effect / flag bits (per-stmt `flag` column; kinds carry a default mask)
# ---------------------------------------------------------------------------

const FLAG_CONSISTENT   = UInt32(1) << 0
const FLAG_EFFECT_FREE  = UInt32(1) << 1
const FLAG_NOTHROW      = UInt32(1) << 2
const FLAG_TERMINATES   = UInt32(1) << 3
const FLAG_NOUB         = UInt32(1) << 4
const FLAG_INBOUNDS     = UInt32(1) << 5
const FLAG_INLINE       = UInt32(1) << 6   # :inline meta
const FLAG_NOINLINE     = UInt32(1) << 7   # :noinline meta
const FLAG_REFINED      = UInt32(1) << 8   # statement type was refined (irinterp)
const FLAG_UNUSED       = UInt32(1) << 9   # scratch bit for passes

const FLAG_REMOVABLE = FLAG_EFFECT_FREE | FLAG_NOTHROW | FLAG_TERMINATES
const FLAG_PURE      = FLAG_CONSISTENT | FLAG_REMOVABLE | FLAG_NOUB

# ---------------------------------------------------------------------------
# Operand schema (arity checks, printing, accessors)
# ---------------------------------------------------------------------------

# Operand classes for schema declarations. OC_VALUE admits any value-producing
# operand word (STMT/CONST/INLINE/GLOBAL/SPARAM); the others are exact-tag.
@enum OpClass::UInt8 OC_VALUE OC_STMT OC_REGION OC_BLOCK OC_CONST OC_IMM OC_ANY

struct OperandSpec
    name::Symbol
    class::OpClass
end

struct KindInfo
    name::Symbol            # unqualified opname
    qualified::Symbol       # `dialect.opname` (core: plain opname)
    dialect::UInt16         # dialect id
    opcode::UInt16
    result::Int8            # 0, 1, or -1 = instance-determined (region owners)
    is_terminator::Bool
    owns_regions::Bool
    is_delay::Bool          # temporal identity: never CSE'd/duplicated (§4.3)
    effects::UInt32         # default flag mask
    schema::Vector{OperandSpec}
    minops::Int32
    maxops::Int32           # -1 = variadic
    inline_ops::Bool        # eligible for the inline `ops`-word encoding (§3.2)
end

mutable struct Dialect
    name::Symbol
    id::UInt16
    mod::Any                        # owning module (Module; or Symbol placeholder)
    kinds::Vector{KindInfo}         # opcode-indexed (opcode 0 => index 1)
    byname::Dict{Symbol,UInt16}     # opname -> opcode
    aliases::Dict{Symbol,UInt16}    # range markers (BEGIN_/END_): name -> opcode
end

mutable struct KindRegistry
    opcode_bits::Int
    dialects::Vector{Union{Dialect,Nothing}}  # dialect id d => index d+1 (sparse:
                                              #   reserved ids may be claimed out of order)
    byname::Dict{Symbol,UInt16}     # dialect name -> dialect id
    # session bindings for external-dialect `K"…"` literals (§3.4):
    bindings::Dict{Symbol,Base.RefValue{Kind}}   # qualified name -> kind
    lock::ReentrantLock
end

const REGISTRY = KindRegistry(10, Union{Dialect,Nothing}[], Dict{Symbol,UInt16}(),
                              Dict{Symbol,Base.RefValue{Kind}}(), ReentrantLock())

# Dialect ids 0 (core) .. FIRST_DYNAMIC_DIALECT-1 are statically reservable —
# the bootstrap stack (core + the vendored syntax dialects) claims fixed ids
# so its K"…" literals are deterministic across sessions and safe in
# pkgimages. Dynamic (session-local) allocation starts at
# FIRST_DYNAMIC_DIALECT; the top dialect id is never allocated so
# typemax(Kind) (KIND_UNSET/KIND_UNREGISTERED) is never a valid kind.
const FIRST_DYNAMIC_DIALECT = 8

const KIND_UNREGISTERED = Base.bitcast(Kind, typemax(UInt16))

dialect_id(k::Kind) = kind_uint(k) >> REGISTRY.opcode_bits
opcode(k::Kind) = kind_uint(k) & ((UInt16(1) << REGISTRY.opcode_bits) - UInt16(1))
make_kind(dialect::Integer, opcode::Integer) =
    Base.bitcast(Kind, (UInt16(dialect) << REGISTRY.opcode_bits) | UInt16(opcode))

max_dialect_id() = (UInt16(1) << (16 - REGISTRY.opcode_bits)) - 2  # top id reserved

function dialect(k::Kind)::Union{Dialect,Nothing}
    d = Int(dialect_id(k))
    d < length(REGISTRY.dialects) || return nothing
    return REGISTRY.dialects[d + 1]
end

dialect_by_name(name::Symbol) = begin
    id = get(REGISTRY.byname, name, nothing)
    id === nothing ? nothing : REGISTRY.dialects[id + 1]
end

function kindinfo(k::Kind)::KindInfo
    dia = dialect(k)
    dia === nothing && throw(ArgumentError("unregistered dialect in kind $(kind_uint(k))"))
    oc = opcode(k)
    oc < length(dia.kinds) || throw(ArgumentError("unregistered opcode $oc in dialect $(dia.name)"))
    return dia.kinds[oc + 1]
end

kindname(k::Kind) = kindinfo(k).qualified

"""
    register_dialect!(name; id=nothing, mod=nothing) -> Dialect

Register a dialect; idempotent by name. `id` claims a statically reserved
dialect id (< FIRST_DYNAMIC_DIALECT — the bootstrap stack); without it the
dialect gets the next free session-local id. `mod` records the owning module
(for `parentmodule` of its kinds).
"""
function register_dialect!(name::Symbol; id::Union{Nothing,Integer} = nothing,
                           mod = nothing)
    lock(REGISTRY.lock) do
        if haskey(REGISTRY.byname, name)
            d = REGISTRY.dialects[REGISTRY.byname[name] + 1]::Dialect
            id === nothing || d.id == id ||
                error("kind registry: dialect $name already registered with id $(d.id), not $id")
            mod === nothing || (d.mod = mod)
            return d
        end
        if id === nothing
            did = UInt16(FIRST_DYNAMIC_DIALECT)
            while did + 1 <= length(REGISTRY.dialects) && REGISTRY.dialects[did + 1] !== nothing
                did += UInt16(1)
            end
        else
            0 <= id < FIRST_DYNAMIC_DIALECT ||
                error("kind registry: reserved dialect ids are 0..$(FIRST_DYNAMIC_DIALECT-1), got $id")
            did = UInt16(id)
            did + 1 <= length(REGISTRY.dialects) && REGISTRY.dialects[did + 1] !== nothing &&
                error("kind registry: dialect id $id already claimed by $(REGISTRY.dialects[did+1].name)")
        end
        did <= max_dialect_id() ||
            error("kind registry: dialect capacity exhausted (widen the bit split)")
        d = Dialect(name, did, mod, KindInfo[], Dict{Symbol,UInt16}(), Dict{Symbol,UInt16}())
        while length(REGISTRY.dialects) < did + 1
            push!(REGISTRY.dialects, nothing)
        end
        REGISTRY.dialects[did + 1] = d
        REGISTRY.byname[name] = did
        return d
    end
end

"""
    alias_kind!(d, name, opcode)

Register `name` as an alias for `opcode` in dialect `d` — range markers
(`BEGIN_x`/`END_x`) that conflate with the first/last kind of a category so
range predicates work. Aliases resolve by name but have no `KindInfo` of
their own. Idempotent.
"""
function alias_kind!(d::Dialect, name::Symbol, oc::Integer)
    lock(REGISTRY.lock) do
        d.aliases[name] = UInt16(oc)
        qualified = d.id == 0 ? name : Symbol(string(d.name), ".", string(name))
        get!(REGISTRY.bindings, qualified, Ref(KIND_UNREGISTERED))[] = make_kind(d.id, oc)
        return make_kind(d.id, oc)
    end
end

"""
    resolve_kind(name, path) -> Kind | nothing

Per-consumer name resolution (names collide across dialects — syntax
`K"call"` and core `K"call"` are distinct kinds): `"dialect.opname"` resolves
directly; an unqualified name is searched along `path` (an iterable of
dialect name Symbols), first match (including range-marker aliases) wins.
"""
# A name is dialect-qualified iff the text before its first '.' is a
# registered dialect name (syntax kind names like "..", ".=", ".&&" contain
# dots but are not qualified).
function _qualified_dialect(name::AbstractString)
    i = findfirst('.', name)
    (i === nothing || i == 1 || i == lastindex(name)) && return nothing
    return dialect_by_name(Symbol(name[1:prevind(name, i)]))
end

function resolve_kind(name::AbstractString, path)
    d = _qualified_dialect(name)
    if d !== nothing
        r = get(REGISTRY.bindings, Symbol(name), nothing)
        r === nothing && return nothing
        k = r[]
        return k === KIND_UNREGISTERED ? nothing : k
    end
    sym = Symbol(name)
    for dn in path
        d = dialect_by_name(dn)
        d === nothing && continue
        oc = get(d.byname, sym, nothing)
        oc === nothing && (oc = get(d.aliases, sym, nothing))
        oc === nothing || return make_kind(d.id, oc)
    end
    return nothing
end

"""
    Kind(namestr)

Resolve a kind by name: `"dialect.opname"` qualified, or an unqualified name
searched across non-core dialects in id order with core as the fallback.
(The string constructor is the historical JuliaSyntax-facing API — kind
names that collide with core, like `"break"`, keep resolving to the syntax
kind; core kinds are unambiguous through `UnifiedIR.@K_str` and qualified
names. Consumers with their own namespace use `K"…"` string macros, which
resolve along per-package search paths.)
"""
function Kind(s::AbstractString)
    if _qualified_dialect(s) !== nothing
        k = resolve_kind(s, ())
        k === nothing && error("unknown Kind name $(repr(s))")
        return k
    end
    sym = Symbol(s)
    for d in REGISTRY.dialects
        (d === nothing || d.id == 0) && continue
        oc = get(d.byname, sym, nothing)
        oc === nothing && (oc = get(d.aliases, sym, nothing))
        oc === nothing || return make_kind(d.id, oc)
    end
    let d = REGISTRY.dialects[1]   # core fallback
        if d !== nothing
            oc = get(d.byname, sym, nothing)
            oc === nothing || return make_kind(UInt16(0), oc)
        end
    end
    error("unknown Kind name $(repr(s))")
end

# --------------------------- display & serialization -----------------------

"Unqualified display name (the historical JuliaSyntax convention; use
`kindname` for the dialect-qualified symbolic identity)."
function Base.string(x::Kind)
    dia = dialect(x)
    dia === nothing && return "<error: unknown kind>"
    oc = opcode(x)
    oc < length(dia.kinds) || return "<error: unknown kind>"
    return string(dia.kinds[oc + 1].name)
end

Base.print(io::IO, x::Kind) = print(io, string(x))

function Base.show(io::IO, k::Kind)
    print(io, "K\"", k, "\"")
end

"Owning module of a kind's dialect (recorded at registration)."
function Base.parentmodule(k::Kind)
    dia = dialect(k)
    dia === nothing && throw(ArgumentError("unregistered dialect in kind $(kind_uint(k))"))
    return dia.mod::Module
end

# Serialize kinds symbolically (dialect-qualified) so bit patterns never
# escape a session (§3.4). Core kinds write their plain name.
function Base.write(io::IO, k::Kind)
    str = string(kindname(k))
    write(io, UInt8(sizeof(str))) + write(io, str)
end
function Base.read(io::IO, ::Type{Kind})
    len = read(io, UInt8)
    str = String(read(io, len))
    Kind(str)
end

"""
Register a statement kind. `schema` is a vector of `name => OpClass` pairs; a
trailing `varargs=true` makes the last spec repeatable. Returns the `Kind`.
Idempotent by name (re-registration with identical shape returns the old kind).
"""
function register_kind!(d::Dialect, name::Symbol;
                        result::Integer = 1,
                        terminator::Bool = false,
                        owns_regions::Bool = false,
                        is_delay::Bool = false,
                        effects::UInt32 = UInt32(0),
                        schema::Vector{Pair{Symbol,OpClass}} = Pair{Symbol,OpClass}[],
                        minops::Integer = length(schema),
                        varargs::Bool = false,
                        inline_ops::Bool = false)
    lock(REGISTRY.lock) do
        if haskey(d.byname, name)
            oc = d.byname[name]
            return make_kind(d.id, oc)
        end
        oc = UInt16(length(d.kinds))
        oc < (UInt16(1) << REGISTRY.opcode_bits) ||
            error("kind registry: opcode capacity exhausted in dialect $(d.name)")
        qualified = d.id == 0 ? name : Symbol(string(d.name), ".", string(name))
        if inline_ops
            # §3.2 eligibility: exactly one STMT operand plus at most one raw
            # immediate — never CONST/REGION/BLOCK/GLOBAL.
            nstmt = count(p -> p.second === OC_STMT || p.second === OC_VALUE, schema)
            nimm = count(p -> p.second === OC_IMM, schema)
            (nstmt == 1 && nimm <= 1 && nstmt + nimm == length(schema) && !varargs) ||
                error("kind $qualified: inline_ops requires exactly one STMT + ≤1 immediate")
        end
        info = KindInfo(name, qualified, d.id, oc, Int8(result), terminator,
                        owns_regions, is_delay, effects,
                        [OperandSpec(p.first, p.second) for p in schema],
                        Int32(minops), varargs ? Int32(-1) : Int32(length(schema)),
                        inline_ops)
        push!(d.kinds, info)
        d.byname[name] = oc
        k = make_kind(d.id, oc)
        get!(REGISTRY.bindings, qualified, Ref(KIND_UNREGISTERED))[] = k
        return k
    end
end

"Session binding for a qualified kind name (external-dialect `K\"…\"` literals)."
function kindref(qualified::Symbol)
    lock(REGISTRY.lock) do
        get!(REGISTRY.bindings, qualified, Ref(KIND_UNREGISTERED))
    end
end

function lookup_kind(qualified::AbstractString)
    r = kindref(Symbol(qualified))[]
    r === KIND_UNREGISTERED && error("kind \"$qualified\" is not registered")
    return r
end

# `K"opname"` / `K"dialect.opname"` literal macro. Core-dialect literals are
# compile-time constants; external-dialect literals read the session binding.
macro K_str(s)
    if !occursin('.', s)
        # core dialect: resolve now, splice the constant
        oc = get(CORE_DIALECT.byname, Symbol(s), nothing)
        oc === nothing && error("unknown core kind K\"$s\"")
        return make_kind(UInt16(0), oc)
    else
        qsym = QuoteNode(Symbol(s))
        return :($check_kindref($kindref($qsym), $s))
    end
end

@inline function check_kindref(r::Base.RefValue{Kind}, name::String)
    k = r[]
    k === KIND_UNREGISTERED && error("kind \"$name\" used before its dialect was registered")
    return k
end

# ---------------------------------------------------------------------------
# The sealed core dialect (dialect id 0)
# ---------------------------------------------------------------------------

const CORE_DIALECT = register_dialect!(:core; id = 0, mod = @__MODULE__)

const P = Pair{Symbol,OpClass}

# opcode 0 is the tombstone
register_kind!(CORE_DIALECT, :deleted; result=0, effects=FLAG_PURE)

# values / structure
register_kind!(CORE_DIALECT, :region_arg; result=1, effects=FLAG_PURE)
register_kind!(CORE_DIALECT, :extract;    result=1, effects=FLAG_PURE,
               schema=P[:value=>OC_STMT, :index=>OC_IMM], inline_ops=true)
register_kind!(CORE_DIALECT, :refine;     result=1, effects=FLAG_PURE,
               schema=P[:value=>OC_VALUE])                  # Pi successor
register_kind!(CORE_DIALECT, :value;      result=1, effects=FLAG_PURE,
               schema=P[:payload=>OC_CONST])                # escape hatch

# computation (Julia-vocabulary kinds; semantics provider-side)
register_kind!(CORE_DIALECT, :call;    result=1, schema=P[:callee=>OC_VALUE], varargs=true, minops=1)
register_kind!(CORE_DIALECT, :invoke;  result=1, schema=P[:codeinst=>OC_CONST, :callee=>OC_VALUE], varargs=true, minops=2)
register_kind!(CORE_DIALECT, :intrinsic; result=1, schema=P[:which=>OC_CONST], varargs=true, minops=1)
register_kind!(CORE_DIALECT, :foreigncall; result=1, varargs=true, minops=1)
register_kind!(CORE_DIALECT, :new;     result=1, schema=P[:type=>OC_VALUE], varargs=true, minops=1)
register_kind!(CORE_DIALECT, :splatnew; result=1, schema=P[:type=>OC_VALUE, :args=>OC_VALUE], minops=2)
register_kind!(CORE_DIALECT, :globalref; result=1, schema=P[:ref=>OC_ANY])  # GLOBAL operand
register_kind!(CORE_DIALECT, :isdefined_global; result=1, schema=P[:ref=>OC_ANY, :allow_import=>OC_IMM], minops=1)
register_kind!(CORE_DIALECT, :copyast; result=1, schema=P[:ast=>OC_VALUE])
register_kind!(CORE_DIALECT, :cfunction; result=1, varargs=true)
register_kind!(CORE_DIALECT, :new_opaque_closure; result=1, varargs=true)

# structured control flow: region owners
register_kind!(CORE_DIALECT, :if;   result=-1, owns_regions=true, schema=P[:cond=>OC_VALUE])
register_kind!(CORE_DIALECT, :loop; result=-1, owns_regions=true, varargs=true, minops=0)  # ops = init values
register_kind!(CORE_DIALECT, Symbol("try"); result=-1, owns_regions=true, varargs=true, minops=0) # optional dynscope operand
register_kind!(CORE_DIALECT, :cfg;  result=-1, owns_regions=true, varargs=true, minops=0)  # ops = entry block args
register_kind!(CORE_DIALECT, :closure; result=1, owns_regions=true, varargs=true, minops=0)
register_kind!(CORE_DIALECT, :select; result=1, effects=FLAG_PURE,
               schema=P[:cond=>OC_VALUE, :iftrue=>OC_VALUE, :iffalse=>OC_VALUE])

# structured terminators (0-result)
register_kind!(CORE_DIALECT, :result;    result=0, terminator=true, varargs=true, minops=0)
register_kind!(CORE_DIALECT, :continue; result=0, terminator=true,
               schema=P[:target=>OC_REGION, :cond=>OC_VALUE], varargs=true, minops=2)
register_kind!(CORE_DIALECT, :break;    result=0, terminator=true,
               schema=P[:target=>OC_REGION], varargs=true, minops=1)
register_kind!(CORE_DIALECT, :return;   result=0, terminator=true, varargs=true, minops=0)
register_kind!(CORE_DIALECT, :unreachable; result=0, terminator=true)

# cfg block terminators (edge bundles; §5.5)
register_kind!(CORE_DIALECT, :goto;   result=0, terminator=true, varargs=true, minops=2)
register_kind!(CORE_DIALECT, :br_if;  result=0, terminator=true, varargs=true, minops=5)
register_kind!(CORE_DIALECT, :switch; result=0, terminator=true, varargs=true, minops=3)
register_kind!(CORE_DIALECT, :await;  result=0, terminator=true, varargs=true, minops=5)

# memory cells (§6)
register_kind!(CORE_DIALECT, :cell;        result=1, schema=P[:type=>OC_CONST])
register_kind!(CORE_DIALECT, :cell_shared; result=1, schema=P[:type=>OC_CONST])
register_kind!(CORE_DIALECT, :cell_get;    result=1, schema=P[:cell=>OC_STMT], inline_ops=true)
register_kind!(CORE_DIALECT, :cell_set;    result=0, schema=P[:cell=>OC_STMT, :value=>OC_VALUE])
register_kind!(CORE_DIALECT, :cell_new;    result=0, schema=P[:cell=>OC_STMT])
register_kind!(CORE_DIALECT, :cell_isdefined; result=1, effects=FLAG_REMOVABLE,
               schema=P[:cell=>OC_STMT], inline_ops=true)
register_kind!(CORE_DIALECT, :throw_undef_if_not; result=0,
               schema=P[:cond=>OC_VALUE, :name=>OC_CONST])

# lowering/runtime vocabulary
register_kind!(CORE_DIALECT, :gc_preserve_begin; result=1, varargs=true, minops=0)
register_kind!(CORE_DIALECT, :gc_preserve_end;   result=0, schema=P[:token=>OC_STMT], inline_ops=true)
register_kind!(CORE_DIALECT, :boundscheck; result=1, effects=FLAG_REMOVABLE, varargs=true, minops=0)
register_kind!(CORE_DIALECT, :latestworld; result=0)
register_kind!(CORE_DIALECT, :coverage_effect; result=0)
register_kind!(CORE_DIALECT, :method_def; result=1, varargs=true, minops=1)
register_kind!(CORE_DIALECT, :global_decl; result=0, varargs=true, minops=1)
register_kind!(CORE_DIALECT, :const_decl; result=0, varargs=true, minops=2)
register_kind!(CORE_DIALECT, :throw_top; result=0, schema=P[:value=>OC_VALUE]) # toplevel-only error forms

const KIND_DELETED = K"deleted"

is_terminator(k::Kind) = kindinfo(k).is_terminator
owns_regions(k::Kind) = kindinfo(k).owns_regions
result_arity(k::Kind) = kindinfo(k).result
default_effects(k::Kind) = kindinfo(k).effects
is_delay_kind(k::Kind) = kindinfo(k).is_delay
has_inline_ops(k::Kind) = kindinfo(k).inline_ops

# Sealed core exit kinds (§5.9): the only inter-region control transfers.
is_exit_kind(k::Kind) = k === K"break" || k === K"continue" || k === K"return" || k === K"goto"
