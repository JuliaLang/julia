# Operands: tagged 64-bit words into a shared pool (§3.2), and the two-mode
# per-statement `ops` word.

struct StmtId
    id::Int32
end
struct RegionId
    id::Int32
end
const Value = StmtId

const NULL_STMT = StmtId(0)
const NULL_REGION = RegionId(0)

Base.isless(a::StmtId, b::StmtId) = a.id < b.id
Base.show(io::IO, s::StmtId) = print(io, "%", s.id)
Base.show(io::IO, r::RegionId) = print(io, "^r", r.id)
isnull(s::StmtId) = s.id == 0
isnull(r::RegionId) = r.id == 0

# ---------------------------------------------------------------------------
# Operand: 4-bit tag (top bits) | 60-bit payload
# ---------------------------------------------------------------------------

struct Operand
    bits::UInt64
end

const TAG_NONE   = 0x0
const TAG_STMT   = 0x1
const TAG_BLOCK  = 0x2   # RegionId of a `block` region (cfg branch target)
const TAG_REGION = 0x3   # general region reference (exit targets, clock refs)
const TAG_CONST  = 0x4   # constant-pool index
const TAG_INLINE = 0x5   # small immediate
const TAG_GLOBAL = 0x6   # globals-table index
const TAG_SPARAM = 0x7   # static-parameter index

const PAYLOAD_MASK = (UInt64(1) << 60) - 1

optag(o::Operand) = (o.bits >> 60) % UInt8
payload(o::Operand) = o.bits & PAYLOAD_MASK
mkoperand(tag::Integer, payload::Integer) =
    Operand((UInt64(tag) << 60) | (UInt64(payload) & PAYLOAD_MASK))

const OP_NONE = mkoperand(TAG_NONE, 0)

# INLINE immediates carry a 4-bit subtype below the payload top:
#   [tag:4 | subtype:4 | value:56]
const IMM_INT    = 0x0   # signed 56-bit
const IMM_BOOL   = 0x1
const IMM_UINT8  = 0x2

imm_subtype(o::Operand) = ((o.bits >> 56) & 0xf) % UInt8
imm_bits(o::Operand) = o.bits & ((UInt64(1) << 56) - 1)

op_stmt(s::StmtId) = mkoperand(TAG_STMT, s.id % UInt64)
op_block(r::RegionId) = mkoperand(TAG_BLOCK, r.id % UInt64)
op_region(r::RegionId) = mkoperand(TAG_REGION, r.id % UInt64)
op_constidx(i::Integer) = mkoperand(TAG_CONST, i)
op_globalidx(i::Integer) = mkoperand(TAG_GLOBAL, i)
op_sparam(i::Integer) = mkoperand(TAG_SPARAM, i)

function op_inline(x::Int64)
    # signed 56-bit range check
    (-(Int64(1) << 55) <= x < (Int64(1) << 55)) || throw(ArgumentError("immediate out of range: $x"))
    mkoperand(TAG_INLINE, (UInt64(IMM_INT) << 56) | (x % UInt64 & ((UInt64(1) << 56) - 1)))
end
op_inline(x::Bool) = mkoperand(TAG_INLINE, (UInt64(IMM_BOOL) << 56) | UInt64(x))
op_inline(x::UInt8) = mkoperand(TAG_INLINE, (UInt64(IMM_UINT8) << 56) | UInt64(x))
op_inline(x::Integer) = op_inline(Int64(x))

is_stmt(o::Operand) = optag(o) == TAG_STMT
is_inline(o::Operand) = optag(o) == TAG_INLINE

asstmt(o::Operand) = (@assert optag(o) == TAG_STMT; StmtId(payload(o) % Int32))
asregion(o::Operand) = (@assert optag(o) == TAG_REGION || optag(o) == TAG_BLOCK;
                        RegionId(payload(o) % Int32))

"Decode an INLINE immediate to its Julia value."
function imm_value(o::Operand)
    @assert optag(o) == TAG_INLINE
    st = imm_subtype(o)
    b = imm_bits(o)
    if st == IMM_INT
        # sign-extend 56 bits
        v = ((b << 8) % Int64) >> 8
        return v
    elseif st == IMM_BOOL
        return b != 0
    elseif st == IMM_UINT8
        return b % UInt8
    else
        error("unknown immediate subtype")
    end
end

# ---------------------------------------------------------------------------
# The per-statement `ops` word: two-mode union (§3.2)
#   pool mode:   [0 | offset:39 | len:24]        (offset = 0-based into pool)
#   inline mode: [1 | arity:2 | spare:5 | imm:24 | stmt:32]
# ---------------------------------------------------------------------------

const OPS_INLINE_BIT = UInt64(1) << 63

ops_pool(offset::Integer, len::Integer) =
    (UInt64(offset) << 24) | UInt64(len)

function ops_inline(stmt::StmtId, imm::Union{Nothing,Int}, arity::Int)
    @assert 1 <= arity <= 2
    immbits = imm === nothing ? UInt64(0) : (UInt64(imm % UInt32) & 0xffffff)
    if imm !== nothing
        (-(1 << 23) <= imm < (1 << 23)) || throw(ArgumentError("inline immediate out of range: $imm"))
    end
    OPS_INLINE_BIT | (UInt64(arity) << 61) | (immbits << 32) | (UInt64(stmt.id % UInt32))
end

is_ops_inline(w::UInt64) = (w & OPS_INLINE_BIT) != 0
ops_offset(w::UInt64) = (w >> 24) & ((UInt64(1) << 39) - 1)
ops_len(w::UInt64) = w & 0xffffff
inline_arity(w::UInt64) = Int((w >> 61) & 0x3)
inline_stmt(w::UInt64) = StmtId((w % UInt32) % Int32)
function inline_imm(w::UInt64)
    b = (w >> 32) & 0xffffff
    Int(((b << 40) % Int64) >> 40)   # sign-extend 24 bits
end
set_inline_stmt(w::UInt64, s::StmtId) = (w & ~UInt64(0xffffffff)) | UInt64(s.id % UInt32)

const OPS_EMPTY = ops_pool(0, 0)
