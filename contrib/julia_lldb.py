# This file is a part of Julia. License is MIT: https://julialang.org/license

"""LLDB type summaries for Julia runtime values.

Load this script into lldb to make `p`/`frame variable` render `jl_value_t*`
(and other Julia runtime pointers such as `jl_datatype_t*`, `jl_sym_t*`,
`jl_svec_t*`, `jl_array_t*`, `jl_method_t*`, ...) the way Julia would show
them, instead of as raw addresses:

    (lldb) p v
    (jl_value_t *) 0x00007f... :foo
    (lldb) p (jl_value_t *)some_array
    (jl_value_t *) 0x00007f... 3-element Vector{Int64} = {1, 2, 3}

Usage (pick one):
  * `command script import /path/to/julia/contrib/julia_lldb.py` inside lldb,
  * add that line to your `~/.lldbinit`, or
  * `lldb -o "command script import /path/to/julia/contrib/julia_lldb.py" -- ./julia ...`

The summaries dispatch on the *runtime* type tag of the object, so a plain
`jl_value_t*` prints as whatever it actually is. Everything is resolved
through the debug info of libjulia-internal, so this script does not
hard-code struct offsets and should work across Julia versions; it requires a
build of Julia with debug info (the default) and gracefully degrades to raw
pointers when the debug info or the memory is unavailable.

The script also installs a stop-hook that transparently resumes the benign
SIGSEGVs Julia's GC uses to stop the world at safepoints (real segfaults
still stop the debugger); control it with `jl-safepoint-filter [on|off]`.
"""

import re
import struct

import lldb

# Render at most this many nested levels in a single summary string.
MAX_DEPTH = 3
# Render at most this many elements of arrays/svecs/tuples in a summary.
MAX_ELEMS = 10
# Truncate strings longer than this many bytes.
MAX_STRING = 200


class Ctx:
    """Debug-info lookups and memory reads for one (target, process) pair."""

    def __init__(self, target, process):
        self.target = target
        self.process = process
        self.ptrsize = target.GetAddressByteSize()
        self.types = {}
        self.sizes = {}
        self.offsets = {}
        self.dt_names = {}

    def lookup_type(self, name):
        t = self.types.get(name)
        if t is None:
            t = self.target.FindFirstType(name)
            if not t.IsValid():
                raise MemoryReadError("no type named %s in debug info" % name)
            self.types[name] = t
        return t

    def type_size(self, name):
        s = self.sizes.get(name)
        if s is None:
            s = self.lookup_type(name).GetByteSize()
            self.sizes[name] = s
        return s

    def field_offset(self, typename, fieldname):
        key = (typename, fieldname)
        off = self.offsets.get(key)
        if off is None:
            t = self.lookup_type(typename)
            for f in t.get_fields_array():
                if f.GetName() == fieldname:
                    off = f.GetOffsetInBytes()
                    break
            if off is None:
                raise MemoryReadError("no field %s in %s"
                                      % (fieldname, typename))
            self.offsets[key] = off
        return off

    def read_mem(self, addr, size):
        err = lldb.SBError()
        buf = self.process.ReadMemory(addr, size, err)
        if err.Fail() or buf is None:
            raise MemoryReadError("cannot read 0x%x" % addr)
        return buf

    def read_uint(self, addr, size, signed=False):
        return int.from_bytes(self.read_mem(addr, size), "little",
                              signed=signed)

    def read_ptr(self, addr):
        return self.read_uint(addr, self.ptrsize)

    def read_cstring(self, addr, maxlen=512):
        err = lldb.SBError()
        s = self.process.ReadCStringFromMemory(addr, maxlen, err)
        if err.Fail() or s is None:
            raise MemoryReadError("cannot read string at 0x%x" % addr)
        return s

    def value_at(self, addr, typename):
        """An SBValue of type `typename` located at `addr`."""
        t = self.lookup_type(typename)
        return self.target.CreateValueFromAddress(
            "jlval", lldb.SBAddress(addr, self.target), t)


class MemoryReadError(Exception):
    pass


_CTX = [None]


def get_ctx(valobj):
    target = valobj.GetTarget()
    process = valobj.GetProcess()
    ctx = _CTX[0]
    if (ctx is None or ctx.target != target or ctx.process != process
            or ctx.process.GetUniqueID() != process.GetUniqueID()):
        ctx = Ctx(target, process)
        _CTX[0] = ctx
    return ctx


def member_u(sbval, *names):
    v = sbval
    for name in names:
        v = v.GetChildMemberWithName(name)
        if not v.IsValid():
            raise MemoryReadError("no member %s" % name)
    return v.GetValueAsUnsigned()


def member_i(sbval, name):
    v = sbval.GetChildMemberWithName(name)
    if not v.IsValid():
        raise MemoryReadError("no member %s" % name)
    return v.GetValueAsSigned()


def typetag(ctx, addr):
    return ctx.read_ptr(addr - ctx.ptrsize) & ~15


def small_typeof_addr(ctx):
    for sym in ("jl_small_typeof", "ijl_small_typeof"):
        var = ctx.target.FindFirstGlobalVariable(sym)
        if var.IsValid():
            addr = var.GetLoadAddress()
            if addr != lldb.LLDB_INVALID_ADDRESS:
                return addr
    return 0


def typeof_addr(ctx, addr):
    tag = typetag(ctx, addr)
    if tag < (64 << 4):
        table = small_typeof_addr(ctx)
        if table == 0:
            return 0
        # entry lives at byte offset `tag` (see jl_to_typeof in julia.h)
        return ctx.read_ptr(table + tag)
    return tag


def symbol_name(ctx, addr):
    return ctx.read_cstring(addr + ctx.type_size("jl_sym_t"))


def svec_len(ctx, addr):
    return member_u(ctx.value_at(addr, "jl_svec_t"), "length")


def svec_ref(ctx, addr, i):
    return ctx.read_ptr(addr + ctx.type_size("jl_svec_t") + i * ctx.ptrsize)


def is_cpu_addrspace(ctx, addr):
    if addr == 0:
        return False
    try:
        dtaddr = typeof_addr(ctx, addr)
        if dtaddr == 0:
            return False
        dt = ctx.value_at(dtaddr, "jl_datatype_t")
        tname = symbol_name(ctx, member_u(
            ctx.value_at(member_u(dt, "name"), "jl_typename_t"), "name"))
        return tname == "AddrSpace" and ctx.read_uint(addr, 1) == 0
    except MemoryReadError:
        return False


def module_path(ctx, addr, depth=0):
    if addr == 0 or depth > 10:
        return "?"
    mod = ctx.value_at(addr, "jl_module_t")
    name = symbol_name(ctx, member_u(mod, "name"))
    parent = member_u(mod, "parent")
    if parent == 0 or parent == addr:
        return name
    pname = module_path(ctx, parent, depth + 1)
    if pname == "Main":
        return name
    return pname + "." + name


def datatype_qualname(ctx, dtaddr):
    name = ctx.dt_names.get(dtaddr)
    if name is None:
        dt = ctx.value_at(dtaddr, "jl_datatype_t")
        tn = ctx.value_at(member_u(dt, "name"), "jl_typename_t")
        name = (module_path(ctx, member_u(tn, "module")) + "."
                + symbol_name(ctx, member_u(tn, "name")))
        ctx.dt_names[dtaddr] = name
    return name


def typename_of(ctx, dtaddr):
    dt = ctx.value_at(dtaddr, "jl_datatype_t")
    tn = ctx.value_at(member_u(dt, "name"), "jl_typename_t")
    return symbol_name(ctx, member_u(tn, "name"))


def string_data(ctx, addr):
    strlen = ctx.read_uint(addr, ctx.ptrsize)
    n = min(strlen, MAX_STRING)
    s = ctx.read_mem(addr + ctx.ptrsize, n).decode("utf-8", errors="replace") \
        if n else ""
    return s, strlen


def escape_string(s):
    out = []
    for c in s:
        if c == '"':
            out.append('\\"')
        elif c == "\\":
            out.append("\\\\")
        elif c == "\n":
            out.append("\\n")
        elif c == "\t":
            out.append("\\t")
        elif ord(c) < 32:
            out.append("\\x%02x" % ord(c))
        else:
            out.append(c)
    return "".join(out)


# --------------------------------------------------------------------------
# rendering of type objects (DataType/Union/UnionAll/TypeVar/Vararg)
# --------------------------------------------------------------------------

def is_type_kind(qual):
    return qual in ("Core.DataType", "Core.Union", "Core.UnionAll",
                    "Core.TypeVar", "Core.TypeofVararg", "Core.TypeofBottom")


def render_typevar(ctx, addr, with_bounds):
    tv = ctx.value_at(addr, "jl_tvar_t")
    name = symbol_name(ctx, member_u(tv, "name"))
    if not with_bounds:
        return name
    lb = render_type(ctx, member_u(tv, "lb"), MAX_DEPTH - 1)
    ub = render_type(ctx, member_u(tv, "ub"), MAX_DEPTH - 1)
    if lb == "Union{}" and ub == "Any":
        return name
    if lb == "Union{}":
        return "%s<:%s" % (name, ub)
    return "%s<:%s<:%s" % (lb, name, ub)


def flatten_union(ctx, addr, parts, depth):
    qual = datatype_qualname(ctx, typeof_addr(ctx, addr))
    if qual == "Core.Union":
        u = ctx.value_at(addr, "jl_uniontype_t")
        flatten_union(ctx, member_u(u, "a"), parts, depth)
        flatten_union(ctx, member_u(u, "b"), parts, depth)
    else:
        parts.append(render_type(ctx, addr, depth))


def render_type(ctx, addr, depth=MAX_DEPTH):
    if addr == 0:
        return "#<null>"
    if depth < 0:
        return "…"
    dtaddr = typeof_addr(ctx, addr)
    if dtaddr == 0:
        return "<?type 0x%x>" % addr
    qual = datatype_qualname(ctx, dtaddr)
    if qual == "Core.TypeofBottom":
        return "Union{}"
    if qual == "Core.Union":
        parts = []
        flatten_union(ctx, addr, parts, depth - 1)
        return "Union{%s}" % ", ".join(parts)
    if qual == "Core.UnionAll":
        ua = ctx.value_at(addr, "jl_unionall_t")
        body = render_type(ctx, member_u(ua, "body"), depth - 1)
        var = render_typevar(ctx, member_u(ua, "var"), True)
        return "%s where %s" % (body, var)
    if qual == "Core.TypeVar":
        return render_typevar(ctx, addr, False)
    if qual == "Core.TypeofVararg":
        va = ctx.value_at(addr, "jl_vararg_t")
        t, n = member_u(va, "T"), member_u(va, "N")
        if t == 0:
            return "Vararg"
        if n == 0:
            return "Vararg{%s}" % render_type(ctx, t, depth - 1)
        return "Vararg{%s, %s}" % (render_type(ctx, t, depth - 1),
                                   render_value(ctx, n, depth - 1))
    if qual == "Core.Module":
        return module_path(ctx, addr)
    if qual != "Core.DataType":
        # a value used as a type parameter (1, :x, true, ...)
        return render_value(ctx, addr, depth)

    dt = ctx.value_at(addr, "jl_datatype_t")
    tn = ctx.value_at(member_u(dt, "name"), "jl_typename_t")
    modpath = module_path(ctx, member_u(tn, "module"))
    name = symbol_name(ctx, member_u(tn, "name"))
    if modpath not in ("Core", "Main") and not name.startswith("typeof("):
        name = modpath + "." + name
    params = member_u(dt, "parameters")
    nparams = svec_len(ctx, params) if params else 0
    if nparams == 0:
        return name + "{}" if name == "Tuple" else name
    # sugar: Array{T, 1} => Vector{T}, Array{T, 2} => Matrix{T}
    if name == "Array" and nparams == 2:
        ndim = render_type(ctx, svec_ref(ctx, params, 1), depth - 1)
        if ndim == "1":
            return "Vector{%s}" % render_type(ctx, svec_ref(ctx, params, 0),
                                              depth - 1)
        if ndim == "2":
            return "Matrix{%s}" % render_type(ctx, svec_ref(ctx, params, 0),
                                              depth - 1)
    # sugar: GenericMemory{:not_atomic, T, Core.CPU} => Memory{T}
    if name == "GenericMemory" and nparams == 3:
        order = svec_ref(ctx, params, 0)
        if is_cpu_addrspace(ctx, svec_ref(ctx, params, 2)):
            eltstr = render_type(ctx, svec_ref(ctx, params, 1), depth - 1)
            oname = symbol_name(ctx, order) if order else ""
            if oname == "not_atomic":
                return "Memory{%s}" % eltstr
            if oname == "atomic":
                return "AtomicMemory{%s}" % eltstr
    rendered = [render_type(ctx, svec_ref(ctx, params, i), depth - 1)
                for i in range(min(nparams, MAX_ELEMS))]
    if nparams > MAX_ELEMS:
        rendered.append("…")
    return "%s{%s}" % (name, ", ".join(rendered))


# --------------------------------------------------------------------------
# rendering of plain data (bits values, structs, arrays)
# --------------------------------------------------------------------------

PRIMITIVE_FMT = {
    "Core.Int8": ("i", 1), "Core.Int16": ("i", 2),
    "Core.Int32": ("i", 4), "Core.Int64": ("i", 8),
    "Core.UInt8": ("u", 1), "Core.UInt16": ("u", 2),
    "Core.UInt32": ("u", 4), "Core.UInt64": ("u", 8),
    "Core.Float16": ("f", 2), "Core.Float32": ("f", 4),
    "Core.Float64": ("f", 8),
}


def render_char(u):
    raw = u.to_bytes(4, "big").rstrip(b"\0") or b"\0"
    try:
        return "'%s'" % raw.decode("utf-8")
    except UnicodeDecodeError:
        return "Char(0x%08x)" % u


def render_primitive(ctx, qual, addr):
    fmt = PRIMITIVE_FMT.get(qual)
    if fmt is not None:
        kind, size = fmt
        if kind == "i":
            return str(ctx.read_uint(addr, size, signed=True))
        if kind == "u":
            return "0x%0*x" % (2 * size, ctx.read_uint(addr, size))
        return repr(struct.unpack("<" + {2: "e", 4: "f", 8: "d"}[size],
                                  ctx.read_mem(addr, size))[0])
    if qual == "Core.Bool":
        return "true" if ctx.read_uint(addr, 1) else "false"
    if qual == "Core.Char":
        return render_char(ctx.read_uint(addr, 4))
    return None


def layout_fields(ctx, dt):
    laddr = member_u(dt, "layout")
    if laddr == 0:
        return None
    layout = ctx.value_at(laddr, "jl_datatype_layout_t")
    nfields = member_u(layout, "nfields")
    fdkind = member_u(layout, "flags", "fielddesc_type")
    if fdkind == 3:  # foreign type: no descriptors
        return None
    fdname = ("jl_fielddesc8_t", "jl_fielddesc16_t", "jl_fielddesc32_t")[fdkind]
    fdsize = ctx.type_size(fdname)
    base = laddr + ctx.type_size("jl_datatype_layout_t")
    fields = []
    for i in range(nfields):
        fd = ctx.value_at(base + i * fdsize, fdname)
        fields.append((member_u(fd, "offset"), member_u(fd, "size"),
                       member_u(fd, "isptr")))
    return fields


def field_names(ctx, dt, nfields):
    names = []
    tnaddr = member_u(dt, "name")
    namesv = member_u(ctx.value_at(tnaddr, "jl_typename_t"), "names") \
        if tnaddr else 0
    n = svec_len(ctx, namesv) if namesv else 0
    for i in range(nfields):
        if i < n:
            sym = svec_ref(ctx, namesv, i)
            names.append(symbol_name(ctx, sym) if sym else str(i + 1))
        else:
            names.append(str(i + 1))
    return names


def field_types(ctx, dt, nfields):
    typesv = member_u(dt, "types")
    n = svec_len(ctx, typesv) if typesv else 0
    return [svec_ref(ctx, typesv, i) if i < n else 0 for i in range(nfields)]


def render_unboxed(ctx, taddr, addr, depth):
    if depth < 0:
        return "…"
    if taddr == 0:
        return "<?>"
    if datatype_qualname(ctx, typeof_addr(ctx, taddr)) != "Core.DataType":
        return "<union field>"
    qual = datatype_qualname(ctx, taddr)
    prim = render_primitive(ctx, qual, addr)
    if prim is not None:
        return prim
    dt = ctx.value_at(taddr, "jl_datatype_t")
    if member_u(dt, "isprimitivetype"):
        laddr = member_u(dt, "layout")
        size = member_u(ctx.value_at(laddr, "jl_datatype_layout_t"), "size") \
            if laddr else 0
        if 0 < size <= 8:
            return "%s(0x%0*x)" % (render_type(ctx, taddr, 1), 2 * size,
                                   ctx.read_uint(addr, size))
        return "%s(...)" % render_type(ctx, taddr, 1)
    fields = layout_fields(ctx, dt)
    if fields is None:
        return "<%s>" % render_type(ctx, taddr, depth - 1)
    if len(fields) == 0:
        inst = member_u(dt, "instance")
        return render_value(ctx, inst, depth) if inst \
            else render_type(ctx, taddr, depth - 1) + "()"
    return render_struct_inline(ctx, taddr, dt, addr, fields, depth)


def namedtuple_names(ctx, dt):
    try:
        params = member_u(dt, "parameters")
        if svec_len(ctx, params) != 2:
            return None
        namestup = svec_ref(ctx, params, 0)
        ndt = ctx.value_at(typeof_addr(ctx, namestup), "jl_datatype_t")
        fields = layout_fields(ctx, ndt)
        if fields is None:
            return None
        return [symbol_name(ctx, ctx.read_ptr(namestup + off))
                for (off, _, _) in fields]
    except MemoryReadError:
        return None


def render_struct_inline(ctx, taddr, dt, addr, fields, depth):
    tn = ctx.value_at(member_u(dt, "name"), "jl_typename_t")
    tname = symbol_name(ctx, member_u(tn, "name"))
    istuple = tname == "Tuple" and \
        module_path(ctx, member_u(tn, "module")) == "Core"
    names = None
    if tname == "NamedTuple":
        names = namedtuple_names(ctx, dt)
    if names is None and not istuple:
        names = field_names(ctx, dt, len(fields))
    ftypes = field_types(ctx, dt, len(fields))
    parts = []
    for i, (off, size, isptr) in enumerate(fields[:MAX_ELEMS]):
        if isptr:
            p = ctx.read_ptr(addr + off)
            r = render_value(ctx, p, depth - 1) if p else "#undef"
        else:
            r = render_unboxed(ctx, ftypes[i], addr + off, depth - 1)
        fname = names[i] if names and i < len(names) else str(i + 1)
        parts.append(r if istuple else "%s = %s" % (fname, r))
    if len(fields) > MAX_ELEMS:
        parts.append("…")
    if istuple and len(fields) == 1:
        return "(%s,)" % parts[0]
    if istuple or tname == "NamedTuple":
        return "(%s)" % ", ".join(parts)
    return "%s(%s)" % (render_type(ctx, taddr, 2), ", ".join(parts))


def array_info(ctx, addr):
    dtaddr = typeof_addr(ctx, addr)
    dt = ctx.value_at(dtaddr, "jl_datatype_t")
    tname = typename_of(ctx, dtaddr)
    if tname == "GenericMemory":
        mem_dt_addr = dtaddr
        mem = ctx.value_at(addr, "jl_genericmemory_t")
        dims = [member_u(mem, "length")]
        dataptr = member_u(mem, "ptr")
        eltype = svec_ref(ctx, member_u(dt, "parameters"), 1)
    else:
        arr = ctx.value_at(addr, "jl_array_t")
        mem_addr = member_u(arr, "ref", "mem")
        mem_dt_addr = typeof_addr(ctx, mem_addr)
        dataptr = member_u(arr, "ref", "ptr_or_offset")
        dimoff = ctx.field_offset("jl_array_t", "dimsize")
        params = member_u(dt, "parameters")
        try:
            ndims = ctx.read_uint(svec_ref(ctx, params, 1), 8)
        except MemoryReadError:
            ndims = 1
        if not (0 < ndims < 33):
            ndims = 1
        dims = [ctx.read_uint(addr + dimoff + i * ctx.ptrsize, ctx.ptrsize)
                for i in range(ndims)]
        eltype = svec_ref(ctx, params, 0)
    mlayout = ctx.value_at(
        member_u(ctx.value_at(mem_dt_addr, "jl_datatype_t"), "layout"),
        "jl_datatype_layout_t")
    elsize = member_u(mlayout, "size")
    isboxed = member_u(mlayout, "flags", "arrayelem_isboxed")
    isunion = member_u(mlayout, "flags", "arrayelem_isunion")
    return dims, eltype, dataptr, elsize, isboxed, isunion


def render_array(ctx, addr, depth):
    dims, eltype, dataptr, elsize, isboxed, isunion = array_info(ctx, addr)
    tstr = render_type(ctx, typeof_addr(ctx, addr), depth)
    if len(dims) == 1:
        summary = "%d-element %s" % (dims[0], tstr)
    else:
        summary = "%s %s" % ("×".join(str(d) for d in dims), tstr)
    if depth < MAX_DEPTH:
        return summary
    n = 1
    for d in dims:
        n *= d
    shown = min(n, MAX_ELEMS)
    parts = []
    for i in range(shown):
        if isboxed:
            p = ctx.read_ptr(dataptr + i * ctx.ptrsize)
            parts.append(render_value(ctx, p, depth - 1) if p else "#undef")
        elif isunion:
            parts.append("<union element>")
        else:
            parts.append(render_unboxed(ctx, eltype, dataptr + i * elsize,
                                        depth - 1))
    if n > shown:
        parts.append("… (%d more)" % (n - shown))
    return "%s = {%s}" % (summary, ", ".join(parts))


# --------------------------------------------------------------------------
# rendering of runtime objects (Method, MethodInstance, Task, ...)
# --------------------------------------------------------------------------

def render_sig_call(ctx, fname, sigaddr, depth):
    seen = 0
    while datatype_qualname(ctx, typeof_addr(ctx, sigaddr)) == \
            "Core.UnionAll" and seen < 32:
        sigaddr = member_u(ctx.value_at(sigaddr, "jl_unionall_t"), "body")
        seen += 1
    if datatype_qualname(ctx, typeof_addr(ctx, sigaddr)) != "Core.DataType":
        return "%s(...)" % fname
    params = member_u(ctx.value_at(sigaddr, "jl_datatype_t"), "parameters")
    n = svec_len(ctx, params) if params else 0
    args = ["::" + render_type(ctx, svec_ref(ctx, params, i), depth - 1)
            for i in range(1, min(n, MAX_ELEMS + 1))]
    if n > MAX_ELEMS + 1:
        args.append("…")
    return "%s(%s)" % (fname, ", ".join(args))


def render_method(ctx, addr, depth):
    m = ctx.value_at(addr, "jl_method_t")
    name = symbol_name(ctx, member_u(m, "name"))
    where = ""
    if member_u(m, "module"):
        where = " @ " + module_path(ctx, member_u(m, "module"))
    faddr = member_u(m, "file")
    if faddr:
        where += " %s:%d" % (symbol_name(ctx, faddr), member_i(m, "line"))
    return render_sig_call(ctx, name, member_u(m, "sig"), depth) + where


def render_method_instance(ctx, addr, depth):
    mi = ctx.value_at(addr, "jl_method_instance_t")
    defaddr = member_u(mi, "def", "value")
    name = "?"
    if defaddr:
        if datatype_qualname(ctx, typeof_addr(ctx, defaddr)) == "Core.Method":
            name = symbol_name(ctx, member_u(
                ctx.value_at(defaddr, "jl_method_t"), "name"))
        else:  # toplevel thunk: def is a module
            return "MethodInstance for top-level scope in " + \
                module_path(ctx, defaddr)
    return "MethodInstance for " + \
        render_sig_call(ctx, name, member_u(mi, "specTypes"), depth)


def render_code_instance(ctx, addr, depth):
    ci = ctx.value_at(addr, "jl_code_instance_t")
    defaddr = member_u(ci, "def")
    inner = "?"
    if defaddr:
        qual = datatype_qualname(ctx, typeof_addr(ctx, defaddr))
        if qual == "Core.ABIOverride":
            defaddr = member_u(ctx.value_at(defaddr, "jl_abi_override_t"),
                               "def")
            qual = datatype_qualname(ctx, typeof_addr(ctx, defaddr))
        if qual == "Core.MethodInstance":
            inner = render_method_instance(ctx, defaddr, depth - 1)
    return "CodeInstance for %s" % inner


TASK_STATES = {0: "runnable", 1: "done", 2: "failed", 3: "abandoned"}


def render_task(ctx, addr):
    t = ctx.value_at(addr, "jl_task_t")
    state = TASK_STATES.get(member_u(t, "_state"), "?")
    return "Task (%s) @0x%016x" % (state, addr)


def render_expr(ctx, addr, depth):
    e = ctx.value_at(addr, "jl_expr_t")
    head = symbol_name(ctx, member_u(e, "head")) if member_u(e, "head") \
        else "?"
    nargs = 0
    if member_u(e, "args"):
        try:
            nargs = array_info(ctx, member_u(e, "args"))[0][0]
        except MemoryReadError:
            pass
    return "Expr(:%s, <%d args>)" % (head, nargs)


def render_svec(ctx, addr, depth):
    n = svec_len(ctx, addr)
    parts = []
    for i in range(min(n, MAX_ELEMS)):
        p = svec_ref(ctx, addr, i)
        parts.append(render_value(ctx, p, depth - 1) if p else "#undef")
    if n > MAX_ELEMS:
        parts.append("…")
    return "svec(%s)" % ", ".join(parts)


# --------------------------------------------------------------------------
# main value renderer
# --------------------------------------------------------------------------

def render_value(ctx, addr, depth=MAX_DEPTH):
    if addr == 0:
        return "#<null>"
    if depth < 0:
        return "…"
    dtaddr = typeof_addr(ctx, addr)
    if dtaddr == 0:
        return "<julia value 0x%x>" % addr
    qual = datatype_qualname(ctx, dtaddr)

    if is_type_kind(qual):
        return render_type(ctx, addr, depth)
    prim = render_primitive(ctx, qual, addr)
    if prim is not None:
        return prim
    if qual == "Core.Nothing":
        return "nothing"
    if qual == "Core.Symbol":
        name = symbol_name(ctx, addr)
        if name.isidentifier():
            return ":" + name
        return 'Symbol("%s")' % escape_string(name)
    if qual == "Core.String":
        s, strlen = string_data(ctx, addr)
        suffix = "…" if strlen > len(s.encode("utf-8", "replace")) else ""
        return '"%s%s"' % (escape_string(s), suffix)
    if qual == "Core.SimpleVector":
        return render_svec(ctx, addr, depth)
    if qual == "Core.Module":
        return "Module %s" % module_path(ctx, addr)
    if qual == "Core.Task":
        return render_task(ctx, addr)
    if qual == "Core.Method":
        return render_method(ctx, addr, depth)
    if qual == "Core.MethodInstance":
        return render_method_instance(ctx, addr, depth)
    if qual == "Core.CodeInstance":
        return render_code_instance(ctx, addr, depth)
    if qual == "Core.Expr":
        return render_expr(ctx, addr, depth)
    if qual == "Core.GlobalRef":
        gr = ctx.value_at(addr, "jl_globalref_t")
        return "%s.%s" % (module_path(ctx, member_u(gr, "mod")),
                          symbol_name(ctx, member_u(gr, "name")))
    if qual == "Core.TypeName":
        tn = ctx.value_at(addr, "jl_typename_t")
        return "typename(%s)" % symbol_name(ctx, member_u(tn, "name"))

    dt = ctx.value_at(dtaddr, "jl_datatype_t")
    tname = typename_of(ctx, dtaddr)
    if tname in ("Array", "GenericMemory"):
        try:
            return render_array(ctx, addr, depth)
        except MemoryReadError:
            return render_type(ctx, dtaddr, depth - 1) + " <unreadable>"

    # generic instances
    if member_u(dt, "instance") == addr:
        tn = ctx.value_at(member_u(dt, "name"), "jl_typename_t")
        sname = member_u(tn, "singletonname") \
            if tn.GetChildMemberWithName("singletonname").IsValid() else 0
        return symbol_name(ctx, sname) if sname \
            else render_type(ctx, dtaddr, depth) + "()"
    if member_u(dt, "isprimitivetype"):
        return render_unboxed(ctx, dtaddr, addr, depth)
    fields = layout_fields(ctx, dt)
    if fields is not None:
        return render_struct_inline(ctx, dtaddr, dt, addr, fields, depth)
    return "%s @0x%016x" % (render_type(ctx, dtaddr, depth - 1), addr)


# --------------------------------------------------------------------------
# GC safepoint handling
#
# Julia's GC stops the world by mprotecting a page that every thread reads at
# safepoints, so during normal operation every thread takes a benign SIGSEGV
# whenever a GC runs. By default lldb stops on each of those, making stepping
# through Julia code nearly unusable. The scripted stop-hook below resumes
# any SIGSEGV whose faulting address lies inside the safepoint page region;
# real segfaults (including stack overflows) still stop the debugger.
# --------------------------------------------------------------------------

_FAULT_ADDR_RE = re.compile(r"fault address:?\s*(0x[0-9a-fA-F]+)")
_stop_hook_installed = [False]


def _global_uint(target, name):
    var = target.FindFirstGlobalVariable(name)
    if not var.IsValid():
        return 0
    return var.GetValueAsUnsigned()


class JLSafepointStopHook:
    enabled = True

    def __init__(self, target, extra_args, internal_dict):
        pass

    def handle_stop(self, exe_ctx, stream):
        """Return False to silently resume from GC safepoint SIGSEGVs."""
        if not JLSafepointStopHook.enabled:
            return True
        thread = exe_ctx.GetThread()
        if not thread.IsValid() or \
                thread.GetStopReason() != lldb.eStopReasonSignal:
            return True
        process = exe_ctx.GetProcess()
        signals = process.GetUnixSignals()
        segv = signals.GetSignalNumberFromName("SIGSEGV")
        if thread.GetStopReasonDataAtIndex(0) != segv:
            return True
        m = _FAULT_ADDR_RE.search(thread.GetStopDescription(1024) or "")
        if m is None:
            return True
        fault = int(m.group(1), 16)
        target = exe_ctx.GetTarget()
        base = _global_uint(target, "jl_safepoint_pages")
        pgsz = _global_uint(target, "jl_page_size")
        if base == 0 or pgsz == 0:
            return True
        # 4 pages, see the layout description in src/safepoint.c
        if base <= fault < base + 4 * pgsz:
            return False
        return True


def _install_stop_hook(debugger):
    if _stop_hook_installed[0]:
        return
    if debugger.GetNumTargets() == 0:
        return
    res = lldb.SBCommandReturnObject()
    debugger.GetCommandInterpreter().HandleCommand(
        "target stop-hook add -P %s.JLSafepointStopHook" % __name__, res)
    if res.Succeeded():
        _stop_hook_installed[0] = True


def jl_safepoint_filter_cmd(debugger, command, exe_ctx, result, internal_dict):
    """jl-safepoint-filter [on|off]: filter out GC safepoint SIGSEGVs.

    When on (the default), lldb silently resumes the benign SIGSEGVs Julia's
    GC uses to stop the world at safepoints, while still stopping on real
    segfaults. When off, lldb's default SIGSEGV behavior is restored."""
    arg = command.strip()
    if arg == "on":
        JLSafepointStopHook.enabled = True
        _install_stop_hook(debugger)
    elif arg == "off":
        JLSafepointStopHook.enabled = False
    elif arg:
        result.SetError("usage: jl-safepoint-filter [on|off]")
        return
    result.AppendMessage(
        "julia safepoint SIGSEGV filter is %s"
        % ("on" if JLSafepointStopHook.enabled and _stop_hook_installed[0]
           else "off"))


# --------------------------------------------------------------------------
# lldb integration
# --------------------------------------------------------------------------

JULIA_POINTER_TYPES = [
    "jl_value_t", "jl_function_t", "jl_sym_t", "jl_datatype_t",
    "jl_tupletype_t", "jl_typename_t", "jl_svec_t", "jl_module_t",
    "jl_array_t", "jl_genericmemory_t", "jl_string_t", "jl_task_t",
    "jl_method_t", "jl_method_instance_t", "jl_code_instance_t",
    "jl_code_info_t", "jl_expr_t", "jl_globalref_t", "jl_tvar_t",
    "jl_unionall_t", "jl_uniontype_t", "jl_vararg_t", "jl_typemap_t",
]


def jl_value_summary(valobj, internal_dict):
    """Type summary provider dispatching on the runtime type tag."""
    addr = valobj.GetValueAsUnsigned()
    if addr == 0:
        return "NULL"
    ctx = get_ctx(valobj)
    if ctx.process.IsValid():
        # opportunistically install the safepoint stop-hook once a real
        # process exists (importing from ~/.lldbinit happens before that)
        _install_stop_hook(valobj.GetTarget().GetDebugger())
    try:
        return render_value(ctx, addr)
    except MemoryReadError as e:
        return "<not a julia value: 0x%x (%s)>" % (addr, e)
    except Exception as e:  # never break `p` on a summary bug
        return "<error rendering julia value 0x%x: %s>" % (addr, e)


def jl_typeof_cmd(debugger, command, exe_ctx, result, internal_dict):
    """jl-typeof <expr>: print the Julia type of a jl_value_t* expression."""
    frame = exe_ctx.GetFrame()
    if not frame.IsValid():
        result.SetError("no frame selected")
        return
    val = frame.EvaluateExpression(command)
    if not val.IsValid() or val.GetError().Fail():
        result.SetError(str(val.GetError()))
        return
    ctx = get_ctx(val)
    try:
        result.AppendMessage(
            render_type(ctx, typeof_addr(ctx, val.GetValueAsUnsigned())))
    except MemoryReadError as e:
        result.SetError(str(e))


def __lldb_init_module(debugger, internal_dict):
    for tname in JULIA_POINTER_TYPES:
        debugger.HandleCommand(
            'type summary add --python-function %s.jl_value_summary "%s *"'
            % (__name__, tname))
    debugger.HandleCommand(
        "command script add -f %s.jl_typeof_cmd jl-typeof" % __name__)
    debugger.HandleCommand(
        "command script add -f %s.jl_safepoint_filter_cmd jl-safepoint-filter"
        % __name__)
    _install_stop_hook(debugger)
    print("julia_lldb: type summaries for Julia values installed "
          "(commands: jl-typeof, jl-safepoint-filter)")
