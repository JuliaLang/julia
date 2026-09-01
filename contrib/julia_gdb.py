# This file is a part of Julia. License is MIT: https://julialang.org/license

"""GDB pretty-printers for Julia runtime values.

Load this script into gdb to make `print` render `jl_value_t*` (and other
Julia runtime pointers such as `jl_datatype_t*`, `jl_sym_t*`, `jl_svec_t*`,
`jl_array_t*`, `jl_method_t*`, ...) the way Julia would show them, instead of
as raw addresses:

    (gdb) print v
    $1 = Symbol("foo")
    (gdb) print (jl_value_t*)some_array
    $2 = 3-element Vector{Int64} = {1, 2, 3}

Usage (pick one):
  * `source /path/to/julia/contrib/julia_gdb.py` inside gdb, or
  * add that line to your `~/.gdbinit`, or
  * `gdb -x /path/to/julia/contrib/julia_gdb.py --args ./julia ...`

The printers dispatch on the *runtime* type tag of the object, so a plain
`jl_value_t*` prints as whatever it actually is. Everything is resolved
through the debug info (DWARF) of libjulia-internal, so this script does not
hard-code struct offsets and should work across Julia versions; it requires a
build of Julia with debug info (the default) and gracefully degrades to raw
pointers when the debug info or the memory is unavailable.

A convenience function `$jl_typeof(v)` is also provided:

    (gdb) print $jl_typeof(v)
    $3 = Vector{Int64}

To temporarily see raw pointers again use `print/r`:

    (gdb) print/r v
    $4 = (jl_value_t *) 0x7f0f2c81f4d0
"""

import re
import struct

import gdb
import gdb.printing

# Render at most this many nested levels in a single summary string.
MAX_DEPTH = 3
# Render at most this many elements of arrays/svecs/tuples in a summary.
MAX_ELEMS = 10
# Truncate strings longer than this many bytes.
MAX_STRING = 200

# Objects whose type tag is a small constant rather than a pointer to the
# jl_datatype_t (see `enum jl_small_typeof_tags` in julia.h). Only used as a
# fallback when the `jl_small_typeof` symbol cannot be found; the list is
# append-only in the runtime so existing indices are stable.
SMALL_TAG_NAMES = [
    "#null",
    "TypeofBottom", "DataType", "UnionAll", "Union",
    "TypeofVararg", "TypeVar", "Symbol", "Module",
    "SimpleVector", "String", "Task",
    "Bool", "Nothing", "Char",
    "Int16", "Int32", "Int64", "Int8",
    "UInt16", "UInt32", "UInt64", "UInt8",
]


class _Cache:
    """Per-process caches for type lookups; flushed when objfiles change."""

    def __init__(self):
        self.clear()

    def clear(self):
        self.types = {}
        self.sizes = {}
        self.small_typeof = -1  # -1: not looked up yet; 0: unavailable
        self.ptrsize = None
        self.dt_names = {}  # jl_datatype_t* address -> qualified name


CACHE = _Cache()


def _clear_cache(event=None):
    CACHE.clear()


gdb.events.new_objfile.connect(_clear_cache)
if hasattr(gdb.events, "free_objfile"):
    gdb.events.free_objfile.connect(_clear_cache)
gdb.events.exited.connect(_clear_cache)


def lookup_type(name):
    t = CACHE.types.get(name)
    if t is None:
        t = gdb.lookup_type(name)
        CACHE.types[name] = t
    return t


def type_size(name):
    s = CACHE.sizes.get(name)
    if s is None:
        s = lookup_type(name).sizeof
        CACHE.sizes[name] = s
    return s


def ptr_size():
    if CACHE.ptrsize is None:
        CACHE.ptrsize = lookup_type("void").pointer().sizeof
    return CACHE.ptrsize


def read_mem(addr, size):
    return bytes(gdb.selected_inferior().read_memory(addr, size))


def read_uint(addr, size, signed=False):
    return int.from_bytes(read_mem(addr, size), "little", signed=signed)


def read_ptr(addr):
    return read_uint(addr, ptr_size())


def read_cstring(addr, maxlen=512):
    buf = read_mem(addr, maxlen)
    nul = buf.find(b"\0")
    if nul >= 0:
        buf = buf[:nul]
    return buf.decode("utf-8", errors="replace")


def value_at(addr, typename):
    """A gdb.Value of type `typename` located at `addr`."""
    return gdb.Value(addr).cast(lookup_type(typename).pointer()).dereference()


def small_typeof_addr():
    """Address of the runtime's jl_small_typeof table, or 0."""
    if CACHE.small_typeof == -1:
        CACHE.small_typeof = 0
        for sym in ("jl_small_typeof", "ijl_small_typeof"):
            try:
                CACHE.small_typeof = int(
                    gdb.parse_and_eval("(unsigned long)&" + sym))
                break
            except gdb.error:
                continue
    return CACHE.small_typeof


def typetag(addr):
    """The type tag of the object at addr: header word with GC bits masked."""
    return read_ptr(addr - ptr_size()) & ~15


def typeof_addr(addr):
    """Address of the jl_datatype_t for the object at addr (0 if unknown)."""
    tag = typetag(addr)
    if tag < (64 << 4):
        table = small_typeof_addr()
        if table == 0:
            return 0
        # entry lives at byte offset `tag` (see jl_to_typeof in julia.h)
        return read_ptr(table + tag)
    return tag


def symbol_name(addr):
    """The name of the jl_sym_t at addr."""
    return read_cstring(addr + type_size("jl_sym_t"))


def svec_len(addr):
    return int(value_at(addr, "jl_svec_t")["length"])


def svec_ref(addr, i):
    return read_ptr(addr + type_size("jl_svec_t") + i * ptr_size())


def is_cpu_addrspace(addr):
    """True when addr is an instance of Core.AddrSpace{Core} with value 0."""
    if addr == 0:
        return False
    try:
        dtaddr = typeof_addr(addr)
        if dtaddr == 0:
            return False
        dt = value_at(dtaddr, "jl_datatype_t")
        tname = symbol_name(int(value_at(int(dt["name"]),
                                         "jl_typename_t")["name"]))
        return tname == "AddrSpace" and read_uint(addr, 1) == 0
    except (gdb.error, gdb.MemoryError):
        return False


def module_path(addr, depth=0):
    """Dotted name of the jl_module_t at addr, e.g. "Base.Iterators"."""
    if addr == 0 or depth > 10:
        return "?"
    mod = value_at(addr, "jl_module_t")
    name = symbol_name(int(mod["name"]))
    parent = int(mod["parent"])
    if parent == 0 or parent == addr:
        return name
    pname = module_path(parent, depth + 1)
    if pname == "Main":
        return name
    return pname + "." + name


def datatype_qualname(dtaddr):
    """Qualified name of a jl_datatype_t, e.g. "Core.Int64". Cached, since
    datatypes are interned and long-lived."""
    name = CACHE.dt_names.get(dtaddr)
    if name is None:
        dt = value_at(dtaddr, "jl_datatype_t")
        tn = value_at(int(dt["name"]), "jl_typename_t")
        name = module_path(int(tn["module"])) + "." + symbol_name(int(tn["name"]))
        CACHE.dt_names[dtaddr] = name
    return name


def string_data(addr):
    """(contents, length) of the Core.String at addr."""
    strlen = read_uint(addr, ptr_size())
    n = min(strlen, MAX_STRING)
    s = read_mem(addr + ptr_size(), n).decode("utf-8", errors="replace")
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


def render_typevar(addr, with_bounds):
    tv = value_at(addr, "jl_tvar_t")
    name = symbol_name(int(tv["name"]))
    if not with_bounds:
        return name
    lb = render_type(int(tv["lb"]), MAX_DEPTH - 1)
    ub = render_type(int(tv["ub"]), MAX_DEPTH - 1)
    if lb == "Union{}" and ub == "Any":
        return name
    if lb == "Union{}":
        return "%s<:%s" % (name, ub)
    return "%s<:%s<:%s" % (lb, name, ub)


def flatten_union(addr, parts, depth):
    qual = datatype_qualname(typeof_addr(addr))
    if qual == "Core.Union":
        u = value_at(addr, "jl_uniontype_t")
        flatten_union(int(u["a"]), parts, depth)
        flatten_union(int(u["b"]), parts, depth)
    else:
        parts.append(render_type(addr, depth))


def render_type(addr, depth=MAX_DEPTH):
    """Render a Julia type object (or type parameter) as a string."""
    if addr == 0:
        return "#<null>"
    if depth < 0:
        return "…"
    dtaddr = typeof_addr(addr)
    if dtaddr == 0:
        return "<?type 0x%x>" % addr
    qual = datatype_qualname(dtaddr)
    if qual == "Core.TypeofBottom":
        return "Union{}"
    if qual == "Core.Union":
        parts = []
        flatten_union(addr, parts, depth - 1)
        return "Union{%s}" % ", ".join(parts)
    if qual == "Core.UnionAll":
        ua = value_at(addr, "jl_unionall_t")
        body = render_type(int(ua["body"]), depth - 1)
        var = render_typevar(int(ua["var"]), True)
        return "%s where %s" % (body, var)
    if qual == "Core.TypeVar":
        return render_typevar(addr, False)
    if qual == "Core.TypeofVararg":
        va = value_at(addr, "jl_vararg_t")
        t, n = int(va["T"]), int(va["N"])
        if t == 0:
            return "Vararg"
        if n == 0:
            return "Vararg{%s}" % render_type(t, depth - 1)
        return "Vararg{%s, %s}" % (render_type(t, depth - 1),
                                   render_value(n, depth - 1))
    if qual == "Core.Module":
        return module_path(addr)
    if qual != "Core.DataType":
        # a value used as a type parameter (1, :x, true, ...)
        return render_value(addr, depth)

    dt = value_at(addr, "jl_datatype_t")
    tn = value_at(int(dt["name"]), "jl_typename_t")
    modpath = module_path(int(tn["module"]))
    name = symbol_name(int(tn["name"]))
    if modpath not in ("Core", "Main") and not name.startswith("typeof("):
        name = modpath + "." + name
    params = int(dt["parameters"])
    nparams = svec_len(params) if params else 0
    if nparams == 0:
        return name + "{}" if name == "Tuple" else name
    # sugar: Array{T, 1} => Vector{T}, Array{T, 2} => Matrix{T}
    if name == "Array" and nparams == 2:
        ndim = render_type(svec_ref(params, 1), depth - 1)
        if ndim == "1":
            return "Vector{%s}" % render_type(svec_ref(params, 0), depth - 1)
        if ndim == "2":
            return "Matrix{%s}" % render_type(svec_ref(params, 0), depth - 1)
    # sugar: GenericMemory{:not_atomic, T, Core.CPU} => Memory{T}
    if name == "GenericMemory" and nparams == 3:
        order = svec_ref(params, 0)
        aspace = svec_ref(params, 2)
        if is_cpu_addrspace(aspace):
            eltstr = render_type(svec_ref(params, 1), depth - 1)
            oname = symbol_name(order) if order else ""
            if oname == "not_atomic":
                return "Memory{%s}" % eltstr
            if oname == "atomic":
                return "AtomicMemory{%s}" % eltstr
    rendered = [render_type(svec_ref(params, i), depth - 1)
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
    # Char stores the UTF-8 bytes left-aligned in a UInt32
    raw = u.to_bytes(4, "big").rstrip(b"\0") or b"\0"
    try:
        return "'%s'" % raw.decode("utf-8")
    except UnicodeDecodeError:
        return "Char(0x%08x)" % u


def render_primitive(qual, addr):
    """Render the unboxed primitive of type `qual` stored at addr, or None."""
    fmt = PRIMITIVE_FMT.get(qual)
    if fmt is not None:
        kind, size = fmt
        if kind == "i":
            return str(read_uint(addr, size, signed=True))
        if kind == "u":
            return "0x%0*x" % (2 * size, read_uint(addr, size))
        return repr(struct.unpack("<" + {2: "e", 4: "f", 8: "d"}[size],
                                  read_mem(addr, size))[0])
    if qual == "Core.Bool":
        return "true" if read_uint(addr, 1) else "false"
    if qual == "Core.Char":
        return render_char(read_uint(addr, 4))
    return None


def layout_fields(dt):
    """[(offset, size, isptr), ...] for the concrete jl_datatype_t value dt,
    or None when there is no field table (opaque/foreign layouts)."""
    laddr = int(dt["layout"])
    if laddr == 0:
        return None
    layout = value_at(laddr, "jl_datatype_layout_t")
    nfields = int(layout["nfields"])
    fdkind = int(layout["flags"]["fielddesc_type"])
    if fdkind == 3:  # foreign type: no descriptors
        return None
    fdname = ("jl_fielddesc8_t", "jl_fielddesc16_t", "jl_fielddesc32_t")[fdkind]
    fdsize = type_size(fdname)
    base = laddr + type_size("jl_datatype_layout_t")
    fields = []
    for i in range(nfields):
        fd = value_at(base + i * fdsize, fdname)
        fields.append((int(fd["offset"]), int(fd["size"]), int(fd["isptr"])))
    return fields


def field_names(dt, nfields):
    names = []
    tnaddr = int(dt["name"])
    namesv = int(value_at(tnaddr, "jl_typename_t")["names"]) if tnaddr else 0
    n = svec_len(namesv) if namesv else 0
    for i in range(nfields):
        if i < n:
            sym = svec_ref(namesv, i)
            names.append(symbol_name(sym) if sym else str(i + 1))
        else:
            names.append(str(i + 1))
    return names


def field_types(dt, nfields):
    typesv = int(dt["types"])
    n = svec_len(typesv) if typesv else 0
    return [svec_ref(typesv, i) if i < n else 0 for i in range(nfields)]


def render_unboxed(taddr, addr, depth):
    """Render the unboxed (inline-stored) value of type taddr at addr."""
    if depth < 0:
        return "…"
    if taddr == 0:
        return "<?>"
    if datatype_qualname(typeof_addr(taddr)) != "Core.DataType":
        return "<union field>"
    qual = datatype_qualname(taddr)
    prim = render_primitive(qual, addr)
    if prim is not None:
        return prim
    dt = value_at(taddr, "jl_datatype_t")
    if int(dt["isprimitivetype"]):
        laddr = int(dt["layout"])
        size = int(value_at(laddr, "jl_datatype_layout_t")["size"]) if laddr else 0
        if 0 < size <= 8:
            return "%s(0x%0*x)" % (render_type(taddr, 1), 2 * size,
                                   read_uint(addr, size))
        return "%s(...)" % render_type(taddr, 1)
    fields = layout_fields(dt)
    if fields is None:
        return "<%s>" % render_type(taddr, depth - 1)
    if len(fields) == 0:
        return render_value(int(dt["instance"]), depth) if int(dt["instance"]) \
            else render_type(taddr, depth - 1) + "()"
    return render_struct_inline(taddr, dt, addr, fields, depth)


def render_struct_inline(taddr, dt, addr, fields, depth):
    tn = value_at(int(dt["name"]), "jl_typename_t")
    tname = symbol_name(int(tn["name"]))
    istuple = tname == "Tuple" and module_path(int(tn["module"])) == "Core"
    names = None
    if tname == "NamedTuple":
        names = namedtuple_names(dt)
    if names is None and not istuple:
        names = field_names(dt, len(fields))
    ftypes = field_types(dt, len(fields))
    parts = []
    for i, (off, size, isptr) in enumerate(fields[:MAX_ELEMS]):
        if isptr:
            p = read_ptr(addr + off)
            r = render_value(p, depth - 1) if p else "#undef"
        else:
            r = render_unboxed(ftypes[i], addr + off, depth - 1)
        fname = names[i] if names and i < len(names) else str(i + 1)
        parts.append(r if istuple else "%s = %s" % (fname, r))
    if len(fields) > MAX_ELEMS:
        parts.append("…")
    if istuple and len(fields) == 1:
        return "(%s,)" % parts[0]
    if istuple or tname == "NamedTuple":
        return "(%s)" % ", ".join(parts)
    return "%s(%s)" % (render_type(taddr, 2), ", ".join(parts))


def namedtuple_names(dt):
    """Field names of a concrete NamedTuple type, from its first type
    parameter (a tuple of symbols), or None."""
    try:
        params = int(dt["parameters"])
        if svec_len(params) != 2:
            return None
        namestup = svec_ref(params, 0)
        ndt = value_at(typeof_addr(namestup), "jl_datatype_t")
        fields = layout_fields(ndt)
        if fields is None:
            return None
        return [symbol_name(read_ptr(namestup + off))
                for (off, _, _) in fields]
    except (gdb.error, gdb.MemoryError):
        return None


def array_info(addr):
    """(dims, eltype, dataptr, elsize, isboxed, isunion) of the jl_array_t (or
    Memory) at addr; eltype and layout come from the underlying Memory."""
    dtaddr = typeof_addr(addr)
    dt = value_at(dtaddr, "jl_datatype_t")
    tname = symbol_name(int(value_at(int(dt["name"]), "jl_typename_t")["name"]))
    if tname == "GenericMemory":
        mem_addr = addr
        mem_dt_addr = dtaddr
        mem = value_at(addr, "jl_genericmemory_t")
        dims = [int(mem["length"])]
        dataptr = int(mem["ptr"])
        eltype = svec_ref(int(dt["parameters"]), 1)
    else:
        arr = value_at(addr, "jl_array_t")
        mem_addr = int(arr["ref"]["mem"])
        mem_dt_addr = typeof_addr(mem_addr)
        dataptr = int(arr["ref"]["ptr_or_offset"])
        dimoff = lookup_type("jl_array_t")["dimsize"].bitpos // 8
        params = int(dt["parameters"])
        try:
            ndims = read_uint(svec_ref(params, 1), 8)
        except (gdb.error, gdb.MemoryError):
            ndims = 1
        if not (0 < ndims < 33):
            ndims = 1
        dims = [read_uint(addr + dimoff + i * ptr_size(), ptr_size())
                for i in range(ndims)]
        eltype = svec_ref(params, 0)
    mlayout = value_at(int(value_at(mem_dt_addr, "jl_datatype_t")["layout"]),
                       "jl_datatype_layout_t")
    elsize = int(mlayout["size"])
    isboxed = int(mlayout["flags"]["arrayelem_isboxed"])
    isunion = int(mlayout["flags"]["arrayelem_isunion"])
    return dims, eltype, dataptr, elsize, isboxed, isunion


def render_array_summary(addr, depth):
    dims, _, _, _, _, _ = array_info(addr)
    tstr = render_type(typeof_addr(addr), depth)
    if len(dims) == 1:
        return "%d-element %s" % (dims[0], tstr)
    return "%s %s" % ("×".join(str(d) for d in dims), tstr)


def array_children(addr):
    dims, eltype, dataptr, elsize, isboxed, isunion = array_info(addr)
    n = 1
    for d in dims:
        n *= d
    shown = min(n, MAX_ELEMS)
    for i in range(shown):
        if isboxed:
            p = read_ptr(dataptr + i * ptr_size())
            yield str(i + 1), jl_value(p) if p else "#undef"
        elif isunion:
            yield str(i + 1), "<union element>"
        else:
            yield str(i + 1), render_unboxed(eltype, dataptr + i * elsize,
                                             MAX_DEPTH - 1)
    if n > shown:
        yield "...", "(%d more)" % (n - shown)


def jl_value(addr):
    """A gdb.Value holding addr as a jl_value_t*."""
    return gdb.Value(addr).cast(lookup_type("jl_value_t").pointer())


# --------------------------------------------------------------------------
# rendering of runtime objects (Method, MethodInstance, Task, ...)
# --------------------------------------------------------------------------

def render_sig_call(fname, sigaddr, depth):
    """Render "f(::T, ::S)" from a Tuple{typeof(f), T, S} signature."""
    # peel UnionAll wrappers
    seen = 0
    while datatype_qualname(typeof_addr(sigaddr)) == "Core.UnionAll" and seen < 32:
        sigaddr = int(value_at(sigaddr, "jl_unionall_t")["body"])
        seen += 1
    if datatype_qualname(typeof_addr(sigaddr)) != "Core.DataType":
        return "%s(...)" % fname
    params = int(value_at(sigaddr, "jl_datatype_t")["parameters"])
    n = svec_len(params) if params else 0
    args = ["::" + render_type(svec_ref(params, i), depth - 1)
            for i in range(1, min(n, MAX_ELEMS + 1))]
    if n > MAX_ELEMS + 1:
        args.append("…")
    return "%s(%s)" % (fname, ", ".join(args))


def render_method(addr, depth):
    m = value_at(addr, "jl_method_t")
    name = symbol_name(int(m["name"]))
    where = ""
    if int(m["module"]):
        where = " @ " + module_path(int(m["module"]))
    faddr = int(m["file"])
    if faddr:
        where += " %s:%d" % (symbol_name(faddr), int(m["line"]))
    return render_sig_call(name, int(m["sig"]), depth) + where


def render_method_instance(addr, depth):
    mi = value_at(addr, "jl_method_instance_t")
    defaddr = int(mi["def"]["value"])
    name = "?"
    if defaddr:
        if datatype_qualname(typeof_addr(defaddr)) == "Core.Method":
            name = symbol_name(int(value_at(defaddr, "jl_method_t")["name"]))
        else:  # toplevel thunk: def is a module
            name = "top-level scope in " + module_path(defaddr)
            return "MethodInstance for " + name
    return "MethodInstance for " + render_sig_call(name, int(mi["specTypes"]),
                                                   depth)


def render_code_instance(addr, depth):
    ci = value_at(addr, "jl_code_instance_t")
    defaddr = int(ci["def"])
    inner = "?"
    if defaddr:
        qual = datatype_qualname(typeof_addr(defaddr))
        if qual == "Core.ABIOverride":
            defaddr = int(value_at(defaddr, "jl_abi_override_t")["def"])
            qual = datatype_qualname(typeof_addr(defaddr))
        if qual == "Core.MethodInstance":
            inner = render_method_instance(defaddr, depth - 1)
    return "CodeInstance for %s" % inner


TASK_STATES = {0: "runnable", 1: "done", 2: "failed", 3: "abandoned"}


def render_task(addr):
    t = value_at(addr, "jl_task_t")
    state = TASK_STATES.get(int(t["_state"]), "?")
    return "Task (%s) @0x%016x" % (state, addr)


def render_expr(addr, depth):
    e = value_at(addr, "jl_expr_t")
    head = symbol_name(int(e["head"])) if int(e["head"]) else "?"
    nargs = 0
    if int(e["args"]):
        try:
            nargs = array_info(int(e["args"]))[0][0]
        except (gdb.error, gdb.MemoryError):
            pass
    return "Expr(:%s, <%d args>)" % (head, nargs)


def render_svec(addr, depth):
    n = svec_len(addr)
    parts = []
    for i in range(min(n, MAX_ELEMS)):
        p = svec_ref(addr, i)
        parts.append(render_value(p, depth - 1) if p else "#undef")
    if n > MAX_ELEMS:
        parts.append("…")
    return "svec(%s)" % ", ".join(parts)


# --------------------------------------------------------------------------
# main value renderer
# --------------------------------------------------------------------------

def render_value(addr, depth=MAX_DEPTH):
    """Render the Julia value at addr as a compact single-line string."""
    if addr == 0:
        return "#<null>"
    if depth < 0:
        return "…"
    dtaddr = typeof_addr(addr)
    if dtaddr == 0:
        tag = typetag(addr)
        idx = tag >> 4
        if tag < (64 << 4) and idx < len(SMALL_TAG_NAMES):
            return "<%s 0x%x>" % (SMALL_TAG_NAMES[idx], addr)
        return "<julia value 0x%x>" % addr
    qual = datatype_qualname(dtaddr)

    if is_type_kind(qual):
        return render_type(addr, depth)
    prim = render_primitive(qual, addr)
    if prim is not None:
        return prim
    if qual == "Core.Nothing":
        return "nothing"
    if qual == "Core.Symbol":
        name = symbol_name(addr)
        if name.isidentifier():
            return ":" + name
        return 'Symbol("%s")' % escape_string(name)
    if qual == "Core.String":
        s, strlen = string_data(addr)
        suffix = "…" if strlen > len(s.encode("utf-8", "replace")) else ""
        return '"%s%s"' % (escape_string(s), suffix)
    if qual == "Core.SimpleVector":
        return render_svec(addr, depth)
    if qual == "Core.Module":
        return "Module %s" % module_path(addr)
    if qual == "Core.Task":
        return render_task(addr)
    if qual == "Core.Method":
        return render_method(addr, depth)
    if qual == "Core.MethodInstance":
        return render_method_instance(addr, depth)
    if qual == "Core.CodeInstance":
        return render_code_instance(addr, depth)
    if qual == "Core.Expr":
        return render_expr(addr, depth)
    if qual == "Core.GlobalRef":
        gr = value_at(addr, "jl_globalref_t")
        return "%s.%s" % (module_path(int(gr["mod"])),
                          symbol_name(int(gr["name"])))
    if qual == "Core.TypeName":
        tn = value_at(addr, "jl_typename_t")
        return "typename(%s)" % symbol_name(int(tn["name"]))

    dt = value_at(dtaddr, "jl_datatype_t")
    tname = symbol_name(int(value_at(int(dt["name"]), "jl_typename_t")["name"]))
    if tname in ("Array", "GenericMemory"):
        try:
            summary = render_array_summary(addr, depth - 1)
            parts = ["%s" % v if isinstance(v, str) else render_value(int(v), depth - 1)
                     for _, v in array_children(addr)]
            return "%s = {%s}" % (summary, ", ".join(parts)) if depth == MAX_DEPTH \
                else "%s" % summary
        except (gdb.error, gdb.MemoryError):
            return render_type(dtaddr, depth - 1) + " <unreadable>"

    # generic instances
    if int(dt["instance"]) == addr:
        tn = value_at(int(dt["name"]), "jl_typename_t")
        sname = int(tn["singletonname"]) if "singletonname" in \
            (f.name for f in lookup_type("jl_typename_t").fields()) else 0
        return symbol_name(sname) if sname else render_type(dtaddr, depth) + "()"
    if int(dt["isprimitivetype"]):
        return render_unboxed(dtaddr, addr, depth)
    fields = layout_fields(dt)
    if fields is not None:
        return render_struct_inline(dtaddr, dt, addr, fields, depth)
    return "%s @0x%016x" % (render_type(dtaddr, depth - 1), addr)


# --------------------------------------------------------------------------
# gdb integration
# --------------------------------------------------------------------------

# struct tags / typedef names whose pointers we pretty-print
JULIA_POINTER_TYPES = {
    "jl_value_t", "_jl_value_t",
    "jl_function_t",
    "jl_sym_t", "_jl_sym_t",
    "jl_datatype_t", "_jl_datatype_t", "jl_tupletype_t",
    "jl_typename_t",
    "jl_svec_t",
    "jl_module_t", "_jl_module_t",
    "jl_array_t", "_jl_array_t",
    "jl_genericmemory_t", "_jl_genericmemory_t",
    "jl_string_t",
    "jl_task_t", "_jl_task_t",
    "jl_method_t", "_jl_method_t",
    "jl_method_instance_t", "_jl_method_instance_t",
    "jl_code_instance_t", "_jl_code_instance_t",
    "jl_code_info_t", "_jl_code_info_t",
    "jl_expr_t",
    "jl_globalref_t", "_jl_globalref_t",
    "jl_tvar_t",
    "jl_unionall_t",
    "jl_uniontype_t",
    "jl_vararg_t", "_jl_vararg_t",
    "jl_typemap_t",
}

# runtime types whose printer exposes expandable children
CHILDREN_KINDS = ("Core.SimpleVector",)


class JuliaValuePrinter:
    """to_string-based printer: dispatches on the runtime type tag."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        try:
            addr = int(self.val)
        except gdb.error:
            return None
        if addr == 0:
            return "(jl_value_t *) NULL"
        try:
            return render_value(addr)
        except (gdb.error, gdb.MemoryError) as e:
            return "<not a julia value: 0x%x (%s)>" % (addr, e)
        except Exception as e:  # never break `print` on a printer bug
            return "<error rendering julia value 0x%x: %s>" % (addr, e)


class JuliaArrayPrinter(JuliaValuePrinter):
    """Adds expandable children for arrays so IDE variable views can drill
    down into elements."""

    def to_string(self):
        try:
            return render_array_summary(int(self.val), MAX_DEPTH)
        except (gdb.error, gdb.MemoryError):
            return JuliaValuePrinter.to_string(self)

    def children(self):
        try:
            for name, v in array_children(int(self.val)):
                yield name, str(v) if not isinstance(v, gdb.Value) else v
        except (gdb.error, gdb.MemoryError):
            return

    def display_hint(self):
        return "array"


def is_array_value(addr):
    try:
        dtaddr = typeof_addr(addr)
        if dtaddr == 0:
            return False
        dt = value_at(dtaddr, "jl_datatype_t")
        tname = symbol_name(int(value_at(int(dt["name"]),
                                         "jl_typename_t")["name"]))
        return tname in ("Array", "GenericMemory")
    except (gdb.error, gdb.MemoryError):
        return False


class JuliaPrettyPrinter(gdb.printing.PrettyPrinter):
    def __init__(self):
        super().__init__("julia")

    def __call__(self, val):
        t = val.type
        if t.code != gdb.TYPE_CODE_PTR:
            return None
        target = t.target()
        name = target.name
        stripped = target.strip_typedefs()
        tag = stripped.tag or stripped.name
        if name not in JULIA_POINTER_TYPES and tag not in JULIA_POINTER_TYPES:
            return None
        try:
            if int(val) == 0:
                return None
        except gdb.error:
            return None
        try:
            if is_array_value(int(val)):
                return JuliaArrayPrinter(val)
        except (gdb.error, gdb.MemoryError):
            pass
        return JuliaValuePrinter(val)


class JlTypeofFunction(gdb.Function):
    """$jl_typeof(v): the jl_datatype_t* of a Julia value.

    Usage: print $jl_typeof(v)"""

    def __init__(self):
        super().__init__("jl_typeof")

    def invoke(self, v):
        addr = typeof_addr(int(v))
        return gdb.Value(addr).cast(lookup_type("jl_datatype_t").pointer())


# --------------------------------------------------------------------------
# GC safepoint handling
#
# Julia's GC stops the world by mprotecting a page that every thread reads at
# safepoints, so during normal operation every thread takes a benign SIGSEGV
# whenever a GC runs. By default gdb stops on each of those, making stepping
# through Julia code nearly unusable. Instead of gdb's all-or-nothing `handle
# SIGSEGV`, we tell gdb not to stop on SIGSEGV in general and install a
# conditional signal catchpoint that only fires when the faulting address is
# *outside* the safepoint page region: safepoint hits are resumed silently,
# real segfaults (including stack overflows) still stop the debugger.
# --------------------------------------------------------------------------

# 4 pages, see the layout description in src/safepoint.c
SAFEPOINT_COND = (
    "!((unsigned long)$_siginfo._sifields._sigfault.si_addr"
    " >= (unsigned long)jl_safepoint_pages"
    " && (unsigned long)$_siginfo._sifields._sigfault.si_addr"
    " < (unsigned long)jl_safepoint_pages"
    " + 4*(unsigned long)jl_page_size)")

_segv_catchpoint = [None]
_segv_cond_armed = [False]


def _try_arm_segv_condition(event=None):
    """Attach the safepoint condition to the SIGSEGV catchpoint. This can
    only succeed once libjulia-internal's symbols are available, so it is
    retried every time an objfile is loaded."""
    if _segv_catchpoint[0] is None or _segv_cond_armed[0]:
        return
    try:
        gdb.execute("condition %d %s" % (_segv_catchpoint[0], SAFEPOINT_COND),
                    to_string=True)
        _segv_cond_armed[0] = True
    except gdb.error:
        pass


gdb.events.new_objfile.connect(_try_arm_segv_condition)


def enable_safepoint_filter():
    if _segv_catchpoint[0] is not None:
        return
    gdb.execute("handle SIGSEGV nostop noprint pass", to_string=True)
    out = gdb.execute("catch signal SIGSEGV", to_string=True)
    m = re.search(r"Catchpoint (\d+)", out)
    if m is None:
        return
    _segv_catchpoint[0] = int(m.group(1))
    _try_arm_segv_condition()


def disable_safepoint_filter():
    if _segv_catchpoint[0] is None:
        return
    gdb.execute("delete %d" % _segv_catchpoint[0], to_string=True)
    gdb.execute("handle SIGSEGV stop print pass", to_string=True)
    _segv_catchpoint[0] = None
    _segv_cond_armed[0] = False


class JlSafepointCommand(gdb.Command):
    """jl-safepoint-filter [on|off]: filter out GC safepoint SIGSEGVs.

    When on (the default), gdb silently resumes the benign SIGSEGVs Julia's
    GC uses to stop the world at safepoints, while still stopping on real
    segfaults. When off, gdb's default SIGSEGV behavior is restored."""

    def __init__(self):
        super().__init__("jl-safepoint-filter", gdb.COMMAND_RUNNING)

    def invoke(self, arg, from_tty):
        arg = arg.strip()
        if arg == "on":
            enable_safepoint_filter()
        elif arg == "off":
            disable_safepoint_filter()
        elif arg:
            raise gdb.GdbError("usage: jl-safepoint-filter [on|off]")
        print("julia safepoint SIGSEGV filter is %s"
              % ("on" if _segv_catchpoint[0] is not None else "off"))


class JlHandleSignalsCommand(gdb.Command):
    """jl-handle-signals: tell gdb to ignore signals Julia uses internally.

    Runs `handle nostop noprint pass` for SIGSEGV and SIGUSR2 (used by the GC
    safepoint and the profiler/thread wakeups). This is a bigger hammer than
    the default `jl-safepoint-filter`: gdb will no longer stop on *any*
    segfault (Julia's own crash handler still reports them), which can be
    useful when the inferior takes many signals, e.g. while profiling."""

    def __init__(self):
        super().__init__("jl-handle-signals", gdb.COMMAND_RUNNING)

    def invoke(self, arg, from_tty):
        disable_safepoint_filter()
        gdb.execute("handle SIGSEGV nostop noprint pass")
        gdb.execute("handle SIGUSR2 nostop noprint pass")


def register(obj=None):
    gdb.printing.register_pretty_printer(obj, JuliaPrettyPrinter(),
                                         replace=True)
    JlTypeofFunction()
    JlSafepointCommand()
    JlHandleSignalsCommand()
    try:
        enable_safepoint_filter()
    except gdb.error:
        pass


register(gdb.current_objfile())
