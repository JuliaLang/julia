# This file is a part of Julia. License is MIT: https://julialang.org/license

"""Debugger-agnostic rendering and inspection of Julia runtime values.

This module contains the logic shared by the gdb (`julia_gdb.py`) and lldb
(`julia_lldb.py`) extensions: rendering of runtime values from their type
tags, Julia-semantics field/index access for the `jl` command, and module
binding lookup. It must be kept next to those files — they import it by path.

Nothing here depends on a specific debugger. All access goes through an
adapter object providing:

    ptrsize                              pointer size in bytes
    read_mem(addr, n) -> bytes           raise JLDebugError on failure
    read_cstr(addr, maxlen) -> str
    type_size(typename) -> int           sizeof from debug info
    field_offset(typename, field) -> int offsetof from debug info
    field(addr, typename, path, signed=False) -> int
        the integer value of (possibly nested, possibly bitfield) member
        `path` (a tuple of names) of the struct `typename` at `addr`
    has_field(typename, field) -> bool
    global_addr(symbol) -> int           load address of a global, 0 if absent

Struct layouts are resolved through the debug info rather than hard-coded, so
this tracks Julia struct changes automatically. The only baked-in knowledge
is the type-tag scheme and the layouts of symbols/strings/svecs (stable ABI),
and the binding-partition kinds for module lookups.
"""

import re

# Render at most this many nested levels in a single summary string.
MAX_DEPTH = 3
# Render at most this many elements of arrays/svecs/tuples in a summary.
MAX_ELEMS = 10
# Truncate strings longer than this many bytes.
MAX_STRING = 200
# Hard cap on the total size of a rendered summary.
MAX_OUTPUT = 4096
# Smaller cap for values rendered as part of an enclosing expansion (e.g.
# each element the debugger prints while expanding an array's children).
BRIEF_OUTPUT = 512

# The GC safepoint region is this many pages at jl_safepoint_pages
# (see the layout description in src/safepoint.c).
SAFEPOINT_PAGES = 4


class JLDebugError(Exception):
    """A memory read / debug info lookup failed, or a `jl` path is invalid."""


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

PRIMITIVE_FMT = {
    "Core.Int8": ("i", 1), "Core.Int16": ("i", 2),
    "Core.Int32": ("i", 4), "Core.Int64": ("i", 8),
    "Core.UInt8": ("u", 1), "Core.UInt16": ("u", 2),
    "Core.UInt32": ("u", 4), "Core.UInt64": ("u", 8),
    "Core.Float16": ("f", 2), "Core.Float32": ("f", 4),
    "Core.Float64": ("f", 8),
}

# C type equivalents of unboxed primitive fields, for debuggers that want to
# hand back a typed native value.
PRIMITIVE_CTYPES = {
    "Core.Int8": "int8_t", "Core.Int16": "int16_t",
    "Core.Int32": "int32_t", "Core.Int64": "int64_t",
    "Core.UInt8": "uint8_t", "Core.UInt16": "uint16_t",
    "Core.UInt32": "uint32_t", "Core.UInt64": "uint64_t",
    "Core.Float32": "float", "Core.Float64": "double",
    "Core.Bool": "unsigned char", "Core.Char": "uint32_t",
}

TASK_STATES = {0: "runnable", 1: "done", 2: "failed", 3: "abandoned"}

# see enum jl_partition_kind in julia.h
PARTITION_KINDS_CONST = (0x0, 0x1, 0x4, 0xb)   # restriction is the value
PARTITION_KINDS_IMPORT = (0x3, 0x5, 0x6)       # restriction is a binding
PARTITION_KINDS_GLOBAL = (0x2, 0x8)            # value lives in binding->value


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


def render_char(u):
    # Char stores the UTF-8 bytes left-aligned in a UInt32
    raw = u.to_bytes(4, "big").rstrip(b"\0") or b"\0"
    try:
        return "'%s'" % raw.decode("utf-8")
    except UnicodeDecodeError:
        return "Char(0x%08x)" % u


def is_type_kind(qual):
    return qual in ("Core.DataType", "Core.Union", "Core.UnionAll",
                    "Core.TypeVar", "Core.TypeofVararg", "Core.TypeofBottom")


_ACCESSOR_RE = re.compile(r"(\.[^\W\d][\w!]*|\.\d+|\[\d+\])$", re.UNICODE)


def split_path(expr):
    """Split `expr` into candidate (base, accessors) pairs for the `jl`
    command, longest base first. Accessors are field names (str) or 1-based
    indices (int), peeled from trailing `.name`, `.i` and `[i]` syntax."""
    expr = expr.strip()
    bases = [(expr, [])]
    s, accessors = expr, []
    while True:
        m = _ACCESSOR_RE.search(s)
        if m is None or m.start() == 0:
            break
        tok = m.group(0)
        if tok.startswith("["):
            key = int(tok[1:-1])
        elif tok[1:].isdigit():
            key = int(tok[1:])
        else:
            key = tok[1:]
        accessors.insert(0, key)
        s = s[:m.start()]
        bases.append((s, list(accessors)))
    return bases


class JuliaRuntime:
    """Renders and inspects the Julia runtime of one debugged process."""

    def __init__(self, adapter):
        self.a = adapter
        self.dt_names = {}  # jl_datatype_t* address -> qualified name
        self._budget = None  # remaining output chars for this render pass

    # ---- output budget ----------------------------------------------------
    #
    # The element/string/depth caps bound each *level* of a summary, but a
    # recursively big structure could still make the renderer compute far
    # more text than MAX_OUTPUT keeps. A per-render-pass character budget
    # makes deeply/widely nested rendering stop early instead: once it is
    # exhausted, nested renderers return "…" immediately.

    def spend(self, n):
        if self._budget is not None:
            self._budget[0] -= n

    def exhausted(self):
        return self._budget is not None and self._budget[0] <= 0

    def _render_budgeted(self, fn, *args, limit=MAX_OUTPUT):
        fresh = self._budget is None
        if fresh:
            self._budget = [limit]
        try:
            out = fn(*args)
        finally:
            if fresh:
                self._budget = None
        if len(out) > limit:
            out = out[:limit] + "…"
        return out

    # ---- basic reads ----------------------------------------------------

    def read_uint(self, addr, size, signed=False):
        return int.from_bytes(self.a.read_mem(addr, size), "little",
                              signed=signed)

    def read_ptr(self, addr):
        return self.read_uint(addr, self.a.ptrsize)

    def field_u(self, addr, typename, *path):
        return self.a.field(addr, typename, path)

    def field_i(self, addr, typename, *path):
        return self.a.field(addr, typename, path, signed=True)

    # ---- type tags -------------------------------------------------------

    def typetag(self, addr):
        """Type tag of the object at addr: header word, GC bits masked."""
        return self.read_ptr(addr - self.a.ptrsize) & ~15

    def typeof_addr(self, addr):
        """Address of the jl_datatype_t of the object at addr (0: unknown)."""
        tag = self.typetag(addr)
        if tag < (64 << 4):
            for sym in ("jl_small_typeof", "ijl_small_typeof"):
                table = self.a.global_addr(sym)
                if table:
                    # entry at byte offset `tag`, see jl_to_typeof in julia.h
                    return self.read_ptr(table + tag)
            return 0
        return tag

    def symbol_name(self, addr):
        return self.a.read_cstr(addr + self.a.type_size("jl_sym_t"))

    def svec_len(self, addr):
        return self.field_u(addr, "jl_svec_t", "length")

    def svec_ref(self, addr, i):
        return self.read_ptr(addr + self.a.type_size("jl_svec_t")
                             + i * self.a.ptrsize)

    def module_path(self, addr, depth=0):
        """Dotted name of the jl_module_t at addr, e.g. "Base.Iterators"."""
        if addr == 0 or depth > 10:
            return "?"
        name = self.symbol_name(self.field_u(addr, "jl_module_t", "name"))
        parent = self.field_u(addr, "jl_module_t", "parent")
        if parent == 0 or parent == addr:
            return name
        pname = self.module_path(parent, depth + 1)
        if pname == "Main":
            return name
        return pname + "." + name

    def datatype_qualname(self, dtaddr):
        """Qualified name of a jl_datatype_t, e.g. "Core.Int64". Cached,
        since datatypes are interned and long-lived."""
        name = self.dt_names.get(dtaddr)
        if name is None:
            tn = self.field_u(dtaddr, "jl_datatype_t", "name")
            name = (self.module_path(self.field_u(tn, "jl_typename_t",
                                                  "module"))
                    + "." + self.symbol_name(
                        self.field_u(tn, "jl_typename_t", "name")))
            self.dt_names[dtaddr] = name
        return name

    def typename_of(self, dtaddr):
        tn = self.field_u(dtaddr, "jl_datatype_t", "name")
        return self.symbol_name(self.field_u(tn, "jl_typename_t", "name"))

    def is_cpu_addrspace(self, addr):
        """True when addr is an instance of Core.AddrSpace{Core}, value 0."""
        if addr == 0:
            return False
        try:
            dtaddr = self.typeof_addr(addr)
            if dtaddr == 0:
                return False
            return self.typename_of(dtaddr) == "AddrSpace" and \
                self.read_uint(addr, 1) == 0
        except JLDebugError:
            return False

    # ---- type rendering ---------------------------------------------------

    def render_typevar(self, addr, with_bounds):
        name = self.symbol_name(self.field_u(addr, "jl_tvar_t", "name"))
        if not with_bounds:
            return name
        lb = self.render_type(self.field_u(addr, "jl_tvar_t", "lb"),
                              MAX_DEPTH - 1)
        ub = self.render_type(self.field_u(addr, "jl_tvar_t", "ub"),
                              MAX_DEPTH - 1)
        if lb == "Union{}" and ub == "Any":
            return name
        if lb == "Union{}":
            return "%s<:%s" % (name, ub)
        return "%s<:%s<:%s" % (lb, name, ub)

    def flatten_union(self, addr, parts, depth):
        if parts and parts[-1] == "…":
            return
        if self.exhausted():
            parts.append("…")
            return
        if self.datatype_qualname(self.typeof_addr(addr)) == "Core.Union":
            self.flatten_union(self.field_u(addr, "jl_uniontype_t", "a"),
                               parts, depth)
            self.flatten_union(self.field_u(addr, "jl_uniontype_t", "b"),
                               parts, depth)
        else:
            parts.append(self.render_type(addr, depth))
            self.spend(len(parts[-1]))

    def render_type(self, addr, depth=MAX_DEPTH):
        """Render a Julia type object (or type parameter) as a string."""
        if addr == 0:
            return "#<null>"
        if depth < 0 or self.exhausted():
            return "…"
        dtaddr = self.typeof_addr(addr)
        if dtaddr == 0:
            return "<?type 0x%x>" % addr
        qual = self.datatype_qualname(dtaddr)
        if qual == "Core.TypeofBottom":
            return "Union{}"
        if qual == "Core.Union":
            parts = []
            self.flatten_union(addr, parts, depth - 1)
            return "Union{%s}" % ", ".join(parts)
        if qual == "Core.UnionAll":
            body = self.render_type(
                self.field_u(addr, "jl_unionall_t", "body"), depth - 1)
            var = self.render_typevar(
                self.field_u(addr, "jl_unionall_t", "var"), True)
            return "%s where %s" % (body, var)
        if qual == "Core.TypeVar":
            return self.render_typevar(addr, False)
        if qual == "Core.TypeofVararg":
            t = self.field_u(addr, "jl_vararg_t", "T")
            n = self.field_u(addr, "jl_vararg_t", "N")
            if t == 0:
                return "Vararg"
            if n == 0:
                return "Vararg{%s}" % self.render_type(t, depth - 1)
            return "Vararg{%s, %s}" % (self.render_type(t, depth - 1),
                                       self.render_value(n, depth - 1))
        if qual == "Core.Module":
            return self.module_path(addr)
        if qual != "Core.DataType":
            # a value used as a type parameter (1, :x, true, ...)
            return self.render_value(addr, depth)

        tn = self.field_u(addr, "jl_datatype_t", "name")
        modpath = self.module_path(self.field_u(tn, "jl_typename_t",
                                                "module"))
        name = self.symbol_name(self.field_u(tn, "jl_typename_t", "name"))
        if modpath not in ("Core", "Main") and not name.startswith("typeof("):
            name = modpath + "." + name
        params = self.field_u(addr, "jl_datatype_t", "parameters")
        nparams = self.svec_len(params) if params else 0
        if nparams == 0:
            return name + "{}" if name == "Tuple" else name
        # sugar: Array{T, 1} => Vector{T}, Array{T, 2} => Matrix{T}
        if name == "Array" and nparams == 2:
            ndim = self.render_type(self.svec_ref(params, 1), depth - 1)
            if ndim == "1":
                return "Vector{%s}" % self.render_type(
                    self.svec_ref(params, 0), depth - 1)
            if ndim == "2":
                return "Matrix{%s}" % self.render_type(
                    self.svec_ref(params, 0), depth - 1)
        # sugar: GenericMemory{:not_atomic, T, Core.CPU} => Memory{T}
        if name == "GenericMemory" and nparams == 3:
            order = self.svec_ref(params, 0)
            if self.is_cpu_addrspace(self.svec_ref(params, 2)):
                eltstr = self.render_type(self.svec_ref(params, 1), depth - 1)
                oname = self.symbol_name(order) if order else ""
                if oname == "not_atomic":
                    return "Memory{%s}" % eltstr
                if oname == "atomic":
                    return "AtomicMemory{%s}" % eltstr
        rendered = []
        for i in range(min(nparams, MAX_ELEMS)):
            if self.exhausted():
                rendered.append("…")
                break
            rendered.append(self.render_type(self.svec_ref(params, i),
                                             depth - 1))
            self.spend(len(rendered[-1]))
        else:
            if nparams > MAX_ELEMS:
                rendered.append("…")
        return "%s{%s}" % (name, ", ".join(rendered))

    # ---- data rendering ----------------------------------------------------

    def string_data(self, addr):
        strlen = self.read_uint(addr, self.a.ptrsize)
        n = min(strlen, MAX_STRING)
        s = self.a.read_mem(addr + self.a.ptrsize, n).decode(
            "utf-8", errors="replace") if n else ""
        return s, strlen

    def render_primitive(self, qual, addr):
        """The unboxed primitive of type `qual` stored at addr, or None."""
        fmt = PRIMITIVE_FMT.get(qual)
        if fmt is not None:
            kind, size = fmt
            if kind == "i":
                return str(self.read_uint(addr, size, signed=True))
            if kind == "u":
                return "0x%0*x" % (2 * size, self.read_uint(addr, size))
            import struct as _struct
            return repr(_struct.unpack(
                "<" + {2: "e", 4: "f", 8: "d"}[size],
                self.a.read_mem(addr, size))[0])
        if qual == "Core.Bool":
            return "true" if self.read_uint(addr, 1) else "false"
        if qual == "Core.Char":
            return render_char(self.read_uint(addr, 4))
        return None

    def layout_fields(self, dtaddr):
        """[(offset, size, isptr), ...] for a concrete jl_datatype_t, or
        None when there is no field table (opaque/foreign layouts)."""
        laddr = self.field_u(dtaddr, "jl_datatype_t", "layout")
        if laddr == 0:
            return None
        nfields = self.field_u(laddr, "jl_datatype_layout_t", "nfields")
        fdkind = self.field_u(laddr, "jl_datatype_layout_t", "flags",
                              "fielddesc_type")
        if fdkind == 3:  # foreign type: no descriptors
            return None
        fdname = ("jl_fielddesc8_t", "jl_fielddesc16_t",
                  "jl_fielddesc32_t")[fdkind]
        fdsize = self.a.type_size(fdname)
        base = laddr + self.a.type_size("jl_datatype_layout_t")
        fields = []
        for i in range(nfields):
            fd = base + i * fdsize
            fields.append((self.field_u(fd, fdname, "offset"),
                           self.field_u(fd, fdname, "size"),
                           self.field_u(fd, fdname, "isptr")))
        return fields

    def field_names(self, dtaddr, nfields):
        """Field names of a concrete datatype (NamedTuple aware)."""
        tn = self.field_u(dtaddr, "jl_datatype_t", "name")
        if self.symbol_name(self.field_u(tn, "jl_typename_t", "name")) == \
                "NamedTuple":
            names = self.namedtuple_names(dtaddr)
            if names is not None:
                return names
        names = []
        namesv = self.field_u(tn, "jl_typename_t", "names") if tn else 0
        n = self.svec_len(namesv) if namesv else 0
        for i in range(nfields):
            if i < n:
                sym = self.svec_ref(namesv, i)
                names.append(self.symbol_name(sym) if sym else str(i + 1))
            else:
                names.append(str(i + 1))
        return names

    def field_types(self, dtaddr, nfields):
        typesv = self.field_u(dtaddr, "jl_datatype_t", "types")
        n = self.svec_len(typesv) if typesv else 0
        return [self.svec_ref(typesv, i) if i < n else 0
                for i in range(nfields)]

    def namedtuple_names(self, dtaddr):
        """Field names of a concrete NamedTuple type, from its first type
        parameter (a tuple of symbols), or None."""
        try:
            params = self.field_u(dtaddr, "jl_datatype_t", "parameters")
            if self.svec_len(params) != 2:
                return None
            namestup = self.svec_ref(params, 0)
            fields = self.layout_fields(self.typeof_addr(namestup))
            if fields is None:
                return None
            return [self.symbol_name(self.read_ptr(namestup + off))
                    for (off, _, _) in fields]
        except JLDebugError:
            return None

    def render_unboxed(self, taddr, addr, depth):
        """Render the unboxed (inline-stored) value of type taddr at addr."""
        if depth < 0 or self.exhausted():
            return "…"
        if taddr == 0:
            return "<?>"
        if self.datatype_qualname(self.typeof_addr(taddr)) != "Core.DataType":
            return "<union field>"
        qual = self.datatype_qualname(taddr)
        prim = self.render_primitive(qual, addr)
        if prim is not None:
            return prim
        if self.field_u(taddr, "jl_datatype_t", "isprimitivetype"):
            laddr = self.field_u(taddr, "jl_datatype_t", "layout")
            size = self.field_u(laddr, "jl_datatype_layout_t", "size") \
                if laddr else 0
            if 0 < size <= 8:
                return "%s(0x%0*x)" % (self.render_type(taddr, 1), 2 * size,
                                       self.read_uint(addr, size))
            return "%s(...)" % self.render_type(taddr, 1)
        fields = self.layout_fields(taddr)
        if fields is None:
            return "<%s>" % self.render_type(taddr, depth - 1)
        if len(fields) == 0:
            inst = self.field_u(taddr, "jl_datatype_t", "instance")
            return self.render_value(inst, depth) if inst \
                else self.render_type(taddr, depth - 1) + "()"
        return self.render_struct_inline(taddr, addr, fields, depth)

    def render_struct_inline(self, taddr, addr, fields, depth):
        tn = self.field_u(taddr, "jl_datatype_t", "name")
        tname = self.symbol_name(self.field_u(tn, "jl_typename_t", "name"))
        istuple = tname == "Tuple" and \
            self.module_path(self.field_u(tn, "jl_typename_t",
                                          "module")) == "Core"
        names = None if istuple else self.field_names(taddr, len(fields))
        ftypes = self.field_types(taddr, len(fields))
        parts = []
        for i, (off, size, isptr) in enumerate(fields[:MAX_ELEMS]):
            if self.exhausted():
                parts.append("…")
                break
            if isptr:
                p = self.read_ptr(addr + off)
                r = self.render_value(p, depth - 1) if p else "#undef"
            else:
                r = self.render_unboxed(ftypes[i], addr + off, depth - 1)
            fname = names[i] if names and i < len(names) else str(i + 1)
            parts.append(r if istuple else "%s = %s" % (fname, r))
            self.spend(len(parts[-1]))
        else:
            if len(fields) > MAX_ELEMS:
                parts.append("…")
        if istuple and len(fields) == 1:
            return "(%s,)" % parts[0]
        if istuple or tname == "NamedTuple":
            return "(%s)" % ", ".join(parts)
        return "%s(%s)" % (self.render_type(taddr, 2), ", ".join(parts))

    # ---- arrays -------------------------------------------------------------

    def array_info(self, addr):
        """(dims, eltype, dataptr, elsize, isboxed, isunion) of the
        jl_array_t (or Memory) at addr."""
        dtaddr = self.typeof_addr(addr)
        tname = self.typename_of(dtaddr)
        params = self.field_u(dtaddr, "jl_datatype_t", "parameters")
        if tname == "GenericMemory":
            mem_dt_addr = dtaddr
            dims = [self.field_u(addr, "jl_genericmemory_t", "length")]
            dataptr = self.field_u(addr, "jl_genericmemory_t", "ptr")
            eltype = self.svec_ref(params, 1)
        else:
            mem_addr = self.field_u(addr, "jl_array_t", "ref", "mem")
            mem_dt_addr = self.typeof_addr(mem_addr)
            dataptr = self.field_u(addr, "jl_array_t", "ref",
                                   "ptr_or_offset")
            dimoff = self.a.field_offset("jl_array_t", "dimsize")
            try:
                ndims = self.read_uint(self.svec_ref(params, 1), 8)
            except JLDebugError:
                ndims = 1
            if not (0 < ndims < 33):
                ndims = 1
            dims = [self.read_uint(addr + dimoff + i * self.a.ptrsize,
                                   self.a.ptrsize) for i in range(ndims)]
            eltype = self.svec_ref(params, 0)
        mlayout = self.field_u(mem_dt_addr, "jl_datatype_t", "layout")
        elsize = self.field_u(mlayout, "jl_datatype_layout_t", "size")
        isboxed = self.field_u(mlayout, "jl_datatype_layout_t", "flags",
                               "arrayelem_isboxed")
        isunion = self.field_u(mlayout, "jl_datatype_layout_t", "flags",
                               "arrayelem_isunion")
        return dims, eltype, dataptr, elsize, isboxed, isunion

    def is_array_value(self, addr):
        try:
            dtaddr = self.typeof_addr(addr)
            return dtaddr != 0 and \
                self.typename_of(dtaddr) in ("Array", "GenericMemory")
        except JLDebugError:
            return False

    def render_array_summary(self, addr, depth):
        dims = self.array_info(addr)[0]
        tstr = self.render_type(self.typeof_addr(addr), depth)
        if len(dims) == 1:
            return "%d-element %s" % (dims[0], tstr)
        return "%s %s" % ("×".join(str(d) for d in dims), tstr)

    def array_children(self, addr):
        """Yield ("index", ("val", addr) | ("str", s)) pairs for the first
        MAX_ELEMS elements of the array/Memory at addr."""
        dims, eltype, dataptr, elsize, isboxed, isunion = self.array_info(addr)
        n = 1
        for d in dims:
            n *= d
        shown = min(n, MAX_ELEMS)
        for i in range(shown):
            if isboxed:
                p = self.read_ptr(dataptr + i * self.a.ptrsize)
                yield str(i + 1), (("val", p) if p else ("str", "#undef"))
            elif isunion:
                yield str(i + 1), ("str", "<union element>")
            else:
                yield str(i + 1), ("str", self.render_unboxed(
                    eltype, dataptr + i * elsize, MAX_DEPTH - 1))
        if n > shown:
            yield "...", ("str", "(%d more)" % (n - shown))

    def render_array(self, addr, depth):
        summary = self.render_array_summary(addr, depth)
        if depth < MAX_DEPTH:
            return summary
        parts = []
        for _, (kind, v) in self.array_children(addr):
            if self.exhausted():
                parts.append("…")
                break
            parts.append(self.render_value(v, depth - 1) if kind == "val"
                         else v)
            self.spend(len(parts[-1]))
        return "%s = {%s}" % (summary, ", ".join(parts))

    # ---- runtime objects ------------------------------------------------------

    def render_sig_call(self, fname, sigaddr, depth):
        """Render "f(::T, ::S)" from a Tuple{typeof(f), T, S} signature."""
        seen = 0
        while self.datatype_qualname(self.typeof_addr(sigaddr)) == \
                "Core.UnionAll" and seen < 32:
            sigaddr = self.field_u(sigaddr, "jl_unionall_t", "body")
            seen += 1
        if self.datatype_qualname(self.typeof_addr(sigaddr)) != \
                "Core.DataType":
            return "%s(...)" % fname
        params = self.field_u(sigaddr, "jl_datatype_t", "parameters")
        n = self.svec_len(params) if params else 0
        args = ["::" + self.render_type(self.svec_ref(params, i), depth - 1)
                for i in range(1, min(n, MAX_ELEMS + 1))]
        if n > MAX_ELEMS + 1:
            args.append("…")
        return "%s(%s)" % (fname, ", ".join(args))

    def render_method(self, addr, depth):
        name = self.symbol_name(self.field_u(addr, "jl_method_t", "name"))
        where = ""
        mod = self.field_u(addr, "jl_method_t", "module")
        if mod:
            where = " @ " + self.module_path(mod)
        faddr = self.field_u(addr, "jl_method_t", "file")
        if faddr:
            where += " %s:%d" % (self.symbol_name(faddr),
                                 self.field_i(addr, "jl_method_t", "line"))
        return self.render_sig_call(
            name, self.field_u(addr, "jl_method_t", "sig"), depth) + where

    def render_method_instance(self, addr, depth):
        defaddr = self.field_u(addr, "jl_method_instance_t", "def", "value")
        name = "?"
        if defaddr:
            if self.datatype_qualname(self.typeof_addr(defaddr)) == \
                    "Core.Method":
                name = self.symbol_name(
                    self.field_u(defaddr, "jl_method_t", "name"))
            else:  # toplevel thunk: def is a module
                return "MethodInstance for top-level scope in " + \
                    self.module_path(defaddr)
        return "MethodInstance for " + self.render_sig_call(
            name, self.field_u(addr, "jl_method_instance_t", "specTypes"),
            depth)

    def render_code_instance(self, addr, depth):
        defaddr = self.field_u(addr, "jl_code_instance_t", "def")
        inner = "?"
        if defaddr:
            qual = self.datatype_qualname(self.typeof_addr(defaddr))
            if qual == "Core.ABIOverride":
                defaddr = self.field_u(defaddr, "jl_abi_override_t", "def")
                qual = self.datatype_qualname(self.typeof_addr(defaddr))
            if qual == "Core.MethodInstance":
                inner = self.render_method_instance(defaddr, depth - 1)
        return "CodeInstance for %s" % inner

    def render_task(self, addr):
        state = TASK_STATES.get(self.field_u(addr, "jl_task_t", "_state"),
                                "?")
        return "Task (%s) @0x%016x" % (state, addr)

    def render_expr(self, addr, depth):
        head = self.field_u(addr, "jl_expr_t", "head")
        headname = self.symbol_name(head) if head else "?"
        args = self.field_u(addr, "jl_expr_t", "args")
        nargs = 0
        if args:
            try:
                nargs = self.array_info(args)[0][0]
            except JLDebugError:
                pass
        return "Expr(:%s, <%d args>)" % (headname, nargs)

    def render_svec(self, addr, depth):
        n = self.svec_len(addr)
        parts = []
        for i in range(min(n, MAX_ELEMS)):
            if self.exhausted():
                parts.append("…")
                break
            p = self.svec_ref(addr, i)
            parts.append(self.render_value(p, depth - 1) if p else "#undef")
            self.spend(len(parts[-1]))
        else:
            if n > MAX_ELEMS:
                parts.append("…")
        return "svec(%s)" % ", ".join(parts)

    # ---- main value renderer -----------------------------------------------

    def render_value(self, addr, depth=MAX_DEPTH):
        """Render the Julia value at addr as a compact single-line string."""
        if addr == 0:
            return "#<null>"
        if depth < 0 or self.exhausted():
            return "…"
        dtaddr = self.typeof_addr(addr)
        if dtaddr == 0:
            tag = self.typetag(addr)
            idx = tag >> 4
            if tag < (64 << 4) and idx < len(SMALL_TAG_NAMES):
                return "<%s 0x%x>" % (SMALL_TAG_NAMES[idx], addr)
            return "<julia value 0x%x>" % addr
        qual = self.datatype_qualname(dtaddr)

        if is_type_kind(qual):
            return self.render_type(addr, depth)
        prim = self.render_primitive(qual, addr)
        if prim is not None:
            return prim
        if qual == "Core.Nothing":
            return "nothing"
        if qual == "Core.Symbol":
            name = self.symbol_name(addr)
            if name.isidentifier():
                return ":" + name
            return 'Symbol("%s")' % escape_string(name)
        if qual == "Core.String":
            s, strlen = self.string_data(addr)
            suffix = "…" if strlen > len(s.encode("utf-8", "replace")) \
                else ""
            self.spend(len(s))
            return '"%s%s"' % (escape_string(s), suffix)
        if qual == "Core.SimpleVector":
            return self.render_svec(addr, depth)
        if qual == "Core.Module":
            return "Module %s" % self.module_path(addr)
        if qual == "Core.Task":
            return self.render_task(addr)
        if qual == "Core.Method":
            return self.render_method(addr, depth)
        if qual == "Core.MethodInstance":
            return self.render_method_instance(addr, depth)
        if qual == "Core.CodeInstance":
            return self.render_code_instance(addr, depth)
        if qual == "Core.Expr":
            return self.render_expr(addr, depth)
        if qual == "Core.GlobalRef":
            return "%s.%s" % (
                self.module_path(self.field_u(addr, "jl_globalref_t",
                                              "mod")),
                self.symbol_name(self.field_u(addr, "jl_globalref_t",
                                              "name")))
        if qual == "Core.TypeName":
            return "typename(%s)" % self.symbol_name(
                self.field_u(addr, "jl_typename_t", "name"))

        tname = self.typename_of(dtaddr)
        if tname in ("Array", "GenericMemory"):
            try:
                return self.render_array(addr, depth)
            except JLDebugError:
                return self.render_type(dtaddr, depth - 1) + " <unreadable>"

        # generic instances
        if self.field_u(dtaddr, "jl_datatype_t", "instance") == addr:
            tn = self.field_u(dtaddr, "jl_datatype_t", "name")
            sname = self.field_u(tn, "jl_typename_t", "singletonname") \
                if self.a.has_field("jl_typename_t", "singletonname") else 0
            # functions and similar get a distinct singleton name ("sin");
            # for other singletons render the type, e.g. Irrational{:π}()
            if sname and sname != self.field_u(tn, "jl_typename_t", "name"):
                return self.symbol_name(sname)
            return self.render_type(dtaddr, depth) + "()"
        if self.field_u(dtaddr, "jl_datatype_t", "isprimitivetype"):
            return self.render_unboxed(dtaddr, addr, depth)
        fields = self.layout_fields(dtaddr)
        if fields is not None:
            return self.render_struct_inline(dtaddr, addr, fields, depth)
        return "%s @0x%016x" % (self.render_type(dtaddr, depth - 1), addr)

    def render_value_capped(self, addr):
        """render_value with a hard cap on the output size, for printers."""
        return self._render_budgeted(self.render_value, addr)

    def render_value_brief(self, addr):
        """A terser render_value (smaller budget, shallower depth) for
        values that appear inside an enclosing expansion, so the total
        output stays bounded when the debugger expands nested children."""
        return self._render_budgeted(self.render_value, addr,
                                     MAX_DEPTH - 1, limit=BRIEF_OUTPUT)

    # ---- Julia-semantics field/index access (the `jl` command) ---------------
    #
    # A "location" is ("val", addr) for a boxed value, or
    # ("inline", typeaddr, addr) for an unboxed value stored inline.

    def loc_of_value(self, addr):
        return ("val", addr)

    def render_loc(self, loc):
        if loc[0] == "val":
            return self._render_budgeted(self.render_value, loc[1])
        return self._render_budgeted(self.render_unboxed, loc[1], loc[2],
                                     MAX_DEPTH)

    def loc_type_and_addr(self, loc):
        if loc[0] == "val":
            return self.typeof_addr(loc[1]), loc[1]
        return loc[1], loc[2]

    def getfield(self, loc, key):
        """Julia-semantics getfield/getindex: `key` is a field name (str) or
        a 1-based index (int). Returns a new location."""
        taddr, addr = self.loc_type_and_addr(loc)
        if addr == 0:
            raise JLDebugError("cannot access field of NULL")
        if taddr == 0:
            raise JLDebugError("cannot determine type of 0x%x" % addr)
        qual = self.datatype_qualname(taddr)
        if qual == "Core.Module" and loc[0] == "val":
            if not isinstance(key, str):
                raise JLDebugError("module access needs a name, got %r"
                                   % key)
            return self.module_getfield(addr, key)
        if qual == "Core.SimpleVector" and loc[0] == "val":
            if not isinstance(key, int):
                raise JLDebugError("svec access needs an index")
            n = self.svec_len(addr)
            if not (1 <= key <= n):
                raise JLDebugError("index %d out of bounds (1:%d)"
                                   % (key, n))
            p = self.svec_ref(addr, key - 1)
            if p == 0:
                raise JLDebugError("svec element %d is #undef" % key)
            return ("val", p)
        tname = self.typename_of(taddr)
        if tname in ("Array", "GenericMemory") and loc[0] == "val" and \
                isinstance(key, int):
            return self.array_getindex(addr, key)

        fields = self.layout_fields(taddr)
        if fields is None or len(fields) == 0:
            raise JLDebugError("%s has no fields" % self.render_type(taddr, 1))
        names = self.field_names(taddr, len(fields))
        if isinstance(key, int):
            idx = key - 1
            if not (0 <= idx < len(fields)):
                raise JLDebugError("field index %d out of bounds (1:%d)"
                                   % (key, len(fields)))
        else:
            try:
                idx = names.index(key)
            except ValueError:
                raise JLDebugError("%s has no field %s; fields are (%s)"
                                   % (self.render_type(taddr, 1), key,
                                      ", ".join(names[:25])))
        off, size, isptr = fields[idx]
        if isptr:
            p = self.read_ptr(addr + off)
            if p == 0:
                raise JLDebugError("field %s is #undef" % key)
            return ("val", p)
        ftype = self.field_types(taddr, len(fields))[idx]
        if ftype == 0 or self.datatype_qualname(self.typeof_addr(ftype)) != \
                "Core.DataType":
            raise JLDebugError("field %s has union layout; cannot access"
                               % key)
        return ("inline", ftype, addr + off)

    def array_getindex(self, addr, key):
        dims, eltype, dataptr, elsize, isboxed, isunion = \
            self.array_info(addr)
        n = 1
        for d in dims:
            n *= d
        if not (1 <= key <= n):
            raise JLDebugError("index %d out of bounds (1:%d)" % (key, n))
        if isunion:
            raise JLDebugError("union-layout array elements are not"
                               " supported")
        if isboxed:
            p = self.read_ptr(dataptr + (key - 1) * self.a.ptrsize)
            if p == 0:
                raise JLDebugError("element %d is #undef" % key)
            return ("val", p)
        return ("inline", eltype, dataptr + (key - 1) * elsize)

    # ---- module bindings ------------------------------------------------------

    def module_getfield(self, modaddr, name, depth=0):
        """Look up the global `name` in the module at modaddr, walking the
        binding table and its partitions (newest partition only)."""
        if depth > 10:
            raise JLDebugError("binding chain too deep")
        bindings = self.field_u(modaddr, "jl_module_t", "bindings")
        if bindings == 0:
            raise JLDebugError("module has no binding table")
        n = self.svec_len(bindings)
        if n > 100000:
            raise JLDebugError("implausible binding table size %d" % n)
        for i in range(n):
            b = self.svec_ref(bindings, i)
            if b == 0:
                continue
            try:
                if self.datatype_qualname(self.typeof_addr(b)) != \
                        "Core.Binding":
                    continue
                gr = self.field_u(b, "jl_binding_t", "globalref")
                if gr == 0:
                    continue
                sym = self.field_u(gr, "jl_globalref_t", "name")
                if self.symbol_name(sym) != name:
                    continue
            except JLDebugError:
                continue
            return self.binding_value(b, name, depth)
        raise JLDebugError("%s has no binding named %s"
                           % (self.module_path(modaddr), name))

    def binding_value(self, baddr, name, depth):
        part = self.field_u(baddr, "jl_binding_t", "partitions")
        if part == 0:
            value = self.field_u(baddr, "jl_binding_t", "value")
            if value:
                return ("val", value)
            raise JLDebugError("binding %s has no value" % name)
        kind = self.field_u(part, "jl_binding_partition_t", "kind") & 0xf
        restriction = self.field_u(part, "jl_binding_partition_t",
                                   "restriction")
        if kind in PARTITION_KINDS_CONST:
            if restriction == 0:
                raise JLDebugError("constant %s is undefined" % name)
            return ("val", restriction)
        if kind in PARTITION_KINDS_IMPORT:
            if restriction == 0:
                raise JLDebugError("import %s is unresolved" % name)
            return self.binding_value(restriction, name, depth + 1)
        if kind in PARTITION_KINDS_GLOBAL:
            value = self.field_u(baddr, "jl_binding_t", "value")
            if value:
                return ("val", value)
            raise JLDebugError("global %s is undefined" % name)
        raise JLDebugError("binding %s is not resolvable (kind %d)"
                           % (name, kind))

    def root_module(self, name):
        """Address of Main/Core/Base, or a top-level module/global visible
        from Main or Core, for use as the base of a `jl` path. 0 if absent."""
        roots = {"Main": "jl_main_module", "Core": "jl_core_module",
                 "Base": "jl_base_module"}
        if name in roots:
            slot = self.a.global_addr(roots[name])
            return self.read_ptr(slot) if slot else 0
        for root in ("jl_main_module", "jl_core_module"):
            slot = self.a.global_addr(root)
            if not slot:
                continue
            mod = self.read_ptr(slot)
            if not mod:
                continue
            try:
                loc = self.module_getfield(mod, name)
                return loc[1] if loc[0] == "val" else 0
            except JLDebugError:
                continue
        return 0

    def eval_accessors(self, loc, accessors):
        for key in accessors:
            loc = self.getfield(loc, key)
        return loc
