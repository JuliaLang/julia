# This file is a part of Julia. License is MIT: https://julialang.org/license

"""GDB pretty-printers and commands for Julia runtime values.

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

The rendering logic lives in `julia_debug_core.py`, which must stay next to
this file. The printers dispatch on the *runtime* type tag of the object, so
a plain `jl_value_t*` prints as whatever it actually is. Everything is
resolved through the debug info (DWARF) of libjulia-internal, so this script
does not hard-code struct offsets and should work across Julia versions; it
requires a build of Julia with debug info (the default) and gracefully
degrades to raw pointers when the debug info or the memory is unavailable.

Julia-semantics field and index access is available through the `jl` command
(1-based indexing, fields by name; also module globals):

    (gdb) jl v.inner.name
    $jl = "hello"
    (gdb) jl v.vec[2]
    $jl = 42
    (gdb) jl Base.pi
    $jl = π

The result is also stored in the convenience variable `$jl` for further use.
Convenience functions `$jl_typeof(v)` and `$jl_field(v, "name")` compose
inside larger gdb expressions:

    (gdb) print $jl_typeof(v)
    $3 = Vector{Int64}
    (gdb) print $jl_field($jl_field(v, "inner"), "count") + 1

To temporarily see raw pointers again use `print/r v`.
"""

import os
import re
import sys

import gdb
import gdb.printing

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import julia_debug_core as core
from julia_debug_core import JLDebugError


class GdbAdapter:
    """Debug-info and memory access for julia_debug_core, via gdb."""

    def __init__(self):
        self.ptrsize = gdb.lookup_type("void").pointer().sizeof
        self.types = {}
        self.sizes = {}
        self.offsets = {}
        self.globals = {}

    def lookup_type(self, name):
        t = self.types.get(name)
        if t is None:
            try:
                t = gdb.lookup_type(name)
            except gdb.error as e:
                raise JLDebugError(str(e))
            self.types[name] = t
        return t

    def value_at(self, addr, typename):
        return gdb.Value(addr).cast(
            self.lookup_type(typename).pointer()).dereference()

    def type_size(self, name):
        s = self.sizes.get(name)
        if s is None:
            s = self.lookup_type(name).sizeof
            self.sizes[name] = s
        return s

    def field_offset(self, typename, fieldname):
        key = (typename, fieldname)
        off = self.offsets.get(key)
        if off is None:
            try:
                off = self.lookup_type(typename)[fieldname].bitpos // 8
            except (gdb.error, KeyError) as e:
                raise JLDebugError(str(e))
            self.offsets[key] = off
        return off

    def field(self, addr, typename, path, signed=False):
        try:
            v = self.value_at(addr, typename)
            for name in path:
                v = v[name]
            return int(v)
        except (gdb.error, gdb.MemoryError, KeyError) as e:
            raise JLDebugError(str(e))

    def has_field(self, typename, fieldname):
        try:
            return any(f.name == fieldname
                       for f in self.lookup_type(typename).fields())
        except JLDebugError:
            return False

    def read_mem(self, addr, size):
        try:
            return bytes(gdb.selected_inferior().read_memory(addr, size))
        except (gdb.error, gdb.MemoryError) as e:
            raise JLDebugError(str(e))

    def read_cstr(self, addr, maxlen=512):
        buf = self.read_mem(addr, maxlen)
        nul = buf.find(b"\0")
        if nul >= 0:
            buf = buf[:nul]
        return buf.decode("utf-8", errors="replace")

    def global_addr(self, name):
        addr = self.globals.get(name)
        if addr is None:
            try:
                addr = int(gdb.parse_and_eval("(unsigned long)&" + name))
            except gdb.error:
                addr = 0
            self.globals[name] = addr
        return addr


_RT = [None]


def get_rt():
    if _RT[0] is None:
        _RT[0] = core.JuliaRuntime(GdbAdapter())
    return _RT[0]


def _clear_cache(event=None):
    _RT[0] = None
    JlCommand.last_loc = None
    _expand_depth[0] = 0


gdb.events.new_objfile.connect(_clear_cache)
if hasattr(gdb.events, "free_objfile"):
    gdb.events.free_objfile.connect(_clear_cache)
gdb.events.exited.connect(_clear_cache)


def jl_value(addr):
    """A gdb.Value holding addr as a jl_value_t*."""
    return gdb.Value(addr).cast(gdb.lookup_type("jl_value_t").pointer())


def loc_to_gdb_value(rt, loc):
    """Convert a core location to a gdb.Value: jl_value_t* for boxed values,
    a native typed value for unboxed primitives, void* otherwise."""
    if loc[0] == "val":
        return jl_value(loc[1])
    _, taddr, addr = loc
    ctype = core.PRIMITIVE_CTYPES.get(rt.datatype_qualname(taddr))
    if ctype is not None:
        return gdb.Value(addr).cast(
            gdb.lookup_type(ctype).pointer()).dereference()
    return gdb.Value(addr).cast(gdb.lookup_type("void").pointer())


# --------------------------------------------------------------------------
# pretty printers
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
            rt = get_rt()
            # inside an array expansion each element is rendered by its own
            # printer; keep those terse and drawing from the expansion-wide
            # budget so the total output stays bounded
            if _expand_depth[0] > 0:
                s = rt.render_value_brief(addr)
                rt.spend(len(s))
                return s
            rt._budget = None  # recover from any abandoned expansion
            return rt.render_value_capped(addr)
        except JLDebugError as e:
            return "<not a julia value: 0x%x (%s)>" % (addr, e)
        except Exception as e:  # never break `print` on a printer bug
            return "<error rendering julia value 0x%x: %s>" % (addr, e)


# Number of array children() generators currently being expanded. gdb drives
# nested expansion itself (our depth caps don't apply to it), so without a
# guard a self-referential array would be re-expanded until gdb's own limits
# kick in. Elements yielded at nesting >= MAX_DEPTH become pre-rendered
# (depth-capped) strings instead of jl_value_t* values.
_expand_depth = [0]


class JuliaArrayPrinter(JuliaValuePrinter):
    """Adds expandable children for arrays so IDE variable views can drill
    down into elements."""

    def to_string(self):
        try:
            return get_rt().render_array_summary(int(self.val),
                                                 core.MAX_DEPTH)
        except (JLDebugError, Exception):
            return JuliaValuePrinter.to_string(self)

    def children(self):
        rt = get_rt()
        # the outermost expansion owns a budget that everything nested
        # (element to_strings, leaf renders) draws from, so gdb's
        # multiplicative child expansion cannot produce unbounded output
        owns_budget = _expand_depth[0] == 0 and rt._budget is None
        if owns_budget:
            rt._budget = [core.MAX_OUTPUT]
        _expand_depth[0] += 1
        try:
            for name, (kind, v) in rt.array_children(int(self.val)):
                if rt.exhausted():
                    yield name, "…"
                    break
                if kind != "val":
                    rt.spend(len(v))
                    yield name, v
                elif _expand_depth[0] < core.MAX_DEPTH:
                    yield name, jl_value(v)
                else:
                    s = rt.render_value_brief(v)
                    rt.spend(len(s))
                    yield name, s
        except (JLDebugError, Exception):
            return
        finally:
            _expand_depth[0] -= 1
            if owns_budget:
                rt._budget = None

    def display_hint(self):
        return "array"


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
            if get_rt().is_array_value(int(val)):
                return JuliaArrayPrinter(val)
        except (gdb.error, JLDebugError):
            pass
        return JuliaValuePrinter(val)


# --------------------------------------------------------------------------
# convenience functions and the `jl` command
# --------------------------------------------------------------------------

class JlTypeofFunction(gdb.Function):
    """$jl_typeof(v): the jl_datatype_t* of a Julia value.

    Usage: print $jl_typeof(v)"""

    def __init__(self):
        super().__init__("jl_typeof")

    def invoke(self, v):
        addr = get_rt().typeof_addr(int(v))
        return gdb.Value(addr).cast(gdb.lookup_type("jl_datatype_t").pointer())


class JlFieldFunction(gdb.Function):
    """$jl_field(v, key): Julia-semantics getfield/getindex on a value.

    `key` is a field name string or a 1-based index. Boxed fields come back
    as jl_value_t*, unboxed primitive fields as native typed values, so the
    result composes inside larger expressions:

        print $jl_field(v, "inner")
        print $jl_field($jl_field(v, "counts"), 2) + 1
    """

    def __init__(self):
        super().__init__("jl_field")

    def invoke(self, v, key):
        rt = get_rt()
        if key.type.code in (gdb.TYPE_CODE_ARRAY, gdb.TYPE_CODE_PTR):
            k = key.string()
        else:
            k = int(key)
        try:
            loc = rt.getfield(("val", int(v)), k)
            if loc[0] == "inline" and \
                    core.PRIMITIVE_CTYPES.get(
                        rt.datatype_qualname(loc[1])) is None:
                raise JLDebugError(
                    "field %s is a struct stored inline in its parent;"
                    " use the `jl` command to access it further" % k)
            return loc_to_gdb_value(rt, loc)
        except JLDebugError as e:
            raise gdb.GdbError(str(e))


class JlCommand(gdb.Command):
    """jl <path>: inspect a Julia value with Julia field/index semantics.

    The path starts from a C expression (a variable, convenience variable,
    or cast) or from a module name, followed by `.field` and `[i]` accessors
    with Julia (1-based) indexing:

        jl v.inner.name
        jl v.vec[2]
        jl v.tup.1
        jl Base.have_fma
        jl $1.x
        jl $jl.name

    The result is rendered like `print` would and stored in the convenience
    variable `$jl` (as jl_value_t*, or a native value for unboxed fields).
    A path starting with `$jl` continues from the previous result, which
    also works when that result was a struct stored inline in its parent."""

    last_loc = None

    def __init__(self):
        super().__init__("jl", gdb.COMMAND_DATA)

    def invoke(self, arg, from_tty):
        arg = arg.strip()
        if not arg:
            raise gdb.GdbError("usage: jl <expr>[.field|[index]]...")
        rt = get_rt()
        first_err = None
        parsed_any = False
        for base, accessors in core.split_path(arg):
            if base == "$jl" and JlCommand.last_loc is not None:
                loc0 = JlCommand.last_loc
            else:
                try:
                    v = gdb.parse_and_eval(base)
                    loc0 = ("val", int(v))
                except gdb.error:
                    continue
            parsed_any = True
            try:
                self.finish(rt, rt.eval_accessors(loc0, accessors))
                return
            except JLDebugError as e:
                first_err = first_err or e
        # base is not a C expression: try Julia module/global resolution
        base, accessors = core.split_path(arg)[-1]
        if not parsed_any and base.isidentifier():
            try:
                modaddr = rt.root_module(base)
            except JLDebugError:
                modaddr = 0
            if modaddr:
                try:
                    self.finish(rt, rt.eval_accessors(("val", modaddr),
                                                      accessors))
                    return
                except JLDebugError as e:
                    first_err = first_err or e
        if first_err is not None:
            raise gdb.GdbError(str(first_err))
        raise gdb.GdbError("cannot evaluate %r" % arg)

    def finish(self, rt, loc):
        JlCommand.last_loc = loc
        try:
            gdb.set_convenience_variable("jl", loc_to_gdb_value(rt, loc))
        except (gdb.error, JLDebugError, AttributeError):
            pass
        print("$jl = %s" % rt.render_loc(loc))


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

SAFEPOINT_COND = (
    "!((unsigned long)$_siginfo._sifields._sigfault.si_addr"
    " >= (unsigned long)jl_safepoint_pages"
    " && (unsigned long)$_siginfo._sifields._sigfault.si_addr"
    " < (unsigned long)jl_safepoint_pages"
    " + %d*(unsigned long)jl_page_size)" % core.SAFEPOINT_PAGES)

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
    JlFieldFunction()
    JlCommand()
    JlSafepointCommand()
    JlHandleSignalsCommand()
    try:
        enable_safepoint_filter()
    except gdb.error:
        pass


register(gdb.current_objfile())
