# This file is a part of Julia. License is MIT: https://julialang.org/license

"""LLDB type summaries and commands for Julia runtime values.

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

The rendering logic lives in `julia_debug_core.py`, which must stay next to
this file. The summaries dispatch on the *runtime* type tag of the object, so
a plain `jl_value_t*` prints as whatever it actually is. Everything is
resolved through the debug info of libjulia-internal, so this script does not
hard-code struct offsets and should work across Julia versions; it requires a
build of Julia with debug info (the default) and gracefully degrades to raw
pointers when the debug info or the memory is unavailable.

Julia-semantics field and index access is available through the `jl` command
(1-based indexing, fields by name; also module globals):

    (lldb) jl v.inner.name
    "hello"
    (lldb) jl v.vec[2]
    42
    (lldb) jl Base.pi
    π

The script also installs a stop-hook that transparently resumes the benign
SIGSEGVs Julia's GC uses to stop the world at safepoints (real segfaults
still stop the debugger); control it with `jl-safepoint-filter [on|off]`.
"""

import os
import re
import sys

import lldb

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import julia_debug_core as core
from julia_debug_core import JLDebugError


class LldbAdapter:
    """Debug-info and memory access for julia_debug_core, via lldb."""

    def __init__(self, target, process):
        self.target = target
        self.process = process
        self.ptrsize = target.GetAddressByteSize()
        self.types = {}
        self.sizes = {}
        self.offsets = {}
        self.globals = {}

    def lookup_type(self, name):
        t = self.types.get(name)
        if t is None:
            t = self.target.FindFirstType(name)
            if not t.IsValid():
                raise JLDebugError("no type named %s in debug info" % name)
            self.types[name] = t
        return t

    def value_at(self, addr, typename):
        t = self.lookup_type(typename)
        return self.target.CreateValueFromAddress(
            "jlval", lldb.SBAddress(addr, self.target), t)

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
            for f in self.lookup_type(typename).get_fields_array():
                if f.GetName() == fieldname:
                    off = f.GetOffsetInBytes()
                    break
            if off is None:
                raise JLDebugError("no field %s in %s"
                                   % (fieldname, typename))
            self.offsets[key] = off
        return off

    def field(self, addr, typename, path, signed=False):
        v = self.value_at(addr, typename)
        for name in path:
            v = v.GetChildMemberWithName(name)
            if not v.IsValid():
                raise JLDebugError("no member %s in %s" % (name, typename))
        return v.GetValueAsSigned() if signed else v.GetValueAsUnsigned()

    def has_field(self, typename, fieldname):
        try:
            return any(f.GetName() == fieldname
                       for f in self.lookup_type(typename).get_fields_array())
        except JLDebugError:
            return False

    def read_mem(self, addr, size):
        err = lldb.SBError()
        buf = self.process.ReadMemory(addr, size, err)
        if err.Fail() or buf is None:
            raise JLDebugError("cannot read 0x%x" % addr)
        return buf

    def read_cstr(self, addr, maxlen=512):
        err = lldb.SBError()
        s = self.process.ReadCStringFromMemory(addr, maxlen, err)
        if err.Fail() or s is None:
            raise JLDebugError("cannot read string at 0x%x" % addr)
        return s

    def global_addr(self, name):
        addr = self.globals.get(name)
        if addr is None:
            addr = 0
            var = self.target.FindFirstGlobalVariable(name)
            if var.IsValid():
                laddr = var.GetLoadAddress()
                if laddr != lldb.LLDB_INVALID_ADDRESS:
                    addr = laddr
            self.globals[name] = addr
        return addr


_RT = [None]
_last_loc = [None]  # previous `jl` command result, reachable as `$jl`


def get_rt(target, process):
    rt = _RT[0]
    if rt is not None and \
            rt.a.process.GetUniqueID() == process.GetUniqueID():
        # same process: keep the caches, refresh the (non-comparable)
        # SB wrapper objects
        rt.a.target = target
        rt.a.process = process
        return rt
    rt = core.JuliaRuntime(LldbAdapter(target, process))
    _RT[0] = rt
    _last_loc[0] = None
    return rt


def rt_of(valobj):
    return get_rt(valobj.GetTarget(), valobj.GetProcess())


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
        if base <= fault < base + core.SAFEPOINT_PAGES * pgsz:
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
    rt = rt_of(valobj)
    if rt.a.process.IsValid():
        # opportunistically install the safepoint stop-hook once a real
        # process exists (importing from ~/.lldbinit happens before that)
        _install_stop_hook(valobj.GetTarget().GetDebugger())
    try:
        return rt.render_value_capped(addr)
    except JLDebugError as e:
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
    rt = rt_of(val)
    try:
        result.AppendMessage(
            rt.render_type(rt.typeof_addr(val.GetValueAsUnsigned())))
    except JLDebugError as e:
        result.SetError(str(e))


def jl_cmd(debugger, command, exe_ctx, result, internal_dict):
    """jl <path>: inspect a Julia value with Julia field/index semantics.

    The path starts from a C expression (a variable or cast) or from a
    module name, followed by `.field` and `[i]` accessors with Julia
    (1-based) indexing:

        jl v.inner.name
        jl v.vec[2]
        jl v.tup.1
        jl Base.have_fma
        jl $jl.name    (continue from the previous `jl` result)
    """
    arg = command.strip()
    if not arg:
        result.SetError("usage: jl <expr>[.field|[index]]...")
        return
    frame = exe_ctx.GetFrame()
    target = exe_ctx.GetTarget()
    process = exe_ctx.GetProcess()
    if not process.IsValid():
        result.SetError("no running process")
        return
    rt = get_rt(target, process)
    first_err = None
    parsed_any = False
    for base, accessors in core.split_path(arg):
        if base == "$jl" and _last_loc[0] is not None:
            loc0 = _last_loc[0]
        else:
            if not frame.IsValid():
                break
            val = frame.EvaluateExpression(base)
            if not val.IsValid() or val.GetError().Fail():
                continue
            canon = val.GetType().GetCanonicalType()
            if canon.GetTypeClass() == lldb.eTypeClassStruct:
                # unboxed Julia struct value (e.g. a by-reference argument):
                # resolve its debug-info type name to the Julia datatype
                laddr = val.GetLoadAddress()
                if laddr == lldb.LLDB_INVALID_ADDRESS:
                    continue
                dt = rt.resolve_type_name(canon.GetName(),
                                          size=canon.GetByteSize())
                if dt == 0:
                    continue
                loc0 = ("inline", dt, laddr)
            else:
                loc0 = ("val", val.GetValueAsUnsigned())
        parsed_any = True
        try:
            loc = rt.eval_accessors(loc0, accessors)
            _last_loc[0] = loc
            result.AppendMessage(rt.render_loc(loc))
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
                loc = rt.eval_accessors(("val", modaddr), accessors)
                _last_loc[0] = loc
                result.AppendMessage(rt.render_loc(loc))
                return
            except JLDebugError as e:
                first_err = first_err or e
    result.SetError(str(first_err) if first_err is not None
                    else "cannot evaluate %r" % arg)


def __lldb_init_module(debugger, internal_dict):
    for tname in JULIA_POINTER_TYPES:
        debugger.HandleCommand(
            'type summary add --python-function %s.jl_value_summary "%s *"'
            % (__name__, tname))
    debugger.HandleCommand(
        "command script add -f %s.jl_cmd jl" % __name__)
    debugger.HandleCommand(
        "command script add -f %s.jl_typeof_cmd jl-typeof" % __name__)
    debugger.HandleCommand(
        "command script add -f %s.jl_safepoint_filter_cmd jl-safepoint-filter"
        % __name__)
    _install_stop_hook(debugger)
    print("julia_lldb: type summaries for Julia values installed "
          "(commands: jl, jl-typeof, jl-safepoint-filter)")
