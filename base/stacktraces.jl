# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Tools for collecting and manipulating stack traces. Mainly used for building errors.
"""
module StackTraces


import Base: hash, ==, show

using Core: CodeInfo, MethodInstance, CodeInstance
using Base.IRShow

export StackTrace, StackFrame, stacktrace

"""
    StackFrame

Stack information representing execution context, with the following fields:

- `func::Symbol`

  The name of the function containing the execution context.

- `linfo::Union{Method, Core.MethodInstance, Core.CodeInstance, Core.CodeInfo, Nothing}`

  The Method, MethodInstance, CodeInstance, or CodeInfo containing the execution context (if it could be found),
     or nothing (for example, if the inlining was a result of macro expansion).

- `file::Symbol`

  The path to the file containing the execution context.

- `line::Int`

  The line number in the file containing the execution context.

- `from_c::Bool`

  True if the code is from C.

- `inlined::Bool`

  True if the code is from an inlined frame.

- `pointer::UInt64`

  Representation of the pointer to the execution context as returned by `backtrace`.

- `pc::Int`

  If `from_c`, this is a column number.  Otherwise, it is a 1-based statement
  index within the frame's `linfo.debuginfo`, recovered from the DWARF column
  emitted by codegen, or `0` when debuginfo is unavailable. Used internally by
  `StackTraces.lookup` to resolve the `MethodInstance` for inlined frames; also
  surfaced in low-level safe backtrace output (e.g. on segfault) but not in
  Julia's default `show` for `StackFrame`.
"""
struct StackFrame # this type should be kept platform-agnostic so that profiles can be dumped on one machine and read on another
    "the name of the function containing the execution context"
    func::Symbol
    "the path to the file containing the execution context"
    file::Symbol
    "the line number in the file containing the execution context"
    line::Int
    "the CodeInstance or CodeInfo containing the execution context (if it could be found), \
     or nothing (for example, if the inlining was a result of macro expansion)."
    linfo::Union{Core.MethodInstance, Core.CodeInstance, Method, CodeInfo, Nothing}
    "true if the code is from C"
    from_c::Bool
    "true if the code is from an inlined frame"
    inlined::Bool
    "representation of the pointer to the execution context as returned by `backtrace`"
    pointer::UInt64  # Large enough to be read losslessly on 32- and 64-bit machines.
    "if !from_c, 1-based statement index (PC) within the frame's CodeInfo, or 0 if unavailable"
    pc::Int
end

StackFrame(func, file, line, linfo, from_c, inlined, pointer) =
    StackFrame(func, file, line, linfo, from_c, inlined, pointer, 0)
StackFrame(func, file, line) = StackFrame(Symbol(func), Symbol(file), line,
                                          nothing, false, false, 0, 0)

"""
    StackTrace

An alias for `Vector{StackFrame}` provided for convenience; returned by calls to
`stacktrace`.
"""
const StackTrace = Vector{StackFrame}

const empty_sym = Symbol("")
const UNKNOWN = StackFrame(empty_sym, empty_sym, -1, nothing, true, false, 0) # === lookup(C_NULL)


#=
If the StackFrame has function and line information, we consider two of them the same if
they share the same function/line information.
=#
function ==(a::StackFrame, b::StackFrame)
    return a.line == b.line && a.from_c == b.from_c && a.func == b.func && a.file == b.file && a.inlined == b.inlined # excluding linfo and pointer
end

function hash(frame::StackFrame, h::UInt)
    h ⊻= 0xf4fbda67fe20ce88 % UInt
    h = hash(frame.line, h)
    h = hash(frame.file, h)
    h = hash(frame.func, h)
    h = hash(frame.from_c, h)
    h = hash(frame.inlined, h)
    return h
end

function _add_linetable_frames!(frames, pointer, di::Core.DebugInfo, pc::Int)
    lt = di.linetable
    if lt isa Core.DebugInfo
        ltpc::Int, _, _ = Base.Compiler.getdebugidx(di, pc)
        _add_linetable_frames!(frames, pointer, lt, ltpc)
        _, eid::Int, epc::Int = Base.Compiler.getdebugidx(lt, ltpc)
        eid != 0 && _add_di_frames!(frames, pointer, lt.edges[eid], epc)
    end
    nothing
end

# 1. Push our own frame
# 2. If there is a linetable, recurse on edges there (and not the linetable)
# 3. Recurse on any of our own edges
function _add_di_frames!(frames, pointer, di::Core.DebugInfo, pc::Int)
    _, eid::Int, epc::Int = Base.Compiler.getdebugidx(di, pc)
    @assert pc > 0 "invalid pc for $di.def"
    push!(frames, StackFrame(
        di.def isa Symbol ? Symbol("macro expansion") : IRShow.method_name(di.def),
        IRShow.debuginfo_file1(di),
        @ccall(jl_cdi_firstxy(di::Any, pc::Int32)::NTuple{2, Int32})[1],
        di.def isa Core.MethodInstance ? di.def : nothing,
        false, # we can assume C frames aren't inlined into julia
        !isempty(frames),
        pointer,
        pc))
    _add_linetable_frames!(frames, pointer, di, pc)
    eid != 0 && _add_di_frames!(frames, pointer, di.edges[eid], epc)
    nothing
end

"""
    lookup(pointer::Ptr{Cvoid})::Vector{StackFrame}

Given a pointer to an execution context (usually generated by a call to `backtrace`), looks
up stack frame context information. Returns an array of frame information for all functions
inlined at that point, innermost function first.
"""
Base.@constprop :none function lookup(pointer::Ptr{Cvoid})
    frames = @ccall jl_lookup_code_address(pointer::Ptr{Cvoid}, false::Cint)::Core.SimpleVector
    pointer = convert(UInt64, pointer)

    # this is equal to UNKNOWN
    isempty(frames) && return [StackFrame(
        empty_sym, empty_sym, -1, nothing, true, false, pointer)]

    # If we aren't given a PC and CodeInstance for the last (non-inlined) frame,
    # we can't recover any more information than `frames`, so use those.
    # Otherwise, DebugInfo lets us recover `linfo` for inlined frames too, so
    # ignore `frames` and construct them by traversing the DebugInfo tree
    # instead.  This is a separate code path since attempting to enhance
    # existing `frames` would require matching them to our tree traversal.
    pc = frames[end][5]::Bool ? 0 : frames[end][7]::Int
    di = let x = frames[end][4]
        x isa Core.CodeInstance ? x.debuginfo : nothing
    end
    if pc <= 0 || !(di isa Core.DebugInfo)
        out = Vector{StackFrame}(undef, length(frames))
        for i in 1:length(frames)
            f = frames[i]
            @assert length(f) == 7 "corrupt return from jl_lookup_code_address"
            func = f[1]::Symbol
            file = f[2]::Symbol
            linenum = f[3]::Int
            linfo = f[4]
            from_c = f[5]::Bool
            inlined = f[6]::Bool
            out[i] = StackFrame(func, file, linenum, linfo, from_c, inlined, pointer, 0)
        end
    else
        out = Vector{StackFrame}()
        _add_di_frames!(out, pointer, di, pc)
        reverse!(out)
    end
    return out
end

const top_level_scope_sym = Symbol("top-level scope")

function lookup(ip::Base.InterpreterIP)
    code = ip.code
    if code === nothing
        # interpreted top-level expression with no CodeInfo
        return [StackFrame(top_level_scope_sym, empty_sym, 0, nothing, false, false, 0)]
    end
    # prepare approximate code info
    if code isa MethodInstance && (meth = code.def; meth isa Method)
        func = meth.name
        file = meth.file
        line = meth.line
        codeinfo = meth.source
    else
        func = top_level_scope_sym
        file = empty_sym
        line = Int32(0)
        if code isa Core.CodeInstance
            codeinfo = code.inferred::CodeInfo
            def = code.def
            if isa(def, Core.ABIOverride)
                def = def.def
            end
            if isa(def, MethodInstance)
                let meth = def.def
                    if isa(meth, Method)
                        func = meth.name
                        file = meth.file
                        line = meth.line
                    end
                end
            end
        else
            codeinfo = code::CodeInfo
        end
    end
    def = (code isa CodeInfo ? StackTraces : code) # Module just used as a token for top-level code
    pc::Int = max(ip.stmt + 1, 0) # n.b. ip.stmt is 0-indexed
    scopes = IRShow.LineInfoNode[]
    IRShow.append_scopes!(scopes, pc, codeinfo.debuginfo, def)
    if isempty(scopes)
        return [StackFrame(func, file, line, code, false, false, 0)]
    end
    res = Vector{StackFrame}(undef, length(scopes))
    inlined = false
    def_local = def
    for i in eachindex(scopes)
        lno = scopes[i]
        if inlined
            def_local = lno.method
            def_local isa Union{Method,Core.CodeInstance,MethodInstance} || (def_local = nothing)
        else
            def_local = codeinfo
        end
        res[i] = StackFrame(IRShow.normalize_method_name(lno.method), lno.file, lno.line,
            def_local, false, inlined, 0)
        inlined = true
    end
    return res
end

"""
    stacktrace([trace::Vector{Ptr{Cvoid}},] [c_funcs::Bool=false])::StackTrace

Return a stack trace in the form of a vector of `StackFrame`s. (By default stacktrace
doesn't return C functions, but this can be enabled.) When called without specifying a
trace, `stacktrace` first calls `backtrace`.
"""
Base.@constprop :none function stacktrace(trace::Vector{<:Union{Base.InterpreterIP,Ptr{Cvoid}}}, c_funcs::Bool=false)
    stack = StackTrace()
    for ip in trace
        for frame in lookup(ip)
            # Skip frames that come from C calls.
            if c_funcs || !frame.from_c
                push!(stack, frame)
            end
        end
    end
    return stack
end

Base.@constprop :none function stacktrace(c_funcs::Bool=false)
    stack = stacktrace(backtrace(), c_funcs)
    # Remove frame for this function (and any functions called by this function).
    remove_frames!(stack, :stacktrace)
    # also remove all of the non-Julia functions that led up to this point (if that list is non-empty)
    c_funcs && deleteat!(stack, 1:(something(findfirst(frame -> !frame.from_c, stack), 1) - 1))
    return stack
end

"""
    remove_frames!(stack::StackTrace, name::Symbol)

Takes a `StackTrace` (a vector of `StackFrames`) and a function name (a `Symbol`) and
removes the `StackFrame` specified by the function name from the `StackTrace` (also removing
all frames above the specified function). Primarily used to remove `StackTraces` functions
from the `StackTrace` prior to returning it.
"""
function remove_frames!(stack::StackTrace, name::Symbol)
    deleteat!(stack, 1:something(findlast(frame -> frame.func == name, stack), 0))
    return stack
end

function remove_frames!(stack::StackTrace, names::Vector{Symbol})
    deleteat!(stack, 1:something(findlast(frame -> frame.func in names, stack), 0))
    return stack
end

"""
    remove_frames!(stack::StackTrace, m::Module)

Return the `StackTrace` with all `StackFrame`s from the provided `Module` removed.
"""
function remove_frames!(stack::StackTrace, m::Module)
    filter!(f -> !from(f, m), stack)
    return stack
end

is_top_level_frame(f::StackFrame) = f.linfo isa CodeInfo || (f.linfo === nothing && f.func === top_level_scope_sym)

function frame_method_or_module(lkup::StackFrame)
    code = lkup.linfo
    code isa Method && return code
    code isa Module && return code
    mi = frame_mi(lkup)
    mi isa MethodInstance || return nothing
    return mi.def
end

function frame_mi(lkup::StackFrame)
    code = lkup.linfo
    code isa Core.CodeInstance && (code = code.def)
    code isa Core.ABIOverride && (code = code.def)
    code isa MethodInstance || return nothing
    return code
end

function show_spec_linfo(io::IO, frame::StackFrame)
    linfo = frame.linfo
    if linfo === nothing
        if frame.func === empty_sym
            print(io, "ip:0x", string(frame.pointer, base=16))
        elseif frame.func === top_level_scope_sym
            print(io, "top-level scope")
        else
            Base.print_within_stacktrace(io, Base.demangle_function_name(string(frame.func)), bold=true)
        end
    elseif linfo isa CodeInfo
        print(io, "top-level scope")
    elseif linfo isa Module
        Base.print_within_stacktrace(io, Base.demangle_function_name(string(frame.func)), bold=true)
    else
        if linfo isa Union{MethodInstance, CodeInstance}
            def = frame_method_or_module(frame)
            if def isa Module
                Base.show_mi(io, linfo::MethodInstance, #=from_stackframe=#true)
            elseif linfo isa CodeInstance && linfo.owner !== nothing
                show_custom_spec_sig(io, linfo.owner, linfo, frame)
            else
                # Equivalent to the default implementation of `show_custom_spec_sig`
                # for `linfo isa CodeInstance`, but saves an extra dynamic dispatch.
                mi = frame_mi(frame)::MethodInstance
                show_spec_sig(io, def::Method, mi.specTypes)
            end
        else
            m = linfo::Method
            show_spec_sig(io, m, m.sig)
        end
    end
end

# Can be extended by compiler packages to customize backtrace display of custom code instance frames
function show_custom_spec_sig(io::IO, @nospecialize(owner), linfo::CodeInstance, frame::StackFrame)
    mi = Base.get_ci_mi(linfo)
    m = mi.def::Method # the case ::Module is handled in show_spec_linfo
    return show_spec_sig(io, m, mi.specTypes)
end

function show_spec_sig(io::IO, m::Method, @nospecialize(sig::Type))
    if get(io, :limit, :false)::Bool
        if !haskey(io, :displaysize)
            io = IOContext(io, :displaysize => displaysize(io))
        end
    end
    argnames = Base.method_argnames(m)
    argnames = replace(argnames, :var"#unused#" => :var"")
    if m.nkw > 0
        # rearrange call kw_impl(kw_args..., func, pos_args...) to func(pos_args...; kw_args)
        kwarg_types = Any[ fieldtype(sig, i) for i = 2:(1+m.nkw) ]
        uw = Base.unwrap_unionall(sig)::DataType
        pos_sig = Base.rewrap_unionall(Tuple{uw.parameters[(m.nkw+2):end]...}, sig)
        kwnames = argnames[2:(m.nkw+1)]
        for i = 1:length(kwnames)
            str = string(kwnames[i])::String
            if endswith(str, "...")
                kwnames[i] = Symbol(str[1:end-3])
            end
        end
        Base.show_tuple_as_call(io, m.name, pos_sig;
                                demangle=true,
                                kwargs=zip(kwnames, kwarg_types),
                                argnames=argnames[m.nkw+2:end])
    else
        Base.show_tuple_as_call(io, m.name, sig; demangle=true, argnames)
    end
end

function show(io::IO, frame::StackFrame)
    show_spec_linfo(io, frame)
    if frame.file !== empty_sym
        file_info = basename(string(frame.file))
        print(io, " at ")
        print(io, file_info, ":")
        if frame.line >= 0
            print(io, frame.line)
        else
            print(io, "?")
        end
    end
    if frame.inlined
        print(io, " [inlined]")
    end
end

function Base.parentmodule(frame::StackFrame)
    linfo = frame.linfo
    if linfo isa CodeInstance
        linfo = linfo.def
        if isa(linfo, Core.ABIOverride)
            linfo = linfo.def
        end
    end
    if linfo isa MethodInstance
        def = linfo.def
        if def isa Module
            return def
        else
            return (def::Method).module
        end
    elseif linfo isa Method
        return linfo.module
    elseif linfo isa Module
        return linfo
    else
        # The module is not always available (common reasons include
        # frames arising from the interpreter)
        nothing
    end
end

"""
    from(frame::StackFrame, filter_mod::Module)::Bool

Return whether the `frame` is from the provided `Module`
"""
function from(frame::StackFrame, m::Module)
    return parentmodule(frame) === m
end

end  # module StackTraces
