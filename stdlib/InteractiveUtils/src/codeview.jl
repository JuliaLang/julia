# This file is a part of Julia. License is MIT: https://julialang.org/license

# highlighting settings
const highlighting = Dict{Symbol, Bool}(
    :warntype => true,
    :llvm => true,
    :native => true,
)

const llstyle = Dict{Symbol, Tuple{Bool, Union{Symbol, Int}}}(
    :default     => (false, :normal), # e.g. comma, equal sign, unknown token
    :comment     => (false, :light_black),
    :label       => (false, :light_red),
    :instruction => ( true, :light_cyan),
    :type        => (false, :cyan),
    :number      => (false, :yellow),
    :bracket     => (false, :yellow),
    :variable    => (false, :normal), # e.g. variable, register
    :keyword     => (false, :light_magenta),
    :funcname    => (false, :light_yellow),
)

function printstyled_ll(io::IO, x, s::Symbol, trailing_spaces="")
    printstyled(io, x, bold=llstyle[s][1], color=llstyle[s][2])
    print(io, trailing_spaces)
end

# displaying type warnings

function warntype_type_printer(io::IO; @nospecialize(type), used::Bool, show_type::Bool=true, _...)
    (show_type && used) || return nothing
    str = "::$type"
    if !highlighting[:warntype]
        print(io, str)
    elseif type isa Union && is_expected_union(type)
        Base.emphasize(io, str, Base.warn_color()) # more mild user notification
    elseif type isa Type && (!Base.isdispatchelem(type) || type == Core.Box)
        Base.emphasize(io, str)
    else
        Base.printstyled(io, str, color=:cyan) # show the "good" type
    end
    return nothing
end

# True if one can be pretty certain that the compiler handles this union well,
# i.e. must be small with concrete types.
function is_expected_union(u::Union)
    Base.unionlen(u) < 4 || return false
    for x in Base.uniontypes(u)
        if !Base.isdispatchelem(x) || x == Core.Box
            return false
        end
    end
    return true
end

function print_warntype_codeinfo(io::IO, src::Core.CodeInfo, @nospecialize(rettype), nargs::Int; lineprinter, label_dynamic_calls)
    if src.slotnames !== nothing
        slotnames = Base.sourceinfo_slotnames(src)
        io = IOContext(io, :SOURCE_SLOTNAMES => slotnames)
        slottypes = src.slottypes
        nargs > 0 && println(io, "Arguments")
        for i = 1:length(slotnames)
            if i == nargs + 1
                println(io, "Locals")
            end
            print(io, "  ", slotnames[i])
            if isa(slottypes, Vector{Any})
                warntype_type_printer(io; type=slottypes[i], used=true)
            end
            println(io)
        end
    end
    print(io, "Body")
    warntype_type_printer(io; type=rettype, used=true)
    println(io)
    irshow_config = Base.IRShow.IRShowConfig(lineprinter(src), warntype_type_printer; label_dynamic_calls)
    Base.IRShow.show_ir(io, src, irshow_config)
    println(io)
end

function print_warntype_mi(io::IO, mi::Core.MethodInstance)
    println(io, mi)
    print(io, "  from ")
    println(io, mi.def)
    if !isempty(mi.sparam_vals)
        println(io, "Static Parameters")
        sig = mi.def.sig
        warn_color = Base.warn_color() # more mild user notification
        for i = 1:length(mi.sparam_vals)
            sig = sig::UnionAll
            name = sig.var.name
            val = mi.sparam_vals[i]
            print_highlighted(io::IO, v::String, color::Symbol) =
                if highlighting[:warntype]
                    Base.printstyled(io, v; color)
                else
                    Base.print(io, v)
                end
            if val isa TypeVar
                if val.lb === Union{}
                    print(io, "  ", name, " <: ")
                    print_highlighted(io, "$(val.ub)", warn_color)
                elseif val.ub === Any
                    print(io, "  ", sig.var.name, " >: ")
                    print_highlighted(io, "$(val.lb)", warn_color)
                else
                    print(io, "  ")
                    print_highlighted(io, "$(val.lb)", warn_color)
                    print(io, " <: ", sig.var.name, " <: ")
                    print_highlighted(io, "$(val.ub)", warn_color)
                end
            elseif val isa typeof(Vararg)
                print(io, "  ", name, "::")
                print_highlighted(io, "Int", warn_color)
            else
                print(io, "  ", sig.var.name, " = ")
                print_highlighted(io, "$(val)", :cyan) # show the "good" type
            end
            println(io)
            sig = sig.body
        end
    end
end

"""
    code_warntype([io::IO], f, types; debuginfo=:default)

Prints lowered and type-inferred ASTs for the methods matching the given generic function
and type signature to `io` which defaults to `stdout`. The ASTs are annotated in such a way
as to cause non-concrete types which may be problematic for performance to be emphasized
(if color is available, displayed in red). This serves as a warning of potential type instability.

Not all non-concrete types are particularly problematic for performance, and the performance
characteristics of a particular type is an implementation detail of the compiler.
`code_warntype` will err on the side of coloring types red if they might be a performance
concern, so some types may be colored red even if they do not impact performance.
Small unions of concrete types are usually not a concern, so these are highlighted in yellow.

Keyword argument `debuginfo` may be one of `:source` or `:none` (default), to specify the verbosity of code comments.

See the [`@code_warntype`](@ref man-code-warntype) section in the Performance Tips page of the manual for more information.

See also: [`@code_warntype`](@ref), [`code_typed`](@ref), [`code_lowered`](@ref), [`code_llvm`](@ref), [`code_native`](@ref).
"""
function code_warntype(io::IO, @nospecialize(f), @nospecialize(tt=Base.default_tt(f));
                       world=Base.get_world_counter(),
                       interp::Base.Compiler.AbstractInterpreter=Base.Compiler.NativeInterpreter(world),
                       debuginfo::Symbol=:default, optimize::Bool=false, kwargs...)
    (ccall(:jl_is_in_pure_context, Bool, ()) || world == typemax(UInt)) &&
        error("code reflection cannot be used from generated functions")
    debuginfo = Base.IRShow.debuginfo(debuginfo)
    lineprinter = Base.IRShow.__debuginfo[debuginfo]
    nargs::Int = 0
    if isa(f, Core.OpaqueClosure)
        isa(f.source, Method) && (nargs = f.source.nargs)
        print_warntype_codeinfo(io, Base.code_typed_opaque_closure(f, tt)[1]..., nargs;
                                lineprinter, label_dynamic_calls = optimize)
        return nothing
    end
    tt = Base.signature_type(f, tt)
    matches = findall(tt, Base.Compiler.method_table(interp))
    matches === nothing && Base.raise_match_failure(:code_warntype, tt)
    for match in matches.matches
        match = match::Core.MethodMatch
        src = Base.Compiler.typeinf_code(interp, match, optimize)
        mi = Base.Compiler.specialize_method(match)
        mi.def isa Method && (nargs = (mi.def::Method).nargs)
        print_warntype_mi(io, mi)
        if src isa Core.CodeInfo
            print_warntype_codeinfo(io, src, src.rettype, nargs;
                                    lineprinter, label_dynamic_calls = optimize)
        else
            println(io, "  inference not successful")
        end
    end
    nothing
end
code_warntype(f; kwargs...) = (@nospecialize; code_warntype(stdout, f; kwargs...))
code_warntype(f, argtypes; kwargs...) = (@nospecialize; code_warntype(stdout, f, argtypes; kwargs...))

using Base: CodegenParams

const GENERIC_SIG_WARNING = "; WARNING: This code may not match what actually runs.\n"
const OC_MISMATCH_WARNING =
"""
; WARNING: The pre-inferred opaque closure is not callable with the given arguments
;          and will error on dispatch with this signature.
"""

# Printing code representations in IR and assembly

function _dump_function(@nospecialize(f), @nospecialize(t), native::Bool, wrapper::Bool,
                        raw::Bool, dump_module::Bool, syntax::Symbol,
                        optimize::Bool, debuginfo::Symbol, binary::Bool,
                        params::CodegenParams=CodegenParams(debug_info_kind=Cint(0), debug_info_level=Cint(2), safepoint_on_entry=raw, gcstack_arg=raw))
    ccall(:jl_is_in_pure_context, Bool, ()) && error("code reflection cannot be used from generated functions")
    if isa(f, Core.Builtin)
        throw(ArgumentError("argument is not a generic function"))
    end
    warning = ""
    # get the MethodInstance for the method match
    if !isa(f, Core.OpaqueClosure)
        world = Base.get_world_counter()
        match = Base._which(signature_type(f, t); world)
        mi = Base.specialize_method(match)
        # TODO: use jl_is_cacheable_sig instead of isdispatchtuple
        isdispatchtuple(mi.specTypes) || (warning = GENERIC_SIG_WARNING)
    else
        world = UInt64(f.world)
        tt = Base.to_tuple_type(t)
        if !isdefined(f.source, :source)
            # OC was constructed from inferred source. There's only one
            # specialization and we can't infer anything more precise either.
            world = f.source.primary_world
            mi = f.source.specializations::Core.MethodInstance
            Base.hasintersect(typeof(f).parameters[1], tt) || (warning = OC_MISMATCH_WARNING)
        else
            mi = Base.specialize_method(f.source, Tuple{typeof(f.captures), tt.parameters...}, Core.svec())
            isdispatchtuple(mi.specTypes) || (warning = GENERIC_SIG_WARNING)
        end
    end
    # get the code for it
    if debuginfo === :default
        debuginfo = :source
    elseif debuginfo !== :source && debuginfo !== :none
        throw(ArgumentError("'debuginfo' must be either :source or :none"))
    end
    if native
        if syntax !== :att && syntax !== :intel
            throw(ArgumentError("'syntax' must be either :intel or :att"))
        end
        str = ""
        if !dump_module
            # if we don't want the module metadata, attempt to disassemble what our JIT has
            str = _dump_function_native_disassembly(mi, world, wrapper, syntax, debuginfo, binary)
        end
        if isempty(str)
            # if that failed (or we want metadata), use LLVM to generate more accurate assembly output
            if !isa(f, Core.OpaqueClosure)
                src = Base.Compiler.typeinf_code(Base.Compiler.NativeInterpreter(world), mi, true)
            else
                src, rt = Base.get_oc_code_rt(nothing, f, tt, true)
            end
            src isa Core.CodeInfo || error("failed to infer source for $mi")
            str = _dump_function_native_assembly(mi, src, wrapper, syntax, debuginfo, binary, raw, params)
        end
    else
        if !isa(f, Core.OpaqueClosure)
            src = Base.Compiler.typeinf_code(Base.Compiler.NativeInterpreter(world), mi, true)
        else
            src, rt = Base.get_oc_code_rt(nothing, f, tt, true)
        end
        src isa Core.CodeInfo || error("failed to infer source for $mi")
        str = _dump_function_llvm(mi, src, wrapper, !raw, dump_module, optimize, debuginfo, params)
    end
    str = warning * str
    return str
end

function _dump_function_native_disassembly(mi::Core.MethodInstance, world::UInt,
                                           wrapper::Bool, syntax::Symbol,
                                           debuginfo::Symbol, binary::Bool)
    str = @ccall jl_dump_method_asm(mi::Any, world::UInt, false::Bool, wrapper::Bool,
                                    syntax::Ptr{UInt8}, debuginfo::Ptr{UInt8},
                                    binary::Bool)::Ref{String}
    return str
end

struct LLVMFDump
    tsm::Ptr{Cvoid} # opaque
    f::Ptr{Cvoid} # opaque
end

function _dump_function_native_assembly(mi::Core.MethodInstance, src::Core.CodeInfo,
                                        wrapper::Bool, syntax::Symbol, debuginfo::Symbol,
                                        binary::Bool, raw::Bool, params::CodegenParams)
    llvmf_dump = Ref{LLVMFDump}()
    @ccall jl_get_llvmf_defn(llvmf_dump::Ptr{LLVMFDump}, mi::Any, src::Any, wrapper::Bool,
                             true::Bool, params::CodegenParams)::Cvoid
    llvmf_dump[].f == C_NULL && error("could not compile the specified method")
    str = @ccall jl_dump_function_asm(llvmf_dump::Ptr{LLVMFDump}, false::Bool,
                                      syntax::Ptr{UInt8}, debuginfo::Ptr{UInt8},
                                      binary::Bool, raw::Bool)::Ref{String}
    return str
end

function _dump_function_llvm(
        mi::Core.MethodInstance, src::Core.CodeInfo, wrapper::Bool,
        strip_ir_metadata::Bool, dump_module::Bool,
        optimize::Bool, debuginfo::Symbol,
        params::CodegenParams)
    llvmf_dump = Ref{LLVMFDump}()
    @ccall jl_get_llvmf_defn(llvmf_dump::Ptr{LLVMFDump}, mi::Any, src::Any,
                             wrapper::Bool, optimize::Bool, params::CodegenParams)::Cvoid
    llvmf_dump[].f == C_NULL && error("could not compile the specified method")
    str = @ccall jl_dump_function_ir(llvmf_dump::Ptr{LLVMFDump}, strip_ir_metadata::Bool,
                                     dump_module::Bool, debuginfo::Ptr{UInt8})::Ref{String}
    return str
end

"""
    code_llvm([io=stdout,], f, types; raw=false, dump_module=false, optimize=true, debuginfo=:default)

Prints the LLVM bitcodes generated for running the method matching the given generic
function and type signature to `io`.

If the `optimize` keyword is unset, the code will be shown before LLVM optimizations.
All metadata and dbg.* calls are removed from the printed bitcode. For the full IR, set the `raw` keyword to true.
To dump the entire module that encapsulates the function (with declarations), set the `dump_module` keyword to true.
Keyword argument `debuginfo` may be one of source (default) or none, to specify the verbosity of code comments.

See also: [`@code_llvm`](@ref), [`code_warntype`](@ref), [`code_typed`](@ref), [`code_lowered`](@ref), [`code_native`](@ref).
"""
function code_llvm(io::IO, @nospecialize(f), @nospecialize(types=Base.default_tt(f));
                   raw::Bool=false, dump_module::Bool=false, optimize::Bool=true, debuginfo::Symbol=:default,
                   params::CodegenParams=CodegenParams(debug_info_kind=Cint(0), debug_info_level=Cint(2), safepoint_on_entry=raw, gcstack_arg=raw))
    d = _dump_function(f, types, false, false, raw, dump_module, :intel, optimize, debuginfo, false, params)
    if highlighting[:llvm] && get(io, :color, false)::Bool
        print_llvm(io, d)
    else
        print(io, d)
    end
end
code_llvm(args...; kwargs...) = (@nospecialize; code_llvm(stdout, args...; kwargs...))

"""
    code_native([io=stdout,], f, types; syntax=:intel, debuginfo=:default, binary=false, dump_module=true)

Prints the native assembly instructions generated for running the method matching the given
generic function and type signature to `io`.

* Set assembly syntax by setting `syntax` to `:intel` (default) for intel syntax or `:att` for AT&T syntax.
* Specify verbosity of code comments by setting `debuginfo` to `:source` (default) or `:none`.
* If `binary` is `true`, also print the binary machine code for each instruction precedented by an abbreviated address.
* If `dump_module` is `false`, do not print metadata such as rodata or directives.
* If `raw` is `false`, uninteresting instructions (like the safepoint function prologue) are elided.

See also: [`@code_native`](@ref), [`code_warntype`](@ref), [`code_typed`](@ref), [`code_lowered`](@ref), [`code_llvm`](@ref).
"""
function code_native(io::IO, @nospecialize(f), @nospecialize(types=Base.default_tt(f));
                     dump_module::Bool=true, syntax::Symbol=:intel, raw::Bool=false,
                     debuginfo::Symbol=:default, binary::Bool=false,
                     params::CodegenParams=CodegenParams(debug_info_kind=Cint(0), debug_info_level=Cint(2), safepoint_on_entry=raw, gcstack_arg=raw))
    d = _dump_function(f, types, true, false, raw, dump_module, syntax, true, debuginfo, binary, params)
    if highlighting[:native] && get(io, :color, false)::Bool
        print_native(io, d)
    else
        print(io, d)
    end
end
code_native(args...; kwargs...) = (@nospecialize; code_native(stdout, args...; kwargs...))

## colorized IR and assembly printing

const num_regex = r"^(?:\$?-?\d+|0x[0-9A-Fa-f]+|-?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?)$"

function print_llvm(io::IO, code::String)
    buf = IOBuffer(code)
    for line in eachline(buf)
        m = match(r"^(\s*)((?:[^;]|;\")*)(.*)$", line)
        m === nothing && continue
        indent, tokens, comment = m.captures
        print(io, indent)
        print_llvm_tokens(io, tokens)
        printstyled_ll(io, comment, :comment)
        println(io)
    end
end

const llvm_types =
    r"^(?:void|half|float|double|x86_\w+|ppc_\w+|label|metadata|type|opaque|token|i\d+)$"
const llvm_cond = r"^(?:[ou]?eq|[ou]?ne|[uso][gl][te]|ord|uno)$" # true|false

function print_llvm_tokens(io, tokens)
    m = match(r"^((?:[^\"\s:]+:|\"[^\"]*\":)?)(\s*)(.*)", tokens)
    if m !== nothing
        label, spaces, tokens = m.captures
        printstyled_ll(io, label, :label, spaces)
    end
    m = match(r"^(%[^\s=]+)(\s*)=(\s*)(.*)", tokens)
    if m !== nothing
        result, spaces, spaces2, tokens = m.captures
        printstyled_ll(io, result, :variable, spaces)
        printstyled_ll(io, '=', :default, spaces2)
    end
    m = match(r"^([a-z]\w*)(\s*)(.*)", tokens)
    if m !== nothing
        inst, spaces, tokens = m.captures
        iskeyword = occursin(r"^(?:define|declare|type)$", inst) || occursin("=", tokens)
        printstyled_ll(io, inst, iskeyword ? :keyword : :instruction, spaces)
    end

    print_llvm_operands(io, tokens)
end

function print_llvm_operands(io, tokens)
    while !isempty(tokens)
        tokens = print_llvm_operand(io, tokens)
    end
    return tokens
end

function print_llvm_operand(io, tokens)
    islabel = false
    while !isempty(tokens)
        m = match(r"^,(\s*)(.*)", tokens)
        if m !== nothing
            spaces, tokens = m.captures
            printstyled_ll(io, ',', :default, spaces)
            break
        end
        m = match(r"^(\*+|=)(\s*)(.*)", tokens)
        if m !== nothing
            sym, spaces, tokens = m.captures
            printstyled_ll(io, sym, :default, spaces)
            continue
        end
        m = match(r"^(\"[^\"]*\")(\s*)(.*)", tokens)
        if m !== nothing
            str, spaces, tokens = m.captures
            printstyled_ll(io, str, :variable, spaces)
            continue
        end
        m = match(r"^([({\[<])(\s*)(.*)", tokens)
        if m !== nothing
            bracket, spaces, tokens = m.captures
            printstyled_ll(io, bracket, :bracket, spaces)
            tokens = print_llvm_operands(io, tokens) # enter
            continue
        end
        m = match(r"^([)}\]>])(\s*)(.*)", tokens)
        if m !== nothing
            bracket, spaces, tokens = m.captures
            printstyled_ll(io, bracket, :bracket, spaces)
            break # leave
        end

        m = match(r"^([^\s,*=(){}\[\]<>]+)(\s*)(.*)", tokens)
        m === nothing && break
        token, spaces, tokens = m.captures
        if occursin(llvm_types, token)
            printstyled_ll(io, token, :type)
            islabel = token == "label"
        elseif occursin(llvm_cond, token) # condition code is instruction-level
            printstyled_ll(io, token, :instruction)
        elseif occursin(num_regex, token)
            printstyled_ll(io, token, :number)
        elseif occursin(r"^@.+$", token)
            printstyled_ll(io, token, :funcname)
        elseif occursin(r"^%.+$", token)
            islabel |= occursin(r"^%[^\d].*$", token) & occursin(r"^\]", tokens)
            printstyled_ll(io, token, islabel ? :label : :variable)
            islabel = false
        elseif occursin(r"^[a-z]\w+$", token)
            printstyled_ll(io, token, :keyword)
        else
            printstyled_ll(io, token, :default)
        end
        print(io, spaces)
    end
    return tokens
end

function print_native(io::IO, code::String, arch::Symbol=sys_arch_category())
    archv = Val(arch)
    buf = IOBuffer(code)
    for line in eachline(buf)
        m = match(r"^(\s*)((?:[^;#/]|#\S|;\"|/[^/])*)(.*)$", line)
        m === nothing && continue
        indent, tokens, comment = m.captures
        print(io, indent)
        print_native_tokens(io, tokens, archv)
        printstyled_ll(io, comment, :comment)
        println(io)
    end
end

function sys_arch_category()
    if Sys.ARCH === :x86_64 || Sys.ARCH === :i686
        :x86
    elseif Sys.ARCH === :aarch64 || startswith(string(Sys.ARCH), "arm")
        :arm
    else
        :unsupported
    end
end

print_native_tokens(io, line, ::Val) = print(io, line)

const x86_ptr = r"^(?:(?:[xyz]mm|[dq])?word|byte|ptr|offset)$"
const avx512flags = r"^(?:z|r[nduz]-sae|sae|1to1?\d)$"
const arm_cond = r"^(?:eq|ne|cs|ho|cc|lo|mi|pl|vs|vc|hi|ls|[lg][te]|al|nv)$"
const arm_keywords = r"^(?:lsl|lsr|asr|ror|rrx|!|/[zm])$"

function print_native_tokens(io, tokens, arch::Union{Val{:x86}, Val{:arm}})
    x86 = arch isa Val{:x86}
    m = match(r"^((?:[^\s:]+:|\"[^\"]+\":)?)(\s*)(.*)", tokens)
    if m !== nothing
        label, spaces, tokens = m.captures
        printstyled_ll(io, label, :label, spaces)
    end
    haslabel = false
    m = match(r"^([a-z][\w.]*)(\s*)(.*)", tokens)
    if m !== nothing
        instruction, spaces, tokens = m.captures
        printstyled_ll(io, instruction, :instruction, spaces)
        haslabel = occursin(r"^(?:bl?|bl?\.\w{2,5}|[ct]bn?z)?$", instruction)
    end

    isfuncname = false
    while !isempty(tokens)
        m = match(r"^([,:*])(\s*)(.*)", tokens)
        if m !== nothing
            sym, spaces, tokens = m.captures
            printstyled_ll(io, sym, :default, spaces)
            isfuncname = false
            continue
        end
        m = match(r"^([(){}\[\]])(\s*)(.*)", tokens)
        if m !== nothing
            bracket, spaces, tokens = m.captures
            printstyled_ll(io, bracket, :bracket, spaces)
            continue
        end
        m = match(r"^#([0-9a-fx.-]+)(\s*)(.*)", tokens)
        if !x86 && m !== nothing && occursin(num_regex, m.captures[1])
            num, spaces, tokens = m.captures
            printstyled_ll(io, "#" * num, :number, spaces)
            continue
        end

        m = match(r"^([^\s,:*(){}\[\]][^\s,:*/(){}\[\]]*)(\s*)(.*)", tokens)
        m === nothing && break
        token, spaces, tokens = m.captures
        if occursin(num_regex, token)
            printstyled_ll(io, token, :number)
        elseif x86 && occursin(x86_ptr, token) || occursin(avx512flags, token)
            printstyled_ll(io, token, :keyword)
            isfuncname = token == "offset"
        elseif !x86 && (occursin(arm_keywords, token) || occursin(arm_cond, token))
            printstyled_ll(io, token, :keyword)
        elseif occursin(r"^L.+$", token)
            printstyled_ll(io, token, :label)
        elseif occursin(r"^\$.+$", token)
            printstyled_ll(io, token, :funcname)
        elseif occursin(r"^%?(?:[a-z][\w.]+|\"[^\"]+\")$", token)
            islabel = haslabel & !occursin(',', tokens)
            printstyled_ll(io, token, islabel ? :label : isfuncname ? :funcname : :variable)
            isfuncname = false
        else
            printstyled_ll(io, token, :default)
        end
        print(io, spaces)
    end
end
