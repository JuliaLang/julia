# This file is a part of Julia. License is MIT: https://julialang.org/license

# highlighting settings
highlighting = Dict{Symbol, Bool}(
    :warntype => true,
    :llvm => true,
    :native => true,
)

llstyle = Dict{Symbol, Tuple{Bool, Union{Symbol, Int}}}(
    :default     => (false, :light_black), # e.g. comma, equal sign, unknown token
    :comment     => (false, :green),
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

function warntype_type_printer(io::IO, @nospecialize(ty), used::Bool)
    used || return
    if ty isa Type && (!Base.isdispatchelem(ty) || ty == Core.Box)
        if highlighting[:warntype] && ty isa Union && Base.is_expected_union(ty)
            Base.emphasize(io, "::$ty", Base.warn_color()) # more mild user notification
        else
            Base.emphasize(io, "::$ty")
        end
    else
        if highlighting[:warntype]
            Base.printstyled(io, "::$ty", color=:cyan) # show the "good" type
        else
            Base.print(io, "::$ty")
        end
    end
    nothing
end

"""
    code_warntype([io::IO], f, types; debuginfo=:default)

Prints lowered and type-inferred ASTs for the methods matching the given generic function
and type signature to `io` which defaults to `stdout`. The ASTs are annotated in such a way
as to cause "non-leaf" types to be emphasized (if color is available, displayed in red).
This serves as a warning of potential type instability. Not all non-leaf types are particularly
problematic for performance, so the results need to be used judiciously.
In particular, unions containing either [`missing`](@ref) or [`nothing`](@ref) are displayed in yellow, since
these are often intentional.

Keyword argument `debuginfo` may be one of `:source` or `:none` (default), to specify the verbosity of code comments.

See [`@code_warntype`](@ref man-code-warntype) for more information.
"""
function code_warntype(io::IO, @nospecialize(f), @nospecialize(t); debuginfo::Symbol=:default, optimize::Bool=false)
    debuginfo = Base.IRShow.debuginfo(debuginfo)
    lineprinter = Base.IRShow.__debuginfo[debuginfo]
    for (src, rettype) in code_typed(f, t, optimize=optimize)
        lambda_io::IOContext = io
        if src.slotnames !== nothing
            slotnames = Base.sourceinfo_slotnames(src)
            lambda_io = IOContext(lambda_io, :SOURCE_SLOTNAMES => slotnames)
            println(io, "Variables")
            slottypes = src.slottypes
            for i = 1:length(slotnames)
                print(io, "  ", slotnames[i])
                if isa(slottypes, Vector{Any})
                    warntype_type_printer(io, slottypes[i], true)
                end
                println(io)
            end
            println(io)
        end
        print(io, "Body")
        warntype_type_printer(io, rettype, true)
        println(io)
        # TODO: static parameter values
        Base.IRShow.show_ir(lambda_io, src, lineprinter(src), warntype_type_printer)
    end
    nothing
end
code_warntype(@nospecialize(f), @nospecialize(t); kwargs...) =
    code_warntype(stdout, f, t; kwargs...)

import Base.CodegenParams

# Printing code representations in IR and assembly
function _dump_function(@nospecialize(f), @nospecialize(t), native::Bool, wrapper::Bool,
                        strip_ir_metadata::Bool, dump_module::Bool, syntax::Symbol,
                        optimize::Bool, debuginfo::Symbol,
                        params::CodegenParams=CodegenParams())
    ccall(:jl_is_in_pure_context, Bool, ()) && error("code reflection cannot be used from generated functions")
    if isa(f, Core.Builtin)
        throw(ArgumentError("argument is not a generic function"))
    end
    # get the MethodInstance for the method match
    world = typemax(UInt)
    meth = which(f, t)
    t = to_tuple_type(t)
    tt = signature_type(f, t)
    (ti, env) = ccall(:jl_type_intersection_with_env, Any, (Any, Any), tt, meth.sig)::Core.SimpleVector
    meth = Base.func_for_method_checked(meth, ti, env)
    linfo = ccall(:jl_specializations_get_linfo, Ref{Core.MethodInstance}, (Any, Any, Any, UInt), meth, ti, env, world)
    # get the code for it
    if native
        str = _dump_function_linfo_native(linfo, world, wrapper, syntax, debuginfo)
    else
        str = _dump_function_linfo_llvm(linfo, world, wrapper, strip_ir_metadata, dump_module, optimize, debuginfo, params)
    end
    # TODO: use jl_is_cacheable_sig instead of isdispatchtuple
    isdispatchtuple(linfo.specTypes) || (str = "; WARNING: This code may not match what actually runs.\n" * str)
    return str
end

function _dump_function_linfo_native(linfo::Core.MethodInstance, world::UInt, wrapper::Bool, syntax::Symbol, debuginfo::Symbol)
    if syntax !== :att && syntax !== :intel
        throw(ArgumentError("'syntax' must be either :intel or :att"))
    end
    if debuginfo === :default
        debuginfo = :source
    elseif debuginfo !== :source && debuginfo !== :none
        throw(ArgumentError("'debuginfo' must be either :source or :none"))
    end
    str = ccall(:jl_dump_method_asm, Ref{String},
                (Any, UInt, Cint, Bool, Ptr{UInt8}, Ptr{UInt8}),
                linfo, world, 0, wrapper, syntax, debuginfo)
    return str
end

function _dump_function_linfo_llvm(
        linfo::Core.MethodInstance, world::UInt, wrapper::Bool,
        strip_ir_metadata::Bool, dump_module::Bool,
        optimize::Bool, debuginfo::Symbol,
        params::CodegenParams)
    if debuginfo === :default
        debuginfo = :source
    elseif debuginfo !== :source && debuginfo !== :none
        throw(ArgumentError("'debuginfo' must be either :source or :none"))
    end
    llvmf = ccall(:jl_get_llvmf_defn, Ptr{Cvoid}, (Any, UInt, Bool, Bool, CodegenParams), linfo, world, wrapper, optimize, params)
    llvmf == C_NULL && error("could not compile the specified method")
    str = ccall(:jl_dump_function_ir, Ref{String},
                (Ptr{Cvoid}, Bool, Bool, Ptr{UInt8}),
                llvmf, strip_ir_metadata, dump_module, debuginfo)
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
"""
function code_llvm(io::IO, @nospecialize(f), @nospecialize(types), raw::Bool,
                   dump_module::Bool=false, optimize::Bool=true, debuginfo::Symbol=:default)
    d = _dump_function(f, types, false, false, !raw, dump_module, :att, optimize, debuginfo)
    if highlighting[:llvm] && get(io, :color, false)
        print_llvm(io, d)
    else
        print(io, d)
    end
end
code_llvm(io::IO, @nospecialize(f), @nospecialize(types=Tuple); raw::Bool=false, dump_module::Bool=false, optimize::Bool=true, debuginfo::Symbol=:default) =
    code_llvm(io, f, types, raw, dump_module, optimize, debuginfo)
code_llvm(@nospecialize(f), @nospecialize(types=Tuple); raw=false, dump_module=false, optimize=true, debuginfo::Symbol=:default) =
    code_llvm(stdout, f, types; raw=raw, dump_module=dump_module, optimize=optimize, debuginfo=debuginfo)


"""
    code_native([io=stdout,], f, types; syntax=:att, debuginfo=:default)

Prints the native assembly instructions generated for running the method matching the given
generic function and type signature to `io`.
Switch assembly syntax using `syntax` symbol parameter set to `:att` for AT&T syntax or `:intel` for Intel syntax.
Keyword argument `debuginfo` may be one of source (default) or none, to specify the verbosity of code comments.
"""
function code_native(io::IO, @nospecialize(f), @nospecialize(types=Tuple);
                     syntax::Symbol=:att, debuginfo::Symbol=:default)
    d = _dump_function(f, types, true, false, false, false, syntax, true, debuginfo)
    if highlighting[:native] && get(io, :color, false)
        print_native(io, d)
    else
        print(io, d)
    end
end
code_native(@nospecialize(f), @nospecialize(types=Tuple); syntax::Symbol=:att, debuginfo::Symbol=:default) =
    code_native(stdout, f, types; syntax=syntax, debuginfo=debuginfo)
code_native(::IO, ::Any, ::Symbol) = error("illegal code_native call") # resolve ambiguous call

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
    m = match(r"^((?:[^\s:]+:)?)(\s*)(.*)", tokens)
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
