# This file is a part of Julia. License is MIT: https://julialang.org/license

import ..Compiler: verify_typeinf_trim, NativeInterpreter, argtypes_to_type, compileable_specialization_for_call

using ..Compiler:
     # operators
     !, !=, !==, %, -, +, :, <, <=, ==, =>, >, >=, ^, ∈, ∉,
     # types
     Array, Builtin, Callable, Cint, CodeInfo, CodeInstance, Csize_t, Exception,
     GenericMemory, GlobalRef, IdDict, IdSet, IntrinsicFunction, Method, MethodInstance,
     NamedTuple, Pair, PhiCNode, PhiNode, PiNode, QuoteNode, SSAValue, SimpleVector, String,
     Tuple, VarState, Vector,
     # functions
     argextype, empty!, error, get, get_ci_mi, get_world_counter, getglobal, getindex, getproperty,
     hasintersect, haskey, in, isdefinedglobal, isdispatchelem, isempty, isexpr, iterate, length, map!, max,
     pop!, popfirst!, push!, pushfirst!, reinterpret, reverse!, reverse, setindex!,
     setproperty!, similar, singleton_type, sptypes_from_meth_instance, sp_type_rewrap,
     unsafe_pointer_to_objref, widenconst, isconcretetype,
     # misc
     @nospecialize, @assert, C_NULL
using ..IRShow: LineInfoNode, print, show, println, append_scopes!, IOContext, IO, normalize_method_name, is_expected_union
using ..Base: Base, sourceinfo_slotnames, printstyled
using ..Base.StackTraces: StackFrame

## declarations ##

struct CallMissing <: Exception
    codeinst::CodeInstance
    codeinfo::CodeInfo
    sptypes::Vector{VarState}
    stmtidx::Int
    desc::String
end

struct CCallableMissing <: Exception
    rt
    sig
    desc
end

const ParentMap = IdDict{CodeInstance,Tuple{CodeInstance,Int}}
const ErrorList = Vector{Pair{Bool,Any}} # severity => exception

const runtime_functions = Symbol[
    # a denylist of any runtime functions which someone might ccall which can call jl_apply or access reflection state
    # which might not be captured by the trim output
    :jl_apply,
]

# Check if a type is "stable" - uses same classification as code_warntype
function is_type_stable(@nospecialize(typ))
    typ isa Core.Const && return true
    typ === Union{} && return false
    typ == Core.Box && return false
    typ isa Union && return is_expected_union(typ)
    return isdispatchelem(typ)
end

# Color printing for types - same colors as code_warntype:
# - red bold: unstable/abstract types
# - yellow: expected unions (small unions of concrete types)
# - light_black: stable concrete types
function print_type_colored(io::IO, @nospecialize(typ))
    color = get(io, :color, false)::Bool
    if !color
        print(io, typ)
    elseif typ === Union{}
        printstyled(io, typ; color=:red)
    elseif typ == Core.Box
        printstyled(io, typ; color=:red, bold=true)
    elseif typ isa Union && is_expected_union(typ)
        printstyled(io, typ; color=:yellow)
    elseif isdispatchelem(typ)
        printstyled(io, typ; color=:light_black)
    else
        printstyled(io, typ; color=:red, bold=true)
    end
end


# Print a value with optional stable coloring (light_black if stable)
function print_value(io::IO, @nospecialize(x), stable::Bool)
    stable ? printstyled(io, x; color=:light_black) : print(io, x)
end

# Unwrap SSAValue and PiNode to get the underlying statement
function unwrap_stmt(codeinfo::CodeInfo, @nospecialize(stmt))
    while true
        if stmt isa SSAValue
            stmt = codeinfo.code[stmt.id]
            isexpr(stmt, :(=)) && (stmt = stmt.args[2])
        elseif stmt isa PiNode
            stmt = stmt.val
        else
            return stmt
        end
    end
end

# Convert Core.Argument to slot name symbol if available
function argument_name(codeinfo::CodeInfo, arg::Core.Argument)
    slotnames = codeinfo.slotnames
    if slotnames !== nothing && arg.n <= length(slotnames)
        return slotnames[arg.n]
    end
    return arg
end

const MAX_NESTING_DEPTH = 1

function is_call_expr(codeinfo::CodeInfo, @nospecialize(stmt))
    stmt = unwrap_stmt(codeinfo, stmt)
    return stmt isa Expr && stmt.head ∈ (:call, :invoke, :foreigncall, :new)
end

function has_unstable_arg(codeinfo::CodeInfo, sptypes::Vector{VarState}, args, startidx::Int)
    for i in (startidx + 1):length(args)
        is_type_stable(widenconst(argextype(args[i], codeinfo, sptypes))) || return true
    end
    return false
end

function print_value(io::IO, codeinfo::CodeInfo, @nospecialize(stmt), stable::Bool)
    stmt = unwrap_stmt(codeinfo, stmt)
    if stmt isa GlobalRef
        # TODO: This is not correct.
        print_value(io, stmt.name, stable)
    elseif stmt isa Core.Argument
        print_value(io, argument_name(codeinfo, stmt), stable)
    elseif stmt isa QuoteNode && stmt.value isa Symbol
        print_value(io, Base.repr(stmt.value), stable)  # Show :symbol with colon
    else
        print_value(io, Base.repr(stmt), stable)
    end
end

# Print value with type annotation: value::type
function print_typed(io::IO, codeinfo::CodeInfo, sptypes::Vector{VarState}, @nospecialize(stmt), stable::Bool;
                     depth::Int=0, indent::Int=0)
    typ = widenconst(argextype(stmt, codeinfo, sptypes))
    arg_stable = is_type_stable(typ)
    # Fold deeply nested stable calls
    if arg_stable && depth >= MAX_NESTING_DEPTH && is_call_expr(codeinfo, stmt)
        printstyled(io, "(…)"; color=:light_black)
    else
        print_stmt_colored(io, codeinfo, sptypes, stmt; depth=depth, stable=arg_stable, indent=indent)
    end
    printstyled(io, "::"; color=:light_black)
    print_type_colored(io, typ)
end

function should_elide_type(@nospecialize(stmt), @nospecialize(typ))
    stmt isa GlobalRef && isdispatchelem(typ) && return true
    stmt isa Callable && return true
    return false
end

function print_stmt_colored(io::IO, codeinfo::CodeInfo, sptypes::Vector{VarState}, @nospecialize(stmt);
                            depth::Int=0, stable::Bool=false, indent::Int=0)
    stmt = unwrap_stmt(codeinfo, stmt)

    if !is_call_expr(codeinfo, stmt)
        return print_value(io, codeinfo, stmt, stable)
    end

    if stmt.head === :foreigncall
        print_value(io, "ccall", stable)
        print(io, "(")
        length(stmt.args) >= 1 && print_value(io, codeinfo, stmt.args[1], stable)
        print(io, ")")
    else
        startidx = stmt.head === :invoke ? 2 : 1
        farg = startidx <= length(stmt.args) ? stmt.args[startidx] : nothing
        nargs = length(stmt.args) - startidx

        # Print function call
        if farg !== nothing
            fstmt = unwrap_stmt(codeinfo, farg)
            ftyp = widenconst(argextype(farg, codeinfo, sptypes))
            needs_type = !should_elide_type(fstmt, ftyp)
            needs_type && print(io, "(")
            print_value(io, codeinfo, farg, stable)
            if needs_type
                printstyled(io, "::"; color=:light_black)
                print_type_colored(io, ftyp)
                print(io, ")")
            end
        end
        print(io, "(")
        use_multiline = !stable && nargs > 0 && has_unstable_arg(codeinfo, sptypes, stmt.args, startidx)
        for i in (startidx + 1):length(stmt.args)
            i > startidx + 1 && print(io, ",")
            if use_multiline
                println(io)
                print(io, "  " ^ (indent + 1))
            elseif i > startidx + 1
                print(io, " ")
            end
            print_typed(io, codeinfo, sptypes, stmt.args[i], stable; depth=depth+1, indent=indent+1)
        end
        use_multiline && nargs > 0 && (println(io); print(io, "  " ^ indent))
        print(io, ")")
    end
end

function verify_print_stmt(io::IO, codeinfo::CodeInfo, sptypes::Vector{VarState}, stmtidx::Int)
    codeinfo.slotnames !== nothing && (io = IOContext(io, :SOURCE_SLOTNAMES => sourceinfo_slotnames(codeinfo)))
    stmt = unwrap_stmt(codeinfo, codeinfo.code[stmtidx])
    typ = widenconst(argextype(SSAValue(stmtidx), codeinfo, sptypes))
    print_stmt_colored(io, codeinfo, sptypes, stmt)
    print(io, "::")
    print_type_colored(io, typ)
end

function verify_print_error(io::IO, desc::CallMissing, parents::ParentMap, warn::Bool)
    (; codeinst, codeinfo, sptypes, stmtidx, desc) = desc
    frames = verify_create_stackframes(codeinst, stmtidx, parents)
    color = warn ? Base.warn_color() : Base.error_color()
    printstyled(io, desc; color=color, bold=true)
    print(io, " from statement ")
    verify_print_stmt(io, codeinfo, sptypes, stmtidx)
    print(io, "\n")
    Base.show_backtrace(io, frames)
    print(io, "\n\n")
    nothing
end

function verify_print_error(io::IO, desc::CCallableMissing, ::ParentMap, warn::Bool)
    color = warn ? Base.warn_color() : Base.error_color()
    printstyled(io, desc.desc; color=color, bold=true)
    print(io, " for ")
    print_type_colored(io, desc.sig)
    print(io, " => ")
    print_type_colored(io, desc.rt)
    print(io, "\n\n")
    nothing
end

function verify_create_stackframes(codeinst::CodeInstance, stmtidx::Int, parents::ParentMap)
    scopes = LineInfoNode[]
    frames = StackFrame[]
    parent = (codeinst, stmtidx)
    visited = IdSet{Tuple{CodeInstance,Int}}()
    while parent !== nothing
        codeinst, stmtidx = parent
        di = codeinst.debuginfo
        append_scopes!(scopes, stmtidx, di, :var"unknown scope")
        for i in reverse(1:length(scopes))
            lno = scopes[i]
            inlined = i != 1
            def = lno.method
            def isa Union{Method,Core.CodeInstance,MethodInstance} || (def = nothing)
            sf = StackFrame(normalize_method_name(lno.method), lno.file, lno.line, def, false, inlined, 0)
            push!(frames, sf)
        end
        empty!(scopes)

        new_parent = get(parents, codeinst, nothing)
        if haskey(visited, new_parent)
            break
        end
        push!(visited, new_parent)
        parent = new_parent
    end
    return frames
end

## code for analysis ##

function may_dispatch(@nospecialize ftyp)
    if ftyp <: IntrinsicFunction
        return true
    elseif ftyp <: Builtin
        # other builtins (including the IntrinsicFunctions) are good
        return Core._apply isa ftyp ||
               Core._apply_iterate isa ftyp ||
               Core._call_in_world_total isa ftyp ||
               Core.invoke isa ftyp ||
               Core.invoke_in_world isa ftyp ||
               Core.invokelatest isa ftyp ||
               Core.finalizer isa ftyp ||
               Core.modifyfield! isa ftyp ||
               Core.modifyglobal! isa ftyp ||
               Core.memoryrefmodify! isa ftyp
    else
        return true
    end
end

function verify_codeinstance!(interp::NativeInterpreter, codeinst::CodeInstance, codeinfo::CodeInfo, inspected::IdSet{CodeInstance}, caches::IdDict{MethodInstance,CodeInstance}, parents::ParentMap, errors::ErrorList)
    mi = get_ci_mi(codeinst)
    sptypes = sptypes_from_meth_instance(mi)
    src = codeinfo.code
    for i = 1:length(src)
        stmt = src[i]
        isexpr(stmt, :(=)) && (stmt = stmt.args[2])
        error = ""
        warn = false
        if isexpr(stmt, :invoke) || isexpr(stmt, :invoke_modify)
            error = "unresolved invoke"
            edge = stmt.args[1]
            if edge isa CodeInstance
                haskey(parents, edge) || (parents[edge] = (codeinst, i))
                edge in inspected && continue
                edge_mi = get_ci_mi(edge)
                if edge_mi === edge.def
                    ci = get(caches, edge_mi, nothing)
                    ci isa CodeInstance && continue # assume that only this_world matters for trim
                end
            end
            # TODO: check for calls to Base.atexit?
        elseif isexpr(stmt, :call)
            farg = stmt.args[1]
            ftyp = widenconst(argextype(farg, codeinfo, sptypes))
            if ftyp <: IntrinsicFunction
                #TODO: detect if f !== Core.Intrinsics.atomic_pointermodify (see statement_cost), otherwise error
                continue
            elseif ftyp <: Builtin
                if !may_dispatch(ftyp)
                    continue
                end
                if !isconcretetype(ftyp)
                    error = "unresolved call to (unknown) builtin"
                elseif Core._apply_iterate isa ftyp
                    if length(stmt.args) >= 3
                        # args[1] is _apply_iterate object
                        # args[2] is invoke object
                        farg = stmt.args[3]
                        ftyp = widenconst(argextype(farg, codeinfo, sptypes))
                        if may_dispatch(ftyp)
                            error = "unresolved call to function"
                        else
                            for i in 4:length(stmt.args)
                                atyp = widenconst(argextype(stmt.args[i], codeinfo, sptypes))
                                if !(atyp <: Union{SimpleVector, GenericMemory, Array, Tuple, NamedTuple})
                                    error = "unresolved argument to call"
                                    break
                                end
                            end
                        end
                    end
                elseif Core.finalizer isa ftyp
                    if length(stmt.args) == 3
                        finalizer = argextype(stmt.args[2], codeinfo, sptypes)
                        obj = argextype(stmt.args[3], codeinfo, sptypes)
                        atype = argtypes_to_type(Any[finalizer, obj])

                        mi = compileable_specialization_for_call(interp, atype)
                        if mi !== nothing
                            ci = get(caches, mi, nothing)
                            ci isa CodeInstance && continue
                        end

                        error = "unresolved finalizer registered"
                    end
                elseif Core._apply isa ftyp
                    error = "trim verification not yet implemented for builtin `Core._apply`"
                elseif Core._call_in_world_total isa ftyp
                    error = "trim verification not yet implemented for builtin `Core._call_in_world_total`"
                elseif Core.invoke isa ftyp
                    error = "trim verification not yet implemented for builtin `Core.invoke`"
                elseif Core.invoke_in_world isa ftyp
                    error = "trim verification not yet implemented for builtin `Core.invoke_in_world`"
                elseif Core.invokelatest isa ftyp
                    error = "trim verification not yet implemented for builtin `Core.invokelatest`"
                elseif Core.modifyfield! isa ftyp
                    error = "trim verification not yet implemented for builtin `Core.modifyfield!`"
                elseif Core.modifyglobal! isa ftyp
                    error = "trim verification not yet implemented for builtin `Core.modifyglobal!`"
                elseif Core.memoryrefmodify! isa ftyp
                    error = "trim verification not yet implemented for builtin `Core.memoryrefmodify!`"
                else @assert false "unexpected builtin" end
            else
                error = "unresolved call"
            end
            extyp = argextype(SSAValue(i), codeinfo, sptypes)
            if extyp === Union{}
                warn = true # downgrade must-throw calls to be only a warning
            end
        elseif isexpr(stmt, :cfunction)
            length(stmt.args) != 5 && continue # required by IR legality
            f, at = stmt.args[2], stmt.args[4]

            at isa SimpleVector || continue  # required by IR legality
            ft = argextype(f, codeinfo, sptypes)
            argtypes = Any[ft]
            for i = 1:length(at)
                push!(argtypes, sp_type_rewrap(at[i], get_ci_mi(codeinst), #= isreturn =# false))
            end
            atype = argtypes_to_type(argtypes)

            mi = compileable_specialization_for_call(interp, atype)
            if mi !== nothing
                # n.b.: Codegen may choose unpredictably to emit this `@cfunction` as a dynamic invoke or a full
                # dynamic call, but in either case it guarantees that the required adapter(s) are emitted. All
                # that we are required to verify here is that the callee CodeInstance is covered.
                ci = get(caches, mi, nothing)
                ci isa CodeInstance && continue
            end

            error = "unresolved cfunction"
        elseif isexpr(stmt, :foreigncall)
            foreigncall = stmt.args[1]
            if isexpr(foreigncall, :tuple, 1)
                foreigncall = foreigncall.args[1]
                if foreigncall isa String
                    foreigncall = QuoteNode(Symbol(foreigncall))
                end
                if foreigncall isa QuoteNode
                    if foreigncall.value in runtime_functions
                        error = "disallowed ccall into a runtime function"
                    end
                else
                    error = "disallowed ccall with non-constant name and no library"
                end
            end
        elseif isexpr(stmt, :new_opaque_closure)
            error = "unresolved opaque closure"
            # TODO: check that this opaque closure has a valid signature for possible codegen and code defined for it
            warn = true
        end
        if !isempty(error)
            push!(errors, warn => CallMissing(codeinst, codeinfo, sptypes, i, error))
        end
    end
end

## entry-point ##

function get_verify_typeinf_trim(codeinfos::Vector{Any})
    this_world = get_world_counter()
    interp = NativeInterpreter(this_world)
    inspected = IdSet{CodeInstance}()
    caches = IdDict{MethodInstance,CodeInstance}()
    errors = ErrorList()
    parents = ParentMap()
    for i = 1:length(codeinfos)
        item = codeinfos[i]
        if item isa CodeInstance
            push!(inspected, item)
            if item.owner === nothing && item.min_world <= this_world <= item.max_world
                mi = get_ci_mi(item)
                if mi === item.def
                    caches[mi] = item
                end
            end
        end
    end
    for i = 1:length(codeinfos)
        item = codeinfos[i]
        if item isa CodeInstance
            src = codeinfos[i + 1]::CodeInfo
            verify_codeinstance!(interp, item, src, inspected, caches, parents, errors)
        elseif item isa SimpleVector
            rt = item[1]::Type
            sig = item[2]::Type
            mi = ccall(:jl_get_specialization1, Any,
                        (Any, Csize_t, Cint),
                        sig, this_world, #= mt_cache =# 0)
            asrt = Any
            valid = if mi !== nothing
                mi = mi::MethodInstance
                ci = get(caches, mi, nothing)
                if ci isa CodeInstance
                    # TODO: should we find a way to indicate to the user that this gets called via ccallable?
                    # parent[ci] = something
                    asrt = ci.rettype
                    true
                else
                    false
                end
            else
                false
            end
            if !valid
                warn = false
                push!(errors, warn => CCallableMissing(rt, sig, "unresolved ccallable"))
            elseif !(asrt <: rt)
                warn = hasintersect(asrt, rt)
                push!(errors, warn => CCallableMissing(asrt, sig, "ccallable declared return type does not match inference"))
            end
        end
    end
    return (errors, parents)
end

# It is unclear if this file belongs in Compiler itself, or should instead be a codegen
# driver / verifier implemented by juliac-buildscript.jl for the purpose of extensibility.
# For now, it is part of Base.Compiler, but executed with invokelatest so that packages
# could provide hooks to change, customize, or tweak its behavior and heuristics.
function verify_typeinf_trim(io::IO, codeinfos::Vector{Any}, onlywarn::Bool)
    errors, parents = get_verify_typeinf_trim(codeinfos)

    # count up how many messages we printed, of each severity
    counts = [0, 0] # errors, warnings
    io = IOContext{IO}(io)
    # print all errors afterwards, when the parents map is fully constructed
    for desc in errors
        warn, desc = desc
        severity = warn ? 2 : 1
        no = (counts[severity] += 1)
        printstyled(io, "Error #", no, ": "; color=Base.error_color(), bold=true)     # TODO: should we coalesce any of these stacktraces to minimize spew?
        verify_print_error(io, desc, parents, warn)
    end

    ## TODO: compute and display the minimum and/or full call graph instead of merely the first parent stacktrace?
    #for i = 1:length(codeinfos)
    #    item = codeinfos[i]
    #    if item isa CodeInstance
    #        println(item, "::", item.rettype)
    #    end
    #end

    let severity = 0
        if counts[1] > 0 || counts[2] > 0
            print(io, "Trim verify finished with ")
            if counts[1] > 0
                printstyled(io, counts[1], counts[1] == 1 ? " error" : " errors"; color=Base.error_color(), bold=true)
            else
                print(io, counts[1], counts[1] == 1 ? " error" : " errors")
            end
            print(io, ", ")
            if counts[2] > 0
                printstyled(io, counts[2], counts[2] == 1 ? " warning" : " warnings"; color=Base.warn_color(), bold=true)
            else
                print(io, counts[2], counts[2] == 1 ? " warning" : " warnings")
            end
            print(io, ".\n")
            severity = 2
        end
        if counts[1] > 0
            severity = 1
        end
        # messages classified as errors are fatal, warnings are not
        0 < severity <= 1 && !onlywarn && throw(Core.TrimFailure())
    end
    nothing
end
