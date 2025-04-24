# This file is a part of Julia. License is MIT: https://julialang.org/license

import ..Compiler: verify_typeinf_trim

using ..Compiler:
     # operators
     !, !=, !==, +, :, <, <=, ==, =>, >, >=, ∈, ∉,
     # types
     Array, Builtin, Callable, Cint, CodeInfo, CodeInstance, Csize_t, Exception,
     GenericMemory, GlobalRef, IdDict, IdSet, IntrinsicFunction, Method, MethodInstance,
     NamedTuple, Pair, PhiCNode, PhiNode, PiNode, QuoteNode, SSAValue, SimpleVector, String,
     Tuple, VarState, Vector,
     # functions
     argextype, empty!, error, get, get_ci_mi, get_world_counter, getindex, getproperty,
     hasintersect, haskey, in, isdispatchelem, isempty, isexpr, iterate, length, map!, max,
     pop!, popfirst!, push!, pushfirst!, reinterpret, reverse!, reverse, setindex!,
     setproperty!, similar, singleton_type, sptypes_from_meth_instance,
     unsafe_pointer_to_objref, widenconst,
     # misc
     @nospecialize, C_NULL
using ..IRShow: LineInfoNode, print, show, println, append_scopes!, IOContext, IO, normalize_method_name
using ..Base: Base, sourceinfo_slotnames
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

## code for pretty printing ##

# wrap a statement in a typeassert for printing clarity, unless that info seems already obvious
function mapssavaluetypes(codeinfo::CodeInfo, sptypes::Vector{VarState}, stmt)
    @nospecialize stmt
    newstmt = mapssavalues(codeinfo, sptypes, stmt)
    typ = widenconst(argextype(stmt, codeinfo, sptypes))
    if newstmt isa Expr
        if newstmt.head ∈ (:quote, :inert)
            return newstmt
        end
    elseif newstmt isa GlobalRef && isdispatchelem(typ)
        return newstmt
    elseif newstmt isa Union{Int, UInt8, UInt16, UInt32, UInt64, Float16, Float32, Float64, String, QuoteNode}
        return newstmt
    elseif newstmt isa Callable
        return newstmt
    end
    return Expr(:(::), newstmt, typ)
end

# map the ssavalues in a (value-producing) statement to the expression they came from, summarizing some things to avoid excess printing
function mapssavalues(codeinfo::CodeInfo, sptypes::Vector{VarState}, stmt)
    @nospecialize stmt
    if stmt isa SSAValue
        return mapssavalues(codeinfo, sptypes, codeinfo.code[stmt.id])
    elseif stmt isa PiNode
        return mapssavalues(codeinfo, sptypes, stmt.val)
    elseif stmt isa Expr
        stmt.head ∈ (:quote, :inert) && return stmt
        newstmt = Expr(stmt.head)
        if stmt.head === :foreigncall
            return Expr(:call, :ccall, mapssavalues(codeinfo, sptypes, stmt.args[1]))
        elseif stmt.head ∉ (:new, :method, :toplevel, :thunk)
            newstmt.args = map!(similar(stmt.args), stmt.args) do arg
                @nospecialize arg
                return mapssavaluetypes(codeinfo, sptypes, arg)
            end
            if newstmt.head === :invoke
                # why is the fancy printing for this not in show_unquoted?
                popfirst!(newstmt.args)
                newstmt.head = :call
            end
        end
        return newstmt
    elseif stmt isa PhiNode
        return PhiNode()
    elseif stmt isa PhiCNode
        return PhiNode()
    end
    return stmt
end

function verify_print_stmt(io::IOContext{IO}, codeinfo::CodeInfo, sptypes::Vector{VarState}, stmtidx::Int)
    if codeinfo.slotnames !== nothing
        io = IOContext(io, :SOURCE_SLOTNAMES => sourceinfo_slotnames(codeinfo))
    end
    print(io, mapssavaluetypes(codeinfo, sptypes, SSAValue(stmtidx)))
end

function verify_print_error(io::IOContext{IO}, desc::CallMissing, parents::ParentMap)
    (; codeinst, codeinfo, sptypes, stmtidx, desc) = desc
    frames = verify_create_stackframes(codeinst, stmtidx, parents)
    print(io, desc, " from statement ")
    verify_print_stmt(io, codeinfo, sptypes, stmtidx)
    Base.show_backtrace(io, frames)
    print(io, "\n\n")
    nothing
end

function verify_print_error(io::IOContext{IO}, desc::CCallableMissing, parents::ParentMap)
    print(io, desc.desc, " for ", desc.sig, " => ", desc.rt, "\n\n")
    nothing
end

function verify_create_stackframes(codeinst::CodeInstance, stmtidx::Int, parents::ParentMap)
    scopes = LineInfoNode[]
    frames = StackFrame[]
    parent = (codeinst, stmtidx)
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
        parent = get(parents, codeinst, nothing)
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

function verify_codeinstance!(codeinst::CodeInstance, codeinfo::CodeInfo, inspected::IdSet{CodeInstance}, caches::IdDict{MethodInstance,CodeInstance}, parents::ParentMap, errors::ErrorList)
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
            error = "unresolved call"
            farg = stmt.args[1]
            ftyp = widenconst(argextype(farg, codeinfo, sptypes))
            if ftyp <: IntrinsicFunction
                #TODO: detect if f !== Core.Intrinsics.atomic_pointermodify (see statement_cost), otherwise error
                continue
            elseif ftyp <: Builtin
                if !may_dispatch(ftyp)
                    continue
                end
                if Core._apply_iterate isa ftyp
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
                        # TODO: check that calling `args[1](args[2])` is defined before warning
                        error = "unresolved finalizer registered"
                        warn = true
                    end
                else
                    error = "unresolved call to builtin"
                end
            end
            extyp = argextype(SSAValue(i), codeinfo, sptypes)
            if extyp === Union{}
                warn = true # downgrade must-throw calls to be only a warning
            end
        elseif isexpr(stmt, :cfunction)
            error = "unresolved cfunction"
            #TODO: parse the cfunction expression to check the target is defined
            warn = true
        elseif isexpr(stmt, :foreigncall)
            foreigncall = stmt.args[1]
            if foreigncall isa QuoteNode
                if foreigncall.value in runtime_functions
                    error = "disallowed ccall into a runtime function"
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
            verify_codeinstance!(item, src, inspected, caches, parents, errors)
        elseif item isa SimpleVector
            rt = item[1]::Type
            sig = item[2]::Type
            ptr = ccall(:jl_get_specialization1,
                        #= MethodInstance =# Ptr{Cvoid}, (Any, Csize_t, Cint),
                        sig, this_world, #= mt_cache =# 0)
            asrt = Any
            valid = if ptr !== C_NULL
                mi = unsafe_pointer_to_objref(ptr)::MethodInstance
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
        print(io, warn ? "Verifier warning #" : "Verifier error #", no, ": ")
        # TODO: should we coalesce any of these stacktraces to minimize spew?
        verify_print_error(io, desc, parents)
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
            print("Trim verify finished with ")
            print(counts[1], counts[1] == 1 ? " error" : " errors")
            print(", ")
            print(counts[2], counts[2] == 1 ? " warning" : " warnings")
            print(".\n")
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
