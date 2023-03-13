
function escape_builtin!(::typeof(bufferlen), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) == 2 || return false
    ir = astate.ir
    buf = args[2]
    atype = argextype(buf, ir)
    if !(atype ⊑ Buffer)
        add_escape_change!(astate, buf, ThrownEscape(pc))
    end
    return true
end

function escape_builtin!(::typeof(bufref), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) ≥ 4 || return false
    # check potential thrown escapes from this bufref call
    boundscheckty = argextype(args[2], astate.ir)
    bufty = argextype(args[3], astate.ir)
    idxty = argextype(args[4], astate.ir)
    if !(boundscheckty ⊑ Bool &&
         (bufty ⊑ Buffer) && idxty ⊑ Int)
        add_thrown_escapes!(astate, pc, args, 2)
    end
    buf = args[3]
    inbounds = isa(boundcheckt, Const) && !boundcheckt.val::Bool
    inbounds || add_escape_change!(astate, buf, ThrownEscape(pc))
    # we don't track precise index information about this array and thus don't know what values
    # can be referenced here: directly propagate the escape information imposed on the return
    # value of this `bufref` call to the buffer itself as the most conservative propagation
    # but also with updated index information
    estate = astate.estate
    if isa(buf, SSAValue) || isa(buf, Argument)
        bufinfo = estate[buf]
    else
        # unanalyzable object, so the return value is also unanalyzable
        add_escape_change!(astate, SSAValue(pc), ⊤)
        return true
    end
    AliasInfo = bufinfo.AliasInfo
    if isa(AliasInfo, Bool)
        AliasInfo && @goto conservative_propagation
        # AliasInfo of this array hasn't been analyzed yet: set AliasInfo now
        idx = buffer_index(astate, buf, args[4])
        if isa(idx, Int)
            AliasInfo = IndexableElements(IdDict{Int,AInfo}())
            @goto record_indexable_use
        end
        AliasInfo = Unindexable()
        @goto record_unindexable_use
    elseif isa(AliasInfo, IndexableElements)
        idx = buffer_index(astate, buf, args[4])
        if !isa(idx, Int)
            AliasInfo = merge_to_unindexable(AliasInfo)
            @goto record_unindexable_use
        end
        @label record_indexable_use
        info = get!(()->AInfo(), AliasInfo.infos, idx)
        push!(info, LocalUse(pc))
        add_escape_change!(astate, buf, EscapeInfo(bufinfo, AliasInfo)) # update with new AliasInfo
    elseif isa(AliasInfo, Unindexable)
        @label record_unindexable_use
        push!(AliasInfo.info, LocalUse(pc))
        add_escape_change!(astate, buf, EscapeInfo(bufinfo, AliasInfo)) # update with new AliasInfo
    else
        # this object has been used as struct, but it is used as array here (thus should throw)
        # update buf's element information and just handle this case conservatively
        bufinfo = escape_unanalyzable_obj!(astate, buf, bufinfo)
        @label conservative_propagation
        # at the extreme case, an element of `buf` may point to `buf` itself
        # so add the alias change here as the most conservative propagation
        add_alias_change!(astate, buf, SSAValue(pc))
    end
    return true
end

function escape_builtin!(::typeof(bufset), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) ≥ 5 || return false
    # check potential escapes from this arrayset call
    # NOTE here we essentially only need to account for TypeError, assuming that
    # UndefRefError or BoundsError don't capture any of the arguments here
    boundscheckty = argextype(args[2], astate.ir)
    bufty = argextype(args[3], astate.ir)
    valty = argextype(args[4], astate.ir)
    idxty = argextype(args[5], astate.ir)
    if !(boundscheckty ⊑ Bool &&
         (bufty ⊑ Buffer) &&
         idxty ⊑ Int && arrayset_typecheck(bufty, valty))
        add_thrown_escapes!(astate, pc, args, 2)
    end
    buf = args[3]
    val = args[4]
    inbounds = isa(boundcheckt, Const) && !boundcheckt.val::Bool
    inbounds || add_escape_change!(astate, buf, ThrownEscape(pc))
    # we don't track precise index information about this array and won't record what value
    # is being assigned here: directly propagate the escape information of this array to
    # the value being assigned as the most conservative propagation
    estate = astate.estate
    if isa(buf, SSAValue) || isa(buf, Argument)
        bufinfo = estate[buf]
    else
        # unanalyzable object (e.g. obj::GlobalRef): escape field value conservatively
        add_escape_change!(astate, val, ⊤)
        return true
    end
    AliasInfo = bufinfo.AliasInfo
    if isa(AliasInfo, Bool)
        AliasInfo && @goto conservative_propagation
        # AliasInfo of this array hasn't been analyzed yet: set AliasInfo now
        idx = buffer_index(astate, buf, args[5])
        if isa(idx, Int)
            AliasInfo = IndexableElements(IdDict{Int,AInfo}())
            @goto escape_indexable_def
        end
        AliasInfo = Unindexable()
        @goto escape_unindexable_def
    elseif isa(AliasInfo, IndexableElements)
        idx = buffer_index(astate, buf, args[5])
        if !isa(idx, Int)
            AliasInfo = merge_to_unindexable(AliasInfo)
            @goto escape_unindexable_def
        end
        @label escape_indexable_def
        info = get!(()->AInfo(), AliasInfo.infos, idx)
        add_alias_escapes!(astate, val, info)
        push!(info, LocalDef(pc))
        add_escape_change!(astate, buf, EscapeInfo(bufinfo, AliasInfo)) # update with new AliasInfo
        # propagate the escape information of this array ignoring elements information
        add_escape_change!(astate, val, ignore_aliasinfo(bufinfo))
    elseif isa(AliasInfo, Unindexable)
        @label escape_unindexable_def
        add_alias_escapes!(astate, val, AliasInfo.info)
        push!(AliasInfo.info, LocalDef(pc))
        add_escape_change!(astate, buf, EscapeInfo(bufinfo, AliasInfo)) # update with new AliasInfo
        # propagate the escape information of this array ignoring elements information
        add_escape_change!(astate, val, ignore_aliasinfo(bufinfo))
    else
        # this object has been used as struct, but it is used as array here (thus should throw)
        # update buf's element information and just handle this case conservatively
        bufinfo = escape_unanalyzable_obj!(astate, buf, bufinfo)
        @label conservative_propagation
        add_alias_change!(astate, val, buf)
    end
    # also propagate escape information imposed on the return value of this `arrayset`
    ssainfo = estate[SSAValue(pc)]
    add_escape_change!(astate, buf, ssainfo)
    return true
end

function buffer_index(astate::AnalysisState, @nospecialize(buf), @nospecialize(arg))
    isa(buf, SSAValue) || return nothing
    bufid = buf.id
    bufinfo = astate.estate.arrayinfo
    isa(bufinfo, ArrayInfo) || return nothing
    haskey(bufinfo, bufid) || return nothing
    argval = argextype(arg, astate.ir)
    isa(argval, Const) || return nothing
    argval = argval.val
    isa(argval, Int) || return nothing
    (1 <= argval <= bufinfo[bufid][1]) || return nothing # BoundsError
    return argval
end


# TODO Buffer: escape analysis for jl_buffer_copy
# is_buffer_copy(name::Symbol) = name === :jl_buffer_copy
# # FIXME this implementation is very conservative, improve the accuracy and solve broken test cases
# function escape_buffer_copy!(astate::AnalysisState, pc::Int, args::Vector{Any})
#     length(args) ≥ 6 || return add_fallback_changes!(astate, pc, args)
#     ary = args[6]
#     aryt = argextype(ary, astate.ir)
#     aryt ⊑ Array || return add_fallback_changes!(astate, pc, args)
#     if isa(ary, SSAValue) || isa(ary, Argument)
#         newary = SSAValue(pc)
#         aryinfo = astate.estate[ary]
#         newaryinfo = astate.estate[newary]
#         add_escape_change!(astate, newary, aryinfo)
#         add_escape_change!(astate, ary, newaryinfo)
#     end
#     add_liveness_changes!(astate, pc, args, 6)
# end

is_buffer_isassigned(name::Symbol) = name === :jl_buffer_isassigned

function escape_buffer_isassigned!(astate::AnalysisState, pc::Int, args::Vector{Any})
    if !array_isassigned_nothrow(args, astate.ir)
        add_thrown_escapes!(astate, pc, args)
    end
    add_liveness_changes!(astate, pc, args, 6)
end

function buffer_isassigned_nothrow(args::Vector{Any}, src::IRCode)
    # if !validate_foreigncall_args(args,
    #     :jl_array_isassigned, Cint, svec(Any,Csize_t), 0, :ccall)
    #     return false
    # end
    length(args) ≥ 7 || return false
    arytype = argextype(args[6], src)
    arytype ⊑ Buffer || return false
    idxtype = argextype(args[7], src)
    idxtype ⊑ Csize_t || return false
    return true
end
