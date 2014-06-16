export git_otype, message, target_id, target, tagger, lookup_tag

function name(t::GitTag)
    @assert t.ptr != C_NULL
    return bytestring(api.git_tag_name(t.ptr))
end

function message(t::GitTag)
    @assert t.ptr != C_NULL
    msg_ptr = api.git_tag_message(t.ptr)
    if msg_ptr == C_NULL
        return nothing
    end
    return bytestring(msg_ptr)
end

function target_id(t::GitTag)
    @assert t.ptr != C_NULL
    oid_ptr = api.git_tag_target_id(t.ptr)
    return Oid(oid_ptr)
end

function target(t::GitTag)
    @assert t.ptr != C_NULL
    target_ptr = Array(Ptr{Void}, 1)
    @check api.git_tag_target(target_ptr, t.ptr)
    return gitobj_from_ptr(target_ptr[1])
end

function tagger(t::GitTag)
    @assert t.ptr != C_NULL
    sig_ptr = api.git_tag_tagger(t.ptr)
    if sig_ptr == C_NULL
        return nothing
    end
    gsig = unsafe_load(sig_ptr)
    sig =  Signature(gsig)
    #TODO: all this signature stuff has
    #to be cleaned up
    #api.free!(gsig)
    return sig
end

#TODO: foreach(tag)...
