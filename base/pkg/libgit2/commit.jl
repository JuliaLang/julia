function message(c::GitCommit, raw::Bool=false)
    local msg_ptr::Ptr{UInt8}
    msg_ptr = raw? ccall((:git_commit_message_raw, :libgit2), Ptr{UInt8}, (Ptr{Void},), c.ptr) :
                   ccall((:git_commit_message, :libgit2), Ptr{UInt8}, (Ptr{Void},), c.ptr)
    if msg_ptr == C_NULL
        return nothing
    end
    return bytestring(msg_ptr)
end

function author(c::GitCommit)
    ptr = ccall((:git_commit_author, :libgit2), Ptr{SignatureStruct}, (Ptr{Void},), c.ptr)
    @assert ptr != C_NULL
    return Signature(ptr)
end

function committer(c::GitCommit)
    ptr = ccall((:git_commit_committer, :libgit2), Ptr{SignatureStruct}, (Ptr{Void},), c.ptr)
    @assert ptr != C_NULL
    return Signature(ptr)
end