# This file is a part of Julia. License is MIT: http://julialang.org/license

function content(blob::GitBlob)
    return ccall((:git_blob_rawcontent, :libgit2), Ptr{Void}, (Ptr{Void},), blob.ptr)
end

function isbinary(blob::GitBlob)
    return ccall((:git_blob_is_binary, :libgit2), CInt, (Ptr{Void},), blob.ptr) == 1
end

function Base.length(blob::GitBlob)
    return ccall((:git_blob_rawsize, :libgit2), Coff_t, (Ptr{Void},), blob.ptr)
end

function lookup(repo::GitRepo, oid::Oid)
    blob_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_blob_lookup, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ref{Oid}),
                   blob_ptr_ptr, repo.ptr, Ref(oid))
    return GitBlob(blob_ptr_ptr[])
end
