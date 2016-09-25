# This file is a part of Julia. License is MIT: http://julialang.org/license

function GitBlob(path::AbstractString)
    blob_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    apath = abspath(path)
    ccall((:git_blob_create_fromdisk, :libgit2), Ptr{Void},
                  (Ptr{Ptr{Void}}, Ptr{UInt8}), blob_ptr_ptr, apath)
    blob_ptr_ptr == C_NULL && throw(Error.GitError(Error.ERROR))
    return GitBlob(blob_ptr_ptr[])
end

function owner(blob::GitBlob)
    repo_ptr = @check ccall((:git_blob_owner, :libgit2), Ptr{Void}, (Ptr{Void},), blob.ptr)
    return GitRepo(repo_ptr)
end

function content(blob::GitBlob)
    return ccall((:git_blob_rawcontent, :libgit2), Ptr{Void}, (Ptr{Void},), blob.ptr)
end

function Base.length(blob::GitBlob)
    return ccall((:git_blob_rawsize, :libgit2), Int64, (Ptr{Void},), blob.ptr)
end

function lookup(repo::GitRepo, oid::Oid)
    blob_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_blob_lookup, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ref{Oid}),
                   blob_ptr_ptr, repo.ptr, Ref(oid))
    return GitBlob(blob_ptr_ptr[])
end
