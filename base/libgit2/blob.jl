# This file is a part of Julia. License is MIT: http://julialang.org/license

function content(blob::GitBlob)
    return ccall((:git_blob_rawcontent, :libgit2), Ptr{Void}, (Ptr{Void},), blob.ptr)
end

function Base.length(blob::GitBlob)
    return ccall((:git_blob_rawsize, :libgit2), Int64, (Ptr{Void},), blob.ptr)
end

"""
Use a heuristic to guess if a file is binary: searching for NULL bytes and
looking for a reasonable ratio of printable to non-printable characters among
the first 8000 bytes.
"""
function isbinary(blob::GitBlob)
    bin_flag = ccall((:git_blob_is_binary, :libgit2), Int64, (Ptr{Void},), blob.ptr)
    return bin_flag == 1
end

function lookup(repo::GitRepo, oid::GitHash)
    blob_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_blob_lookup, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ref{GitHash}),
                   blob_ptr_ptr, repo.ptr, Ref(oid))
    return GitBlob(blob_ptr_ptr[])
end

function GitBlob(repo::GitRepo, path::AbstractString)
    blob_id_ptr = Ref(GitHash())
    @check ccall((:git_blob_create_fromdisk, :libgit2), Cint,
                 (Ptr{GitHash}, Ptr{Void}, Ptr{UInt8}), blob_id_ptr,
                 repo.ptr, path)
    blob = get(GitBlob, repo, blob_id_ptr[])
    return blob
end

function Base.show(io::IO, blob::GitBlob)
    if !isbinary(blob)
        conts   = split(content(blob), "\n")
        showlen = max(len(conts), 3)
        print(io, "GitBlob:\nBlob id: ", GitHash(blob.ptr), "\nContents:\n")
        for i in 1:showlen
            print(io, conts[i],"\n")
        end
    else
        print(io, "GitBlob:\nBlob id: ", GitHash(blob.ptr), "\nContents are binary.")
    end
end
