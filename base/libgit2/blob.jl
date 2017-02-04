# This file is a part of Julia. License is MIT: http://julialang.org/license

function Base.length(blob::GitBlob)
    return ccall((:git_blob_rawsize, :libgit2), Int64, (Ptr{Void},), blob.ptr)
end

function rawcontent(blob::GitBlob)
    ptr = ccall((:git_blob_rawcontent, :libgit2), Ptr{UInt8}, (Ptr{Void},), blob.ptr)
    copy(unsafe_wrap(Array, ptr, (length(blob),), false))
end

function content(blob::GitBlob)
    s = String(rawcontent(blob))
    isvalid(s) || error("Blob does not contain valid UTF-8 data")
    return s
end

"""
Use a heuristic to guess if a file is binary: searching for NULL bytes and
looking for a reasonable ratio of printable to non-printable characters among
the first 8000 bytes.
"""
function isbinary(blob::GitBlob)
    bin_flag = ccall((:git_blob_is_binary, :libgit2), Cint, (Ptr{Void},), blob.ptr)
    return bin_flag == 1
end

function lookup(repo::GitRepo, oid::GitHash)
    blob_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_blob_lookup, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ref{GitHash}),
                   blob_ptr_ptr, repo.ptr, Ref(oid))
    return GitBlob(blob_ptr_ptr[])
end

"""
    LibGit2.addblob!(repo::GitRepo, path::AbstractString)

Reads the file at `path` and adds it to the object database of `repo` as a loose blob.
Returns the `GitHash` of the resulting blob.
"""
function addblob!(repo::GitRepo, path::AbstractString)
    id_ref = Ref{GitHash}()
    @check ccall((:git_blob_create_fromdisk, :libgit2), Cint,
                 (Ptr{GitHash}, Ptr{Void}, Cstring),
                 id_ref, repo.ptr, path)
    return id_ref[]
end

function Base.show(io::IO, blob::GitBlob)
    if !isbinary(blob)
        conts   = split(content(blob), "\n")
        showlen = max(length(conts), 3)
        println(io, "GitBlob:\nBlob id: ", GitHash(blob), "\nContents:")
        for i in 1:showlen
            println(io, conts[i])
        end
    else
        println(io, "GitBlob:\nBlob id: ", GitHash(blob), "\nContents are binary.")
    end
end
