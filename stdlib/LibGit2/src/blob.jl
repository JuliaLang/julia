# This file is a part of Julia. License is MIT: https://julialang.org/license

function Base.length(blob::GitBlob)
    ensure_initialized()
    return ccall((:git_blob_rawsize, :libgit2), Int64, (Ptr{Cvoid},), blob.ptr)
end

"""
    rawcontent(blob::GitBlob) -> Vector{UInt8}

Fetch the *raw* contents of the [`GitBlob`](@ref) `blob`. This is an
`Array` containing the contents of the blob, which may be binary or may be Unicode.
If you write to this `Array` the blob on disk will not be updated.
`rawcontent` will allow the user to load the raw binary data into
the output `Array` and will not check to ensure it is valid Unicode, so errors
may occur if the result is passed to functions which expect valid Unicode data.

See also [`content`](@ref), which *will* throw an error if the content of the `blob`
is binary and not valid Unicode.
"""
function rawcontent(blob::GitBlob)
    ensure_initialized()
    ptr = ccall((:git_blob_rawcontent, :libgit2), Ptr{UInt8}, (Ptr{Cvoid},), blob.ptr)
    copy(unsafe_wrap(Array, ptr, (length(blob),), own = false))
end

"""
    content(blob::GitBlob) -> String

Fetch the contents of the [`GitBlob`](@ref) `blob`. If the `blob` contains
binary data (which can be determined using [`isbinary`](@ref)),
throw an error. Otherwise, return a `String` containing the contents
of the `blob`.
"""
function content(blob::GitBlob)
    s = String(rawcontent(blob))
    isvalid(s) || error("Blob does not contain valid UTF-8 data")
    return s
end

"""
    isbinary(blob::GitBlob) -> Bool

Use a heuristic to guess if a file is binary: searching for NULL bytes and
looking for a reasonable ratio of printable to non-printable characters among
the first 8000 bytes.
"""
function isbinary(blob::GitBlob)
    ensure_initialized()
    bin_flag = ccall((:git_blob_is_binary, :libgit2), Cint, (Ptr{Cvoid},), blob.ptr)
    return bin_flag == 1
end

"""
    LibGit2.addblob!(repo::GitRepo, path::AbstractString)

Read the file at `path` and adds it to the object database of `repo` as a loose blob.
Return the [`GitHash`](@ref) of the resulting blob.

# Examples
```julia
hash_str = string(commit_oid)
blob_file = joinpath(repo_path, ".git", "objects", hash_str[1:2], hash_str[3:end])
id = LibGit2.addblob!(repo, blob_file)
```
"""
function addblob!(repo::GitRepo, path::AbstractString)
    ensure_initialized()
    id_ref = Ref{GitHash}()
    @check ccall((:git_blob_create_from_disk, :libgit2), Cint,
                 (Ptr{GitHash}, Ptr{Cvoid}, Cstring),
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
