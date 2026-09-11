# This file is a part of Julia. License is MIT: https://julialang.org/license

# Git tree hashing of a directory, as git would compute it, without libgit2.
# This code gratefully adapted from https://github.com/simonbyrne/GitX.jl
module GitTreeHashTools

using SHA
import Base: SHA1

export tree_hash, blob_hash

@enum GitMode mode_dir = 0o040000 mode_normal = 0o100644 mode_executable = 0o100755 mode_symlink = 0o120000 mode_submodule = 0o160000
Base.string(mode::GitMode) = string(UInt32(mode); base = 8)
Base.print(io::IO, mode::GitMode) = print(io, string(mode))

function gitmode(path::AbstractString)
    # Windows doesn't deal with executable permissions in quite the same way,
    # `stat()` gives a different answer than we actually want, so we use
    # `isexecutable()` which uses `uv_fs_access()` internally.  On other
    # platforms however, we just want to check via `stat()`.
    function isexec(p)
        @static if Sys.iswindows()
            return Sys.isexecutable(p)
        end
        return !iszero(filemode(p) & 0o100)
    end
    if islink(path)
        return mode_symlink
    elseif isdir(path)
        return mode_dir
    elseif isexec(path)
        return mode_executable
    else
        return mode_normal
    end
end

"""
    blob_hash(HashType::Type, path::AbstractString)

Calculate the git blob hash of a given path.
"""
function blob_hash(::Type{HashType}, path::AbstractString) where {HashType}
    ctx = HashType()
    if islink(path)
        datalen = length(readlink(path))
    else
        datalen = filesize(path)
    end

    # First, the header
    SHA.update!(ctx, Vector{UInt8}("blob $(datalen)\0"))

    # Next, read data in in chunks of 4KB
    buff = Vector{UInt8}(undef, 4 * 1024)

    try
        if islink(path)
            update!(ctx, Vector{UInt8}(readlink(path)))
        else
            open(path, "r") do io
                while !eof(io)
                    num_read = readbytes!(io, buff)
                    update!(ctx, buff, num_read)
                end
            end
        end
    catch e
        if isa(e, InterruptException)
            rethrow(e)
        end
        @warn("Unable to open $(path) for hashing; git-tree-sha1 likely suspect")
    end

    # Finish it off and return the digest!
    return SHA.digest!(ctx)
end
blob_hash(path::AbstractString) = blob_hash(SHA1_CTX, path)

"""
    contains_files(root::AbstractString)

Helper function to determine whether a directory contains files; e.g. it is a
direct parent of a file or it contains some other directory that itself is a
direct parent of a file. This is used to exclude directories from tree hashing.
"""
function contains_files(path::AbstractString)
    st = lstat(path)
    ispath(st) || throw(ArgumentError("non-existent path: $(repr(path))"))
    isdir(st) || return true
    for p in readdir(path)
        contains_files(joinpath(path, p)) && return true
    end
    return false
end

"""
    tree_hash(HashType::Type, root::AbstractString)

Calculate the git tree hash of a given path.
"""
function tree_hash(::Type{HashType}, root::AbstractString; debug_out::Union{IO, Nothing} = nothing, indent::Int = 0) where {HashType}
    entries = Tuple{String, Vector{UInt8}, GitMode}[]
    for f in sort(readdir(root; join = true); by = f -> gitmode(f) == mode_dir ? f * "/" : f)
        # Skip `.git` directories
        if basename(f) == ".git"
            continue
        end

        filepath = abspath(f)
        mode = gitmode(filepath)
        if mode == mode_dir
            # If this directory contains no files, then skip it
            contains_files(filepath) || continue

            # Otherwise, hash it up!
            child_stream = nothing
            if debug_out !== nothing
                child_stream = IOBuffer()
            end
            hash = tree_hash(HashType, filepath; debug_out = child_stream, indent = indent + 1)
            if debug_out !== nothing
                indent_str = "| "^indent
                println(debug_out, "$(indent_str)+ [D] $(basename(filepath)) - $(bytes2hex(hash))")
                print(debug_out, String(take!(child_stream::IOBuffer)))
                println(debug_out, indent_str)
            end
        else
            hash = blob_hash(HashType, filepath)
            if debug_out !== nothing
                indent_str = "| "^indent
                mode_str = mode == mode_normal ? "F" : "X"
                println(debug_out, "$(indent_str)[$(mode_str)] $(basename(filepath)) - $(bytes2hex(hash))")
            end
        end
        push!(entries, (basename(filepath), hash, mode))
    end

    content_size = 0
    for (n, h, m) in entries
        content_size += ndigits(UInt32(m); base = 8) + 1 + sizeof(n) + 1 + sizeof(h)
    end

    # Return the hash of these entries
    ctx = HashType()
    SHA.update!(ctx, Vector{UInt8}("tree $(content_size)\0"))
    for (name, hash, mode) in entries
        SHA.update!(ctx, Vector{UInt8}("$(mode) $(name)\0"))
        SHA.update!(ctx, hash)
    end
    return SHA.digest!(ctx)
end
tree_hash(root::AbstractString; debug_out::Union{IO, Nothing} = nothing) = tree_hash(SHA.SHA1_CTX, root; debug_out)

end # module GitTreeHashTools
