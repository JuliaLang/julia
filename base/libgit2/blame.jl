# This file is a part of Julia. License is MIT: https://julialang.org/license

function GitBlame(repo::GitRepo, path::AbstractString; options::BlameOptions=BlameOptions())
    blame_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_blame_file, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Ptr{BlameOptions}),
                   blame_ptr_ptr, repo.ptr, path, Ref(options))
    return GitBlame(repo, blame_ptr_ptr[])
end

function counthunks(blame::GitBlame)
    return ccall((:git_blame_get_hunk_count, :libgit2), Int32, (Ptr{Void},), blame.ptr)
end

function Base.getindex(blame::GitBlame, i::Integer)
    if !(1 <= i <= counthunks(blame))
        throw(BoundsError(blame, (i,)))
    end
    hunk_ptr = ccall((:git_blame_get_hunk_byindex, :libgit2),
                      Ptr{BlameHunk},
                      (Ptr{Void}, Csize_t), blame.ptr, i-1)
    return unsafe_load(hunk_ptr)
end

function Base.show(io::IO, blame_hunk::BlameHunk)
    println(io, "GitBlameHunk:")
    println(io, "Original path: ", unsafe_string(blame_hunk.orig_path))
    println(io, "Lines in hunk: ", blame_hunk.lines_in_hunk)
    println(io, "Final commit oid: ", blame_hunk.final_commit_id)
    print(io, "Final signature: ")
    show(io, Signature(blame_hunk.final_signature))
    println(io)
    println(io, "Original commit oid: ", blame_hunk.orig_commit_id)
    print(io, "Original signature: ")
    show(io, Signature(blame_hunk.orig_signature))
end
