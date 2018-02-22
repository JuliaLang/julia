# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    GitBlame(repo::GitRepo, path::AbstractString; options::BlameOptions=BlameOptions())

Construct a `GitBlame` object for the file at `path`, using change information gleaned
from the history of `repo`. The `GitBlame` object records who changed which chunks of
the file when, and how. `options` controls how to separate the contents of the file and
which commits to probe - see [`BlameOptions`](@ref) for more information.
"""
function GitBlame(repo::GitRepo, path::AbstractString; options::BlameOptions=BlameOptions())
    blame_ptr_ptr = Ref{Ptr{Cvoid}}(C_NULL)
    @check ccall((:git_blame_file, :libgit2), Cint,
                  (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}, Cstring, Ptr{BlameOptions}),
                   blame_ptr_ptr, repo.ptr, path, Ref(options))
    return GitBlame(repo, blame_ptr_ptr[])
end

"""
    counthunks(blame::GitBlame)

Return the number of distinct "hunks" with a file. A hunk may contain multiple lines.
A hunk is usually a piece of a file that was added/changed/removed together, for example,
a function added to a source file or an inner loop that was optimized out of
that function later.
"""
function counthunks(blame::GitBlame)
    return ccall((:git_blame_get_hunk_count, :libgit2), Int32, (Ptr{Cvoid},), blame.ptr)
end

function Base.getindex(blame::GitBlame, i::Integer)
    if !(1 <= i <= counthunks(blame))
        throw(BoundsError(blame, (i,)))
    end
    GC.@preserve blame begin
        hunk_ptr = ccall((:git_blame_get_hunk_byindex, :libgit2),
                          Ptr{BlameHunk},
                          (Ptr{Cvoid}, Csize_t), blame.ptr, i-1)
        elem = unsafe_load(hunk_ptr)
    end
    return elem
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
