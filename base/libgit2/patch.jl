export GitPatch, lines, nlines, DiffHunk, hunks, nchanges, delta

type GitPatch
    ptr::Ptr{Void}
    
    function GitPatch(ptr::Ptr{Void})
        @assert ptr != C_NULL
        p = new(ptr)
        finalizer(p, free!)
        return p
    end
end

function free!(p::GitPatch)
    if p.ptr != C_NULL
        api.git_patch_free(p.ptr)
        p.ptr = C_NULL
    end
end

Base.diff(repo::Repository, blob::GitBlob, other::Nothing, opts=nothing) = begin
    old_path_ptr::Ptr{Cchar} = C_NULL
    new_path_ptr::Ptr{Cchar} = C_NULL
    if opts != nothing
        if get(opts, :old_path, nothing) != nothing
            old_path_ptr = convert(Ptr{Cchar}, opts[:old_path]::ByteString)
        end
        if get(opts, :new_path, nothing) != nothing
            new_path_ptr = convert(Ptr{Cchar}, opts[:new_path]::ByteString)
        end
    end
    gopts = parse_git_diff_options(opts)
    patch_ptr = Array(Ptr{Void}, 1)
    @check ccall((:git_patch_from_blobs, api.libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Void}, Ptr{Cchar}, Ptr{api.GitDiffOptions}),
                 patch_ptr, blob.ptr, old_path_ptr, C_NULL, new_path_ptr, &gopts)
    return GitPatch(patch_ptr[1])
end

Base.diff(repo::Repository, blob::GitBlob, other::GitBlob, opts=nothing) = begin
    old_path_ptr::Ptr{Cchar} = C_NULL
    new_path_ptr::Ptr{Cchar} = C_NULL
    if opts != nothing
        if get(opts, :old_path, nothing) != nothing
            old_path_ptr = convert(Ptr{Cchar}, pointer(opts[:old_path]::ByteString))
        end
        if get(opts, :new_path, nothing) != nothing
            new_path_ptr = convert(Ptr{Cchar}, pointer(opts[:new_path]::ByteString))
        end
    end
    gopts = parse_git_diff_options(opts)
    patch_ptr = Array(Ptr{Void}, 1)
    @check ccall((:git_patch_from_blobs, api.libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Void}, Ptr{Cchar}, Ptr{api.GitDiffOptions}),
                 patch_ptr, blob.ptr, old_path_ptr, other.ptr, new_path_ptr, &gopts)
    return GitPatch(patch_ptr[1])
end

Base.diff(repo::Repository, blob::GitBlob, other::String, opts= nothing) = begin
    old_path_ptr::Ptr{Cchar} = C_NULL
    new_path_ptr::Ptr{Cchar} = C_NULL
    if opts != nothing
        if get(opts, :old_path, nothing) != nothing
            old_path_ptr = convert(Ptr{Cchar}, opts[:old_path]::ByteString)
        end
        if get(opts, :new_path, nothing) != nothing
            new_path_ptr = convert(Ptr{Cchar}, opts[:new_path]::ByteString)
        end
    end
    gopts = parse_git_diff_options(opts)
    patch_ptr = Array(Ptr{Void}, 1)
    @check ccall((:git_patch_from_blob_and_buffer, api.libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar},
                  Ptr{Cchar}, Csize_t, Ptr{Cchar}, Ptr{api.GitDiffOptions}),
                 patch_ptr, blob.ptr, old_path_ptr, 
                 buffer, length(buffer), new_path_ptr, &gopts)
    return GitPatch(patch_ptr[1])
end

type PatchStat
    adds::Int
    dels::Int
end

Base.stat(p::GitPatch) = begin
    @assert p.ptr != C_NULL
    adds = Csize_t[0]
    dels = Csize_t[0]
    api.git_patch_line_stats(C_NULL, adds, dels, p.ptr)
    return PatchStat(int(adds[1]), int(dels[1]))
end

function nchanges(p::GitPatch)
    s = stat(p)
    return s.adds + s.dels
end

type DiffHunk
    patch::GitPatch
    header::String 
    line_count::Int
    hunk_index::Int
    old_start::Int
    old_lines::Int
    new_start::Int 
    new_lines::Int

    function DiffHunk(p::GitPatch, 
                      ptr::Ptr{api.GitDiffHunk}, 
                      idx::Integer,
                      lc::Integer)
        @assert ptr != C_NULL
        h = unsafe_load(ptr)
        head_arr = zeros(Cchar, 128)
        @get_header_fieldnames(head_arr, h)
        return new(p,
                   bytestring(convert(Ptr{Cchar}, head_arr)),
                   lc,
                   idx,
                   h.old_start,
                   h.old_lines,
                   h.new_start,
                   h.new_lines)
    end
end 

function hunks(p::GitPatch)
    @assert p.ptr != C_NULL
    nhunks = api.git_patch_num_hunks(p.ptr)
    if nhunks == 0
        return nothing
    end
    err::Cint = 0
    hunk_ptr = Array(Ptr{api.GitDiffHunk}, 1)
    lines_ptr = Csize_t[0]
    hs = DiffHunk[]
    for i in 1:nhunks
        err = api.git_patch_get_hunk(hunk_ptr, lines_ptr, p.ptr, i-1)
        if bool(err)
            break
        end
        push!(hs, DiffHunk(p, hunk_ptr[1], i, lines_ptr[1]))
    end
    if err != api.GIT_OK
        throw(GitError(err))
    end
    return hs
end

function line_origin_to_symbol(o::Cchar)
    if o == api.DIFF_LINE_CONTEXT
        return :context
    end
    if o == api.DIFF_LINE_ADDITION
        return :addition
    end
    if o == api.DIFF_LINE_DELETION
        return :deletion
    end
    if o == api.DIFF_LINE_CONTEXT_EOFNL
        return :eof_no_newline
    end
    if o == api.DIFF_LINE_ADD_EOFNL
        return :eof_newline_added
    end
    if o == api.DIFF_LINE_DEL_EOFNL
        return :eof_newline_removed
    end
    return :unknown
end

type DiffLine
    hunk::DiffHunk
    line_origin::Symbol
    content::String
    old_lineno::Int
    new_lineno::Int
    content_offset::Union(Nothing, Int)

    function DiffLine(h::DiffHunk, ptr::Ptr{api.GitDiffLine})
        @assert ptr != C_NULL
        l = unsafe_load(ptr)
        c = Array(Uint8, l.content_len)
        for i in 1:l.content_len
            c[i] = unsafe_load(l.content, i)
        end
        return new(h,
                   line_origin_to_symbol(l.origin),
                   UTF8String(c),
                   l.old_lineno,
                   l.new_lineno,
                   l.content_offset == -1 ? nothing : l.content_offset)
    end 
end 

function lines(h::DiffHunk)
    err::Cint = 0
    ls = DiffLine[]
    lc = h.line_count
    hi = h.hunk_index
    line_ptr = Array(Ptr{api.GitDiffLine}, 1) 
    for i in 1:lc
        err = api.git_patch_get_line_in_hunk(
                        line_ptr, h.patch.ptr, hi - 1, i-1)
        if bool(err)
            break
        end
        push!(ls, DiffLine(h, line_ptr[1]))
    end
    if err != api.GIT_OK
        throw(GitError(err))
    end
    return ls
end

#TODO: memory leaks?
#TODO: unsafe_pointer_to_objref for payload?
function cb_patch_print(delta_ptr::Ptr{Void}, hunk_ptr::Ptr{Void},
                        line_ptr::Ptr{api.GitDiffLine}, payload::Ptr{Void})
    l = unsafe_load(line_ptr)
    s = unsafe_pointer_to_objref(payload)::Array{Uint8,1}
    add_origin = false
    if l.origin == api.DIFF_LINE_CONTEXT ||
       l.origin == api.DIFF_LINE_ADDITION ||
       l.origin == api.DIFF_LINE_DELETION
       add_origin = true
    end 
    prev_len = length(s)
    if add_origin
        resize!(s, prev_len + l.content_len + 1)
        s[prev_len + 1] = l.origin
        for i in 1:l.content_len
            s[prev_len + i + 1] = unsafe_load(l.content, i)
        end
    else
        resize!(s, prev_len + l.content_len)
        for i in 1:l.content_len
            s[prev_len + i] = unsafe_load(l.content, i)
        end
    end
    return api.GIT_OK
end

const c_cb_patch_print = cfunction(cb_patch_print, Cint,
                                  (Ptr{Void}, Ptr{Void}, Ptr{api.GitDiffLine}, Ptr{Void}))

Base.string(p::GitPatch) = begin
    @assert p.ptr != C_NULL
    s = Uint8[]
    @check ccall((:git_patch_print, api.libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Any),
                 p.ptr, c_cb_patch_print, &s)
    return UTF8String(s)
end

function nlines(p::GitPatch)
    @assert p.ptr != C_NULL
    ctx  = Csize_t[0]
    adds = Csize_t[0]
    dels = Csize_t[0]
    @check api.git_patch_line_stats(ctx, adds, dels, p.ptr)
    return ctx[1] + adds[1] + dels[1]
end

function nhunks(p::GitPatch)
    @assert p.ptr != C_NULL
    return int(api.git_patch_num_hunks(p.ptr))
end

function delta(p::GitPatch)
    @assert p.ptr != C_NULL
    delta_ptr = api.git_patch_get_delta(p.ptr)
    @assert delta_ptr != C_NULL
    return DiffDelta(delta_ptr)
end

function line_stats(p::GitPatch)
    @assert p.ptr != C_NULL
    lines = Array(Csize_t, 1)
    additions = Array(Csize_t, 1)
    deletions = Array(Csize_t, 1)
    @check api.git_patch_line_stats(lines, additions, deletions, p.ptr)
    return (lines[1], additions[1], deletions[1])
end
