export GitDiff, parse_git_diff_options, deltas, patches, diff_workdir, patch

type GitDiff
    ptr::Ptr{Void}

    function GitDiff(ptr::Ptr{Void})
        @assert ptr != C_NULL
        d = new(ptr)
        finalizer(d, free!)
        return d
    end
end

free!(d::GitDiff) = begin
    if d.ptr != C_NULL
        api.git_diff_free(d.ptr)
        d.ptr = C_NULL
    end
end


type DiffStats
    files::Int
    adds::Int
    dels::Int

    function DiffStats()
        return new(0, 0, 0)
    end
end

function cb_diff_file_stats(delta_ptr::Ptr{api.GitDiffDelta}, 
                            progress::Cfloat, 
                            payload::Ptr{Void})
    delta = unsafe_load(delta_ptr)
    stats = unsafe_pointer_to_objref(payload)::DiffStats
    if delta.status == api.DELTA_ADDED ||
       delta.status == api.DELTA_DELETED ||
       delta.status == api.DELTA_MODIFIED ||
       delta.status == api.DELTA_RENAMED ||
       delta.status == api.DELTA_COPIED ||
       delta.status == api.DELTA_TYPECHANGE
        stats.files += 1
    end
    return api.GIT_OK
end

const c_cb_diff_file_stats = cfunction(cb_diff_file_stats, Cint,
                                       (Ptr{api.GitDiffDelta}, Cfloat, Ptr{Void}))


function cb_diff_line_stats(delta_ptr::Ptr{Void},
                            hunk_ptr::Ptr{Void},
                            line_ptr::Ptr{api.GitDiffLine},
                            payload::Ptr{Void})
    line = unsafe_load(line_ptr)
    stats = unsafe_pointer_to_objref(payload)::DiffStats
    if line.origin == api.DIFF_LINE_ADDITION
        stats.adds += 1
    elseif line.origin == api.DIFF_LINE_DELETION
        stats.dels += 1
    end
    return api.GIT_OK
end

const c_cb_diff_line_stats = cfunction(cb_diff_line_stats, Cint,
                                       (Ptr{Void}, Ptr{Void}, 
                                        Ptr{api.GitDiffLine}, Ptr{Void}))


Base.stat(d::GitDiff) = begin
    stats = DiffStats()
    ccall((:git_diff_foreach, api.libgit2), Void,
          (Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Any),
          d.ptr, c_cb_diff_file_stats, C_NULL, c_cb_diff_line_stats, &stats)
    return stats
end

type DiffFile
    oid::Oid
    path::String
    size::Int
    flags::Int
    mode::Int
end

function delta_status_symbol(s::Integer)
    if s == api.DELTA_UNMODIFIED
        return :unmodified
    end
    if s == api.DELTA_ADDED
        return :added
    end
    if s == api.DELTA_DELETED
        return :deleted
    end
    if s == api.DELTA_MODIFIED
        return :modified
    end
    if s == api.DELTA_RENAMED
        return :renamed
    end
    if s == api.DELTA_COPIED
        return :copied
    end 
    if s == api.DELTA_IGNORED
        return :ignored
    end
    if s == api.DELTA_UNTRACKED
        return :untracked
    end
    if s == api.DELTA_TYPECHANGE
        return :typechange
    end
    return :unknown
end


type DiffDelta
    old_file::DiffFile
    new_file::DiffFile
    similarity::Int
    status::Symbol
    isbinary::Bool

    function DiffDelta(ptr::Ptr{api.GitDiffDelta})
        @assert ptr != C_NULL
        d = unsafe_load(ptr)
        
        arr = Array(Uint8, api.OID_RAWSZ)
        @get_oid_fieldnames(arr, d, old_file_oid)
        old_file_oid = Oid(arr)
        
        fold = DiffFile(old_file_oid,
                        d.old_file_path != C_NULL ? bytestring(d.old_file_path) : "",
                        int(d.old_file_size),
                        int(d.old_file_flags),
                        int(d.old_file_mode))
        
        arr = Array(Uint8, api.OID_RAWSZ)
        @get_oid_fieldnames(arr, d, new_file_oid)
        new_file_oid = Oid(arr)
                        
        fnew = DiffFile(new_file_oid,
                        d.new_file_path != C_NULL ? bytestring(d.new_file_path) : "",
                        int(d.new_file_size),
                        int(d.new_file_flags),
                        int(d.new_file_mode))
        return new(fold, 
                   fnew, 
                   int(d.similarity),
                   delta_status_symbol(d.status),
                   (bool(d.flags & api.DIFF_FLAG_NOT_BINARY) &&
                    bool(d.flags & api.DIFF_FLAG_BINARY)))
    end
end


Base.length(d::GitDiff) = begin
    @assert d.ptr != C_NULL
    return int(api.git_diff_num_deltas(d.ptr))
end

function deltas(d::GitDiff)
    @assert d.ptr != C_NULL
    ndelta = api.git_diff_num_deltas(d.ptr)
    if ndelta == 0
        return nothing
    end
    ds = Array(DiffDelta, ndelta)
    for i in 1:ndelta
        delta_ptr = api.git_diff_get_delta(d.ptr, i-1)
        @assert delta_ptr != C_NULL
        ds[i] = DiffDelta(delta_ptr)
    end
    return ds
end

function patches(d::GitDiff)
    @assert d.ptr != C_NULL
    ndelta = api.git_diff_num_deltas(d.ptr)
    if ndelta == 0
        return nothing
    end 
    err::Cint = 0
    ps = GitPatch[]
    patch_ptr = Array(Ptr{Void}, 1)
    for i in 1:ndelta
        err = api.git_patch_from_diff(patch_ptr, d.ptr, i-1)
        if err != api.GIT_OK
            break
        end
        @assert patch_ptr != C_NULL
        push!(ps, GitPatch(patch_ptr[1]))
    end
    if err != api.GIT_OK
        throw(GitError(err))
    end
    return ps
end 

#TODO: memory leaks?
#TODO: unsafe_pointer_to_objref for payload?
function cb_diff_print(delta_ptr::Ptr{Void}, hunk_ptr::Ptr{Void},
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


const c_cb_diff_print = cfunction(cb_diff_print, Cint,
                                  (Ptr{Void}, Ptr{Void}, Ptr{api.GitDiffLine}, Ptr{Void}))

function patch(d::GitDiff; format::Symbol=:patch)
    @assert d.ptr != C_NULL
    s = Uint8[]
    if format == :name_status
        ccall((:git_diff_print, api.libgit2), Cint,
              (Ptr{Void}, Cuint, Ptr{Void}, Any),
              d.ptr, api.DIFF_FORMAT_NAME_STATUS, c_cb_diff_print, &s)
    elseif format == :name_only
        ccall((:git_diff_print, api.libgit2), Cint,
              (Ptr{Void}, Cuint, Ptr{Void}, Any),
              d.ptr, api.DIFF_FORMAT_NAME_ONLY, c_cb_diff_print, &s)
    elseif format == :raw
        ccall((:git_diff_print, api.libgit2), Cint,
              (Ptr{Void}, Cuint, Ptr{Void}, Any),
              d.ptr, api.DIFF_FORMAT_RAW, c_cb_diff_print, &s)
    elseif format == :header
        ccall((:git_diff_print, api.libgit2), Cint,
              (Ptr{Void}, Cuint, Ptr{Void}, Any),
              d.ptr, api.DIFF_FORMAT_PATCH_HEADER, c_cb_diff_print, &s)
    elseif format == :patch
        ccall((:git_diff_print, api.libgit2), Cint,
              (Ptr{Void}, Cuint, Ptr{Void}, Any),
              d.ptr, api.DIFF_FORMAT_PATCH, c_cb_diff_print, &s)
    else
      error("Unknown diff output format ($format)")
    end
    return UTF8String(s)
end

# diffable GitTree, GitCommit, GitIndex, or Nothing
typealias Diffable Union(GitTree, GitCommit, GitIndex, Nothing)

Base.diff(repo::Repository, 
         left::Nothing, 
         right::Nothing, 
         opts=nothing) = begin
    return nothing
end

Base.diff(repo::Repository,
          left::Union(Nothing, String),
          right::Union(Nothing, String), 
          opts=nothing) = begin
    l = left  != nothing ? rev_parse(repo, left)  : nothing
    r = right != nothing ? rev_parse(repo, right) : nothing
    if l != nothing
        return diff(repo, l, r, opts)
    elseif r != nothing
        opts = opts == nothing ? {} : opts
        return diff(repo, l, r, 
                merge(opts, {:reverse => !get(opts, :reverse, true)}))
    end
    return nothing
end

Base.diff(repo::Repository,
          left::GitCommit,
          right::GitCommit,
          opts=nothing) = begin
    return diff(repo, 
                GitTree(left), 
                GitTree(right),
                opts)
end

Base.diff(repo::Repository,
          left::GitCommit,
          right::Nothing,
          opts=nothing) = begin
    return diff(repo, 
                GitTree(left), 
                nothing,
                opts)
end

Base.diff(repo::Repository,
          left::Nothing,
          right::GitCommit,
          opts=nothing) = begin
    return diff(repo,
                nothing, 
                GitTree(right),
                opts)
end

Base.diff(repo::Repository, c::GitCommit, opts=nothing) = begin
    ps = parents(c)
    if length(ps) > 0
        p = first(ps)
        return diff(repo, GitTree(c), GitTree(p), opts)
    else
        return diff(repo, GitTree(c), nothing, opts)
    end
end

Base.diff(repo::Repository, left::GitTree, right::String, opts=nothing) = begin
    other = rev_parse(repo, right)
    return diff(repo, left, other, opts)
end

Base.diff(repo::Repository, left::GitTree, right::GitCommit, opts=nothing) = begin
    return diff(repo, left, GitTree(right), opts)
end

Base.diff(repo::Repository,
          left::Union(Nothing, GitTree),
          right::Union(Nothing, GitTree),
          opts=nothing) = begin
    gopts = parse_git_diff_options(opts)
    diff_ptr = Array(Ptr{Void}, 1)
    @check ccall((:git_diff_tree_to_tree, api.libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, 
                  Ptr{Void}, Ptr{api.GitDiffOptions}),
                 diff_ptr, repo.ptr, 
                 left  != nothing ? left.ptr : C_NULL, 
                 right != nothing ? right.ptr : C_NULL,
                 &gopts)
    return GitDiff(diff_ptr[1])
end

Base.diff(repo::Repository, left::String, right::GitIndex, opts=nothing) = begin
    other = rev_parse(repo, left)
    return diff(repo, other, right, opts)
end

Base.diff(repo::Repository, left::GitCommit, right::GitIndex, opts=nothing) = begin
    return diff(repo, GitTree(left), right, opts)
end

Base.diff(repo::Repository, left::GitTree, right::GitIndex, opts=nothing) = begin
    gopts = parse_git_diff_options(opts)
    diff_ptr = Array(Ptr{Void}, 1)
    @check ccall((:git_diff_tree_to_index, api.libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, 
                  Ptr{Void}, Ptr{api.GitDiffOptions}),
                  diff_ptr, repo.ptr, left.ptr, right.ptr, &gopts)
    return GitDiff(diff_ptr[1])
end


Base.diff(repo::Repository, idx::GitIndex, opts=nothing) = begin
    return diff(repo, idx, nothing, opts)
end

Base.diff(repo::Repository, idx::GitIndex, other::Nothing, opts=nothing) = begin
    gopts = parse_git_diff_options(opts)
    diff_ptr = Array(Ptr{Void}, 1)
    @check ccall((:git_diff_index_to_workdir, api.libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{api.GitDiffOptions}),
                  diff_ptr, repo.ptr, idx.ptr, &gopts)
    return GitDiff(diff_ptr[1])
end

Base.diff(repo::Repository, opts=nothing) = begin
    return diff(repo, nothing, nothing, opts)
end

Base.diff(repo::Repository, idx::Nothing, other::Nothing, opts=nothing) = begin
    gopts = parse_git_diff_options(opts)
    diff_ptr = Array(Ptr{Void}, 1)
    @check ccall((:git_diff_index_to_workdir, api.libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{api.GitDiffOptions}),
                  diff_ptr, repo.ptr, C_NULL, &gopts)
    return GitDiff(diff_ptr[1])
end

Base.diff(repo::Repository, idx::GitIndex, other::GitCommit, opts=nothing) = begin
    return diff(repo, idx, GitTree(other), opts)
end

Base.diff(repo::Repository, idx::GitIndex, other::GitTree, opts=nothing) = begin
    gopts = parse_git_diff_options(opts)
    diff_ptr = Array(Ptr{Void}, 1)
    @check ccall((:git_diff_tree_to_index, api.libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, 
                  Ptr{Void}, Ptr{api.GitDiffOptions}),
                  diff_ptr, repo.ptr, other.ptr, idx.ptr, &gopts)
   return GitDiff(diff_ptr[1])
end


Base.merge!(d1::GitDiff, d2::GitDiff) = begin
    @check api.git_diff_merge(d1.ptr, d2.ptr)
    return d1
end

function diff_workdir(repo::Repository, left::String, opts=nothing)
    l = rev_parse(repo, left)
    diff_workdir(repo, l, opts)
end

function diff_workdir(repo::Repository, left::GitCommit, opts=nothing)
    diff_workdir(repo, GitTree(left), opts)
end

function diff_workdir(repo::Repository, left::GitTree, opts=nothing)
    gopts = parse_git_diff_options(opts)
    diff_ptr = Array(Ptr{Void}, 1)
    @check ccall((:git_diff_tree_to_workdir, api.libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{api.GitDiffOptions}),
                 diff_ptr, repo.ptr, left.ptr, &gopts)
    return GitDiff(diff_ptr[1])
end

function parse_git_diff_options()
    return api.GitDiffOptions()
end

function parse_git_diff_options(o::Nothing)
    return api.GitDiffOptions()
end

#TODO: better type error handling
#TODO: git str array leaks memory?
function parse_git_diff_options(opts::Dict)
    gdiff = api.GitDiffOptions()
    if haskey(opts, :max_size)
        gdiff.max_size = int64(opts[:max_size])
    end
    if haskey(opts, :context_lines)
        gdiff.context_lines = uint16(opts[:context_lines])
    end
    if haskey(opts, :interhunk_lines)
        gdiff.interhunk_lines = uint16(opts[:interhunk_lines])
    end
    if get(opts, :reverse, false)
        gdiff.flags |= api.DIFF_REVERSE
    end
    if get(opts, :force_text, false)
        gdiff.flags |= api.DIFF_FORCE_TEXT
    end
    if get(opts, :ignore_whitespace, false)
        gdiff.flags |= api.DIFF_IGNORE_WHITESPACE
    end
    if get(opts, :ignore_whitespace_change, false)
        gdiff.flags |= api.DIFF_IGNORE_WHITESPACE_CHANGE
    end
    if get(opts, :ignore_whitespace_eol, false)
        gdiff.flags |= api.DIFF_IGNORE_WHITESPACE_EOL
    end
    if get(opts, :ignore_submodules, false)
        gdiff.flags |= api.DIFF_IGNORE_SUBMODULES
    end
    if get(opts, :patience, false)
        gdiff.flags |= api.DIFF_PATIENCE
    end
    if get(opts, :minimal, false)
        gdiff.flags |= api.DIFF_MINIMAL
    end
    if get(opts, :include_ignored, false)
        gdiff.flags |= api.DIFF_INCLUDE_IGNORED
    end
    if get(opts, :include_untracked, false)
        gdiff.flags |= api.DIFF_INCLUDE_UNTRACKED
    end
    if get(opts, :include_unmodified, false)
       gdiff.flags |= api.DIFF_INCLUDE_UNMODIFIED
    end
    if get(opts, :recurse_untracked_dirs, false)
       gdiff.flags |= api.DIFF_RECURSE_UNTRACKED_DIRS
    end
    if get(opts, :disable_pathspec_match, false)
       gdiff.flags |= api.DIFF_DISABLE_PATHSPEC_MATCH
    end
    if get(opts, :show_untracked_content, false)
       gdiff.flags |= api.DIFF_SHOW_UNTRACKED_CONTENT
    end
    if get(opts, :skip_binary_check, false)
       gdiff.flags |= api.DIFF_SKIP_BINARY_CHECK
    end
    if get(opts, :include_typechange, false)
       gdiff.flags |= api.DIFF_INCLUDE_TYPECHANGE
    end
    if get(opts, :include_typechange_trees, false)
       gdiff.flags |= api.DIFF_INCLUDE_TYPECHANGE_TREES
    end
    if get(opts, :ignore_filemode, false)
       gdiff.flags |= api.DIFF_IGNORE_FILEMODE
    end
    if get(opts, :recurse_ignored_dirs, false)
       gdiff.flags |= api.DIFF_RECURSE_IGNORED_DIRS
    end
    if haskey(opts, :paths)
        paths = opts[:paths]
        if !(isa(paths, Array{ASCIIString, 1}))
            throw(ArgumentError("opts[:paths] must be of type Array{String}"))
        end
        gdiff.pathspec_count = convert(Csize_t, length(paths))
        str_ptrs = Array(Ptr{Cchar}, length(paths))
        for i in 1:length(paths)
            str_ptrs[i] = convert(Ptr{Cchar}, pointer(bytestring(paths[i])))
        end
        gdiff.pathspec_strings = convert(Ptr{Ptr{Cchar}}, str_ptrs)
    end 
    return gdiff
end
