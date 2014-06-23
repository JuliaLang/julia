module api

@unix_only begin
    const libgit2 = "libgit2"
end
@windows_only begin
    const libgit2 = "git2"
end

macro libgit(func, ret_type, arg_types)
  local args_in = Symbol[symbol("arg$i::$T")
                         for (i, T) in enumerate(arg_types.args)]
  quote
      function $(esc(func))($(args_in...))
          err = ccall(($(string(func)), libgit2),
                       $ret_type,
                       $arg_types,
                       $(args_in...))
          return err
      end
   end
end

cint(i)  = convert(Cint, i)
cuint(i) = convert(Cuint, i)

# ----- libgit constants -----
const OID_RAWSZ = 20
const OID_HEXSZ = 40

const GIT_OK     = cint(0)
const ERROR      = cint(-01)  
const ENOTFOUND  = cint(-03)  
const EEXISTS    = cint(-04) 
const EAMBIGUOUS = cint(-05) 
const EBUFS      = cint(-06) 
const EUSER      = cint(-07) 
const EBAREREPO       = cint(-08) 
const EUNBORNBRANCH   = cint(-09) 
const EUNMERGED       = cint(-10) 
const ENONFASTFORWARD = cint(-11) 
const EINVALIDSPEC    = cint(-12) 
const EMERGECONFLICT  = cint(-13) 
const ELOCKED         = cint(-14) 
const PASSTHROUGH     = cint(-30) 
const ITEROVER        = cint(-31)

const OBJ_ANY    = cint(-2)
const OBJ_BAD    = cint(-1)
const OBJ_COMMIT = cint(1)
const OBJ_TREE   = cint(2)
const OBJ_BLOB   = cint(3)
const OBJ_TAG    = cint(4)

const SORT_NONE = cint(0)
const SORT_TOPOLOGICAL = cint(1) << cint(0)
const SORT_TIME = cint(1) << cint(1)
const SORT_REVERSE = cint(1) << cint(2)

const REF_INVALID = cint(0)
const REF_OID = cint(1)    
const REF_SYMBOLIC = cint(2)
const REF_LISTALL = REF_OID | REF_SYMBOLIC

const BRANCH_LOCAL = cint(1)
const BRANCH_REMOTE = cint(2)

const FILEMODE_NEW             = cint(00000)
const FILEMODE_TREE            = cint(16384)
const FILEMODE_BLOB            = cint(33188)
const FILEMODE_BLOB_EXECUTABLE = cint(33261)
const FILEMODE_LINK            = cint(40960)
const FILEMODE_COMMIT          = cint(57344)

const CHECKOUT_NONE                    = cint(0)
const CHECKOUT_SAFE                    = cuint(1) << cint(0)
const CHECKOUT_SAFE_CREATE             = cuint(1) << cint(1)
const CHECKOUT_FORCE                   = cuint(1) << cint(2)
const CHECKOUT_ALLOW_CONFLICTS         = cuint(1) << cint(4)
const CHECKOUT_REMOVE_UNTRACKED        = cuint(1) << cint(5)
const CHECKOUT_REMOVE_IGNORED          = cuint(1) << cint(6)
const CHECKOUT_UPDATE_ONLY             = cuint(1) << cint(7)
const CHECKOUT_DONT_UPDATE_INDEX       = cuint(1) << cint(8)
const CHECKOUT_NO_REFRESH              = cuint(1) << cint(9)
const CHECKOUT_SKIP_UNMERGED           = cuint(1) << cint(10)
const CHECKOUT_USE_OURS                = cuint(1) << cint(11)
const CHECKOUT_USE_THEIRS              = cuint(1) << cint(12)
const CHECKOUT_DISABLE_PATHSPEC_MATCH  = cuint(1) << cint(13)
const CHECKOUT_SKIP_LOCKED_DIRECTORIES = cuint(1) << cint(18)
const CHECKOUT_DONT_OVERWRITE_IGNORED  = cuint(1) << cint(19)

const CHECKOUT_UPDATE_SUBMODULES       = cuint(1) << cint(16)
const CHECKOUT_UPDATE_SUBMODULES_IF_CHANGED = cuint(1) << cint(17)

const CHECKOUT_NOTIFY_NONE      = cint(0)
const CHECKOUT_NOTIFY_CONFLICT  = cuint(1) << cint(0)
const CHECKOUT_NOTIFY_DIRTY     = cuint(1) << cint(1)
const CHECKOUT_NOTIFY_UPDATED   = cuint(1) << cint(2)
const CHECKOUT_NOTIFY_UNTRACKED = cuint(1) << cint(3)
const CHECKOUT_NOTIFY_IGNORED   = cuint(1) << cint(4)
const CHECKOUT_NOTIFY_ALL       = 0x0FFFF

const SUBMODULE_UPDATE_RESET    = cint(-1)
const SUBMODULE_UPDATE_CHECKOUT = cint(1)
const SUBMODULE_UPDATE_REBASE   = cint(2)
const SUBMODULE_UPDATE_MERGE    = cint(3)
const SUBMODULE_UPDATE_NONE     = cint(4)
const SUBMODULE_UPDATE_DEFAULT  = cint(0)

# git_submodule_ignore_t
const SUBMODULE_IGNORE_RESET     = cint(-1)
const SUBMODULE_IGNORE_NONE      = cint(1)  
const SUBMODULE_IGNORE_UNTRACKED = cint(2)  
const SUBMODULE_IGNORE_DIRTY     = cint(3)  
const SUBMODULE_IGNORE_ALL       = cint(4) 
const SUBMODULE_IGNORE_DEFAULT   = cint(0)

const TREEWALK_PRE  = cint(0)
const TREEWALK_POST = cint(1)

const GIT_PATH_MAX = cint(4096)

const DIFF_OPTIONS_VERSION = cint(1)

const DIFF_NORMAL  = cint(0)
const DIFF_REVERSE = cuint(1) << cint(0)
const DIFF_INCLUDE_IGNORED = cuint(1) << cint(1)
const DIFF_RECURSE_IGNORED_DIRS = cuint(1) << cint(2)
const DIFF_INCLUDE_UNTRACKED = cuint(1) << cint(3)
const DIFF_RECURSE_UNTRACKED_DIRS = cuint(1) << cint(4)
const DIFF_INCLUDE_UNMODIFIED = cuint(1) << cint(5)
const DIFF_INCLUDE_TYPECHANGE = cuint(1) << cint(6)
const DIFF_INCLUDE_TYPECHANGE_TREES = cuint(1) << cint(7)
const DIFF_IGNORE_FILEMODE = cuint(1) << cint(8)
const DIFF_IGNORE_SUBMODULES = cuint(1) << cint(9)
const DIFF_IGNORE_CASE = cuint(1) << cint(10)
const DIFF_DISABLE_PATHSPEC_MATCH = cuint(1) << cint(12)
const DIFF_SKIP_BINARY_CHECK = cuint(1) << cint(13)
const DIFF_ENABLE_FAST_UNTRACKED_DIRS = cuint(1) << cint(14)

const DIFF_FORCE_TEXT = cuint(1) << cint(20)
const DIFF_FORCE_BINARY = cuint(1) << cint(21)
const DIFF_IGNORE_WHITESPACE = cuint(1) << cint(22)
const DIFF_IGNORE_WHITESPACE_CHANGE = cuint(1) << cint(23)
const DIFF_IGNORE_WHITESPACE_EOL = cuint(1) << cint(24)
const DIFF_SHOW_UNTRACKED_CONTENT = cuint(1) << cint(25)
const DIFF_SHOW_UNMODIFIED = cuint(1) << cint(26)
const DIFF_PATIENCE = cuint(1) << cint(28)
const DIFF_MINIMAL = cuint(1) << cint(29)

const DIFF_FLAG_BINARY     = cuint(1) << cint(0)
const DIFF_FLAG_NOT_BINARY = cuint(1) << cint(1)
const DIFF_FLAG_VALID_OID  = cuint(1) << cint(2)

const DIFF_FORMAT_PATCH = cuint(1)
const DIFF_FORMAT_PATCH_HEADER = cuint(2)
const DIFF_FORMAT_RAW = cuint(3)
const DIFF_FORMAT_NAME_ONLY = cuint(4)
const DIFF_FORMAT_NAME_STATUS = cuint(5)

const DELTA_UNMODIFIED = cint(0)
const DELTA_ADDED      = cint(1)
const DELTA_DELETED    = cint(2)
const DELTA_MODIFIED   = cint(3)
const DELTA_RENAMED    = cint(4)
const DELTA_COPIED     = cint(5)
const DELTA_IGNORED    = cint(6)
const DELTA_UNTRACKED  = cint(7)
const DELTA_TYPECHANGE = cint(8)

cchar(c::Char) = convert(Cchar, c)

const DIFF_LINE_CONTEXT   = cchar(' ')
const DIFF_LINE_ADDITION  = cchar('+')
const DIFF_LINE_DELETION  = cchar('-')

const DIFF_LINE_CONTEXT_EOFNL = cchar('=')
const DIFF_LINE_ADD_EOFNL = cchar('>')
const DIFF_LINE_DEL_EOFNL = cchar('<')

const DIFF_LINE_FILE_HDR  = cchar('F')
const DIFF_LINE_HUNK_HDR  = cchar('H')
const DIFF_LINE_BINARY    = cchar('B')

# index 
const IDXENTRY_NAMEMASK   = (0x0fff)
const IDXENTRY_STAGEMASK  = (0x3000)
const IDXENTRY_EXTENDED   = (0x4000)
const IDXENTRY_VALID      = (0x8000)
const IDXENTRY_STAGESHIFT = cint(12)

const IDXENTRY_UPDATE            = cint(1) << cint(0)
const IDXENTRY_REMOVE            = cint(1) << cint(1)
const IDXENTRY_UPTODATE          = cint(1) << cint(2)
const IDXENTRY_ADDED             = cint(1) << cint(3)

const IDXENTRY_HASHED            = cint(1) << cint(4)
const IDXENTRY_UNHASHED          = cint(1) << cint(5)
const IDXENTRY_WT_REMOVE         = cint(1) << cint(6)
const IDXENTRY_CONFLICTED        = cint(1) << cint(7)

const IDXENTRY_UNPACKED          = cint(1) << cint(8)
const IDXENTRY_NEW_SKIP_WORKTREE = cint(1) << cint(9)

const INDEXCAP_IGNORE_CASE = cuint(1)
const INDEXCAP_NO_FILEMODE = cuint(2)
const INDEXCAP_NO_SYMLINKS = cuint(4)
const INDEXCAP_FROM_OWNER  = ~(cuint(0))

const INDEX_ADD_DEFAULT = cint(0)
const INDEX_ADD_FORCE   = cuint(1) << cint(0)
const INDEX_ADD_DISABLE_PATHSPEC_MATCH = cuint(1) << cint(1)
const INDEX_ADD_CHECK_PATHSPEC = cuint(1) << cint(2)

const INDEX_STAGE_ANY = cint(-1)

const MERGE_TREE_FIND_RENAMES = cint(1) << cint(0)
const MERGE_AUTOMERGE_NORMAL  = cint(0)
const MERGE_AUTOMERGE_NONE    = cint(1)
const MERGE_AUTOMERGE_FAVOR_OURS = cint(2)
const MERGE_AUTOMERGE_FAVOR_THEIRS = cint(3)

const MERGE_NO_FASTFORWARD = cint(1)
const MERGE_FASTFORWARD_ONLY = cint(2)

const DIRECTION_FETCH = cint(0)
const DIRECTION_PUSH  = cint(1)

const BLAME_NORMAL = cint(0)

const CREDTYPE_USERPASS_PLAINTEXT = cuint(1) << cint(0)
const CREDTYPE_SSH_KEY = cuint(1) << cint(1)
const CREDTYPE_SSH_CUSTOM = cuint(1) << cint(2)
const CREDTYPE_DEFAULT = cuint(1) << cint(3)

type GitMergeOpts
    version::Cuint
    merge_flags::Cint
    merge_tree_opts::Cint
    checkout_opts::Cint

    function GitMergeOpts()
        return new(1, 0, 1, 1)
    end
end

type GitMergeTreeOpts
    version::Cuint
    flags::Cint
    rename_threshold::Cuint
    target_limit::Cuint
    metric::Ptr{Void}
    automerge_flags::Cint

    function GitMergeTreeOpts()
        return new(1, 0, 0, C_NULL, 0)
    end
end

type GitStrArray
   strings::Ptr{Ptr{Cchar}}
   count::Csize_t

   function GitStrArray()
       sa = new(C_NULL, 0)
       finalizer(sa, free!)
       return sa
   end
   
   function GitStrArray(s::Array{String, 1})
       #TODO: memory management?
       str_ptr = Array(Ptr{Cchar}, length(s))
       for i in length(s)
           str_ptr[i] = convert(Ptr{Cchar}, pointer(bytestring(s[i])))
       end
       sa = new(str_ptr, length(s))
       return sa
   end
end

    
free!(sa::GitStrArray) = begin
    if sa.strings != C_NULL && sa.count > 0
        for i in 1:sa.count
            cptr = unsafe_load(sa.strings, i)
            @assert cptr != C_NULL
            c_free(cptr)
        end
        sa.strings = C_NULL
        sa.count  = 0
    end
end

# ----- libgit buffer -----
type GitBuffer
    ptr::Ptr{Cchar}
    asize::Csize_t
    size::Csize_t
end

GitBuffer() = begin
    buf = GitBuffer(C_NULL, 0, 0)
    return buf
end

free!(b::GitBuffer) = begin
    # this does not free the git_buf
    # itself but memory pointed to by buf-> ptr
    if b.ptr != C_NULL
        ccall((:git_buf_free, libgit2), Void,
              (Ptr{GitBuffer},), &buf)
        b.ptr = C_NULL
    end
end

# ----- libgit repo ------
@libgit(git_repository_discover, Cint, 
        (Ptr{Cchar}, Csize_t, Ptr{Cchar}, Cint, Ptr{Void}))  
@libgit(git_repository_open, Cint, (Ptr{Ptr{Void}}, Ptr{Cchar}))
@libgit(git_repository_init, Cint, (Ptr{Ptr{Void}}, Ptr{Cchar}, Cint))
@libgit(git_repository_free, Cint, (Ptr{Void},))
@libgit(git_repository__cleanup, Void, (Ptr{Void},))
@libgit(git_repository_index, Cint, (Ptr{Ptr{Void}}, Ptr{Void}))
@libgit(git_repository_workdir, Ptr{Cchar}, (Ptr{Void},))
@libgit(git_repository_path, Ptr{Cchar}, (Ptr{Void},))
@libgit(git_repository_odb, Cint, (Ptr{Ptr{Void}}, Ptr{Void}))
@libgit(git_revparse_single, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}))
@libgit(git_repository_config, Cint, (Ptr{Ptr{Void}}, Ptr{Void}))
@libgit(git_repository_head, Cint, (Ptr{Ptr{Void}}, Ptr{Void}))
@libgit(git_repository_set_head, Cint, (Ptr{Void}, Ptr{Cchar}))
@libgit(git_repository_set_head_detached, Cint, (Ptr{Void}, Ptr{Uint8}, Ptr{Void}, Ptr{Void}))
# TODO: Implement it
@libgit(git_repository_detach_head, Cint, (Ptr{Void}, Ptr{Void}, Ptr{Cchar}))
@libgit(git_repository_set_namespace, Cint, (Ptr{Void}, Ptr{Cchar}))
@libgit(git_repository_get_namespace, Ptr{Cchar}, (Ptr{Void},))

@libgit(git_repository_is_empty, Cint, (Ptr{Void},))
@libgit(git_repository_is_shallow, Cint, (Ptr{Void},))
@libgit(git_repository_is_bare, Cint, (Ptr{Void},))
@libgit(git_repository_head_detached, Cint, (Ptr{Void},))

# ----- libgit index ------

type GitIndexEntry
    ctime_seconds::Int64
    ctime_nanoseconds::Cuint
    
    mtime_seconds::Int64
    mtime_nanoseconds::Cuint
    
    #TODO: why is this necessary??
    padding::Cuint
    
    dev::Cuint
    ino::Cuint
    mode::Cuint
    uid::Cuint
    gid::Cuint
    file_size::Int64

    oid1::Uint8
    oid2::Uint8
    oid3::Uint8
    oid4::Uint8
    oid5::Uint8
    oid6::Uint8
    oid7::Uint8
    oid8::Uint8
    oid9::Uint8
    oid10::Uint8
    oid11::Uint8
    oid12::Uint8
    oid13::Uint8
    oid14::Uint8
    oid15::Uint8
    oid16::Uint8
    oid17::Uint8
    oid18::Uint8
    oid19::Uint8
    oid20::Uint8
    
    flags::Uint16
    flags_extended::Uint16

    path::Ptr{Cchar}
end

@libgit(git_index_open, Cint, (Ptr{Ptr{Void}}, Ptr{Cchar}))
@libgit(git_index_free, Cint, (Ptr{Void},))
@libgit(git_index_add_bypath, Cint, (Ptr{Void}, Ptr{Cchar}))
@libgit(git_index_write_tree, Cint, (Ptr{Uint8}, Ptr{Void}))
@libgit(git_index_clear, Void, (Ptr{Void},))
@libgit(git_index_read, Cint, (Ptr{Void}, Cint))
@libgit(git_index_write, Cint, (Ptr{Void},))
@libgit(git_index_entrycount, Csize_t, (Ptr{Void},))
@libgit(git_index_get_byindex, Ptr{GitIndexEntry}, (Ptr{Void}, Csize_t))
@libgit(git_index_get_bypath,  Ptr{GitIndexEntry}, (Ptr{Void}, Ptr{Cchar}, Cint))
@libgit(git_index_remove, Cint, (Ptr{Void}, Ptr{Cchar}, Cint))
@libgit(git_index_remove_directory, Cint, (Ptr{Void}, Ptr{Cchar}, Cint))
@libgit(git_index_read_tree, Cint, (Ptr{Void}, Ptr{Void}))
@libgit(git_index_has_conflicts, Cint, (Ptr{Void},))

# ----- libgit object ------
@libgit(git_object_id, Ptr{Uint8}, (Ptr{Void},))
@libgit(git_object_free, Cint, (Ptr{Void},))
@libgit(git_object_id, Ptr{Uint8}, (Ptr{Void},))
@libgit(git_object_lookup, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Uint8}, Cint))
@libgit(git_object_lookup_prefix, Cint,
        (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Uint8}, Csize_t, Cint))
@libgit(git_object_type, Cint, (Ptr{Void},))
@libgit(git_object_owner, Ptr{Void}, (Ptr{Void},))

@libgit(git_oid_cmp, Cint, (Ptr{Uint8}, Ptr{Uint8}))
@libgit(git_oid_fmt, Cint, (Ptr{Cchar}, Ptr{Uint8}))
@libgit(git_oid_fromstr, Cint, (Ptr{Uint8}, Ptr{Cchar}))
@libgit(git_oid_fromstrn, Cint, (Ptr{Uint8}, Ptr{Cchar}, Csize_t))

# ----- libgit note ------
@libgit(git_note_read, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Uint8}))
@libgit(git_note_message, Ptr{Cchar}, (Ptr{Void},))
@libgit(git_note_id, Ptr{Uint8}, (Ptr{Void},))
@libgit(git_note_free, Void, (Ptr{Void},))
@libgit(git_note_default_ref, Cint, (Ptr{Ptr{Cchar}}, Ptr{Void},))
@libgit(git_note_foreach, Cint, (Ptr{Void}, Ptr{Cchar}, Ptr{Void}, Ptr{Void}))

# ----- libgit patch ------
@libgit(git_patch_free, Void, (Ptr{Void},))
@libgit(git_patch_line_stats, Cint, 
        (Ptr{Csize_t}, Ptr{Csize_t}, Ptr{Csize_t}, Ptr{Void}))
@libgit(git_patch_print, Cint,
        (Ptr{Void}, Ptr{Void}, Ptr{Void}))
@libgit(git_patch_num_hunks, Csize_t, (Ptr{Void},))
@libgit(git_patch_get_delta, Ptr{GitDiffDelta}, (Ptr{Void},))

# ----- libgit graph ------
@libgit(git_graph_ahead_behind, Cint, 
        (Ptr{Csize_t}, Ptr{Csize_t}, Ptr{Void}, Ptr{Uint8}, Ptr{Uint8}))

# ----- libgit merge ------
@libgit(git_merge_base_many, Cint,
        (Ptr{Uint8}, Ptr{Void}, Csize_t, Ptr{Void}))

# ----- libgit diff ------
type GitDiffFile
    oid::Ptr{Uint8}
    path::Ptr{Cchar}
    size::Int64
    flags::Uint32
    mode::Uint16
end

type GitDiffDelta 
    status::Cint
    flags::Uint32
    similarity::Uint16
    nfiles::Uint16
    
    #TODO: why is padding necessary?
    pad1::Cuint

    old_file_oid1::Uint8
    old_file_oid2::Uint8
    old_file_oid3::Uint8
    old_file_oid4::Uint8
    old_file_oid5::Uint8
    old_file_oid6::Uint8
    old_file_oid7::Uint8
    old_file_oid8::Uint8
    old_file_oid9::Uint8
    old_file_oid10::Uint8
    old_file_oid11::Uint8
    old_file_oid12::Uint8
    old_file_oid13::Uint8
    old_file_oid14::Uint8
    old_file_oid15::Uint8
    old_file_oid16::Uint8
    old_file_oid17::Uint8
    old_file_oid18::Uint8
    old_file_oid19::Uint8
    old_file_oid20::Uint8

    old_file_path::Ptr{Cchar}
    old_file_size::Int64
    old_file_flags::Uint32
    old_file_mode::Uint16

    pad2::Cuint

    new_file_oid1::Uint8
    new_file_oid2::Uint8
    new_file_oid3::Uint8
    new_file_oid4::Uint8
    new_file_oid5::Uint8
    new_file_oid6::Uint8
    new_file_oid7::Uint8
    new_file_oid8::Uint8
    new_file_oid9::Uint8
    new_file_oid10::Uint8
    new_file_oid11::Uint8
    new_file_oid12::Uint8
    new_file_oid13::Uint8
    new_file_oid14::Uint8
    new_file_oid15::Uint8
    new_file_oid16::Uint8
    new_file_oid17::Uint8
    new_file_oid18::Uint8
    new_file_oid19::Uint8
    new_file_oid20::Uint8

    new_file_path::Ptr{Cchar}
    new_file_size::Int64
    new_file_flags::Uint32
    new_file_mode::Uint16
end

type GitDiffOptions
    version::Cuint
    flags::Uint32
    # opts controlling which files are in the diff
    ignore_submodules::Cint
    pathspec_strings::Ptr{Ptr{Cchar}}
    pathspec_count::Csize_t
    notify_cb::Ptr{Void}
    notify_payload::Ptr{Void}
    # opts controlling how the diff text is generated
    context_lines::Uint16
    interhunk_lines::Uint16
    oid_abbrev::Uint16
    max_size::Int64
    old_prefix::Ptr{Cchar}
    new_prefix::Ptr{Cchar}

    function GitDiffOptions()
        return new(DIFF_OPTIONS_VERSION, 
                   0, 
                   SUBMODULE_IGNORE_DEFAULT, 
                   C_NULL, 
                   0, 
                   C_NULL, 
                   C_NULL, 
                   3, 
                   0, 
                   0, 
                   0, 
                   C_NULL, 
                   C_NULL)
    end
end

@libgit(git_diff_free, Void, (Ptr{Void},))
@libgit(git_diff_num_deltas, Csize_t, (Ptr{Void},))
@libgit(git_diff_get_delta, Ptr{GitDiffDelta}, (Ptr{Void}, Csize_t))
@libgit(git_patch_from_diff, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Csize_t))
@libgit(git_diff_merge, Cint, (Ptr{Void}, Ptr{Void}))
@libgit(git_diff_print, Void, (Ptr{Void}, Cuint, Ptr{Void}, Ptr{Cchar}))

# ----- libgit signature ------
type GitSignature
    name::Ptr{Cchar}
    email::Ptr{Cchar}
    time::Int64
    time_offset::Cint
end

free!(s::GitSignature) = begin
    if s.name != C_NULL
        c_free(s.name)
    end
    s.name = C_NULL
    if s.email != C_NULL
        c_free(s.email)
    end
    s.email = C_NULL
end

@libgit(git_signature_default, Cint,
        (Ptr{Ptr{GitSignature}}, Ptr{Void}))
@libgit(git_signature_new, Cint, 
        (Ptr{Ptr{GitSignature}}, Ptr{Cchar}, Ptr{Cchar}, Int64, Cint))
@libgit(git_signature_now, Cint, 
        (Ptr{Ptr{GitSignature}}, Ptr{Cchar}, Ptr{Cchar}))

# ----- libgit commit ------
@libgit(git_commit_message, Ptr{Cchar}, (Ptr{Void},))
@libgit(git_commit_message_raw, Ptr{Cchar}, (Ptr{Void},))
@libgit(git_commit_tree, Cint, (Ptr{Ptr{Void}}, Ptr{Void}))
@libgit(git_commit_tree_id, Ptr{Uint8}, (Ptr{Void},))
@libgit(git_commit_committer, Ptr{GitSignature}, (Ptr{Void},))
@libgit(git_commit_author, Ptr{GitSignature}, (Ptr{Void},))
@libgit(git_commit_parent, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Cuint))
@libgit(git_commit_parentcount, Csize_t, (Ptr{Void},))
@libgit(git_commit_parent_id, Ptr{Void}, (Ptr{Void}, Cuint))
@libgit(git_commit_lookup, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Uint8}))
@libgit(git_commit_lookup_prefix, Cint,
        (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Csize_t))
@libgit(git_commit_create, Cint,
        (Ptr{Uint8}, Ptr{Void}, Ptr{Cchar}, Ptr{GitSignature},
         Ptr{GitSignature}, Ptr{Cchar}, Ptr{Cchar}, Ptr{Void},
         Cint, Ptr{Ptr{Void}}))
                                        
# ----- libgit tag ------
@libgit(git_tag_name, Ptr{Cchar}, (Ptr{Void},))
@libgit(git_tag_message, Ptr{Cchar}, (Ptr{Void},))
@libgit(git_tag_target_id, Ptr{Uint8}, (Ptr{Void},))
@libgit(git_tag_target, Cint, (Ptr{Ptr{Void}}, Ptr{Void}))
@libgit(git_tag_tagger, Ptr{GitSignature}, (Ptr{Void},))
@libgit(git_tag_delete, Cint, (Ptr{Void}, Ptr{Cchar}))
@libgit(git_tag_create_lightweight, Cint, 
        (Ptr{Uint8}, Ptr{Void}, Ptr{Cchar}, Ptr{Void}, Cint))

# ------ libgit blob ------
@libgit(git_blob_rawsize, Int64, (Ptr{Void},))
@libgit(git_blob_owner, Ptr{Void}, (Ptr{Void},))
@libgit(git_blob_rawcontent, Ptr{Uint8}, (Ptr{Void},))
@libgit(git_blob_is_binary, Cint, (Ptr{Void},))
@libgit(git_blob_create_frombuffer, Cint, 
        (Ptr{Uint8}, Ptr{Void}, Ptr{Uint8}, Csize_t))
@libgit(git_blob_create_fromworkdir, Cint,
        (Ptr{Uint8}, Ptr{Void}, Ptr{Cchar}))
@libgit(git_blob_create_fromdisk, Cint,
        (Ptr{Uint8}, Ptr{Void}, Ptr{Cchar}))
@libgit(git_blob_is_binary, Cint,
        (Ptr{Void},))

# ------ libgit tree ------
@libgit(git_tree_entry_bypath, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar})) 
@libgit(git_tree_entry_byname, Ptr{Void}, (Ptr{Void}, Ptr{Cchar}))
@libgit(git_tree_entry_byindex, Ptr{Void}, (Ptr{Void}, Csize_t))
@libgit(git_tree_entry_byid, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}))
@libgit(git_tree_entry_free, Void, (Ptr{Void},))
@libgit(git_tree_entry_name, Ptr{Cchar}, (Ptr{Void},))
@libgit(git_tree_entry_id, Ptr{Uint8}, (Ptr{Void},))
@libgit(git_tree_entry_type, Cint, (Ptr{Void},))
@libgit(git_tree_entry_filemode, Cint, (Ptr{Void},))
@libgit(git_tree_entrycount, Csize_t, (Ptr{Void},))
@libgit(git_tree_walk, Cint, (Ptr{Void}, Cint, Ptr{Void}, Ptr{Void}))

@libgit(git_treebuilder_create, Cint, (Ptr{Ptr{Void}}, Ptr{Void}))
@libgit(git_treebuilder_free, Void, (Ptr{Void},))
@libgit(git_treebuilder_write, Cint, (Ptr{Uint8}, Ptr{Void}, Ptr{Void}))
@libgit(git_treebuilder_insert, Cint,
        (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Void}, Cint)) 
                                        
# ------ libgit walker ------
@libgit(git_revwalk_new, Cint, (Ptr{Ptr{Void}}, Ptr{Void}))
@libgit(git_revwalk_next, Cint, (Ptr{Uint8}, Ptr{Void}))
@libgit(git_revwalk_free, Void, (Ptr{Void},))
@libgit(git_revwalk_repository, Ptr{Void}, (Ptr{Void},))
@libgit(git_revwalk_push, Cint, (Ptr{Void}, Ptr{Uint8}))
@libgit(git_revwalk_hide, Cint, (Ptr{Void}, Ptr{Uint8}))
@libgit(git_revwalk_sorting, Void, (Ptr{Void}, Cint))
@libgit(git_revwalk_reset, Void, (Ptr{Void},))

# ------ libgit remote ------
@libgit(git_remote_free, Void, (Ptr{Void},))
@libgit(git_remote_list, Cint, (Ptr{GitStrArray}, Ptr{Void}))
@libgit(git_remote_name, Ptr{Cchar}, (Ptr{Void},))
@libgit(git_remote_load, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}))
@libgit(git_remote_create, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Cchar}))
@libgit(git_remote_create_anonymous, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Cchar}))
@libgit(git_remote_create_with_fetchspec, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Cchar}, Ptr{Cchar}))
@libgit(git_remote_connected, Cint, (Ptr{Void},))
@libgit(git_remote_connect, Cint, (Ptr{Void}, Cint))
@libgit(git_remote_disconnect, Cint, (Ptr{Void}, ))
@libgit(git_remote_url, Ptr{Cchar}, (Ptr{Void},))
@libgit(git_remote_valid_url, Cint, (Ptr{Cchar},))
@libgit(git_remote_set_url, Cint, (Ptr{Void}, Ptr{Cchar}))
@libgit(git_remote_pushurl, Ptr{Cchar}, (Ptr{Void},))
@libgit(git_remote_set_pushurl, Cint, (Ptr{Void}, Ptr{Cchar}))
@libgit(git_remote_add_push, Cint, (Ptr{Void}, Ptr{Cchar}))
@libgit(git_remote_add_fetch, Cint, (Ptr{Void}, Ptr{Cchar}))
@libgit(git_remote_clear_refspecs, Void, (Ptr{Void},))
@libgit(git_remote_save, Cint, (Ptr{Void},))
@libgit(git_remote_delete, Cint, (Ptr{Void},))
@libgit(git_remote_fetch, Cint, (Ptr{Void}, Ptr{Void}, Ptr{Cchar}))
@libgit(git_remote_download, Cint, (Ptr{Void},))
@libgit(git_remote_update_tips, Cint, (Ptr{Void},))

type GitRemoteHead
    islocal::Cint
    # oid
    oid1::Uint8
    oid2::Uint8
    oid3::Uint8
    oid4::Uint8
    oid5::Uint8
    oid6::Uint8
    oid7::Uint8
    oid8::Uint8
    oid9::Uint8
    oid10::Uint8
    oid11::Uint8
    oid12::Uint8
    oid13::Uint8
    oid14::Uint8
    oid15::Uint8
    oid16::Uint8
    oid17::Uint8
    oid18::Uint8
    oid19::Uint8
    oid20::Uint8
    
    #loid
    loid1::Uint8
    loid2::Uint8
    loid3::Uint8
    loid4::Uint8
    loid5::Uint8
    loid6::Uint8
    loid7::Uint8
    loid8::Uint8
    loid9::Uint8
    loid10::Uint8
    loid11::Uint8
    loid12::Uint8
    loid13::Uint8
    loid14::Uint8
    loid15::Uint8
    loid16::Uint8
    loid17::Uint8
    loid18::Uint8
    loid19::Uint8
    loid20::Uint8
    name::Ptr{Cchar}
end

@libgit(git_remote_ls, Cint, (Ptr{Ptr{Ptr{GitRemoteHead}}}, Ptr{Csize_t}, Ptr{Void}))

# ------ libgit remote ------
@libgit(git_push_new, Cint, (Ptr{Ptr{Void}}, Ptr{Void}))
@libgit(git_push_free, Void, (Ptr{Void},))
@libgit(git_push_add_refspec, Cint, (Ptr{Void}, Ptr{Cchar}))
@libgit(git_push_finish, Cint, (Ptr{Void},))
@libgit(git_push_unpack_ok, Cint, (Ptr{Void},))
@libgit(git_push_update_tips, Cint, (Ptr{Void},))

# ------ libgit branch ------
@libgit(git_branch_create, Cint,
        (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Void}, Cint))
@libgit(git_branch_iterator_new, Cint, (Ptr{Void}, Ptr{Void}, Cint))
@libgit(git_branch_next, Cint, (Ptr{Void}, Ptr{Cint}, Ptr{Void}))
@libgit(git_branch_iterator_free, Void, (Ptr{Void},))
@libgit(git_branch_lookup, Cint, (Ptr{Void}, Ptr{Void}, Ptr{Cchar}, Cint))
@libgit(git_branch_name, Cint, (Ptr{Ptr{Cchar}}, Ptr{Void}))
@libgit(git_branch_delete, Cint, (Ptr{Void},))
@libgit(git_branch_is_head, Cint, (Ptr{Void},))
@libgit(git_branch_move, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Cint))
@libgit(git_branch_remote_name, Cint, 
        (Ptr{Cchar}, Ptr{Void}, Ptr{Cchar}))
@libgit(git_branch_upstream, Cint, (Ptr{Ptr{Void}}, Ptr{Void}))
@libgit(git_branch_set_upstream, Cint, (Ptr{Void}, Ptr{Cchar}))

# ------ libgit odb ------
@libgit(git_odb_exists, Cint, (Ptr{Void}, Ptr{Uint8}))
@libgit(git_odb_free, Void, (Ptr{Void},))
@libgit(git_odb_foreach, Cint, (Ptr{Void}, Ptr{Void}, Ptr{Cint})) 
@libgit(git_odb_add_disk_alternate, Cint, (Ptr{Void}, Ptr{Cchar}))
@libgit(git_odb_hash, Cint, (Ptr{Uint8}, Ptr{Cchar}, Csize_t, Cint))

@libgit(git_odb_write, Cint, 
        (Ptr{Uint8}, Ptr{Void}, Ptr{Uint8}, Csize_t))
@libgit(git_odb_read, Cint,
        (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Uint8}))
@libgit(git_odb_read_header, Cint,
        (Ptr{Csize_t}, Ptr{Cint}, Ptr{Void}, Ptr{Uint8}))
@libgit(git_odb_open_rstream, Cint,
        (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Uint8}))
@libgit(git_odb_open_wstream, Cint,
        (Ptr{Ptr{Void}}, Ptr{Void}, Csize_t, Cint))
@libgit(git_odb_stream_read, Cint,
        (Ptr{Void}, Ptr{Uint8}, Csize_t))
@libgit(git_odb_stream_write, Cint,
        (Ptr{Void}, Ptr{Cchar}, Csize_t)) 
@libgit(git_odb_stream_finalize_write, Cint,
        (Ptr{Uint8}, Ptr{Void}))
@libgit(git_odb_stream_free, Void, (Ptr{Void},))
@libgit(git_odb_object_free, Void, (Ptr{Void},))
@libgit(git_odb_object_id, Ptr{Uint8}, (Ptr{Void},))
@libgit(git_odb_object_data, Ptr{Void}, (Ptr{Void},))
@libgit(git_odb_object_size, Csize_t, (Ptr{Void},))
@libgit(git_odb_object_type, Cint, (Ptr{Void},))

# ------ libgit reference  ------
@libgit(git_reference_is_valid_name, Cint, (Ptr{Cchar},))
@libgit(git_reference_create, Cint, 
        (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Uint8}, 
         Cint, Ptr{GitSignature}, Ptr{Cchar}))
@libgit(git_reference_free, Void, (Ptr{Void},))
@libgit(git_reference_peel, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Cint))
@libgit(git_reference_symbolic_create, Cint,
        (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Uint8}, Cint))
@libgit(git_reference_lookup, Cint, 
        (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}))
@libgit(git_reference_symbolic_target, Ptr{Cchar}, (Ptr{Void},)) 
@libgit(git_reference_set_target, Cint, 
        (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Uint8}))
@libgit(git_reference_resolve, Cint, (Ptr{Ptr{Void}}, Ptr{Void}))
@libgit(git_reference_rename, Cint, 
        (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Cint))
@libgit(git_reference_target, Ptr{Uint8}, (Ptr{Void},))
@libgit(git_reference_delete, Cint, (Ptr{Void},))
@libgit(git_reference_name, Ptr{Cchar}, (Ptr{Void},))
@libgit(git_reference_type, Cint, (Ptr{Void},))
@libgit(git_reference_iterator_new, Cint, (Ptr{Ptr{Void}}, Ptr{Void}))
@libgit(git_reference_iterator_glob_new, Cint, 
        (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}))
@libgit(git_reference_next_name, Cint, (Ptr{Ptr{Void}}, Ptr{Void}))
@libgit(git_reference_next, Cint, (Ptr{Ptr{Void}}, Ptr{Void}))
@libgit(git_reference_iterator_free, Cint, (Ptr{Void},))
@libgit(git_reference_has_log, Cint, (Ptr{Void},))
@libgit(git_reference_owner, Ptr{Void}, (Ptr{Void},))
@libgit(git_reference_shorthand, Ptr{Cchar}, (Ptr{Void},))
@libgit(git_reference_is_remote, Cint, (Ptr{Void},))

@libgit(git_reflog_write, Cint, (Ptr{Void},))
@libgit(git_reflog_read, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}))
@libgit(git_reflog_append, Cint, (Ptr{Void}, Ptr{Void}, Ptr{GitSignature}, Ptr{Cchar}))
@libgit(git_reflog_entrycount, Cint, (Ptr{Void},))
@libgit(git_reflog_entry_byindex, Ptr{Void}, (Ptr{Void}, Csize_t))
@libgit(git_reflog_free, Void, (Ptr{Void},))
@libgit(git_reflog_entry_id_old, Ptr{Uint8}, (Ptr{Void},))
@libgit(git_reflog_entry_id_new, Ptr{Uint8}, (Ptr{Void},))
@libgit(git_reflog_entry_committer, Ptr{GitSignature}, (Ptr{Void},))
@libgit(git_reflog_entry_message, Ptr{Cchar}, (Ptr{Void},))

# ------ libgit checkout  ------
#TODO:...
type GitCheckoutOpts
    version::Cuint
    checkout_strategy::Cuint
    disable_filters::Cint
    dir_mode::Cuint
    file_mode::Cuint
    file_open_flags::Cint
    
    notify_flags::Cuint 
    notify_cb::Ptr{Void}
    notify_payload::Ptr{Void}
    
    progress_cb::Ptr{Void}
    progress_payload::Ptr{Void}

    paths_strings::Ptr{Ptr{Cchar}}
    paths_count::Csize_t

    baseline::Ptr{Void}

    target_directory::Ptr{Cchar}
    our_label::Ptr{Cchar}
    their_label::Ptr{Cchar}

    function GitCheckoutOpts()
        return new(1,
                   0,
                   0,
                   0,
                   0,
                   0,
                   0,
                   C_NULL,
                   C_NULL,
                   C_NULL,
                   C_NULL,
                   C_NULL,
                   0,
                   C_NULL,
                   C_NULL,
                   C_NULL,
                   C_NULL)
    end
end

@libgit(git_checkout_opts_init, Ptr{Void}, ())
@libgit(git_checkout_head, Cint, (Ptr{Void}, Ptr{Void})) 
@libgit(git_checkout_index, Cint, (Ptr{Void}, Ptr{Void}, Ptr{Void}))


# ------ libgit clone------
type GitTransferProgress
    total_objects::Cuint
    indexed_objects::Cuint
    received_objects::Cuint
    local_objects::Cuint
    total_deltas::Cuint
    indexed_deltas::Cuint
    received_bytes::Csize_t
end

immutable IGitStrArray
   strings::Ptr{Ptr{Cchar}}
   count::Csize_t

   IGitStrArray() = new(C_NULL, 0)
end

# git checkout options
immutable IGitCheckoutOptions
    version::Cuint
    
    strategy::Cuint
    
    disable_filters::Cint
    dir_mode::Cuint
    file_mode::Cuint
    file_open_flags::Cint
    
    notify_flags::Cuint 
    notify_cb::Ptr{Void}
    notify_payload::Ptr{Void}
    
    progress_cb::Ptr{Void}
    progress_payload::Ptr{Void}

    paths::IGitStrArray

    baseline::Ptr{Void}

    target_directory::Ptr{Cchar}
    our_label::Ptr{Cchar}
    their_label::Ptr{Cchar}

    IGitCheckoutOptions() = new(1,
                                0,
                                0,
                                0,
                                0,
                                0,
                                0,
                                C_NULL,
                                C_NULL,
                                C_NULL,
                                C_NULL,
                                IGitStrArray(), 
                                C_NULL,
                                C_NULL,
                                C_NULL,
                                C_NULL)
end

immutable IGitRemoteCallbacks
    version::Cuint
    progress_cb::Ptr{Void}
    completion_cb::Ptr{Void}
    credentials_cb::Ptr{Void}
    transfer_progress_cb::Ptr{Void}
    update_tips_cb::Ptr{Void}
    payload::Ptr{Void}

    IGitRemoteCallbacks() = new(1,
                                C_NULL,
                                C_NULL,
                                C_NULL,
                                C_NULL,
                                C_NULL,
                                C_NULL)
end

type GitCloneOptsNew
    version::Cuint
    
    checkout_opts::IGitCheckoutOptions
    pad1::Cuint
    pad2::Cuint
    remote_callbacks::IGitRemoteCallbacks
   
    bare::Cint
    ignore_cert_errors::Cint
    remote_name::Ptr{Cchar}
    checkout_branch::Ptr{Cchar}

    function GitCloneOpts()
        return new(1,
                   IGitCheckoutOptions(),
                   0,0,
                   IGitRemoteCallbacks(),
                   0,
                   0,
                   C_NULL,
                   C_NULL)
                   #convert(Ptr{Cchar}, "a")) useful for testing
    end
end

type GitCloneOpts
    version::Cuint

    pad1::Cuint
     
    # git checkout options
    checkout_version::Cuint
    checkout_strategy::Cuint
    disable_filters::Cint
    dir_mode::Cuint
    file_mode::Cuint
    file_open_flags::Cint
    
    notify_flags::Cuint 
    notify_cb::Ptr{Void}
    notify_payload::Ptr{Void}
    
    progress_cb::Ptr{Void}
    progress_payload::Ptr{Void}

    paths_strings::Ptr{Ptr{Cchar}}
    paths_count::Csize_t

    baseline::Ptr{Void}

    target_directory::Ptr{Cchar}
    our_label::Ptr{Cchar}
    their_label::Ptr{Cchar}

    pad2::Cuint
    pad3::Cuint

    # git remote callback options
    remote_version::Cuint
    remote_progress_cb::Ptr{Void}
    remote_completion_cb::Ptr{Void}
    remote_credentials_cb::Ptr{Void}
    remote_transfer_progress_cb::Ptr{Void}
    remote_update_tips_cb::Ptr{Void}
    remote_payload::Ptr{Void}

    bare::Cint
    ignore_cert_errors::Cint
    remote_name::Ptr{Cchar}
    checkout_branch::Ptr{Cchar}

    function GitCloneOpts()
        return new(1,
                   0, # padding
                   
                   1,
                   0,
                   0,
                   0,
                   0,
                   0,
                   0,
                   C_NULL,
                   C_NULL,
                   C_NULL,
                   C_NULL,
                   C_NULL,
                   0,
                   C_NULL,
                   C_NULL,
                   C_NULL,
                   C_NULL,
                   
                   0, #padding
                   0, #padding

                   1,
                   C_NULL,
                   C_NULL,
                   C_NULL,
                   C_NULL,
                   C_NULL,
                   C_NULL,

                   0,
                   0,
                   C_NULL,
                   C_NULL)
                   #convert(Ptr{Cchar}, "a")) useful for testing
    end
end

@libgit(git_clone_into, Cint, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Cchar}, Ptr{Void}))

# ------ libgit config  ------
type GitConfigEntry
    name::Ptr{Cchar}
    value::Ptr{Cchar}
    level::Cint
end

@libgit(git_config_open_default, Cint, (Ptr{Ptr{Void}},))
@libgit(git_config_open_ondisk,  Cint, (Ptr{Ptr{Void}}, Ptr{Cchar}))
@libgit(git_config_set_string, Cint, (Ptr{Void}, Ptr{Cchar}, Ptr{Cchar}))
@libgit(git_config_get_string, Cint, (Ptr{Ptr{Cchar}}, Ptr{Void}, Ptr{Cchar}))
@libgit(git_config_set_int64,  Cint, (Ptr{Void}, Ptr{Cchar}, Int64))
@libgit(git_config_get_int64,  Cint, (Ptr{Int64}, Ptr{Void}, Ptr{Cchar}))
@libgit(git_config_set_int32,  Cint, (Ptr{Void}, Ptr{Cchar}, Int32))
@libgit(git_config_get_int32,  Cint, (Ptr{Int32}, Ptr{Void}, Ptr{Cchar}))
@libgit(git_config_set_bool,   Cint, (Ptr{Void}, Ptr{Cchar}, Cint))
@libgit(git_config_get_bool,   Cint, (Ptr{Cint}, Ptr{Void}l, Ptr{Cchar}))
@libgit(git_config_delete_entry, Cint, (Ptr{Void}, Ptr{Cchar}))
@libgit(git_config_free, Void, (Ptr{Void},)) 
@libgit(git_config_foreach, Cint, (Ptr{Void}, Ptr{Void}, Ptr{Void})) 

# --- libgit blame ----
type GitBlameOptions
    version::Cuint
    flags::Uint32
    min_match_characters::Uint16

    newest_commit_oid1::Uint8
    newest_commit_oid2::Uint8
    newest_commit_oid3::Uint8
    newest_commit_oid4::Uint8
    newest_commit_oid5::Uint8
    newest_commit_oid6::Uint8
    newest_commit_oid7::Uint8
    newest_commit_oid8::Uint8
    newest_commit_oid9::Uint8
    newest_commit_oid10::Uint8
    newest_commit_oid11::Uint8
    newest_commit_oid12::Uint8
    newest_commit_oid13::Uint8
    newest_commit_oid14::Uint8
    newest_commit_oid15::Uint8
    newest_commit_oid16::Uint8
    newest_commit_oid17::Uint8
    newest_commit_oid18::Uint8
    newest_commit_oid19::Uint8
    newest_commit_oid20::Uint8

    oldest_commit_oid1::Uint8
    oldest_commit_oid2::Uint8
    oldest_commit_oid3::Uint8
    oldest_commit_oid4::Uint8
    oldest_commit_oid5::Uint8
    oldest_commit_oid6::Uint8
    oldest_commit_oid7::Uint8
    oldest_commit_oid8::Uint8
    oldest_commit_oid9::Uint8
    oldest_commit_oid10::Uint8
    oldest_commit_oid11::Uint8
    oldest_commit_oid12::Uint8
    oldest_commit_oid13::Uint8
    oldest_commit_oid14::Uint8
    oldest_commit_oid15::Uint8
    oldest_commit_oid16::Uint8
    oldest_commit_oid17::Uint8
    oldest_commit_oid18::Uint8
    oldest_commit_oid19::Uint8
    oldest_commit_oid20::Uint8

    min_line::Uint32
    max_line::Uint32
end

# --- libgit diff hunk ----
type GitDiffHunk
    old_start::Cint
    old_lines::Cint
    new_start::Cint
    new_lines::Cint
    header_len::Csize_t
    # char header[128] 
    header1::Cchar
    header2::Cchar
    header3::Cchar
    header4::Cchar
    header5::Cchar
    header6::Cchar
    header7::Cchar
    header8::Cchar
    header9::Cchar
    header10::Cchar
    header11::Cchar
    header12::Cchar
    header13::Cchar
    header14::Cchar
    header15::Cchar
    header16::Cchar
    header17::Cchar
    header18::Cchar
    header19::Cchar
    header20::Cchar
    header21::Cchar
    header22::Cchar
    header23::Cchar
    header24::Cchar
    header25::Cchar
    header26::Cchar
    header27::Cchar
    header28::Cchar
    header29::Cchar
    header30::Cchar
    header31::Cchar
    header32::Cchar
    header33::Cchar
    header34::Cchar
    header35::Cchar
    header36::Cchar
    header37::Cchar
    header38::Cchar
    header39::Cchar
    header40::Cchar
    header41::Cchar
    header42::Cchar
    header43::Cchar
    header44::Cchar
    header45::Cchar
    header46::Cchar
    header47::Cchar
    header48::Cchar
    header49::Cchar
    header50::Cchar
    header51::Cchar
    header52::Cchar
    header53::Cchar
    header54::Cchar
    header55::Cchar
    header56::Cchar
    header57::Cchar
    header58::Cchar
    header59::Cchar
    header60::Cchar
    header61::Cchar
    header62::Cchar
    header63::Cchar
    header64::Cchar
    header65::Cchar
    header66::Cchar
    header67::Cchar
    header68::Cchar
    header69::Cchar
    header70::Cchar
    header71::Cchar
    header72::Cchar
    header73::Cchar
    header74::Cchar
    header75::Cchar
    header76::Cchar
    header77::Cchar
    header78::Cchar
    header79::Cchar
    header80::Cchar
    header81::Cchar
    header82::Cchar
    header83::Cchar
    header84::Cchar
    header85::Cchar
    header86::Cchar
    header87::Cchar
    header88::Cchar
    header89::Cchar
    header90::Cchar
    header91::Cchar
    header92::Cchar
    header93::Cchar
    header94::Cchar
    header95::Cchar
    header96::Cchar
    header97::Cchar
    header98::Cchar
    header99::Cchar
    header100::Cchar
    header101::Cchar
    header102::Cchar
    header103::Cchar
    header104::Cchar
    header105::Cchar
    header106::Cchar
    header107::Cchar
    header108::Cchar
    header109::Cchar
    header110::Cchar
    header111::Cchar
    header112::Cchar
    header113::Cchar
    header114::Cchar
    header115::Cchar
    header116::Cchar
    header117::Cchar
    header118::Cchar
    header119::Cchar
    header120::Cchar
    header121::Cchar
    header122::Cchar
    header123::Cchar
    header124::Cchar
    header125::Cchar
    header126::Cchar
    header127::Cchar
    header128::Cchar
end 

@libgit(git_patch_num_hunks, Csize_t, (Ptr{Void},))
@libgit(git_patch_get_hunk, Cint, (Ptr{Ptr{GitDiffHunk}}, Ptr{Csize_t}, Ptr{Void}, Csize_t))

type GitDiffLine
    origin::Cchar
    old_lineno::Cint
    new_lineno::Cint
    num_lines::Cint
    content_len::Csize_t
    content_offset::Coff_t
    content::Ptr{Cchar}
end

@libgit(git_patch_get_line_in_hunk, Cint, 
        (Ptr{Ptr{GitDiffLine}}, Ptr{Void}, Csize_t, Csize_t))


end # module api
