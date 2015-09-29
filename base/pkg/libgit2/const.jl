# This file is a part of Julia. License is MIT: http://julialang.org/license

module GitConst

    const HEAD_FILE  = "HEAD"
    const REMOTE_ORIGIN = "origin"

    # objs
    const OBJ_ANY    = Cint(-2)
    const OBJ_BAD    = Cint(-1)
    const OBJ_COMMIT = Cint(1)
    const OBJ_TREE   = Cint(2)
    const OBJ_BLOB   = Cint(3)
    const OBJ_TAG    = Cint(4)

    #revwalk
    const SORT_NONE        = Cint(0)
    const SORT_TOPOLOGICAL = Cint(1 << 0)
    const SORT_TIME        = Cint(1 << 1)
    const SORT_REVERSE     = Cint(1 << 2)

    # refs
    const REF_INVALID  = Cint(0)
    const REF_OID      = Cint(1)
    const REF_SYMBOLIC = Cint(2)
    const REF_LISTALL  = REF_OID | REF_SYMBOLIC

    # branch
    const BRANCH_LOCAL  = Cint(1)
    const BRANCH_REMOTE = Cint(2)

    # file
    const FILEMODE_NEW             = Cint(00000)
    const FILEMODE_TREE            = Cint(16384)
    const FILEMODE_BLOB            = Cint(33188)
    const FILEMODE_BLOB_EXECUTABLE = Cint(33261)
    const FILEMODE_LINK            = Cint(40960)
    const FILEMODE_COMMIT          = Cint(57344)

    # checkout
    const CHECKOUT_NONE                    = Cuint(0)
    const CHECKOUT_SAFE                    = Cuint(1 << 0)
    const CHECKOUT_SAFE_CREATE             = Cuint(1 << 1)
    const CHECKOUT_FORCE                   = Cuint(1 << 2)
    const CHECKOUT_ALLOW_CONFLICTS         = Cuint(1 << 4)
    const CHECKOUT_REMOVE_UNTRACKED        = Cuint(1 << 5)
    const CHECKOUT_REMOVE_IGNORED          = Cuint(1 << 6)
    const CHECKOUT_UPDATE_ONLY             = Cuint(1 << 7)
    const CHECKOUT_DONT_UPDATE_INDEX       = Cuint(1 << 8)
    const CHECKOUT_NO_REFRESH              = Cuint(1 << 9)
    const CHECKOUT_SKIP_UNMERGED           = Cuint(1 << 10)
    const CHECKOUT_USE_OURS                = Cuint(1 << 11)
    const CHECKOUT_USE_THEIRS              = Cuint(1 << 12)
    const CHECKOUT_DISABLE_PATHSPEC_MATCH  = Cuint(1 << 13)
    const CHECKOUT_SKIP_LOCKED_DIRECTORIES = Cuint(1 << 18)
    const CHECKOUT_DONT_OVERWRITE_IGNORED  = Cuint(1 << 19)

    const CHECKOUT_UPDATE_SUBMODULES       = Cuint(1 << 16)
    const CHECKOUT_UPDATE_SUBMODULES_IF_CHANGED = Cuint(1 << 17)

    const CHECKOUT_NOTIFY_NONE      = Cuint(0)
    const CHECKOUT_NOTIFY_CONFLICT  = Cuint(1 << 0)
    const CHECKOUT_NOTIFY_DIRTY     = Cuint(1 << 1)
    const CHECKOUT_NOTIFY_UPDATED   = Cuint(1 << 2)
    const CHECKOUT_NOTIFY_UNTRACKED = Cuint(1 << 3)
    const CHECKOUT_NOTIFY_IGNORED   = Cuint(1 << 4)
    const CHECKOUT_NOTIFY_ALL       = 0x0FFFF

    # diff
    const DIFF_OPTIONS_VERSION = Cuint(1)

    const DIFF_NORMAL                     = Cuint(0)
    const DIFF_REVERSE                    = Cuint(1 << 0)
    const DIFF_INCLUDE_IGNORED            = Cuint(1 << 1)
    const DIFF_RECURSE_IGNORED_DIRS       = Cuint(1 << 2)
    const DIFF_INCLUDE_UNTRACKED          = Cuint(1 << 3)
    const DIFF_RECURSE_UNTRACKED_DIRS     = Cuint(1 << 4)
    const DIFF_INCLUDE_UNMODIFIED         = Cuint(1 << 5)
    const DIFF_INCLUDE_TYPECHANGE         = Cuint(1 << 6)
    const DIFF_INCLUDE_TYPECHANGE_TREES   = Cuint(1 << 7)
    const DIFF_IGNORE_FILEMODE            = Cuint(1 << 8)
    const DIFF_IGNORE_SUBMODULES          = Cuint(1 << 9)
    const DIFF_IGNORE_CASE                = Cuint(1 << 10)
    const DIFF_DISABLE_PATHSPEC_MATCH     = Cuint(1 << 12)
    const DIFF_SKIP_BINARY_CHECK          = Cuint(1 << 13)
    const DIFF_ENABLE_FAST_UNTRACKED_DIRS = Cuint(1 << 14)

    const DIFF_FORCE_TEXT               = Cuint(1 << 20)
    const DIFF_FORCE_BINARY             = Cuint(1 << 21)
    const DIFF_IGNORE_WHITESPACE        = Cuint(1 << 22)
    const DIFF_IGNORE_WHITESPACE_CHANGE = Cuint(1 << 23)
    const DIFF_IGNORE_WHITESPACE_EOL    = Cuint(1 << 24)
    const DIFF_SHOW_UNTRACKED_CONTENT   = Cuint(1 << 25)
    const DIFF_SHOW_UNMODIFIED          = Cuint(1 << 26)
    const DIFF_PATIENCE                 = Cuint(1 << 28)
    const DIFF_MINIMAL                  = Cuint(1 << 29)

    const DIFF_FLAG_BINARY     = Cuint(1 << 0)
    const DIFF_FLAG_NOT_BINARY = Cuint(1 << 1)
    const DIFF_FLAG_VALID_OID  = Cuint(1 << 2)

    const DIFF_FORMAT_PATCH        = Cuint(1)
    const DIFF_FORMAT_PATCH_HEADER = Cuint(2)
    const DIFF_FORMAT_RAW          = Cuint(3)
    const DIFF_FORMAT_NAME_ONLY    = Cuint(4)
    const DIFF_FORMAT_NAME_STATUS  = Cuint(5)

    const DELTA_UNMODIFIED = Cint(0)
    const DELTA_ADDED      = Cint(1)
    const DELTA_DELETED    = Cint(2)
    const DELTA_MODIFIED   = Cint(3)
    const DELTA_RENAMED    = Cint(4)
    const DELTA_COPIED     = Cint(5)
    const DELTA_IGNORED    = Cint(6)
    const DELTA_UNTRACKED  = Cint(7)
    const DELTA_TYPECHANGE = Cint(8)

    # index
    const IDXENTRY_NAMEMASK   = (0x0fff)
    const IDXENTRY_STAGEMASK  = (0x3000)
    const IDXENTRY_EXTENDED   = (0x4000)
    const IDXENTRY_VALID      = (0x8000)
    const IDXENTRY_STAGESHIFT = Cint(12)

    const IDXENTRY_UPDATE            = Cint(1 << 0)
    const IDXENTRY_REMOVE            = Cint(1 << 1)
    const IDXENTRY_UPTODATE          = Cint(1 << 2)
    const IDXENTRY_ADDED             = Cint(1 << 3)

    const IDXENTRY_HASHED            = Cint(1 << 4)
    const IDXENTRY_UNHASHED          = Cint(1 << 5)
    const IDXENTRY_WT_REMOVE         = Cint(1 << 6)
    const IDXENTRY_CONFLICTED        = Cint(1 << 7)

    const IDXENTRY_UNPACKED          = Cint(1 << 8)
    const IDXENTRY_NEW_SKIP_WORKTREE = Cint(1 << 9)

    const INDEXCAP_IGNORE_CASE = Cuint(1)
    const INDEXCAP_NO_FILEMODE = Cuint(2)
    const INDEXCAP_NO_SYMLINKS = Cuint(4)
    const INDEXCAP_FROM_OWNER  = ~(Cuint(0))

    const INDEX_ADD_DEFAULT                = Cuint(0)
    const INDEX_ADD_FORCE                  = Cuint(1 << 0)
    const INDEX_ADD_DISABLE_PATHSPEC_MATCH = Cuint(1 << 1)
    const INDEX_ADD_CHECK_PATHSPEC         = Cuint(1 << 2)

    const INDEX_STAGE_ANY = Cint(-1)

    # merge
    const MERGE_TREE_FIND_RENAMES = Cint(1 << 0)

    const MERGE_FILE_FAVOR_NORMAL = Cint(0)
    const MERGE_FILE_FAVOR_OURS   = Cint(1)
    const MERGE_FILE_FAVOR_THEIRS = Cint(2)
    const MERGE_FILE_FAVOR_UNION  = Cint(3)

    const MERGE_AUTOMERGE_NORMAL       = Cint(0)
    const MERGE_AUTOMERGE_FAVOR_OURS   = Cint(1)
    const MERGE_AUTOMERGE_FAVOR_THEIRS = Cint(2)
    const MERGE_AUTOMERGE_FAVOR_UNION  = Cint(3)

    const MERGE_NO_FASTFORWARD   = Cint(1)
    const MERGE_FASTFORWARD_ONLY = Cint(2)

    const MERGE_ANALYSIS_NONE        = Cint(0)
    const MERGE_ANALYSIS_NORMAL      = Cint(1 << 0)
    const MERGE_ANALYSIS_UP_TO_DATE  = Cint(1 << 1)
    const MERGE_ANALYSIS_FASTFORWARD = Cint(1 << 2)
    const MERGE_ANALYSIS_UNBORN      = Cint(1 << 3)

    const MERGE_PREFERENCE_NONE             = Cint(0)
    const MERGE_PREFERENCE_NO_FASTFORWARD   = Cint(1 << 0)
    const MERGE_PREFERENCE_FASTFORWARD_ONLY = Cint(1 << 1)

    # reset
    const RESET_SOFT  = Cint(1) # Move the head to the given commit
    const RESET_MIXED = Cint(2) # SOFT plus reset index to the commit
    const RESET_HARD  = Cint(3) # MIXED plus changes in working tree discarded

    #rebase
    const REBASE_OPERATION_PICK   = Cint(0)
    const REBASE_OPERATION_REWORD = Cint(1)
    const REBASE_OPERATION_EDIT   = Cint(2)
    const REBASE_OPERATION_SQUASH = Cint(3)
    const REBASE_OPERATION_FIXUP  = Cint(4)
    const REBASE_OPERATION_EXEC   = Cint(5)

    # credentials
    const CREDTYPE_USERPASS_PLAINTEXT = Cuint(1 << 0)
    const CREDTYPE_SSH_KEY            = Cuint(1 << 1)
    const CREDTYPE_SSH_CUSTOM         = Cuint(1 << 2)
    const CREDTYPE_DEFAULT            = Cuint(1 << 3)
end