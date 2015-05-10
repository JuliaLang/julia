module GitConst

    const OBJ_ANY    = Cint(-2)
    const OBJ_BAD    = Cint(-1)
    const OBJ_COMMIT = Cint(1)
    const OBJ_TREE   = Cint(2)
    const OBJ_BLOB   = Cint(3)
    const OBJ_TAG    = Cint(4)

    const SORT_NONE = Cint(0)
    const SORT_TOPOLOGICAL = Cint(1) << Cint(0)
    const SORT_TIME = Cint(1) << Cint(1)
    const SORT_REVERSE = Cint(1) << Cint(2)

    const REF_INVALID = Cint(0)
    const REF_OID = Cint(1)
    const REF_SYMBOLIC = Cint(2)
    const REF_LISTALL = REF_OID | REF_SYMBOLIC

    const BRANCH_LOCAL = Cint(1)
    const BRANCH_REMOTE = Cint(2)

    const FILEMODE_NEW             = Cint(00000)
    const FILEMODE_TREE            = Cint(16384)
    const FILEMODE_BLOB            = Cint(33188)
    const FILEMODE_BLOB_EXECUTABLE = Cint(33261)
    const FILEMODE_LINK            = Cint(40960)
    const FILEMODE_COMMIT          = Cint(57344)

    const CHECKOUT_NONE                    = Cint(0)
    const CHECKOUT_SAFE                    = Cuint(1) << Cint(0)
    const CHECKOUT_SAFE_CREATE             = Cuint(1) << Cint(1)
    const CHECKOUT_FORCE                   = Cuint(1) << Cint(2)
    const CHECKOUT_ALLOW_CONFLICTS         = Cuint(1) << Cint(4)
    const CHECKOUT_REMOVE_UNTRACKED        = Cuint(1) << Cint(5)
    const CHECKOUT_REMOVE_IGNORED          = Cuint(1) << Cint(6)
    const CHECKOUT_UPDATE_ONLY             = Cuint(1) << Cint(7)
    const CHECKOUT_DONT_UPDATE_INDEX       = Cuint(1) << Cint(8)
    const CHECKOUT_NO_REFRESH              = Cuint(1) << Cint(9)
    const CHECKOUT_SKIP_UNMERGED           = Cuint(1) << Cint(10)
    const CHECKOUT_USE_OURS                = Cuint(1) << Cint(11)
    const CHECKOUT_USE_THEIRS              = Cuint(1) << Cint(12)
    const CHECKOUT_DISABLE_PATHSPEC_MATCH  = Cuint(1) << Cint(13)
    const CHECKOUT_SKIP_LOCKED_DIRECTORIES = Cuint(1) << Cint(18)
    const CHECKOUT_DONT_OVERWRITE_IGNORED  = Cuint(1) << Cint(19)

    const CHECKOUT_UPDATE_SUBMODULES       = Cuint(1) << Cint(16)
    const CHECKOUT_UPDATE_SUBMODULES_IF_CHANGED = Cuint(1) << Cint(17)

    const CHECKOUT_NOTIFY_NONE      = Cuint(0)
    const CHECKOUT_NOTIFY_CONFLICT  = Cuint(1) << Cint(0)
    const CHECKOUT_NOTIFY_DIRTY     = Cuint(1) << Cint(1)
    const CHECKOUT_NOTIFY_UPDATED   = Cuint(1) << Cint(2)
    const CHECKOUT_NOTIFY_UNTRACKED = Cuint(1) << Cint(3)
    const CHECKOUT_NOTIFY_IGNORED   = Cuint(1) << Cint(4)
    const CHECKOUT_NOTIFY_ALL       = 0x0FFFF

    const SUBMODULE_UPDATE_RESET    = Cint(-1)
    const SUBMODULE_UPDATE_CHECKOUT = Cint(1)
    const SUBMODULE_UPDATE_REBASE   = Cint(2)
    const SUBMODULE_UPDATE_MERGE    = Cint(3)
    const SUBMODULE_UPDATE_NONE     = Cint(4)
    const SUBMODULE_UPDATE_DEFAULT  = Cint(0)

    # git_submodule_ignore_t
    const SUBMODULE_IGNORE_RESET     = Cint(-1)
    const SUBMODULE_IGNORE_NONE      = Cint(1)
    const SUBMODULE_IGNORE_UNTRACKED = Cint(2)
    const SUBMODULE_IGNORE_DIRTY     = Cint(3)
    const SUBMODULE_IGNORE_ALL       = Cint(4)
    const SUBMODULE_IGNORE_DEFAULT   = Cint(0)

    const TREEWALK_PRE  = Cint(0)
    const TREEWALK_POST = Cint(1)

    const GIT_PATH_MAX = Cint(4096)

    const DIFF_OPTIONS_VERSION = Cuint(1)

    const DIFF_NORMAL  = Cuint(0)
    const DIFF_REVERSE = Cuint(1) << Cint(0)
    const DIFF_INCLUDE_IGNORED = Cuint(1) << Cint(1)
    const DIFF_RECURSE_IGNORED_DIRS = Cuint(1) << Cint(2)
    const DIFF_INCLUDE_UNTRACKED = Cuint(1) << Cint(3)
    const DIFF_RECURSE_UNTRACKED_DIRS = Cuint(1) << Cint(4)
    const DIFF_INCLUDE_UNMODIFIED = Cuint(1) << Cint(5)
    const DIFF_INCLUDE_TYPECHANGE = Cuint(1) << Cint(6)
    const DIFF_INCLUDE_TYPECHANGE_TREES = Cuint(1) << Cint(7)
    const DIFF_IGNORE_FILEMODE = Cuint(1) << Cint(8)
    const DIFF_IGNORE_SUBMODULES = Cuint(1) << Cint(9)
    const DIFF_IGNORE_CASE = Cuint(1) << Cint(10)
    const DIFF_DISABLE_PATHSPEC_MATCH = Cuint(1) << Cint(12)
    const DIFF_SKIP_BINARY_CHECK = Cuint(1) << Cint(13)
    const DIFF_ENABLE_FAST_UNTRACKED_DIRS = Cuint(1) << Cint(14)

    const DIFF_FORCE_TEXT = Cuint(1) << Cint(20)
    const DIFF_FORCE_BINARY = Cuint(1) << Cint(21)
    const DIFF_IGNORE_WHITESPACE = Cuint(1) << Cint(22)
    const DIFF_IGNORE_WHITESPACE_CHANGE = Cuint(1) << Cint(23)
    const DIFF_IGNORE_WHITESPACE_EOL = Cuint(1) << Cint(24)
    const DIFF_SHOW_UNTRACKED_CONTENT = Cuint(1) << Cint(25)
    const DIFF_SHOW_UNMODIFIED = Cuint(1) << Cint(26)
    const DIFF_PATIENCE = Cuint(1) << Cint(28)
    const DIFF_MINIMAL = Cuint(1) << Cint(29)

    const DIFF_FLAG_BINARY     = Cuint(1) << Cint(0)
    const DIFF_FLAG_NOT_BINARY = Cuint(1) << Cint(1)
    const DIFF_FLAG_VALID_OID  = Cuint(1) << Cint(2)

    const DIFF_FORMAT_PATCH = Cuint(1)
    const DIFF_FORMAT_PATCH_HEADER = Cuint(2)
    const DIFF_FORMAT_RAW = Cuint(3)
    const DIFF_FORMAT_NAME_ONLY = Cuint(4)
    const DIFF_FORMAT_NAME_STATUS = Cuint(5)

    const DELTA_UNMODIFIED = Cint(0)
    const DELTA_ADDED      = Cint(1)
    const DELTA_DELETED    = Cint(2)
    const DELTA_MODIFIED   = Cint(3)
    const DELTA_RENAMED    = Cint(4)
    const DELTA_COPIED     = Cint(5)
    const DELTA_IGNORED    = Cint(6)
    const DELTA_UNTRACKED  = Cint(7)
    const DELTA_TYPECHANGE = Cint(8)

    const DIFF_LINE_CONTEXT   = Cchar(' ')
    const DIFF_LINE_ADDITION  = Cchar('+')
    const DIFF_LINE_DELETION  = Cchar('-')

    const DIFF_LINE_CONTEXT_EOFNL = Cchar('=')
    const DIFF_LINE_ADD_EOFNL = Cchar('>')
    const DIFF_LINE_DEL_EOFNL = Cchar('<')

    const DIFF_LINE_FILE_HDR  = Cchar('F')
    const DIFF_LINE_HUNK_HDR  = Cchar('H')
    const DIFF_LINE_BINARY    = Cchar('B')

    # index
    const IDXENTRY_NAMEMASK   = (0x0fff)
    const IDXENTRY_STAGEMASK  = (0x3000)
    const IDXENTRY_EXTENDED   = (0x4000)
    const IDXENTRY_VALID      = (0x8000)
    const IDXENTRY_STAGESHIFT = Cint(12)

    const IDXENTRY_UPDATE            = Cint(1) << Cint(0)
    const IDXENTRY_REMOVE            = Cint(1) << Cint(1)
    const IDXENTRY_UPTODATE          = Cint(1) << Cint(2)
    const IDXENTRY_ADDED             = Cint(1) << Cint(3)

    const IDXENTRY_HASHED            = Cint(1) << Cint(4)
    const IDXENTRY_UNHASHED          = Cint(1) << Cint(5)
    const IDXENTRY_WT_REMOVE         = Cint(1) << Cint(6)
    const IDXENTRY_CONFLICTED        = Cint(1) << Cint(7)

    const IDXENTRY_UNPACKED          = Cint(1) << Cint(8)
    const IDXENTRY_NEW_SKIP_WORKTREE = Cint(1) << Cint(9)

    const INDEXCAP_IGNORE_CASE = Cuint(1)
    const INDEXCAP_NO_FILEMODE = Cuint(2)
    const INDEXCAP_NO_SYMLINKS = Cuint(4)
    const INDEXCAP_FROM_OWNER  = ~(Cuint(0))

    const INDEX_ADD_DEFAULT = Cuint(0)
    const INDEX_ADD_FORCE   = Cuint(1) << Cint(0)
    const INDEX_ADD_DISABLE_PATHSPEC_MATCH = Cuint(1) << Cint(1)
    const INDEX_ADD_CHECK_PATHSPEC = Cuint(1) << Cint(2)

    const INDEX_STAGE_ANY = Cint(-1)

    const MERGE_TREE_FIND_RENAMES = Cint(1) << Cint(0)

    const MERGE_AUTOMERGE_NORMAL  = Cint(0)
    const MERGE_AUTOMERGE_FAVOR_OURS = Cint(1)
    const MERGE_AUTOMERGE_FAVOR_THEIRS = Cint(2)
    const MERGE_AUTOMERGE_FAVOR_UNION  = Cint(3)

    const MERGE_NO_FASTFORWARD = Cint(1)
    const MERGE_FASTFORWARD_ONLY = Cint(2)

    const GIT_MERGE_ANALYSIS_NONE = 0,
    const GIT_MERGE_ANALYSIS_NORMAL = (1 << 0)
    const GIT_MERGE_ANALYSIS_UP_TO_DATE = (1 << 1)
    const GIT_MERGE_ANALYSIS_FASTFORWARD = (1 << 2)
    const GIT_MERGE_ANALYSIS_UNBORN = (1 << 3)

    const GIT_MERGE_PREFERENCE_NONE = 0
    const GIT_MERGE_PREFERENCE_NO_FASTFORWARD = (1 << 0)
    const GIT_MERGE_PREFERENCE_FASTFORWARD_ONLY = (1 << 1)

    const DIRECTION_FETCH = Cint(0)
    const DIRECTION_PUSH  = Cint(1)

    const BLAME_NORMAL = Cint(0)

    const CREDTYPE_USERPASS_PLAINTEXT = Cuint(1) << Cint(0)
    const CREDTYPE_SSH_KEY = Cuint(1) << Cint(1)
    const CREDTYPE_SSH_CUSTOM = Cuint(1) << Cint(2)
    const CREDTYPE_DEFAULT = Cuint(1) << Cint(3)

    const GIT_REPOSITORY_STATE_NONE = Cint(0)
    const GIT_REPOSITORY_STATE_MERGE = Cint(1)
    const GIT_REPOSITORY_STATE_REVERT = Cint(2)
    const GIT_REPOSITORY_STATE_CHERRY_PICK = Cint(3)
    const GIT_REPOSITORY_STATE_BISECT = Cint(4)
    const GIT_REPOSITORY_STATE_REBASE = Cint(5)
    const GIT_REPOSITORY_STATE_REBASE_INTERACTIVE = Cint(6)
    const GIT_REPOSITORY_STATE_REBASE_MERGE = Cint(7)
    const GIT_REPOSITORY_STATE_APPLY_MAILBOX = Cint(8)
    const GIT_REPOSITORY_STATE_APPLY_MAILBOX_OR_REBASE = Cint(9)
end