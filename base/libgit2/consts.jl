# This file is a part of Julia. License is MIT: http://julialang.org/license

module Consts

    const HEAD_FILE  = "HEAD"
    const FETCH_HEAD  = "FETCH_HEAD"
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

    # checkout
    @enumset(CHECKOUT,
             CHECKOUT_SAFE                    = 1 << 0,
             CHECKOUT_FORCE                   = 1 << 1,
             CHECKOUT_RECREATE_MISSING        = 1 << 2,
             CHECKOUT_ALLOW_CONFLICTS         = 1 << 4,
             CHECKOUT_REMOVE_UNTRACKED        = 1 << 5,
             CHECKOUT_REMOVE_IGNORED          = 1 << 6,
             CHECKOUT_UPDATE_ONLY             = 1 << 7,
             CHECKOUT_DONT_UPDATE_INDEX       = 1 << 8,
             CHECKOUT_NO_REFRESH              = 1 << 9,
             CHECKOUT_SKIP_UNMERGED           = 1 << 10,
             CHECKOUT_USE_OURS                = 1 << 11,
             CHECKOUT_USE_THEIRS              = 1 << 12,
             CHECKOUT_DISABLE_PATHSPEC_MATCH  = 1 << 13,
             CHECKOUT_SKIP_LOCKED_DIRECTORIES = 1 << 18,
             CHECKOUT_DONT_OVERWRITE_IGNORED  = 1 << 19,
             CHECKOUT_CONFLICT_STYLE_MERGE    = 1 << 20,
             CHECKOUT_CONFLICT_STYLE_DIFF3    = 1 << 21,
             CHECKOUT_DONT_REMOVE_EXISTING    = 1 << 22)
    const CHECKOUT_NONE = CHECKOUT()

    const CHECKOUT_UPDATE_SUBMODULES       = Cuint(1 << 16)
    const CHECKOUT_UPDATE_SUBMODULES_IF_CHANGED = Cuint(1 << 17)

    @enumset(CHECKOUT_NOTIFY,
             CHECKOUT_NOTIFY_CONFLICT  = 1 << 0,
             CHECKOUT_NOTIFY_DIRTY     = 1 << 1,
             CHECKOUT_NOTIFY_UPDATED   = 1 << 2,
             CHECKOUT_NOTIFY_UNTRACKED = 1 << 3,
             CHECKOUT_NOTIFY_IGNORED   = 1 << 4)
    const CHECKOUT_NOTIFY_NONE      = CHECKOUT_NOTIFY()
    const CHECKOUT_NOTIFY_ALL       = typemax(CHECKOUT_NOTIFY)

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
    const INDEXCAP_FROM_OWNER  = ~Cuint(0)

    const INDEX_ADD_DEFAULT                = Cuint(0)
    const INDEX_ADD_FORCE                  = Cuint(1 << 0)
    const INDEX_ADD_DISABLE_PATHSPEC_MATCH = Cuint(1 << 1)
    const INDEX_ADD_CHECK_PATHSPEC         = Cuint(1 << 2)

    const INDEX_STAGE_ANY = Cint(-1)

    # merge
    @enum(GIT_MERGE, MERGE_FIND_RENAMES     = 1 << 0,
                     MERGE_FAIL_ON_CONFLICT = 1 << 1,
                     MERGE_SKIP_REUC        = 1 << 2,
                     MERGE_NO_RECURSIVE     = 1 << 3)

    @enum(GIT_MERGE_FILE, MERGE_FILE_DEFAULT                  = 0,       # Defaults
                          MERGE_FILE_STYLE_MERGE              = 1 << 0,  # Create standard conflicted merge files
                          MERGE_FILE_STYLE_DIFF3              = 1 << 1,  # Create diff3-style files
                          MERGE_FILE_SIMPLIFY_ALNUM           = 1 << 2,  # Condense non-alphanumeric regions for simplified diff file
                          MERGE_FILE_IGNORE_WHITESPACE        = 1 << 3,  # Ignore all whitespace
                          MERGE_FILE_IGNORE_WHITESPACE_CHANGE = 1 << 4,  # Ignore changes in amount of whitespace
                          MERGE_FILE_IGNORE_WHITESPACE_EOL    = 1 << 5,  # Ignore whitespace at end of line
                          MERGE_FILE_DIFF_PATIENCE            = 1 << 6,  # Use the "patience diff" algorithm
                          MERGE_FILE_DIFF_MINIMAL             = 1 << 7)  # Take extra time to find minimal diff

    @enum(GIT_MERGE_FILE_FAVOR, MERGE_FILE_FAVOR_NORMAL = 0,
                                MERGE_FILE_FAVOR_OURS   = 1,
                                MERGE_FILE_FAVOR_THEIRS = 2,
                                MERGE_FILE_FAVOR_UNION  = 3)

    @enum(GIT_MERGE_PREFERENCE, MERGE_PREFERENCE_NONE             = 0,
                                MERGE_PREFERENCE_NO_FASTFORWARD   = 1,
                                MERGE_PREFERENCE_FASTFORWARD_ONLY = 2)

    @enum(GIT_MERGE_ANALYSIS, MERGE_ANALYSIS_NONE        = 0,
                              MERGE_ANALYSIS_NORMAL      = 1 << 0,
                              MERGE_ANALYSIS_UP_TO_DATE  = 1 << 1,
                              MERGE_ANALYSIS_FASTFORWARD = 1 << 2,
                              MERGE_ANALYSIS_UNBORN      = 1 << 3)

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

    # fetch_prune
    const FETCH_PRUNE_UNSPECIFIED = Cint(0)
    const FETCH_PRUNE             = Cint(1)
    const FETCH_NO_PRUNE          = Cint(2)

    # remote_autotag
    const REMOTE_DOWNLOAD_TAGS_UNSPECIFIED = Cint(0)
    const REMOTE_DOWNLOAD_TAGS_AUTO        = Cint(1)
    const REMOTE_DOWNLOAD_TAGS_NONE        = Cint(2)
    const REMOTE_DOWNLOAD_TAGS_ALL         = Cint(3)

    # clone
    const CLONE_LOCAL_AUTO     = Cint(0)
    const CLONE_LOCAL          = Cint(1)
    const CLONE_NO_LOCAL       = Cint(2)
    const CLONE_LOCAL_NO_LINKS = Cint(3)

    # status
    const STATUS_CURRENT          = Cuint(0)
    const STATUS_INDEX_NEW        = Cuint(1 << 0)
    const STATUS_INDEX_MODIFIED   = Cuint(1 << 1)
    const STATUS_INDEX_DELETED    = Cuint(1 << 2)
    const STATUS_INDEX_RENAMED    = Cuint(1 << 3)
    const STATUS_INDEX_TYPECHANGE = Cuint(1 << 4)
    const STATUS_WT_NEW           = Cuint(1 << 7)
    const STATUS_WT_MODIFIED      = Cuint(1 << 8)
    const STATUS_WT_DELETED       = Cuint(1 << 9)
    const STATUS_WT_TYPECHANGE    = Cuint(1 << 10)
    const STATUS_WT_RENAMED       = Cuint(1 << 11)
    const STATUS_WT_UNREADABLE    = Cuint(1 << 12)
    const STATUS_IGNORED          = Cuint(1 << 14)
    const STATUS_CONFLICTED       = Cuint(1 << 15)

    # status show
    const STATUS_SHOW_INDEX_AND_WORKDIR = Cint(0)
    const STATUS_SHOW_INDEX_ONLY        = Cint(1)
    const STATUS_SHOW_WORKDIR_ONLY      = Cint(2)

    # status options
    @enumset(STATUS_OPT,
             STATUS_OPT_INCLUDE_UNTRACKED                = 1 << 0,
             STATUS_OPT_INCLUDE_IGNORED                  = 1 << 1,
             STATUS_OPT_INCLUDE_UNMODIFIED               = 1 << 2,
             STATUS_OPT_EXCLUDE_SUBMODULES               = 1 << 3,
             STATUS_OPT_RECURSE_UNTRACKED_DIRS           = 1 << 4,
             STATUS_OPT_DISABLE_PATHSPEC_MATCH           = 1 << 5,
             STATUS_OPT_RECURSE_IGNORED_DIRS             = 1 << 6,
             STATUS_OPT_RENAMES_HEAD_TO_INDEX            = 1 << 7,
             STATUS_OPT_RENAMES_INDEX_TO_WORKDIR         = 1 << 8,
             STATUS_OPT_SORT_CASE_SENSITIVELY            = 1 << 9,
             STATUS_OPT_SORT_CASE_INSENSITIVELY          = 1 << 10,
             STATUS_OPT_RENAMES_FROM_REWRITES            = 1 << 11,
             STATUS_OPT_NO_REFRESH                       = 1 << 12,
             STATUS_OPT_UPDATE_INDEX                     = 1 << 13,
             STATUS_OPT_INCLUDE_UNREADABLE               = 1 << 14,
             STATUS_OPT_INCLUDE_UNREADABLE_AS_UNTRACKED  = 1 << 15)

    @enum(GIT_SUBMODULE_IGNORE, SUBMODULE_IGNORE_UNSPECIFIED  = -1, # use the submodule's configuration
                                SUBMODULE_IGNORE_NONE         = 1,  # any change or untracked == dirty
                                SUBMODULE_IGNORE_UNTRACKED    = 2,  # dirty if tracked files change
                                SUBMODULE_IGNORE_DIRTY        = 3,  # only dirty if HEAD moved
                                SUBMODULE_IGNORE_ALL          = 4)  # never dirty

    """
Option flags for `GitRepo`.

* `REPOSITORY_OPEN_NO_SEARCH` - Only open the repository if it can be immediately found in the `path`.  Do not walk up from the `path` looking at parent directories.
* `REPOSITORY_OPEN_CROSS_FS` - Unless this flag is set, open will not continue searching across filesystem boundaries. (E.g. Searching in a user's home directory `/home/user/source/` will not return `/.git/` as the found repo if `/` is a different filesystem than `/home`.)
* `REPOSITORY_OPEN_BARE` - Open repository as a bare repo regardless of core.bare config, and defer loading config file for faster setup.
    """
    @enum(GIT_REPOSITORY_OPEN, REPOSITORY_OPEN_DEFAULT   = 0,
                               REPOSITORY_OPEN_NO_SEARCH = 1<<0,
                               REPOSITORY_OPEN_CROSS_FS  = 1<<1,
                               REPOSITORY_OPEN_BARE      = 1<<2)

    @enum(GIT_BRANCH, BRANCH_LOCAL = 1, BRANCH_REMOTE = 2)

    @enum(GIT_FILEMODE, FILEMODE_UNREADABLE          = 0o000000,
                        FILEMODE_TREE                = 0o040000,
                        FILEMODE_BLOB                = 0o100644,
                        FILEMODE_BLOB_EXECUTABLE     = 0o100755,
                        FILEMODE_LINK                = 0o120000,
                        FILEMODE_COMMIT              = 0o160000)

    @enum(GIT_CREDTYPE, CREDTYPE_USERPASS_PLAINTEXT = Cuint(1 << 0),
                        CREDTYPE_SSH_KEY            = Cuint(1 << 1),
                        CREDTYPE_SSH_CUSTOM         = Cuint(1 << 2),
                        CREDTYPE_DEFAULT            = Cuint(1 << 3),
                        CREDTYPE_SSH_INTERACTIVE    = Cuint(1 << 4),
                        CREDTYPE_USERNAME           = Cuint(1 << 5),
                        CREDTYPE_SSH_MEMORY         = Cuint(1 << 6))

    @enum(GIT_FEATURE, FEATURE_THREADS = Cuint(1 << 0),
                       FEATURE_HTTPS   = Cuint(1 << 1),
                       FEATURE_SSH     = Cuint(1 << 2),
                       FEATURE_NSEC    = Cuint(1 << 3))

if LibGit2.version() >= v"0.24.0"
    """
Priority level of a config file.

These priority levels correspond to the natural escalation logic (from higher to lower) when searching for config entries in git.

* `CONFIG_LEVEL_DEFAULT` - Open the global, XDG and system configuration files if any available.
* `CONFIG_LEVEL_PROGRAMDATA` - System-wide on Windows, for compatibility with portable git
* `CONFIG_LEVEL_SYSTEM` - System-wide configuration file; `/etc/gitconfig` on Linux systems
* `CONFIG_LEVEL_XDG` - XDG compatible configuration file; typically `~/.config/git/config`
* `CONFIG_LEVEL_GLOBAL` - User-specific configuration file (also called Global configuration file); typically `~/.gitconfig`
* `CONFIG_LEVEL_LOCAL` - Repository specific configuration file; `\$WORK_DIR/.git/config` on non-bare repos
* `CONFIG_LEVEL_APP` - Application specific configuration file; freely defined by applications
* `CONFIG_HIGHEST_LEVEL` - Represents the highest level available config file (i.e. the most specific config file available that actually is loaded)
    """
    @enum(GIT_CONFIG, CONFIG_LEVEL_DEFAULT     = 0,
                      CONFIG_LEVEL_PROGRAMDATA = 1,
                      CONFIG_LEVEL_SYSTEM      = 2,
                      CONFIG_LEVEL_XDG         = 3,
                      CONFIG_LEVEL_GLOBAL      = 4,
                      CONFIG_LEVEL_LOCAL       = 5,
                      CONFIG_LEVEL_APP         = 6,
                      CONFIG_HIGHEST_LEVEL     =-1)
else
    """
Priority level of a config file.

These priority levels correspond to the natural escalation logic (from higher to lower) when searching for config entries in git.

* `CONFIG_LEVEL_DEFAULT` - Open the global, XDG and system configuration files if any available.
* `CONFIG_LEVEL_SYSTEM` - System-wide configuration file; `/etc/gitconfig` on Linux systems
* `CONFIG_LEVEL_XDG` - XDG compatible configuration file; typically `~/.config/git/config`
* `CONFIG_LEVEL_GLOBAL` - User-specific configuration file (also called Global configuration file); typically `~/.gitconfig`
* `CONFIG_LEVEL_LOCAL` - Repository specific configuration file; `\$WORK_DIR/.git/config` on non-bare repos
* `CONFIG_LEVEL_APP` - Application specific configuration file; freely defined by applications
* `CONFIG_HIGHEST_LEVEL` - Represents the highest level available config file (i.e. the most specific config file available that actually is loaded)
    """
    @enum(GIT_CONFIG, CONFIG_LEVEL_DEFAULT     = 0,
                      CONFIG_LEVEL_SYSTEM      = 1,
                      CONFIG_LEVEL_XDG         = 2,
                      CONFIG_LEVEL_GLOBAL      = 3,
                      CONFIG_LEVEL_LOCAL       = 4,
                      CONFIG_LEVEL_APP         = 5,
                      CONFIG_HIGHEST_LEVEL     =-1)
end

    """
Global library options.

These are used to select which global option to set or get and are used in `git_libgit2_opts()`.
    """
    @enum(GIT_OPT,  GET_MWINDOW_SIZE         = 0,
                    SET_MWINDOW_SIZE         = 1,
                    GET_MWINDOW_MAPPED_LIMIT = 2,
                    SET_MWINDOW_MAPPED_LIMIT = 3,
                    GET_SEARCH_PATH          = 4,
                    SET_SEARCH_PATH          = 5,
                    SET_CACHE_OBJECT_LIMIT   = 6,
                    SET_CACHE_MAX_SIZE       = 7,
                    ENABLE_CACHING           = 8,
                    GET_CACHED_MEMORY        = 9,
                    GET_TEMPLATE_PATH        = 10,
                    SET_TEMPLATE_PATH        = 11,
                    SET_SSL_CERT_LOCATIONS   = 12)

end
