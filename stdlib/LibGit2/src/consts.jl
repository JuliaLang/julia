# This file is a part of Julia. License is MIT: https://julialang.org/license

module Consts

import ..LibGit2: version, ensure_initialized

const HEAD_FILE  = "HEAD"
const FETCH_HEAD  = "FETCH_HEAD"
const REMOTE_ORIGIN = "origin"

# objs
@enum(OBJECT,
      OBJ_ANY    = -2,
      OBJ_BAD    = -1,
      OBJ_COMMIT = 1,
      OBJ_TREE   = 2,
      OBJ_BLOB   = 3,
      OBJ_TAG    = 4)

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

# blame
const BLAME_NORMAL                          = Cuint(0)
const BLAME_TRACK_COPIES_SAME_FILE          = Cuint(1 << 0)
const BLAME_TRACK_COPIES_SAME_COMMIT_MOVES  = Cuint(1 << 1)
const BLAME_TRACK_COPIES_SAME_COMMIT_COPIES = Cuint(1 << 2)
const BLAME_TRACK_COPIES_ANY_COMMIT_COPIES  = Cuint(1 << 3)
const BLAME_FIRST_PARENT                    = Cuint(1 << 4)

# checkout
const CHECKOUT_NONE                    = Cuint(0)
const CHECKOUT_SAFE                    = Cuint(1 << 0)
const CHECKOUT_FORCE                   = Cuint(1 << 1)
const CHECKOUT_RECREATE_MISSING        = Cuint(1 << 2)
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
const CHECKOUT_CONFLICT_STYLE_MERGE    = Cuint(1 << 20)
const CHECKOUT_CONFLICT_STYLE_DIFF3    = Cuint(1 << 21)
const CHECKOUT_DONT_REMOVE_EXISTING    = Cuint(1 << 22)

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

@enum(DELTA_STATUS, DELTA_UNMODIFIED = Cint(0),
                    DELTA_ADDED      = Cint(1),
                    DELTA_DELETED    = Cint(2),
                    DELTA_MODIFIED   = Cint(3),
                    DELTA_RENAMED    = Cint(4),
                    DELTA_COPIED     = Cint(5),
                    DELTA_IGNORED    = Cint(6),
                    DELTA_UNTRACKED  = Cint(7),
                    DELTA_TYPECHANGE = Cint(8))

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
""" Option flags for git merge.
* `MERGE_FIND_RENAMES`: detect if a file has been renamed between the common
  ancestor and the "ours" or "theirs" side of the merge. Allows merges where
  a file has been renamed.
* `MERGE_FAIL_ON_CONFLICT`: exit immediately if a conflict is found rather
  than trying to resolve it.
* `MERGE_SKIP_REUC`: do not write the REUC extension on the index resulting
  from the merge.
* `MERGE_NO_RECURSIVE`: if the commits being merged have multiple merge bases,
  use the first one, rather than trying to recursively merge the bases.
"""
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
""" Option flags for git merge file favoritism.
  * `MERGE_FILE_FAVOR_NORMAL`: if both sides of the merge have changes to a section,
    make a note of the conflict in the index which `git checkout` will use to create
    a merge file, which the user can then reference to resolve the conflicts. This is
    the default.
  * `MERGE_FILE_FAVOR_OURS`: if both sides of the merge have changes to a section,
    use the version in the "ours" side of the merge in the index.
  * `MERGE_FILE_FAVOR_THEIRS`: if both sides of the merge have changes to a section,
    use the version in the "theirs" side of the merge in the index.
  * `MERGE_FILE_FAVOR_UNION`: if both sides of the merge have changes to a section,
    include each unique line from both sides in the file which is put into the index.
"""
@enum(GIT_MERGE_FILE_FAVOR, MERGE_FILE_FAVOR_NORMAL = 0,
                            MERGE_FILE_FAVOR_OURS   = 1,
                            MERGE_FILE_FAVOR_THEIRS = 2,
                            MERGE_FILE_FAVOR_UNION  = 3)
""" The user's instructions for how to perform a possible merge.
* `MERGE_PREFERENCE_NONE`: the user has no preference.
* `MERGE_PREFERENCE_NO_FASTFORWARD`: do not allow any fast-forward merges.
* `MERGE_PREFERENCE_FASTFORWARD_ONLY`: allow only fast-forward merges and no
  other type (which may introduce conflicts).
"""
@enum(GIT_MERGE_PREFERENCE, MERGE_PREFERENCE_NONE             = 0,
                            MERGE_PREFERENCE_NO_FASTFORWARD   = 1,
                            MERGE_PREFERENCE_FASTFORWARD_ONLY = 2)
""" Result of analysis on merge possibilities.
* `MERGE_ANALYSIS_NONE`: it is not possible to merge the elements of the input commits.
* `MERGE_ANALYSIS_NORMAL`: a regular merge, when HEAD and the commits that the
  user wishes to merge have all diverged from a common ancestor. In this case the
  changes have to be resolved and conflicts may occur.
* `MERGE_ANALYSIS_UP_TO_DATE`: all the input commits the user wishes to merge can
  be reached from HEAD, so no merge needs to be performed.
* `MERGE_ANALYSIS_FASTFORWARD`: the input commit is a descendant of HEAD and so no
  merge needs to be performed - instead, the user can simply checkout the
  input commit(s).
* `MERGE_ANALYSIS_UNBORN`: the HEAD of the repository refers to a commit which does not
  exist. It is not possible to merge, but it may be possible to checkout the input
  commits.
"""
@enum(GIT_MERGE_ANALYSIS, MERGE_ANALYSIS_NONE        = 0,
                          MERGE_ANALYSIS_NORMAL      = 1 << 0,
                          MERGE_ANALYSIS_UP_TO_DATE  = 1 << 1,
                          MERGE_ANALYSIS_FASTFORWARD = 1 << 2,
                          MERGE_ANALYSIS_UNBORN      = 1 << 3)

# reset
const RESET_SOFT  = Cint(1) # Move the head to the given commit
const RESET_MIXED = Cint(2) # SOFT plus reset index to the commit
const RESET_HARD  = Cint(3) # MIXED plus changes in working tree discarded

# rebase
""" Options for what rebase operation is currently being performed on a commit.
* `REBASE_OPERATION_PICK`: cherry-pick the commit in question.
* `REBASE_OPERATION_REWORD`: cherry-pick the commit in question, but rewrite its
  message using the prompt.
* `REBASE_OPERATION_EDIT`: cherry-pick the commit in question, but allow the user
  to edit the commit's contents and its message.
* `REBASE_OPERATION_SQUASH`: squash the commit in question into the previous commit.
  The commit messages of the two commits will be merged.
* `REBASE_OPERATION_FIXUP`: squash the commit in question into the previous commit.
  Only the commit message of the previous commit will be used.
* `REBASE_OPERATION_EXEC`: do not cherry-pick a commit. Run a command and continue if
  the command exits successfully.
"""
@enum(GIT_REBASE_OPERATION, REBASE_OPERATION_PICK   = Cint(0),
                            REBASE_OPERATION_REWORD = Cint(1),
                            REBASE_OPERATION_EDIT   = Cint(2),
                            REBASE_OPERATION_SQUASH = Cint(3),
                            REBASE_OPERATION_FIXUP  = Cint(4),
                            REBASE_OPERATION_EXEC   = Cint(5))

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

# describe
const DESCRIBE_DEFAULT = Cuint(0)
const DESCRIBE_TAGS    = Cuint(1 << 0)
const DESCRIBE_ALL     = Cuint(1 << 1)

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
const STATUS_OPT_INCLUDE_UNTRACKED                = Cuint(1 << 0)
const STATUS_OPT_INCLUDE_IGNORED                  = Cuint(1 << 1)
const STATUS_OPT_INCLUDE_UNMODIFIED               = Cuint(1 << 2)
const STATUS_OPT_EXCLUDE_SUBMODULES               = Cuint(1 << 3)
const STATUS_OPT_RECURSE_UNTRACKED_DIRS           = Cuint(1 << 4)
const STATUS_OPT_DISABLE_PATHSPEC_MATCH           = Cuint(1 << 5)
const STATUS_OPT_RECURSE_IGNORED_DIRS             = Cuint(1 << 6)
const STATUS_OPT_RENAMES_HEAD_TO_INDEX            = Cuint(1 << 7)
const STATUS_OPT_RENAMES_INDEX_TO_WORKDIR         = Cuint(1 << 8)
const STATUS_OPT_SORT_CASE_SENSITIVELY            = Cuint(1 << 9)
const STATUS_OPT_SORT_CASE_INSENSITIVELY          = Cuint(1 << 10)
const STATUS_OPT_RENAMES_FROM_REWRITES            = Cuint(1 << 11)
const STATUS_OPT_NO_REFRESH                       = Cuint(1 << 12)
const STATUS_OPT_UPDATE_INDEX                     = Cuint(1 << 13)
const STATUS_OPT_INCLUDE_UNREADABLE               = Cuint(1 << 14)
const STATUS_OPT_INCLUDE_UNREADABLE_AS_UNTRACKED  = Cuint(1 << 15)

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

if version() >= v"0.24.0"
    @doc """
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
    @doc """
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
@enum(GIT_OPT, GET_MWINDOW_SIZE         = 0,
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


"""
Option flags for `GitProxy`.

* `PROXY_NONE`: do not attempt the connection through a proxy.
* `PROXY_AUTO`: attempt to figure out the proxy configuration from the git configuration.
* `PROXY_SPECIFIED`: connect using the URL given in the `url` field of this struct.
"""
@enum(GIT_PROXY, PROXY_NONE,
                 PROXY_AUTO,
                 PROXY_SPECIFIED)

end
