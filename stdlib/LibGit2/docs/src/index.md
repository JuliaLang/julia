# LibGit2

The LibGit2 module provides bindings to [libgit2](https://libgit2.org/), a portable C library that
implements core functionality for the [Git](https://git-scm.com/) version control system.
These bindings are currently used to power Julia's package manager.
It is expected that this module will eventually be moved into a separate package.

### Functionality

Some of this documentation assumes some prior knowledge of the libgit2 API.
For more information on some of the objects and methods referenced here, consult the upstream
[libgit2 API reference](https://libgit2.org/libgit2/#v0.25.1).

```@docs
LibGit2.Buffer
LibGit2.CheckoutOptions
LibGit2.CloneOptions
LibGit2.DescribeOptions
LibGit2.DescribeFormatOptions
LibGit2.DiffDelta
LibGit2.DiffFile
LibGit2.DiffOptionsStruct
LibGit2.FetchHead
LibGit2.FetchOptions
LibGit2.GitAnnotated
LibGit2.GitBlame
LibGit2.GitBlob
LibGit2.GitCommit
LibGit2.GitHash
LibGit2.GitObject
LibGit2.GitRemote
LibGit2.GitRemoteAnon
LibGit2.GitRepo
LibGit2.GitRepoExt
LibGit2.GitRevWalker
LibGit2.GitShortHash
LibGit2.GitSignature
LibGit2.GitStatus
LibGit2.GitTag
LibGit2.GitTree
LibGit2.IndexEntry
LibGit2.IndexTime
LibGit2.BlameOptions
LibGit2.MergeOptions
LibGit2.ProxyOptions
LibGit2.PushOptions
LibGit2.RebaseOperation
LibGit2.RebaseOptions
LibGit2.RemoteCallbacks
LibGit2.SignatureStruct
LibGit2.StatusEntry
LibGit2.StatusOptions
LibGit2.StrArrayStruct
LibGit2.TimeStruct
LibGit2.add!
LibGit2.add_fetch!
LibGit2.add_push!
LibGit2.addblob!
LibGit2.author
LibGit2.authors
LibGit2.branch
LibGit2.branch!
LibGit2.checkout!
LibGit2.clone
LibGit2.commit
LibGit2.committer
LibGit2.count
LibGit2.counthunks
LibGit2.create_branch
LibGit2.credentials_callback
LibGit2.credentials_cb
LibGit2.default_signature
LibGit2.delete_branch
LibGit2.diff_files
LibGit2.entryid
LibGit2.entrytype
LibGit2.fetch
LibGit2.fetchheads
LibGit2.fetch_refspecs
LibGit2.fetchhead_foreach_cb
LibGit2.merge_base
LibGit2.merge!(::LibGit2.GitRepo; ::Any...)
LibGit2.merge!(::LibGit2.GitRepo, ::Vector{LibGit2.GitAnnotated}; ::LibGit2.MergeOptions, ::LibGit2.CheckoutOptions)
LibGit2.merge!(::LibGit2.GitRepo, ::Vector{LibGit2.GitAnnotated}, ::Bool; ::LibGit2.MergeOptions, ::LibGit2.CheckoutOptions)
LibGit2.ffmerge!
LibGit2.fullname
LibGit2.features
LibGit2.filename
LibGit2.filemode
LibGit2.gitdir
LibGit2.git_url
LibGit2.@githash_str
LibGit2.head
LibGit2.head!
LibGit2.head_oid
LibGit2.headname
LibGit2.init
LibGit2.is_ancestor_of
LibGit2.isbinary
LibGit2.iscommit
LibGit2.isdiff
LibGit2.isdirty
LibGit2.isorphan
LibGit2.isset
LibGit2.iszero
LibGit2.lookup_branch
LibGit2.map
LibGit2.mirror_callback
LibGit2.mirror_cb
LibGit2.message
LibGit2.merge_analysis
LibGit2.name
LibGit2.need_update
LibGit2.objtype
LibGit2.path
LibGit2.peel
LibGit2.posixpath
LibGit2.push
LibGit2.push!(::LibGit2.GitRevWalker, ::LibGit2.GitHash)
LibGit2.push_head!
LibGit2.push_refspecs
LibGit2.raw
LibGit2.read_tree!
LibGit2.rebase!
LibGit2.ref_list
LibGit2.reftype
LibGit2.remotes
LibGit2.remove!
LibGit2.reset
LibGit2.reset!
LibGit2.restore
LibGit2.revcount
LibGit2.set_remote_url
LibGit2.shortname
LibGit2.snapshot
LibGit2.split_cfg_entry
LibGit2.status
LibGit2.stage
LibGit2.tag_create
LibGit2.tag_delete
LibGit2.tag_list
LibGit2.target
LibGit2.toggle
LibGit2.transact
LibGit2.treewalk
LibGit2.upstream
LibGit2.update!
LibGit2.url
LibGit2.version
LibGit2.with
LibGit2.with_warn
LibGit2.workdir
LibGit2.GitObject(::LibGit2.GitTreeEntry)
LibGit2.UserPasswordCredential
LibGit2.SSHCredential
LibGit2.isfilled
LibGit2.CachedCredentials
LibGit2.CredentialPayload
LibGit2.approve
LibGit2.reject
```
