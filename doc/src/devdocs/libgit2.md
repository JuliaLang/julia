# Base.LibGit2

The LibGit2 module provides bindings to [libgit2](https://libgit2.github.com/), a portable C library that
implements core functionality for the [Git](https://git-scm.com/) version control system.
These bindings are currently used to power Julia's package manager.
It is expected that this module will eventually be moved into a separate package.

### Functionality

Some of this documentation assumes some prior knowledge of the libgit2 API.
For more information on some of the objects and methods referenced here, consult the upstream
[libgit2 API reference](https://libgit2.github.com/libgit2/#v0.25.1).

```@docs
Base.LibGit2.Buffer
Base.LibGit2.CheckoutOptions
Base.LibGit2.CloneOptions
Base.LibGit2.DescribeOptions
Base.LibGit2.DescribeFormatOptions
Base.LibGit2.DiffDelta
Base.LibGit2.DiffFile
Base.LibGit2.DiffOptionsStruct
Base.LibGit2.FetchHead
Base.LibGit2.FetchOptions
Base.LibGit2.GitAnnotated
Base.LibGit2.GitBlame
Base.LibGit2.GitBlob
Base.LibGit2.GitCommit
Base.LibGit2.GitHash
Base.LibGit2.GitObject
Base.LibGit2.GitRemote
Base.LibGit2.GitRemoteAnon
Base.LibGit2.GitRepo
Base.LibGit2.GitRepoExt
Base.LibGit2.GitRevWalker
Base.LibGit2.GitShortHash
Base.LibGit2.GitSignature
Base.LibGit2.GitStatus
Base.LibGit2.GitTag
Base.LibGit2.GitTree
Base.LibGit2.IndexEntry
Base.LibGit2.IndexTime
Base.LibGit2.BlameOptions
Base.LibGit2.MergeOptions
Base.LibGit2.ProxyOptions
Base.LibGit2.PushOptions
Base.LibGit2.RebaseOperation
Base.LibGit2.RebaseOptions
Base.LibGit2.RemoteCallbacks
Base.LibGit2.SignatureStruct
Base.LibGit2.StatusEntry
Base.LibGit2.StatusOptions
Base.LibGit2.StrArrayStruct
Base.LibGit2.TimeStruct
Base.LibGit2.add!
Base.LibGit2.add_fetch!
Base.LibGit2.add_push!
Base.LibGit2.addblob!
Base.LibGit2.author
Base.LibGit2.authors
Base.LibGit2.branch
Base.LibGit2.branch!
Base.LibGit2.checkout!
Base.LibGit2.clone
Base.LibGit2.commit
Base.LibGit2.committer
Base.LibGit2.count(::Function, ::Base.LibGit2.GitRevWalker; ::Base.LibGit2.GitHash, ::Cint, ::Bool)
Base.LibGit2.counthunks
Base.LibGit2.create_branch
Base.LibGit2.credentials_callback
Base.LibGit2.credentials_cb
Base.LibGit2.default_signature
Base.LibGit2.delete_branch
Base.LibGit2.diff_files
Base.LibGit2.entryid
Base.LibGit2.entrytype
Base.LibGit2.fetch
Base.LibGit2.fetchheads
Base.LibGit2.fetch_refspecs
Base.LibGit2.fetchhead_foreach_cb
Base.LibGit2.merge_base
Base.LibGit2.merge!(::Base.LibGit2.GitRepo; ::Any...)
Base.LibGit2.merge!(::Base.LibGit2.GitRepo, ::Vector{Base.LibGit2.GitAnnotated}; ::Base.LibGit2.MergeOptions, ::Base.LibGit2.CheckoutOptions)
Base.LibGit2.merge!(::Base.LibGit2.GitRepo, ::Vector{Base.LibGit2.GitAnnotated}, ::Bool; ::Base.LibGit2.MergeOptions, ::Base.LibGit2.CheckoutOptions)
Base.LibGit2.ffmerge!
Base.LibGit2.fullname
Base.LibGit2.features
Base.LibGit2.filename
Base.LibGit2.filemode
Base.LibGit2.gitdir
Base.LibGit2.git_url
Base.LibGit2.@githash_str
Base.LibGit2.head
Base.LibGit2.head!
Base.LibGit2.head_oid
Base.LibGit2.headname
Base.LibGit2.init
Base.LibGit2.is_ancestor_of
Base.LibGit2.isbinary
Base.LibGit2.iscommit
Base.LibGit2.isdiff
Base.LibGit2.isdirty
Base.LibGit2.isorphan
Base.LibGit2.isset
Base.LibGit2.iszero
Base.LibGit2.lookup_branch
Base.LibGit2.map(::Function, ::Base.LibGit2.GitRevWalker; ::Base.LibGit2.GitHash, ::AbstractString, ::Cint, ::Bool)
Base.LibGit2.mirror_callback
Base.LibGit2.mirror_cb
Base.LibGit2.message
Base.LibGit2.merge_analysis
Base.LibGit2.name
Base.LibGit2.need_update
Base.LibGit2.objtype
Base.LibGit2.path
Base.LibGit2.peel
Base.LibGit2.posixpath
Base.LibGit2.push
Base.LibGit2.push!(::Base.LibGit2.GitRevWalker, ::Base.LibGit2.GitHash)
Base.LibGit2.push_head!
Base.LibGit2.push_refspecs
Base.LibGit2.raw
Base.LibGit2.read_tree!
Base.LibGit2.rebase!
Base.LibGit2.ref_list
Base.LibGit2.reftype
Base.LibGit2.remotes
Base.LibGit2.remove!
Base.LibGit2.reset
Base.LibGit2.reset!
Base.LibGit2.restore
Base.LibGit2.revcount
Base.LibGit2.set_remote_url
Base.LibGit2.shortname
Base.LibGit2.snapshot
Base.LibGit2.status
Base.LibGit2.stage
Base.LibGit2.tag_create
Base.LibGit2.tag_delete
Base.LibGit2.tag_list
Base.LibGit2.target
Base.LibGit2.toggle
Base.LibGit2.transact
Base.LibGit2.treewalk
Base.LibGit2.upstream
Base.LibGit2.update!
Base.LibGit2.url
Base.LibGit2.version
Base.LibGit2.with
Base.LibGit2.with_warn
Base.LibGit2.workdir
Base.LibGit2.GitObject(::Base.LibGit2.GitTreeEntry)
Base.LibGit2.UserPasswordCredential
Base.LibGit2.SSHCredential
Base.LibGit2.isfilled
Base.LibGit2.CachedCredentials
Base.LibGit2.CredentialPayload
Base.LibGit2.approve
Base.LibGit2.reject
```
