export repo_isbare, repo_isempty, repo_workdir, repo_path, path,
       repo_open, repo_init, repo_index, head, set_head!, tags, tag!, commits, references,
       repo_lookup, lookup_tree, lookup_commit, commit, ref_names,
       repo_revparse_single, create_ref, create_sym_ref, lookup_ref,
       repo_odb, iter_refs, config, repo_treebuilder, TreeBuilder,
       insert!, write!, close, lookup, rev_parse, rev_parse_oid, remotes,
       ahead_behind, merge_base, merge_commits,  blob_at, is_shallow, hash_data,
       default_signature, repo_discover, is_bare, is_empty, namespace, set_namespace!,
       notes, create_note!, remove_note!, each_note, note_default_ref, iter_notes,
       blob_from_buffer, blob_from_workdir, blob_from_disk, blob_from_stream,
       branch_names, lookup_branch, create_branch, lookup_remote, iter_branches,
       remote_names, remote_add!, checkout_tree!, checkout_head!, checkout!, 
       ishead_detached, GitCredential, CredDefault, CredPlainText, CredSSHKey, 
       repo_clone, repo_mirror

Repository(path::String; alternates=nothing) = begin
    bpath = bytestring(path)
    repo_ptr = Array(Ptr{Void}, 1)
    err_code = api.git_repository_open(repo_ptr, bpath)
    if err_code < 0
        if repo_ptr[1] != C_NULL
            api.git_repository_free(repo_ptr[1])
        end
        throw(LibGitError(err_code))
    end
    repo = Repository(repo_ptr[1])
    if alternates != nothing && length(alternates) > 0
        odb = repo_odb(repo)
        for path in alternates
            if !isdir(path)
                throw(ArgumentError("alternate $path is not a valid dir"))
            end
            bpath = bytestring(path)
            err = api.git_odb_add_disk_alternate(odb.ptr, bpath)
            if err != api.GIT_OK
                throw(LibGitError(err))
            end
        end
    end
    return repo
end

Base.close(r::Repository) = begin
    if r.ptr != C_NULL
        api.git_repository__cleanup(r.ptr)
    end
end

Base.in(id::Oid, r::Repository) = begin
    odb = repo_odb(r)
    return exists(odb, id)::Bool
end

function cb_iter_oids(idptr::Ptr{Uint8}, o::Ptr{Void})
    try
        produce(Oid(idptr))
        return api.GIT_OK
    catch err
        return api.ERROR
    end
end

const c_cb_iter_oids = cfunction(cb_iter_oids, Cint, (Ptr{Uint8}, Ptr{Void}))

#TODO: better error handling
Base.start(r::Repository) = begin
    odb = repo_odb(r)
    t = @task api.git_odb_foreach(odb.ptr, c_cb_iter_oids, C_NULL)
    (consume(t), t)
end

Base.done(r::Repository, state) = begin
    istaskdone(state[2])
end

Base.next(r::Repository, state) = begin
    v = consume(state[2])
    (state[1], (v, state[2]))
end

Base.read(r::Repository, id::Oid) = begin
    odb = repo_odb(r)
    obj_ptr = Array(Ptr{Void}, 1)
    @check api.git_odb_read(obj_ptr, odb.ptr, id.oid)
    return OdbObject(obj_ptr[1])
end

Base.delete!(r::Repository, ref::GitReference) = begin
    @assert r.ptr != C_NULL && ref.ptr != C_NULL
    @check api.git_reference_delete(ref.ptr)
    return r
end

Base.delete!(r::Repository, t::GitTag) = begin
    @assert r.ptr != C_NULL && t.ptr != C_NULL
    @check api.git_tag_delete(r.ptr, name(t))
    return nothing
end

Base.getindex(r::Repository, o) = lookup(r, o)

function read_header(r::Repository, id::Oid)
    odb = repo_odb(r)
    return read_header(odb, id)
end

exists(r::Repository, id::Oid) = id in r 

exists(r::Repository, ref::String) = begin
    @assert r.ptr != C_NULL
    ref_ptr = Array(Ptr{Void}, 1)
    err = api.git_reference_lookup(ref_ptr, r.ptr, bytestring(ref))
    api.git_reference_free(ref_ptr[1])
    if err == api.ENOTFOUND
        return false
    elseif err != api.GIT_OK
        throw(LibGitError(err))
    end
    return true
end

        
@deprecate repo_isbare is_bare
repo_isbare(r::Repository) = is_bare(r)

function is_bare(r::Repository)
    @assert r.ptr != C_NULL
    res = api.git_repository_is_bare(r.ptr)
    return res > zero(Cint) ? true : false
end

@deprecate repo_isbare is_bare
repo_isempty(r) = is_empty(r)

function is_empty(r::Repository)
    @assert r.ptr != C_NULL
    res = api.git_repository_is_empty(r.ptr) 
    return res > zero(Cint) ? true : false
end

function is_shallow(r::Repository)
    @assert r.ptr != C_NULL
    res = api.git_repository_is_shallow(r.ptr)
    return res > zero(Cint) ? true : false
end

function repo_workdir(r::Repository)
    @assert r.ptr != C_NULL
    res = api.git_repository_workdir(r.ptr)
    if res == C_NULL
        return nothing
    end
    # remove trailing slash
    return bytestring(res)[1:end-1]
end

@deprecate repo_path path
repo_path(r::Repository) = path(r)

function path(r::Repository)
    @assert r.ptr != C_NULL
    cpath = api.git_repository_path(r.ptr)
    if cpath == C_NULL
        return nothing
    end
    # remove trailing slash
    return bytestring(cpath)[1:end-1]
end

function hash_data{T<:GitObject}(::Type{T}, content::String)
    out = Oid()
    bcontent = bytestring(content)
    @check api.git_odb_hash(out.oid, bcontent, length(bcontent), git_otype(T))
    return out
end

function default_signature(r::Repository)
    @assert r.ptr != C_NULL
    sig_ptr = Array(Ptr{api.GitSignature}, 1)
    err = api.git_signature_default(sig_ptr, r.ptr)
    if err == api.ENOTFOUND
        return nothing
    elseif err != api.GIT_OK
        throw(LibGitError(err))
    end
    gsig = unsafe_load(sig_ptr[1])
    sig = Signature(gsig)
    api.free!(gsig)
    return sig
end

function repo_head_orphaned(r::Repository)
end

function ishead_detached(r::Repository)
    @assert r.ptr != C_NULL
    return bool(api.git_repository_head_detached(r.ptr))
end

function repo_open(path::String)
    Repository(path)
end

function repo_init(path::String; bare::Bool=false)
    bpath = bytestring(path)
    repo_ptr = Array(Ptr{Void}, 1)
    err_code = api.git_repository_init(repo_ptr, bpath, bare? 1 : 0)
    if err_code < 0
        if repo_ptr[1] != C_NULL
            api.git_repository_free(repo_ptr[1])
        end
        throw(LibGitError(err_code))
    end
    return Repository(repo_ptr[1])
end

repo_mirror(url::String, path::String; name::String="origin") = begin
    # Init the repo
    r = repo_init(path; bare=true)
    @assert r.ptr != C_NULL
    check_valid_url(url)
    remote_ptr = Array(Ptr{Void}, 1)
    # Create the mirror remote
    @check api.git_remote_create_with_fetchspec(remote_ptr, r.ptr, bytestring(name), bytestring(url), bytestring("+refs/*:refs/*"))
    remote = GitRemote(remote_ptr[1])
    # Set remote.origin.mirror to true
    c = config(r)
    c["remote.origin.mirror"] = "true"
    @check api.git_clone_into(r.ptr, remote.ptr, C_NULL, C_NULL, C_NULL)
end

function set_namespace!(r::Repository, ns)
    if ns == nothing || isempty(ns)
        @check api.git_repository_set_namespace(r.ptr, C_NULL)
    else
        @check api.git_repository_set_namespace(r.ptr, bytestring(ns))
    end
    return r
end

function namespace(r::Repository)
    ns_ptr = api.git_repository_get_namespace(r.ptr)
    if ns_ptr == C_NULL
        return nothing
    end
    return bytestring(ns_ptr)
end

function head(r::Repository)
    @assert r.ptr != C_NULL
    head_ptr = Array(Ptr{Void}, 1)
    err = api.git_repository_head(head_ptr, r.ptr)
    if err == api.ENOTFOUND || err == api.EUNBORNBRANCH
        return nothing
    elseif err != api.GIT_OK
        throw(LibGitError(err))
    end 
    return GitReference(head_ptr[1])
end

Oid(r::Repository, val::GitObject) = Oid(val)
Oid(r::Repository, val::Oid) = val

function Oid(r::Repository, val::String)
    if length(val) == api.OID_HEXSZ
        try
            return Oid(val)
        catch
        end
    end
    return Oid(rev_parse(r, val))
end

#TODO: need to add tests for sig/logmsg
function set_head!(r::Repository, ref::String; sig=nothing, logmsg=nothing)
    @assert r.ptr != C_NULL
    bref = bytestring(ref)
    bmsg = logmsg != nothing ? bytestring(logmsg) : C_NULL
    if sig == nothing
        @check ccall((:git_repository_set_head, api.libgit2), Cint,
                     (Ptr{Void}, Ptr{Cchar}, Ptr{api.GitSignature}, Ptr{Cchar}),
                      r.ptr, bref, C_NULL, bmsg)
    else
        @assert isa(sig, Signature)
        gsig = git_signature(sig)
        @check ccall((:git_repository_set_head, api.libgit2), Cint,
                     (Ptr{Void}, Ptr{Cchar}, Ptr{api.GitSignature}, Ptr{Cchar}),
                      r.ptr, bref, &gsig, bmsg)
    end
    return r
end

function set_head_detached!(r::Repository, oid::Oid; sig=nothing, logmsg=nothing)
    @assert r.ptr != C_NULL
    @check api.git_repository_set_head_detached(r.ptr, oid.oid, C_NULL, C_NULL)
    return r
end
function set_head_detached!(r::Repository, oid::String; sig=nothing, logmsg=nothing)
    set_head_detached!(r, rev_parse_oid(r, oid); sig=sig, logmsg=logmsg)
end

function commits(r::Repository)
    return nothing 
end

function remotes(r::Repository)
    @assert r.ptr != C_NULL
    rs = api.GitStrArray()
    @check ccall((:git_remote_list, api.libgit2), Cint,
                 (Ptr{api.GitStrArray}, Ptr{Void}),
                 &rs, r.ptr)
    if rs.count == 0 
        return nothing 
    end
    remote_ptr = Array(Ptr{Void}, 1)
    out = Array(GitRemote, rs.count)
    for i in 1:rs.count 
        rstr = bytestring(unsafe_load(rs.strings, i))
        @check api.git_remote_load(remote_ptr, r.ptr, rstr)
        out[i] = GitRemote(remote_ptr[1])
    end
    return out
end

#TODO: this should be moved to remote
GitRemote(r::Repository, url::String) = begin
    @assert r.ptr != C_NULL
    check_valid_url(url)
    remote_ptr = Array(Ptr{Void}, 1)
    @check api.git_remote_create_anonymous(remote_ptr, r.ptr, bytestring(url), C_NULL)
    return GitRemote(remote_ptr[1])
end

#TODO: this is redundant with above function
function remote_names(r::Repository)
    @assert r.ptr != C_NULL
    rs = api.GitStrArray()
    @check ccall((:git_remote_list, api.libgit2), Cint,
                  (Ptr{api.GitStrArray}, Ptr{Void}),
                  &rs, r.ptr)
    ns = Array(String, rs.count)
    for i in 1:rs.count
        ns[i] = bytestring(unsafe_load(rs.strings, i))
    end
    return ns
end

function remote_add!(r::Repository, name::String, url::String)
    @assert r.ptr != C_NULL
    check_valid_url(url)
    remote_ptr = Array(Ptr{Void}, 1)
    @check api.git_remote_create(remote_ptr, r.ptr, bytestring(name), bytestring(url))
    return GitRemote(remote_ptr[1])
end

function remote_add!(r::Repository, name::String, url::String, fetchspec::String)
    @assert r.ptr != C_NULL
    check_valid_url(url)
    remote_ptr = Array(Ptr{Void}, 1)
    @check api.git_remote_create_with_fetchspec(remote_ptr, r.ptr, bytestring(name), bytestring(url), bytestring(fetchspec))
    return GitRemote(remote_ptr[1])
end

function remote_delete!(r::GitRemote)
    @assert r.ptr != C_NULL
    @check api.git_remote_delete(r.ptr)
end

function cb_push_status(ref_ptr::Ptr{Cchar}, msg_ptr::Ptr{Cchar}, payload::Ptr{Void})
    if msg_ptr != C_NULL 
        result = unsafe_pointer_to_objref(payload)::Dict{UTF8String,UTF8String}
        result[utf8(bytestring(ref_ptr))] = utf8(bytestring(msg_ptr))
    end
    return api.GIT_OK
end

const c_cb_push_status = cfunction(cb_push_status, Cint,
                                   (Ptr{Cchar}, Ptr{Cchar}, Ptr{Void}))

#TODO: git push update tips takes a signature and message
#TODO: better error messages
Base.push!{T<:String}(r::Repository, remote::GitRemote, refs::Vector{T}) = begin
    @assert r.ptr != C_NULL && remote.ptr != C_NULL
    err = zero(Cint) 
    push_ptr = Ptr{Void}[0]
    err = api.git_push_new(push_ptr, remote.ptr)
    if err != api.GIT_OK
        if push_ptr != C_NULL
            api.git_push_free(push_ptr[1])
        end
        throw(LibGitError(err))
    end
    for ref in refs
        err = api.git_push_add_refspec(push_ptr[1], bytestring(ref))
    end
    if err != api.GIT_OK
        api.git_push_free(push_ptr[1])
        throw(LibGitError(err))
    end
    err = api.git_push_finish(push_ptr[1])
    if err != api.GIT_OK
        api.git_push_free(push_ptr[1])
        if err == api.ENONFASTFORWARD
            error("non-fast-forward upate rejected")
        elseif err == -8
            error("could not push to repo (check for non-bare repo)")
        end
    end
    err = api.git_push_unpack_ok(push_ptr[1])
    if err == api.GIT_OK
        api.git_push_free(push_ptr[1])
        error("remote side did not unpack successfully")
    end
    result = (UTF8String=>UTF8String)[]
    err = ccall((:git_push_status_foreach, api.libgit2), Cint,
                (Ptr{Void}, Ptr{Void}, Any),
                push_ptr[1], c_cb_push_status, &result)
    if err != api.GIT_OK
        api.git_push_free(push_ptr[1])
        throw(LibGitError(err))
    end
    err = ccall((:git_push_update_tips, api.libgit2), Cint,
                (Ptr{Void}, Ptr{api.GitSignature}, Ptr{Cchar}),
                push_ptr[1], C_NULL, C_NULL)
    if err != api.GIT_OK
        api.git_push_free(push_ptr[1])
        throw(LibGitError(err))
    end
    return result
end

Base.push!{T<:String}(r::Repository, remote::String, refs::Vector{T}) = begin
    @assert r.ptr != C_NULL
    remote_ptr = Array(Ptr{Void}, 1)
    @check api.git_remote_load(remote_ptr, r.ptr, bytestring(remote))
    return push!(r, GitRemote(remote_ptr[1]), refs)
end

function lookup(::Type{GitRemote}, r::Repository, remote_name::String)
    @assert r.ptr != C_NULL
    remote_ptr =  Array(Ptr{Void}, 1)
    err = api.git_remote_load(remote_ptr, r.ptr, bytestring(remote_name))
    if err == api.ENOTFOUND
        return nothing
    elseif err != api.GIT_OK
        throw(LibGitError(err))
    end
    return GitRemote(remote_ptr[1])
end

lookup_remote(r::Repository, remote_name::String) = lookup(GitRemote, r, remote_name)
lookup_tag(r::Repository, id::Oid) = lookup(GitTag, r, id)

function tags(r::Repository, glob=nothing)
    @assert r.ptr != C_NULL
    local cglob::ByteString
    if glob != nothing
        cglob = bytestring(glob)
    else
        cglob = bytestring("") 
    end
    gittags = api.GitStrArray()
    @check ccall((:git_tag_list_match, api.libgit2), Cint,
                 (Ptr{api.GitStrArray}, Ptr{Cchar}, Ptr{Void}),
                 &gittags, cglob, r.ptr)
    if gittags.count == 0
        return nothing
    end
    out = Array(ASCIIString, gittags.count)
    for i in 1:gittags.count
        cptr = unsafe_load(gittags.strings, i)
        out[i] = bytestring(cptr)
    end
    return out
end

function tag!(r::Repository; 
              name::String="",
              message::String="",
              target::Union(Nothing,Oid)=nothing,
              tagger::Union(Nothing,Signature)=nothing,
              force::Bool=false)
   @check r.ptr != C_NULL
   if target != nothing
       obj = lookup(r, target)
   end
   tid = Oid()
   if !isempty(message)
       if tagger != nothing
           gsig = git_signature(tagger)
       else
           gsig = git_signature(default_signature(r))
       end
       @check ccall((:git_tag_create, api.libgit2), Cint,
                    (Ptr{Uint8}, Ptr{Void}, Ptr{Cchar},
                     Ptr{Void}, Ptr{api.GitSignature}, Ptr{Cchar}, Cint),
                     tid.oid, r.ptr, bytestring(name), obj.ptr, 
                     &gsig, bytestring(message), force)
   else
       @check api.git_tag_create_lightweight(tid.oid,
                                             r.ptr,
                                             bytestring(name),
                                             obj.ptr,
                                             force? 1 : 0)
   end
   return tid
end

function create_note!(
               obj::GitObject, msg::String; 
               committer::Union(Nothing, Signature)=nothing,
               author::Union(Nothing, Signature)=nothing,
               ref::Union(Nothing, String)=nothing,
               force::Bool=false)
    @assert obj.ptr != nothing
    #repo = Repository(api.git_object_owner(obj.ptr))
    repo_ptr = api.git_object_owner(obj.ptr)
    target_id = Oid(obj)
    bref = ref != nothing ? bytestring(ref) : C_NULL
    if committer != nothing
        committer_ptr = git_signature_ptr(committer)
    else
        # gcommitter = git_signature(default_signature(repo))
        sig_ptr = Array(Ptr{api.GitSignature}, 1)
        api.git_signature_default(sig_ptr, repo_ptr)
        committer_ptr = sig_ptr[1]
 
    end
    if author != nothing
        author_ptr = git_signature_ptr(author)
    else
        # gauthor = git_signature(default_signature(repo))
        sig_ptr = Array(Ptr{api.GitSignature}, 1)
        api.git_signature_default(sig_ptr, repo_ptr)
        author_ptr = sig_ptr[1]
    end
    note_id  = Oid()
    @check ccall((:git_note_create, api.libgit2), Cint,
                 (Ptr{Uint8}, Ptr{Void}, Ptr{api.GitSignature},
                  Ptr{api.GitSignature}, Ptr{Cchar}, Ptr{Uint8},
                  Ptr{Cchar}, Cint),
                 note_id.oid, repo_ptr, committer_ptr, author_ptr,
                 bref, target_id.oid, bytestring(msg),
                 force? 1 : 0)
    return note_id
end

function remove_note!(obj::GitObject;
                      committer::Union(Nothing,Signature)=nothing,
                      author::Union(Nothing,Signature)=nothing,
                      ref::Union(Nothing,String)=nothing)
    @assert obj.ptr != C_NULL
    target_id = Oid(obj)
    #repo = Repository(api.git_object_owner(obj.ptr))
    repo_ptr = api.git_object_owner(obj.ptr)
    notes_ref = ref != nothing ? bytestring(ref) : bytestring("refs/notes/commits")
    if committer != nothing
        committer_ptr = git_signature_ptr(committer)
    else
        sig_ptr = Array(Ptr{api.GitSignature}, 1)
        api.git_signature_default(sig_ptr, repo_ptr)
        committer_ptr = sig_ptr[1]
        #gcommitter = git_signature(default_signature(repo))
    end
    if author != nothing
        author_ptr = git_signature_ptr(author)
    else
        #gauthor = git_signature(default_signature(repo))
        sig_ptr = Array(Ptr{api.GitSignature}, 1)
        api.git_signature_default(sig_ptr, repo_ptr)
        author_ptr = sig_ptr[1]
    end 
    err = ccall((:git_note_remove, api.libgit2), Cint,
                (Ptr{Void}, Ptr{Cchar}, 
                 Ptr{api.GitSignature}, Ptr{api.GitSignature}, Ptr{Uint8}),
                 repo_ptr, notes_ref, author_ptr, committer_ptr, target_id.oid)
    if err == api.ENOTFOUND
        return false
    elseif err != api.GIT_OK
        throw(LibGitError(err))
    end
    return true
end

function note_default_ref(r::Repository)
    refname_ptr = Array(Ptr{Cchar}, 1)
    @check api.git_note_default_ref(refname_ptr, r.ptr)
    return bytestring(refname_ptr[1])
end

# todo the following should be moved to not    
function notes(obj::GitObject, ref=nothing)
    lookup_note(obj, ref)
end

function lookup_note(obj::GitObject, ref=nothing)
    if ref == nothing
        bref = C_NULL
    else 
        bref = bytestring(ref)
    end
    note_ptr = Array(Ptr{Void}, 1)
    repo_ptr = api.git_object_owner(obj.ptr)
    err = api.git_note_read(note_ptr, repo_ptr, bref, Oid(obj).oid)
    if err == api.ENOTFOUND
        return nothing
    elseif err != api.GIT_OK
        throw(LibGitError(err))
    end
    n = GitNote(note_ptr[1])
    api.git_note_free(note_ptr[1])
    return n
end

function cb_iter_notes(blob_id::Ptr{Uint8}, ann_obj_id::Ptr{Uint8}, repo_ptr::Ptr{Void})
    ann_obj_ptr = Array(Ptr{Void}, 1)
    blob_ptr = Array(Ptr{Void}, 1)
    err = api.git_object_lookup(ann_obj_ptr, repo_ptr, ann_obj_id, api.OBJ_BLOB)
    err |= api.git_object_lookup(blob_ptr, repo_ptr, blob_id, api.OBJ_BLOB)
    if err == api.GIT_OK
        res = (gitobj_from_ptr(ann_obj_id[1]), 
               gitobj_from_ptr(blob_ptr[1]))
        produce(res)
    end
    return err
end

const c_cb_iter_notes = cfunction(cb_iter_notes, Cint, (Ptr{Uint8}, Ptr{Uint8}, Ptr{Void}))

function iter_notes(r::Repository, notes_ref=nothing)
    @assert r.ptr != C_NULL
    if ref == nothing
        bnotes_ref = C_NULL
    else
        bnotes_ref = bytestring(notes_ref)
    end
    return @task api.git_note_foreach(r.ptr, notes_ref, c_cb_iter_notes, r.ptr)
end

function ahead_behind(r::Repository,
                      lcommit::GitCommit,
                      ucommit::GitCommit)
    ahead_behind(r, Oid(lcommit), Oid(ucommit))
end

function ahead_behind(r::Repository, lid::Oid, uid::Oid)
    @assert r.ptr != C_NULL
    ahead = Csize_t[0]
    behind = Csize_t[0]
    @check api.git_graph_ahead_behind(
                ahead, behind, r.ptr, lid.oid, uid.oid)
    return (int(ahead[1]), int(behind[1]))
end

function blob_at(r::Repository, rev::Oid, p::String)
    tree = git_tree(lookup_commit(r, rev))
    local blob_entry::GitTreeEntry
    try
        blob_entry = entry_bypath(tree, p)
        @assert isa(blob_entry, GitTreeEntry{GitBlob})
    catch
        return nothing
    end
    blob = lookup_blob(r, Oid(blob_entry))
    return blob
end

function blob_from_buffer(r::Repository, buf::ByteString)
    @assert  r.ptr != C_NULL
    blob_id = Oid()
    @check api.git_blob_create_frombuffer(blob_id.oid, r.ptr, buf, length(buf))
    return blob_id
end

function blob_from_workdir(r::Repository, path::String)
    @assert r.ptr != C_NULL
    blob_id = Oid()
    @check api.git_blob_create_fromworkdir(blob_id.oid, r.ptr, bytestring(path))
    return blob_id
end

function blob_from_disk(r::Repository, path::String)
    @assert r.ptr != C_NULL
    blob_id = Oid()
    @check api.git_blob_create_fromdisk(blob_id.oid, r.ptr, bytestring(path))
    return blob_id
end

function cb_blob_get_chunk(content_ptr::Ptr{Uint8}, 
                           max_len::Csize_t, 
                           payload::Ptr{Void})
    payload = unsafe_pointer_to_objref(payload)::Array{Any,1}
    stream = payload[1]
    local buff::Array{Uint8,1}
    try
        buff = readbytes(stream, max_len)
    catch err
        payload[2] = err
        return api.ERROR
    end
    len = length(buff)
    len = len > max_len ? max_len : len
    unsafe_copy!(content_ptr, convert(Ptr{Uint8}, buff), len)
    return convert(Cint, len)
end

const c_cb_blob_get_chunk = cfunction(cb_blob_get_chunk, Cint,
                                         (Ptr{Uint8}, Csize_t, Ptr{Void}))

function blob_from_stream(r::Repository, stream, hintpath=nothing)
    @assert r.ptr != C_NULL
    blob_id = Oid()
    local path_ptr::Ptr{Cchar}
    if hintpath != nothing
        path_ptr = convert(Ptr{Cchar}, pointer(bytestring(hintpath)))
    else
        path_ptr = C_NULL
    end
    payload = {stream, nothing}
    err = ccall((:git_blob_create_fromchunks, api.libgit2), Cint,
                (Ptr{Uint8}, Ptr{Void}, Ptr{Cchar}, Ptr{Void}, Any),
                blob_id.oid, r.ptr, path_ptr, c_cb_blob_get_chunk, &payload)
    if isa(payload[2], Exception)
        throw(payload[2])
    end
    if err != api.GIT_OK
        throw(LibGitError(err))
    end
    return blob_id
end

#TODO: consolidate with odb
function write!{T<:GitObject}(::Type{T}, r::Repository, buf::ByteString)
    @assert r.ptr != C_NULL
    odb = repo_odb(r)
    gty = git_otype(T)
    stream_ptr = Array(Ptr{Void}, 1) 
    @check api.git_odb_open_wstream(stream_ptr, odb.ptr, length(buf), gty)
    err = api.git_odb_stream_write(stream_ptr[1], buf, length(buf))
    out = Oid()
    if !(bool(err))
        err = api.git_odb_stream_finalize_write(out.oid, stream_ptr[1])
    end
    api.git_odb_stream_free(stream_ptr[1])
    if err != api.GIT_OK
        throw(LibGitError(err))
    end
    return out
end

# git_reference_list
function references(r::Repository)
    return nothing
end

function repo_discover(p::String="", acrossfs::Bool=true)
    if isempty(p); p = pwd(); end
    brepo = Array(Cchar, api.GIT_PATH_MAX)
    bp = bytestring(p)
    buf = api.GitBuffer()
    @check ccall((:git_repository_discover, api.libgit2), Cint,
                 (Ptr{api.GitBuffer}, Ptr{Cchar}, Cint, Ptr{Cchar}),
                  &buf, bp, acrossfs? 1 : 0, C_NULL)
    return Repository(bytestring(buf.ptr))
end

function rev_parse(r::Repository, rev::String)
    @assert r.ptr != C_NULL
    brev = bytestring(rev)
    obj_ptr = Array(Ptr{Void}, 1)
    @check api.git_revparse_single(obj_ptr, r.ptr, brev)
    obj = gitobj_from_ptr(obj_ptr[1]) 
    return obj
end

function rev_parse(r::Repository, rev::Oid)
    return rev_parse(r, string(rev))
end

function merge_base(r::Repository, o1::Oid, o2::Oid)
    @assert r.ptr != C_NULL
    id = Oid()
    err = api.git_merge_base(id.oid, r.ptr, o1.oid, o2.oid)
    if err == api.ENOTFOUND
        return nothing
    elseif err != api.GIT_OK
        throw(LibGitError(err))
    end
    return id
end

function merge_base(r::Repository, args...)
    @assert r.ptr != C_NULL
    if length(args) < 2
        throw(ArgumentError("merge_base needs 2+ commits"))
    end
    arg_oids = vcat([raw(Oid(r, a)) for a in args]...)
    @assert length(arg_oids) == length(args) * api.OID_RAWSZ
    len = convert(Csize_t, length(args))
    id = Oid()
    err = api.git_merge_base_many(id.oid, r.ptr, len, arg_oids)
    if err == api.ENOTFOUND
        return nothing
    elseif err != api.GIT_OK
        throw(LibGitError(err))
    end
    return id
end
    
function rev_parse_oid(r::Repository, rev::String)
    Oid(rev_parse(r, rev))
end

#TODO: this could be more efficient
function rev_parse_oid(r::Repository, rev::Oid)
    return Oid(rev_parse(r, string(rev)))
end

function config(r::Repository)
    @assert r.ptr != C_NULL
    config_ptr = Array(Ptr{Void}, 1)
    @check api.git_repository_config(config_ptr, r.ptr)
    return GitConfig(config_ptr[1])
end

function repo_odb(r::Repository)
    @assert r.ptr != C_NULL
    odb_ptr = Array(Ptr{Void}, 1)
    @check api.git_repository_odb(odb_ptr, r.ptr)
    return Odb(odb_ptr[1])
end

function repo_index(r::Repository)
    @assert r.ptr != C_NULL
    idx_ptr = Array(Ptr{Void}, 1)
    @check api.git_repository_index(idx_ptr, r.ptr)
    return GitIndex(idx_ptr[1])
end

function lookup{T<:GitObject}(::Type{T}, r::Repository, id::Oid)
    @assert r.ptr != C_NULL
    obj_ptr = Array(Ptr{Void}, 1)
    @check api.git_object_lookup(obj_ptr, r.ptr, id.oid, git_otype(T))
    return T(obj_ptr[1])
end

function lookup{T<:GitObject}(::Type{T}, r::Repository, id::String)
    id_arr  = Array(Uint8, api.OID_RAWSZ)
    @check api.git_oid_fromstrn(id_arr, bytestring(id), length(id))
    obj_ptr = Array(Ptr{Void}, 1)
    if length(id) < api.OID_HEXSZ
        @check api.git_object_lookup_prefix(obj_ptr, r.ptr, 
                                           id_arr, length(id),
                                           git_otype(T))
    else
        @check api.git_object_lookup(obj_ptr, r.ptr, 
                                     id_arr, length(id),
                                     git_otype(T))
    end
    return T(obj_ptr[1]) 
end

function lookup(r::Repository, id::String)
    id_arr  = Array(Uint8, api.OID_RAWSZ)
    @check api.git_oid_fromstrn(id_arr, bytestring(id), length(id))
    obj_ptr = Array(Ptr{Void}, 1)
    if length(id) < api.OID_HEXSZ
        @check api.git_object_lookup_prefix(obj_ptr, r.ptr, 
                                           id_arr, length(id),
                                           api.OBJ_ANY)
    else
        @check api.git_object_lookup(obj_ptr, r.ptr, 
                                     id_arr, length(id),
                                     api.OBJ_ANY)
    end
    return gitobj_from_ptr(obj_ptr[1]) 
end

function lookup(r::Repository, id::Oid)
    @assert r.ptr != C_NULL
    obj_ptr = Array(Ptr{Void}, 1)
    @check api.git_object_lookup(obj_ptr, r.ptr, id.oid, api.OBJ_ANY)
    return gitobj_from_ptr(obj_ptr[1]) 
end

lookup_tree(r::Repository, id::Oid) = lookup(GitTree, r, id)
lookup_tree(r::Repository, id::String) = lookup(GitTree, r, id)
lookup_blob(r::Repository, id::Oid) = lookup(GitBlob, r, id)
lookup_blob(r::Repository, id::String) = lookup(GitBlob, r, id)
lookup_commit(r::Repository, id::Oid) = lookup(GitCommit, r, id)
lookup_commit(r::Repository, id::String) = lookup(GitCommit, r, id)

function lookup_ref(r::Repository, refname::String)
    @assert r.ptr != C_NULL
    bname = bytestring(refname)
    ref_ptr = Array(Ptr{Void}, 1)
    err = api.git_reference_lookup(ref_ptr, r.ptr, bname)
    if err == api.ENOTFOUND
        return nothing
    elseif err != api.GIT_OK
        throw(LibGitError(err))
    end
    return GitReference(ref_ptr[1])
end

function create_ref(r::Repository, refname::String, id::Oid; 
                    force::Bool=false, sig=nothing, msg=nothing)
    @assert r.ptr != C_NULL
    bname = bytestring(refname)
    bmsg  = msg != nothing ? bytestring(msg) : C_NULL
    ref_ptr = Array(Ptr{Void}, 1)
    if sig != nothing
        @assert isa(sig, Signature)
        gsig = git_signature(sig)
        @check ccall((:git_reference_create, api.libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Uint8},
                      Cint, Ptr{api.GitSignature}, Ptr{Cchar}),
                      ref_ptr, r.ptr, bname, id.oid, force? 1:0, &gsig, bmsg)
    else
        @check ccall((:git_reference_create, api.libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Uint8},
                      Cint, Ptr{api.GitSignature}, Ptr{Cchar}),
                      ref_ptr, r.ptr, bname, id.oid, force? 1:0, C_NULL, bmsg)
    end
    return GitReference(ref_ptr[1])
end

function create_ref(r::Repository, refname::String, target::String; 
                    force::Bool=false, sig=nothing, logmsg=nothing)
    create_sym_ref(r, refname, target; force=force, sig=sig, logmsg=logmsg)
end

function create_sym_ref(r::Repository, refname::String, target::String; 
                        force::Bool=false, sig=sig, logmsg=logmsg)
    @assert r.ptr != C_NULL
    bname   = bytestring(refname)
    btarget = bytestring(target)
    bmsg    = logmsg != nothing ? bytestring(logmsg) : C_NULL
    ref_ptr = Array(Ptr{Void}, 1)
    if sig != nothing
        @assert isa(sig, Signature)
        gsig = git_signature(sig)
        @check ccall((:git_reference_symbolic_create, api.libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Uint8},
                      Cint, Ptr{api.GitSignature}, Ptr{Cchar}),
                      ref_ptr, r.ptr, bname, btarget, force? 1:0, &gsig, bmsg)
    else
        @check ccall((:git_reference_symbolic_create, api.libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Cchar},
                      Cint, Ptr{api.GitSignature}, Ptr{Cchar}),
                      ref_ptr, r.ptr, bname, btarget, force? 1:0, C_NULL, bmsg)
    end
    return GitReference(ref_ptr[1])
end


function repo_revparse_single(r::Repository, spec::String)
    @assert r.ptr != C_NULL
    bspec = bytestring(spec)
    obj_ptr = Array(Ptr{Void}, 1)
    @check api.git_revparse_single(obj_ptr, r.ptr, bspec)
    return gitobj_from_ptr(obj_ptr[1])
end

function commit(r::Repository,
                refname::String,
                author::Signature,
                committer::Signature,
                msg::String,
                tree::GitTree,
                parents::GitCommit...)
    @assert r.ptr != C_NULL
    @assert tree.ptr != C_NULL
    commit_oid  = Oid()
    bref = bytestring(refname)
    bmsg = bytestring(msg)
    nparents = convert(Cint, length(parents))
    cparents = Array(Ptr{Void}, nparents)
    if nparents > zero(Cint)
        for (i, commit) in enumerate(parents)
            @assert commit.ptr != C_NULL
            cparents[i] = commit.ptr

        end
    end
    gauthor = git_signature(author)
    gcommitter = git_signature(committer)
    #TODO: encoding?
    err_code = ccall((:git_commit_create, api.libgit2), Cint,
                     (Ptr{Uint8}, Ptr{Void}, Ptr{Cchar}, 
                      Ptr{api.GitSignature}, Ptr{api.GitSignature}, 
                      Ptr{Cchar}, Ptr{Cchar}, Ptr{Void},
                      Csize_t, Ptr{Ptr{Void}}),
                      commit_oid.oid, r.ptr, bref,
                      &gauthor, &gcommitter,
                      C_NULL, bmsg, tree.ptr, 
                      nparents, nparents > 0 ? cparents : C_NULL)
    if err_code < 0
        throw(LibGitError(err_code))
    end
    return commit_oid
end

function repo_set_workdir(r::Repository, dir::String, update::Bool)
end

# filter can be :all, :local, :remote
function branch_names(r::Repository, filter=:all)
    @assert r.ptr != C_NULL
    local git_filter::Cint 
    if filter == :all
        git_filter = api.BRANCH_LOCAL | api.BRANCH_REMOTE
    elseif filter == :local
        git_filter = api.BRANCH_LOCAL
    elseif filter == :remote
        git_filter = api.BRANCH_REMOTE
    else
        throw(ArgumentError("filter can be :all, :local, or :remote"))
    end
    iter_ptr = Array(Ptr{Void}, 1)
    @check api.git_branch_iterator_new(iter_ptr, r.ptr, git_filter)
    branch_ptr = Array(Ptr{Void}, 1)
    branch_type = Array(Cint, 1)
    names = String[]
    while true
        err = api.git_branch_next(branch_ptr, branch_type, iter_ptr[1])
        if err == api.ITEROVER
            break
        end
        if err != api.GIT_OK
            if iter_ptr[1] != C_NULL
                api.git_branch_iterator_free(iter_ptr[1])
            end
            throw(LibGitError(err))
        end
        name_ptr = api.git_reference_shorthand(branch_ptr[1])
        push!(names, bytestring(name_ptr)) 
    end
    api.git_branch_iterator_free(iter_ptr[1])
    return names
end

function lookup(::Type{GitBranch}, r::Repository,
                branch_name::String, branch_type=:local)
    @assert r.ptr != C_NULL
    local git_branch_type::Cint
    if branch_type == :local
        git_branch_type = api.BRANCH_LOCAL
    elseif branch_type == :remote 
        git_branch_type = api.BRANCH_REMOTE
    else
        throw(ArgumentError("branch_type can be :local or :remote"))
    end
    branch_ptr = Array(Ptr{Void}, 1)
    err = api.git_branch_lookup(branch_ptr, r.ptr,
                                 bytestring(branch_name), git_branch_type)
    if err == api.GIT_OK
        return GitBranch(branch_ptr[1])
    elseif err == api.ENOTFOUND
        return nothing
    else
        throw(LibGitError(err))
    end
end

lookup_branch(r::Repository, branch_name::String, branch_type=:local) = 
        lookup(GitBranch, r, branch_name, branch_type)

#lookup_branch(r::Repository, branch_id::Oid, branch_type=:local) = 
#        lookup(GitBranch, r, string(branch_id), branch_type)


function create_branch(r::Repository, n::String, target::Oid;
                       force::Bool=false, sig=nothing, logmsg=nothing)
    @assert r.ptr != C_NULL
    #TODO: give intelligent error msg when target
    # does not exist
    c = lookup_commit(r, target)
    bmsg = logmsg != nothing ? bytestring(logmsg) : C_NULL
    branch_ptr = Array(Ptr{Void}, 1)
    if sig != nothing
        @assert(isa(sig, Signature))
        gsig = git_signature(sig)
        @check ccall((:git_branch_create, api.libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Void},
                      Cint, Ptr{api.GitSignature}, Ptr{Cchar}),
                     branch_ptr, r.ptr, bytestring(n), c.ptr, force? 1:0, &gsig, bmsg)
    else
        @check ccall((:git_branch_create, api.libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Ptr{Void},
                      Cint, Ptr{api.GitSignature}, Ptr{Cchar}),
                     branch_ptr, r.ptr, bytestring(n), c.ptr, force? 1:0, C_NULL, bmsg)
    end
    return GitBranch(branch_ptr[1]) 
end

function create_branch(r::Repository, n::String, target::String="HEAD"; 
                       force::Bool=false, sig=nothing, logmsg=nothing)
    id = rev_parse_oid(r, target)
    return create_branch(r, n, id, force=force, sig=sig, logmsg=logmsg)
end

function create_branch(r::Repository, n::String, target::GitCommit;
                       force::Bool=false, sig=nothing, logmsg=nothing)
    id = Oid(target)
    return create_branch(r, n, id, force=force, sig=sig, logmsg=logmsg)
end

type BranchIterator 
    ptr::Ptr{Void}
    repo::Repository

    function BranchIterator(ptr::Ptr{Void}, r::Repository)
        @assert ptr != C_NULL
        bi = new(ptr, r)
        finalizer(bi, free!)
        return bi
    end
end

free!(b::BranchIterator) = begin
    if b.ptr != C_NULL
        api.git_branch_iterator_free(b.ptr)
        b.ptr = C_NULL
    end
end

function iter_branches(r::Repository, filter=:all)
    @assert r.ptr != C_NULL
    local git_filter::Cint
    if filter == :all
        git_filter = api.BRANCH_LOCAL | api.BRANCH_REMOTE
    elseif filter == :local
        git_filter = api.BRANCH_LOCAL
    elseif filter == :remote
        git_filter == api.BRANCH_REMOTE
    else
        throw(ArgumentError("filter can be :all, :local, or :remote"))
    end 
    iter_ptr = Array(Ptr{Void}, 1)
    @check api.git_branch_iterator_new(iter_ptr, r.ptr, git_filter)
    return BranchIterator(iter_ptr[1], r)
end

Base.start(b::BranchIterator) = begin
    @assert b != C_NULL
    branch_ptr = Array(Ptr{Void}, 1)
    btype_ptr  = Array(Cint, 1)
    ret = api.git_branch_next(branch_ptr, btype_ptr, b.ptr)
    if ret == api.ITEROVER
        return nothing
    elseif ret != api.GIT_OK
        throw(LibGitError(ret))
    end
    return GitBranch(branch_ptr[1])
end

Base.done(b::BranchIterator, state) = begin
    state == nothing
end

Base.next(b::BranchIterator, state) = begin
    @assert b.ptr != C_NULL
    branch_ptr = Array(Ptr{Void}, 1)
    btype_ptr  = Array(Cint, 1)
    ret = api.git_branch_next(branch_ptr, btype_ptr, b.ptr)
    if ret == api.ITEROVER
        return (state, nothing)
    elseif ret != api.GIT_OK
        throw(LibGitError(ret))
    end
    return (state, GitBranch(branch_ptr[1]))
end

#------- Tree merge ---------
function parse_merge_options(opts::Nothing)
    return api.GitMergeTreeOpts()
end

function parse_merge_options(opts::Dict)
    merge_opts = api.GitMergeTreeOpts()
    if isempty(opts)
        return merge_opts
    end
    if haskey(opts, :rename_threshold)
        merge_opts.rename_threshold = convert(Cuint, opts[:rename_threshold])
    end
    if haskey(opts, :target_limit)
        merge_opts.target_limit = convert(Cuint, opts[:target_limit])
    end
    if haskey(opts, :automerge)
        a = opts[:automerge]
        if a == :normal
            merge_opts.flags = api.MERGE_AUTOMERGE_NORMAL
        elseif a == :none
            merge_opts.flags = api.MERGE_AUTOMERGE_NONE
        elseif a == :favor_ours
            merge_opts.flags = api.MERGE_AUTOMERGE_FAVOR_OURS
        elseif a == :favor_theirs
            merge_opts.flags = api.MERGE_AUTOMERGE_FAVOR_THEIRS
        else
            error("Unknown automerge option :$a")
        end
    end
    if get(opts, :renames, false)
        merge_opts.flags |= MERGE_TREE_FIND_RENAMES
    end
    return merge_opts
end

#TODO: tree's should reference owning repository
Base.merge!(r::Repository, t1::GitTree, t2::GitTree, opts=nothing) = begin
    @assert r.ptr != C_NULL t1.ptr != C_NULL && t2.ptr != C_NULL 
    gopts = parse_merge_options(opts)
    idx_ptr = Array(Ptr{Void}, 1)
    @check ccall((:git_merge_trees, api.libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{api.GitMergeTreeOpts}),
                 idx_ptr, r.ptr, C_NULL, t1.ptr, t2.ptr, &gopts)
    return GitIndex(idx_ptr[1])
end

Base.merge!(r::Repository, t1::GitTree, t2::GitTree, ancestor::GitTree, opts=nothing) = begin
    @assert r.ptr != C_NULL t1.ptr != C_NULL && t2.ptr != C_NULL && ancestor.ptr != C_NULL
    gopts = parse_merge_options(opts)
    idx_ptr = Array(Ptr{Void}, 1)
    @check ccall((:git_merge_trees, api.libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{api.GitMergeTreeOpts}),
                 idx_ptr, r.ptr, ancestor.ptr, t1.ptr, t2.ptr, &gopts)
    return GitIndex(idx_ptr[1])
end

#------- Merge Commits -----
function merge_commits(r::Repository, 
                       ours::Union(GitCommit, Oid), 
                       theirs::Union(GitCommit, Oid), 
                       opts=nothing)
    @assert r.ptr != C_NULL && ours.ptr != C_NULL && theirs.ptr != C_NULL
    gopts = parse_merge_options(opts)
    if isa(ours, Oid)
        ours = lookup_commit(r, ours)
    end
    if isa(theirs, Oid)
        theirs = lookup_commit(r, theirs)
    end
    index_ptr = Array(Ptr{Void}, 1)
    @check ccall((:git_merge_commits, api.libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void},
                  Ptr{Void}, Ptr{api.GitMergeTreeOpts}),
                  index_ptr, r.ptr, ours.ptr, theirs.ptr, &gopts)
    return GitIndex(index_ptr[1])
end

#------- Repo Checkout -------
function cb_checkout_progress(path_ptr::Ptr{Cchar}, 
                              completed_steps::Csize_t,
                              total_steps::Csize_t,
                              payload::Ptr{Void})
    callback = unsafe_pointer_to_objref(payload)::Function
    path = path_ptr != C_NULL ? bytestring(path_ptr) : nothing
    callback(path, completed_steps, total_steps)
    return
end

const c_cb_checkout_progress = cfunction(cb_checkout_progress, Void,
                                         (Ptr{Cchar}, Csize_t, Csize_t, Ptr{Void}))

function cb_checkout_notify(why::Cint, 
                            path_ptr::Ptr{Cchar}, 
                            baseline::Ptr{api.GitDiffFile}, 
                            target::Ptr{api.GitDiffFile},
                            workdir::Ptr{api.GitDiffFile},
                            payload::Ptr{Void})
    callback = unsafe_pointer_to_objref(payload)::Function
    path = path_ptr != C_NULL ? bytestring(path_ptr) : nothing
    local reason::Symbol
    if why == api.CHECKOUT_NOTIFY_CONFLICT
        reason = :conflict
    elseif why == api.CHECKOUT_NOTIFY_DIRTY
        reason = :dirty
    elseif why == api.CHECKOUT_NOTIFY_UPDATED
        reason = :updated
    elseif why == api.CHECKOUT_NOTIFY_UNTRACKED
        reason = :untracked
    elseif why == api.CHECKOUT_NOTIFY_IGNORED
        reason = :ignored
    else
        reason = :unknown
    end
    try
        callback(why, path, 
                 DiffFile(baseline),
                 DiffFile(target), 
                 DiffFile(workdir))
        return api.GIT_OK
    catch
        return api.ERROR
    end
end

const c_cb_checkout_notify = cfunction(cb_checkout_notify, Cint,
                                       (Cint,
                                        Ptr{Cchar}, 
                                        Ptr{api.GitDiffFile}, 
                                        Ptr{api.GitDiffFile}, 
                                        Ptr{api.GitDiffFile},
                                        Ptr{Void}))

function parse_checkout_options(opts::Nothing)
    return api.GitCheckoutOpts()
end

function parse_checkout_options(opts::Dict)
    gopts = api.GitCheckoutOpts()
    if isempty(opts)
        return gopts
    end
    if haskey(opts, :progress)
        if !isa(opts[:progress], Function)
            throw(ArgumentError("opts[:progress] must be a Function object"))
        end
        gopts.progress_payload = pointer_from_objref(opts[:progress])
        gopts.progress_cb = c_cb_checkout_progress
    end
    if haskey(opts, :notify)
        if !isa(opts[:notify], Function)
            throw(ArgumentError("opts[:notify] must be a Function object"))
        end
        gopts.notify_payload = pointer_from_objref(opts[:notify])
        gopts.notify_cb = c_cb_checkout_notify
    end
    if haskey(opts, :strategy)
        local strategy::Vector{Symbol}
        if isa(opts[:strategy], Symbol)
            strategy = [opts[:strategy]]
        else
            strategy = opts[:strategy]
        end
        for s in strategy
            if s == :safe
                gopts.checkout_strategy |= api.CHECKOUT_SAFE
            elseif s == :safe_create
                gopts.checkout_strategy |= api.CHECKOUT_SAFE_CREATE
            elseif s == :force
                gopts.checkout_strategy |= api.CHECKOUT_FORCE
            elseif s == :allow_conflicts
                gopts.checkout_strategy |= api.CHECKOUT_ALLOW_CONFLICTS
            elseif s == :remove_untracked
                gopts.checkout_strategy |= api.CHECKOUT_REMOVE_UNTRACKED
            elseif s == :remove_ignored
                gopts.checkout_strategy |= api.CHECKOUT_REMOVE_IGNORED
            elseif s == :update_only
                gopts.checkout_strategy |= api.CHECKOUT_UPDATE_ONLY
            elseif s == :dont_update_index
                gopts.checkout_strategy |= api.CHECKOUT_DONT_UPDATE_INDEX
            elseif s == :no_refresh
                gopts.checkout_strategy |= api.CHECKOUT_NO_REFRESH
            elseif s == :disable_pathspec_match
                gopts.checkout_strategy |= api.CHECKOUT_DISABLE_PATHSPEC_MATCH
            elseif s == :skip_locked_directories
                gopts.checkout_strategy |= api.CHECKOUT_SKIP_LOCKED_DIRECTORIES
            elseif s == :skip_unmerged
                gopts.checkout_strategy |= api.CHECKOUT_SKIP_UNMERGED
            elseif s == :use_ours
                gopts.checkout_strategy |= api.CHECKOUT_USE_OURS
            elseif s == :use_theirs
                gopts.checkout_strategy |= api.CHECKOUT_USE_THEIRS
            elseif s == :update_submodules
                gopts.checkout_strategy |= api.CHECKOUT_UPDATE_SUBMODULES
            elseif s == :update_submodules_if_changed
                gopts.checkout_strategy |= api.CHECKOUT_UPDATE_SUBMODULES_IF_CHANGED
            else
                throw(ArgumentError("unknown checkout strategy flag :$s"))
            end
        end
    end

    if haskey(opts, :notify_flags)
        local flags::Vector{Symbol}
        if isa(opts[:notify_flags], Symbol)
            flags = [opts[:notify_flags]]
        else
            flags = opts[:notify_flags]
        end
        for f in flags
            if f == :conflict
                gopts.notify_flags |= api.CHECKOUT_NOTIFY_CONFLICT
            elseif f == :dirty
                gopts.notify_flags |= api.CHECKOUT_NOTIFY_DIRTY
            elseif f == :updated
                gopts.notify_flags |= api.CHECKOUT_NOTIFY_UPDATED
            elseif f == :untracked
                gopts.notify_flags |= api.CHECKOUT_NOTIFY_UNTRACKED
            elseif f == :ignored
                gopts.notify_flags |= api.CHECKOUT_NOTIFY_IGNORED
            elseif f == :all
                gopts.notify_flags |= api.CHECKOUT_NOTIFY_ALL
            else
                throw(ArgumentError("unknown checkout notify flag :$f"))
            end
        end
    end

    gopts.disable_filters = convert(Cint, get(opts, :disable_filters, false) ? 1 : 0)
    
    if haskey(opts, :dir_mode)
        gopts.dir_mode = convert(Cuint, opts[:dir_mode])
    end
    if haskey(opts, :file_mode)
        gopts.file_mode = convert(Cuint, opts[:file_mode])
    end
    if haskey(opts, :file_open_flags)
        gopts.file_open_flags = convert(Cint, opts[:file_open_flags])
    end
    if haskey(opts, :target_directory)
        gopts.target_directory = convert(Ptr{Cchar}, 
					 pointer(bytestring(opts[:target_directory])))
    end
    if haskey(opts, :baseline)
        if isa(opts[:baseline], GitTree)
            gopts.baseline = opts[:baseline].ptr
        else
            throw(ArgumentError("checkout options :baseline should be a GitTree"))
        end
    end
    if haskey(opts, :paths)
        paths = opts[:paths]
        if isa(paths, String)
            paths = [paths]
        end
        npaths = length(paths)
        cpaths = Array(Ptr{Cchar}, npaths)
        gopts.paths_count = convert(Csize_t, npaths)
        for i in 1:npaths
            cpaths[i] = convert(Ptr{Cchar}, pointer(paths[i]))
        end
        gopts.paths_strings = convert(Ptr{Ptr{Cchar}}, cpaths)
    end
    return gopts
end

typealias Treeish Union(GitCommit, GitTag, GitTree)

function checkout_tree!(r::Repository, tree::String, opts=nothing)
    t = rev_parse(r, tree)
    return checkout_tree!(r, t, opts)
end

function checkout_tree!(r::Repository, tree::Treeish, opts=nothing)
    gopts = parse_checkout_options(opts)
    err = ccall((:git_checkout_tree, api.libgit2), Cint,
                (Ptr{Void}, Ptr{Void}, Ptr{api.GitCheckoutOpts}),
                r.ptr, tree.ptr, &gopts)
    #TODO: memory leak with option strings
    if err != api.GIT_OK
        throw(LibGitError(err))
    end
    return r 
end

function checkout_head!(r::Repository, opts=nothing)
    @assert r.ptr != C_NULL
    gopts = parse_checkout_options(opts)
    err = ccall((:git_checkout_head, api.libgit2), Cint,
                (Ptr{Void}, Ptr{api.GitCheckoutOpts}),
                r.ptr, &gopts)
    #TODO: memory leak with option strings??, cleanup on exceptions, etc...
    if err != api.GIT_OK
        throw(GitError(err))
    end
    return r
end

function checkout!(r::Repository, target, opts={})
    if !haskey(opts, :strategy)
        opts[:strategy] = :safe
    end
    delete!(opts, :paths)
    if target == "HEAD"
        return checkout_head!(r, opts)
    end
    local branch
    if isa(target, GitBranch)
        branch = target
    else
        branch = lookup_branch(r, string(target), :local)
        if branch == nothing
            branch = lookup_branch(r, string(target), :remote)
        end
    end
    if branch != nothing
        checkout_tree!(r, tip(branch), opts)
        if isremote(branch)
            create_ref(r, "HEAD", Oid(tip(branch)), force=true)
        else
            create_ref(r, "HEAD", canonical_name(branch), force=true)
        end
    else
        commit = lookup_commit(r, rev_parse_oid(r, target))
        create_ref(r, "HEAD", Oid(commit), force=true)
        checkout_tree!(r, commit, opts)
    end
end


#------- Repo Clone -------
abstract GitCredential

type CredDefault <: GitCredential end

type CredPlainText <: GitCredential
    username::String
    password::String
end

type CredSSHKey <: GitCredential
    username::Union(Nothing, String)
    publickey::Union(Nothing, String)
    privatekey::String
    passphrase::Union(Nothing, String)
end

function cb_remote_transfer(stats_ptr::Ptr{api.GitTransferProgress},
                            payload_ptr::Ptr{Void})
    stats = unsafe_load(stats_ptr)
    payload = unsafe_pointer_to_objref(payload_ptr)::Dict
    callback = payload[:callbacks][:transfer_progress]
    try
        callback(stats.total_objects,
                 stats.indexed_objects,
                 stats.received_objects,
                 stats.received_bytes)
        return api.GIT_OK
    catch err
        payload[:exception] = err
        return api.ERROR
    end
end

const c_cb_remote_transfer = cfunction(cb_remote_transfer, Cint,
                                       (Ptr{api.GitTransferProgress}, Ptr{Void}))

function extract_cred!(cred::GitCredential, cred_ptr::Ptr{Ptr{Void}}, allowed_types::Cuint)
    if isa(cred, CredPlainText)
        if !bool(allowed_types & api.CREDTYPE_USERPASS_PLAINTEXT)
            error("invalid credential type")
        end
        @check ccall((:git_cred_userpass_plaintext_new, api.libgit2), Cint,
                      (Ptr{Ptr{Void}}, Ptr{Cchar}, Ptr{Cchar}),
                       cred_ptr,
                       bytestring(cred.username),
                       bytestring(cred.password))
    elseif isa(cred, CredSSHKey)
        if !bool(allowed_types & api.CREDTYPE_SSH_KEY)
            error("invalid credential type")
        end
        @check ccall((:git_cred_ssh_key_new, api.libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Cchar}, Ptr{Cchar}, Ptr{Cchar}, Ptr{Cchar}),
                     cred_ptr,
                     cred.username != nothing ? cred.username : C_NULL,
                     cred.publickey != nothing ? cred.publickey : C_NULL,
                     cred.privatekey != nothing ? cred.privatekey : C_NULL,
                     cred.passphrase != nothing ? cred.passphrase : C_NULL)
    elseif (cred, CredDefault)
        if !bool(allowed_types & api.CREDTYPE_SSH_KEY)
            error("invalid credential type")
        end
        @check ccall((:git_cred_default_new, api.libgit2), Cint,
                     (Ptr{Ptr{Void}},), cred_ptr)
    else
        error("invalid credential type")
    end
    return nothing
end

function cb_default_remote_credentials(cred_ptr::Ptr{Ptr{Void}},
                                       url::Ptr{Cchar},
                                       username_from_url::Ptr{Cchar},
                                       allowed_types::Cuint,
                                       payload_ptr::Ptr{Void})
    payload = unsafe_pointer_to_objref(payload_ptr)::Dict
    cred = payload[:credentials]
    try
        extract_cred!(cred, cred_ptr, allowed_types)
        return api.GIT_OK
    catch err
        payload[:exception] = err
        return api.ERROR
    end
end

const c_cb_default_remote_credentials = cfunction(cb_default_remote_credentials, Cint,
                                                  (Ptr{Ptr{Void}}, Ptr{Cchar}, Ptr{Cchar}, Cuint, Ptr{Void}))

function cb_remote_credentials(cred_ptr::Ptr{Ptr{Void}},
                               url::Ptr{Cchar},
                               username::Ptr{Cchar},
                               allowed_types::Cuint,
                               payload_ptr::Ptr{Void})
    payload = unsafe_pointer_to_objref(payload_ptr)::Dict
    cred_func = payload[:credentials]::Function
    types = Symbol[]
    if bool(allowed_types & api.CREDTYPE_USERPASS_PLAINTEXT)
        push!(types, :plaintext)
    end
    if bool(allowed_types & api.CREDTYPE_SSH_KEY)
        push!(types, :sshkey)
    end
    if bool(allowed_types & api.CREDTYPE_DEFAULT)
        push!(types, :default)
    end 
    try
        cred = cred_func(url != C_NULL ? bytestring(url) : nothing,
                         username != C_NULL ? bytestring(username) : nothing,
                         types)
        #TODO: better error msg
        if !(isa(cred, GitCredential))
            error("returned credential is not a git credential subtype")
        end
        extract_cred!(cred, cred_ptr, allowed_types)
        return api.GIT_OK
    catch err
        payload[:exception] = err
        return api.ERROR
    end
end

const c_cb_remote_credential = cfunction(cb_remote_credentials, Cint,
                                         (Ptr{Ptr{Void}}, Ptr{Cchar}, Ptr{Cchar}, Cuint, Ptr{Void}))

function parse_clone_options(opts, payload::Dict)
    gopts = api.GitCloneOpts()
    if opts == nothing || isempty(opts)
        return gopts
    end
    if haskey(opts, :bare)
        gopts.bare = convert(Cint, opts[:bare] ? 1 : 0)
    end
    if haskey(opts, :credentials)
        cred = opts[:credentials]
        if isa(cred, GitCredential)
            payload[:credentials] = cred
            gopts.remote_credentials_cb = convert(Ptr{Void}, c_cb_default_remote_credentials)
        elseif isa(cred, Function)
            payload[:credentials] = cred
            gopts.remote_credentials_cb = convert(Ptr{Void}, c_cb_remote_credential)
        else
            throw(ArgumentError("clone option :credentials must be a GitCredential or Function type"))
        end
    end
    if haskey(opts, :callbacks)
        callbacks = opts[:callbacks]
        if haskey(callbacks, :transfer_progress)
            if !isa(callbacks[:transfer_progress], Function)
                throw(ArgumentError("clone callback :transfer_progress must be a Function"))
            end
            payload[:callbacks] = callbacks
            gopts.remote_transfer_progress_cb = convert(Ptr{Void}, c_cb_remote_transfer)
        end
    end
    gopts.remote_payload = convert(Ptr{Void}, pointer_from_objref(payload))
    return gopts
end

function repo_clone(url::String, path::String, opts=nothing)
    # we initalize the payload here as pointer_from_objref
    # hides the object from julia's gc.  A reference in this
    # function's scope allows the object to be preserved
    # for the lifetime of the function call.
    gpayload = Dict()
    gopts    = parse_clone_options(opts, gpayload)
    repo_ptr = Array(Ptr{Void}, 1)
    err = ccall((:git_clone, api.libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Cchar}, Ptr{Cchar}, Ptr{api.GitCloneOpts}),
                 repo_ptr, bytestring(url), bytestring(path), C_NULL)
    if err != api.GIT_OK
        throw(LibGitError(err))
    end
    return Repository(repo_ptr[1])
end



#------- Tree Builder -------
type TreeBuilder
    ptr::Ptr{Void}
    repo::Repository
    
    function TreeBuilder(ptr::Ptr{Void}, r::Repository)
        @assert ptr != C_NULL
        bld = new(ptr, r)
        finalizer(bld, free!)
        return bld
    end
end

free!(t::TreeBuilder) = begin
    if t.ptr != C_NULL
        api.git_treebuilder_free(t.ptr)
        t.ptr = C_NULL
    end
end

Base.insert!(t::TreeBuilder, filename::String,
                 id::Oid, filemode::Int) = begin
    @assert t.ptr != C_NULL
    bfilename = bytestring(filename)
    cfilemode = convert(Cint, filemode)
    @check api.git_treebuilder_insert(C_NULL,
                                      t.ptr,
                                      bfilename, 
                                      id.oid, 
                                      cfilemode)
    return t
end

function write!(t::TreeBuilder)
    @assert t.ptr != C_NULL
    @assert t.repo.ptr != C_NULL
    oid_arr = Array(Uint8, api.OID_RAWSZ)
    @check api.git_treebuilder_write(oid_arr, t.repo.ptr, t.ptr)
    return Oid(oid_arr)
end

function repo_treebuilder(r::Repository)
    @assert r.ptr != C_NULL
    bld_ptr = Array(Ptr{Void}, 1)
    @check api.git_treebuilder_create(bld_ptr, C_NULL)
    return TreeBuilder(bld_ptr[1], r)
end

TreeBuilder(r::Repository) = repo_treebuilder(r)

#-------- Reference Iterator --------
#TODO: handle error's when iterating (see branch)
type ReferenceIterator
    ptr::Ptr{Void}
    repo::Repository

    function ReferenceIterator(ptr::Ptr{Void}, r::Repository)
        @assert ptr != C_NULL
        ri = new(ptr, r)
        finalizer(ri, free!)
        return ri
    end
end

free!(r::ReferenceIterator) = begin
    if r.ptr != C_NULL
        api.git_reference_iterator_free(r.ptr)
        r.ptr = C_NULL
    end
end

function ref_names(r::Repository, glob=nothing)
    rnames = String[]
    for r in iter_refs(r, glob)
        push!(rnames, name(r))
    end
    return rnames
end

function iter_refs(r::Repository, glob=nothing)
    @assert r.ptr != C_NULL
    iter_ptr = Array(Ptr{Void}, 1)
    if glob == nothing
        @check api.git_reference_iterator_new(iter_ptr, r.ptr)
    else
        bglob = bytestring(glob)
        @check api.git_reference_iterator_glob_new(iter_ptr, r.ptr, bglob)
    end
    return ReferenceIterator(iter_ptr[1], r)
end

Base.start(r::ReferenceIterator) = begin
    @assert r != C_NULL
    ref_ptr = Array(Ptr{Void}, 1)
    ret = api.git_reference_next(ref_ptr, r.ptr)
    if ret == api.ITEROVER
        return nothing
    end
    return GitReference(ref_ptr[1])
end

Base.done(r::ReferenceIterator, state) = begin
    state == nothing
end

Base.next(r::ReferenceIterator, state) = begin
    @assert r.ptr != C_NULL
    ref_ptr = Array(Ptr{Void}, 1)
    ret = api.git_reference_next(ref_ptr, r.ptr)
    if ret == api.ITEROVER
        return (state, nothing)
    end
    return (state, GitReference(ref_ptr[1]))
end
