export Repository, GitObject, GitAny, GitBlob, GitCommit, GitTag,
       GitTree, GitReference, GitBranch, GitRemote, Sym

# --------------
# Git Repository
# --------------
type Repository
    ptr::Ptr{Void}
    
    function Repository(ptr::Ptr{Void}, owns::Bool=true)
        if ptr == C_NULL
            throw(ArgumentError("Repository initialized with NULL pointer"))
        end
        this = new(ptr)
        owns && finalizer(this, free!)
        return this
    end
end

free!(r::Repository) = begin
    if r.ptr != C_NULL
        close(r)
        api.git_repository_free(r.ptr)
        r.ptr = C_NULL
    end
end


# -------------
# Git Objects
# -------------
abstract GitObject

free!(o::GitObject) = begin
    if o.ptr != C_NULL
        api.git_object_free(o.ptr)
        o.ptr = C_NULL
    end
end

immutable GitAny <: GitObject end

type GitBlob <: GitObject
    ptr::Ptr{Void}
    
    function GitBlob(ptr::Ptr{Void})
        @assert ptr != C_NULL
        this = new(ptr)
        finalizer(this, free!)
        return this 
    end
end


type GitCommit <: GitObject
    ptr::Ptr{Void}

    function GitCommit(ptr::Ptr{Void})
        @assert ptr != C_NULL
        this = new(ptr)
        finalizer(this, free!)
        return this
    end
end

type GitTag <: GitObject
    ptr::Ptr{Void}

    function GitTag(ptr::Ptr{Void})
        @assert ptr != C_NULL 
        this = new(ptr)
        finalizer(this, free!)
        return this
    end
end

type GitTree <: GitObject
    ptr::Ptr{Void}

    function GitTree(ptr::Ptr{Void})
        @assert ptr != C_NULL
        this = new(ptr)
        finalizer(this, free!)
        return this
    end
end

git_otype(::Type{GitAny})     = api.OBJ_ANY
git_otype(::Type{GitBlob})    = api.OBJ_BLOB
git_otype(::Type{GitCommit})  = api.OBJ_COMMIT
git_otype(::Type{GitTag})     = api.OBJ_TAG 
git_otype(::Type{GitTree})    = api.OBJ_TREE
git_otype{T<:GitObject}(o::T) = git_otype(T)

# ---------------
# Git Reftypes
# ---------------
immutable Sym end 
const RefType = Union(Oid, Sym)

type GitReference{T<:RefType}
    ptr::Ptr{Void}
end

free!(r::GitReference) = begin
    if r.ptr != C_NULL
        api.git_reference_free(r.ptr)
        r.ptr = C_NULL
    end
end

type GitBranch #<: GitReference{Sym}
    ptr::Ptr{Void}
    
    function GitBranch(ptr::Ptr{Void})
        @assert ptr != C_NULL
        this = new(ptr)
        finalizer(this, free!)
        return this
    end
end

free!(b::GitBranch) = begin
    if b.ptr != C_NULL
        api.git_reference_free(b.ptr)
        b.ptr = C_NULL
    end
end

# ---------------
# Git Remote
# ---------------
type GitRemote
    ptr::Ptr{Void}

    function GitRemote(ptr::Ptr{Void})
        @assert ptr != C_NULL
        r = new(ptr)
        finalizer(r, free!)
        return r
    end
end

free!(r::GitRemote) = begin
    if r.ptr != C_NULL
        api.git_remote_free(r.ptr)
        r.ptr = C_NULL
    end
end
