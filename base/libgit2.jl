module LibGit2

type LibGitThreadsHandle

    function LibGitThreadsHandle()
        handle = new()
        finalizer(handle, h -> begin
            err = ccall((:git_threads_shutdown, @unix? :libgit2 : :git2), Void, ())
        end)
        return handle
    end
end

function __init__()
    err = ccall((:git_threads_init, @unix? :libgit2 : :git2), Cint, ())
    if err != zero(Cint)
        error("could not initialize LibGit2 library")
    end
end

__threads_handle = LibGitThreadsHandle()

include("libgit2/api.jl")
include("libgit2/macros.jl")
include("libgit2/error.jl")
include("libgit2/oid.jl")
include("libgit2/types.jl")
include("libgit2/object.jl")
include("libgit2/config.jl")
include("libgit2/tree.jl")
include("libgit2/index.jl")
include("libgit2/signature.jl")
include("libgit2/commit.jl")
include("libgit2/tag.jl")
include("libgit2/blob.jl")
include("libgit2/reference.jl")
include("libgit2/odb.jl")
include("libgit2/branch.jl")
include("libgit2/note.jl")
include("libgit2/remote.jl")
include("libgit2/repository.jl")
include("libgit2/diff.jl")
include("libgit2/patch.jl")
include("libgit2/walker.jl")

end # module
