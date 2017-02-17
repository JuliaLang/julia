isdefined(LibGit2, :discover) ||
@eval LibGit2 function discover(
    start_path::AbstractString = pwd();
    ceiling::Union{AbstractString,Vector} = "",
    across_fs::Bool = false,
)
    sep = @static is_windows() ? ";" : ":"
    ceil = ceiling isa AbstractString ? ceiling :
        join(convert(Vector{String}, ceiling), sep)
    buf_ref = Ref(Buffer())
    @check ccall((:git_repository_discover, :libgit2), Cint,
                 (Ptr{Buffer}, Cstring, Cint, Cstring),
                 buf_ref, start_path, across_fs, ceil)
    buf = buf_ref[]
    str = unsafe_string(buf.ptr, buf.size)
    free(buf_ref)
    return str
end
