module GitTools

using ..Pkg3
import LibGit2
using Printf

Base.@kwdef mutable struct MiniProgressBar
    max::Float64 = 1
    header::String = ""
    color::Symbol = :white
    width::Int = 40
    current::Float64 = 0.0
    prev::Float64 = 0.0
    has_shown::Bool = false
end

const PROGRESS_BAR_PERCENTAGE_GRANULARITY = Ref(0.1)

function showprogress(io::IO, p::MiniProgressBar)
    perc = p.current / p.max * 100
    prev_perc = p.prev / p.max * 100
    # Bail early if we are not updating the progress bar,
    # Saves printing to the terminal
    if p.has_shown && !((perc - prev_perc) > PROGRESS_BAR_PERCENTAGE_GRANULARITY[])
        return
    end
    p.prev = p.current
    p.has_shown = true
    n_filled = ceil(Int, p.width * perc / 100)
    n_left = p.width - n_filled
    print(io, "    ")
    printstyled(io, p.header, color=p.color, bold=true)
    print(io, " [")
    print(io, "="^n_filled, ">")
    print(io, " "^n_left, "]  ", )
    @printf io "%2.1f %%" perc
    print(io, "\r")
end

function transfer_progress(progress::Ptr{LibGit2.TransferProgress}, p::Any)
    progress = unsafe_load(progress)
    @assert haskey(p, :transfer_progress)
    bar = p[:transfer_progress]
    @assert typeof(bar) == MiniProgressBar
    if progress.total_deltas != 0
        bar.header = "Resolving Deltas:"
        bar.max = progress.total_deltas
        bar.current = progress.indexed_deltas
    else
        bar.max = progress.total_objects
        bar.current = progress.received_objects
    end
    showprogress(stdout, bar)
    return Cint(0)
end

function clone(url, source_path; header=nothing, kwargs...)
    Pkg3.Types.printpkgstyle(stdout, :Cloning, header == nothing ? "git-repo `$url`" : header)
    transfer_payload = MiniProgressBar(header = "Fetching:", color = Base.info_color())
    callbacks = LibGit2.Callbacks(
        :transfer_progress => (
            @cfunction(transfer_progress, Cint, (Ptr{LibGit2.TransferProgress}, Any)),
            transfer_payload,
        )
    )
    print(stdout, "\e[?25l") # disable cursor
    try
        return LibGit2.clone(url, source_path; callbacks=callbacks, kwargs...)
    catch e
        rm(source_path; force=true, recursive=true)
        rethrow(e)
    finally
        print(stdout, "\033[2K") # clear line
        print(stdout, "\e[?25h") # put back cursor
    end
end

function fetch(repo::LibGit2.GitRepo, remoteurl=nothing; header=nothing, kwargs...)
    if remoteurl === nothing
        remoteurl = LibGit2.with(LibGit2.get(LibGit2.GitRemote, repo, "origin")) do remote
            LibGit2.url(remote)
        end
    end
    Pkg3.Types.printpkgstyle(stdout, :Updating, header == nothing ? "git-repo `$remoteurl`" : header)
    transfer_payload = MiniProgressBar(header = "Fetching:", color = Base.info_color())
    callbacks = LibGit2.Callbacks(
        :transfer_progress => (
            @cfunction(transfer_progress, Cint, (Ptr{LibGit2.TransferProgress}, Any)),
            transfer_payload,
        )
    )
    print(stdout, "\e[?25l") # disable cursor
    try
        return LibGit2.fetch(repo; remoteurl=remoteurl, callbacks=callbacks, kwargs...)
    finally
        print(stdout, "\033[2K") # clear line
        print(stdout, "\e[?25h") # put back cursor
    end
end

end # module
