# This file is a part of Julia. License is MIT: https://julialang.org/license

# file downloading

if Sys.iswindows()
    function download(url::AbstractString, filename::AbstractString)
        curl = joinpath(Sys.BINDIR,"curl.exe")
        run(`$curl -g -L -f -o $filename $url`)
        filename
    end
else
    function download(url::AbstractString, filename::AbstractString)
        if Sys.which("curl") !== nothing
            run(`curl -g -L -f -o $filename $url`)
        elseif Sys.which("wget") !== nothing
            try
                run(`wget -O $filename $url`)
            catch
                rm(filename, force=true)  # wget always creates a file
                rethrow()
            end
        elseif Sys.which("fetch") !== nothing
            run(`fetch -f $filename $url`)
        else
            error("no download agent available; install curl, wget, or fetch")
        end
        filename
    end
end
function download(url::AbstractString)
    filename = tempname()
    download(url, filename)
end

"""
    download(url::AbstractString, [localfile::AbstractString])

Download a file from the given url, optionally renaming it to the given local file name. If
no filename is given this will download into a randomly-named file in your temp directory.
Note that this function relies on the availability of external tools such as `curl`, `wget`
or `fetch` to download the file and is provided for convenience. For production use or
situations in which more options are needed, please use a package that provides the desired
functionality instead.

Returns the filename of the downloaded file.
"""
download(url, filename)
