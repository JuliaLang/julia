# This file is a part of Julia. License is MIT: https://julialang.org/license

# file downloading

downloadcmd = nothing
if Sys.iswindows()
    downloadcmd = :powershell
    function download(url::AbstractString, filename::AbstractString)
        ps = "C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\powershell.exe"
        tls12 = "[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]::Tls12"
        client = "New-Object System.Net.Webclient"
        # in the following we escape ' with '' (see https://ss64.com/ps/syntax-esc.html)
        downloadfile = "($client).DownloadFile('$(replace(url, "'" => "''"))', '$(replace(filename, "'" => "''"))')"
        run(`$ps -NoProfile -Command "$tls12; $downloadfile"`)
        filename
    end
else
    function download(url::AbstractString, filename::AbstractString)
        global downloadcmd
        if downloadcmd === nothing
            for checkcmd in (:curl, :wget, :fetch)
                if success(pipeline(`which $checkcmd`, devnull))
                    downloadcmd = checkcmd
                    break
                end
            end
        end
        if downloadcmd == :wget
            try
                run(`wget -O $filename $url`)
            catch
                rm(filename)  # wget always creates a file
                rethrow()
            end
        elseif downloadcmd == :curl
            run(`curl -g -L -f -o $filename $url`)
        elseif downloadcmd == :fetch
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

Download a file from the given url, optionally renaming it to the given local file name.
Note that this function relies on the availability of external tools such as `curl`, `wget`
or `fetch` to download the file and is provided for convenience. For production use or
situations in which more options are needed, please use a package that provides the desired
functionality instead.
"""
download(url, filename)
