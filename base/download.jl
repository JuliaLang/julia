# This file is a part of Julia. License is MIT: https://julialang.org/license

# file downloading

if Sys.iswindows()
    function download(url::AbstractString, filename::AbstractString)
        ps = "C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\powershell.exe"
        tls12 = "[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]::Tls12"
        client = "New-Object System.Net.Webclient"
        # in the following we escape ' with '' (see https://ss64.com/ps/syntax-esc.html)
        downloadfile = "($client).DownloadFile('$(replace(url, "'" => "''"))', '$(replace(filename, "'" => "''"))')"
        # PowerShell v3 or later is required for Tls12
        proc = run(pipeline(`$ps -Version 3 -NoProfile -Command "$tls12; $downloadfile"`; stderr=stderr); wait=false)
        if !success(proc)
            if proc.exitcode % Int32 == -393216
                # appears to be "wrong version" exit code, based on
                # https://docs.microsoft.com/en-us/azure/cloud-services/cloud-services-startup-tasks-common
                error("Downloading files requires Windows Management Framework 3.0 or later.")
            end
            pipeline_error(proc)
        end
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

Download a file from the given url, optionally renaming it to the given local file name.
Note that this function relies on the availability of external tools such as `curl`, `wget`
or `fetch` to download the file and is provided for convenience. For production use or
situations in which more options are needed, please use a package that provides the desired
functionality instead.
"""
download(url, filename)
