# This file is a part of Julia. License is MIT: https://julialang.org/license

# file downloading

if Sys.iswindows()
    function download_powershell(url::AbstractString, filename::AbstractString)
        ps = joinpath(get(ENV, "SYSTEMROOT", "C:\\Windows"), "System32\\WindowsPowerShell\\v1.0\\powershell.exe")
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
                @error "Downloading files requires Windows Management Framework 3.0 or later."
            end
            pipeline_error(proc)
        end
        return filename
    end
end

function find_curl()
    if Sys.isapple() && Sys.isexecutable("/usr/bin/curl")
        "/usr/bin/curl"
    elseif Sys.iswindows() && Sys.isexecutable(joinpath(get(ENV, "SYSTEMROOT", "C:\\Windows"), "System32\\curl.exe"))
        joinpath(get(ENV, "SYSTEMROOT", "C:\\Windows"), "System32\\curl.exe")
    elseif !Sys.iswindows() && Sys.which("curl") !== nothing
        "curl"
    else
        nothing
    end
end

function download_curl(curl_exe::AbstractString, url::AbstractString, filename::AbstractString)
    err = PipeBuffer()
    process = run(pipeline(`$curl_exe -s -S -g -L -f -o $filename $url`, stderr=err), wait=false)
    if !success(process)
        error_msg = readline(err)
        @error "Download failed: $error_msg"
        pipeline_error(process)
    end
    return filename
end

const DOWNLOAD_HOOKS = Callable[]

function download_url(url::AbstractString)
    for hook in DOWNLOAD_HOOKS
        url = String(hook(url)::AbstractString)
    end
    return url
end

function download(url::AbstractString, filename::AbstractString)
    url = download_url(url)
    curl_exe = find_curl()
    if curl_exe !== nothing
        return download_curl(curl_exe, url, filename)
    elseif Sys.iswindows()
        return download_powershell(url, filename)
    elseif Sys.which("wget") !== nothing
        try
            run(`wget -O $filename $url`)
        catch
            rm(filename, force=true)  # wget always creates a file
            rethrow()
        end
    elseif Sys.which("busybox") !== nothing
        try
            run(`busybox wget -O $filename $url`)
        catch
            rm(filename, force=true)  # wget always creates a file
            rethrow()
        end
    elseif Sys.which("fetch") !== nothing
        run(`fetch -f $filename $url`)
    else
        error("No download agent available; install curl, wget, busybox or fetch.")
    end
    return filename
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
