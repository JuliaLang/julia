# This file is a part of Julia. License is MIT: https://julialang.org/license

const DOWNLOAD_HOOKS = Callable[]

function download_url(url::AbstractString)
    for hook in DOWNLOAD_HOOKS
        url = String(hook(url)::AbstractString)
    end
    return url
end

Downloads() = require(PkgId(
        UUID((0xf43a241f_c20a_4ad4, 0x852c_f6b1247861c6)),
        "Downloads",
    ))

"""
    download(url::AbstractString, [path::AbstractString = tempname()]) -> path

Download a file from the given url, saving it to the location `path`, or if not
specified, a temporary path. Returns the path of the downloaded file.

!!! note
    Since Julia 1.6, this function is deprecated and is just a thin wrapper
    around `Downloads.download`. In new code, you should use that function
    directly instead of calling this.
"""
function download(url::AbstractString, path::AbstractString)
    depwarn("Base.download is deprecated; use Downloads.download instead", :download)
    invokelatest(Downloads().download, download_url(url), path)
end
function download(url::AbstractString)
    depwarn("Base.download is deprecated; use Downloads.download instead", :download)
    invokelatest(Downloads().download, download_url(url))
end
