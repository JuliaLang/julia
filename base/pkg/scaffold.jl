module Scaffold

using Base.Git, ..Dir

function scaffold(
    pkg::String;
    license::Symbol = :__unspecified__,
    years::Union(Int,String) = readchomp(`date +%Y`),
    authors::String = readchomp(`git config --global --get user.name`),
)
    if license == :__unspecified__
        error("""
        No license chosen -- you must choose a license, e.g.:

            julia> Pkg.scaffold("$pkg", license=:MIT)
        """)
    end
    if !haskey(LICENSES,license)
        error("$license is not a known license option.")
    end
    license_text = LICENSES[license](pkg,string(years),authors)
    # d = Dir.path(pkg)
end

mit(pkg::String, years::String, authors::String) = """
The $pkg.jl package is licensed under the MIT License:

> Copyright (c) $years: $authors.
>
> Permission is hereby granted, free of charge, to any person obtaining
> a copy of this software and associated documentation files (the
> "Software"), to deal in the Software without restriction, including
> without limitation the rights to use, copy, modify, merge, publish,
> distribute, sublicense, and/or sell copies of the Software, and to
> permit persons to whom the Software is furnished to do so, subject to
> the following conditions:
>
> The above copyright notice and this permission notice shall be
> included in all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
> EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
> MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
> NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
> LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
> OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
> WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
"""

const LICENSES = [ :MIT => mit ]

end # module
