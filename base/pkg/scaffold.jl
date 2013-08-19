module Scaffold

using Base.Git, ..Dir, ..Read

function scaffold(
    pkg::String;
    license = nothing,
    years::Union(Int,String) = readchomp(`date +%Y`),
    authors::String = readchomp(`git config --global --get user.name`),
)
    license === nothing && error("""
    No license chosen -- you must choose a license, e.g.:

        julia> Pkg.scaffold("$pkg", license="MIT")
    """)
    haskey(LICENSES,license) ||
        error("$license is not a known license choice.")
    avail = Dir.cd(Read.available)
    haskey(avail,pkg) &&
        error("$pkg is already a registered package name.")
    d = Dir.path(pkg)
    ispath(d) &&
        error("$d exists, refusing to overwrite.")
    mkpath(d)
    try cd(d) do
            info("Initializing $pkg repo")
            Git.run(`init -q`)
            info("Generating $pkg/LICENSE.md")
            open("LICENSE.md","w") do f
                print(f, LICENSES[license](pkg, string(years), authors))
            end
            info("Generating $pkg/README.md")
            open("README.md","w") do f
                # TODO: add some content?
            end
            mkdir("src")
            open("src/$pkg.jl", "w") do f
                # TODO: add some content?
            end
            info("Initial commit of $pkg")
            Git.run(`add LICENSE.md README.md src/$pkg.jl`)
            Git.run(`commit -q -m "$pkg.jl scaffold with $license license."`)
        end
    catch
        run(`rm -rf $d`)
        rethrow()
    end
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

const LICENSES = [ "MIT" => mit ]

end # module
