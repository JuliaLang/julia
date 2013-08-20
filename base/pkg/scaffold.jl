module Scaffold

using Base.Git, ..Dir, ..Read

function scaffold(
    pkg::String;
    license = nothing,
    years::Union(Int,String) = readchomp(`date +%Y`),
    authors::String = Git.readchomp(`config --global --get user.name`),
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
                print(f, "module $pkg\n\nend # module\n")
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
The $pkg.jl package is licensed under the MIT Expat License:

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
> MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
> IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
> CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
> TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
> SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

"""

bsd(pkg::String, years::String, authors::String) = """
The $pkg.jl package is licensed under the Simplified BSD License:

> Copyright (c) $years: $authors.
>
> Redistribution and use in source and binary forms, with or without
> modification, are permitted provided that the following conditions are
> met:
>
> 1. Redistributions of source code must retain the above copyright
>    notice, this list of conditions and the following disclaimer.
> 2. Redistributions in binary form must reproduce the above copyright
>    notice, this list of conditions and the following disclaimer in the
>    documentation and/or other materials provided with the distribution.
>
> THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
> "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
> LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
> A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
> OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
> SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
> LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
> DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
> THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
> (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
> OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

"""

const LICENSES = [ "MIT" => mit, "BSD" => bsd ]

end # module
