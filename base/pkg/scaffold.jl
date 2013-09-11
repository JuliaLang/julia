module Scaffold

using Base.Git, ..Dir, ..Read

function generate(f::Function, path::String)
    info("Generating $path")
    mkpath(dirname(path))
    open(f, path, "w")
    Git.run(`add $path`)
end

function scaffold(
    pkg::String;
    dir::String = "",
    license::String = "",
    years::Union(Int,String) = readchomp(`date +%Y`),
    authors::String = Git.readchomp(`config --global --get user.name`),
    github::Bool = Git.success(`config --get github.user`),
    travis::Bool = Git.success(`config --get github.user`),
)
    isempty(license) && error("""
        You must choose a license, e.g.:

            julia> Pkg.scaffold("$pkg", license="MIT")

        Predefined licenses: $(join(sort!([keys(LICENSES)...], by=lowercase), ", ", " and ")).
        """)
    haskey(LICENSES,license) ||
        error("$license is not a known license choice.")
    d = if isempty(dir)
        avail = Dir.cd(Read.available)
        haskey(avail,pkg) &&
            error("$pkg is already a registered package name.")
        Dir.path(pkg)
    else
        joinpath(dir,pkg)
    end
    ispath(d) &&
        error("$d exists, refusing to overwrite.")
    mkpath(d)
    try cd(d) do
            info("Initializing $pkg repo: $d")
            Git.run(`init -q`)
            Git.run(`commit -q --allow-empty -m "initial empty commit"`)
            generate("README.md") do io
                if travis
                    user = Git.readchomp(`config --get github.user`)
                    url = "https://travis-ci.org/$user/$pkg.jl"
                    print(io, """
                        # $pkg

                        [![Build Status]($url.png)]($url)
                        """)
                end
            end
            generate("LICENSE.md") do io
                print(io, LICENSES[license](pkg, string(years), authors))
            end
            generate("src/$pkg.jl") do io
                print(io, """
                    module $pkg

                    # package code goes here

                    end # module
                    """)
            end
            travis && generate(".travis.yml") do io
                print(io, """
                    language: cpp
                    compiler:
                      - clang
                    notifications:
                      email: false
                    before_install:
                      - sudo add-apt-repository ppa:staticfloat/julia-deps -y
                      - sudo add-apt-repository ppa:staticfloat/julianightlies -y
                      - sudo apt-get update -qq -y
                      - sudo apt-get install libpcre3-dev julia -y
                    script:
                      - julia -e 'Pkg.init(); run(`ln -s \$(pwd()) \$(Pkg.dir())/$pkg`); Pkg.resolve()'
                      - julia -e 'using $pkg; @assert isdefined(:$pkg); @assert typeof($pkg) === Module'
                    """)
            end
            info("Committing scaffold")
            Git.run(`commit -q -m "$pkg.jl scaffold with $license license."`)
            if github
                user = Git.readchomp(`config --get github.user`)
                url = "git://github.com/$user/$pkg.jl.git"
                info("Setting URL: $url")
                Git.set_remote_url(url)
            end
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
