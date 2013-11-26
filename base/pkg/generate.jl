module Generate

import ..Git, ..Read

copyright_year() = readchomp(`date +%Y`)
copyright_name() = readchomp(`git config --global --get user.name`)
github_user() = readchomp(ignorestatus(`git config --global --get github.user`))

function package(
    pkg::String,
    license::String;
    force::Bool = false,
    authors::String = copyright_name(),
    years::Union(Int,String) = copyright_year(),
    username::String = github_user(),
)
    isnew = !ispath(pkg)
    if !isnew
        force || error("$pkg exists, refusing to overwrite.")
        Git.dirty(dir=pkg) && error("$pkg is dirty – commit or stash your changes")
    end
    url = isempty(username) ? "" : "git://github.com/$username/$pkg.jl.git"
    try
        Generate.init(pkg,url)
        Generate.license(pkg,license,years,authors)
        Generate.readme(pkg,username)
        Generate.entrypoint(pkg)
        Generate.travis(pkg)

        msg = """
        $pkg.jl $(isnew ? "generated" : "regenerated") files.

            license:  $license
            authors:  $authors
            years:    $years
            username: $username

        Julia Version $VERSION [$(Base.BUILD_INFO.commit[1:10])]
        """

        if isnew
            info("Committing $pkg generated files")
            Git.run(`commit -q -m $msg`, dir=pkg)
        elseif Git.staged(dir=pkg)
            Git.run(`reset -q --`, dir=pkg)
            info("Regenerated files left unstaged, use `git add -p` to select")
            open(io->print(io,msg), joinpath(Git.dir(pkg),"MERGE_MSG"), "w")
        else
            info("Regenerated files are unchanged")
        end
    catch
        isnew ? run(`rm -rf $pkg`) : Git.run(`checkout -q -f`, dir=pkg)
        rethrow()
    end
end

function init(pkg::String, url::String="")
    if !ispath(pkg)
        info("Initializing $pkg repo: $(abspath(pkg))")
        Git.run(`init -q $pkg`)
        Git.run(`commit -q --allow-empty -m "initial empty commit"`, dir=pkg)
    end
    isempty(url) && return
    info("Origin: $url")
    Git.set_remote_url(url,dir=pkg)
end

function license(pkg::String, license::String, years::Union(Int,String), authors::String)
    if !haskey(LICENSES,license)
        licenses = join(sort!([keys(LICENSES)...], by=lowercase), ", ")
        error("$license is not a known license choice, choose one of: $licenses.")
    end
    genfile(pkg,"LICENSE.md") do io
        print(io, LICENSES[license](pkg, string(years), authors))
    end
end

function readme(pkg::String, username::String="")
    genfile(pkg,"README.md") do io
        println(io, "# $pkg")
        isempty(username) && return
        url = "https://travis-ci.org/$username/$pkg.jl"
        println(io, "\n[![Build Status]($url.png)]($url)")
    end
end

function travis(pkg::String)
    genfile(pkg,".travis.yml") do io
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
          - julia -e 'Pkg.init(); run(`ln -s \$(pwd()) \$(Pkg.dir("$pkg"))`); Pkg.pin("$pkg"); Pkg.resolve()'
          - julia -e 'using $pkg; @assert isdefined(:$pkg); @assert typeof($pkg) === Module'
        """)
    end
end

function entrypoint(pkg::String)
    genfile(pkg,"src/$pkg.jl") do io
        print(io, """
        module $pkg

        # package code goes here

        end # module
        """)
    end
end

function genfile(f::Function, pkg::String, file::String)
    path = joinpath(pkg,file)
    info("Generating $file")
    mkpath(dirname(path))
    open(f, path, "w")
    Git.run(`add $file`, dir=pkg)
end

mit(pkg::String, years::String, authors::String) =
"""
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

bsd(pkg::String, years::String, authors::String) =
"""
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
