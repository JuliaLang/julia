module Generate

import ..Git, ..Read

copyright_year() = readchomp(`date +%Y`)
copyright_name() = readchomp(`git config --global --get user.name`)
github_user() = readchomp(ignorestatus(`git config --global --get github.user`))

function git_contributors(dir::String, n::Int=typemax(Int))
    contrib = Dict()
    tty = @windows? "CON:" : "/dev/tty"
    for line in eachline(tty |> Git.cmd(`shortlog -nes`, dir=dir))
        m = match(r"\s*(\d+)\s+(.+?)\s+\<(.+?)\>\s*$", line)
        m == nothing && continue
        commits, name, email = m.captures
        if haskey(contrib,email)
            contrib[email][1] += int(commits)
        else
            contrib[email] = [int(commits),name]
        end
    end
    names = Dict()
    for (commits,name) in values(contrib)
        names[name] = get(names,name,0) + commits
    end
    names = sort!(collect(keys(names)),by=name->names[name],rev=true)
    length(names) <= n ? names : [names[1:n], "et al."]
end

function package(
    pkg::String,
    license::String;
    force::Bool = false,
    authors::String = "",
    years::Union(Int,String) = copyright_year(),
    user::String = github_user(),
)
    isnew = !ispath(pkg)
    try
        if isnew
            url = isempty(user) ? "" : "git://github.com/$user/$pkg.jl.git"
            Generate.init(pkg,url)
        else
            Git.dirty(dir=pkg) && error("$pkg is dirty – commit or stash your changes")
        end
        Git.transact(dir=pkg) do
            if isempty(authors)
                authors = isnew ? copyright_name() : git_contributors(pkg,5)
            end
            Generate.license(pkg,license,years,authors,force=force)
            Generate.readme(pkg,user,force=force)
            Generate.entrypoint(pkg,force=force)
            Generate.tests(pkg,force=force)
            Generate.travis(pkg,force=force)

            msg = """
            $pkg.jl $(isnew ? "generated" : "regenerated") files.

                license:  $license
                authors:  $(join([authors],", "))
                years:    $years
                user:     $user

            Julia Version $VERSION [$(Base.GIT_VERSION_INFO.commit_short)]
            """

            if isnew
                info("Committing $pkg generated files")
                Git.run(`commit -q -m $msg`, dir=pkg)
            elseif Git.dirty(dir=pkg)
                Git.run(`reset -q --`, dir=pkg)
                info("Regenerated files left unstaged, use `git add -p` to select")
                open(io->print(io,msg), joinpath(Git.dir(pkg),"MERGE_MSG"), "w")
            else
                info("Regenerated files are unchanged")
            end
        end
    catch
        isnew && run(`rm -rf $pkg`)
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

function license(pkg::String, license::String,
                 years::Union(Int,String),
                 authors::Union(String,Array);
                 force::Bool=false)
    genfile(pkg,"LICENSE.md",force) do io
        if !haskey(LICENSES,license)
            licenses = join(sort!([keys(LICENSES)...], by=lowercase), ", ")
            error("$license is not a known license choice, choose one of: $licenses.")
        end
        print(io, LICENSES[license](pkg, string(years), authors))
    end || info("License file exists, leaving unmodified; use `force=true` to overwrite")
end

function readme(pkg::String, user::String=""; force::Bool=false)
    genfile(pkg,"README.md",force) do io
        println(io, "# $pkg")
        isempty(user) && return
        url = "https://travis-ci.org/$user/$pkg.jl"
        println(io, "\n[![Build Status]($url.png)]($url)")
    end
end

function tests(pkg::String; force::Bool=false)
    genfile(pkg,"test/runtests.jl",force) do io
        print(io, """
        using $pkg
        using Base.Test  

        # write your own tests here
        @test 1 == 1
        """)
    end
end

function travis(pkg::String; force::Bool=false)
    genfile(pkg,".travis.yml",force) do io
        print(io, """
        language: cpp
        compiler:
          - clang
        notifications:
          email: false
        env:
          matrix: 
            - JULIAVERSION="juliareleases" 
            - JULIAVERSION="julianightlies" 
        before_install:
          - sudo add-apt-repository ppa:staticfloat/julia-deps -y
          - sudo add-apt-repository ppa:staticfloat/\${JULIAVERSION} -y
          - sudo apt-get update -qq -y
          - sudo apt-get install libpcre3-dev julia -y
        script:
          - julia -e 'Pkg.init(); Pkg.clone(pwd()); Pkg.test("$pkg")'
        """)
    end
end

function entrypoint(pkg::String; force::Bool=false)
    genfile(pkg,"src/$pkg.jl",force) do io
        print(io, """
        module $pkg

        # package code goes here

        end # module
        """)
    end
end

function genfile(f::Function, pkg::String, file::String, force::Bool=false)
    path = joinpath(pkg,file)
    if force || !ispath(path)
        info("Generating $file")
        mkpath(dirname(path))
        open(f, path, "w")
        Git.run(`add $file`, dir=pkg)
        return true
    end
    return false
end

copyright(years::String, authors::String) = "> Copyright (c) $years: $authors."

function copyright(years::String, authors::Array)
    text = "> Copyright (c) $years:"
    for author in authors
        text *= "\n>  * $author"
    end
    return text
end

mit(pkg::String, years::String, authors::Union(String,Array)) =
"""
The $pkg.jl package is licensed under the MIT "Expat" License:

$(copyright(years,authors))
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

bsd(pkg::String, years::String, authors::Union(String,Array)) =
"""
The $pkg.jl package is licensed under the Simplified "2-clause" BSD License:

$(copyright(years,authors))
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
