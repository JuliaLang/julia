@__FILE__() != nothing && cd(dirname(@__FILE__))

include("../base/markdown/Markdown.jl")

import Markdown
using .Markdown
using Base.Markdown: MD

function mapfiles{S<:AbstractString}(f::Function, files::Vector{S}; clobber::Bool=false)
    for file in files
        open(file, "r") do r
            try
                tmp, w = mktemp()
                mv(tmp, f(file, r, w), remove_destination=clobber)
            catch
                rm(tmp)
                close(w)
                rethrow()
            end
        end
    end
end

using Glob

syntax_css = pwd() * "/css/syntax.css"
screen_css = pwd() * "/css/screen.css"
highlight_js = pwd() * "/js/highlight.pack.js"

mapfiles(glob("_build/markdown/**/*.md"), clobber=true) do name, r, w
    print(w, """
    <html><head>
        <link rel="stylesheet" href="$syntax_css" type="text/css" />
        <link rel="stylesheet" href="$screen_css" type="text/css" media="screen, projection" />
        <script src="$highlight_js"></script>
        <script>hljs.initHighlightingOnLoad();</script>
    </head><body>
    """)
    Markdown.html(w, Markdown.parse(readall(r)))
    print(w, "</body>")
    replace(name, r"\.md$", ".html")
end
