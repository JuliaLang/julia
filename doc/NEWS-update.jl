# Script to automatically insert Markdown footnotes for all [#xxxx] issue
# cross-references in the NEWS file.

NEWS = get(ARGS, 1, "NEWS.md")

s = readstring(NEWS)

s = s[1:match(r"\[#[0-9]+\]:", s).offset-1];

footnote(n) = "[#$n]: https://github.com/JuliaLang/julia/issues/$n"
N = map(m -> parse(Int,m.captures[1]), eachmatch(r"\[#([0-9]+)\]", s))
foots = join(map(footnote, sort!(unique(N))), "\n")

open(NEWS, "w") do f
    println(f, s, foots)
end
