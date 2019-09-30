# Script to automatically insert Markdown footnotes for all [#xxxx] issue
# cross-references in the NEWS file.

NEWS = get(ARGS, 1, "NEWS.md")

s = read(NEWS, String)

m = match(r"\[#[0-9]+\]:", s)
if m !== nothing
    s = s[1:m.offset-1]
end

footnote(n) = "[#$n]: https://github.com/JuliaLang/julia/issues/$n"
N = map(m -> parse(Int,m.captures[1]), eachmatch(r"\[#([0-9]+)\]", s))
foots = join(map(footnote, sort!(unique(N))), "\n")

open(NEWS, "w") do f
    println(f, s, foots)
end
