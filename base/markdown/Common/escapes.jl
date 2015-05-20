# This file is a part of Julia. License is MIT: http://julialang.org/license

const escape_chars = "\\`*_#+-.!{[(\$"

@trigger '\\' ->
function escapes(stream::IO, md::MD)
    withstream(stream) do
        if startswith(stream, "\\") && !eof(stream) && (c = read(stream, Char)) in escape_chars
            return string(c)
        end
    end
end
