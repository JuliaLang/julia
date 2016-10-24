module GenStdLib

import Base.Docs: Binding, DocStr

# Constants.
const DOCSTRING_DIRECTIVE = r"^(.. (function|type|data):: ).*"

# Types.

typealias Signature Tuple{Binding, Type}

type State
    files      :: Dict{String, Vector{String}}
    validdocs  :: Dict{String, Tuple{Module, Signature, DocStr}}
    baddocs    :: Dict{Signature, Tuple{Module, DocStr}}
    documented :: Dict{Signature, Tuple{Module, DocStr, String}}
    errorlevel :: Int
    debug      :: Bool
    State() = new(Dict(), Dict(), Dict(), Dict(), 0, "JULIA_GENSTDLIB_DEBUG" in keys(ENV))
end

# Documentation Translator.

function translate(dirs::Vector; root = dirname(@__FILE__))
    cd(root) do
        state = State()
        println("\n# GENSTDLIB\n")
        # Find all available docstrings within `Base` and its submodules.
        info("loading docstrings from modules.")
        loaddocs!(state)
        # Read in rst pages and update all docstrings.
        info("parsing external documentation.")
        for dir in dirs, file in readdir(dir)
            translate(state, joinpath(dir, file))
        end
        # Write the newly updated rst docs back to their files -- replaces original content.
        if state.errorlevel < 2
            info("writing documentation back to file.")
            for (file, contents) in state.files
                open(file, "w") do io
                    for line in contents
                        println(io, line)
                    end
                end
            end
        else
            warn("errors found while generating documentation. Aborting file writing.")
        end
        # Report statistics about 'Base' docs.
        summarise(state)
    end
end
function translate(state::State, file::AbstractString)
    input, output = split(readstring(file), '\n')[1:end-1], []
    while !isempty(input)
        if ismatch(DOCSTRING_DIRECTIVE, first(input))
            append!(output, getdoc(state, file, input))
        else
            push!(output, shift!(input))
        end
    end
    state.files[file] = output
    return state
end

# Documentation Summary Report.

function summarise(state::State)
    println("\n# SUMMARY\n")
    println("   ", length(state.validdocs),  " docstrings found in modules.")
    println("   ", length(state.documented), " docstrings found in external docs.")

    # Of the missing docstrings, which are public and which are private?
    public, private = [], []
    for (sig, (mod, signature, docstr)) in state.validdocs
        # The docstring is valid, but not found in the external docs.
        if !haskey(state.documented, signature)
            binding, typesig = signature
            exported = binding.var in names(binding.mod)
            push!(exported ? public : private, (binding, typesig))
        end
    end
    println("   ", length(public), " public docstrings missing from external docs.")
    state.debug && (println(); foreach(printmissing, public); println())
    println("   ", length(private), " private docstrings missing from external docs.")
    state.debug && (println(); foreach(printmissing, private); println())

    # Expected format is: first element in docstring should be a code block.
    println("   ", length(state.baddocs),    " docstrings not matching expected format. Skipped.")
    println("   ", length(state.files),      " files parsed.\n")
    state.debug || println("Set 'JULIA_GENSTDLIB_DEBUG' ENV for additional debug info.\n")
end
printmissing(x) = println("     X ", x[1], " :: ", x[2])

# Documentation Loader.

function loaddocs!(state::State)
    for mod in Base.Docs.modules
        for (binding, multidoc) in Base.Docs.meta(mod)
            for (typesig, docstr) in multidoc.docs
                loaddocs!(state, mod, binding, typesig, docstr)
            end
        end
    end
    for (keyword, docstr) in Base.Docs.keywords
        loaddocs!(state, Main, keyword, Union{}, docstr)
    end
    return state
end
function loaddocs!(state::State, mod, binding::Binding, typesig, docstr)
    markdown = Base.Docs.parsedoc(docstr)
    if validdocstr(markdown)
        code = rstrip(markdown.content[1].code)
        if haskey(state.validdocs, code) && state.validdocs[code][3] !== docstr
            code = indent(code)
            warn("duplicate signature found for '$binding' in module '$mod':\n\n$code\n")
            state.errorlevel = 2
        else
            state.validdocs[code] = (mod, (binding, typesig), docstr)
        end
    else
        if haskey(state.baddocs, (binding, typesig))
            warn("duplicate binding '$binding :: $typesig' found in module '$mod'.")
            state.errorlevel = 2
        else
            state.baddocs[(binding, typesig)] = (mod, docstr)
        end
    end
    return state
end
function loaddocs!(state::State, mod, keyword::Symbol, typesig, docstr)
    binding = Base.Docs.Binding(mod, keyword)
    loaddocs!(state, mod, binding, typesig, docstr)
end

# Retrieve and format docstrings.

function getdoc(state::State, file::AbstractString, input::Vector)
    # Capture the lines containing the docstring signature.
    output = []
    while !isempty(input)
        line = rstrip(shift!(input))
        push!(output, line)
        ismatch(r"^$", line) && break
    end
    # Recover the unindented version of the signature.
    n = length(match(DOCSTRING_DIRECTIVE, first(output))[1])
    b = IOBuffer()
    for line in output[1:end-1]
        println(b, line[(n + 1):end])
    end
    # The signature may contain `\` characaters, which must be unescaped.
    signature = unescape_string(rstrip(String(take!(b))))
    # Splice the correct docstring into the output after the signature.
    if haskey(state.validdocs, signature)
        # Push the rst text for the docstring into the output.
        mod, (binding, typesig), docstr = state.validdocs[signature]
        md  = Markdown.MD(Base.Docs.parsedoc(docstr).content[2:end])
        rst = Base.Markdown.rst(dropheaders(md))
        push!(output, "   .. Docstring generated from Julia source", "")
        for line in split(rst, '\n')
            line = isempty(line) ? "" : string(" "^3, line)
            push!(output, rstrip(line))
        end
        # Consume all indented lines from the current docstring.
        while !isempty(input)
            line = first(input)
            ismatch(r"^[^\s]", line) ? break : shift!(input)
        end
        # Track which docstrings have been found in the external docs.
        state.documented[(binding, typesig)] = (mod, docstr, signature)
    else
        signature = indent(signature)
        warn("missing docs for signature:\n\n$signature\n")
        state.errorlevel = 1
    end
    return output
end

# Replace headers in docs with bold since Sphinx does not allow headers inside docstrings.
dropheaders(md) = Markdown.MD(map(bold, md.content))
bold(x::Markdown.Header) = Markdown.Paragraph(Markdown.Bold(x.text))
bold(other) = other

# Utilities.

function indent(str::AbstractString, indent = 4)
    buf = IOBuffer()
    for line in split(str, '\n')
        println(buf, " "^indent, line)
    end
    String(take!(buf))
end

function validdocstr(markdown::Base.Markdown.MD)
    content = markdown.content
    !isempty(content) && isa(first(content), Base.Markdown.Code)
end
validdocstr(other) = false

end

# The docstring for `build_sysimg` is defined is this file and included within the
# `devdocs/sysimg.rst` file, so we include it here to make it visible to the docsystem.
include(joinpath("..", "contrib", "build_sysimg.jl"))

GenStdLib.translate(["manual", "stdlib", "devdocs"])
