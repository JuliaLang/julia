module REPLMode

using Pkg3.Types

import Base: LineEdit, REPL, REPLCompletions
import Base.Random: UUID

const cmds = Dict(
    "help"      => :help,
    "?"         => :help,
    "status"    => :status,
    "search"    => :search,
    "find"      => :search,
    "info"      => :info,
    "add"       => :add,
    "install"   => :add,
    "rm"        => :rm,
    "remove"    => :rm,
    "uninstall" => :rm,
    "up"        => :up,
    "update"    => :up,
    "upgrade"   => :up,
    "test"      => :test,
    "gc"        => :gc,
    "fsck"      => :fsck,
)

function parse_option(word::AbstractString)
    m = match(r"^--(\w+)(?:\s*=\s*(\S*))?$", word)
    m == nothing && error("invalid option: ", repr(word))
    return m.captures[2] == nothing ?
        (:opt, Symbol(m.captures[1])) :
        (:opt, Symbol(m.captures[1]), String(m.captures[2]))
end

let uuid = raw"(?i)[0-9a-z]{8}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{12}(?-i)",
    name = raw"(\w+)(?:\.jl)?"
    global name_re = Regex("^$name\$")
    global uuid_re = Regex("^$uuid\$")
    global name_uuid_re = Regex("^$name\\s*=\\s*($uuid)\$")
end

const lex_re = r"[^@\s]+\s*=\s*[^@\s]+ | @\s*[^@\s]* | [^@\s]+"x

function tokenize(cmd::String)::Vector{Tuple{Symbol,Vararg{Any}}}
    tokens = Tuple{Symbol,Vararg{Any}}[]
    # TODO: handle string-quoted values, e.g. path names
    words = map(m->m.match, eachmatch(lex_re, cmd))
    help_mode = false
    while !isempty(words)
        word = shift!(words)
        if word[1] == '-'
            push!(tokens, parse_option(word))
        else
            word in keys(cmds) || error("invalid command: ", repr(word))
            push!(tokens, (:cmd, cmds[word]))
            help_mode || cmds[word] != :help && break
            help_mode = true
        end
    end
    while !isempty(words)
        word = shift!(words)
        if word[1] == '-'
            push!(tokens, parse_option(word))
        elseif word[1] == '@'
            push!(tokens, (:ver, VersionRange(strip(word[2:end]))))
        elseif ismatch(uuid_re, word)
            push!(tokens, (:pkg, UUID(word)))
        elseif ismatch(name_re, word)
            push!(tokens, (:pkg, String(match(name_re, word).captures[1])))
        elseif ismatch(name_uuid_re, word)
            m = match(name_uuid_re, word)
            push!(tokens, (:pkg, String(m.captures[1]), UUID(m.captures[2])))
        else
            error("invalid argument: ", repr(word))
        end
    end
    isempty(tokens) && push!(tokens, (:cmd, :help))
    return tokens
end

function do_cmd(repl::Base.REPL.AbstractREPL, input::String)
    disp = REPL.REPLDisplay(repl)
    try
        tokens = tokenize(input)
        top_opts = Dict{Symbol,Any}()
        local cmd::Symbol
        while !isempty(tokens)
            token = shift!(tokens)
            if token[1] == :cmd
                cmd = token[2]
                break
            elseif token[1] == :opt
                top_opts[token[2]] = length(token) == 2 ? true : token[3]
            else
                error("misplaced token: ", token)
            end
        end
        if cmd == :add
            isempty(tokens) &&
                error("`add` – list packages to add to the environment")
            tokens[1][1] == :ver &&
                error("package name/uuid must precede version spec `@$(tokens[1][2])`")
            pkgs = PackageVersion[]
            # tokens: package names and/or uuids, optionally followed by version specs
            while !isempty(tokens)
                token = shift!(tokens)
                if token[1] == :pkg
                    push!(pkgs, PackageVersion(Package(token[2:end]...)))
                elseif token[1] == :ver
                    pkgs[end].version = token[2]
                    isempty(tokens) || tokens[1][1] == :pkg ||
                        error("package name/uuid must precede version spec `@$(tokens[1][2])`")
                elseif token[1] == :opt
                    error("`add` doesn't take options: --$(join(token[2:end], '='))\ninvalid command: $input")
                end
            end
            # Pkg3.Operations.add(pkgs)
            display(disp, pkgs)
        elseif cmd == :rm
            isempty(tokens) &&
                error("`rm` – list packages to remove from the current environment")
            pkgs = Package[]
            # tokens: package names and/or uuids
            while !isempty(tokens)
                token = shift!(tokens)
                token[1] != :pkg &&
                    error("`rm` only accepts package names and/or UUIDs")
                push!(pkgs, Package(token[2:end]...))
            end
            # Pkg3.Operations.rm(pkgs)
            display(disp, pkgs)
        else
            error("`$cmd` command not yet implemented")
        end
    catch exc
        Base.display_error(repl.t.err_stream, exc, [])
    end
end

function create_mode(repl, main)
    pkg_mode = LineEdit.Prompt("pkg> ";
        prompt_prefix = Base.text_colors[:blue],
        prompt_suffix = "",
        sticky = true)

    hp = main.hist
    hp.mode_mapping[:pkg] = pkg_mode
    pkg_mode.hist = hp

    search_prompt, skeymap = LineEdit.setup_search_keymap(hp)
    prefix_prompt, prefix_keymap = LineEdit.setup_prefix_keymap(hp, pkg_mode)

    pkg_mode.on_done = (s, buf, ok) -> begin
        ok || return REPL.transition(s, :abort)
        input = String(take!(buf))
        REPL.reset(repl)
        do_cmd(repl, input)
        REPL.prepare_next(repl)
        REPL.reset_state(s)
        s.current_mode.sticky || REPL.transition(s, main)
    end

    mk = REPL.mode_keymap(main)
    # ^C should not exit prompt
    delete!(mk, "^C")

    b = Dict{Any,Any}[
        skeymap, mk, prefix_keymap, LineEdit.history_keymap,
        LineEdit.default_keymap, LineEdit.escape_defaults
    ]
    pkg_mode.keymap_dict = LineEdit.keymap(b)
    return pkg_mode
end

function repl_init(repl)
    main_mode = repl.interface.modes[1]
    pkg_mode = create_mode(repl, main_mode)
    push!(repl.interface.modes, pkg_mode)
    keymap = Dict{Any,Any}(
        ']' => function (s,args...)
            if isempty(s) || position(LineEdit.buffer(s)) == 0
                buf = copy(LineEdit.buffer(s))
                LineEdit.transition(s, pkg_mode) do
                    LineEdit.state(s, pkg_mode).input_buffer = buf
                end
            else
                LineEdit.edit_insert(s, ']')
            end
        end
    )
    main_mode.keymap_dict = LineEdit.keymap_merge(main_mode.keymap_dict, keymap)
    return
end

end
