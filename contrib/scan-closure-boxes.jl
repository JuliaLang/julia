# This file is a part of Julia. License is MIT: https://julialang.org/license
# NOTE: This script is mostly AI-generated

function is_box_call(expr)
    if !(expr isa Expr)
        return false
    end
    if expr.head === :call
        callee = expr.args[1]
        return callee === Core.Box || (callee isa GlobalRef && callee.mod === Core && callee.name === :Box)
    elseif expr.head === :new
        callee = expr.args[1]
        return callee === Core.Box || (callee isa GlobalRef && callee.mod === Core && callee.name === :Box)
    end
    return false
end

function slot_name(ci, slot)
    if slot isa Core.SlotNumber
        idx = Int(slot.id)
        if 1 <= idx <= length(ci.slotnames)
            return string(ci.slotnames[idx])
        end
    end
    return string(slot)
end

function method_location(m::Method)
    file = m.file
    line = m.line
    file_str = file isa Symbol ? String(file) : string(file)
    if file_str == "none" || line == 0
        return ("", 0)
    end
    return (file_str, line)
end

function root_module(mod::Module)
    while true
        parent = parentmodule(mod)
        if parent === mod || parent === Main || parent === Core
            return mod
        end
        mod = parent
    end
end

function format_box_fields(var, m::Method)
    file, line = method_location(m)
    location = isempty(file) ? "" : string(file, ":", line)
    return (
        mod = string(root_module(m.module)),
        var = string(var),
        func = string(m.name),
        sig = string(m.sig),
        location = location,
    )
end

function escape_md(s)
    return replace(string(s), "|" => "\\|")
end

function md_code(s)
    return "`" * replace(string(s), "`" => "``") * "`"
end

function scan_method!(lines, m::Method, modules)
    root = string(root_module(m.module))
    if !isempty(modules) && !(root in modules)
        return
    end
    ci = try
        Base.uncompressed_ast(m)
    catch
        return
    end
    for stmt in ci.code
        if stmt isa Expr && stmt.head === :(=)
            lhs = stmt.args[1]
            rhs = stmt.args[2]
            if is_box_call(rhs)
                push!(lines, format_box_fields(slot_name(ci, lhs), m))
            end
        elseif is_box_call(stmt)
            push!(lines, format_box_fields("<unknown>", m))
        end
    end
end

function print_help()
    println("scan-closure-boxes.jl: scan loaded methods for Core.Box allocations")
    println()
    println("usage:")
    println("  scan-closure-boxes.jl [--module=Base,Core,...|stdlibs] [--format=plain|markdown]")
    println()
    println("options:")
    println("  --module=...   Comma-separated root modules to include; 'stdlibs' expands to all stdlibs.")
    println("  --format=...   Output format: plain or markdown (tables).")
end

function parse_args(args)
    modules = String[]
    format = "plain"
    for arg in args
        if arg == "--help" || arg == "-h"
            print_help()
            exit(0)
        elseif startswith(arg, "--module=")
            modlist = split(arg[(length("--module=") + 1):end], ',')
            for mod in modlist
                isempty(mod) || push!(modules, mod)
            end
        elseif startswith(arg, "--format=")
            format = arg[(length("--format=") + 1):end]
            if !(format in ("plain", "markdown", "markdown-table"))
                error("unknown format: " * format)
            end
        else
            error("unknown argument: " * arg)
        end
    end
    return Set(modules), format
end

function stdlib_modules()
    stdlib_dir = Sys.STDLIB
    mods = String[]
    for entry in readdir(stdlib_dir)
        path = joinpath(stdlib_dir, entry)
        if isdir(path) && isfile(joinpath(path, "src", entry * ".jl"))
            push!(mods, entry)
        end
    end
    return Set(mods)
end

function load_requested_modules(modules)
    for name in modules
        name == "Base" && continue
        name == "Core" && continue
        name == "Main" && continue
        try
            Base.require(Main, Symbol(name))
        catch err
            @warn "failed to load module" mod=name err=err
        end
    end
end

function scan_all_methods()
    modules, format = parse_args(ARGS)
    if "stdlibs" in modules
        delete!(modules, "stdlibs")
        union!(modules, stdlib_modules())
    end
    load_requested_modules(modules)
    lines = Vector{NamedTuple}()
    Base.visit(Core.methodtable) do m
        scan_method!(lines, m, modules)
    end
    sort!(lines, by = entry -> (entry.mod, entry.func, entry.var))
    if format == "plain"
        for entry in lines
            println("mod=", entry.mod,
                    "\tvar=", entry.var,
                    "\tfunc=", entry.func,
                    "\tsig=", entry.sig,
                    "\tlocation=", entry.location)
        end
    else
        # treat "markdown" and "markdown-table" as table output
        last_mod = ""
        for entry in lines
            if entry.mod != last_mod
                if !isempty(last_mod)
                    println()
                end
                println("## ", entry.mod)
                println("| var | func | sig | location |")
                println("| --- | --- | --- | --- |")
                last_mod = entry.mod
            end
            println("| ", md_code(escape_md(entry.var)),
                    " | ", md_code(escape_md(entry.func)),
                    " | ", md_code(escape_md(entry.sig)),
                    " | ", md_code(escape_md(entry.location)),
                    " |")
        end
    end
    return nothing
end

scan_all_methods()
