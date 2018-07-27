Base.__init__()

# Prevent this from being put into the Main namespace
let
M = Module()
@eval M begin
julia_cmd() = (julia = joinpath(Sys.BINDIR, Base.julia_exename()); `$julia`)

function generate_precompilable_package(path, pkgname)
    mkpath(joinpath(path, pkgname, "src"))
    write(joinpath(path, pkgname, "src", "$pkgname.jl"),
          """
          __precompile__()
          module $pkgname
          end
          """)
end

function generate_precompile_statements()
    t = time()
    println("Generating precompile statements...")
    println("──────────────────────────────────────")

	# Reset code loading vars
	Base.ACTIVE_PROJECT[] = nothing
	Base.HOME_PROJECT[] = nothing
	empty!(LOAD_PATH)
	empty!(DEPOT_PATH)
	push!(LOAD_PATH, "@stdlib")

    tmpd = mktempdir()
    push!(DEPOT_PATH, tmpd)
    push!(LOAD_PATH, tmpd)
    pkgname = "__PackagePrecompilationStatementModule"
    generate_precompilable_package(tmpd, pkgname)
    @eval using __PackagePrecompilationStatementModule
    pop!(LOAD_PATH)
    pop!(DEPOT_PATH)
    rm(tmpd; recursive=true)

    if isempty(ARGS)
        sysimg = joinpath(dirname(Sys.BINDIR), "lib", "julia", "sys.ji")
    else
        sysimg = ARGS[1]
    end

    output = Pipe()
    task = @async read(output, String)
    have_repl =  haskey(Base.loaded_modules,
                        Base.PkgId(Base.UUID("3fa0cd96-eef1-5676-8a61-b3b8758bbffb"), "REPL"))
    if have_repl
        # Have a REPL, run the repl replayer and an interactive session that we immidiately kill
        setup = """
        include($(repr(joinpath(@__DIR__, "precompile_replay.jl"))))
        @async while true
            sleep(0.01)
            if isdefined(Base, :active_repl)
                exit(0)
            end
        end
        """
        # Do not redirect stdin unless it is to a tty, because that changes code paths
        ok = try
            run(pipeline(`$(julia_cmd()) --sysimage $sysimg --trace-compile=yes -O0
                     --startup-file=no --q -e $setup -i`; stderr=output))
            true
        catch
            false
        end
    else
        # No REPL, just record the startup
        ok = try
            run(pipeline(`$(julia_cmd()) --sysimage $sysimg --trace-compile=yes -O0
                     --startup-file=no --q -e0`; stderr=output))
            true
        catch
            false
        end
    end

    # Replace the FakeTerminal with a TTYTerminal and filter out everything we compiled in Main
    close(output)
    s = fetch(task)
    if !ok
        error("precompilation process failed, stderr is:\n$s")
    end
    new_precompiles = Set{String}()
    for statement in split(s, '\n')
        startswith(statement, "precompile(Tuple{") || continue
        statement = replace(statement, "FakeTerminals.FakeTerminal" => "REPL.Terminals.TTYTerminal")
        (occursin(r"Main.", statement) || occursin(r"FakeTerminals.", statement)) && continue
        # AppVeyor CI emits a single faulty precompile statement:
        # precompile(Tuple{getfield(precompile(Tuple{typeof(Base.uvfinalize), Base.PipeEndpoint})
        # which lacks the correct closing brackets.
        # Filter out such lines here.
        for (l, r) in ('(' => ')', '{' => '}')
            if count(isequal(l), statement) != count(isequal(r), statement)
                continue
            end
            push!(new_precompiles, statement)
        end
    end

    tmp = tempname()
    write(tmp, join(sort(collect(new_precompiles)), '\n'))
    # Load the precompile statements
    PrecompileStagingArea = Module()
    for (_pkgid, _mod) in Base.loaded_modules
        if !(_pkgid.name in ("Main", "Core", "Base"))
            eval(PrecompileStagingArea, :($(Symbol(_mod)) = $_mod))
        end
    end
    Base.include(PrecompileStagingArea, tmp)
    rm(tmp)

    # Add a few manual things, run `julia` with `--trace-compile` to find these.
    if have_repl
        @eval PrecompileStagingArea begin
            # Could startup with REPL banner instead but it is a bit visually noisy,
            # so just precompile it manualally here.
            precompile(Tuple{typeof(Base.banner), REPL.Terminals.TTYTerminal})

            # This is probablably important for precompilation (0.2s precompile time)
            # but doesn't seem to get caught in the script above
            precompile(Tuple{typeof(Base.create_expr_cache), String, String, Array{Base.Pair{Base.PkgId, UInt64}, 1}, Base.UUID})
        end
    end

    println("──────────────────────────────────────")
    print("$(length(new_precompiles)) precompile statements generated in "), Base.time_print((time() - t) * 10^9)
    println()
    return
end

generate_precompile_statements()

end # @eval
end # let
