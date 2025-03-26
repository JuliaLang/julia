# Install dependencies needed to build the documentation.
Base.ACTIVE_PROJECT[] = nothing
empty!(LOAD_PATH)
push!(LOAD_PATH, @__DIR__, "@stdlib")
empty!(DEPOT_PATH)
push!(DEPOT_PATH, joinpath(@__DIR__, "deps"))
push!(DEPOT_PATH, abspath(Sys.BINDIR, "..", "share", "julia"))
using Pkg
Pkg.instantiate()

using Documenter
import LibGit2

baremodule GenStdLib end

# Documenter Setup.

symlink_q(tgt, link) = isfile(link) || symlink(tgt, link)
cp_q(src, dest) = isfile(dest) || cp(src, dest)

# make links for stdlib package docs, this is needed until #552 in Documenter.jl is finished
const STDLIB_DOCS = []
const STDLIB_DIR = Sys.STDLIB
const EXT_STDLIB_DOCS = ["Pkg"]
cd(joinpath(@__DIR__, "src")) do
    Base.rm("stdlib"; recursive=true, force=true)
    mkdir("stdlib")
    for dir in readdir(STDLIB_DIR)
        sourcefile = joinpath(STDLIB_DIR, dir, "docs", "src")
        if dir in EXT_STDLIB_DOCS
            sourcefile = joinpath(sourcefile, "basedocs.md")
        else
            sourcefile = joinpath(sourcefile, "index.md")
        end
        if isfile(sourcefile)
            targetfile = joinpath("stdlib", dir * ".md")
            push!(STDLIB_DOCS, (stdlib = Symbol(dir), targetfile = targetfile))
            if Sys.iswindows()
                cp_q(sourcefile, targetfile)
            else
                symlink_q(sourcefile, targetfile)
            end
        end
    end
end

# Because we have standard libraries that are hosted outside of the julia repo,
# but their docs are included in the manual, we need to populate the remotes argument
# of makedocs(), to make sure that Documenter knows how to resolve the directories
# in stdlib/ to the correct remote Git repositories (for source and edit links).
#
# This function parses the *.version files in stdlib/, returning a dictionary with
# all the key-value pairs from those files. *_GIT_URL and *_SHA1 fields are the ones
# we will actually be interested in.
function parse_stdlib_version_file(path)
    values = Dict{String,String}()
    for line in readlines(path)
        m = match(r"^([A-Z0-9_]+)\s+:?=\s+(\S+)$", line)
        if isnothing(m)
            @warn "Unable to parse line in $(path)" line
        else
            values[m[1]] = m[2]
        end
    end
    return values
end
# This generates the value that will be passed to the `remotes` argument of makedocs(),
# by looking through all *.version files in stdlib/.
documenter_stdlib_remotes = let stdlib_dir = realpath(joinpath(@__DIR__, "..", "stdlib"))
    # Get a list of all *.version files in stdlib/..
    version_files = filter(readdir(stdlib_dir)) do fname
        isfile(joinpath(stdlib_dir, fname)) && endswith(fname, ".version")
    end
    # .. and then parse them, each becoming an entry for makedocs's remotes.
    # The values for each are of the form path => (remote, sha1), where
    #  - path: the path to the stdlib package's root directory, i.e. "stdlib/$PACKAGE"
    #  - remote: a Documenter.Remote object, pointing to the Git repository where package is hosted
    #  - sha1: the SHA1 of the commit that is included with the current Julia version
    remotes_list = map(version_files) do version_fname
        package = match(r"(.+)\.version", version_fname)[1]
        versionfile = parse_stdlib_version_file(joinpath(stdlib_dir, version_fname))
        # From the (all uppercase) $(package)_GIT_URL and $(package)_SHA1 fields, we'll determine
        # the necessary information. If this logic happens to fail for some reason for any of the
        # standard libraries, we'll crash the documentation build, so that it could be fixed.
        remote = let git_url_key = "$(uppercase(package))_GIT_URL"
            haskey(versionfile, git_url_key) || error("Missing $(git_url_key) in $version_fname")
            m = match(LibGit2.GITHUB_REGEX, versionfile[git_url_key])
            isnothing(m) && error("Unable to parse $(git_url_key)='$(versionfile[git_url_key])' in $version_fname")
            Documenter.Remotes.GitHub(m[2], m[3])
        end
        package_sha = let sha_key = "$(uppercase(package))_SHA1"
            haskey(versionfile, sha_key) || error("Missing $(sha_key) in $version_fname")
            versionfile[sha_key]
        end
        # Construct the absolute (local) path to the stdlib package's root directory
        package_root_dir = joinpath(stdlib_dir, "$(package)-$(package_sha)")
        # Documenter needs package_root_dir to exist --- it's just a sanity check it does on the remotes= keyword.
        # In normal (local) builds, this will be the case, since the Makefiles will have unpacked the standard
        # libraries. However, on CI we do this thing where we actually build docs in a clean worktree, just
        # unpacking the `usr/` directory from the main build, and the unpacked stdlibs will be missing, and this
        # will cause Documenter to throw an error. However, we don't _actually_ need the source files of the standard
        # libraries to be present, so we just generate empty root directories to satisfy the check in Documenter.
        isdir(package_root_dir) || mkpath(package_root_dir)
        package_root_dir => (remote, package_sha)
    end
    Dict(
        # We also add the root of the repository to `remotes`, because we do not always build the docs in a
        # checked out JuliaLang/julia repository. In particular, when building Julia from tarballs, there is no
        # Git information available. And also the way the BuildKite CI is configured to check out the code means
        # that in some circumstances the Git repository information is incorrect / no available via Git.
        dirname(@__DIR__) => (Documenter.Remotes.GitHub("JuliaLang", "julia"), Base.GIT_VERSION_INFO.commit),
        remotes_list...
    )
end

# Check if we are building a PDF
const render_pdf = "pdf" in ARGS

# Generate a suitable markdown file from NEWS.md and put it in src
function generate_markdown(basename)
    str = read(joinpath(@__DIR__, "..", "$basename.md"), String)
    splitted = split(str, "<!--- generated by $basename-update.jl: -->")
    @assert length(splitted) == 2
    replaced_links = replace(splitted[1], r"\[\#([0-9]*?)\]" => s"[#\g<1>](https://github.com/JuliaLang/julia/issues/\g<1>)")
    write(
        joinpath(@__DIR__, "src", "$basename.md"),
        """
        ```@meta
        EditURL = "https://github.com/JuliaLang/julia/blob/master/$basename.md"
        ```
        """ * replaced_links)
end
generate_markdown("NEWS")

Manual = [
    "manual/getting-started.md",
    "manual/installation.md",
    "manual/variables.md",
    "manual/integers-and-floating-point-numbers.md",
    "manual/mathematical-operations.md",
    "manual/complex-and-rational-numbers.md",
    "manual/strings.md",
    "manual/functions.md",
    "manual/control-flow.md",
    "manual/variables-and-scoping.md",
    "manual/types.md",
    "manual/methods.md",
    "manual/constructors.md",
    "manual/conversion-and-promotion.md",
    "manual/interfaces.md",
    "manual/modules.md",
    "manual/documentation.md",
    "manual/metaprogramming.md",
    "manual/arrays.md",
    "manual/missing.md",
    "manual/networking-and-streams.md",
    "manual/parallel-computing.md",
    "manual/asynchronous-programming.md",
    "manual/multi-threading.md",
    "manual/distributed-computing.md",
    "manual/running-external-programs.md",
    "manual/calling-c-and-fortran-code.md",
    "manual/handling-operating-system-variation.md",
    "manual/environment-variables.md",
    "manual/embedding.md",
    "manual/code-loading.md",
    "manual/profile.md",
    "manual/stacktraces.md",
    "manual/performance-tips.md",
    "manual/workflow-tips.md",
    "manual/style-guide.md",
    "manual/faq.md",
    "manual/noteworthy-differences.md",
    "manual/unicode-input.md",
    "manual/command-line-interface.md",
]

BaseDocs = [
    "base/base.md",
    "base/collections.md",
    "base/math.md",
    "base/numbers.md",
    "base/strings.md",
    "base/arrays.md",
    "base/parallel.md",
    "base/multi-threading.md",
    "base/scopedvalues.md",
    "base/constants.md",
    "base/file.md",
    "base/io-network.md",
    "base/punctuation.md",
    "base/sort.md",
    "base/iterators.md",
    "base/reflection.md",
    "base/c.md",
    "base/libc.md",
    "base/stacktraces.md",
    "base/simd-types.md",
]

StdlibDocs = [stdlib.targetfile for stdlib in STDLIB_DOCS]

DevDocs = [
    "Documentation of Julia's Internals" => [
        "devdocs/init.md",
        "devdocs/ast.md",
        "devdocs/types.md",
        "devdocs/object.md",
        "devdocs/eval.md",
        "devdocs/callconv.md",
        "devdocs/compiler.md",
        "devdocs/functions.md",
        "devdocs/cartesian.md",
        "devdocs/meta.md",
        "devdocs/subarrays.md",
        "devdocs/isbitsunionarrays.md",
        "devdocs/sysimg.md",
        "devdocs/pkgimg.md",
        "devdocs/llvm-passes.md",
        "devdocs/llvm.md",
        "devdocs/stdio.md",
        "devdocs/boundscheck.md",
        "devdocs/locks.md",
        "devdocs/offset-arrays.md",
        "devdocs/require.md",
        "devdocs/inference.md",
        "devdocs/ssair.md",
        "devdocs/EscapeAnalysis.md",
        "devdocs/aot.md",
        "devdocs/gc-sa.md",
        "devdocs/gc.md",
        "devdocs/gc-mmtk.md",
        "devdocs/jit.md",
        "devdocs/builtins.md",
        "devdocs/precompile_hang.md",
    ],
    "Developing/debugging Julia's C code" => [
        "devdocs/backtraces.md",
        "devdocs/debuggingtips.md",
        "devdocs/valgrind.md",
        "devdocs/external_profilers.md",
        "devdocs/sanitizers.md",
        "devdocs/probes.md",
    ],
    "Building Julia" => [
        "devdocs/build/build.md",
        "devdocs/build/linux.md",
        "devdocs/build/macos.md",
        "devdocs/build/windows.md",
        "devdocs/build/freebsd.md",
        "devdocs/build/arm.md",
        "devdocs/build/riscv.md",
        "devdocs/build/distributing.md",
    ]
]


if render_pdf
const PAGES = [
    "Manual" => ["index.md", Manual...],
    "Base" => BaseDocs,
    "Standard Library" => StdlibDocs,
    # Add "Release Notes" to devdocs
    "Developer Documentation" => [DevDocs..., hide("NEWS.md")],
]
else
const PAGES = [
    "Julia Documentation" => "index.md",
    hide("NEWS.md"),
    "Manual" => Manual,
    "Base" => BaseDocs,
    "Standard Library" => StdlibDocs,
    "Developer Documentation" => DevDocs,
]
end

const use_revise = "revise=true" in ARGS
if use_revise
    let revise_env = joinpath(@__DIR__, "deps", "revise")
        Pkg.activate(revise_env)
        Pkg.add("Revise"; preserve=Pkg.PRESERVE_NONE)
        Base.ACTIVE_PROJECT[] = nothing
        pushfirst!(LOAD_PATH, revise_env)
    end
end
function maybe_revise(ex)
    use_revise || return ex
    STDLIB_DIR = Sys.STDLIB
    STDLIBS = filter!(x -> isfile(joinpath(STDLIB_DIR, x, "src", "$(x).jl")), readdir(STDLIB_DIR))
    return quote
        $ex
        using Revise
        const STDLIBS = $STDLIBS
        union!(Revise.stdlib_names, Symbol.(STDLIBS))
        Revise.track(Core.Compiler)
        Revise.track(Base)
        for (id, mod) in Base.loaded_modules
            if id.name in STDLIBS
                Revise.track(mod)
            end
        end
        Revise.revise()
    end
end

for stdlib in STDLIB_DOCS
    @eval using $(stdlib.stdlib)
    # All standard library modules get `using $STDLIB` as their global
    DocMeta.setdocmeta!(
        Base.root_module(Base, stdlib.stdlib),
        :DocTestSetup,
        maybe_revise(:(using $(stdlib.stdlib)));
        recursive=true,
    )
end
# A few standard libraries need more than just the module itself in the DocTestSetup.
# This overwrites the existing ones from above though, hence the warn=false.
DocMeta.setdocmeta!(
    SparseArrays,
    :DocTestSetup,
    maybe_revise(:(using SparseArrays, LinearAlgebra));
    recursive=true, warn=false,
)
DocMeta.setdocmeta!(
    UUIDs,
    :DocTestSetup,
    maybe_revise(:(using UUIDs, Random));
    recursive=true, warn=false,
)
DocMeta.setdocmeta!(
    Pkg,
    :DocTestSetup,
    maybe_revise(:(using Pkg, Pkg.Artifacts));
    recursive=true, warn=false,
)
DocMeta.setdocmeta!(
    Base,
    :DocTestSetup,
    maybe_revise(:(;;));
    recursive=true,
)
DocMeta.setdocmeta!(
    Base.BinaryPlatforms,
    :DocTestSetup,
    maybe_revise(:(using Base.BinaryPlatforms));
    recursive=true, warn=false,
)

let r = r"buildroot=(.+)", i = findfirst(x -> occursin(r, x), ARGS)
    global const buildroot = i === nothing ? (@__DIR__) : first(match(r, ARGS[i]).captures)
end

const format = if render_pdf
    Documenter.LaTeX(
        platform = "texplatform=docker" in ARGS ? "docker" : "native"
    )
else
    Documenter.HTML(
        prettyurls = ("deploy" in ARGS),
        canonical = ("deploy" in ARGS) ? "https://docs.julialang.org/en/v1/" : nothing,
        assets = [
            "assets/julia-manual.css",
            "assets/julia.ico",
        ],
        analytics = "UA-28835595-6",
        collapselevel = 1,
        sidebar_sitename = false,
        ansicolor = true,
        size_threshold = 800 * 2^10, # 800 KiB
        size_threshold_warn = 200 * 2^10, # the manual has quite a few large pages, so we warn at 200+ KiB only
        inventory_version = VERSION,
    )
end

const output_path = joinpath(buildroot, "doc", "_build", (render_pdf ? "pdf" : "html"), "en")
makedocs(
    build     = output_path,
    modules   = [Main, Base, Core, [Base.root_module(Base, stdlib.stdlib) for stdlib in STDLIB_DOCS]...],
    clean     = true,
    doctest   = ("doctest=fix" in ARGS) ? (:fix) : ("doctest=only" in ARGS) ? (:only) : ("doctest=true" in ARGS) ? true : false,
    linkcheck = "linkcheck=true" in ARGS,
    linkcheck_ignore = ["https://bugs.kde.org/show_bug.cgi?id=136779"], # fails to load from nanosoldier?
    checkdocs = :none,
    format    = format,
    sitename  = "The Julia Language",
    authors   = "The Julia Project",
    pages     = PAGES,
    remotes   = documenter_stdlib_remotes,
)

# Update URLs to external stdlibs (JuliaLang/julia#43199)
for (root, _, files) in walkdir(output_path), file in joinpath.(root, files)
    endswith(file, ".html") || continue
    local str
    str = read(file, String)
    # Index page links, update
    #   https://github.com/JuliaLang/julia/blob/master/stdlib/${STDLIB_NAME}-${STDLIB_COMMIT}/path/to.md
    # to
    #   https://github.com/JuliaLang/${STDLIB_NAME}.jl/blob/master/docs/src/index.md
    str = replace(str, r"https://github.com/JuliaLang/julia/blob/master/stdlib/(.*)-\w{40}/(.*\.md)" =>
                       s"https://github.com/JuliaLang/\1.jl/blob/master/\2")
    # Link to source links, update
    #   https://github.com/JuliaLang/julia/blob/${JULIA_COMMIT}/stdlib/${STDLIB_NAME}-${STDLIB_COMMIT}/path/to.jl#${LINES}
    # to
    #   https://github.com/JuliaLang/${STDLIB_NAME}.jl/blob/${STDLIB_COMMIT}/path/to.jl#${LINES}
    str = replace(str, r"https://github\.com/JuliaLang/julia/blob/\w{40}/stdlib/(.*)-(\w{40})/(.*\.jl#L\d+(?:-L\d+)?)" =>
                       s"https://github.com/JuliaLang/\1.jl/blob/\2/\3")
    # Some stdlibs are not hosted by JuliaLang
    str = replace(str, r"(https://github\.com)/JuliaLang/(ArgTools\.jl/blob)" => s"\1/JuliaIO/\2")
    str = replace(str, r"(https://github\.com)/JuliaLang/(LibCURL\.jl/blob)" => s"\1/JuliaWeb/\2")
    str = replace(str, r"(https://github\.com)/JuliaLang/(SHA\.jl/blob)" => s"\1/JuliaCrypto/\2")
    str = replace(str, r"(https://github\.com)/JuliaLang/(Tar\.jl/blob)" => s"\1/JuliaIO/\2")
    # Write back to the file
    write(file, str)
end

# Define our own DeployConfig
struct BuildBotConfig <: Documenter.DeployConfig end
Documenter.authentication_method(::BuildBotConfig) = Documenter.HTTPS
Documenter.authenticated_repo_url(::BuildBotConfig) = "https://github.com/JuliaLang/docs.julialang.org.git"
function Documenter.deploy_folder(::BuildBotConfig; devurl, repo, branch, kwargs...)
    if !haskey(ENV, "DOCUMENTER_KEY")
        @info "Unable to deploy the documentation: DOCUMENTER_KEY missing"
        return Documenter.DeployDecision(; all_ok=false)
    end
    release = match(r"^release-([0-9]+\.[0-9]+)$", Base.GIT_VERSION_INFO.branch)
    if Base.GIT_VERSION_INFO.tagged_commit
        # Strip extra pre-release info (1.5.0-rc2.0 -> 1.5.0-rc2)
        ver = VersionNumber(VERSION.major, VERSION.minor, VERSION.patch,
            isempty(VERSION.prerelease) ? () : (VERSION.prerelease[1],))
        subfolder = "v$(ver)"
        return Documenter.DeployDecision(; all_ok=true, repo, branch, subfolder)
    elseif Base.GIT_VERSION_INFO.branch == "master"
        return Documenter.DeployDecision(; all_ok=true, repo, branch, subfolder=devurl)
    elseif !isnothing(release)
        # If this is a non-tag build from a release-* branch, we deploy them as dev docs into the
        # appropriate vX.Y-dev subdirectory.
        return Documenter.DeployDecision(; all_ok=true, repo, branch, subfolder="v$(release[1])-dev")
    end
    @info """
    Unable to deploy the documentation: invalid GIT_VERSION_INFO
    GIT_VERSION_INFO.tagged_commit: $(Base.GIT_VERSION_INFO.tagged_commit)
    GIT_VERSION_INFO.branch: $(Base.GIT_VERSION_INFO.branch)
    """
    return Documenter.DeployDecision(; all_ok=false)
end

const devurl = "v$(VERSION.major).$(VERSION.minor)-dev"

# Hack to make rc docs visible in the version selector
struct Versions versions end
function Documenter.Writers.HTMLWriter.expand_versions(dir::String, v::Versions)
    # Find all available docs
    available_folders = readdir(dir)
    cd(() -> filter!(!islink, available_folders), dir)
    filter!(x -> occursin(Base.VERSION_REGEX, x), available_folders)

    # Look for docs for an "active" release candidate and insert it
    vnums = [VersionNumber(x) for x in available_folders]
    master_version = maximum(vnums)
    filter!(x -> x.major == 1 && x.minor == master_version.minor-1, vnums)
    rc = maximum(vnums)
    if !isempty(rc.prerelease) && occursin(r"^rc", rc.prerelease[1])
        src = "v$(rc)"
        @assert src ∈ available_folders
        push!(v.versions, src => src, pop!(v.versions))
    end

    return Documenter.Writers.HTMLWriter.expand_versions(dir, v.versions)
end

if "deploy" in ARGS
    deploydocs(
        repo = "github.com/JuliaLang/docs.julialang.org.git",
        deploy_config = BuildBotConfig(),
        target = joinpath(buildroot, "doc", "_build", "html", "en"),
        dirname = "en",
        devurl = devurl,
        versions = Versions(["v#.#", devurl => devurl]),
        archive = get(ENV, "DOCUMENTER_ARCHIVE", nothing),
    )
else
    @info "Skipping deployment ('deploy' not passed)"
end
