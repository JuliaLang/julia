# Hacky script to download the latest version of all packages registered in the
# General registry for testing the parser.
#
# This uses internal Pkg APIs and seems to work on Julia 1.7

using Pkg
using Downloads

registry = only(filter(r->r.name == "General", Pkg.Registry.reachable_registries()))

packages = []

for (uuid,pkg) in registry
    versions = collect(Pkg.Registry.registry_info(pkg).version_info)
    latest_ver, ver_info = last(sort(versions, by=first))
    if ver_info.yanked
        continue
    end

    push!(packages, (; uuid, pkg.name, version=latest_ver, ver_info.git_tree_sha1))

end

server = Pkg.pkg_server()
output_dir = "pkgs"
mkpath(output_dir)

asyncmap(packages, ntasks=5) do pkg
    url = "$server/package/$(pkg.uuid)/$(pkg.git_tree_sha1)"
    outfile_path = joinpath(output_dir, "$(pkg.name)_$(pkg.version).tgz")
    if isfile(outfile_path)
        @info "Skipping package" pkg
        return outfile_path
    else
        @info "Download package" url outfile_path
        for i=1:5
            try
                Downloads.download(url, outfile_path)
                break
            catch
                @error "Error downloading" pkg exception=current_exceptions()
            end
            sleep(i)
        end
    end
end
