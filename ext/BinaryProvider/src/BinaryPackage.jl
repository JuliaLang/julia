# Package objects provide a _slightly_ higher-level API for dealing with
# installing/uninstalling tarballs to a Prefix.
export BinaryPackage, install, uninstall, satisfied

"""
A `BinaryPackage` collects all the information needed to download and install a
tarball containing binary objects; it has the `url` to download form, the
`hash` to verify package integrity, a list of `products` to check for proper
functioning after installation, and a list of `dependencies` that must also be
installed before this package can be used.

There exist `install()`, `uninstall()` and `satisfied()` methods for
`BinaryPackage` objects, similar to the lower-level versions that take direct
`url` and `hash` arguments.
"""
struct BinaryPackage
    url::String
    hash::String
    platform::Platform
    products::Vector{Product}
    dependencies::Vector{BinaryPackage}

    function BinaryPackage(url::AbstractString,
                           hash::AbstractString,
                           platform::Platform,
                           products::Vector{Product}=Product[],
                           dependencies::Vector{BinaryPackage}=BinaryPackage[])
        return new(url, hash, platform, products, dependencies)
    end
end

function pkg_name(pkg::BinaryPackage)
    name = basename(pkg.url)
    if endswith(name, ".tar.gz")
        return name[1:end-7]
    end
    return name
end

"""
`install(pkg::BinaryPackage; verbose::Bool = false, kwargs...)`

A thin wrapper over the main `install(url, path; ...)` method.  Installs all
of `pkg`'s dependencies, then installs `pkg`, but only of it is not already
satisfied.
"""
function install(pkg::BinaryPackage; verbose::Bool = false, kwargs...)
    name = pkg_name(pkg)

    # If we are already satisfied, don't do nuthin'
    if satisfied(pkg)
        if verbose
            info("Not installing $(name), as it is already satisfied")
        end
        return true
    end

    # Begin by installing all the dependencies if they are not already
    for dep in pkg.dependencies
        # TODO: We may want to handle this through `Pkg3` operations
        install(dep; verbose=verbose, kwargs...)
    end

    # Finally, install ourselves
    install(pkg.url, pkg.hash; verbose=verbose, kwargs...)

    # Check to see if we are actually satisfied
    if !satisfied(pkg; verbose=verbose)
        warn("$(name) did not satisfy itself after installation!")
        return false
    end

    return true
end

"""
manifest_path(pkg::BinaryPackage; prefix::Prefix = global_prefix(),
                                  verbose::Bool = false)

Discovers the manifest path for the given `BinaryPackage` within the given
`Prefix`.  First attempts to guess from the `url` what the manifest file would
have been named, if that doesn't work, will search for manifests that contain
any of the `products` that are within `pkg`.  If neither approach works, throws
an error.
"""
function manifest_path(pkg::BinaryPackage; prefix::Prefix = global_prefix(),
                                           verbose::Bool = false)
    name = pkg_name(pkg)
    # First, see if we can auto-guess the manifest file path:
    manifest_path = manifest_from_url(pkg.url, prefix=prefix)
    if isfile(manifest_path)
        if verbose
            info("Correctly auto-guessed manifest path $(manifest_path)")
        end
        return manifest_path
    end

    if verbose
        info("Could not auto-guess manifest path for $(name)")
    end

    # Otherwise, let's try to guess from our products
    if isempty(pkg.products)
        msg = """
        Cannot find manifest path for package $(name) with unguessable manifest
        file and no products.
        """
        error(replace(strip(msg),"\n", " "))
    end

    for product in pkg.products
        product_path = locate(product, platform=pkg.platform)
        if product_path != nothing
            try
                manifest_path = manifest_for_file(product_path; prefix=prefix)
                relmani = relpath(manifest_path, prefix.path)
                relprod = relpath(product_path, prefix.path)
                info("Found $(relmani) for product $(relprod)")
                return manifest_path
            end
        end
    end

    error("Cannot find manifest path for package $(name)")
end


"""
uninstall(pkg::BinaryPackage; prefix::Prefix = global_prefix,
                              verbose::Bool = false)

Uninstall `pkg` from the given `prefix` by automatically determining the
manifest path created when the package was installed.  Throws an error if
this `pkg` was not installed in the first place.
"""
function uninstall(pkg::BinaryPackage; prefix::Prefix = global_prefix,
                                       verbose::Bool = false)
    # Find the manifest path for this pkg, then uninstall it
    manipath = manifest_path(pkg; prefix=prefix, verbose=verbose)
    uninstall(manipath; verbose=verbose)
end

"""
`satisfied(pkg::BinaryPackage; verbose::Bool = false)`

Returns `true` if all products defined within `pkg` are satisfied.
"""
function satisfied(pkg::BinaryPackage; verbose::Bool = false)
    s = p -> satisfied(p; platform=pkg.platform, verbose=verbose)
    return all(s(p) for p in pkg.products)
end
