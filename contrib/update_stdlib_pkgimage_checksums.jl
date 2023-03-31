
Sys.isapple() || error("This tool only exists to update after MacOS codesigning")

# Updates cache file checksum for pkgimg i.e. after codesigning pkgimages
function update_cache_pkgimg_checksum!(ji_file::String, pkgimg_file::String)
    crc_so = open(Base._crc32c, pkgimg_file, "r")
    open(ji_file, "r+") do f
        if Base.isvalid_pkgimage_crc(f, pkgimg_file)
            @info "pkgimage checksum already correct in $(repr(ji_file))"
            return
        end
        if iszero(Base.isvalid_cache_header(f))
            error("Invalid header in cache file $(repr(ji_file)).")
        end
        seekend(f)
        seek(f, filesize(f) - 8)
        write(f, crc_so)
        seekstart(f)
        write(f, Base._crc32c(f))

        if Base.isvalid_pkgimage_crc(f, pkgimg_file)
            @info "pkgimage checksum updated in $(repr(ji_file)) for $(repr(pkgimg_file))"
        else
            error("Failed to set checksum correctly")
        end
    end
    return nothing
end

STDLIB_DIR = abspath(joinpath(@__DIR__, "..", "usr", "share", "julia", "compiled", "v$(VERSION.major).$(VERSION.minor)"))

for dir in readdir(STDLIB_DIR, join = true)
    for basename in unique(first.(splitext.(readdir(dir, join=true))))
        endswith(basename, ".dylib") && continue # happens because of .dSYM files
        ji_file = basename * ".ji"
        pkgimg_file = basename * ".dylib"
        update_cache_pkgimg_checksum!(ji_file, pkgimg_file)
    end
end
