# This file is a part of Julia. License is MIT: http://julialang.org/license

bt = backtrace()
have_backtrace = false
for l in bt
    lkup = ccall(:jl_lookup_code_address, Any, (Ptr{Void},), l)
    if lkup[1] == :backtrace
        @test lkup[4] == false # fromC
        have_backtrace = true
        break
    end
end

@test have_backtrace

# these could fail on an embedded installation
# but for now, we don't handle that case
dlls = Libdl.dllist()
@test !isempty(dlls)
@test length(dlls) > 3 # at a bare minimum, probably have some version of libstdc, libgcc, libjulia, ...
if @unix? true : (Base.windows_version() >= Base.WINDOWS_VISTA_VER)
    for dl in dlls
        if isfile(dl)
            @test Base.samefile(Libdl.dlpath(dl), dl)
        end
    end
end
@test length(filter(dlls) do dl
        return ismatch(Regex("^libjulia(?:.*)\.$(Libdl.dlext)(?:\..+)?\$"), basename(dl))
    end) == 1 # look for something libjulia-like (but only one)
