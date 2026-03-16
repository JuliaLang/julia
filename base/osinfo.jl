
# NB: This file is `Core.eval`-uated into the (pre-existing) module Sys

export KERNEL,
       isapple,
       isbsd,
       isdragonfly,
       isfreebsd,
       islinux,
       isnetbsd,
       isopenbsd,
       isunix,
       iswindows,
       isjsvm,
       detectwsl

"""
    Sys.KERNEL::Symbol

A symbol representing the name of the operating system, as returned by `uname` of the build configuration.
"""
const KERNEL = ccall(:jl_get_UNAME, Any, ())::Symbol
"""
    Sys.isunix([os])

Predicate for testing if the OS provides a Unix-like interface.
See documentation in [Handling Operating System Variation](@ref).
"""
function isunix(os::Symbol)
    if iswindows(os)
        return false
    elseif islinux(os) || isbsd(os)
        return true
    elseif os === :Emscripten
        # Emscripten implements the POSIX ABI and provides traditional
        # Unix-style operating system functions such as file system support.
        # Therefore, we consider it a unix, even though this need not be
        # generally true for a jsvm embedding.
        return true
    else
        throw(ArgumentError("unknown operating system \"$os\""))
    end
end

"""
    Sys.islinux([os])

Predicate for testing if the OS is a derivative of Linux.
See documentation in [Handling Operating System Variation](@ref).
"""
islinux(os::Symbol) = (os === :Linux)

"""
    Sys.isbsd([os])

Predicate for testing if the OS is a derivative of BSD.
See documentation in [Handling Operating System Variation](@ref).

!!! note
    The Darwin kernel descends from BSD, which means that `Sys.isbsd()` is
    `true` on macOS systems. To exclude macOS from a predicate, use
    `Sys.isbsd() && !Sys.isapple()`.
"""
isbsd(os::Symbol) = (isfreebsd(os) || isopenbsd(os) || isnetbsd(os) || isdragonfly(os) || isapple(os))

"""
    Sys.isfreebsd([os])

Predicate for testing if the OS is a derivative of FreeBSD.
See documentation in [Handling Operating System Variation](@ref).

!!! note
    Not to be confused with `Sys.isbsd()`, which is `true` on FreeBSD but also on
    other BSD-based systems. `Sys.isfreebsd()` refers only to FreeBSD.
!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.
"""
isfreebsd(os::Symbol) = (os === :FreeBSD)

"""
    Sys.isopenbsd([os])

Predicate for testing if the OS is a derivative of OpenBSD.
See documentation in [Handling Operating System Variation](@ref).

!!! note
    Not to be confused with `Sys.isbsd()`, which is `true` on OpenBSD but also on
    other BSD-based systems. `Sys.isopenbsd()` refers only to OpenBSD.
!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.
"""
isopenbsd(os::Symbol) = (os === :OpenBSD)

"""
    Sys.isnetbsd([os])

Predicate for testing if the OS is a derivative of NetBSD.
See documentation in [Handling Operating System Variation](@ref).

!!! note
    Not to be confused with `Sys.isbsd()`, which is `true` on NetBSD but also on
    other BSD-based systems. `Sys.isnetbsd()` refers only to NetBSD.
!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.
"""
isnetbsd(os::Symbol) = (os === :NetBSD)

"""
    Sys.isdragonfly([os])

Predicate for testing if the OS is a derivative of DragonFly BSD.
See documentation in [Handling Operating System Variation](@ref).

!!! note
    Not to be confused with `Sys.isbsd()`, which is `true` on DragonFly but also on
    other BSD-based systems. `Sys.isdragonfly()` refers only to DragonFly.
!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.
"""
isdragonfly(os::Symbol) = (os === :DragonFly)

"""
    Sys.iswindows([os])

Predicate for testing if the OS is a derivative of Microsoft Windows NT.
See documentation in [Handling Operating System Variation](@ref).
"""
iswindows(os::Symbol) = (os === :Windows || os === :NT)

"""
    Sys.isapple([os])

Predicate for testing if the OS is a derivative of Apple Macintosh OS X or Darwin.
See documentation in [Handling Operating System Variation](@ref).
"""
isapple(os::Symbol) = (os === :Apple || os === :Darwin)

"""
    Sys.isjsvm([os])

Predicate for testing if Julia is running in a JavaScript VM (JSVM),
including e.g. a WebAssembly JavaScript embedding in a web browser.

!!! compat "Julia 1.2"
    This function requires at least Julia 1.2.
"""
isjsvm(os::Symbol) = (os === :Emscripten)

"""
    Sys.detectwsl()

Runtime predicate for testing if Julia is running inside
Windows Subsystem for Linux (WSL).

!!! note
    Unlike `Sys.iswindows`, `Sys.islinux` etc., this is a runtime test, and thus
    cannot meaningfully be used in `@static if` constructs.

!!! compat "Julia 1.12"
    This function requires at least Julia 1.12.
"""
function detectwsl()
    # We use the same approach as canonical/snapd do to detect WSL
    islinux() && (
        isfile("/proc/sys/fs/binfmt_misc/WSLInterop")
        || isdir("/run/WSL")
    )
end

for f in (:isunix, :islinux, :isbsd, :isapple, :iswindows, :isfreebsd, :isopenbsd, :isnetbsd, :isdragonfly, :isjsvm)
    @eval $f() = $(getfield(@__MODULE__, f)(KERNEL))
end
