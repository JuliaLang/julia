# This file is a part of Julia. License is MIT: http://julialang.org/license

import .Docs: keywords

include("helpdb/BLAS.jl")
include("helpdb/Libdl.jl")
include("helpdb/Libc.jl")
include("helpdb/Collections.jl")
include("helpdb/Profile.jl")
include("helpdb/Cartesian.jl")
include("helpdb/Base.jl")

# Dates

doc"""
    firstdayofweek(dt::TimeType) -> TimeType

Adjusts `dt` to the Monday of its week.
"""
Dates.firstdayofweek

doc"""
    datetime2unix(dt::DateTime) -> Float64

Takes the given `DateTime` and returns the number of seconds since the unix epoch as a `Float64`.
"""
Dates.datetime2unix

doc"""
    dayofweekofmonth(dt::TimeType) -> Int

For the day of week of `dt`, returns which number it is in `dt`'s month. So if the day of the week of `dt` is Monday, then `1 = First Monday of the month, 2 = Second Monday of the month, etc.` In the range 1:5.
"""
Dates.dayofweekofmonth

doc"""
    monthabbr(dt::TimeType; locale="english") -> AbstractString

Return the abbreviated month name of the `Date` or `DateTime` in the given `locale`.
"""
Dates.monthabbr

doc"""
    datetime2julian(dt::DateTime) -> Float64

Takes the given `DateTime` and returns the number of Julian calendar days since the julian epoch as a `Float64`.
"""
Dates.datetime2julian

doc"""
    dayabbr(dt::TimeType; locale="english") -> AbstractString

Return the abbreviated name corresponding to the day of the week of the `Date` or `DateTime` in the given `locale`.
"""
Dates.dayabbr

doc"""
```rst
..  DateTime(y, [m, d, h, mi, s, ms]) -> DateTime

Construct a ``DateTime`` type by parts. Arguments must be convertible to ``Int64``.
```
"""
Dates.DateTime(y)

doc"""
```rst
..  DateTime(periods::Period...) -> DateTime

Constuct a ``DateTime`` type by ``Period`` type parts. Arguments may be in any order.
DateTime parts not provided will default to the value of ``Dates.default(period)``.
```
"""
Dates.DateTime(periods::Dates.Period...)

doc"""
```rst
..  DateTime(f::Function, y[, m, d, h, mi, s]; step=Day(1), negate=false, limit=10000) -> DateTime

Create a ``DateTime`` through the adjuster API. The starting point will be constructed from the
provided ``y, m, d...`` arguments, and will be adjusted until ``f::Function`` returns ``true``. The step size in
adjusting can be provided manually through the ``step`` keyword. If ``negate=true``, then the adjusting
will stop when ``f::Function`` returns ``false`` instead of ``true``. ``limit`` provides a limit to
the max number of iterations the adjustment API will pursue before throwing an error (in the case that ``f::Function`` is never satisfied).
```
"""
Dates.DateTime(f::Function, y)

doc"""
```rst
..  DateTime(dt::Date) -> DateTime

Converts a ``Date`` type to a ``DateTime``.
The hour, minute, second, and millisecond parts of the new ``DateTime`` are assumed to be zero.
```
"""
Dates.DateTime(dt::Date)

doc"""
```rst
..  DateTime(dt::AbstractString, format::AbstractString; locale="english") -> DateTime

Construct a ``DateTime`` type by parsing the ``dt`` date string following the pattern given in
the ``format`` string. The following codes can be used for constructing format strings:

=============== ========= ===============================================================
Code            Matches    Comment
=============== ========= ===============================================================
``y``           1996, 96  Returns year of 1996, 0096
``m``           1, 01     Matches 1 or 2-digit months
``u``           Jan       Matches abbreviated months according to the ``locale`` keyword
``U``           January   Matches full month names according to the ``locale`` keyword
``d``           1, 01     Matches 1 or 2-digit days
``H``           00        Matches hours
``M``           00        Matches minutes
``S``           00        Matches seconds
``s``           .500      Matches milliseconds
``e``           Mon, Tues Matches abbreviated days of the week
``E``           Monday    Matches full name days of the week
``yyyymmdd``    19960101  Matches fixed-width year, month, and day
=============== ========= ===============================================================

All characters not listed above are treated as delimiters between date and time slots.
So a ``dt`` string of "1996-01-15T00:00:00.0" would have a ``format`` string like "y-m-dTH:M:S.s".
```
"""
Dates.DateTime(dt::AbstractString, format::AbstractString)

doc"""
```rst
..  DateTime(dt::AbstractString, df::DateFormat) -> DateTime

Similar form as above for parsing a ``DateTime``, but passes a ``DateFormat`` object instead of a raw formatting string. It is more efficient if similarly formatted date strings will be parsed repeatedly to first create a ``DateFormat`` object then use this method for parsing.
```
"""
Dates.DateTime(dt::AbstractString, df::Dates.DateFormat)

doc"""
    datetime2rata(dt::TimeType) -> Int64

Returns the number of Rata Die days since epoch from the given `Date` or `DateTime`.
"""
Dates.datetime2rata

doc"""
    monthname(dt::TimeType; locale="english") -> AbstractString

Return the full name of the month of the `Date` or `DateTime` in the given `locale`.
"""
Dates.monthname

doc"""
    dayname(dt::TimeType; locale="english") -> AbstractString

Return the full day name corresponding to the day of the week of the `Date` or `DateTime` in the given `locale`.
"""
Dates.dayname

doc"""
    Date(y, [m, d]) -> Date

Construct a `Date` type by parts. Arguments must be convertible to `Int64`.
"""
Dates.Date(y)

doc"""
    Date(period::Period...) -> Date

Constuct a `Date` type by `Period` type parts. Arguments may be in any order. `Date` parts not provided will default to the value of `Dates.default(period)`.
"""
Dates.Date(period::Dates.Period...)

doc"""
    Date(f::Function, y[, m]; step=Day(1), negate=false, limit=10000) -> Date

Create a `Date` through the adjuster API. The starting point will be constructed from the provided `y, m` arguments, and will be adjusted until `f::Function` returns `true`. The step size in adjusting can be provided manually through the `step` keyword. If `negate=true`, then the adjusting will stop when `f::Function` returns `false` instead of `true`. `limit` provides a limit to the max number of iterations the adjustment API will pursue before throwing an error (given that `f::Function` is never satisfied).
"""
Dates.Date(f::Function, y)

doc"""
    Date(dt::DateTime) -> Date

Converts a `DateTime` type to a `Date`. The hour, minute, second, and millisecond parts of the `DateTime` are truncated, so only the year, month and day parts are used in construction.
"""
Dates.Date(dt::DateTime)

doc"""
    Date(dt::AbstractString, format::AbstractString; locale="english") -> Date

Construct a `Date` type by parsing a `dt` date string following the pattern given in the `format` string. Follows the same conventions as `DateTime` above.
"""
Dates.Date(dt::AbstractString, format::AbstractString)

doc"""
    Date(dt::AbstractString, df::DateFormat) -> Date

Parse a date from a date string `dt` using a `DateFormat` object `df`.
"""
Dates.Date(dt::AbstractString, df::Dates.DateFormat)

doc"""
    firstdayofmonth(dt::TimeType) -> TimeType

Adjusts `dt` to the first day of its month.
"""
Dates.firstdayofmonth

doc"""
    tonext(dt::TimeType,dow::Int;same::Bool=false) -> TimeType

Adjusts `dt` to the next day of week corresponding to `dow` with `1 = Monday, 2 = Tuesday, etc`. Setting `same=true` allows the current `dt` to be considered as the next `dow`, allowing for no adjustment to occur.
"""
Dates.tonext(::Dates.TimeType,::Int,?)

doc"""
    tonext(func::Function,dt::TimeType;step=Day(1),negate=false,limit=10000,same=false) -> TimeType

Adjusts `dt` by iterating at most `limit` iterations by `step` increments until `func` returns `true`. `func` must take a single `TimeType` argument and return a `Bool`. `same` allows `dt` to be considered in satisfying `func`. `negate` will make the adjustment process terminate when `func` returns `false` instead of `true`.
"""
Dates.tonext(::Function,::Dates.TimeType)

doc"""
    dayofyear(dt::TimeType) -> Int

Returns the day of the year for `dt` with January 1st being day 1.
"""
Dates.dayofyear

doc"""
    tolast(dt::TimeType,dow::Int;of=Month) -> TimeType

Adjusts `dt` to the last `dow` of its month. Alternatively, `of=Year` will adjust to the last `dow` of the year.
"""
Dates.tolast

doc"""
    firstdayofquarter(dt::TimeType) -> TimeType

Adjusts `dt` to the first day of its quarter.
"""
Dates.firstdayofquarter

doc"""
```rst
..  julian2datetime(julian_days) -> DateTime

Takes the number of Julian calendar days since epoch
``-4713-11-24T12:00:00`` and returns the corresponding ``DateTime``.
```
"""
Dates.julian2datetime

doc"""
    year(dt::TimeType) -> Int64
    month(dt::TimeType) -> Int64
    week(dt::TimeType) -> Int64
    day(dt::TimeType) -> Int64
    hour(dt::TimeType) -> Int64
    minute(dt::TimeType) -> Int64
    second(dt::TimeType) -> Int64
    millisecond(dt::TimeType) -> Int64

Return the field part of a `Date` or `DateTime` as an `Int64`.
"""
Dates.year

doc"""
    toprev(dt::TimeType,dow::Int;same::Bool=false) -> TimeType

Adjusts `dt` to the previous day of week corresponding to `dow` with `1 = Monday, 2 = Tuesday, etc`. Setting `same=true` allows the current `dt` to be considered as the previous `dow`, allowing for no adjustment to occur.
"""
Dates.toprev(::Dates.TimeType,::Int,?)

doc"""
    toprev(func::Function,dt::TimeType;step=Day(-1),negate=false,limit=10000,same=false) -> TimeType

Adjusts `dt` by iterating at most `limit` iterations by `step` increments until `func` returns `true`. `func` must take a single `TimeType` argument and return a `Bool`. `same` allows `dt` to be considered in satisfying `func`. `negate` will make the adjustment process terminate when `func` returns `false` instead of `true`.
"""
Dates.toprev(::Function,::Dates.TimeType)

doc"""
    daysinyear(dt::TimeType) -> Int

Returns 366 if the year of `dt` is a leap year, otherwise returns 365.
"""
Dates.daysinyear

doc"""
```rst
..  trunc(dt::TimeType, ::Type{Period}) -> TimeType

Truncates the value of ``dt`` according to the provided ``Period`` type.
E.g. if ``dt`` is ``1996-01-01T12:30:00``, then ``trunc(dt,Day) == 1996-01-01T00:00:00``.
```
"""
Dates.trunc(::Dates.TimeType, ::Type{Dates.Period})

doc"""
    daysinmonth(dt::TimeType) -> Int

Returns the number of days in the month of `dt`. Value will be 28, 29, 30, or 31.
"""
Dates.daysinmonth

doc"""
    yearmonth(dt::TimeType) -> (Int64, Int64)

Simultaneously return the year and month parts of a `Date` or `DateTime`.
"""
Dates.yearmonth

doc"""
    daysofweekinmonth(dt::TimeType) -> Int

For the day of week of `dt`, returns the total number of that day of the week in `dt`'s month. Returns 4 or 5. Useful in temporal expressions for specifying the last day of a week in a month by including `dayofweekofmonth(dt) == daysofweekinmonth(dt)` in the adjuster function.
"""
Dates.daysofweekinmonth

doc"""
    yearmonthday(dt::TimeType) -> (Int64, Int64, Int64)

Simultaneously return the year, month, and day parts of a `Date` or `DateTime`.
"""
Dates.yearmonthday

doc"""
    Dates.DateFormat(format::AbstractString) -> DateFormat

Construct a date formatting object that can be passed repeatedly for parsing similarly formatted date strings. `format` is a format string in the form described above (e.g. `"yyyy-mm-dd"`).
"""
Dates.Dates.DateFormat

doc"""
    lastdayofweek(dt::TimeType) -> TimeType

Adjusts `dt` to the Sunday of its week.
"""
Dates.lastdayofweek

doc"""
    recur{T<:TimeType}(func::Function,dr::StepRange{T};negate=false,limit=10000) -> Vector{T}

`func` takes a single TimeType argument and returns a `Bool` indicating whether the input should be "included" in the final set. `recur` applies `func` over each element in the range of `dr`, including those elements for which `func` returns `true` in the resulting Array, unless `negate=true`, then only elements where `func` returns `false` are included.
"""
Dates.recur

doc"""
    monthday(dt::TimeType) -> (Int64, Int64)

Simultaneously return the month and day parts of a `Date` or `DateTime`.
"""
Dates.monthday

doc"""
    default(p::Period) -> Period

Returns a sensible "default" value for the input Period by returning `one(p)` for Year, Month, and Day, and `zero(p)` for Hour, Minute, Second, and Millisecond.
"""
Dates.default

doc"""
```rst
..  unix2datetime(x) -> DateTime

Takes the number of seconds since unix epoch ``1970-01-01T00:00:00``
and converts to the corresponding ``DateTime``.
```
"""
Dates.unix2datetime

doc"""
    eps(::DateTime) -> Millisecond
    eps(::Date) -> Day

Returns `Millisecond(1)` for `DateTime` values and `Day(1)` for `Date` values.
"""
Dates.eps(::Union{Date,DateTime})

doc"""
    firstdayofyear(dt::TimeType) -> TimeType

Adjusts `dt` to the first day of its year.
"""
Dates.firstdayofyear

doc"""
```rst
..  rata2datetime(days) -> DateTime

Takes the number of Rata Die days since epoch ``0000-12-31T00:00:00``
and returns the corresponding ``DateTime``.
```
"""
Dates.rata2datetime

doc"""
    now() -> DateTime

Returns a `DateTime` corresponding to the user's system time including the system timezone locale.
"""
now

doc"""
    now(::Type{UTC}) -> DateTime

Returns a `DateTime` corresponding to the user's system time as UTC/GMT.
"""
Dates.now(::Type{Dates.UTC})

doc"""
    isleapyear(dt::TimeType) -> Bool

Returns `true` if the year of `dt` is a leap year.
"""
Dates.isleapyear

doc"""
    today() -> Date

Returns the date portion of `now()`.
"""
Dates.today

doc"""
    lastdayofyear(dt::TimeType) -> TimeType

Adjusts `dt` to the last day of its year.
"""
Dates.lastdayofyear

doc"""
    tofirst(dt::TimeType,dow::Int;of=Month) -> TimeType

Adjusts `dt` to the first `dow` of its month. Alternatively, `of=Year` will adjust to the first `dow` of the year.
"""
Dates.tofirst

doc"""
    lastdayofmonth(dt::TimeType) -> TimeType

Adjusts `dt` to the last day of its month.
"""
Dates.lastdayofmonth

doc"""
    dayofweek(dt::TimeType) -> Int64

Returns the day of the week as an `Int64` with `1 = Monday, 2 = Tuesday, etc.`.
"""
Dates.dayofweek

doc"""
    Year(dt::TimeType) -> Year
    Month(dt::TimeType) -> Month
    Week(dt::TimeType) -> Week
    Day(dt::TimeType) -> Day
    Hour(dt::TimeType) -> Hour
    Minute(dt::TimeType) -> Minute
    Second(dt::TimeType) -> Second
    Millisecond(dt::TimeType) -> Millisecond

Return the field part of a `Date` or `DateTime` as a `Period` type.
"""
Dates.Year(dt::Dates.TimeType)

doc"""
    Year(v)
    Month(v)
    Week(v)
    Day(v)
    Hour(v)
    Minute(v)
    Second(v)
    Millisecond(v)

Construct a `Period` type with the given `v` value. Input must be losslessly
convertible to an `Int64`.
"""
Dates.Year(v)

doc"""
    quarterofyear(dt::TimeType) -> Int

Returns the quarter that `dt` resides in. Range of value is 1:4.
"""
Dates.quarterofyear

doc"""
    dayofquarter(dt::TimeType) -> Int

Returns the day of the current quarter of `dt`. Range of value is 1:92.
"""
Dates.dayofquarter

doc"""
    lastdayofquarter(dt::TimeType) -> TimeType

Adjusts `dt` to the last day of its quarter.
"""
Dates.lastdayofquarter

# Base.Pkg

doc"""
    build()

Run the build scripts for all installed packages in depth-first recursive order.
"""
Pkg.build()

doc"""
    build(pkgs...)

Run the build script in `deps/build.jl` for each package in `pkgs` and all of their dependencies in depth-first recursive order. This is called automatically by `Pkg.resolve()` on all installed or updated packages.
"""
Pkg.build(pkgs...)

doc"""
    init(meta::AbstractString=DEFAULT_META, branch::AbstractString=META_BRANCH)

Initialize `Pkg.dir()` as a package directory. This will be done automatically when the `JULIA_PKGDIR` is not set and `Pkg.dir()` uses its default value. As part of this process, clones a local METADATA git repository from the site and branch specified by its arguments, which are typically not provided. Explicit (non-default) arguments can be used to support a custom METADATA setup.
"""
Pkg.init()

doc"""
    pin(pkg)

Pin `pkg` at the current version. To go back to using the newest compatible released version, use `Pkg.free(pkg)`
"""
Pkg.pin(pkg)

doc"""
    pin(pkg, version)

Pin `pkg` at registered version `version`.
"""
Pkg.pin(pkg, version)

doc"""
    resolve()

Determines an optimal, consistent set of package versions to install or upgrade to. The optimal set of package versions is based on the contents of `Pkg.dir("REQUIRE")` and the state of installed packages in `Pkg.dir()`, Packages that are no longer required are moved into `Pkg.dir(".trash")`.
"""
Pkg.resolve()

doc"""
    available() -> Vector{ASCIIString}

Returns the names of available packages.
"""
Pkg.available()

doc"""
    available(pkg) -> Vector{VersionNumber}

Returns the version numbers available for package `pkg`.
"""
Pkg.available(pkg)

doc"""
    rm(pkg)

Remove all requirement entries for `pkg` from `Pkg.dir("REQUIRE")` and call `Pkg.resolve()`.
"""
Pkg.rm(pkg)

doc"""
    free(pkg)

Free the package `pkg` to be managed by the package manager again. It calls `Pkg.resolve()` to determine optimal package versions after. This is an inverse for both `Pkg.checkout` and `Pkg.pin`.

You can also supply an iterable collection of package names, e.g., `Pkg.free(("Pkg1", "Pkg2"))` to free multiple packages at once.
"""
Pkg.free()

doc"""
    status()

Prints out a summary of what packages are installed and what version and state they're in.
"""
Pkg.status

doc"""
    edit()

Opens `Pkg.dir("REQUIRE")` in the editor specified by the `VISUAL` or `EDITOR` environment variables; when the editor command returns, it runs `Pkg.resolve()` to determine and install a new optimal set of installed package versions.
"""
Pkg.edit()

doc"""
    clone(url, [pkg])

Clone a package directly from the git URL `url`. The package does not need to be a registered in `Pkg.dir("METADATA")`. The package repo is cloned by the name `pkg` if provided; if not provided, `pkg` is determined automatically from `url`.
"""
Pkg.clone(url,?)

doc"""
    clone(pkg)

If `pkg` has a URL registered in `Pkg.dir("METADATA")`, clone it from that URL on the default branch. The package does not need to have any registered versions.
"""
Pkg.clone(pkg)

doc"""
    checkout(pkg, [branch="master"]; merge=true, pull=true)

Checkout the `Pkg.dir(pkg)` repo to the branch `branch`. Defaults to checking out the "master" branch. To go back to using the newest compatible released version, use `Pkg.free(pkg)`. Changes are merged (fast-forward only) if the keyword argument `merge == true`, and the latest version is pulled from the upsream repo if `pull == true`.
"""
Pkg.checkout(pkg)

doc"""
    update()

Update package the metadata repo – kept in `Pkg.dir("METADATA")` – then update any fixed packages that can safely be pulled from their origin; then call `Pkg.resolve()` to determine a new optimal set of packages versions.
"""
Pkg.update

doc"""
    add(pkg, vers...)

Add a requirement entry for `pkg` to `Pkg.dir("REQUIRE")` and call `Pkg.resolve()`. If `vers` are given, they must be `VersionNumber` objects and they specify acceptable version intervals for `pkg`.
"""
Pkg.add(pkg, vers...)

doc"""
    test()

Run the tests for all installed packages ensuring that each package's test dependencies are installed for the duration of the test. A package is tested by running its `test/runtests.jl` file and test dependencies are specified in `test/REQUIRE`.
"""
Pkg.test()

doc"""
    test(pkgs...)

Run the tests for each package in `pkgs` ensuring that each package's test dependencies are installed for the duration of the test. A package is tested by running its `test/runtests.jl` file and test dependencies are specified in `test/REQUIRE`.
"""
Pkg.test(pkgs...)

doc"""
    dir() -> AbstractString

Returns the absolute path of the package directory. This defaults to `joinpath(homedir(),".julia","v$(VERSION.major).$(VERSION.minor)")` on all platforms (i.e. `~/.julia/v0.4` in UNIX shell syntax). If the `JULIA_PKGDIR` environment variable is set, then that path is used in the returned value as `joinpath(ENV["JULIA_PKGDIR"],"v$(VERSION.major).$(VERSION.minor)")`. If `JULIA_PKGDIR` is a relative path, it is interpreted relative to whatever the current working directory is.
"""
Pkg.dir()

doc"""
    dir(names...) -> AbstractString

Equivalent to `normpath(Pkg.dir(),names...)` – i.e. it appends path components to the package directory and normalizes the resulting path. In particular, `Pkg.dir(pkg)` returns the path to the package `pkg`.
"""
Pkg.dir(names...)

doc"""
    installed() -> Dict{ASCIIString,VersionNumber}

Returns a dictionary mapping installed package names to the installed version number of each package.
"""
Pkg.installed()

doc"""
    installed(pkg) -> Void | VersionNumber

If `pkg` is installed, return the installed version number, otherwise return `nothing`.
"""
Pkg.installed(pkg)

doc"""
    randjump(r::MersenneTwister, jumps, [jumppoly]) -> Vector{MersenneTwister}

Create an array of the size `jumps` of initialized `MersenneTwister` RNG objects where the first RNG object given as a parameter and following `MersenneTwister` RNGs in the array initialized such that a state of the RNG object in the array would be moved forward (without generating numbers) from a previous RNG object array element on a particular number of steps encoded by the jump polynomial `jumppoly`.

Default jump polynomial moves forward `MersenneTwister` RNG state by 10^20 steps.
"""
randjump

doc"""
```rst
..  \:(start, [step], stop)

Range operator. ``a:b`` constructs a range from ``a`` to ``b`` with a step size of 1, and ``a:s:b`` is similar but uses a step size of ``s``. These syntaxes call the function ``colon``.
The colon is also used in indexing to select whole dimensions.
```
"""
colon(start, step, stop)

doc"""
```rst
..  $(x, y)

Bitwise exclusive or
```
"""
Base.(:$)(x, y)

doc"""
    getsockname(sock::Union{TCPServer, TCPSocket}) -> (IPAddr,UInt16)

Get the IP address and the port that the given TCP socket is connected to (or bound to, in the case of TCPServer).
"""
getsockname

doc"""
    Base.remoteref_id(r::AbstractRemoteRef) -> (whence, id)

A low-level API which returns the unique identifying tuple for a remote reference. A reference id is a tuple of two
elements - pid where the reference was created from and a one-up number from that node.
"""
Base.remoteref_id

doc"""
    Base.channel_from_id(refid) -> c

A low-level API which returns the backing AbstractChannel for an id returned by `remoteref_id`. The call is valid only on the node where the backing channel exists.
"""
Base.channel_from_id

doc"""
    Base.worker_id_from_socket(s::IO) -> pid

A low-level API which given a `IO` connection, returns the pid of the worker it is connected to. This is useful when writing custom `serialize` methods for a type, which
optimizes the data written out depending on the receiving process id.
"""
Base.worker_id_from_socket



