.. module:: Dates

Dates Types
-----------

.. data:: Period
.. data:: Year
.. data:: Month
.. data:: Week
.. data:: Day
.. data:: Hour
.. data:: Minute
.. data:: Second
.. data:: Millisecond
   
   ``Period`` types represent discrete, human representations of time.

.. data:: Instant

   ``Instant`` types represent integer-based, machine representations of time as continuous timelines starting from an epoch.
   
.. data:: UTInstant{T}

   The ``UTInstant`` represents a machine timeline based on `UT` time (1 day = one revolution of the earth). The ``{T}`` is a ``Period`` parameter that indicates the resolution or precision of the instant.

.. data:: TimeType

   ``TimeType`` types wrap ``Instant`` machine instances to provide human representations of the machine instant.
   
.. data:: DateTime

   ``DateTime`` wraps a ``UTInstant{Millisecond}`` and interprets it according to the proleptic Gregorian calendar.
   
.. data:: Date

   ``Date`` wraps a ``UTInstant{Day}`` and interprets it according to the proleptic Gregorian calendar.

Dates Functions
---------------

All Dates functions are defined in the ``Dates`` module; note that only the ``Date``, ``DateTime``, and ``now`` functions are exported;
to use all other ``Dates`` functions, you'll need to prefix each function call with an explicit ``Dates.``, e.g. ``Dates.dayofweek(dt)``;
alternatively, you could call ``using Dates`` to bring all exported functions into ``Main`` to be used without the ``Dates.`` prefix.


.. function:: DateTime(y, [m, d, h, mi, s, ms]) -> DateTime

   Construct a DateTime type by parts. Arguments must be convertible to
   ``Int64``.

.. function:: DateTime(periods::Period...) -> DateTime

   Constuct a DateTime type by ``Period`` type parts. Arguments may be in any order.
   DateTime parts not provided will default to the value of ``Dates.default(period)``.

.. function:: DateTime(f::Function, y[, m, d, h, mi, s]; step=Day(1), negate=false, limit=10000) -> DateTime

    Create a DateTime through the adjuster API. The starting point will be constructed from the 
    provided ``y, m, d...`` arguments, and will be adjusted until ``f::Function`` returns true. The step size in
    adjusting can be provided manually through the ``step`` keyword. If ``negate=true``, then the adjusting
    will stop when ``f::Function`` returns false instead of true. ``limit`` provides a limit to
    the max number of iterations the adjustment API will pursue before throwing an error (in the case that ``f::Function``
    is never satisfied).

.. function:: DateTime(dt::Date) -> DateTime

    Converts a ``Date`` type to a ``DateTime``. The hour, minute, second, and millisecond
    parts of the new ``DateTime`` are assumed to be zero.

.. function:: DateTime(dt::AbstractString, format::AbstractString; locale="english") -> DateTime

   Construct a DateTime type by parsing the ``dt`` date string following the pattern given in
   the ``format`` string. The following codes can be used for constructing format strings:

   =============== ========= ===============================================================
   Code            Matches    Comment
   --------------- --------- ---------------------------------------------------------------
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
   So a ``dt`` string of "1996-01-15T00:00:00.0" would have a ``format`` string
   like "y-m-dTH:M:S.s".

.. function:: Date(y, [m, d]) -> Date

   Construct a ``Date`` type by parts. Arguments must be convertible to
   ``Int64``.

.. function:: Date(period::Period...) -> Date

   Constuct a Date type by ``Period`` type parts. Arguments may be in any order.
   Date parts not provided will default to the value of ``Dates.default(period)``.

.. function:: Date(f::Function, y[, m]; step=Day(1), negate=false, limit=10000) -> Date

    Create a Date through the adjuster API. The starting point will be constructed from the 
    provided ``y, m`` arguments, and will be adjusted until ``f::Function`` returns true. The step size in
    adjusting can be provided manually through the ``step`` keyword. If ``negate=true``, then the adjusting
    will stop when ``f::Function`` returns false instead of true. ``limit`` provides a limit to
    the max number of iterations the adjustment API will pursue before throwing an error (given that ``f::Function``
    is never satisfied).

.. function:: Date(dt::DateTime) -> Date

    Converts a ``DateTime`` type to a ``Date``. The hour, minute, second, and millisecond
    parts of the ``DateTime`` are truncated, so only the year, month and day parts are used in construction.

.. function:: Date(dt::AbstractString, format::AbstractString; locale="english") -> Date

   Construct a Date type by parsing a ``dt`` date string following the pattern given in
   the ``format`` string. Follows the same conventions as ``DateTime`` above.

.. function:: now() -> DateTime

   Returns a DateTime corresponding to the user's system
   time including the system timezone locale.

.. function:: nowutc() -> DateTime
  
   Returns a DateTime corresponding to the user's system
   time as UTC/GMT.
   
Accessor Functions
~~~~~~~~~~~~~~~~~~

.. function:: year(dt::TimeType) -> Int64
              month(dt::TimeType) -> Int64
              week(dt::TimeType) -> Int64
              day(dt::TimeType) -> Int64
              hour(dt::TimeType) -> Int64
              minute(dt::TimeType) -> Int64
              second(dt::TimeType) -> Int64
              millisecond(dt::TimeType) -> Int64

   Return the field part of a Date or DateTime as an ``Int64``.

.. function:: Year(dt::TimeType) -> Year
              Month(dt::TimeType) -> Month
              Week(dt::TimeType) -> Week
              Day(dt::TimeType) -> Day
              Hour(dt::TimeType) -> Hour
              Minute(dt::TimeType) -> Minute
              Second(dt::TimeType) -> Second
              Millisecond(dt::TimeType) -> Millisecond

   Return the field part of a Date or DateTime as a ``Period`` type.

.. function:: yearmonth(dt::TimeType) -> (Int64, Int64)

    Simultaneously return the year and month parts of a Date or DateTime.

.. function:: monthday(dt::TimeType) -> (Int64, Int64)

    Simultaneously return the month and day parts of a Date or DateTime.

.. function:: yearmonthday(dt::TimeType) -> (Int64, Int64, Int64)

    Simultaneously return the year, month, and day parts of a Date or DateTime.

Query Functions
~~~~~~~~~~~~~~~

.. function:: dayname(dt::TimeType; locale="english") -> AbstractString

   Return the full day name corresponding to the day of the week
   of the Date or DateTime in the given ``locale``.

.. function:: dayabbr(dt::TimeType; locale="english") -> AbstractString

   Return the abbreviated name corresponding to the day of the week
   of the Date or DateTime in the given ``locale``.

.. function:: dayofweek(dt::TimeType) -> Int64

    Returns the day of the week as an ``Int64`` with ``1 = Monday, 2 = Tuesday, etc.``.

.. function:: dayofweekofmonth(dt::TimeType) -> Int

    For the day of week of ``dt``, returns which number it is in ``dt``'s month.
    So if the day of the week of ``dt`` is Monday, then ``1 = First Monday of the month,
    2 = Second Monday of the month, etc.`` In the range 1:5.

.. function:: daysofweekinmonth(dt::TimeType) -> Int

    For the day of week of ``dt``, returns the total number of that day of the week
    in ``dt``'s month. Returns 4 or 5. Useful in temporal expressions for specifying
    the last day of a week in a month by including ``dayofweekofmonth(dt) == daysofweekinmonth(dt)``
    in the adjuster function.

.. function:: monthname(dt::TimeType; locale="english") -> AbstractString

   Return the full name of the month of the Date or DateTime in the given ``locale``.

.. function:: monthabbr(dt::TimeType; locale="english") -> AbstractString

   Return the abbreviated month name of the Date or DateTime in the given ``locale``.

.. function:: daysinmonth(dt::TimeType) -> Int

    Returns the number of days in the month of ``dt``. Value will be 28, 29, 30, or 31.

.. function:: isleapyear(dt::TimeType) -> Bool 

    Returns true if the year of ``dt`` is a leap year.

.. function:: dayofyear(dt::TimeType) -> Int

    Returns the day of the year for ``dt`` with January 1st being day 1.

.. function:: daysinyear(dt::TimeType) -> Int

    Returns 366 if the year of ``dt`` is a leap year, otherwise returns 365.

.. function:: quarterofyear(dt::TimeType) -> Int

    Returns the quarter that ``dt`` resides in. Range of value is 1:4.

.. function:: dayofquarter(dt::TimeType) -> Int

    Returns the day of the current quarter of ``dt``. Range of value is 1:92.

Adjuster Functions
~~~~~~~~~~~~~~~~~~

.. function:: trunc(dt::TimeType, ::Type{Period}) -> TimeType

    Truncates the value of ``dt`` according to the provided ``Period`` type.
    E.g. if ``dt`` is ``1996-01-01T12:30:00``, then ``trunc(dt,Day) == 1996-01-01T00:00:00``.

.. function:: firstdayofweek(dt::TimeType) -> TimeType

    Adjusts ``dt`` to the Monday of its week.

.. function:: lastdayofweek(dt::TimeType) -> TimeType

    Adjusts ``dt`` to the Sunday of its week.

.. function:: firstdayofmonth(dt::TimeType) -> TimeType

    Adjusts ``dt`` to the first day of its month.

.. function:: lastdayofmonth(dt::TimeType) -> TimeType

    Adjusts ``dt`` to the last day of its month.

.. function:: firstdayofyear(dt::TimeType) -> TimeType

    Adjusts ``dt`` to the first day of its year.

.. function:: lastdayofyear(dt::TimeType) -> TimeType

    Adjusts ``dt`` to the last day of its year.

.. function:: firstdayofquarter(dt::TimeType) -> TimeType

    Adjusts ``dt`` to the first day of its quarter.

.. function:: lastdayofquarter(dt::TimeType) -> TimeType

    Adjusts ``dt`` to the last day of its quarter.

.. function:: tonext(dt::TimeType,dow::Int;same::Bool=false) -> TimeType

    Adjusts ``dt`` to the next day of week corresponding to ``dow`` with 
    ``1 = Monday, 2 = Tuesday, etc``. Setting ``same=true`` allows the current
    ``dt`` to be considered as the next ``dow``, allowing for no adjustment to occur.

.. function:: toprev(dt::TimeType,dow::Int;same::Bool=false) -> TimeType

    Adjusts ``dt`` to the previous day of week corresponding to ``dow`` with 
    ``1 = Monday, 2 = Tuesday, etc``. Setting ``same=true`` allows the current
    ``dt`` to be considered as the previous ``dow``, allowing for no adjustment to occur.

.. function:: tofirst(dt::TimeType,dow::Int;of=Month) -> TimeType

    Adjusts ``dt`` to the first ``dow`` of its month. Alternatively, ``of=Year``
    will adjust to the first ``dow`` of the year.

.. function:: tolast(dt::TimeType,dow::Int;of=Month) -> TimeType

    Adjusts ``dt`` to the last ``dow`` of its month. Alternatively, ``of=Year``
    will adjust to the last ``dow`` of the year.

.. function:: tonext(func::Function,dt::TimeType;step=Day(1),negate=false,limit=10000,same=false) -> TimeType

    Adjusts ``dt`` by iterating at most ``limit`` iterations by ``step`` increments until
    ``func`` returns true. ``func`` must take a single ``TimeType`` argument and return a ``Bool``.
    ``same`` allows ``dt`` to be considered in satisfying ``func``. ``negate`` will make the adjustment
    process terminate when ``func`` returns false instead of true.

.. function:: toprev(func::Function,dt::TimeType;step=Day(-1),negate=false,limit=10000,same=false) -> TimeType

    Adjusts ``dt`` by iterating at most ``limit`` iterations by ``step`` increments until
    ``func`` returns true. ``func`` must take a single ``TimeType`` argument and return a ``Bool``.
    ``same`` allows ``dt`` to be considered in satisfying ``func``. ``negate`` will make the adjustment
    process terminate when ``func`` returns false instead of true.

.. function:: recur{T<:TimeType}(func::Function,dr::StepRange{T};negate=false,limit=10000) -> Vector{T}

    ``func`` takes a single TimeType argument and returns a ``Bool`` indicating whether the input
    should be "included" in the final set. ``recur`` applies ``func`` over each element in the 
    range of ``dr``, including those elements for which ``func`` returns ``true`` in the resulting
    Array, unless ``negate=true``, then only elements where ``func`` returns ``false`` are included.


Periods
~~~~~~~

.. function:: Year(v)
              Month(v)
              Week(v)
              Day(v)
              Hour(v)
              Minute(v)
              Second(v)
              Millisecond(v)

   Construct a ``Period`` type with the given ``v`` value.
   Input must be losslessly convertible to an ``Int64``.

.. function:: default(p::Period) => Period

    Returns a sensible "default" value for the input Period by returning
    ``one(p)`` for Year, Month, and Day, and ``zero(p)`` for Hour, Minute,
    Second, and Millisecond.

Conversion Functions
~~~~~~~~~~~~~~~~~~~~

.. function:: today() -> Date

    Returns the date portion of ``now()``.

.. function:: unix2datetime(x) -> DateTime

   Takes the number of seconds since unix epoch ``1970-01-01T00:00:00``
   and converts to the corresponding DateTime.

.. function:: datetime2unix(dt::DateTime) -> Float64

   Takes the given DateTime and returns the number of seconds since
   the unix epoch as a ``Float64``.

.. function:: julian2datetime(julian_days) -> DateTime

   Takes the number of Julian calendar days since epoch
   ``-4713-11-24T12:00:00`` and returns the corresponding DateTime.

.. function:: datetime2julian(dt::DateTime) -> Float64

   Takes the given DateTime and returns the number of Julian calendar days
   since the julian epoch as a ``Float64``.

.. function:: rata2datetime(days) -> DateTime

   Takes the number of Rata Die days since epoch ``0000-12-31T00:00:00``
   and returns the corresponding DateTime.

.. function:: datetime2rata(dt::TimeType) -> Int64

   Returns the number of Rata Die days since epoch from the
   given Date or DateTime.


Constants
~~~~~~~~~

Days of the Week:

=============== ========= =============
Variable        Abbr.     Value (Int64)
--------------- --------- -------------
``Monday``      ``Mon``   1
``Tuesday``     ``Tue``   2
``Wednesday``   ``Wed``   3
``Thursday``    ``Thu``   4
``Friday``      ``Fri``   5
``Saturday``    ``Sat``   6
``Sunday``      ``Sun``   7
=============== ========= =============

Months of the Year:

=============== ========= =============
Variable        Abbr.     Value (Int64)
--------------- --------- -------------
``January``     ``Jan``   1
``February``    ``Feb``   2
``March``       ``Mar``   3
``April``       ``Apr``   4
``May``         ``May``   5
``June``        ``Jun``   6
``July``        ``Jul``   7
``August``      ``Aug``   8
``September``   ``Sep``   9
``October``     ``Oct``   10
``November``    ``Nov``   11
``December``    ``Dec``   12
=============== ========= =============

