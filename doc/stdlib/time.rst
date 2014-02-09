.. _stdlib-datetime:

Date and DateTime Functionality
===============================

.. module:: Base.Time

With roots in `Joda <http://joda-time.sourceforge.net/>`_/`JSR-310 <http://threeten.github.io/>`_, `numpy datetime64 <http://docs.scipy.org/doc/numpy-dev/reference/arrays.datetime.html>`_, `lubridate <http://cran.r-project.org/web/packages/lubridate/index.html>`_, `zoo <http://cran.r-project.org/web/packages/zoo/index.html>`_, and `runt <https://github.com/mlipper/runt>`_, the ``Time`` module aims to `greedily <http://julialang.org/blog/2012/02/why-we-created-julia/>`_ provide a ``DateTime`` implementation top in efficiency and performance. Date implementations are notoriously tricky due to the effects of using different calendars (Julian, Gregorian, Islamic, etc.), time zones, and from a programming perspective, precision. Should the type be in terms of seconds? Milliseconds? Nanoseconds? Luckily, Julia's unique use of type parameters and multiple dispatch allow for an extremely efficient and simple implementation while allowing all the flexibility and extensibility required by dates and times.


Period Types
------------

Periods are a human view of discrete, sometimes irregular durations of time. Consider 1 month; it could represent, in days, a value of 28, 29, 30, or 31 depending on the year and month context. Or 1 year could represent 365 or 366 days in the case of a leap year. These points are relevant when ``Date/DateTime-Period`` arithmetic is considered (which is discussed below). Despite their irregular nature, ``Period`` types in Julia are useful when working with ``TimeType``\s. ``Period`` types are simple wrappers of a ``Int64`` type and constructed by wrapping any ``Integer`` type, i.e. ``Year(1)`` or ``Month(3)``. Arithmetic between ``Period``\s of the same type behave like ``Integer``\s, and ``Period-Real`` arithmetic promotes the ``Real`` to the ``Period`` type::

  julia> y1 = Time.Year(1)
  1 year

  julia> y2 = Time.Year(2)
  2 years

  julia> y3 = Time.Year(10)
  10 years

  julia> y1 + y2
  3 years

  julia> div(y3,y2)
  5 years

  julia> y3 - y2
  8 years

  julia> y3 * y2
  20 years

  julia> y3 % y2
  0 years

  julia> y1 + 20
  21 years

  julia> div(y3,3) # truncates
  3 years
 

TimeTypes
---------

The ``Time`` module provides two ``TimeType``\s: ``Date`` and ``DateTime``, representing different levels of precision/resolution. The ``Date`` type has a day precision while the ``DateTime`` type provides a millisecond precision. The motivation for distinct types is simple, some operations are much simpler, both in terms of code and mental reasoning, when the complexities of greater precision don't have to be dealt with. For example, since the ``Date`` type has a "day-precision" (i.e. no hours, minutes, or seconds), it means that normal consideration for time zones, daylight savings/summer time, and leap seconds is unneccesary. Users should take care to think about the level precision they will actually require and choose a ``TimeType`` appropriately, remembering that the simpler the date type, the faster, more efficient, and less complex the code will be.

Both ``Date`` and ``DateTime`` are immutable, and wrap ``Int64`` "instants", which represent a continuously increasing machine timeline. ``Date``\s are represented by "yyyy-mm-dd", while ``DateTime``\s go by the ISO standard "yyyy-mm-ddTHH:MM:SS.MS UTC". As mentioned, the ``DateTime`` type takes a ``TimeZone`` parameter, and ``UTC`` is the only default time zone provided in Base. Additional time zone functionality can be added through the ``TimeZone.jl`` package, which compiles the `Olsen TimeZone Database <http://www.iana.org/time-zones>`_. While the ``Date`` type could technically be represented as a ``DateTime{Day,0,ISOCalendar}`` constructor, it is provided as a separate type to aid in mentally separating the concerns between day and sub-second precision levels. Consequently, the ``Date`` type takes no parameters, assuming ``UTC`` and ``ISOCalendar``.

Another important feature of the ``DateTime`` type is that it includes full support for leap seconds. What are leap seconds? Go `read up <https://en.wikipedia.org/wiki/Leap_second>`_, because every programmer should be aware, just ask `LinkedIn <http://mashable.com/2012/07/01/leap-second-bug-outages/>`_, `Reddit <http://www.theverge.com/2012/7/1/3130079/leap-second-bug-reddit-mozilla-gawker>`_, or `Amazon <http://www.tweaktown.com/news/24776/amazon_ec2_outage_happened_because_of_the_leap_second_bug_affected_netflix_instagram_and_more/index.html>`_. In short, sometimes on June 30 or December 31, an extra second is added at midnight UTC to make up for the fact that the earth doesn't always spin at same, *predictable* rate (due to volcanos, earthquakes, tectonics, etc.). Obviously this causes problems for companies/databases who aren't prepared for an extra second. Imagine taking timestamps and a second repeats itself, or inserts get out of order, or even worse, your `kernel panics <https://lkml.org/lkml/2012/7/1/203>`_ and everything shuts down. While many datetime implementations choose to ignore leap seconds and allow the OS or `npt <http://www.ntp.org/>`_ to figure it out, the Time module realizes they're a fact of life and includes them at very little cost to performance. Observe::

  #just a happy server churning out timestamps
  julia> dt = DateTime(2012,6,30,23,59,50,0)
  2012-06-30T23:59:50 UTC
  
  julia> for i = 1:20
    println(dt)
    dt += Time.Second(1)
  end
  2012-06-30T23:59:50 UTC
  2012-06-30T23:59:51 UTC
  2012-06-30T23:59:52 UTC
  2012-06-30T23:59:53 UTC
  2012-06-30T23:59:54 UTC
  2012-06-30T23:59:55 UTC
  2012-06-30T23:59:56 UTC
  2012-06-30T23:59:57 UTC
  2012-06-30T23:59:58 UTC
  2012-06-30T23:59:59 UTC
  2012-06-30T23:59:60 UTC # Here's our scary leap second
  2012-07-01T00:00:00 UTC
  2012-07-01T00:00:01 UTC
  2012-07-01T00:00:02 UTC
  2012-07-01T00:00:03 UTC
  2012-07-01T00:00:04 UTC
  2012-07-01T00:00:05 UTC
  2012-07-01T00:00:06 UTC
  2012-07-01T00:00:07 UTC
  2012-07-01T00:00:08 UTC

And it's as simple as that. No crashes, no hiccups, and no need for `second-smearing <http://googleblog.blogspot.com/2011/09/time-technology-and-leaping-seconds.html>`_. We'll talk a little more about leap seconds with ``DateTime-Period`` arithmetic.

The default ``ISOCalendar`` type ISO 8601 standards. This means dates follow the proleptic Gregorian calendar, which means that the normal calendar we think of toady, which was implemented as it currently stands in 1582, is applied retroactively back through time. So even though the folks in 1582 fast-forwarded 10 days and didn't experience October 5-14, these are valid dates for the ``ISOCalendar`` (non-ISO implementations sometimes switch to the Julian calendar before Oct 14, 1582).

The ISO 8601 standard is also particular about BC/BCE dates. In common text, the date 1-12-31 BC/BCE was followed by 1-1-1 AD/CE, thus no year 0 exists. The ISO standard however states that 1 BC/BCE is year 0, so ``0000-12-31`` is the day before ``0001-01-01``, and year ``-0001`` (yes, negative 1 for the year) is 2 BC/BCE, year ``-0002`` is 3 BC/BCE, etc.

Constructors
^^^^^^^^^^^^
``Date`` and ``DateTime`` types can be constructed by parts with integers or Period types, or by parsing::

  julia> DateTime(2013)
  2013-01-01T00:00:00 UTC

  julia> DateTime(2013,7)
  2013-07-01T00:00:00 UTC

  julia> DateTime(2013,7,1)
  2013-07-01T00:00:00 UTC

  julia> DateTime(2013,7,1,12)
  2013-07-01T12:00:00 UTC

  julia> DateTime(2013,7,1,12,0)
  2013-07-01T12:00:00 UTC

  julia> DateTime(2013,7,1,12,0,0)
  2013-07-01T12:00:00 UTC

  julia> Date(2013)
  2013-01-01

  julia> Date(2013,7)
  2013-07-01

  julia> Date(2013,7,1)
  2013-07-01

Date or DateTime parsing is accomplished by the use of ``format`` strings. The easiest way to understand is to see some examples in action. Also refer to the function reference below for additional information::

  dt = Time.DateTime(1996,1,15)
  f = "yy-mm-dd"
  a = "96-01-15"
  Time.DateTime(a,f) + Time.Year(1900) == dt
  a1 = "96-1-15"
  Time.DateTime(a1,f) + Time.Year(1900) == dt
  a2 = "96-1-1"
  Time.DateTime(a2,f) + Time.Year(1900) + Time.Day(14) == dt
  a3 = "1996-1-15"
  Time.DateTime(a3,f) == dt

  f = "yy/mmm/dd"
  b = "96/Feb/15"
  Time.DateTime(b,f) + Time.Year(1900) == dt + Time.Month(1)
  b1 = "1996/Feb/15"
  Time.DateTime(b1,f) == dt + Time.Month(1)
  b2 = "96/Feb/1"
  Time.DateTime(b2,f) + Time.Year(1900) + Time.Day(14) == dt + Time.Month(1)

  f = "yy:dd:mm"
  c = "96:15:01"
  Time.DateTime(c,f) + Time.Year(1900) == dt
  c1 = "1996:15:01"
  Time.DateTime(c1,f) == dt
  c2 = "96:15:1"
  Time.DateTime(c2,f) + Time.Year(1900) == dt
  c3 = "96:1:01"
  Time.DateTime(c3,f) + Time.Year(1900) + Time.Day(14) == dt

  f = "yyyy,mmm,dd"
  d = "1996,Jan,15"
  Time.DateTime(d,f) == dt
  d1 = "96,Jan,15"
  Time.DateTime(d1,f) + Time.Year(1900) == dt
  d2 = "1996,Jan,1"
  Time.DateTime(d2,f) + Time.Day(14) == dt

  f = "yyyy.mmmm.dd"
  e = "1996.January.15"
  Time.DateTime(e,f) == dt
  e1 = "96.January.15"
  Time.DateTime(e1,f) + Time.Year(1900) == dt

  fo = "yyyy m dd"
  f = "1996 1 15"
  Time.DateTime(f,fo) == dt
  f1 = "1996 01 15"
  Time.DateTime(f1,fo) == dt
  f2 = "1996 1 1"
  Time.DateTime(f2,fo) + Time.Day(14) == dt

  j = "1996-01-15 UTC"
  f = "yyyy-mm-dd zzz"
  Time.DateTime(j,f) == dt
  k = "1996-01-15 10:00:00 UTC"
  f = "yyyy-mm-dd HH:MM:SS zzz"
  Time.DateTime(k,f) == dt + Time.Hour(10)
  l = "1996-01-15 10:10:10.25 UTC"
  f = "yyyy-mm-dd HH:MM:SS.ss zzz"
  Time.DateTime(l,f) == dt + Time.Hour(10) + Time.Minute(10) + Time.Second(10) + Time.Millisecond(250)

  r = "1/15/1996" # Excel
  f = "m/dd/yyyy"
  Time.DateTime(r,f) == dt
  s = "19960115"
  f = "yyyymmdd"
  Time.DateTime(s,f) == dt
  v = "1996-01-15 10:00:00"
  f = "yyyy-mm-dd HH:MM:SS"
  Time.DateTime(v,f) == dt + Time.Hour(10)
  w = "1996-01-15T10:00:00 UTC"
  f = "yyyy-mm-ddTHH:MM:SS zzz"
  Time.DateTime(w,f;sep="T") == dt + Time.Hour(10)

  f = "yyyy/m"
  y = "1996/1"
  Time.DateTime(y,f) == dt - Time.Day(14)
  y2 = "96/1"
  Time.DateTime(y2,f) + Time.Year(1900) == dt - Time.Day(14)

  f = "yyyy"
  z = "1996"
  Time.DateTime(z,f) == dt - Time.Day(14)
  z1 = "1996-3"
  Time.DateTime(z1,f) != Time.DateTime(1996,3)
  z2 = "1996-3-1"
  Time.DateTime(z2,f) != Time.DateTime(1996,3)

  aa = "1/5/1996"
  f = "m/d/yyyy"
  Time.DateTime(aa,f) == dt - Time.Day(10)
  bb = "5/1/1996"
  f = "d/m/yyyy"
  Time.DateTime(bb,f) == dt - Time.Day(10)
  cc = "01151996"
  f = "mmddyyyy"
  Time.DateTime(cc,f) == dt
  dd = "15011996"
  f = "ddmmyyyy"
  Time.DateTime(dd,f) == dt
  ee = "01199615"
  f = "mmyyyydd"
  Time.DateTime(ee,f) == dt
  ff = "1996-15-Jan"
  f = "yyyy-dd-mmm"
  Time.DateTime(ff,f) == dt
  gg = "Jan-1996-15"
  f = "mmm-yyyy-dd"
  Time.DateTime(gg,f) == dt

  Date("2009年12月01日","yyyy年mm月dd日") == Date(2009,12,1)
  Date("2009-12-01","yyyy-mm-dd") == Date(2009,12,1)

Durations/Comparisons
^^^^^^^^^^^^^^^^^^^^^

Finding the length of time between two ``Date``\s or ``DateTime``\s is straightforward. The difference between ``Date``\s is returned in the number of ``Day``\s, and ``DateTime``\s in the number of ``Millisecond``\s. Similarly, comparing ``TimeType``\s is a simple matter of comparing the underlying machine instants::
  
  julia> dt = Date(2012,2,29)
  2012-02-29

  julia> dt2 = Date(2000,2,1)
  2000-02-01

  julia> dt > dt2
  true

  julia> dt != dt2
  true

  julia> dt + dt2
  Operation not defined for TimeTypes

  julia> dt * dt2
  Operation not defined for TimeTypes

  julia> dt / dt2
  Operation not defined for TimeTypes

  julia> dt - dt2
  4411 days

  julia> dt = DateTime(2012,2,29)
  2012-02-29T00:00:00 UTC

  julia> dt2 = DateTime(2000,2,1)
  2000-02-01T00:00:00 UTC

  julia> dt - dt2
  381110402000 milliseconds

TimeType-Period Arithmetic
^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``Time`` module's approach to ``Period`` arithmetic tries to be very simple and clear while still giving the user flexibility and options. It's good practice when using any language/date framework to be familiar with how period arithmetic is handled as there is no strong consensus and some `tricky issues <http://msmvps.com/blogs/jon_skeet/archive/2010/12/01/the-joys-of-date-time-arithmetic.aspx>`_ to deal with (though much less so for day-precision types).

The ``Time`` module tries to follow the simple principle of trying to change as little as possible when doing ``Period`` arithmetic. What that means is that arithmetic works pretty much like you expect. Just be aware when dealing with Seconds, leap seconds are included, so ``DateTime(2012,6,30,23,59,59) + Time.Second(1) == DateTime(2012,6,30,23,59,60)`` (this is really only a concern if you're doing a calculation with seconds that span a leap second). 

This type of arithmetic is called *calendrical* arithmetic, which is a slightly vague term for what a person would guess when doing date-period arithmetic. What this means is that the ``Period``\s will be added/subtracted from their respective date-part similar to a 7-7-7 slot machine (in this case, Y-M-D are the slots). Adding/subtracting months from a date will only affect the M slot, leaving the Y and D slots untouched unless absolutely necessary. Only when necessary? Yes, because sometimes when you try to add 1 month to ``2013-01-31``, it's not apparently clear what you're expecting. ``2013-02-28`` because it's the last day of February? ``2013-03-02`` because you wanted to add 30 days? ``Time``'s approach is of only changing when necessary, which means if a month is added/subtracted and the new day of month happens to be "out of range" for the new month, then the new day of month becomes the last day of the new month. So for our example above ``Date(2013,1,31) + month(1) == Date(2013,2,28)``. Got it? It's not too difficult, but just be aware. Here are a few examples::

  julia> dt = Time.Date(2000,1,28)
  2000-01-28

  julia> dt1 = Time.Date(2000,1,29)
  2000-01-29

  julia> dt2 = Time.Date(2000,1,30)
  2000-01-30

  julia> dt3 = Time.Date(2000,1,31)
  2000-01-31

  julia> dt + Time.Month(1)
  2000-02-28

  julia> dt1 + Time.Month(1)
  2000-02-29

  julia> dt2 + Time.Month(1)
  2000-02-29

  julia> dt3 + Time.Month(1)
  2000-02-29

  julia> dt = Time.Date(2000,2,29)
  2000-02-29

  julia> dt + Time.Month(1) == Time.Date(2000,3,29)
  true

  julia> dt - Time.Month(1) == Time.Date(2000,1,29)
  true

  julia> dt = Time.DateTime(1972,6,30,23,59,60)
  1972-06-30T23:59:60 UTC

  julia> dt + Time.Month(1) == Time.DateTime(1972,7,30,23,59,59)
  true

  julia> dt - Time.Month(1) == Time.DateTime(1972,5,30,23,59,59)
  true

  julia> dt = Time.DateTime(1972,6,30,23,59,59)
  1972-06-30T23:59:59 UTC

  julia> dt + Time.Month(1) == Time.DateTime(1972,7,30,23,59,59)
  true

  julia> dt - Time.Month(1) == Time.DateTime(1972,5,30,23,59,59)
  true

Function Reference
------------------

.. currentmodule:: Base

.. function:: DateTime(y, [m, [d,] [h,] [mi,] [s,] [ms]])

   Construct a DateTime type by parts. Arguments must be of type
   ``::Integer``. Returned DateTime corresponds to ISO 8601, UTC.

.. function:: DateTime([y::Year, [m::Month,] [d::Day,] [h::Hour,] [mi::Minute,] [s::Second,] [ms::Millisecond]])

   Constuct a DateTime type by ``Period`` type parts. Arguments must be
   in order of greatest value (Year) to least (Millisecond).

.. function:: DateTime(dt::String, [format::String=ISODateTimeFormat]; sep::String="")

   Construct a DateTime type by parsing a ``dt`` string given a ``format`` string.
   The default format string is ``ISODateTimeFormat`` which is how 
   DateTime types are represented. The ``sep`` keyword specifies the substring that
   separates the date and time parts of a DateTime string. If no ``sep`` is provided,
   a best guess for a non-word character is sought that divides the date and time
   components. The following codes can be used for constructing format strings:

   ===============  ========  ============================
   Code             Matches   Comment
   ---------------  --------  ----------------------------
   ``yy``            96       Returns year of ``0096``
   ``yyyy``          1996     Returns year of ``1996``
   ``m`` or ``mm``   1, 01    Matches 1 or 2-digit months
   ``mmm``           Jan      Matches abbreviated months
   ``mmmm``          January  Matches full month names
   ``d`` or ``dd``   1, 01    Matches 1 or 2-digit days
   ``HH``            00       Matches hours
   ``MM``            00       Matches minutes
   ``SS``            00       Matches seconds
   ``.s``            .500     Matches milliseconds
   ===============  ========  ============================

.. function:: Date(y, [m, [d,])

   Construct a Date type by parts. Arguments must be of type
   ``::Integer``. Returned Date corresponds to ISO 8601.

.. function:: Date([y::Year, [m::Month,] [d::Day])

   Constuct a Date type by ``Period`` type parts. Arguments must be
   in order of greatest value (Year) to least (Day).

.. function:: Date(dt::String, [format::String=ISODateTimeFormat]; sep::String="")

   Construct a Date type by parsing a ``dt`` string given a ``format`` string.
   The default format string is ``ISODateFormat`` which is how 
   Date types are represented. Same codes and rules for DateTime types
   apply for Dates.
.. currentmodule:: Base.Time

Period Constructors
^^^^^^^^^^^^^^^^^^^

.. function:: Year(y::Integer)

   Construct a ``Year`` Period type with the given ``Integer`` value.

.. function:: Month(y::Integer)

   Construct a ``Month`` Period type with the given ``Integer`` value.

.. function:: Week(y::Integer)

   Construct a ``Week`` Period type with the given ``Integer`` value.

.. function:: Day(y::Integer)

   Construct a ``Day`` Period type with the given ``Integer`` value.

.. function:: Hour(y::Integer)

   Construct a ``Hour`` Period type with the given ``Integer`` value.

.. function:: Minute(y::Integer)

   Construct a ``Minute`` Period type with the given ``Integer`` value.

.. function:: Second(y::Integer)

   Construct a ``Second`` Period type with the given ``Integer`` value.

.. function:: Millisecond(y::Integer)

   Construct a ``Millisecond`` Period type with the given ``Integer`` value.      

Accessor Functions
^^^^^^^^^^^^^^^^^^

.. function:: year(dt::TimeType) -> Int64

   Return the year part of a Date or DateTime.

.. function:: month(dt::TimeType) -> Int64

   Return the month part of a Date or DateTime.

.. function:: week(dt::TimeType) -> Int64

   Return the ISO 8601 week number of a Date or DateTime.

.. function:: day(dt::TimeType) -> Int64

   Return the day part of a Date or DateTime.

.. function:: hour(dt::TimeType) -> Int64

   Return the hour part of a DateTime.

.. function:: minute(dt::TimeType) -> Int64

   Return the minute part of a DateTime.

.. function:: second(dt::TimeType) -> Int64

   Return the second part of a DateTime.

.. function:: millisecond(dt::TimeType) -> Int64

   Return the millisecond part of a DateTime.

Date Functions
^^^^^^^^^^^^^^

.. function:: now() -> DateTime

   Returns a DateTime type corresponding to the user's system
   time, converted to UTC.

.. function:: monthname(dt::TimeType) -> String

   Return the full name of the month of the Date or DateTime.

.. function:: monthabbr(dt::TimeType) -> String

   Return the abbreviated month name of the Date or DateTime.

.. function:: dayname(dt::TimeType) -> String

   Return the full day name corresponding to the day of the week
   of the Date or DateTime.

.. function:: dayabbr(dt::TimeType) -> String

   Return the abbreviated name corresponding to the day of the week
   of the Date or DateTime.

.. function:: isleap(dt::TimeType) -> Bool

   Returns if the year of the Date or DateTime is a leap year.

.. function:: lastdayofmonth(dt::TimeType) -> TimeType

   Returns a Date or DateTime corresponding to the last day of the
   month of the Date or DateTime.

.. function:: firstdayofmonth(dt::TimeType) -> TimeType

   Similar to ``lastdayofmonth``, but for the 1st day of the month.

.. function:: dayofweek(dt::TimeType) -> Int

   Returns the day of the week of the Date or DateTime as an ``Int``.
   0 => Sunday, 1 => Monday...6 => Saturday

.. function:: dayofweekofmonth(dt::TimeType) -> Int

   Returns the number of the day of the week of the Date or DateTime
   in the month. For example, if the day of the week for a Date is 1
   (Monday), ``dayofweekofmonth`` will return a value between 1 and 5
   corresponding to the 1st Monday, 2nd Monday...5th Monday of the month.

.. function:: daysofweekinmonth(dt::TimeType) -> 4 or 5

   Returns 4 or 5, corresponding to the total number of days of the week
   for the given Date or DateTime. 

.. function:: firstdayofweek(dt::TimeType) -> TimeType

   Returns a Date or DateTime corresponding to midnight Sunday of the
   week of the given Date or DateTime.

.. function:: lastdayofweek(dt::TimeType) -> TimeType

   Returns a Date or DateTime corresponding to midnight Saturday of the
   week of the given Date or DateTime.

.. function:: dayofyear(dt::TimeType) -> Int

   Returns the day of the year for the Date or DateTime.

.. function:: recur(fun::Function, start::TimeType, stop::TimeType[, step::Period]; inclusion=true) -> Array{TimeType,1}

   ``recur`` takes a boolean function as 1st argument (or used with a ``do`` block), and will
   apply the boolean function for each Date or DateTime from ``start`` to ``stop`` incrementing
   by ``step``. If the boolean function returns ``true``, the evaluated Date or DateTime is 
   "included" in the returned ``Array``. The ``inclusion`` keyword specifies whether the boolean
   function will "include" or "exclude" the TimeType from the set.


Conversion Functions
^^^^^^^^^^^^^^^^^^^^

.. function:: unix2date(x::Float64) -> DateTime

   Takes the number of seconds since unix epoch ``1970-01-01T00:00:00 UTC``
   and converts to the corresponding DateTime.

.. function:: date2unix(dt::DateTime) -> Float64

   Takes the given DateTime and returns the number of seconds since
   the unix epoch as a ``Float64``.

.. function:: julian2date(j) -> DateTime

   Takes the number of Julian calendar days since epoch 
   ``-4713-11-24T12:00:00`` and returns the corresponding DateTime.
.. function:: date2julian(dt::DateTime) -> Float64

   Takes the given DateTime and returns teh number of Julian calendar days
   since the julian epoch as a ``Float64``.

.. function:: ratadays2date(days) -> DateTime

   Takes the number of Rata Die days since epoch ``0000-12-31T00:00:00``
   and returns the corresponding DateTime.

.. function:: date2ratadays(dt::TimeType) -> Int64

   Returns the number of Rata Die days since epoch from the
   given Date or DateTime.


Constants
^^^^^^^^^

Days of the Week:

  ``Sunday``     = ``Sun`` = 0

  ``Monday``     = ``Mon`` = 1

  ``Tuesday``    = ``Tue`` = 2

  ``Wednesday``  = ``Wed`` = 3

  ``Thursday``   = ``Thu`` = 4

  ``Friday``     = ``Fri`` = 5

  ``Saturday``   = ``Sat`` = 6

Months of the Year:

  ``January``    = ``Jan`` = 1

  ``February``   = ``Feb`` = 2

  ``March``      = ``Mar`` = 3

  ``April``      = ``Apr`` = 4

  ``May``        = ``May`` = 5

  ``June``       = ``Jun`` = 6

  ``July``       = ``Jul`` = 7

  ``August``     = ``Aug`` = 8

  ``September``  = ``Sep`` = 9

  ``October``    = ``Oct`` = 10

  ``November``   = ``Nov`` = 11

  ``December``   = ``Dec`` = 12