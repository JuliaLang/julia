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
``Date`` and ``DateTime`` types can be constructed by parts, or by parsing::

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

TODO: Document parsing

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
