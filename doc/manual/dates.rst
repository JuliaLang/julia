.. _man-dates:

.. currentmodule:: Dates

*************************************
 Date and DateTime
*************************************

The :mod:`Dates` module provides two types for working with dates: :class:`Date` and :class:`DateTime`, representing day and millisecond precision, respectively; both are subtypes of the abstract :class:`TimeType`. The motivation for distinct types is simple: some operations are much simpler, both in terms of code and mental reasoning, when the complexities of greater precision don't have to be dealt with. For example, since the :class:`Date` type only resolves to the precision of a single date (i.e. no hours, minutes, or seconds), normal considerations for time zones, daylight savings/summer time, and leap seconds are unnecessary and avoided.

Both :class:`Date` and :class:`DateTime` are basically immutable ``Int64`` wrappers. The single ``instant`` field of either type is actually a ``UTInstant{P}`` type, which represents a continuously increasing machine timeline based on the UT second [1]_. The :class:`DateTime` type is *timezone-unaware* (in Python parlance) or is analogous to a *LocalDateTime* in Java 8. Additional time zone functionality can be added through the `Timezones.jl package <https://github.com/quinnj/Timezones.jl/>`_, which compiles the `Olsen Time Zone Database <http://www.iana.org/time-zones>`_. Both :class:`Date` and :class:`DateTime` are based on the ISO 8601 standard, which follows the proleptic Gregorian calendar. One note is that the ISO 8601 standard is particular about BC/BCE dates. In general, the last day of the BC/BCE era, 1-12-31 BC/BCE, was followed by 1-1-1 AD/CE, thus no year zero exists. The ISO standard, however, states that 1 BC/BCE is year zero, so ``0000-12-31`` is the day before ``0001-01-01``, and year ``-0001`` (yes, negative one for the year) is 2 BC/BCE, year ``-0002`` is 3 BC/BCE, etc.

.. [1] The notion of the UT second is actually quite fundamental. There are basically two different notions of time generally accepted, one based on the physical rotation of the earth (one full rotation = 1 day), the other based on the SI second (a fixed, constant value). These are radically different! Think about it, a "UT second", as defined relative to the rotation of the earth, may have a different absolute length depending on the day! Anyway, the fact that :class:`Date` and :class:`DateTime` are based on UT seconds is a simplifying, yet honest assumption so that things like leap seconds and all their complexity can be avoided. This basis of time is formally called `UT <https://en.wikipedia.org/wiki/Universal_Time>`_ or UT1. Basing types on the UT second basically means that every minute has 60 seconds and every day has 24 hours and leads to more natural calculations when working with calendar dates.

Constructors
------------
:class:`Date` and :class:`DateTime` types can be constructed by integer or :class:`Period` types, by parsing, or through adjusters (more on those later)::

  julia> DateTime(2013)
  2013-01-01T00:00:00

  julia> DateTime(2013,7)
  2013-07-01T00:00:00

  julia> DateTime(2013,7,1)
  2013-07-01T00:00:00

  julia> DateTime(2013,7,1,12)
  2013-07-01T12:00:00

  julia> DateTime(2013,7,1,12,30)
  2013-07-01T12:30:00

  julia> DateTime(2013,7,1,12,30,59)
  2013-07-01T12:30:59

  julia> DateTime(2013,7,1,12,30,59,1)
  2013-07-01T12:30:59.001

  julia> Date(2013)
  2013-01-01

  julia> Date(2013,7)
  2013-07-01

  julia> Date(2013,7,1)
  2013-07-01

  julia> Date(Dates.Year(2013),Dates.Month(7),Dates.Day(1))
  2013-07-01

  julia> Date(Dates.Month(7),Dates.Year(2013))
  2013-07-01

:class:`Date` or :class:`DateTime` parsing is accomplished by the use of format strings. Format strings work by the notion of defining *delimited* or *fixed-width* "slots" that contain a period to parse and passing the text to parse and format string to a :class:`Date` or :class:`DateTime` constructor, of the form ``Date("2015-01-01","y-m-d")`` or ``DateTime("20150101","yyyymmdd")``.

Delimited slots are marked by specifying the delimiter the parser should expect between two subsequent periods; so ``"y-m-d"`` lets the parser know that between the first and second slots in a date string like ``"2014-07-16"``, it should find the ``-`` character. The ``y``, ``m``, and ``d`` characters let the parser know which periods to parse in each slot.

Fixed-width slots are specified by repeating the period character the number of times corresponding to the width with no delimiter between characters. So ``"yyyymmdd"`` would correspond to a date string like ``"20140716"``. The parser distinguishes a fixed-width slot by the absence of a delimiter, noting the transition ``"yyyymm"`` from one period character to the next.

Support for text-form month parsing is also supported through the ``u`` and ``U`` characters, for abbreviated and full-length month names, respectively. By default, only English month names are supported, so ``u`` corresponds to "Jan", "Feb", "Mar", etc. And ``U`` corresponds to "January", "February", "March", etc. Similar to other name=>value mapping functions :func:`dayname` and :func:`monthname`, custom locales can be loaded by passing in the ``locale=>Dict{String,Int}`` mapping to the :const:`MONTHTOVALUEABBR` and :const:`MONTHTOVALUE` dicts for abbreviated and full-name month names, respectively.

One note on parsing performance: using the ``Date(date_string,format_string)`` function is fine if only called a few times. If there are many similarly formatted date strings to parse however, it is much more efficient to first create a :class:`Dates.DateFormat`, and pass it instead of a raw format string.

::

  julia> df = Dates.DateFormat("y-m-d");

  julia> dt = Date("2015-01-01",df)
  2015-01-01

  julia> dt2 = Date("2015-01-02",df)
  2015-01-02


A full suite of parsing and formatting tests and examples is available in `tests/dates/io.jl <https://github.com/JuliaLang/julia/blob/master/test/dates/io.jl>`_.

Durations/Comparisons
---------------------

Finding the length of time between two :class:`Date` or :class:`DateTime` is straightforward given their underlying representation as ``UTInstant{Day}`` and ``UTInstant{Millisecond}``, respectively. The difference between :class:`Date` is returned in the number of :class:`Day`, and :class:`DateTime` in the number of :class:`Millisecond`. Similarly, comparing :class:`TimeType` is a simple matter of comparing the underlying machine instants (which in turn compares the internal ``Int64`` values).

::

  julia> dt = Date(2012,2,29)
  2012-02-29

  julia> dt2 = Date(2000,2,1)
  2000-02-01

  julia> dump(dt)
  Date
    instant: UTInstant{Day}
      periods: Day
        value: Int64 734562

  julia> dump(dt2)
  Date
  instant: UTInstant{Day}
    periods: Day
      value: Int64 730151

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

  julia> dt2 - dt
  -4411 days

  julia> dt = DateTime(2012,2,29)
  2012-02-29T00:00:00

  julia> dt2 = DateTime(2000,2,1)
  2000-02-01T00:00:00

  julia> dt - dt2
  381110402000 milliseconds


Accessor Functions
------------------

Because the :class:`Date` and :class:`DateTime` types are stored as single ``Int64`` values, date parts or fields can be retrieved through accessor functions. The lowercase accessors return the field as an integer::

  julia> t = Date(2014,1,31)
  2014-01-31

  julia> Dates.year(t)
  2014

  julia> Dates.month(t)
  1

  julia> Dates.week(t)
  5

  julia> Dates.day(t)
  31

While propercase return the same value in the corresponding :class:`Period` type::

  julia> Dates.Year(t)
  2014 years

  julia> Dates.Day(t)
  31 days

Compound methods are provided, as they provide a measure of efficiency if multiple fields are needed at the same time::

  julia> Dates.yearmonth(t)
  (2014,1)

  julia> Dates.monthday(t)
  (1,31)

  julia> Dates.yearmonthday(t)
  (2014,1,31)

One may also access the underlying ``UTInstant`` or integer value::

  julia> dump(t)
  Date
  instant: UTInstant{Day}
    periods: Day
      value: Int64 735264

  julia> t.instant
  UTInstant{Day}(735264 days)

  julia> Dates.value(t)
  735264

Query Functions
---------------

Query functions provide calendrical information about a :class:`TimeType`. They include information about the day of the week::

  julia> t = Date(2014,1,31)
  2014-01-31

  julia> Dates.dayofweek(t)
  5

  julia> Dates.dayname(t)
  "Friday"

  julia> Dates.dayofweekofmonth(t)
  5  # 5th Friday of January

Month of the year::

  julia> Dates.monthname(t)
  "January"

  julia> Dates.daysinmonth(t)
  31

As well as information about the :class:`TimeType`'s year and quarter::

  julia> Dates.isleapyear(t)
  false

  julia> Dates.dayofyear(t)
  31

  julia> Dates.quarterofyear(t)
  1

  julia> Dates.dayofquarter(t)
  31

The :func:`dayname` and :func:`monthname` methods can also take an optional ``locale`` keyword that can be used to return the name of the day or month of the year for other languages/locales::

  julia> const french_daysofweek = Dict(1=>"Lundi",2=>"Mardi",3=>"Mercredi",4=>"Jeudi",5=>"Vendredi",6=>"Samedi",7=>"Dimanche");

  # Load the mapping into the Dates module under locale name "french"
  julia> Dates.VALUETODAYOFWEEK["french"] = french_daysofweek;

  julia> Dates.dayname(t;locale="french")
  "Vendredi"

Similarly for the :func:`monthname` function, a mapping of ``locale=>Dict{Int,String}`` should be loaded in :const:`VALUETOMONTH`.

TimeType-Period Arithmetic
--------------------------

It's good practice when using any language/date framework to be familiar with how date-period arithmetic is handled as there are some `tricky issues <http://codeblog.jonskeet.uk/2010/12/01/the-joys-of-date-time-arithmetic>`_ to deal with (though much less so for day-precision types).

The :mod:`Dates` module approach tries to follow the simple principle of trying to change as little as possible when doing :class:`Period` arithmetic. This approach is also often known as *calendrical* arithmetic or what you would probably guess if someone were to ask you the same calculation in a conversation. Why all the fuss about this? Let's take a classic example: add 1 month to January 31st, 2014. What's the answer? Javascript will say `March 3 <http://www.markhneedham.com/blog/2009/01/07/javascript-add-a-month-to-a-date/>`_ (assumes 31 days). PHP says `March 2 <http://stackoverflow.com/questions/5760262/php-adding-months-to-a-date-while-not-exceeding-the-last-day-of-the-month>`_ (assumes 30 days). The fact is, there is no right answer. In the :mod:`Dates` module, it gives the result of February 28th. How does it figure that out? I like to think of the classic 7-7-7 gambling game in casinos.

Now just imagine that instead of 7-7-7, the slots are Year-Month-Day, or in our example, 2014-01-31. When you ask to add 1 month to this date, the month slot is incremented, so now we have 2014-02-31. Then the day number is checked if it is greater than the last valid day of the new month; if it is (as in the case above), the day number is adjusted down to the last valid day (28). What are the ramifications with this approach? Go ahead and add another month to our date, ``2014-02-28 + Month(1) == 2014-03-28``. What? Were you expecting the last day of March? Nope, sorry, remember the 7-7-7 slots. As few slots as possible are going to change, so we first increment the month slot by 1, 2014-03-28, and boom, we're done because that's a valid date. On the other hand, if we were to add 2 months to our original date, 2014-01-31, then we end up with 2014-03-31, as expected. The other ramification of this approach is a loss in associativity when a specific ordering is forced (i.e. adding things in different orders results in different outcomes). For example::

  julia> (Date(2014,1,29)+Dates.Day(1)) + Dates.Month(1)
  2014-02-28

  julia> (Date(2014,1,29)+Dates.Month(1)) + Dates.Day(1)
  2014-03-01

What's going on there? In the first line, we're adding 1 day to January 29th, which results in 2014-01-30; then we add 1 month, so we get 2014-02-30, which then adjusts down to 2014-02-28. In the second example, we add 1 month *first*, where we get 2014-02-29, which adjusts down to 2014-02-28, and *then* add 1 day, which results in 2014-03-01. One design principle that helps in this case is that, in the presence of multiple Periods, the operations will be ordered by the Periods' *types*, not their value or positional order; this means ``Year`` will always be added first, then ``Month``, then ``Week``, etc. Hence the following *does* result in associativity and Just Works::

  julia> Date(2014,1,29) + Dates.Day(1) + Dates.Month(1)
  2014-03-01

  julia> Date(2014,1,29) + Dates.Month(1) + Dates.Day(1)
  2014-03-01

Tricky? Perhaps. What is an innocent :mod:`Dates` user to do? The bottom line is to be aware that explicitly forcing a certain associativity, when dealing with months, may lead to some unexpected results, but otherwise, everything should work as expected. Thankfully, that's pretty much the extent of the odd cases in date-period arithmetic when dealing with time in UT (avoiding the "joys" of dealing with daylight savings, leap seconds, etc.).


Adjuster Functions
------------------

As convenient as date-period arithmetics are, often the kinds of calculations needed on dates take on a *calendrical* or *temporal* nature rather than a fixed number of periods. Holidays are a perfect example; most follow rules such as "Memorial Day = Last Monday of May", or "Thanksgiving = 4th Thursday of November". These kinds of temporal expressions deal with rules relative to the calendar, like first or last of the month, next Tuesday, or the first and third Wednesdays, etc.

The :mod:`Dates` module provides the *adjuster* API through several convenient methods that aid in simply and succinctly expressing temporal rules. The first group of adjuster methods deal with the first and last of weeks, months, quarters, and years. They each take a single :class:`TimeType` as input and return or *adjust to* the first or last of the desired period relative to the input.

::

  # Adjusts the input to the Monday of the input's week
  julia> Dates.firstdayofweek(Date(2014,7,16))
  2014-07-14

  # Adjusts to the last day of the input's month
  julia> Dates.lastdayofmonth(Date(2014,7,16))
  2014-07-31

  # Adjusts to the last day of the input's quarter
  julia> Dates.lastdayofquarter(Date(2014,7,16))
  2014-09-30

The next two higher-order methods, :func:`tonext`, and :func:`toprev`, generalize working with temporal expressions by taking a :class:`DateFunction` as first argument, along with a starting :class:`TimeType`. A :class:`DateFunction` is just a function, usually anonymous, that takes a single :class:`TimeType` as input and returns a ``Bool``, ``true`` indicating a satisfied adjustment criterion.
For example::

  julia> istuesday = x->Dates.dayofweek(x) == Dates.Tuesday  # Returns true if the day of the week of x is Tuesday
  (anonymous function)

  julia> Dates.tonext(istuesday, Date(2014,7,13)) # 2014-07-13 is a Sunday
  2014-07-15

  # Convenience method provided for day of the week adjustments
  julia> Dates.tonext(Date(2014,7,13), Dates.Tuesday)
  2014-07-15

This is useful with the do-block syntax for more complex temporal expressions::

  julia> Dates.tonext(Date(2014,7,13)) do x
            # Return true on the 4th Thursday of November (Thanksgiving)
            Dates.dayofweek(x) == Dates.Thursday &&
            Dates.dayofweekofmonth(x) == 4 &&
            Dates.month(x) == Dates.November
        end
  2014-11-27

The final method in the adjuster API is the :func:`recur` function. :func:`recur` vectorizes the adjustment process by taking a start and stop date (optionally specificed by a :class:`StepRange`), along with a :class:`DateFunction` to specify all valid dates/moments to be returned in the specified range. In this case, the :class:`DateFunction` is often referred to as the "inclusion" function because it specifies (by returning ``true``) which dates/moments should be included in the returned vector of dates.

::

  # Pittsburgh street cleaning; Every 2nd Tuesday from April to November
  # Date range from January 1st, 2014 to January 1st, 2015
  julia> dr = Dates.Date(2014):Dates.Date(2015);
  julia> recur(dr) do x
             Dates.dayofweek(x) == Dates.Tue &&
             Dates.April <= Dates.month(x) <= Dates.Nov &&
             Dates.dayofweekofmonth(x) == 2
         end
   8-element Array{Date,1}:
    2014-04-08
    2014-05-13
    2014-06-10
    2014-07-08
    2014-08-12
    2014-09-09
    2014-10-14
    2014-11-11

Additional examples and tests are available in `test/dates/adjusters.jl <https://github.com/JuliaLang/julia/blob/master/test/dates/adjusters.jl>`_.


Period Types
------------

Periods are a human view of discrete, sometimes irregular durations of time. Consider 1 month; it could represent, in days, a value of 28, 29, 30, or 31 depending on the year and month context. Or a year could represent 365 or 366 days in the case of a leap year. :class:`Period` types are simple ``Int64`` wrappers and are constructed by wrapping any ``Int64`` convertible type, i.e. ``Year(1)`` or ``Month(3.0)``. Arithmetic between :class:`Period` of the same type behave like integers, and limited ``Period-Real`` arithmetic is available.
::

  julia> y1 = Dates.Year(1)
  1 year

  julia> y2 = Dates.Year(2)
  2 years

  julia> y3 = Dates.Year(10)
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

  julia> div(y3,3) # mirrors integer division
  3 years


See the `API reference <http://docs.julialang.org/en/latest/stdlib/dates/>`_ for additional information on methods exported from the :mod:`Dates` module.
