.. _man-handling-operating-system-variation:

*************************************
 Handling Operating System Variation
*************************************

When dealing with platform libraries, it is often necessary to provide special cases
for various platforms. The variable ``OS_NAME`` can be used to write these special
cases. There are several macros intended to make this easier: ``@windows_only``,
``@unix_only``, ``@linux_only``, and ``@osx_only``. These may be used as follows::

    @windows_only begin
        some_complicated_thing(a)
    end

Note that ``@linux_only`` and ``@osx_only`` are mutually exclusive subsets of ``@unix_only``\ . (This
similarly applies to ``@unix``\ .)
Additionally, there are:``@windows``, ``@unix``, ``@linux``, and ``@osx``. Their usage takes
the form of a ternary conditional operator, as demonstrated in the following examples.

Simple blocks::

    ccall( (@windows? :_fopen : :fopen), ...)

Complex blocks::

    @linux? (
             begin
                 some_complicated_thing(a)
             end
           : begin
                 some_different_thing(a)
             end
           )

Chaining (parentheses optional, but recommended for readability)::

    @windows? :a : (@osx? :b : :c)
