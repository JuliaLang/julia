.. _man-running-external-programs:

.. currentmodule:: Base

***************************
 Running External Programs
***************************

Julia borrows backtick notation for commands from the shell, Perl, and
Ruby. However, in Julia, writing

.. doctest::

    julia> `echo hello`
    `echo hello`

differs in several aspects from the behavior in various shells, Perl,
or Ruby:

-  Instead of immediately running the command, backticks create a
   :obj:`Cmd` object to represent the command. You can use this object to
   connect the command to others via pipes, run it, and read or write to
   it.
-  When the command is run, Julia does not capture its output unless you
   specifically arrange for it to. Instead, the output of the command by
   default goes to :const:`STDOUT` as it would using ``libc``'s ``system``
   call.
-  The command is never run with a shell. Instead, Julia parses the
   command syntax directly, appropriately interpolating variables and
   splitting on words as the shell would, respecting shell quoting
   syntax. The command is run as ``julia``'s immediate child process,
   using ``fork`` and ``exec`` calls.

Here's a simple example of running an external program:

.. doctest::

    julia> run(`echo hello`)
    hello

The ``hello`` is the output of the ``echo`` command, sent to :const:`STDOUT`.
The run method itself returns ``nothing``, and throws an :exc:`ErrorException`
if the external command fails to run successfully.

If you want to read the output of the external command, :func:`readstring`
can be used instead:

.. doctest::

    julia> a=readstring(`echo hello`)
    "hello\n"

    julia> (chomp(a)) == "hello"
    true

More generally, you can use :func:`open` to read from or write to an external
command.

.. doctest::

    julia> open(`less`, "w", STDOUT) do io
               for i = 1:3
                   println(io, i)
               end
           end
    1
    2
    3

.. _man-command-interpolation:

Interpolation
-------------

Suppose you want to do something a bit more complicated and use the name
of a file in the variable ``file`` as an argument to a command. You can
use ``$`` for interpolation much as you would in a string literal (see
:ref:`man-strings`):

.. doctest::

    julia> file = "/etc/passwd"
    "/etc/passwd"

    julia> `sort $file`
    `sort /etc/passwd`

A common pitfall when running external programs via a shell is that if a
file name contains characters that are special to the shell, they may
cause undesirable behavior. Suppose, for example, rather than
``/etc/passwd``, we wanted to sort the contents of the file
``/Volumes/External HD/data.csv``. Let's try it:

.. doctest::

    julia> file = "/Volumes/External HD/data.csv"
    "/Volumes/External HD/data.csv"

    julia> `sort $file`
    `sort '/Volumes/External HD/data.csv'`

How did the file name get quoted? Julia knows that ``file`` is meant to
be interpolated as a single argument, so it quotes the word for you.
Actually, that is not quite accurate: the value of ``file`` is never
interpreted by a shell, so there's no need for actual quoting; the
quotes are inserted only for presentation to the user. This will even
work if you interpolate a value as part of a shell word:

.. doctest::

    julia> path = "/Volumes/External HD"
    "/Volumes/External HD"

    julia> name = "data"
    "data"

    julia> ext = "csv"
    "csv"

    julia> `sort $path/$name.$ext`
    `sort '/Volumes/External HD/data.csv'`

As you can see, the space in the ``path`` variable is appropriately
escaped. But what if you *want* to interpolate multiple words? In that
case, just use an array (or any other iterable container):

.. doctest::

    julia> files = ["/etc/passwd","/Volumes/External HD/data.csv"]
    2-element Array{String,1}:
     "/etc/passwd"
     "/Volumes/External HD/data.csv"


    julia> `grep foo $files`
    `grep foo /etc/passwd '/Volumes/External HD/data.csv'`

If you interpolate an array as part of a shell word, Julia emulates the
shell's ``{a,b,c}`` argument generation:

.. doctest::

    julia> names = ["foo","bar","baz"]
    3-element Array{String,1}:
     "foo"
     "bar"
     "baz"

    julia> `grep xylophone $names.txt`
    `grep xylophone foo.txt bar.txt baz.txt`

Moreover, if you interpolate multiple arrays into the same word, the
shell's Cartesian product generation behavior is emulated:

.. doctest::

    julia> names = ["foo","bar","baz"]
    3-element Array{String,1}:
     "foo"
     "bar"
     "baz"

    julia> exts = ["aux","log"]
    2-element Array{String,1}:
     "aux"
     "log"

    julia> `rm -f $names.$exts`
    `rm -f foo.aux foo.log bar.aux bar.log baz.aux baz.log`

Since you can interpolate literal arrays, you can use this generative
functionality without needing to create temporary array objects first:

.. doctest::

    julia> `rm -rf $["foo","bar","baz","qux"].$["aux","log","pdf"]`
    `rm -rf foo.aux foo.log foo.pdf bar.aux bar.log bar.pdf baz.aux baz.log baz.pdf qux.aux qux.log qux.pdf`

Quoting
-------

.. highlight:: sh

Inevitably, one wants to write commands that aren't quite so simple, and
it becomes necessary to use quotes. Here's a simple example of a Perl
one-liner at a shell prompt::

    sh$ perl -le '$|=1; for (0..3) { print }'
    0
    1
    2
    3

The Perl expression needs to be in single quotes for two reasons: so
that spaces don't break the expression into multiple shell words, and so
that uses of Perl variables like ``$|`` (yes, that's the name of a
variable in Perl), don't cause interpolation. In other instances, you
may want to use double quotes so that interpolation *does* occur::

    sh$ first="A"
    sh$ second="B"
    sh$ perl -le '$|=1; print for @ARGV' "1: $first" "2: $second"
    1: A
    2: B

.. highlight:: julia

In general, the Julia backtick syntax is carefully designed so that you
can just cut-and-paste shell commands as is into backticks and they will
work: the escaping, quoting, and interpolation behaviors are the same as
the shell's. The only difference is that the interpolation is integrated
and aware of Julia's notion of what is a single string value, and what
is a container for multiple values. Let's try the above two examples in
Julia:

.. doctest::

    julia> `perl -le '$|=1; for (0..3) { print }'`
    `perl -le '$|=1; for (0..3) { print }'`

    julia> run(ans)
    0
    1
    2
    3

    julia> first = "A"; second = "B";

    julia> `perl -le 'print for @ARGV' "1: $first" "2: $second"`
    `perl -le 'print for @ARGV' '1: A' '2: B'`

    julia> run(ans)
    1: A
    2: B

The results are identical, and Julia's interpolation behavior mimics the
shell's with some improvements due to the fact that Julia supports
first-class iterable objects while most shells use strings split on
spaces for this, which introduces ambiguities. When trying to port shell
commands to Julia, try cut and pasting first. Since Julia shows commands
to you before running them, you can easily and safely just examine its
interpretation without doing any damage.

Pipelines
---------

Shell metacharacters, such as ``|``, ``&``, and ``>``, are not special
inside of Julia's backticks: unlike in the shell, inside of Julia's
backticks, a pipe is always just a pipe:

.. doctest::

    julia> run(`echo hello | sort`)
    hello | sort

This expression invokes the ``echo`` command with three words as
arguments: "hello", "\|", and "sort". The result is that a single line
is printed: "hello \| sort". Inside of backticks, a "\|" is just a
literal pipe character. How, then, does one construct a pipeline?
Instead of using "\|" inside of backticks, one uses :func:`pipeline`:

.. doctest::

    julia> run(pipeline(`echo hello`, `sort`))
    hello

This pipes the output of the ``echo`` command to the ``sort`` command.
Of course, this isn't terribly interesting since there's only one line
to sort, but we can certainly do much more interesting things::

    julia> run(pipeline(`cut -d: -f3 /etc/passwd`, `sort -n`, `tail -n5`))
    210
    211
    212
    213
    214

This prints the highest five user IDs on a UNIX system. The ``cut``,
``sort`` and ``tail`` commands are all spawned as immediate children of
the current ``julia`` process, with no intervening shell process. Julia
itself does the work to setup pipes and connect file descriptors that is
normally done by the shell. Since Julia does this itself, it retains
better control and can do some things that shells cannot.

Julia can run multiple commands in parallel::

    julia> run(`echo hello` & `echo world`)
    world
    hello

The order of the output here is non-deterministic because the two
``echo`` processes are started nearly simultaneously, and race to make
the first write to the :const:`STDOUT` descriptor they share with each other
and the ``julia`` parent process. Julia lets you pipe the output from
both of these processes to another program::

    julia> run(pipeline(`echo world` & `echo hello`, `sort`))
    hello
    world

In terms of UNIX plumbing, what's happening here is that a single UNIX
pipe object is created and written to by both ``echo`` processes, and
the other end of the pipe is read from by the ``sort`` command.

IO redirection can be accomplished by passing keyword arguments stdin,
stdout, and stderr to the ``pipeline`` function::

    pipeline(`do_work`, stdout=pipeline(`sort`, "out.txt"), stderr="errs.txt")

Avoiding Deadlock in Pipelines
``````````````````````````````

When reading and writing to both ends of a pipeline from a single process,
it is important to avoid forcing the kernel to buffer all of the data.

For example, when reading all of the output from a command,
call ``readstring(out)``, not ``wait(process)``, since the former
will actively consume all of the data written by the process,
whereas the latter will attempt to store the data in the kernel's
buffers while waiting for a reader to be connected.

Another common solution is to separate the reader and writer
of the pipeline into separate Tasks::

     writer = @async writeall(process, "data")
     reader = @async do_compute(readstring(process))
     wait(process)
     fetch(reader)

Complex Example
```````````````

The combination of a high-level programming language, a first-class
command abstraction, and automatic setup of pipes between processes is a
powerful one. To give some sense of the complex pipelines that can be
created easily, here are some more sophisticated examples, with
apologies for the excessive use of Perl one-liners::

    julia> prefixer(prefix, sleep) = `perl -nle '$|=1; print "'$prefix' ", $_; sleep '$sleep';'`;

    julia> run(pipeline(`perl -le '$|=1; for(0..9){ print; sleep 1 }'`, prefixer("A",2) & prefixer("B",2)))
    A 0
    B 1
    A 2
    B 3
    A 4
    B 5
    A 6
    B 7
    A 8
    B 9

This is a classic example of a single producer feeding two concurrent
consumers: one ``perl`` process generates lines with the numbers 0
through 9 on them, while two parallel processes consume that output, one
prefixing lines with the letter "A", the other with the letter "B".
Which consumer gets the first line is non-deterministic, but once that
race has been won, the lines are consumed alternately by one process and
then the other. (Setting ``$|=1`` in Perl causes each print statement to
flush the :const:`STDOUT` handle, which is necessary for this example to
work. Otherwise all the output is buffered and printed to the pipe at
once, to be read by just one consumer process.)

Here is an even more complex multi-stage producer-consumer example::

    julia> run(pipeline(`perl -le '$|=1; for(0..9){ print; sleep 1 }'`,
               prefixer("X",3) & prefixer("Y",3) & prefixer("Z",3),
               prefixer("A",2) & prefixer("B",2)))
    A X 0
    B Y 1
    A Z 2
    B X 3
    A Y 4
    B Z 5
    A X 6
    B Y 7
    A Z 8
    B X 9

This example is similar to the previous one, except there are two stages
of consumers, and the stages have different latency so they use a
different number of parallel workers, to maintain saturated throughput.

We strongly encourage you to try all these examples to see how they work.
