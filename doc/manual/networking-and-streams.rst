.. _man-networking-and-streams:

.. currentmodule:: Base

************************
 Networking and Streams
************************

Julia provides a rich interface to deal with streaming I/O objects such as
terminals, pipes and TCP sockets. This interface, though asynchronous at the
system level, is presented in a synchronous manner to the programmer and it is
usually unnecessary to think about the underlying asynchronous operation. This
is achieved by making heavy use of Julia cooperative threading (:ref:`coroutine <man-tasks>`)
functionality.

Basic Stream I/O
----------------

All Julia streams expose at least a :func:`read` and a :func:`write` method,
taking the stream as their first argument, e.g.::

    julia> write(STDOUT,"Hello World");  # suppress return value 11 with ;
    Hello World

    julia> read(STDIN,Char)

    '\n'

Note that :func:`write` returns 11, the number of bytes (in ``"Hello World"``) written to :const:`STDOUT`,
but this return value is suppressed with the ``;``.

Here Enter was pressed again so that Julia would read the newline. Now, as you can see from this example,
:func:`write` takes the data to write as its second argument, while :func:`read` takes the type of the
data to be read as the second argument.

For example, to read a simple byte array, we could do::

    julia> x = zeros(UInt8,4)
    4-element Array{UInt8,1}:
     0x00
     0x00
     0x00
     0x00

    julia> read!(STDIN,x)
    abcd
    4-element Array{UInt8,1}:
     0x61
     0x62
     0x63
     0x64

However, since this is slightly cumbersome, there are several convenience methods provided. For example, we could have written the
above as::

    julia> read(STDIN,4)
    abcd
    4-element Array{UInt8,1}:
     0x61
     0x62
     0x63
     0x64

or if we had wanted to read the entire line instead::

    julia> readline(STDIN)
    abcd
    "abcd\n"

Note that depending on your terminal settings, your TTY may be line buffered and might thus require an additional enter before the data
is sent to Julia.

To read every line from :const:`STDIN` you can use :func:`eachline`::

    for line in eachline(STDIN)
        print("Found $line")
    end

or :func:`read` if you wanted to read by character instead::

    while !eof(STDIN)
        x = read(STDIN, Char)
        println("Found: $x")
    end

Text I/O
--------

Note that the :func:`write` method mentioned above operates on binary streams.
In particular, values do not get converted to any canonical text
representation but are written out as is::

    julia> write(STDOUT,0x61);  # suppress return value 1 with ;
    a

Note that ``a`` is written to :const:`STDOUT` by the :func:`write` function and
that the returned value is ``1`` (since ``0x61`` is one byte).

For text I/O, use the :func:`print` or :func:`show` methods, depending on your needs (see the standard library reference for a detailed discussion of
the difference between the two)::

    julia> print(STDOUT,0x61)
    97

IO Output Contextual Properties
-------------------------------

Sometimes IO output can benefit from the ability to pass contextual information into show methods. The ``IOContext`` object provides this framework for associating arbitrary metadata with an IO object. For example, ``showlimited`` adds a hinting parameter to the IO object that the invoked show method should print a shorter output (if applicable).

Working with Files
------------------

Like many other environments, Julia has an :func:`open` function, which takes a filename and returns an :class:`IOStream` object
that you can use to read and write things from the file. For example if we have a file, ``hello.txt``, whose contents
are ``Hello, World!``::

    julia> f = open("hello.txt")
    IOStream(<file hello.txt>)

    julia> readlines(f)
    1-element Array{String,1}:
     "Hello, World!\n"

If you want to write to a file, you can open it with the write (``"w"``) flag::

    julia> f = open("hello.txt","w")
    IOStream(<file hello.txt>)

    julia> write(f,"Hello again.")
    12

If you examine the contents of ``hello.txt`` at this point, you will notice that it is empty; nothing has actually
been written to disk yet. This is because the :class:`IOStream` must be closed before the write is actually flushed to disk::

    julia> close(f)

Examining ``hello.txt`` again will show its contents have been changed.

Opening a file, doing something to its contents, and closing it again is a very common pattern.
To make this easier, there exists another invocation of :func:`open` which takes a function
as its first argument and filename as its second, opens the file, calls the function with the file as
an argument, and then closes it again. For example, given a function::

    function read_and_capitalize(f::IOStream)
        return uppercase(readstring(f))
    end

You can call::

    julia> open(read_and_capitalize, "hello.txt")
    "HELLO AGAIN."

to open ``hello.txt``, call ``read_and_capitalize on it``, close ``hello.txt`` and return the capitalized contents.

To avoid even having to define a named function, you can use the ``do`` syntax, which creates an anonymous
function on the fly::

    julia> open("hello.txt") do f
              uppercase(readstring(f))
           end
    "HELLO AGAIN."


A simple TCP example
--------------------

Let's jump right in with a simple example involving TCP sockets. Let's first create a simple server::

    julia> @async begin
             server = listen(2000)
             while true
               sock = accept(server)
               println("Hello World\n")
             end
           end
    Task

    julia>

To those familiar with the Unix socket API, the method names will feel familiar,
though their usage is somewhat simpler than the raw Unix socket API. The first
call to :func:`listen` will create a server waiting for incoming connections on the
specified port (2000) in this case. The same function may also be used to
create various other kinds of servers::

    julia> listen(2000) # Listens on localhost:2000 (IPv4)
    TCPServer(active)

    julia> listen(ip"127.0.0.1",2000) # Equivalent to the first
    TCPServer(active)

    julia> listen(ip"::1",2000) # Listens on localhost:2000 (IPv6)
    TCPServer(active)

    julia> listen(IPv4(0),2001) # Listens on port 2001 on all IPv4 interfaces
    TCPServer(active)

    julia> listen(IPv6(0),2001) # Listens on port 2001 on all IPv6 interfaces
    TCPServer(active)

    julia> listen("testsocket") # Listens on a domain socket/named pipe
    PipeServer(active)

Note that the return type of the last invocation is different. This is because
this server does not listen on TCP, but rather on a named pipe (Windows)
or domain socket (UNIX). The difference
is subtle and has to do with the :func:`accept` and :func:`connect` methods. The :func:`accept`
method retrieves a connection to the client that is connecting on the server we
just created, while the :func:`connect` function connects to a server using the
specified method. The :func:`connect` function takes the same arguments as
:func:`listen`, so, assuming the environment (i.e. host, cwd, etc.) is the same you
should be able to pass the same arguments to :func:`connect` as you did to listen to
establish the connection. So let's try that out (after having created the server above)::

    julia> connect(2000)
    TCPSocket(open, 0 bytes waiting)

    julia> Hello World

As expected we saw "Hello World" printed. So, let's actually analyze what happened behind the scenes. When we called :func:`connect`, we connect to the server we had just created. Meanwhile, the accept function returns a server-side connection to the newly created socket and prints "Hello World" to indicate that the connection was successful.

A great strength of Julia is that since the API is exposed synchronously even though the I/O is actually happening asynchronously, we didn't have to worry callbacks or even making sure that the server gets to run. When we called :func:`connect` the current task waited for the connection to be established and only continued executing after that was done. In this pause, the server task resumed execution (because a connection request was now available), accepted the connection, printed the message and waited for the next client. Reading and writing works in the same way. To see this, consider the following simple echo server::

    julia> @async begin
             server = listen(2001)
             while true
               sock = accept(server)
               @async while isopen(sock)
                 write(sock,readline(sock))
               end
             end
           end
    Task

    julia> clientside=connect(2001)
    TCPSocket(open, 0 bytes waiting)

    julia> @async while true
              write(STDOUT,readline(clientside))
           end

    julia> println(clientside,"Hello World from the Echo Server")

    julia> Hello World from the Echo Server

As with other streams, use :func:`close` to disconnect the socket::

    julia> close(clientside)

Resolving IP Addresses
----------------------

One of the :func:`connect` methods that does not follow the :func:`listen` methods is ``connect(host::String,port)``, which will attempt to connect to the host
given by the ``host`` parameter on the port given by the port parameter. It
allows you to do things like::

    julia> connect("google.com",80)
    TCPSocket(open, 0 bytes waiting)

At the base of this functionality is :func:`getaddrinfo`, which will do the appropriate address resolution::

    julia> getaddrinfo("google.com")
    IPv4(74.125.226.225)

