.. _man-networking-and-streams

************************
 Networking and Streams  
************************

Julia provides a rich interface to deal with streaming I/O objects such as
Terminals, Pipes and Tcp Sockets. This interface, though asynchronous at the 
system level, is presented in a synchronous manner to the programmer and it is
usually unnecessary to think about the underlying asynchronous operation. This 
is achieved by making heavy use of Julia cooperative threading (coroutine) 
functionality.

So, let's jump right in with a simple example involving Tcp Sockets. To do, let's first create a simple server.

:: 

    julia> @async begin
             server = listen(2000)
             while true
               sock = accept(server)
               println("Hello World\n")
             end
           end
    Task

    julia>

Those familiar with the Unix socket API, the method names will feel familiar, 
though their usage is somewhat simpler than the raw Unix socket API. The first
call to `listen` will create a server waiting for incoming connections on the 
specified port (2000) in this case. The same function may also be used to 
create various other kinds of servers:

::
    
    julia> listen(2000) # Listens on localhost:2000 (IPv4)
    TcpServer(active)

    julia> listen(ip"127.0.0.1",2000) # Equivalent to the first
    TcpServer(active)

    julia> listen(ip"::1",2000) # Listens on localhost:2000 (IPv6)
    TcpServer(active)

    julia> listen(IPv4(0),2001) # Listens on port 2001 on all IPv4 interfaces
    TcpServer(active)

    julia> listen(IPv6(0),2001) # Listens on port 2001 on all IPv6 interfaces
    TcpServer(active)

    julia> listen("testsocket") # Listens on a domain socket/named pipe
    PipeServer(active)

Note that the return type of the last invocation is different. This is because 
this server does not listen on TCP, but rather on a Named Pipe (Windows 
terminology) - also called a Domain Socket (UNIX Terminology). The difference 
is subtle but, has to do with the `accept` and `connect` methods. The `accept`
method retrieves a connection to the client that is connecting on the server we
just created, while the `connect` function connects to a server using the 
specified method. The `connect` function takes the same arguments as the 
`listen`, so, assuming the environment (i.e. host, cwd, etc.) is the same you 
should be able to pass the same arguments to `connect` as you did to listen to 
establish the connection. So let's try that out (after having created the server above): ::
    
    julia> connect(2000)
    TcpSocket(open, 0 bytes waiting)

    julia> Hello World



As expected we saw "Hello World" printed. So, let's actually analyze what happened behind the scenes. When we called connect, we connect to the server we had just created. Meanwhile, the accept function returns a server-side connection to the newly created socket and prints "Hello World" to indicate that the connection was successful. 

A great strength of Julia is that since the API is exposed synchronously even though the I/O is actually happening asynchronously, we didn't have to worry callbacks or even making sure that the server gets to run. When we called `connect` the current task waited for the connection to be established and only continued executing after that was done. In this pause, the server task resumed execution (because a connection request was now available), accepted the connection, printed the message and waited for the next client. Reading and writing works in the same way. To see this, consider the following simple echo server::
    
    julia> @async begin
             server = listen(2001)
             while true
               sock = accept(server)
               @async while true
                 write(sock,readline(sock))
               end
             end
           end
    Task

    julia> clientside=connect(2001)
    TcpSocket(open, 0 bytes waiting)

    julia> @async while true
              write(STDOUT,readline(clientside))
           end

    julia> println(clientside,"Hello World from the Echo Server")

    julia> Hello World from the Echo Server

Resolving IP Addresses
----------------------

One of the `connect` methods that does not follow the `listen` methods is `connect(host::ASCIIString,port)`, which will attempt to connect to the host 
given by the `host` parameter on the port given by the port parameter. It 
allows you to do thins like::
    
    julia> connect("google.com",80)
    TcpSocket(open, 0 bytes waiting)

At the base of this functionality is the getaddrinfo function which will do the appropriate address resolution::
        
    julia> Base.getaddrinfo("google.com")
    IPv4(74.125.226.225)