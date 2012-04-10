/*
 * Networking Library
 * Version 1.0
 * Stephan Boyer
 * May 2011
 */

#ifndef INCLUDE_NETWORK
#define INCLUDE_NETWORK

#include <string>
#include <sstream>
#include <cstring>
#ifdef _WIN32
	#include <winsock2.h>
	#include <ws2tcpip.h>
	#pragma comment(lib, "Ws2_32.lib")
#else
	#include <sys/socket.h>
	#include <netdb.h>
  #include <unistd.h>
#endif

#ifdef __FreeBSD__
	#include <netinet/in.h>
#endif

namespace network
{
	// error class for exception handling
	class socket_error
	{
	public:
		// constructor
		socket_error(std::string msg);

		// error message
		std::string message;
	};

	// a socket class
	class socket
	{
	public:
		// constructor
		socket();

		// destructor
		~socket();

		// copy constructor
		socket(const socket & other);

		// assignment operator
		socket & operator= (const socket & other);

		// for servers, bind to a port
		void bind(int port);

		// for servers, wait for an incoming connection on the bound port and accept it
		void listen(socket* client);

		// for clients, make an outgoing connection
		void connect(std::string hostname, int port);

		// terminate the connection (fails silently if socket already closed)
		void close();

		// return if there is data to read
		bool has_data();

		// read data (returns empty string if socket closed)
		std::string read();

		// write data (fails silently if socket is closed)
		void write(std::string str);

		// check if the socket is connected
		bool is_open();

	private:
		// the socket identifier
		#ifdef _WIN32
			SOCKET socket_id;
		#else
			int socket_id;
		#endif
	};
}

#endif
