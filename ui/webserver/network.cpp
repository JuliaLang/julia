#include "network.h"

using namespace std;

namespace network_wrappers
{
	template <class T> string to_string(const T& t)
	{
		// convert a value to a string
		stringstream ss;
		ss<<t;
		return ss.str();
	}

	template <class T> T from_string(const string& t)
	{
		// parse a value from a string
		T ret;
		stringstream(t)>>ret;
		return ret;
	}

	#ifdef _WIN32
		SOCKET wrap_bind(int port)
	#else
		int wrap_bind(int port)
	#endif
	{
		// create a new socket bound to a particular port and return the socket
		#ifdef _WIN32
			string port_str = to_string(port);
			addrinfo* result = NULL;
			addrinfo hints;
			ZeroMemory(&hints, sizeof(hints));
			hints.ai_family = AF_INET;
			hints.ai_socktype = SOCK_STREAM;
			hints.ai_protocol = IPPROTO_TCP;
			hints.ai_flags = AI_PASSIVE;
			if (getaddrinfo(NULL, port_str.c_str(), &hints, &result) != 0)
				throw network::socket_error("error creating socket");
			SOCKET socket_desc = socket(result->ai_family, result->ai_socktype, result->ai_protocol);
			if (socket_desc == INVALID_SOCKET)
			{
				freeaddrinfo(result);
				throw network::socket_error("error creating socket");
			}
			int optval = 1;
			if (setsockopt(socket_desc, SOL_SOCKET, SO_REUSEADDR, (char*)(&optval), sizeof(optval)) == SOCKET_ERROR)
			{
				freeaddrinfo(result);
				closesocket(socket_desc);
				throw network::socket_error("error setting socket options");
			}
			if (bind(socket_desc, result->ai_addr, (int)result->ai_addrlen) == SOCKET_ERROR)
			{
				freeaddrinfo(result);
				closesocket(socket_desc);
				throw network::socket_error("error binding socket");
			}
			freeaddrinfo(result);
			return socket_desc;
		#else
			int socket_desc = socket(AF_INET, SOCK_STREAM, 0);
			if (socket_desc == -1)
				throw network::socket_error("error creating socket");
			int optval = 1;
			if (setsockopt(socket_desc, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval)))
			{
				close(socket_desc);
				throw network::socket_error("error setting socket options");
			}
			sockaddr_in server;
			server.sin_family = AF_INET;
			server.sin_addr.s_addr = INADDR_ANY;
			server.sin_port = htons(port);
			if (bind(socket_desc, (sockaddr*)&server, sizeof(server)))
			{
				close(socket_desc);
				throw network::socket_error("error binding socket");
			}
			return socket_desc;
		#endif
	}

	#ifdef _WIN32
		SOCKET wrap_listen(SOCKET socket_id)
	#else
		int wrap_listen(int socket_id)
	#endif
	{
		// listen for an incoming connection, accept it, and return the new socket
		#ifdef _WIN32
			if (listen(socket_id, 1) == SOCKET_ERROR)
				throw network::socket_error("error listening for connections");
			SOCKET socket_desc = accept(socket_id, NULL, NULL);
			if (socket_desc == INVALID_SOCKET)
				throw network::socket_error("error accepting connection");
			return socket_desc;
		#else
			if (listen(socket_id, 1))
				throw network::socket_error("error listening for connections");
			sockaddr_in client;
			socklen_t client_len = sizeof(sockaddr_in);
			int socket_desc = accept(socket_id, (sockaddr*)&client, &client_len);
			if (socket_desc == -1)
				throw network::socket_error("error accepting connection");
			return socket_desc;
		#endif
	}

	#ifdef _WIN32
		SOCKET wrap_connect(string hostname, int port)
	#else
		int wrap_connect(string hostname, int port)
	#endif
	{
		// connect to a remote host and return the socket
		#ifdef _WIN32
			string port_str = to_string(port);
			addrinfo *result = NULL;
			addrinfo hints;
			ZeroMemory(&hints, sizeof(hints));
			hints.ai_family = AF_UNSPEC;
			hints.ai_socktype = SOCK_STREAM;
			hints.ai_protocol = IPPROTO_TCP;
			if (getaddrinfo(hostname.c_str(), port_str.c_str(), &hints, &result) != 0)
				throw network::socket_error("error creating socket");
			SOCKET socket_desc = INVALID_SOCKET;
			for (addrinfo* ptr = result; ptr != NULL; ptr = ptr->ai_next)
			{
				socket_desc = socket(ptr->ai_family, ptr->ai_socktype, ptr->ai_protocol);
				if (socket_desc == INVALID_SOCKET)
				{
					freeaddrinfo(result);
					throw network::socket_error("error creating socket");
				}
				if (connect(socket_desc, ptr->ai_addr, (int)ptr->ai_addrlen) == SOCKET_ERROR)
				{
					closesocket(socket_desc);
					socket_desc = NULL;
				}
				else
					break;
			}
			freeaddrinfo(result);
			if (socket_desc == INVALID_SOCKET)
				throw network::socket_error("error creating socket");
			return socket_desc;
		#else
			int socket_desc = socket(AF_INET, SOCK_STREAM, 0);
			if (socket_desc == -1)
				throw network::socket_error("error creating socket");
			hostent *he = gethostbyname(hostname.c_str());
			if (!he)
				throw network::socket_error("error resolving hostname");
			sockaddr_in server;
			memcpy(&server.sin_addr, he->h_addr_list[0], he->h_length);
			server.sin_family = AF_INET;
			server.sin_port = htons(port);
			if (connect(socket_desc, (sockaddr*)&server, sizeof(server)))
				throw network::socket_error("error connecting to host");
			return socket_desc;
		#endif
	}

	#ifdef _WIN32
		void wrap_close(SOCKET socket_id)
	#else
		void wrap_close(int socket_id)
	#endif
	{
		// close a socket
		#ifdef _WIN32
			shutdown(socket_id, SD_BOTH);
			closesocket(socket_id);
		#else
			shutdown(socket_id, SHUT_RDWR);
			close(socket_id);
		#endif
	}

	#ifdef _WIN32
		bool wrap_select(SOCKET socket_id)
	#else
		bool wrap_select(int socket_id)
	#endif
	{
		// return whether a socket has data to read
		#ifdef _WIN32
			fd_set set;
			FD_ZERO(&set);
			FD_SET(socket_id, &set);
			TIMEVAL select_timeout;
			select_timeout.tv_sec = 0;
			select_timeout.tv_usec = 100000;
			return (select(0, &set, 0, 0, &select_timeout) != 0);
		#else
			fd_set set;
			FD_ZERO(&set);
			FD_SET(socket_id, &set);
			timeval select_timeout;
			select_timeout.tv_sec = 0;
			select_timeout.tv_usec = 100000;
			return (select(FD_SETSIZE, &set, 0, 0, &select_timeout) != 0);
		#endif
	}

	#ifdef _WIN32
		string wrap_read(SOCKET socket_id)
	#else
		string wrap_read(int socket_id)
	#endif
	{
		// read from a socket and return the data or an empty string if the socket was closed
		#ifdef _WIN32
			char buffer[1024];
			int bytes_read = recv(socket_id, buffer, 1024, 0);
			if (bytes_read == -1)
				throw network::socket_error("error reading data from socket");
			string result(bytes_read, '\0');
			for (int i = 0; i < bytes_read; i++)
				result[i] = buffer[i];
			return result;
		#else
			char buffer[1024];
			int bytes_read = recv(socket_id, buffer, 1024, 0);
			if (bytes_read == -1)
				throw network::socket_error("error reading data from socket");
			string result(bytes_read, '\0');
			for (int i = 0; i < bytes_read; i++)
				result[i] = buffer[i];
			return result;
		#endif
	}

	#ifdef _WIN32
		bool wrap_write(SOCKET socket_id, string str)
	#else
		bool wrap_write(int socket_id, string str)
	#endif
	{
		// write to a socket and return whether the socket is still open
		#ifdef _WIN32
			while (true)
			{
				int bytes_sent = send(socket_id, str.c_str(), str.size(), 0);
				if (bytes_sent == SOCKET_ERROR)
					return false;
				if (bytes_sent == (int)str.size())
					return true;
				str = str.substr(bytes_sent, str.size()-bytes_sent);
			}
		#else
			while (true)
			{
				int bytes_sent = send(socket_id, str.c_str(), str.size(), 0);
				if (bytes_sent == -1)
					return false;
				if (bytes_sent == (int)str.size())
					return true;
				str = str.substr(bytes_sent, str.size()-bytes_sent);
			}
		#endif
	}

	// this class allows us to simulate a static constructor/destructor pair
	class socket_static
	{
	public:
		socket_static()
		{
			#ifdef _WIN32
				// initialize Winsock
				WSADATA wsa_data;
				if (WSAStartup(MAKEWORD(2, 2), &wsa_data) != 0)
					throw network::socket_error("error initializing Winsock");
			#else
				// nothing to do here for UNIX/Linux systems
			#endif
		}

		~socket_static()
		{
			#ifdef _WIN32
				// release winsock
				if (WSACleanup() != 0)
					throw network::socket_error("error releasing Winsock");
			#else
				// nothing to do here for UNIX/Linux systems
			#endif
		}
	};

	// call the constructor on program start and the destructor on program exit
	socket_static sock_static;
}

network::socket_error::socket_error(string msg)
{
	// constructor
	message = msg;
}

network::socket::socket()
{
	// constructor
	#ifdef _WIN32
		socket_id = INVALID_SOCKET;
	#else
		socket_id = -1;
	#endif
}

network::socket::~socket()
{
	// destructor
	close();
}

network::socket::socket(const network::socket & other)
{
	// disallow copy constructor
	throw socket_error("socket copy constructor disallowed");
}

network::socket & network::socket::operator= (const network::socket & other)
{
	// disallow assignment operator
	throw socket_error("socket assignment operator disallowed");
}

void network::socket::bind(int port)
{
	// bind to a particular port
	#ifdef _WIN32
		SOCKET socket_desc = network_wrappers::wrap_bind(port);
	#else
		int socket_desc = network_wrappers::wrap_bind(port);
	#endif
	close();
	socket_id = socket_desc;
}

void network::socket::listen(network::socket* client_sock)
{
	// listen on the binded port
	#ifdef _WIN32
		if (socket_id == INVALID_SOCKET)
			throw socket_error("socket not open for listening");
		SOCKET socket_desc = network_wrappers::wrap_listen(socket_id);
	#else
		if (socket_id == -1)
			throw socket_error("socket not open for listening");
		int socket_desc = network_wrappers::wrap_listen(socket_id);
	#endif
	client_sock->close();
	client_sock->socket_id = socket_desc;
}

void network::socket::connect(string hostname, int port)
{
	// connect to a foreign host
	#ifdef _WIN32
		SOCKET socket_desc = network_wrappers::wrap_connect(hostname, port);
	#else
		int socket_desc = network_wrappers::wrap_connect(hostname, port);
	#endif
	close();
	socket_id = socket_desc;
}

void network::socket::close()
{
	// close the connection
	#ifdef _WIN32
		if (socket_id != INVALID_SOCKET)
		{
			network_wrappers::wrap_close(socket_id);
			socket_id = INVALID_SOCKET;
		}
	#else
		if (socket_id != -1)
		{
			network_wrappers::wrap_close(socket_id);
			socket_id = -1;
		}
	#endif
}

bool network::socket::has_data()
{
	// return whether there is data to read
	#ifdef _WIN32
		if (socket_id == INVALID_SOCKET)
			return false;
	#else
		if (socket_id == -1)
			return false;
	#endif
	return network_wrappers::wrap_select(socket_id);
}

string network::socket::read()
{
	// read some data from the socket
	#ifdef _WIN32
		if (socket_id == INVALID_SOCKET)
			return "";
	#else
		if (socket_id == -1)
			return "";
	#endif
	string data = network_wrappers::wrap_read(socket_id);
	if (data.empty())
		close();
	return data;
}

void network::socket::write(string str)
{
	// write some data to the socket
	#ifdef _WIN32
		if (socket_id == INVALID_SOCKET)
			return;
	#else
		if (socket_id == -1)
			return;
	#endif
	if (!network_wrappers::wrap_write(socket_id, str))
		close();
}

bool network::socket::is_open()
{
	// return whether the socket is open
	#ifdef _WIN32
		return (socket_id != INVALID_SOCKET);
	#else
		return (socket_id != -1);
	#endif
}
