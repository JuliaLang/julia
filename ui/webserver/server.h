/*
 * SCGI Server
 * Version 1.0
 * Stephan Boyer
 * May 2011
 */

#ifndef INCLUDE_SERVER
#define INCLUDE_SERVER

#include "network.h"
#include <string>
#include <vector>
#include <sstream>

namespace scgi
{
	// represents a GET/POST field
	class field
	{
	public:
		std::string name;
		std::string value;
		std::string filename;
	};

	// represents an HTTP request
	class request
	{
	public:
		// get the number of fields
		int get_field_num();

		// get a field name
		std::string get_field_name(int id);

		// get a field value (not case sensitive)
		std::string get_field_value(std::string name);

		// get a field filename (for file uploads; not case sensitive)
		std::string get_field_filename(std::string name);

		// get whether a field exists (not case sensitive)
		bool get_field_exists(std::string name);

		// the form fields
		std::vector<field> field_list;
	};

	// the callback function for requests:  string response = callback(request req);
	// if the "multithreaded" parameter of run_server is true, this callback must be
	// thread safe!
	typedef std::string (*callback)(request*);

	// run the server - this blocks forever
	void run_server(int port, callback cb);
}

#endif