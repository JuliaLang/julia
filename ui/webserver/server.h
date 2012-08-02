#ifndef INCLUDE_SERVER
#define INCLUDE_SERVER

#include "../../deps/libuv/include/uv.h"
#include <string>
#include <vector>
#include <sstream>
//#define JULIA_DEBUG_TRACE
//#define CPP_DEBUG_TRACE

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

	// represents a cookie
	class cookie
	{
	public:
		std::string name;
		std::string value;
	};

	// represents an HTTP request
	class request
	{
	public:
		/////////////////////////////////////////////
		// form fields
		/////////////////////////////////////////////

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

		/////////////////////////////////////////////
		// cookies
		/////////////////////////////////////////////

		// get the number of cookies
		int get_cookie_num();

		// get a cookie name
		std::string get_cookie_name(int id);

		// get a cookie value (not case sensitive)
		std::string get_cookie_value(std::string name);

		// get a cookie filename (for file uploads; not case sensitive)
		std::string get_cookie_filename(std::string name);

		// get whether a cookie exists (not case sensitive)
		bool get_cookie_exists(std::string name);

		// the form cookies
		std::vector<cookie> cookie_list;
	};

    // represents an SCGI header
    class header
    {
    public:
        std::string name;
        std::string value;
    };

    // the callback function for requests:  string response = callback(request req);
    // if the "multithreaded" parameter of run_server is true, this callback must be
    // thread safe!
    typedef void (*callback)(request*,uv_stream_t *client);

    struct reading_in_progress {
        std::string header_length_str;
        int header_length;
        int body_length;
        int pos;
        std::vector<header> header_list;
        header current_header;
        bool inName;
        bool isComma;
        request request_obj;
        char *cstr;
        std::string body;
        callback cb;
        const char *bufBase;
    };

	// run the server - this blocks forever
    void run_server(int port, callback cb);
}

struct julia_session;

#endif
