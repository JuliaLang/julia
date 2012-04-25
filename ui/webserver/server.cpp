#include "server.h"

using namespace std;

namespace scgi
{
	// convert a value to a string
	template <class T> string to_string(const T& t)
	{
		// convert a value to a string
		stringstream ss;
		ss<<t;
		return ss.str();
	}

	// parse a value from a string
	template <class T> T from_string(const string& t)
	{
		// parse a value from a string
		T ret;
		stringstream(t)>>ret;
		return ret;
	}

	// strip a string of leading and trailing whitespace
	string strip(string str)
	{
		// remove leading whitespace
		while (!str.empty())
		{
			if (str[0] == ' ' || str[0] == '\t' || str[0] == '\n' || str[0] == '\r')
				str = str.substr(1, str.size()-1);
			else
				break;
		}
		// remove trailing whitespace
		while (!str.empty())
		{
			if (str[str.size()-1] == ' ' || str[str.size()-1] == '\t' || str[str.size()-1] == '\n' || str[str.size()-1] == '\r')
				str = str.substr(0, str.size()-1);
			else
				break;
		}
		// return the result
		return str;
	}

	// convert a string to lowercase
	string to_lower(string str)
	{
		// convert a string to lowercase
		for (size_t j = 0; j < str.size(); j++)
			str[j] = tolower(str[j]);
		return str;
	}

	// convert a string to uppercase
	string to_upper(string str)
	{
		// convert a string to uppercase
		for (size_t j = 0; j < str.size(); j++)
			str[j] = toupper(str[j]);
		return str;
	}

	// split strings by a character
	vector<string> split(string str, char separator)
	{
		// split a string
		vector<string> result;
		string current_str;
		for (size_t i = 0; i < str.size(); i++)
		{
			if (str[i] == separator)
			{
				result.push_back(current_str);
				current_str = "";
			}
			else
				current_str += str.substr(i, 1);
		}
		result.push_back(current_str);
		return result;
	}

	// decode data of type application/x-www-form-urlencoded
	string url_decode(string str)
	{
		// replace + characters with spaces
		for (size_t i = 0; i < str.size(); i++)
		{
			if (str[i] == '+')
				str[i] = ' ';
		}
		// escape characters
		for (size_t i = 0; i < str.size(); i++)
		{
			if (str[i] == '%')
			{
				// get the next two characters
				string code = str.substr(i+1, 2);

				// read the value
				unsigned int x;
				stringstream stream;
				stream<<std::hex<<code;
				stream>>x;
				char c = (char)x;

				// replace the character
				str = str.substr(0, i)+c+str.substr(i+3, str.size()-(i+3));
			}
		}
		// return the result
		return str;
	}

	// represents an SCGI header
	class header
	{
	public:
		string name;
		string value;
	};
}

int scgi::request::get_cookie_num()
{
	// return the number of cookies
	return (int)cookie_list.size();
}

string scgi::request::get_cookie_name(int id)
{
	// get the name of a particular cookie
	return cookie_list[id].name;
}

string scgi::request::get_cookie_value(string name)
{
	// search for the cookie by name
	for (size_t i = 0; i < cookie_list.size(); i++)
	{
		// compare the lowercase names
		if (to_lower(name) == to_lower(cookie_list[i].name))
			return cookie_list[i].value;
	}
	return "";
}

bool scgi::request::get_cookie_exists(string name)
{
	// search for the cookie by name
	for (size_t i = 0; i < cookie_list.size(); i++)
	{
		// compare the lowercase names
		if (to_lower(name) == to_lower(cookie_list[i].name))
			return true;
	}
	return false;
}

using namespace scgi;

int scgi::request::get_field_num()
{
	// return the number of fields
	return (int)field_list.size();
}

string scgi::request::get_field_name(int id)
{
	// get the name of a particular field
	return field_list[id].name;
}

string scgi::request::get_field_value(string name)
{
	// search for the field by name
	for (size_t i = 0; i < field_list.size(); i++)
	{
		// compare the lowercase names
		if (to_lower(name) == to_lower(field_list[i].name))
			return field_list[i].value;
	}
	return "";
}

string scgi::request::get_field_filename(string name)
{
	// search for the field by name
	for (size_t i = 0; i < field_list.size(); i++)
	{
		// compare the lowercase names
		if (to_lower(name) == to_lower(field_list[i].name))
			return field_list[i].filename;
	}
	return "";
}

bool scgi::request::get_field_exists(string name)
{
	// search for the field by name
	for (size_t i = 0; i < field_list.size(); i++)
	{
		// compare the lowercase names
		if (to_lower(name) == to_lower(field_list[i].name))
			return true;
	}
	return false;
}

namespace scgi
{
	// handle a single request
	void handle_request_and_release_socket(network::socket* client_sock, callback cb)
	{
		// create the request object
		request request_obj;
		vector<header> header_list;
		string body;

		// start reading the request
		string request_str = client_sock->read();
		int request_pos = 0;
		int header_pos = 0;
		int header_length = 0;
		int body_length = 0;

		// get the length of the header netstring and the following colon
		string header_length_str;
		while (true)
		{
			// make sure there is at least one character to read
			while (request_pos >= (int)request_str.size())
				request_str += client_sock->read();

			// read the next character
			string c = request_str.substr(request_pos, 1);
			request_pos++;

			// check if it is a colon
			if (c == ":")
			{
				header_pos = request_pos;
				break;
			}
			else
				header_length_str += c;
		}
		header_length = from_string<int>(header_length_str);

		// get the headers
		header_list.clear();
		while (request_pos < header_pos+header_length)
		{
			// create a new header
			header header_obj;

			// read the name
			while (true)
			{
				// make sure there is at least one character to read
				while (request_pos >= (int)request_str.size())
					request_str += client_sock->read();

				// read the next character
				string c = request_str.substr(request_pos, 1);
				request_pos++;

				// check for the end of the string
				if (c[0] == 0)
					break;

				// add the character to the string
				header_obj.name += c;
			}

			// read the value
			while (true)
			{
				// make sure there is at least one character to read
				while (request_pos >= (int)request_str.size())
					request_str += client_sock->read();

				// read the next character
				string c = request_str.substr(request_pos, 1);
				request_pos++;

				// check for the end of the string
				if (c[0] == 0)
					break;

				// add the character to the string
				header_obj.value += c;
			}

			// check if this is the "CONTENT_LENGTH" header
			if (header_list.empty())
				body_length = from_string<int>(header_obj.value);

			// add the header to the list
			header_list.push_back(header_obj);
		}

		// get the cookies
		for (size_t i = 0; i < header_list.size(); i++)
		{
			if (to_upper(header_list[i].name) == "HTTP_COOKIE")
			{
				vector<string> crumb_list = split(header_list[i].value, ';');
				for (size_t i = 0; i < crumb_list.size(); i++)
				{
					// get the crumb
					vector<string> crumb = split(crumb_list[i], '=');
					if (crumb.size() == 2)
					{
						// create the cookie
						cookie cookie_obj;
						cookie_obj.name = strip(crumb[0]);
						cookie_obj.value = strip(crumb[1]);

						// add the cookie the list
						request_obj.cookie_list.push_back(cookie_obj);
					}
				}
			}
		}

		// make sure there is at least one character to read
		while (request_pos >= (int)request_str.size())
			request_str += client_sock->read();

		// read the comma
		request_pos++;

		// get the body
		body = "";
		while ((int)body.size() < body_length)
		{
			// make sure there is at least one character to read
			while (request_pos >= (int)request_str.size())
				request_str += client_sock->read();

			// read the next character
			string c = request_str.substr(request_pos, 1);
			request_pos++;

			// add the character to the string
			body += c;
		}

		// get the type of request
		bool get = true;

		bool request_method_exists = false;
		string request_method;
		for (size_t i = 0; i < header_list.size(); i++)
		{
			if (to_upper(header_list[i].name) == "REQUEST_METHOD")
			{
				request_method_exists = true;
				request_method = header_list[i].value;
				break;
			}
		}

		if (request_method_exists)
		{
			if (to_upper(request_method) == "POST")
				get = false;
		}
		if (get)
		{
			// the encoding for GET is application/x-www-form-urlencoded
			bool query_string_exists = false;
			string query_string;
			for (size_t i = 0; i < header_list.size(); i++)
			{
				if (to_upper(header_list[i].name) == "QUERY_STRING")
				{
					query_string_exists = true;
					query_string = header_list[i].value;
					break;
				}
			}

			if (query_string_exists)
			{
				// get the key=value strings
				vector<string> pairs = split(query_string, '&');

				// construct the values
				for (size_t i = 0; i < pairs.size(); i++)
				{
					vector<string> pair = split(pairs[i], '=');
					if (pair.size() == 2)
					{
						field field_obj;
						field_obj.name = url_decode(strip(pair[0]));
						field_obj.value = url_decode(strip(pair[1]));
						request_obj.field_list.push_back(field_obj);
					}
				}
			}
		}
		else
		{
			// the encoding for POST can be application/x-www-form-urlencoded or multipart/form-data
			bool content_type_exists = false;
			string content_type;
			for (size_t i = 0; i < header_list.size(); i++)
			{
				if (to_upper(header_list[i].name) == "CONTENT_TYPE")
				{
					content_type_exists = true;
					content_type = header_list[i].value;
					break;
				}
			}

			if (content_type_exists)
			{
				// get the subparts of the CONTENT_TYPE header
				vector<string> content_type_fields = split(content_type, ';');
				for (size_t i = 0; i < content_type_fields.size(); i++)
					content_type_fields[i] = strip(content_type_fields[i]);

				// application/x-www-form-urlencoded
				if (to_lower(content_type_fields[0]) == "application/x-www-form-urlencoded")
				{
					// get the key=value strings
					vector<string> pairs = split(body, '&');

					// construct the values
					for (size_t i = 0; i < pairs.size(); i++)
					{
						vector<string> pair = split(pairs[i], '=');
						if (pair.size() == 2)
						{
							field field_obj;
							field_obj.name = url_decode(strip(pair[0]));
							field_obj.value = url_decode(strip(pair[1]));
							request_obj.field_list.push_back(field_obj);
						}
					}
				}

				// multipart/form-data
				if (to_lower(content_type_fields[0]) == "multipart/form-data")
				{
					// get the boundary fields
					vector<string> boundary_fields = split(content_type_fields[1], '=');
					for (size_t i = 0; i < boundary_fields.size(); i++)
						boundary_fields[i] = strip(boundary_fields[i]);
					if (boundary_fields.size() == 2)
					{
						// extract the boundary
						string boundary = boundary_fields[1];

						// get the field data from the body
						string field_data = body;

						// loop through the fields
						while (true)
						{
							// make a new field
							field field_obj;

							// skip whitespace
							while (!field_data.empty())
							{
								if (field_data[0] != ' ' && field_data[0] != '\t' && field_data[0] != '\r' && field_data[0] != '\n')
									break;
								field_data = field_data.substr(1, field_data.size()-1);
							}

							// skip the boundary
							if (field_data.size() < boundary.size()+2)
								break;
							if (field_data.substr(0, boundary.size()+2) != "--"+boundary)
								break;
							field_data = field_data.substr(boundary.size()+2, field_data.size()-(boundary.size()+2));

							// skip whitespace
							while (!field_data.empty())
							{
								if (field_data[0] != ' ' && field_data[0] != '\t' && field_data[0] != '\r' && field_data[0] != '\n')
									break;
								field_data = field_data.substr(1, field_data.size()-1);
							}

							// check if we have reached the end
							if (field_data.size() < 2)
								break;
							if (field_data.substr(0, 2) == "--")
								break;

							// start parsing lines
							while (true)
							{
								// get the line
								size_t newline_pos = field_data.size();
								for (size_t i = 0; i < field_data.size(); i++)
								{
									if (field_data[i] == '\n')
									{
										newline_pos = i;
										break;
									}
								}
								string line = strip(field_data.substr(0, newline_pos));
								field_data = field_data.substr(newline_pos+1, field_data.size()-(newline_pos+1));

								// check if we're done parsing the headers
								if (line.empty())
									break;

								// check if this is a "Content-Disposition" header
								string disposition_header = "Content-Disposition:";
								if (line.size() >= disposition_header.size())
								{
									if (to_lower(line.substr(0, disposition_header.size())) == to_lower(disposition_header))
									{
										// take out the "Content-Disposition:" tag
										line = line.substr(disposition_header.size(), line.size()-disposition_header.size());

										// get the disposition fields
										vector<string> disposition_fields = split(line, ';');
										for (size_t i = 0; i < disposition_fields.size(); i++)
											disposition_fields[i] = strip(disposition_fields[i]);

										// parse the fields
										for (size_t i = 0; i < disposition_fields.size(); i++)
										{
											// get the key=value pair
											vector<string> pair = split(disposition_fields[i], '=');
											if (pair.size() == 2)
											{
												// name
												if (to_upper(strip(pair[0])) == "NAME")
												{
													// get the name
													field_obj.name = strip(pair[1]);

													// strip the quotes
													if (field_obj.name.size() >= 2)
													{
														if (field_obj.name[0] == '\"' && field_obj.name[field_obj.name.size()-1] == '\"')
															field_obj.name = field_obj.name.substr(1, field_obj.name.size()-2);
													}

													// move on
													continue;
												}
											
												// filename
												if (to_upper(strip(pair[0])) == "FILENAME")
												{
													// get the filename
													field_obj.filename = strip(pair[1]);

													// strip the quotes
													if (field_obj.filename.size() >= 2)
													{
														if (field_obj.filename[0] == '\"' && field_obj.filename[field_obj.filename.size()-1] == '\"')
															field_obj.filename = field_obj.filename.substr(1, field_obj.filename.size()-2);
													}

													// move on
													continue;
												}
											}
										}
									}
								}
							}

							// find out how much data to extract
							size_t field_size = 0;
							for (size_t i = 0; i < field_data.size()-(boundary.size()+2); i++)
							{
								if (field_data.substr(i, boundary.size()+3) == "\n--"+boundary || field_data.substr(i, boundary.size()+4) == "\r\n--"+boundary)
								{
									field_size = i;
									break;
								}
							}

							// extract the data
							field_obj.value = field_data.substr(0, field_size);
							field_data = field_data.substr(field_size, field_data.size()-field_size);

							// add the field to the list
							request_obj.field_list.push_back(field_obj);
						}
					}
				}
			}
		}

		// get the response
		string response = cb(&request_obj);

		// write the response
		client_sock->write(response);

		// close the connection to the client
		client_sock->close();

		// release memory
		delete client_sock;
	}
}

void scgi::run_server(int port, callback cb)
{
	// bind the socket
	network::socket server_sock;
	server_sock.bind(port);

	// main server loop
	while (true)
	{
		// accept a new connection
		network::socket* client_sock = new network::socket;
		server_sock.listen(client_sock);
		
		// handle the request
		handle_request_and_release_socket(client_sock, cb);
	}
}
