#include <iostream>
#include "server.h"
#include "../../deps/libuv/include/uv.h"

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

#define PASS_ON(f) stream->read_cb = &f;    \
                uv_buf_t buf2;              \
    buf2.base=const_cast<char*>(pos);       \
    buf2.len=buf.len-nread-(pos-buf.base);  \
    f(stream,nread-(pos-buf.base),buf2);

void request_aborted(uv_handle_t *stream)
{
    std::cout<<"Request Aborted";
    delete (reading_in_progress*)stream->data;
    delete (uv_tcp_t*)stream;
}

void abort_request(uv_stream_t *stream)
{
    uv_close((uv_handle_t*)stream,request_aborted);
}

void read_body(uv_stream_t* stream, ssize_t nread, uv_buf_t buf)
{
    if(nread<0) {
        abort_request(stream);
        return;
    }

    reading_in_progress *p = (reading_in_progress*)(stream->data);
    if(p->isComma) {
        buf.base=buf.base+2;
        nread-=2;
    }
    if(p->bufBase==0) p->bufBase=buf.base;
    if(p->body_length>p->pos+nread) {
        p->body.append(buf.base,nread);
        p->pos+=nread;
    } else {
        p->body.append(buf.base,p->body_length-p->pos);
        //do the actual processing
        // get the type of request
        bool get = true;

        bool request_method_exists = false;
        string request_method;
        for (size_t i = 0; i < p->header_list.size(); i++)
        {
            if (to_upper(p->header_list[i].name) == "REQUEST_METHOD")
            {
                request_method_exists = true;
                request_method = p->header_list[i].value;
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
            for (size_t i = 0; i < p->header_list.size(); i++)
            {
                if (to_upper(p->header_list[i].name) == "QUERY_STRING")
                {
                    query_string_exists = true;
                    query_string = p->header_list[i].value;
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
                        p->request_obj.field_list.push_back(field_obj);
                    }
                }
            }
        }
        else
        {
            // the encoding for POST can be application/x-www-form-urlencoded or multipart/form-data
            bool content_type_exists = false;
            string content_type;
            for (size_t i = 0; i < p->header_list.size(); i++)
            {
                if (to_upper(p->header_list[i].name) == "CONTENT_TYPE")
                {
                    content_type_exists = true;
                    content_type = p->header_list[i].value;
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
                    vector<string> pairs = split(p->body, '&');

                    // construct the values
                    for (size_t i = 0; i < pairs.size(); i++)
                    {
                        vector<string> pair = split(pairs[i], '=');
                        if (pair.size() == 2)
                        {
                            field field_obj;
                            field_obj.name = url_decode(strip(pair[0]));
                            field_obj.value = url_decode(strip(pair[1]));
                            p->request_obj.field_list.push_back(field_obj);
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
                        string field_data = p->body;

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
                            p->request_obj.field_list.push_back(field_obj);
                        }
                    }
                }
            }
        }

        // get the response
        p->cb(&(p->request_obj),stream);
    }
    delete[] p->bufBase;
    p->bufBase=0;
}


void read_header(uv_stream_t* stream, ssize_t nread, uv_buf_t buf)
{
    if(nread<0) {
        abort_request(stream);
        return;
    }

    const char *pos = buf.base;
    reading_in_progress *p = (reading_in_progress*)(stream->data);
    if(p->bufBase==0)p->bufBase=buf.base;
    int i;
    if(p->isComma) {
        (++(p->pos),++pos);
        p->isComma=false;
    }
    for(i=0; i<buf.len&&p->pos<p->header_length; ++i, ++pos, ++p->pos)
    {
        if(p->inName) {
            if(*pos==0)
                p->inName=false;
            else
                p->current_header.name+=*pos;
        } else {
            if(*pos!=0)
                p->current_header.value+=*pos;
            else {
                //header complete

                // check if this is the "CONTENT_LENGTH" header
                if (p->header_list.empty())
                    p->body_length = from_string<int>(p->current_header.value);

                // add the header to the list
                p->header_list.push_back(p->current_header); //this creates a copy
                p->current_header.name="";
                p->current_header.value="";
                p->inName=true;
            }
        }
    }
    if(p->current_header.name!="")
        p->header_list.push_back(p->current_header);
    if(p->pos>=p->header_length) {
        //process cookies
        for (size_t j = 0; j < p->header_list.size(); j++)
        {
            if (to_upper(p->header_list[j].name) == "HTTP_COOKIE")
            {
                vector<string> crumb_list = split(p->header_list[j].value, ';');
                for (size_t k = 0; k < crumb_list.size(); k++)
                {
                    // get the crumb
                    vector<string> crumb = split(crumb_list[k], '=');
                    if (crumb.size() == 2)
                    {
                        // create the cookie
                        cookie cookie_obj;
                        cookie_obj.name = strip(crumb[0]);
                        cookie_obj.value = strip(crumb[1]);

                        // add the cookie the list
                        p->request_obj.cookie_list.push_back(cookie_obj);
                    }
                }
            }
        }
        p->pos=0;
        p->isComma=true;
        PASS_ON(read_body)
    } else {
        delete[] p->bufBase;
        p->bufBase=0;
    }
}

//read the length of the header;
void read_header_length(uv_stream_t* stream, ssize_t nread, uv_buf_t buf)
{
#ifdef DEBUG_TRACE
    cout << "Header Length!\n";
#endif
    if(nread<0) {
        abort_request(stream);
        return;
    }

    const char *pos = buf.base;
    reading_in_progress *p = (reading_in_progress*)(stream->data);
    if(p->bufBase==0) p->bufBase=buf.base;
    int i;
    for(i = 0; i<buf.len; ++i, ++pos)
    {
        if(*pos!=':') {
            p->header_length_str+=*pos;
        } else
            break;
    }
    if(buf.len>0&&*pos==':') {
        //we're done
        p->header_length = from_string<int>(p->header_length_str);
        PASS_ON(read_header)
    } else {
        delete[] p->bufBase;
        p->bufBase=0;
    }
}

uv_buf_t allocBuffer(uv_handle_t* handle, size_t suggested_size)
{
    uv_buf_t buf;
    buf.base = new char[suggested_size];
    buf.len  = suggested_size;
    return buf;
}

// handle a single request
void handle_request_and_release_socket(uv_stream_t* server, int status)
{
    uv_tcp_t *client = new uv_tcp_t;
    uv_tcp_init(uv_default_loop(),client);
    uv_accept((uv_stream_t*)server,(uv_stream_t*)client);

#ifdef DEBUG_TRACE
    std::cout<<"Accepted connection!\n";
#endif

    // create the request object

    reading_in_progress *p= new reading_in_progress;
    p->body_length=0;
    p->inName=true;
    p->header_length=0;
    p->pos=0;
    p->isComma=true;
    p->cstr=0;
    p->cb=(callback)server->data;
    p->bufBase=0;
    client->data=p;

    uv_read_start((uv_stream_t*)client,&allocBuffer,&read_header_length);
}

void run_server(int port, callback cb)
{
    struct sockaddr_in addr = uv_ip4_addr("0.0.0.0", port);
    uv_tcp_t server;
    int err;

    server.data=(void*)cb;

    err = uv_tcp_init(uv_default_loop(), &server);
    //assert(err == 0);
    err = uv_tcp_bind(&server, addr);
    //assert(err == 0);

#ifdef __WIN32__
	uv_tcp_simultaneous_accepts(&server,0);
#endif
	
    err = uv_listen((uv_stream_t*)&server, 128, &handle_request_and_release_socket);
    //assert(r == 0)

    uv_run(uv_default_loop(),UV_RUN_DEFAULT);
}

}
