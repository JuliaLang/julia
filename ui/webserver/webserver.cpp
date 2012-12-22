#include <iostream>
#include <fstream>
#include <map>
#include <ctime>
#include <stdlib.h>
#include <cstring>
#include <unistd.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include "server.h"
#include "json.h"
#include "message_types.h"
#ifndef __WIN32__
#include <sys/wait.h>
#endif

extern char **environ; //only unistd.h post- POSIX 2008

using namespace std;
using namespace scgi;

/////////////////////////////////////////////////////////////////////////////
// string helper functions
/////////////////////////////////////////////////////////////////////////////

// convert a value to a string
template <class T> std::string to_string(const T& t)
{
    // convert a value to a string
    stringstream ss;
    ss<<t;
    return ss.str();
}

// parse a string from a value
template <class T> T from_string(const std::string& t)
{
    // parse a value from a string
    T ret;
    stringstream(t)>>ret;
    return ret;
}

/////////////////////////////////////////////////////////////////////////////
// messages
/////////////////////////////////////////////////////////////////////////////

// a message
class message
{
public:
    // see ui/webserver/julia_web_base.j for documentation
    uint8_t type;
    vector<string> args;
};

/////////////////////////////////////////////////////////////////////////////
// julia_sessions
/////////////////////////////////////////////////////////////////////////////

// if a julia_session hasn't been queried in this time, it dies
const int WEB_SESSION_TIMEOUT = 20; // in seconds

// a web julia_session
struct web_session {
    // time since watchdog was last "pet"
    time_t update_time;

    // to be sent to the client
    vector<message> outbox;

    // the user name
    string user_name;
};

enum julia_session_status
{
    SESSION_WAITING_FOR_PORT_NUM,
    SESSION_NORMAL,
    SESSION_TERMINATING,
    SESSION_KILLED,
};

// a julia_session
struct julia_session
{
    // a map from julia_session tokens to web julia_sessions that use this julia julia_session
    map<string, web_session> web_session_map;

    // the session name
    string session_name;

    // to be sent to julia
    string inbox_std;

    // to be sent to julia
    vector<message> inbox;

    // to be sent to the client as MSG_OTHER_OUTPUT
    string outbox_std;

    // to be converted into messages
    string outbox_raw;

    //temporary buffer for reading the portnumber
    string portbuf;

    // keep track of messages sent to all the clients (not messages sent to particular users)
    vector<message> outbox_history;

    //pipes
    uv_process_t *proc;
    uv_pipe_t *julia_in;
    uv_pipe_t *julia_out;
    uv_pipe_t *julia_err;

    // the socket for communicating to julia
    uv_tcp_t *sock;

    // whether the julia_session should terminate
    bool should_terminate;

    // the status of the julia_session
    julia_session_status status;

    //notifier
    uv_async_t *data_notifier;
};

// a list of all julia_sessions
vector<julia_session*> julia_session_list;

void close_process(julia_session *julia_session_ptr);

/////////////////////////////////////////////////////////////////////////////
// THREAD:  inbox_thread (from browser to julia)
/////////////////////////////////////////////////////////////////////////////

// add to the inbox regularly according to this interval
const int INBOX_INTERVAL = 10000; // in nanoseconds

void free_socket_write_buffer(uv_write_t* req, int status)
{
    delete[] ((char*)req->data);
    delete req;
}

void free_write_buffer(uv_write_t* req, int status)
{
    delete[] (char*)req->data;
    delete req;
}

static uv_buf_t alloc_buf(uv_handle_t* handle, size_t suggested_size) {
    uv_buf_t buf;
    buf.len = suggested_size;
    buf.base = new char[suggested_size];
    return buf;
}


//when data arrives from client (currently async, will be websocket)
void read_client(uv_async_t* handle, int status)
{
    julia_session *julia_session_ptr=(julia_session*)handle->data;

    if(julia_session_ptr->status != SESSION_NORMAL) {
        return;
    }

    // get the inbox data
    string inbox = julia_session_ptr->inbox_std;

    // if there is no inbox data and no messages to send, or if julia isn't ready, wait and try again
    if ((inbox == "" && julia_session_ptr->inbox.empty()) || julia_session_ptr->status != SESSION_NORMAL)
    {
        return;
    }

    if(inbox.size()>0) {
        char *cstr = new char [inbox.size()];
        memcpy (cstr, inbox.data(), inbox.size());

        uv_buf_t buffer;
        buffer.base = cstr;
        buffer.len = inbox.size();

        uv_write_t *req = new uv_write_s;
        req->data=(void*)cstr;
        uv_write(req,(uv_stream_t*)julia_session_ptr->julia_in,&buffer,1,&free_write_buffer);
    }

    // write messages to julia
    for (size_t i = 0; i < julia_session_ptr->inbox.size(); i++)
    {
        // get the message
        message msg = julia_session_ptr->inbox[i];

        uv_write_t *req2 = new uv_write_s;

        size_t total_size=2+4*msg.args.size();
        for (size_t j = 0; j < msg.args.size(); j++) {
            total_size+=msg.args[j].size();
        }
        char *msg_buf = new char[total_size];
        uv_buf_t buf;
        buf.base=msg_buf;
        buf.len=total_size;
        req2->data = msg_buf;

        *(msg_buf++)=msg.type;
        *(msg_buf++)=msg.args.size();

        if(msg.args.size()>0) {

            // iterate through the arguments
            for (size_t j = 0; j < msg.args.size(); j++) {
                // write the size of the argument
                *((uint32_t*)(msg_buf)) = (uint32_t)msg.args[j].size();
                memcpy((msg_buf+sizeof(uint32_t)),msg.args[j].data(),msg.args[j].size());
                msg_buf+=sizeof(uint32_t)+msg.args[j].size();
            }
        }


        uv_buf_t buf2;
        uv_write(req2,(uv_stream_t*)julia_session_ptr->sock,&buf,1,&free_socket_write_buffer);
    }
    julia_session_ptr->inbox.clear();

    // remove the written data from the inbox
    julia_session_ptr->inbox_std.clear();
}

/////////////////////////////////////////////////////////////////////////////
// THREAD:  outbox_thread (from julia to browser)
/////////////////////////////////////////////////////////////////////////////

// add to the outbox regularly according to this interval
const int OUTBOX_INTERVAL = 10000; // in nanoseconds

// check for the port number from julia according to this interval
const int PORT_NUM_INTERVAL = 10000; // in nanoseconds

void socketClosed(uv_handle_t *stream)
{
    delete (uv_tcp_t *)stream;
}

//read from julia on metadata socket (typically starts at 4444)
void readSocketData(uv_stream_t *sock,ssize_t nread, uv_buf_t buf)
{
    julia_session *julia_session_ptr=(julia_session*)sock->data;
    if(nread == -1)
        return close_process(julia_session_ptr);
    else if(julia_session_ptr->status != SESSION_NORMAL)
        return;

    julia_session_ptr->outbox_raw.append(buf.base,nread);

    // try to convert the raw outbox data into messages
    string outbox_raw = julia_session_ptr->outbox_raw;
    while (outbox_raw.size() >= 2)
    {
        // construct the message
        message msg;

        // get the message type
        msg.type = (*((uint8_t*)(&outbox_raw[0])));

        // get the number of arguments
        uint8_t arg_num = *((uint8_t*)(&outbox_raw[1]));

        // try to read the arguments
        int pos = 2;
        for (uint8_t i = 0; i < arg_num; i++)
        {
            // make sure there is enough data left to read
            if (outbox_raw.size() < pos+4)
                goto done;

            // get the size of this argument
            uint32_t arg_size = *((uint32_t*)(&outbox_raw[pos]));
            pos += 4;

            // make sure there is enough data left to read
            if (outbox_raw.size() < pos+arg_size)
                goto done;

            // get the argument
            msg.args.push_back(outbox_raw.substr(pos, arg_size));
            pos += arg_size;
        }

        // check if we have a whole message
        if (msg.args.size() == arg_num)
        {
            // we have a whole message - eat it from outbox_raw
            outbox_raw = julia_session_ptr->outbox_raw = outbox_raw.substr(pos, outbox_raw.size()-pos);


            // add the message to the outbox queue of all the users of this julia session if necessary
            if (msg.type == MSG_OUTPUT_EVAL_INPUT ||
                msg.type == MSG_OUTPUT_EVAL_RESULT ||
                msg.type == MSG_OUTPUT_EVAL_ERROR ||
                msg.type == MSG_OUTPUT_PLOT ||
                msg.type == MSG_OUTPUT_HTML) {
                julia_session_ptr->outbox_history.push_back(msg);
                for (map<string, web_session>::iterator iter = julia_session_ptr->web_session_map.begin(); iter != julia_session_ptr->web_session_map.end(); iter++)
                    iter->second.outbox.push_back(msg);
            }
            if (msg.type == MSG_OUTPUT_EVAL_INCOMPLETE) {
                for (map<string, web_session>::iterator iter = julia_session_ptr->web_session_map.begin(); iter != julia_session_ptr->web_session_map.end(); iter++) {
                    if (iter->first == msg.args[0])
                        iter->second.outbox.push_back(msg);
                }
            }
        }
    }

    done:
    return;
}

void cleanup_web_sessions(julia_session *session, time_t t);
void cleanup_session(julia_session *session);

void connected(uv_connect_t* req, int status)
{
    julia_session *julia_session_ptr=(julia_session*)req->handle->data;
    if(status == -1) {
        std::cerr << "An error occured during the connection: " << uv_last_error(uv_default_loop()).code;
        cleanup_web_sessions(julia_session_ptr,0); //close all web sessions
        cleanup_session(julia_session_ptr);
    } else {
        // switch to normal operation
        julia_session_ptr->status = SESSION_NORMAL;

        // send a ready message
        message ready_message;
        ready_message.type = MSG_OUTPUT_READY;
        julia_session_ptr->outbox_history.push_back(ready_message);
        for (map<string, web_session>::iterator iter = julia_session_ptr->web_session_map.begin(); iter != julia_session_ptr->web_session_map.end(); iter++)
            iter->second.outbox.push_back(ready_message);
#ifdef JULIA_DEBUG_TRACE
        cout<<"Julia Process Connected\n";
#endif
        uv_read_start((uv_stream_t*)julia_session_ptr->sock,&alloc_buf,readSocketData);
    }
    delete req;
}


#define JL_TCP_LOOP uv_default_loop()

//read from julia (on stdin)
void julia_incoming(uv_stream_t* stream, ssize_t nread, uv_buf_t buf)
{
#ifdef JULIA_DEBUG_TRACE
    cout<<"Recevied Data from Julia on STDOUT:\n";
    cout<<std::string(buf.base,buf.len)<<"\n";
#endif

    julia_session *julia_session_ptr=(julia_session*)stream->data;

    if(nread==-1)
        return close_process(julia_session_ptr);

    // prepare for reading from julia
    uv_pipe_t *pipe = julia_session_ptr->julia_out;


    // send the outbox data to the client
    if (julia_session_ptr->status == SESSION_NORMAL)
    {
        // just dump the output into the julia_session
        julia_session_ptr->outbox_std.append(buf.base,nread);
    }

    // get the port number
    if (julia_session_ptr->status == SESSION_WAITING_FOR_PORT_NUM)
    {
        if(stream!=(uv_stream_t*)julia_session_ptr->julia_out) //allow debugging tools to print to stderr before startup
            return;
        julia_session_ptr->portbuf.append(buf.base,nread);
        // wait for a newline
        size_t newline_pos = julia_session_ptr->portbuf.find("\n");
        if (newline_pos == string::npos)
        {
            return;
        }

        // read the port number
        string num_string = julia_session_ptr->portbuf.substr(0, newline_pos);
        julia_session_ptr->portbuf = julia_session_ptr->portbuf.substr(newline_pos+1, julia_session_ptr->portbuf.size()-(newline_pos+1));
        int port_num = from_string<int>(num_string);

        // start
        julia_session_ptr->sock = new uv_tcp_t;
        uv_tcp_init(JL_TCP_LOOP,julia_session_ptr->sock);
        uv_connect_t *c=new uv_connect_t;
        sockaddr_in address=uv_ip4_addr("127.0.0.1", port_num);
        julia_session_ptr->sock->data=julia_session_ptr;
        uv_tcp_connect(c, julia_session_ptr->sock,address,&connected);

        cout << "Port number received. Connecting ...\n";
    }


    delete[] buf.base;
}

/////////////////////////////////////////////////////////////////////////////
// THREAD:  watchdog_thread
/////////////////////////////////////////////////////////////////////////////

// the watchdog runs regularly according to this interval
const int WATCHDOG_INTERVAL = 10000; // every second

// this is defined below but we need it here too
std::string create_julia_session(bool idle);


void pipes_done(uv_handle_t *pipe)
{
    julia_session *julia_session_ptr = ((julia_session*)pipe->data);
    if((uv_pipe_t*)pipe==julia_session_ptr->julia_in) julia_session_ptr->julia_in=0;
    else if((uv_pipe_t*)pipe==julia_session_ptr->julia_out) julia_session_ptr->julia_out=0;
    else if((uv_pipe_t*)pipe==julia_session_ptr->julia_err) julia_session_ptr->julia_err=0;
    delete (uv_pipe_t*)pipe;
}

void process_exited(uv_handle_t*p)
{
    julia_session *julia_session_ptr = (julia_session*)p->data;
    delete (uv_process_t*)p;
        julia_session_ptr->proc = 0;
    if(julia_session_ptr->status != SESSION_KILLED) {
        julia_session_ptr->status = SESSION_TERMINATING;
        close_process(julia_session_ptr);
    }
    cout<<"Process Exited\n";
}

void process_exited2(uv_process_t*p, int exit_status, int term_signal)
{
    process_exited((uv_handle_t*)p);
}

void close_process(julia_session *julia_session_ptr)
{
    if(julia_session_ptr->status==SESSION_KILLED)
        return;

    julia_session_ptr->status = SESSION_KILLED;

    // close the pipes
    if(julia_session_ptr->julia_in != 0)
        uv_close((uv_handle_t*)julia_session_ptr->julia_in,&pipes_done);
    if(julia_session_ptr->julia_out != 0)
        uv_close((uv_handle_t*)julia_session_ptr->julia_out,&pipes_done);
    if(julia_session_ptr->julia_err != 0)
        uv_close((uv_handle_t*)julia_session_ptr->julia_err,&pipes_done);
    if(julia_session_ptr->sock != 0)
        uv_close((uv_handle_t*)julia_session_ptr->sock,&socketClosed);

    if(julia_session_ptr->proc != 0) {
        // kill the julia process
        uv_process_kill(julia_session_ptr->proc, 9);
        uv_close((uv_handle_t*)julia_session_ptr->proc,&process_exited);
    }
}

void cleanup_web_sessions(julia_session *session, time_t t)
{
    vector<string> web_session_zombies;
    for (map<string, web_session>::iterator iter = session->web_session_map.begin(); iter != session->web_session_map.end(); iter++)
    {
        if ((t == 0) || (t-(iter->second).update_time >= WEB_SESSION_TIMEOUT))
            web_session_zombies.push_back(iter->first);
    }
    for (size_t j = 0; j < web_session_zombies.size(); j++)
    {
        cout<<"User \""<<session->web_session_map[web_session_zombies[j]].user_name<<"\" has left session \""<<session->session_name<<"\".\n";
        session->web_session_map.erase(web_session_zombies[j]);
    }
}

void cleanup_session(julia_session *session)
{
    if(session->status==SESSION_NORMAL)
        session->status = SESSION_TERMINATING;
    close_process(session);
}

void watchdog(uv_timer_t* handle, int status)
{
    // get the current time
    time_t t = time(0);

    // delete old web sessions and mark julia sessions with no web sessions as terminating
    for (size_t i = 0; i < julia_session_list.size(); i++)
    {
        julia_session* julia_session_ptr = julia_session_list[i];
        cleanup_web_sessions(julia_session_ptr,t);
        if (julia_session_ptr->web_session_map.empty()) {
            cleanup_session(julia_session_ptr);
        }
    }

    // remove all sessions that have successfully closed
    for (size_t i = 0; i < julia_session_list.size(); i++)
    {
        julia_session* julia_session_ptr = julia_session_list[i];
        if(julia_session_ptr->status != SESSION_TERMINATING && julia_session_ptr->status != SESSION_KILLED)
            continue;
        if(julia_session_ptr->proc==0&&julia_session_ptr->julia_in==0&&julia_session_ptr->julia_out==0&&julia_session_ptr->julia_err==0) {
            // print a message
            cout<<"Session \""<<julia_session_ptr->session_name<<"\" is terminating because it has no more users.\n";

            // remove the session from the map
            delete julia_session_ptr;
            julia_session_list.erase(julia_session_list.begin()+i);

            // print the number of open sessions
            if (julia_session_list.size() == 1)
                cout<<julia_session_list.size()<<" open session.\n";
            else
                cout<<julia_session_list.size()<<" open sessions.\n";
        }
    }

}


/////////////////////////////////////////////////////////////////////////////
// THREAD:  main_thread
/////////////////////////////////////////////////////////////////////////////

// the maximum number of concurrent julia_sessions
const size_t MAX_CONCURRENT_SESSIONS = 16;

// give julia this much time to respond to messages
const int JULIA_TIMEOUT = 500; // in milliseconds

// give julia this much time to respond to messages
const int JULIA_TIMEOUT_INTERVAL = 10000; // in nanoseconds

// generate a session token
string make_session_token() {
    // add a random integer to a prefix
    return "SESSION_"+to_string(rand());
}

// format a web response with the appropriate header
string respond(string session_token, string body) {
    std::ostringstream header;
    header << "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\nCache-control: no-cache\r\n";
    header << "Set-Cookie: SESSION_TOKEN=" << session_token << "\r\n";
    header << "Content-Length: " << body.size() << "\r\n";
    header << "\r\n";
    header << body;
    return header.str();
}


#define READ_STDERR
#ifdef READ_STDERR
#define STDIO_COUNT 3
#else
#define STDIO_COUNT 2
#endif

// get a session with a particular name (creating it if it does not exist) and return a session token
// returns the empty string if a new session could not be created
string get_session(string user_name, string session_name) {
    // generate a session token
    string session_token = make_session_token();

    // check if the julia session exists
    for (size_t i = 0; i < julia_session_list.size(); i++)
    {
        // look for the right session name
        if (julia_session_list[i]->session_name == session_name && session_name != "")
        {
            // create a user for this session
            web_session ws;
            ws.update_time = time(0);
            ws.user_name = user_name;
            message welcome_message;
            welcome_message.type = MSG_OUTPUT_WELCOME;
            ws.outbox.push_back(welcome_message);
            julia_session_list[i]->web_session_map[session_token] = ws;

            // print a message
            cout<<"New user \""<<user_name<<"\".\n";
            cout<<"User \""<<user_name<<"\" has joined session \""<<session_name<<"\".\n";
            return session_token;
        }
    }

    // check if we've reached max capacity
    if (julia_session_list.size() >= MAX_CONCURRENT_SESSIONS)
        return "";

    // create the session
    julia_session* session_data = new julia_session;

    session_data->sock = NULL;

    // generate the web session
    web_session ws;
    ws.update_time = time(0);
    ws.user_name = user_name;
    message welcome_message;
    welcome_message.type = MSG_OUTPUT_WELCOME;
    ws.outbox.push_back(welcome_message);
    session_data->web_session_map[session_token] = ws;

    // session name
    session_data->session_name = session_name;

    // keep the julia_session alive for now
    //session_data->inbox_thread_alive = true;
    //session_data->outbox_thread_alive = true;
    session_data->should_terminate = false;
    session_data->status = SESSION_WAITING_FOR_PORT_NUM;

    session_data->proc=new uv_process_t;

//-------------- SPAWN ------------------------------//
    //------------- ALLOCATE PIPES-------------------//
    session_data->julia_in = new uv_pipe_t;
    session_data->julia_out = new uv_pipe_t;
#ifdef READ_STDERR
    session_data->julia_err = new uv_pipe_t;
#else
    session_data->julia_err = NULL;
#endif
    session_data->julia_in->data = session_data->julia_out->data =
#ifdef READ_STDERR
            session_data->julia_err->data =
#endif
            session_data;
    uv_pipe_t child_pipes[3];

    //------------- SETUP PIPES---------------------//

    uv_pipe_init(uv_default_loop(), session_data->julia_in, UV_PIPE_WRITEABLE);
    uv_pipe_init(uv_default_loop(), session_data->julia_out, UV_PIPE_READABLE);
#ifdef READ_STDERR
    uv_pipe_init(uv_default_loop(), session_data->julia_err, UV_PIPE_READABLE);
#endif
    uv_pipe_init(uv_default_loop(), &child_pipes[0], UV_PIPE_SPAWN_SAFE|UV_PIPE_READABLE);
    uv_pipe_init(uv_default_loop(), &child_pipes[1], UV_PIPE_SPAWN_SAFE|UV_PIPE_WRITEABLE);
#ifdef READ_STDERR
    uv_pipe_init(uv_default_loop(), &child_pipes[2], UV_PIPE_SPAWN_SAFE|UV_PIPE_WRITEABLE);
#endif

    uv_pipe_link(&child_pipes[0],session_data->julia_in);
    uv_pipe_link(session_data->julia_out,&child_pipes[1]);
#ifdef READ_STDERR
    uv_pipe_link(session_data->julia_err,&child_pipes[2]);
#endif
    //-------------- SETUP PROCESS OPTIONS ---------//
    uv_process_options_t opts;
    int r;

    //------------------ STDIO ---------------------//
    uv_stdio_container_t stdio[STDIO_COUNT];
    stdio[0].type = UV_STREAM;
    stdio[1].type = UV_STREAM;
    stdio[0].data.stream = (uv_stream_t*)&child_pipes[0];
    stdio[1].data.stream = (uv_stream_t*)&child_pipes[1];
    stdio[2].type = UV_STREAM;
#ifdef READ_STDERR
    stdio[2].data.stream = (uv_stream_t*)&child_pipes[2];
#endif
    opts.stdio_count = STDIO_COUNT;
    opts.stdio = stdio;

    //------------------ ARGUMENTS ----------------//
#if 0
    char *argv[5] = {"gdbserver","localhost:2222","./julia-debug-readline", "../share/julia/extras/julia_web_base.jl", NULL};
#else
    char arg0[]="./julia-release-readline";
    char arg1[]="--no-history";
    char arg2[]="../share/julia/extras/julia_web_base.jl";
    char *argv[4]={arg0,arg1,arg2,NULL};
#endif
    opts.exit_cb=&process_exited2;
    opts.cwd=NULL; //cwd
#ifndef __WIN32__
    opts.env=environ;
    #else
    opts.env=NULL;
    #endif
    opts.args=argv;
    opts.file=argv[0];
    opts.flags=0;


    int err = uv_spawn(uv_default_loop(),session_data->proc,opts);

    //------------------ CLEAN UP ---------------//
    uv_pipe_close_sync(&child_pipes[0]);
    uv_pipe_close_sync(&child_pipes[1]);
#ifdef READ_STDERR
    uv_pipe_close_sync(&child_pipes[2]);
#endif

    if(err!=0)
        return "";
    session_data->data_notifier = new uv_async_t;
    session_data->data_notifier->data = session_data;
    uv_async_init(uv_default_loop(),session_data->data_notifier,read_client);

    session_data->proc->data = session_data;

    //start reading
    cout << "Started Reading from Julia stdout";
    uv_read_start((uv_stream_t*)session_data->julia_out,&alloc_buf,&julia_incoming);
#ifdef READ_STDERR
    uv_read_start((uv_stream_t*)session_data->julia_err,&alloc_buf,&julia_incoming);
#endif
    
    /*
    // start the inbox thread
    if (pthread_create(&session_data->inbox_proc, 0, inbox_thread, (void*)session_data))
        session_data->inbox_proc = 0;

    // start the outbox thread
    if (pthread_create(&session_data->outbox_proc, 0, outbox_thread, (void*)session_data))
        session_data->outbox_proc = 0; */

    // store the session
    julia_session_list.push_back(session_data);

    // print a message
    cout<<"New user \""<<user_name<<"\".\n";
    if (session_name == "")
        cout<<"User \""<<user_name<<"\" has started a new private session.\n";
    else
        cout<<"User \""<<user_name<<"\" has started session \""<<session_name<<"\".\n";

    // print the number of open sessions
    if (julia_session_list.size() == 1)
        cout<<"1 open session.\n";
    else
        cout<<julia_session_list.size()<<" open sessions.\n";

    // return the session token
    return session_token;
}

void requestDone(uv_handle_t *handle)
{
#ifdef CPP_DEBUG_TRACE
    cout << "Request Done\n";
#endif
    delete (reading_in_progress*)handle->data;
    delete (uv_tcp_t*)(handle);
}

void close_stream(uv_shutdown_t* req, int status)
{
    uv_close((uv_handle_t*)req->handle,&requestDone);
	delete req;
#ifdef CPP_DEBUG_TRACE
    cout << "Closing connection "
#ifdef __WIN32__
<<WSAGetLastError()
#endif
<<"\n";
#endif
}

// this function is called when an HTTP request is made - the response is the return value
void get_response(request* req,uv_stream_t *client)
{
    // the julia_session token
    string session_token;
    
    // check for the session cookie
    if (req->get_cookie_exists("SESSION_TOKEN"))
        session_token = req->get_cookie_value("SESSION_TOKEN");

#ifdef CPP_DEBUG_TRACE
    cout << "Request from user " << session_token << "\n";
#endif

    // get the julia session if there is one
    julia_session* julia_session_ptr = 0;
    if (session_token != "")
    {
        for (size_t i = 0; i < julia_session_list.size(); i++)
        {
            if (julia_session_list[i]->web_session_map.find(session_token) != julia_session_list[i]->web_session_map.end())
            {
                // store this session
                julia_session_ptr = julia_session_list[i];
                break;
            }
        }
        if (julia_session_ptr == 0)
            session_token = "";
    }

    // the response
    vector <message> response_messages;

    // whether we are waiting for an eval
    bool waiting_for_eval = false;

    // process input if there is any
    if (req->get_field_exists("request"))
    {
        // parse the request
        Json::Value request_root;
        Json::Reader reader;
        if (reader.parse(req->get_field_value("request"), request_root))
        {
            // iterate through the messages
            for (int i = 0; i < request_root.size(); i++)
            {
                // make sure the message has at least a type
                if (request_root[i].size() > 0)
                {
                    // extract the message from the request
                    message request_message;
                    request_message.type = request_root[i][0].asInt();
                    for (int j = 1; j < request_root[i].size(); j++)
                        request_message.args.push_back(request_root[i][j].asString());

                    // MSG_INPUT_START
                    if (request_message.type == MSG_INPUT_START)
                    {
                        // make sure the message is well-formed
                        if (request_message.args.size() >= 2)
                        {
                            // get the user name and session name
                            string user_name = request_message.args[0];
                            if (user_name == "")
                                user_name = "julia";
                            string session_name = request_message.args[1];

                            // try to create/join a new session
                            session_token = get_session(user_name, session_name);
                            if (session_token == "")
                            {
                                // too many sessions
                                message msg;
                                msg.type = MSG_OUTPUT_FATAL_ERROR;
                                msg.args.push_back("the server is currently at maximum capacity");
                                response_messages.push_back(msg);
                            }

                            // get the new session pointer
                            julia_session_ptr = 0;
                            for (size_t j = 0; j < julia_session_list.size(); j++)
                            {
                                if (julia_session_list[j]->web_session_map.find(session_token) != julia_session_list[j]->web_session_map.end())
                                {
                                    // store this session
                                    julia_session_ptr = julia_session_list[j];
                                    break;
                                }
                            }
                        }

                        // don't send this message to julia
                        continue;
                    }

                    // MSG_INPUT_POLL
                    if (request_message.type == MSG_INPUT_POLL)
                    {
                        if (julia_session_ptr == 0)
                        {
                            // if not, send an error message
                            message msg;
                            msg.type = MSG_OUTPUT_FATAL_ERROR;
                            msg.args.push_back("session expired");
                            response_messages.push_back(msg);
                        }

                        // don't send this message to julia
                        continue;
                    }

                    // MSG_INPUT_EVAL
                    if (request_message.type == MSG_INPUT_EVAL)
                    {
                        // make sure we have a valid session
                        if (session_token == "")
                        {
                            // if not, send an error message
                            message msg;
                            msg.type = MSG_OUTPUT_FATAL_ERROR;
                            msg.args.push_back("session expired");
                            response_messages.push_back(msg);
                        }
                        else
                        {
                            // forward the message to this julia session
                            julia_session_ptr->inbox.push_back(request_message);
                            uv_async_send(julia_session_ptr->data_notifier);
                        }
                    }
                    
                                        // MSG_INPUT_REPLAY_HISTORY
                    if (request_message.type == MSG_INPUT_REPLAY_HISTORY)
                    {
                        if (julia_session_ptr == 0)
                        {
                            // if not, send an error message
                            message msg;
                            msg.type = MSG_OUTPUT_FATAL_ERROR;
                            msg.args.push_back("session expired");
                            response_messages.push_back(msg);
                        }
                        else
                        {
                            // send the entire outbox history to the client
                            for (size_t i = 0; i < julia_session_ptr->outbox_history.size(); i++)
                                response_messages.push_back(julia_session_ptr->outbox_history[i]);

                            // the outbox history includes messages that were already going to be sent, so delete those
                            julia_session_ptr->web_session_map[session_token].outbox.clear();
                        }

                        // don't send this message to julia
                        continue;
                    }

                    // MSG_INPUT_GET_USER
                    if (request_message.type == MSG_INPUT_GET_USER)
                    {
                        if (julia_session_ptr == 0)
                        {
                            // if not, send an error message
                            message msg;
                            msg.type = MSG_OUTPUT_FATAL_ERROR;
                            msg.args.push_back("session expired");
                            response_messages.push_back(msg);
                        }
                        else
                        {
                            // send back the user name
                            message msg;
                            msg.type = MSG_OUTPUT_GET_USER;
                            msg.args.push_back(julia_session_ptr->web_session_map[session_token].user_name);
                            msg.args.push_back(session_token);
                            response_messages.push_back(msg);
                        }

                        // don't send this message to julia
                        continue;
                    }

                    // bad message, just ignore it
                }
            }
        }
    }

    // perform maintenance on the session if there is one
    if (julia_session_ptr != 0)
    {
        // pet the watchdog
        julia_session_ptr->web_session_map[session_token].update_time = time(0);

        // catch any extra output from julia during normal operation
        if (julia_session_ptr->outbox_std != "" && julia_session_ptr->status == SESSION_NORMAL)
        {
            // construct the output message
            message output_message;
            output_message.type = MSG_OUTPUT_OTHER;
            output_message.args.push_back(julia_session_ptr->outbox_std);

            // eat the outbox contents
            julia_session_ptr->outbox_std = "";

            // add the message to the outbox queue of all the users of this julia session
            julia_session_ptr->outbox_history.push_back(output_message);
            for (map<string, web_session>::iterator iter = julia_session_ptr->web_session_map.begin(); iter != julia_session_ptr->web_session_map.end(); iter++)
                iter->second.outbox.push_back(output_message);
        }
        
        // get any output messages from julia
        for (size_t i = 0; i < julia_session_ptr->web_session_map[session_token].outbox.size(); i++)
            response_messages.push_back(julia_session_ptr->web_session_map[session_token].outbox[i]);
        julia_session_ptr->web_session_map[session_token].outbox.clear();
    }

    // convert the message to json
    Json::Value response_root(Json::arrayValue);
    for (size_t i = 0; i < response_messages.size(); i++)
    {
        Json::Value message_root(Json::arrayValue);
        message_root.append(response_messages[i].type);
        for (size_t j = 0; j < response_messages[i].args.size(); j++)
            message_root.append(response_messages[i].args[j]);
#ifdef CPP_DEBUG_TRACE
        cout<<"Sending message "<<(int)response_messages[i].type<<" to user "<<session_token<<"\n"; 
#endif
        response_root.append(message_root);
    }

    // return the header and response
    Json::StyledWriter writer;
    string response =  respond(session_token, writer.write(response_root));

    reading_in_progress *p = (reading_in_progress *)client->data;
    p->cstr = new char [response.size()];
#ifdef VERY_VERBOSE
    cout<<response;
    ofstream log;
    log.open("raw.log", ios::app | ios::out | ios::binary);
    log<<response;
    log.close();
#endif
    memcpy (p->cstr, response.data(),response.size());
    // write the response
    uv_buf_t buf;
    buf.base=p->cstr;
    buf.len=response.size();
    uv_write_t *wr = new uv_write_t;
    wr->data=(void*)buf.base;
    uv_write(wr,(uv_stream_t*)client,&buf,1,&free_write_buffer);

    // destroySoon the connection to the client
    uv_shutdown_t *sr = new uv_shutdown_t;
    uv_shutdown(sr, (uv_stream_t*)client, &close_stream);
}


// CTRL+C signal handler
#if defined(__WIN32__)
BOOL WINAPI sigint_handler(DWORD wsig) { //This needs winapi types to guarantee __stdcall
#else
void sigint_handler(int sig, siginfo_t *info, void *context) {
#endif
    // print a message
    cout<<"cleaning up...\nexiting...\n";
#if defined(__WIN32__)
    cout<<"\nImportant: Please answer N to the following question:\n";
#endif
    
    // clean up
    for (size_t i = 0; i < julia_session_list.size(); i++)
    {
        // close the pipes
        uv_close((uv_handle_t*)(julia_session_list[i]->julia_in),&pipes_done);
        uv_close((uv_handle_t*)(julia_session_list[i]->julia_out),&pipes_done);
        uv_close((uv_handle_t*)(julia_session_list[i]->julia_err),&pipes_done);

        // kill the julia process
        uv_process_kill(julia_session_list[i]->proc, 9);
        //run_event_loop(julia_session_list[i]);

        //uv_loop_delete((julia_session_list[i])->event_loop);
        // delete the session
        delete julia_session_list[i];
    }

    // terminate
    exit(0);
}

uv_tty_t *stdin_handle;
static void jl_install_sigint_handler() {
#ifdef __WIN32__
    SetConsoleCtrlHandler(NULL,0); //turn on ctrl-c handler
    SetConsoleCtrlHandler((PHANDLER_ROUTINE)sigint_handler,1);
#else
    struct sigaction act;
    memset(&act, 0, sizeof(struct sigaction));
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = sigint_handler;
    act.sa_flags = SA_SIGINFO;
    if (sigaction(SIGINT, &act, NULL) < 0) {
        fprintf(stderr, "sigaction: %s\n", strerror(errno));
        exit(1);
    }
#endif
}

// program entrypoint
int main(int argc, char* argv[])
{
    // set the Ctrl+C handler
    jl_install_sigint_handler();

    // get the command line arguments
    int port_num = 1441;
    for (int i = 1; i < argc-1; i++)
    {
        if (string(argv[i]) == "-p")
            port_num = from_string<int>(argv[i+1]);
    }

    // seed the random number generator for generating julia_session tokens
    srand(time(0));

#ifndef __WIN32__
    // ignore the SIGPIPE signal (when julia crashes or exits, we don't want to die too)
    struct sigaction act;
    act.sa_flags = 0;
    act.sa_handler = SIG_IGN;
    sigemptyset(&act.sa_mask);
    sigaction(SIGPIPE, &act, NULL);
#endif

    uv_timer_t wd;
    uv_timer_init(uv_default_loop(),&wd);
    uv_timer_start(&wd,&watchdog,WATCHDOG_INTERVAL,WATCHDOG_INTERVAL);

    // print a welcome message
    cout<<"SCGI server started on port "<<port_num<<".\n";

    // print the number of open julia_sessions
    cout<<"0 open sessions.\n";

    // start the server
    run_server(port_num, &get_response);

    // never reached
    return 0;
}
