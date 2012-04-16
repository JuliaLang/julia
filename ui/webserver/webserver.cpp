#include <iostream>
#include <map>
#include <ctime>
#include <stdlib.h>
#include <cstring>
#include <unistd.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include "server.h"
#include "json.h"
#include "message_types.h"
#ifndef __WIN32__
#include <sys/wait.h>
#endif

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
// sessions
/////////////////////////////////////////////////////////////////////////////

// if a session hasn't been queried in this time, it dies
const int SESSION_TIMEOUT = 20; // in seconds

enum session_status
{
    SESSION_WAITING_FOR_PORT_NUM,
    SESSION_NORMAL,
    SESSION_TERMINATING,
};

// a session
struct session
{
    // time since watchdog was last "pet"
    time_t update_time;

    // whether this is an "idle" session
    bool is_idle;

    // to be sent to julia
    string inbox_std;

    // to be sent to julia
    vector<message> inbox;

    // to be sent to the client as MSG_OTHER_OUTPUT
    string outbox_std;

    // to be converted into messages
    string outbox_raw;

    // to be sent to the client
    vector<message> outbox;

    //pipes
    uv_process_t *proc;
    uv_pipe_t *julia_in;
    uv_pipe_t *julia_out;

    // the socket for communicating to julia
    uv_tcp_t *sock;

    // io threads
    pthread_t thread;

    // when both threads have terminated, we can kill the session
    bool inbox_thread_alive;
    bool outbox_thread_alive;

    // whether the session should terminate
    bool should_terminate;

    // the status of the session
    session_status status;

    //event loop
    uv_loop_t *event_loop;

    //notifier
    uv_async_t *data_notifier;
};

// a list of sessions
map<string, session> session_map;

// a mutex for accessing session_map
pthread_mutex_t session_mutex;

/////////////////////////////////////////////////////////////////////////////
// THREAD:  inbox_thread (from browser to julia)
/////////////////////////////////////////////////////////////////////////////

// add to the inbox regularly according to this interval
const int INBOX_INTERVAL = 10000; // in nanoseconds


void closeInput(uv_stream_t *handle,int status)
{
    clientData *data = (clientData *)handle->data;
    string session_token=data->session_token;

    // lock the mutex
    pthread_mutex_lock(&session_mutex);

    // tell the watchdog that this thread is done
    session_map[session_token].inbox_thread_alive = false;

    // unlock the mutex
    pthread_mutex_unlock(&session_mutex);
}

void free_socket_write_buffer(uv_write_t* req, int status)
{
    vector<string>* tmp = (vector<string>*) req->data;
    delete tmp;
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
    // lock the mutex
    pthread_mutex_lock(&session_mutex);

    clientData *data = (clientData *)handle->data;
    string session_token=data->session_token;

    // terminate if necessary
    if (session_map[session_token].status == SESSION_TERMINATING)
    {
        // unlock the mutex
        pthread_mutex_unlock(&session_mutex);

        // terminate
        uv_close((uv_handle_t*)handle,(uv_close_cb)&closeInput);

        return;
    } else if(session_map[session_token].status != SESSION_NORMAL)
        return;

    // get the inbox data
    string inbox = session_map[session_token].inbox_std;

    // if there is no inbox data and no messages to send, or if julia isn't ready, wait and try again
    if ((inbox == "" && session_map[session_token].inbox.empty()) || session_map[session_token].status != SESSION_NORMAL)
    {
        // unlock the mutex
        pthread_mutex_unlock(&session_mutex);

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
        uv_write(req,(uv_stream_t*)session_map[session_token].julia_in,&buffer,1,&free_write_buffer);
    }

    // write messages to julia
    for (size_t i = 0; i < session_map[session_token].inbox.size(); i++)
    {
        // get the message
        message msg = session_map[session_token].inbox[i];

        uv_buf_t bufs[2*(1+msg.args.size())];
        uv_write_t *req2 = new uv_write_s;

        vector<string> *tmp = new vector<string>(msg.args);
        req2->data=(void*)tmp;

        //write the message type
        string t=" ";
        t[0]=msg.type;
        bufs[0].base=t.data();
        bufs[0].len=1;

        tmp->push_back(t);

        string t2=" ";
        t2[0]=msg.args.size();
        // write the number of arguments
        bufs[1].base=t2.data();
        bufs[1].len=1;

        tmp->push_back(t2);


        if(msg.args.size()>0) {

            // iterate through the arguments
            for (size_t j = 0; j < msg.args.size(); j++)
            {
                // write the size of the argument
                string str_arg_size = "    ";
                *((uint32_t*)(&(str_arg_size[0]))) = (uint32_t)msg.args[j].size();
                tmp->push_back(str_arg_size);
                bufs[2*(j+1)].base=str_arg_size.data();
                bufs[2*(j+1)].len=str_arg_size.size();
                //assert(str_arg_size.size()==4);

                bufs[2*(j+1)+1].base=msg.args[j].data();
                bufs[2*(j+1)+1].len=msg.args[j].size();
            }
        }

        uv_write(req2,(uv_stream_t*)session_map[session_token].sock,bufs,2*(1+msg.args.size()),&free_socket_write_buffer);
    }
    session_map[session_token].inbox.clear();

    // remove the written data from the inbox
    session_map[session_token].inbox_std.clear();

    // unlock the mutex
    pthread_mutex_unlock(&session_mutex);
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

void close_session(uv_handle_t *stream)
{
    clientData *data = (clientData *)stream->data;
    string session_token=data->session_token;
    // lock the mutex
    pthread_mutex_lock(&session_mutex);

    // tell the watchdog that this thread is done
    session_map[session_token].outbox_thread_alive = false;

    // release the socket
    uv_close((uv_handle_t*)session_map[session_token].sock,&socketClosed);

    // unlock the mutex
    pthread_mutex_unlock(&session_mutex);
}

//read from julia on metadata socket (4444)
void readSocketData(uv_stream_t *sock,ssize_t nread, uv_buf_t buf)
{
    // lock the mutex
    pthread_mutex_lock(&session_mutex);

    clientData *data = (clientData* )sock->data;
    string session_token=data->session_token;
    if(session_map[session_token].status != SESSION_NORMAL)
        return;

    session_map[session_token].outbox_raw.append(buf.base,nread);

    // try to convert the raw outbox data into messages
    string outbox_raw = session_map[session_token].outbox_raw;
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
            outbox_raw = session_map[session_token].outbox_raw = outbox_raw.substr(pos, outbox_raw.size()-pos);

            // add the message to the queue
            session_map[session_token].outbox.push_back(msg);
        }
    }

    done:

    // unlock the mutex
    pthread_mutex_unlock(&session_mutex);
}


void connected(uv_connect_t* req, int status)
{
    clientData *data = (clientData* )req->handle->data;
    string session_token=data->session_token;
    // switch to normal operation
    session_map[session_token].status = SESSION_NORMAL;

    // send a ready message
    message ready_message;
    ready_message.type = MSG_OUTPUT_READY;
    session_map[session_token].outbox.push_back(ready_message);

    uv_read_start((uv_stream_t*)session_map[session_token].sock,&alloc_buf,readSocketData);

}

//read from julia (on stdin)
void julia_incoming(uv_stream_t* stream, ssize_t nread, uv_buf_t buf)
{
    clientData *data = (clientData *)stream->data;
    string session_token=data->session_token;
    // lock the mutex
    pthread_mutex_lock(&session_mutex);

    // terminate if necessary
    if (session_map[session_token].status == SESSION_TERMINATING)
    {
        // unlock the mutex
        pthread_mutex_unlock(&session_mutex);

        // terminate
        uv_close((uv_handle_t*)stream,&close_session);
    }

    // prepare for reading from julia
    uv_pipe_t *pipe = session_map[session_token].julia_out;


    // send the outbox data to the client
    if (session_map[session_token].status == SESSION_NORMAL)
    {
        // just dump the output into the session
        session_map[session_token].outbox_std.append(buf.base,buf.len);
    }

    // get the port number
    if (session_map[session_token].status == SESSION_WAITING_FOR_PORT_NUM)
    {
        data->buf+=buf.base;
        // wait for a newline
        size_t newline_pos = data->buf.find("\n");
        if (newline_pos == string::npos)
        {
            // unlock the mutex
            pthread_mutex_unlock(&session_mutex);
            return;
        }

        // read the port number
        string num_string = data->buf.substr(0, newline_pos);
        data->buf = data->buf.substr(newline_pos+1, data->buf.size()-(newline_pos+1));
        int port_num = from_string<int>(num_string);

        // start
        session_map[session_token].sock = new uv_tcp_t;
        uv_tcp_init(session_map[session_token].event_loop,session_map[session_token].sock);
        uv_connect_t *c=new uv_connect_t;
        sockaddr_in address=uv_ip4_addr("127.0.0.1", port_num);
        session_map[session_token].sock->data=data;
        uv_tcp_connect(c, session_map[session_token].sock,address,&connected);
    }

    delete[] buf.base;

    // unlock the mutex
    pthread_mutex_unlock(&session_mutex);
}

/////////////////////////////////////////////////////////////////////////////
// THREAD:  watchdog_thread
/////////////////////////////////////////////////////////////////////////////

// the watchdog runs regularly according to this interval
const int WATCHDOG_INTERVAL = 100000000; // in nanoseconds

// this is defined below but we need it here too
std::string create_session(bool idle);


void pipes_done(uv_handle_t *pipe)
{
    delete (uv_pipe_t*)pipe;
}

void watchdog(uv_timer_t* handle, int status)
{
    // lock the mutex
    pthread_mutex_lock(&session_mutex);

    // get the current time
    time_t t = time(0);

    // start terminating old sessions
    for (map<string, session>::iterator iter = session_map.begin(); iter != session_map.end(); iter++)
    {
        if ((iter->second).status == SESSION_NORMAL && !(iter->second).is_idle)
        {
            if (t-(iter->second).update_time >= SESSION_TIMEOUT || (iter->second).should_terminate)
                (iter->second).status = SESSION_TERMINATING;
        }
    }

    // get a list of zombie sessions
    vector<string> zombie_list;
    for (map<string, session>::iterator iter = session_map.begin(); iter != session_map.end(); iter++)
    {
        if (!(iter->second).inbox_thread_alive && !(iter->second).outbox_thread_alive)
            zombie_list.push_back(iter->first);
    }

    // kill the zombies
    for (vector<string>::iterator iter = zombie_list.begin(); iter != zombie_list.end(); iter++)
    {
        // thread will terminate automatically
        uv_loop_delete(session_map[*iter].event_loop);

        // close the pipes
        uv_close((uv_handle_t*)session_map[*iter].julia_in,&pipes_done);
        uv_close((uv_handle_t*)session_map[*iter].julia_out,&pipes_done);

        // kill the julia process
        uv_process_kill(session_map[*iter].proc, 9);
        // remove the session from the map
        session_map.erase(*iter);

        // print the number of open sessions
        if (session_map.size() == 1)
            cout<<session_map.size()<<" open session.\n";
        else
            cout<<session_map.size()<<" open sessions.\n";
    }

    // if nobody is using this node, spawn an idle session
    if (session_map.empty())
        create_session(true);

    // unlock the mutex
    pthread_mutex_unlock(&session_mutex);

}


/////////////////////////////////////////////////////////////////////////////
// THREAD:  main_thread
/////////////////////////////////////////////////////////////////////////////

// the maximum number of concurrent sessions
const size_t MAX_CONCURRENT_SESSIONS = 4;

// give julia this much time to respond to messages
const int JULIA_TIMEOUT = 500; // in milliseconds

// give julia this much time to respond to messages
const int JULIA_TIMEOUT_INTERVAL = 10000; // in nanoseconds

// generate a session token
string make_session_token()
{
    // add a random integer to a prefix
    return "SESSION_"+to_string(rand());
}

string respond(string session_token, string body)
{
    string header = "HTTP/1.1 200 OK\r\nContent-Type: application/json; charset=UTF-8\r\nSet-Cookie: SESSION_TOKEN=";
    header += session_token;
    header += "\r\n\r\n";
    return header+body;
}


void *run_event_loop(void *token)
{
    uv_loop_t *loop = session_map[(char *)token].event_loop;
    delete[] (char*)token;
    uv_run(loop);
    return 0;
}

void process_exited(uv_process_t*p, int exit_status, int term_signal)
{
    session_map[((clientData*)p->data)->session_token].status = SESSION_TERMINATING;
    cout<<"Process Exited";
}

// create a session and return a session token
// a new session can be "idle", which means it isn't yet matched with a browser session
// idle sessions don't expire
string create_session(bool idle)
{
    // check if we've reached max capacity
    if (session_map.size() >= MAX_CONCURRENT_SESSIONS)
        return "";

    // create the session
    session session_data;

    // generate a session token
    string session_token = make_session_token();

    // set the idleness of the session
    session_data.is_idle = idle;

    // keep the session alive for now
    session_data.inbox_thread_alive = true;
    session_data.outbox_thread_alive = true;
    session_data.should_terminate = false;
    session_data.status = SESSION_WAITING_FOR_PORT_NUM;

    session_data.julia_in=new uv_pipe_t;
    session_data.julia_out=new uv_pipe_t;
    session_data.proc=new uv_process_t;
    session_data.event_loop=uv_loop_new();

    uv_pipe_init(session_data.event_loop,session_data.julia_in,0);
    uv_pipe_init(session_data.event_loop,session_data.julia_out,0);

    uv_process_options_t opts;
    opts.stdin_stream = session_data.julia_in;
    opts.stdout_stream = session_data.julia_out;
#if 0
    char *argv[5] = {"gdbserver","localhost:2222","./julia-debug-readline", "ui/webserver/julia_web_base.jl", NULL};
#else
    char arg0[]="./julia-release-readline";
    char arg1[]="ui/webserver/julia_web_base.jl";
    char *argv[3]={arg0,arg1,NULL};
#endif
    opts.stderr_stream = 0; //parent stderr
    opts.exit_cb=&process_exited;
    opts.cwd=NULL; //cwd
    #ifndef __WIN32__
    opts.env=environ;
    #else
    opts.env=NULL;
    #endif
    opts.args=argv;
    opts.file=argv[0];
    uv_spawn(uv_default_loop(),session_data.proc,opts);

    clientData *data = new clientData;

    // set the start time
    session_data.update_time = time(0);

    session_data.data_notifier = new uv_async_t;
    session_data.data_notifier->data = (void*)data;
    uv_async_init(uv_default_loop(),session_data.data_notifier,read_client);

    // store the session
    session_map[session_token] = session_data;
    // start the event thread
    char* session_token_inbox = new char[session_token.length()+1];
    strcpy(session_token_inbox, session_token.c_str());

    data->session_token=session_token;
    session_data.proc->data = session_data.julia_in->data = session_data.julia_out->data = data;
    if (pthread_create(&session_data.thread, 0, &run_event_loop, (void*)session_token_inbox))
    {
        //session_data.thread = 0;
    }

    //start reading
    uv_read_start((uv_stream_t*)opts.stdout_stream,&alloc_buf,&julia_incoming);

    // print the number of open sessions
    if (session_map.size() == 1)
    {
        if (idle)
            cout<<"1 open session [idle].\n";
        else
            cout<<"1 open session.\n";
    }
    else {
        cout<<session_map.size()<<" open sessions.\n";
    }

    // return the session token
    return session_token;
}

void requestDone(uv_handle_t *handle)
{
    delete (reading_in_progress*)handle->data;
    delete (uv_tcp_t*)(handle);
}


// this function is called when an HTTP request is made - the response is the return value
void get_response(request* req,uv_stream_t *client)
{
    // lock the mutex
    pthread_mutex_lock(&session_mutex);

    // the session token
    string session_token="";

    // check for the session cookie
    if (req->get_cookie_exists("SESSION_TOKEN")) {
        session_token = req->get_cookie_value("SESSION_TOKEN").c_str(); //should stay valid
    }

    // check if the session is real
    if (session_token.length()>0)
    {
        if (session_map.find(session_token) == session_map.end())
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

                    // determine the type of request
                    bool request_recognized = false;

                    // MSG_INPUT_START
                    if (request_message.type == MSG_INPUT_START)
                    {
                        // we recognize the request
                        request_recognized = true;

                        // kill the old session if there is one
                        if (session_token != "")
                            session_map[session_token].should_terminate = true;

                        // look for an idle session to harvest
                        bool found_idle_session = false;
                        for (map<string, session>::iterator iter = session_map.begin(); iter != session_map.end(); iter++)
                        {
                            // check if the session is idle
                            if ((iter->second).is_idle)
                            {
                                // the session is no longer idle
                                session_token = iter->first;
                                (iter->second).update_time = time(0);
                                (iter->second).is_idle = false;
                                found_idle_session = true;
                                cout<<"1 open session.\n";
                                break;
                            }
                        }

                        // no extra idle sessions -- try to create a new session
                        if (!found_idle_session)
                        {
                            session_token = create_session(false);
                            if (session_token == "")
                            {
                                // too many sessions
                                message msg;
                                msg.type = MSG_OUTPUT_FATAL_ERROR;
                                msg.args.push_back("the server is currently at maximum capacity");
                                response_messages.push_back(msg);
                            }
                        }
                    }

                    // other messages go straight to julia
                    if (!request_recognized)
                    {
                        // make sure we have a valid session
                        if (session_token == "")
                        {
                            message msg;
                            msg.type = MSG_OUTPUT_FATAL_ERROR;
                            msg.args.push_back("session expired");
                            response_messages.push_back(msg);
                        }
                        else
                        {
                            // forward the message to julia
                            if (request_message.type != MSG_INPUT_POLL) {
                                session_map[session_token].inbox.push_back(request_message);
                                uv_async_send(session_map[session_token].data_notifier);
                            }

                            // check if this was an eval message
                            if (request_message.type == MSG_INPUT_EVAL)
                                waiting_for_eval = true;
                        }
                    }
                }
            }
        }
    }

    // if we asked julia for an eval, wait a little and see if julia responds
    if (waiting_for_eval)
    {
    //to be implemented using websockets
    /*
        // unlock the mutex
        pthread_mutex_unlock(&session_mutex);

        // try to get the response from julia (and time out if it takes too long)
        clock_t start_time = clock();
        while (clock()-start_time < JULIA_TIMEOUT*(CLOCKS_PER_SEC/1000))
        {
            // don't waste cpu time
            timespec timeout;
            timeout.tv_sec = 0;
            timeout.tv_nsec = JULIA_TIMEOUT_INTERVAL;
#ifndef __WIN32__
            nanosleep(&timeout, 0);
#endif

            // lock the mutex
            pthread_mutex_lock(&session_mutex);

            // iterate through the messages
            bool eval_done = false;
            for (size_t i = 0; i < session_map[session_token].outbox.size(); i++)
            {
                // check for parse errors
                if (session_map[session_token].outbox[i].type == MSG_OUTPUT_PARSE_ERROR)
                    eval_done = true;

                // check if we need more input to parse
                if (session_map[session_token].outbox[i].type == MSG_OUTPUT_PARSE_INCOMPLETE)
                    eval_done = true;

                // check if the eval is done
                if (session_map[session_token].outbox[i].type == MSG_OUTPUT_EVAL_RESULT)
                    eval_done = true;

                // check if an exception was thrown
                if (session_map[session_token].outbox[i].type == MSG_OUTPUT_EVAL_ERROR)
                    eval_done = true;
            }

            // check if there was a message from julia
            if (eval_done)
            {
                // unlock the mutex
                pthread_mutex_unlock(&session_mutex);

                // stop waiting
                break;
            }

            // unlock the mutex
            pthread_mutex_unlock(&session_mutex);
        }

        // lock the mutex
        pthread_mutex_lock(&session_mutex);*/
    }

    // perform maintenance on the session if there is one
    if (session_token != "")
    {
        // pet the watchdog
        session_map[session_token].update_time = time(0);

        // catch any extra output from julia during normal operation
        if (session_map[session_token].outbox_std != "" && session_map[session_token].status == SESSION_NORMAL)
        {
            message output_message;
            output_message.type = MSG_OUTPUT_OTHER;
            output_message.args.push_back(session_map[session_token].outbox_std);
            session_map[session_token].outbox_std = "";
            if (session_map[session_token].outbox.size() > 0)
            {
                if (session_map[session_token].outbox[session_map[session_token].outbox.size()-1].type == MSG_OUTPUT_OTHER)
                    session_map[session_token].outbox[session_map[session_token].outbox.size()-1].args[0] += session_map[session_token].outbox_std;
                else
                    session_map[session_token].outbox.push_back(output_message);
            }
            else
                session_map[session_token].outbox.push_back(output_message);
        }
        
        // get any output messages from julia
        for (size_t i = 0; i < session_map[session_token].outbox.size(); i++)
            response_messages.push_back(session_map[session_token].outbox[i]);
        session_map[session_token].outbox.clear();
    }

    // unlock the mutex
    pthread_mutex_unlock(&session_mutex);

    // convert the message to json
    Json::Value response_root(Json::arrayValue);
    for (size_t i = 0; i < response_messages.size(); i++)
    {
        Json::Value message_root(Json::arrayValue);
        message_root.append(response_messages[i].type);
        for (size_t j = 0; j < response_messages[i].args.size(); j++)
            message_root.append(response_messages[i].args[j]);
        response_root.append(message_root);
    }

    // return the header and response
    Json::StyledWriter writer;
    string response =  respond(session_token, writer.write(response_root));

    reading_in_progress *p = (reading_in_progress *)client->data;
    p->cstr = new char [response.size()];
    memcpy (p->cstr, response.data(),response.size());
    // write the response
    uv_buf_t buf;
    buf.base=p->cstr;
    buf.len=response.size();
    uv_write_t *wr = new uv_write_t;
    uv_write(wr,(uv_stream_t*)client,&buf,1,&free_write_buffer);
    wr->data=(void*)buf.base;

    // close the connection to the client
    uv_close((uv_handle_t*)client,&requestDone);
}


// CTRL+C signal handler
void sigproc(int)
{
    // lock the mutex
    pthread_mutex_lock(&session_mutex);

    // clean up
    for (map<string, session>::iterator iter = session_map.begin(); iter != session_map.end(); iter++)
    {
        // close the pipes
        uv_close((uv_handle_t*)(iter->second).julia_in,&pipes_done);
        uv_close((uv_handle_t*)(iter->second).julia_out,&pipes_done);

        // kill the julia process
        uv_process_kill((iter->second).proc, 9);

        uv_loop_delete((iter->second).event_loop);
    }

    // unlock the mutex
    pthread_mutex_unlock(&session_mutex);

    // terminate
    exit(0);
}

// program entrypoint
int main(int argc, char* argv[])
{
    // set the Ctrl+C handler
    signal(SIGINT, sigproc);

    // get the command line arguments
    int port_num = 1441;
    for (int i = 1; i < argc-1; i++)
    {
        if (string(argv[i]) == "-p")
            port_num = from_string<int>(argv[i+1]);
    }

    // seed the random number generator for generating session tokens
    srand(time(0));

#ifndef __WIN32__
    // ignore the SIGPIPE signal (when julia crashes or exits, we don't want to die too)
    struct sigaction act;
    act.sa_flags = 0;
    act.sa_handler = SIG_IGN;
    sigemptyset(&act.sa_mask);
    sigaction(SIGPIPE, &act, NULL);
#endif

    // initialize the mutex
    pthread_mutex_init(&session_mutex, 0);

    uv_timer_t wd;
    uv_timer_init(uv_default_loop(),&wd);
    uv_timer_start(&wd,&watchdog,WATCHDOG_INTERVAL,WATCHDOG_INTERVAL);

    // print a welcome message
    cout<<"server started on port "<<port_num<<".\n";

    // print the number of open sessions
    cout<<"0 open sessions.\n";

    // start the server
    run_server(port_num, &get_response);

    // never reached
    return 0;
}
