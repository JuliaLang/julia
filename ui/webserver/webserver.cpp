#include <iostream>
#include <map>
#include <ctime>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <sys/wait.h>
#include "server.h"
#include "json.h"
#include "message_types.h"

using namespace std;
using namespace scgi;

/////////////////////////////////////////////////////////////////////////////
// string helper functions
/////////////////////////////////////////////////////////////////////////////

// convert a value to a string
template <class T> std::string to_string(const T& t) {
    // convert a value to a string
    stringstream ss;
    ss<<t;
    return ss.str();
}

// parse a string from a value
template <class T> T from_string(const std::string& t) {
    // parse a value from a string
    T ret;
    stringstream(t)>>ret;
    return ret;
}

/////////////////////////////////////////////////////////////////////////////
// messages
/////////////////////////////////////////////////////////////////////////////

// a message
class message {
public:
    // see ui/webserver/julia_web_base.jl for documentation
    uint8_t type;
    vector<string> args;
};

/////////////////////////////////////////////////////////////////////////////
// sessions
/////////////////////////////////////////////////////////////////////////////

// if a session hasn't been queried in this time, it dies
const int WEB_SESSION_TIMEOUT = 20; // in seconds

// a web session
struct web_session {
    // time since watchdog was last "pet"
    time_t update_time;

    // to be sent to the client
    vector<message> outbox;

    // the user name
    string user_name;
};

// julia session states
enum julia_session_status {
    SESSION_WAITING_FOR_PORT_NUM,
    SESSION_NORMAL,
    SESSION_TERMINATING,
};

// a julia session
struct julia_session {
    // a map from session tokens to web sessions that use this julia session
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

    // keep track of messages sent to all the clients (not messages sent to particular users)
    vector<message> outbox_history;

    // process id of julia instance
    int pid;

    // write to julia_in[1], read from julia_out[0] and julia_err[0]
    int julia_in[2];
    int julia_out[2];
    int julia_err[2];

    // the socket for communicating to julia
    network::socket* sock;

    // io threads
    pthread_t inbox_proc;
    pthread_t outbox_proc;

    // when both threads have terminated, we can kill the session
    bool inbox_thread_alive;
    bool outbox_thread_alive;

    // whether the session should terminate
    bool should_terminate;

    // the status of the session
    julia_session_status status;
};

// a list of julia sessions
vector<julia_session*> julia_session_list;

// a mutex for accessing session_map
pthread_mutex_t julia_session_mutex;

/////////////////////////////////////////////////////////////////////////////
// THREAD:  inbox_thread (from browser to julia)
/////////////////////////////////////////////////////////////////////////////

// add to the inbox regularly according to this interval
const int INBOX_INTERVAL = 10000; // in nanoseconds

// this thread sends input from the client to julia
void* inbox_thread(void* arg) {
    // get the session
    julia_session* julia_session_ptr = (julia_session*)arg;

    // loop for the duration of the session
    while (true)
    {
        // lock the mutex
        pthread_mutex_lock(&julia_session_mutex);

        // terminate if necessary
        if (julia_session_ptr->status == SESSION_TERMINATING)
        {
            // unlock the mutex
            pthread_mutex_unlock(&julia_session_mutex);

            // terminate
            break;
        }

        // get the inbox data
        string inbox = julia_session_ptr->inbox_std;

        // if there is no inbox data and no messages to send, or if julia isn't ready, wait and try again
        if ((inbox == "" && julia_session_ptr->inbox.empty()) || julia_session_ptr->status != SESSION_NORMAL)
        {
            // unlock the mutex
            pthread_mutex_unlock(&julia_session_mutex);

            // no data from client; pause before checking again
            timespec timeout;
            timeout.tv_sec = 0;
            timeout.tv_nsec = INBOX_INTERVAL;
            nanosleep(&timeout, 0);

            // try again
            continue;
        }

        // prepare for writing to julia
        int pipe = julia_session_ptr->julia_in[1];

        // unlock the mutex
        pthread_mutex_unlock(&julia_session_mutex);

        // write if there is data
        fd_set set;
        FD_ZERO(&set);
        FD_SET(pipe, &set);
        timeval select_timeout;
        select_timeout.tv_sec = 0;
        select_timeout.tv_usec = 100000;
        ssize_t bytes_written = 0;
        if (select(FD_SETSIZE, 0, &set, 0, &select_timeout))
            bytes_written = write(pipe, inbox.c_str(), inbox.size());

        // lock the mutex
        pthread_mutex_lock(&julia_session_mutex);

        // write messages to julia
        for (size_t i = 0; i < julia_session_ptr->inbox.size(); i++)
        {
            // get the message
            message msg = julia_session_ptr->inbox[i];

            // write the message type
            string str_msg_type = " ";
            str_msg_type[0] = msg.type;
            julia_session_ptr->sock->write(str_msg_type);

            // write the number of arguments
            string str_arg_num = " ";
            str_arg_num[0] = msg.args.size();
            julia_session_ptr->sock->write(str_arg_num);

            // iterate through the arguments
            for (size_t j = 0; j < msg.args.size(); j++)
            {
                // write the size of the argument
                string str_arg_size = "    ";
                *((uint32_t*)(&(str_arg_size[0]))) = (uint32_t)msg.args[j].size();
                julia_session_ptr->sock->write(str_arg_size);

                // write the argument
                julia_session_ptr->sock->write(msg.args[j]);
            }
        }
        julia_session_ptr->inbox.clear();

        // remove the written data from the inbox
        if (bytes_written < 0)
            julia_session_ptr->inbox_std = "";
        else
            julia_session_ptr->inbox_std = julia_session_ptr->inbox_std.substr(bytes_written, julia_session_ptr->inbox_std.size()-bytes_written);

        // unlock the mutex
        pthread_mutex_unlock(&julia_session_mutex);
    }

    // lock the mutex
    pthread_mutex_lock(&julia_session_mutex);

    // tell the watchdog that this thread is done
    julia_session_ptr->inbox_thread_alive = false;

    // unlock the mutex
    pthread_mutex_unlock(&julia_session_mutex);

    // terminate
    return 0;
}

/////////////////////////////////////////////////////////////////////////////
// THREAD:  outbox_thread (from julia to browser)
/////////////////////////////////////////////////////////////////////////////

// add to the outbox regularly according to this interval
const int OUTBOX_INTERVAL = 10000; // in nanoseconds

// check for the port number from julia according to this interval
const int PORT_NUM_INTERVAL = 10000; // in nanoseconds

// this thread waits for output from julia and stores it in a buffer (for later polling by the client)
void* outbox_thread(void* arg) {
    // get the session
    julia_session* julia_session_ptr = (julia_session*)arg;

    // keep track of the output from julia
    string outbox_std;

    // loop for the duration of the session
    while (true)
    {
        // lock the mutex
        pthread_mutex_lock(&julia_session_mutex);

        // terminate if necessary
        if (julia_session_ptr->status == SESSION_TERMINATING)
        {
            // unlock the mutex
            pthread_mutex_unlock(&julia_session_mutex);

            // terminate
            break;
        }

        // flags to indicate whether raw and formatted data was available
        bool new_raw_data = false;
        bool new_formatted_data = false;

        // prepare for reading from julia
        int pipe_out = julia_session_ptr->julia_out[0];
        int pipe_err = julia_session_ptr->julia_err[0];

        // unlock the mutex
        pthread_mutex_unlock(&julia_session_mutex);

        // read output data while there is data
        while (true)
        {
            // select to determine if there is a byte to read
            char buffer[2];
            fd_set set;
            FD_ZERO(&set);
            FD_SET(pipe_out, &set);
            timeval select_timeout;
            select_timeout.tv_sec = 0;
            select_timeout.tv_usec = 100000;
            if (select(FD_SETSIZE, &set, 0, 0, &select_timeout))
            {
                // try to read the byte
                if (read(pipe_out, buffer, 1) > 0)
                    new_raw_data = true;
                else
                    break;
            }
            else
                break;
            buffer[1] = 0;

            // add the byte to the outbox
            outbox_std += buffer[0];
        }

        // read error data while there is data
        while (true)
        {
            // select to determine if there is a byte to read
            char buffer[2];
            fd_set set;
            FD_ZERO(&set);
            FD_SET(pipe_err, &set);
            timeval select_timeout;
            select_timeout.tv_sec = 0;
            select_timeout.tv_usec = 100000;
            if (select(FD_SETSIZE, &set, 0, 0, &select_timeout))
            {
                // try to read the byte
                if (read(pipe_err, buffer, 1) > 0)
                    new_raw_data = true;
                else
                    break;
            }
            else
                break;
            buffer[1] = 0;

            // add the byte to the outbox
            outbox_std += buffer[0];
        }

        // lock the mutex
        pthread_mutex_lock(&julia_session_mutex);

        // send the outbox data to the client
        if (julia_session_ptr->status == SESSION_NORMAL)
        {
            // just dump the output into the session
            julia_session_ptr->outbox_std += outbox_std;
            outbox_std = "";
        }

        // get the port number
        if (julia_session_ptr->status == SESSION_WAITING_FOR_PORT_NUM)
        {
            // wait for a newline
            size_t newline_pos = outbox_std.find("\n");
            if (newline_pos == string::npos)
            {
                // unlock the mutex
                pthread_mutex_unlock(&julia_session_mutex);

                // wait before trying again
                timespec timeout;
                timeout.tv_sec = 0;
                timeout.tv_nsec = PORT_NUM_INTERVAL;
                nanosleep(&timeout, 0);

                // try again
                continue;
            }

            // read the port number
            string num_string = outbox_std.substr(0, newline_pos);
            outbox_std = outbox_std.substr(newline_pos+1, outbox_std.size()-(newline_pos+1));
            int port_num = from_string<int>(num_string);

            // start
            julia_session_ptr->sock = new network::socket;
            julia_session_ptr->sock->connect("127.0.0.1", port_num);

            // switch to normal operation
            julia_session_ptr->status = SESSION_NORMAL;

            // send a ready message to all users of this julia session
            message ready_message;
            ready_message.type = MSG_OUTPUT_READY;
            julia_session_ptr->outbox_history.push_back(ready_message);
            for (map<string, web_session>::iterator iter = julia_session_ptr->web_session_map.begin(); iter != julia_session_ptr->web_session_map.end(); iter++)
                iter->second.outbox.push_back(ready_message);
        }

        // try to read some data from the socket
        if (julia_session_ptr->status == SESSION_NORMAL)
        {
            // get the socket before we unlock the mutex
            network::socket* sock = julia_session_ptr->sock;

            // unlock the mutex
            pthread_mutex_unlock(&julia_session_mutex);

            // try to read some data
            string data;
            if (sock->has_data())
            {
                data += sock->read();
                new_formatted_data = true;
            }
            
            // lock the mutex
            pthread_mutex_lock(&julia_session_mutex);

            // add the data to the outbox
            julia_session_ptr->outbox_raw += data;
        }

        // try to convert the raw outbox data into messages
        string outbox_raw = julia_session_ptr->outbox_raw;
        if (outbox_raw.size() >= 2)
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
                    break;

                // get the size of this argument
                uint32_t arg_size = *((uint32_t*)(&outbox_raw[pos]));
                pos += 4;

                // make sure there is enough data left to read
                if (outbox_raw.size() < pos+arg_size)
                    break;

                // get the argument
                msg.args.push_back(outbox_raw.substr(pos, arg_size));
                pos += arg_size;
            }

            // check if we have a whole message
            if (msg.args.size() == arg_num)
            {
                // we have a whole message - eat it from outbox_raw
                julia_session_ptr->outbox_raw = outbox_raw.substr(pos, outbox_raw.size()-pos);

                // add the message to the outbox queue of all the users of this julia session if necessary
                if (msg.type == MSG_OUTPUT_EVAL_INPUT ||
                    msg.type == MSG_OUTPUT_EVAL_RESULT ||
                    msg.type == MSG_OUTPUT_EVAL_ERROR ||
                    msg.type == MSG_OUTPUT_PLOT) {
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

        // unlock the mutex
        pthread_mutex_unlock(&julia_session_mutex);

        // nothing from julia; wait before trying again
        if (!new_raw_data && !new_formatted_data)
        {
            timespec timeout;
            timeout.tv_sec = 0;
            timeout.tv_nsec = OUTBOX_INTERVAL;
            nanosleep(&timeout, 0);
        }
    }

    // lock the mutex
    pthread_mutex_lock(&julia_session_mutex);

    // tell the watchdog that this thread is done
    julia_session_ptr->outbox_thread_alive = false;

    // release the socket
    delete julia_session_ptr->sock;

    // unlock the mutex
    pthread_mutex_unlock(&julia_session_mutex);

    // terminate
    return 0;
}

/////////////////////////////////////////////////////////////////////////////
// THREAD:  watchdog_thread
/////////////////////////////////////////////////////////////////////////////

// the watchdog runs regularly according to this interval
const int WATCHDOG_INTERVAL = 100000000; // in nanoseconds

// this thread kills old web sessions and kills julia sessions with no users
void* watchdog_thread(void* arg) {
    // run forever
    while (true)
    {
        // lock the mutex
        pthread_mutex_lock(&julia_session_mutex);

        // get the current time
        time_t t = time(0);

        // delete old web sessions and mark julia sessions with no web sessions as terminating
        for (size_t i = 0; i < julia_session_list.size(); i++)
        {
            julia_session* julia_session_ptr = julia_session_list[i];
            vector<string> web_session_zombies;
            for (map<string, web_session>::iterator iter = julia_session_ptr->web_session_map.begin(); iter != julia_session_ptr->web_session_map.end(); iter++)
            {
                if (t-(iter->second).update_time >= WEB_SESSION_TIMEOUT)
                    web_session_zombies.push_back(iter->first);
            }
            for (size_t j = 0; j < web_session_zombies.size(); j++)
            {
                cout<<"User \""<<julia_session_ptr->web_session_map[web_session_zombies[j]].user_name<<"\" has left session \""<<julia_session_ptr->session_name<<"\".\n";
                julia_session_ptr->web_session_map.erase(web_session_zombies[j]);
            }
            if (julia_session_ptr->web_session_map.empty())
                julia_session_ptr->status = SESSION_TERMINATING;
        }

        // kill julia sessions whose i/o threads have terminated
        for (size_t i = 0; i < julia_session_list.size(); i++)
        {
            julia_session* julia_session_ptr = julia_session_list[i];
            if (julia_session_ptr->inbox_thread_alive || julia_session_ptr->outbox_thread_alive)
                continue;

            // wait for the threads to terminate
            if (julia_session_ptr->inbox_proc)
                pthread_join(julia_session_ptr->inbox_proc, 0);
            if (julia_session_ptr->outbox_proc)
                pthread_join(julia_session_ptr->outbox_proc, 0);

            // close the pipes
            close(julia_session_ptr->julia_in[1]);
            close(julia_session_ptr->julia_out[0]);
            close(julia_session_ptr->julia_err[0]);

            // kill the julia process
            kill(julia_session_ptr->pid, 9);
            waitpid(julia_session_ptr->pid, 0, 0);

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

        // unlock the mutex
        pthread_mutex_unlock(&julia_session_mutex);

        // don't waste cpu time
        timespec timeout;
        timeout.tv_sec = 0;
        timeout.tv_nsec = WATCHDOG_INTERVAL;
        nanosleep(&timeout, 0);
    }

    // never reached
    return 0;
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
string make_session_token() {
    // add a random integer to a prefix
    return "SESSION_"+to_string(rand());
}

// format a web response with the appropriate header
string respond(string session_token, string body) {
    string header = "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\n";
    header += "Set-Cookie: SESSION_TOKEN="+session_token+"\r\n";
    header += "\r\n";
    return header+body;
}

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

    // keep the session alive for now
    session_data->inbox_thread_alive = true;
    session_data->outbox_thread_alive = true;
    session_data->should_terminate = false;
    session_data->status = SESSION_WAITING_FOR_PORT_NUM;

    // start the julia instance
    if (pipe(session_data->julia_in))
        return "";
    if (pipe(session_data->julia_out))
    {
        close(session_data->julia_in[0]);
        close(session_data->julia_in[1]);
        return "";
    }
    if (pipe(session_data->julia_err))
    {
        close(session_data->julia_in[0]);
        close(session_data->julia_in[1]);
        close(session_data->julia_out[0]);
        close(session_data->julia_out[1]);
        return "";
    }
    
    pid_t pid = fork();
    if (pid == -1)
    {
        close(session_data->julia_in[0]);
        close(session_data->julia_in[1]);
        close(session_data->julia_out[0]);
        close(session_data->julia_out[1]);
        close(session_data->julia_err[0]);
        close(session_data->julia_err[1]);
        return "";
    }
    if (pid == 0)
    {
        // this is the child process - redirect standard streams
        close(STDIN_FILENO);
        close(STDOUT_FILENO);
        dup2(session_data->julia_in[0], STDIN_FILENO);
        dup2(session_data->julia_out[1], STDOUT_FILENO);
        dup2(session_data->julia_err[1], STDERR_FILENO);
        close(session_data->julia_in[0]);
        close(session_data->julia_in[1]);
        close(session_data->julia_out[0]);
        close(session_data->julia_out[1]);
        close(session_data->julia_err[0]);
        close(session_data->julia_err[1]);

        // acutally spawn julia instance
        execl("./julia", "julia", "--no-history", "./ui/webserver/julia_web_base.jl", (char*)0);

        // if exec failed, terminate with an error
        exit(1);
    }
    close(session_data->julia_in[0]);
    close(session_data->julia_out[1]);
    close(session_data->julia_err[1]);

    // set the pid of the julia instance
    session_data->pid = pid;
    
    // start the inbox thread
    if (pthread_create(&session_data->inbox_proc, 0, inbox_thread, (void*)session_data))
        session_data->inbox_proc = 0;

    // start the outbox thread
    if (pthread_create(&session_data->outbox_proc, 0, outbox_thread, (void*)session_data))
        session_data->outbox_proc = 0;

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

// this function is called when an HTTP request is made - the response is the return value
string get_response(request* req) {
    // lock the mutex
    pthread_mutex_lock(&julia_session_mutex);

    // the session token
    string session_token;

    // check for the session cookie
    if (req->get_cookie_exists("SESSION_TOKEN"))
        session_token = req->get_cookie_value("SESSION_TOKEN");

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

    // unlock the mutex
    pthread_mutex_unlock(&julia_session_mutex);

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
    return respond(session_token, writer.write(response_root));
}

// CTRL+C signal handler
void sigproc(int) {
    // print a message
    cout<<"cleaning up...\n";

    // lock the mutex
    pthread_mutex_lock(&julia_session_mutex);

    // clean up
    for (size_t i = 0; i < julia_session_list.size(); i++)
    {
        // close the pipes
        close(julia_session_list[i]->julia_in[1]);
        close(julia_session_list[i]->julia_out[0]);
        close(julia_session_list[i]->julia_err[0]);

        // kill the julia process
        kill(julia_session_list[i]->pid, 9);
        waitpid(julia_session_list[i]->pid, 0, 0);

        // delete the session
        delete julia_session_list[i];
    }

    // unlock the mutex
    pthread_mutex_unlock(&julia_session_mutex);

    // terminate
    exit(0);
}

// program entrypoint
int main(int argc, char* argv[]) {
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

    // ignore the SIGPIPE signal (when julia crashes or exits, we don't want to die too)
    struct sigaction act;
    act.sa_flags = 0;
    act.sa_handler = SIG_IGN;
    sigemptyset(&act.sa_mask);
    sigaction(SIGPIPE, &act, NULL);

    // initialize the mutex
    pthread_mutex_init(&julia_session_mutex, 0);

    // start the watchdog thread
    pthread_t watchdog;
    pthread_create(&watchdog, 0, watchdog_thread, 0);

    // print a welcome message
    cout<<"SCGI server started on port "<<port_num<<".\n";

    // print the number of open sessions
    cout<<"0 open sessions.\n";

    // start the server
    run_server(port_num, &get_response);

    // never reached
    return 0;
}
