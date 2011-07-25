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

using namespace std;
using namespace scgi;

/////////////////////////////////////////////////////////////////////////////
// helpers
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

// replace all occurrences of from with to in str
string str_replace(string str, string from, string to)
{
    // start from the beginning
    size_t pos = str.find(from, 0);
    while (pos != string::npos)
    {
        str.replace(pos, from.size(), to);
        pos = str.find(from, pos+to.size());
    }
    return str;
}

/////////////////////////////////////////////////////////////////////////////
// sessions
/////////////////////////////////////////////////////////////////////////////

// if a session hasn't been queried in this time, it dies
const int SESSION_TIMEOUT = 10; // in seconds

// a session
struct session
{
    // time since watchdog was last "pet"
    time_t update_time;

    // to be sent to julia
    string inbox;

    // to be sent to the client
    string outbox;

    // process id of julia instance
    int pid;

    // write to julia_in[1], read from julia_out[0]
    int julia_in[2];
    int julia_out[2];

    // io threads
    pthread_t inbox_proc;
    pthread_t outbox_proc;

    // when both threads have terminated, we can kill the session
    bool inbox_thread_alive;
    bool outbox_thread_alive;

    // whether this session should terminate
    bool terminating;
};

// a list of sessions
map<string, session> session_map;

// a mutex for accessing session_map
pthread_mutex_t session_mutex;

/////////////////////////////////////////////////////////////////////////////
// THREAD:  inbox_thread
/////////////////////////////////////////////////////////////////////////////

// add to the inbox regularly according to this interval
const int INBOX_INTERVAL = 10000; // in nanoseconds

// this thread sends input from the client to julia
void* inbox_thread(void* arg)
{
    // get the session token
    string session_token = (char*)arg;
    delete [] (char*)arg;

    // loop for the duration of the session
    while (true)
    {
        // lock the mutex
        pthread_mutex_lock(&session_mutex);

        // terminate if necessary
        if (session_map[session_token].terminating)
        {
            // unlock the mutex
            pthread_mutex_unlock(&session_mutex);

            // terminate
            break;
        }

        // get the inbox data
        string inbox = session_map[session_token].inbox;
        if (inbox == "")
        {
            // unlock the mutex
            pthread_mutex_unlock(&session_mutex);

            // no data from client; pause before checking again
            timespec timeout;
            timeout.tv_sec = 0;
            timeout.tv_nsec = INBOX_INTERVAL;
            nanosleep(&timeout, 0);

            // try again
            continue;
        }

        // prepare for writing to julia
        int pipe = session_map[session_token].julia_in[1];

        // unlock the mutex
        pthread_mutex_unlock(&session_mutex);

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
        pthread_mutex_lock(&session_mutex);

        // remove the written data from the inbox
        if (bytes_written < 0)
            session_map[session_token].inbox = "";
        else
            session_map[session_token].inbox = session_map[session_token].inbox.substr(bytes_written, session_map[session_token].inbox.size()-bytes_written);

        // unlock the mutex
        pthread_mutex_unlock(&session_mutex);
    }

    // lock the mutex
    pthread_mutex_lock(&session_mutex);

    // tell the watchdog that this thread is done
    session_map[session_token].inbox_thread_alive = false;

    // unlock the mutex
    pthread_mutex_unlock(&session_mutex);

    // terminate
    return 0;
}

/////////////////////////////////////////////////////////////////////////////
// THREAD:  outbox_thread
/////////////////////////////////////////////////////////////////////////////

// add to the outbox regularly according to this interval
const int OUTBOX_INTERVAL = 10000; // in nanoseconds

// this thread waits for output from julia and stores it in a buffer (for later polling by the client)
void* outbox_thread(void* arg)
{
    // get the session token
    string session_token = (char*)arg;
    delete [] (char*)arg;

    // keep track of the output from julia
    string outbox;

    // loop for the duration of the session
    while (true)
    {
        // lock the mutex
        pthread_mutex_lock(&session_mutex);

        // terminate if necessary
        if (session_map[session_token].terminating)
        {
            // unlock the mutex
            pthread_mutex_unlock(&session_mutex);

            // terminate
            break;
        }

        // prepare for reading from julia
        char buffer[2];
        int pipe = session_map[session_token].julia_out[0];

        // unlock the mutex
        pthread_mutex_unlock(&session_mutex);

        // read if there is data
        fd_set set;
        FD_ZERO(&set);
        FD_SET(pipe, &set);
        timeval select_timeout;
        select_timeout.tv_sec = 0;
        select_timeout.tv_usec = 100000;
        ssize_t bytes_read = 0;
        if (select(FD_SETSIZE, &set, 0, 0, &select_timeout))
            bytes_read = read(pipe, buffer, 1);
            buffer[1] = 0;

        // lock the mutex
        pthread_mutex_lock(&session_mutex);

        // get the read data
        string new_data;
        if (bytes_read == 1)
            new_data = buffer;
        outbox += new_data;

        // try to read a character
        if (outbox != "")
        {
            // get the first character
            char c = outbox[0];

            // is it a control sequence?
            if (c == 0x1B)
            {
                // if we don't have enough characters, try again later
                if (outbox.size() < 2)
                {
                    // unlock the mutex
                    pthread_mutex_unlock(&session_mutex);

                    // if we didn't get any new data from julia, wait before trying again
                    if (new_data == "")
                    {
                        timespec timeout;
                        timeout.tv_sec = 0;
                        timeout.tv_nsec = OUTBOX_INTERVAL;
                        nanosleep(&timeout, 0);
                    }

                    // try again
                    continue;
                }

                // check for multi-character escape sequences
                if (outbox[1] == '[')
                {
                    // find the last character in the sequence
                    int pos = -1;
                    for (int i = 2; i < outbox.size(); i++)
                    {
                        if (outbox[i] >= 64 && outbox[i] <= 126)
                        {
                            pos = i;
                            break;
                        }
                    }

                    // if we don't have enouch characters, try again later
                    if (pos == -1)
                    {
                        // unlock the mutex
                        pthread_mutex_unlock(&session_mutex);

                        // if we didn't get any new data from julia, wait before trying again
                        if (new_data == "")
                        {
                            timespec timeout;
                            timeout.tv_sec = 0;
                            timeout.tv_nsec = OUTBOX_INTERVAL;
                            nanosleep(&timeout, 0);
                        }

                        // try again
                        continue;
                    }
                    else
                    {
                        // eat the control sequence and output nothing
                        outbox = outbox.substr(pos+1, outbox.size()-(pos+1));
                        
                        // unlock the mutex
                        pthread_mutex_unlock(&session_mutex);

                        // keep going
                        continue;
                    }
                }
                else
                {
                    // eat the two-character control sequence and output nothing
                    outbox = outbox.substr(2, outbox.size()-2);
                    
                    // unlock the mutex
                    pthread_mutex_unlock(&session_mutex);

                    // keep going
                    continue;
                }
            }

            // just output the raw character
            session_map[session_token].outbox += c;
            outbox = outbox.substr(1, outbox.size()-1);
        }

        // unlock the mutex
        pthread_mutex_unlock(&session_mutex);

        // nothing from julia; wait before trying again
        timespec timeout;
        timeout.tv_sec = 0;
        timeout.tv_nsec = OUTBOX_INTERVAL;
        nanosleep(&timeout, 0);
    }

    // lock the mutex
    pthread_mutex_lock(&session_mutex);

    // tell the watchdog that this thread is done
    session_map[session_token].outbox_thread_alive = false;

    // unlock the mutex
    pthread_mutex_unlock(&session_mutex);

    // terminate
    return 0;
}

/////////////////////////////////////////////////////////////////////////////
// THREAD:  watchdog_thread
/////////////////////////////////////////////////////////////////////////////

// the watchdog runs regularly according to this interval
const int WATCHDOG_INTERVAL = 100000000; // in nanoseconds

// this thread kills old sessions after they have timed out
void* watchdog_thread(void* arg)
{
    // run forever
    while (true)
    {
        // lock the mutex
        pthread_mutex_lock(&session_mutex);

        // get the current time
        time_t t = time(0);

        // start terminating old sessions
        for (map<string, session>::iterator iter = session_map.begin(); iter != session_map.end(); iter++)
        {
            if (!(iter->second).terminating)
            {
                if (t-(iter->second).update_time >= SESSION_TIMEOUT)
                    (iter->second).terminating = true;
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
            // wait for the threads to terminate
            if (session_map[*iter].inbox_proc)
                pthread_join(session_map[*iter].inbox_proc, 0);
            if (session_map[*iter].outbox_proc)
                pthread_join(session_map[*iter].outbox_proc, 0);

            // close the pipes
            close(session_map[*iter].julia_in[1]);
            close(session_map[*iter].julia_out[0]);

            // kill the julia process
            kill(session_map[*iter].pid, 9);
            waitpid(session_map[*iter].pid, 0, 0);

            // remove the session from the map
            session_map.erase(*iter);

            // print the number of open sessions
            if (session_map.size() == 1)
                cout<<session_map.size()<<" open session.\n";
            else
                cout<<session_map.size()<<" open sessions.\n";
        }

        // unlock the mutex
        pthread_mutex_unlock(&session_mutex);

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

// generate a session token
string make_session_token()
{
    // add a random integer to a prefix
    return "SESSION_"+to_string(rand());
}

string respond(string session_token, string body)
{
    string header = "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\n";
    header += "Set-Cookie: SESSION_TOKEN="+session_token+"\r\n";
    header += "\r\n";
    return header+body;
}

// create a session and return a session token
string create_session()
{
    // create the session
    session session_data;

    // generate a session token
    string session_token = make_session_token();

    // keep the session alive for now
    session_data.inbox_thread_alive = true;
    session_data.outbox_thread_alive = true;
    session_data.terminating = false;

    // start the julia instance
    if (pipe(session_data.julia_in))
        return "";
    if (pipe(session_data.julia_out))
    {
        close(session_data.julia_in[0]);
        close(session_data.julia_in[1]);
        return "";
    }
    
    pid_t pid = fork();
    if (pid == -1)
    {
        close(session_data.julia_in[0]);
        close(session_data.julia_in[1]);
        close(session_data.julia_out[0]);
        close(session_data.julia_out[1]);
        return "";
    }
    if (pid == 0)
    {
        // this is the child process - redirect standard streams
        close(STDIN_FILENO);
        close(STDOUT_FILENO);
        dup2(session_data.julia_in[0], STDIN_FILENO);
        dup2(session_data.julia_out[1], STDOUT_FILENO);
        close(session_data.julia_in[0]);
        close(session_data.julia_in[1]);
        close(session_data.julia_out[0]);
        close(session_data.julia_out[1]);

        // set a high nice value
        if (nice(20) == -1)
            exit(1);

        // limit memory usage
        rlimit limits;
        limits.rlim_max = 200000000;
        limits.rlim_cur = limits.rlim_max;
        setrlimit(RLIMIT_AS, &limits);

        // acutally spawn julia instance
        execl("./julia-release-web", "julia-release-web", "-q", (char*)0);

        // if exec failed, terminate with an error
        exit(1);
    }
    close(session_data.julia_in[0]);
    close(session_data.julia_out[1]);

    // set the pid of the julia instance
    session_data.pid = pid;
    
    // start the inbox thread
    char* session_token_inbox = new char[256];
    strcpy(session_token_inbox, session_token.c_str());
    if (pthread_create(&session_data.inbox_proc, 0, inbox_thread, (void*)session_token_inbox))
    {
        delete [] session_token_inbox;
        session_data.inbox_proc = 0;
    }

    // start the outbox thread
    char* session_token_outbox = new char[256];
    strcpy(session_token_outbox, session_token.c_str());
    if (pthread_create(&session_data.outbox_proc, 0, outbox_thread, (void*)session_token_outbox))
    {
        delete [] session_token_outbox;
        session_data.outbox_proc = 0;
    }

    // set the start time
    session_data.update_time = time(0);

    // store the session
    session_map[session_token] = session_data;

    // print the number of open sessions
    if (session_map.size() == 1)
        cout<<session_map.size()<<" open session.\n";
    else
        cout<<session_map.size()<<" open sessions.\n";
    
    // return the session token
    return session_token;
}

// this function is called when an HTTP request is made - the response is the return value
string get_response(request* req)
{
    // lock the mutex
    pthread_mutex_lock(&session_mutex);

    // the response
    Json::Value response_root;
    Json::StyledWriter writer;
    string session_token;

    // process input if there is any
    if (req->get_field_exists("request"))
    {
        // parse the request
        Json::Value request_root;
        Json::Reader reader;
        if (reader.parse(req->get_field_value("request"), request_root))
        {
            // determine the type of request
            bool request_recognized = false;

            // init message
            if (request_root.get("type", "").asString() == "init")
            {
                // we recognize the request
                request_recognized = true;

                // create a new session
                if (session_map.size() < MAX_CONCURRENT_SESSIONS)
                    session_token = create_session();
                if (session_token == "")
                {
                    // too many sessions
                    response_root["type"] = "fatal_error";
                    response_root["message"] = "&lt;maximum server capacity reached for now&gt;<br />";
                }
                else
                {
                    // successfully created new session
                    response_root["type"] = "init_message";
                    response_root["message"] = "&lt;initializing&gt;<br />";
                }
            }

            // sending input to julia
            if (request_root.get("type", "").asString() == "input")
            {
                // we recognize the request
                request_recognized = true;

                // check for the session cookie
                if (req->get_cookie_exists("SESSION_TOKEN"))
                    session_token = req->get_cookie_value("SESSION_TOKEN");

                // check if the session is real
                if (session_token != "")
                {
                    if (session_map.find(session_token) == session_map.end())
                        session_token = "";
                }
                if (session_token == "")
                {
                    response_root["type"] = "fatal_error";
                    response_root["message"] = "&lt;session expired&gt;<br />";
                }
                else
                {
                    // add the data to the inbox
                    session_map[session_token].inbox += request_root.get("input", "").asString();

                    // respond with a success message
                    response_root["type"] = "success";

                    // pet the watchdog
                    session_map[session_token].update_time = time(0);
                }
            }

            // polling for julia output
            if (request_root.get("type", "").asString() == "poll")
            {
                // we recognize the request
                request_recognized = true;

                // check for the session cookie
                if (req->get_cookie_exists("SESSION_TOKEN"))
                    session_token = req->get_cookie_value("SESSION_TOKEN");

                // check if the session is real
                if (session_token != "")
                {
                    if (session_map.find(session_token) == session_map.end())
                        session_token = "";
                }
                if (session_token == "")
                {
                    response_root["type"] = "fatal_error";
                    response_root["message"] = "&lt;session expired&gt;<br />";
                }
                else
                {
                    // escape for html
                    string content = session_map[session_token].outbox;
                    content = str_replace(content, "&", "&amp;");
                    content = str_replace(content, "<", "&lt;");
                    content = str_replace(content, ">", "&gt;");
                    content = str_replace(content, " ", "&nbsp;");
                    content = str_replace(content, "\n", "<br />");

                    // if julia has outputted any data since last poll
                    response_root["type"] = "content";
                    response_root["content"] = content;
                    session_map[session_token].outbox = "";
                    string response = writer.write(response_root);

                    // pet the watchdog
                    session_map[session_token].update_time = time(0);
                }
            }

            // unrecognized request
            if (!request_recognized)
            {
                response_root["type"] = "fatal_error";
                response_root["message"] = "&lt;invalid request&gt;<br />";
            }
        }
        else
        {
            // error parsing json
            response_root["type"] = "fatal_error";
            response_root["message"] = "&lt;invalid request&gt;<br />";
        }
    }
    else
    {
        // no request
        response_root["type"] = "fatal_error";
        response_root["message"] = "&lt;invalid request&gt;<br />";
    }

    // unlock the mutex
    pthread_mutex_unlock(&session_mutex);

    // return the header and response
    return respond(session_token, writer.write(response_root));
}

// program entrypoint
int main()
{
    // seed the random number generator for generating session tokens
    srand(time(0));

    // ignore the SIGPIPE signal (when julia crashes or exits, we don't want to die too)
    struct sigaction act;
    act.sa_flags = 0;
    act.sa_handler = SIG_IGN;
    sigemptyset(&act.sa_mask);
    sigaction(SIGPIPE, &act, NULL);

    // initialize the mutex
    pthread_mutex_init(&session_mutex, 0);

    // start the watchdog thread
    pthread_t watchdog;
    pthread_create(&watchdog, 0, watchdog_thread, 0);

    // print the number of open sessions
    cout<<"0 open sessions.\n";

    // start the server
    run_server(1441, &get_response);

    // never reached
    return 0;
}
