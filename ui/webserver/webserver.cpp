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

using namespace std;
using namespace scgi;

/*

    TODO:
    - fix weird "Killed" bug
    - make it work on server
    - more of a terminal-like display
        - curses-like behavior

*/

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
    // replace all occurrences of from with to in str
    size_t pos = str.find(from, 0);
    while (pos != string::npos)
    {
        str.replace(pos, from.size(), to);
        pos = str.find(from, pos+to.size());
    }
    return str;
}

/////////////////////////////////////////////////////////////////////////////
// session management
/////////////////////////////////////////////////////////////////////////////

const int SESSION_TIMEOUT = 10; // in seconds
const int IO_INTERVAL = 100000; // in nanoseconds
const int WATCHDOG_INTERVAL = 100000000; // in nanoseconds

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
};

// a list of sessions
map<string, session> session_map;

// a mutex for accessing session_map
pthread_mutex_t session_mutex;

// generate a session token
string make_session_token()
{
    return "SESSION_"+to_string(rand());
}

// this thread sends input from the client to julia
void* inbox_thread(void* arg)
{
    // get the session token
    string session_token = (char*)arg;
    delete [] (char*)arg;

    // loop for the duration of the session
    while (true)
    {
        // lock the session mutex
        pthread_mutex_lock(&session_mutex);

        // send some data to julia
        int pipe = session_map[session_token].julia_in[1];
        string inbox = session_map[session_token].inbox;
        pthread_mutex_unlock(&session_mutex);
        size_t bytes_written = write(pipe, inbox.c_str(), inbox.size());
        pthread_mutex_lock(&session_mutex);
        
        // and remove it from the inbox
        if (bytes_written > 0)
            session_map[session_token].inbox = session_map[session_token].inbox.substr(bytes_written, session_map[session_token].inbox.size()-bytes_written);

        // unlock the session mutex
        pthread_mutex_unlock(&session_mutex);

        // if no data was available, sleep before trying again
        if (bytes_written <= 0)
        {
            timespec timeout;
            timeout.tv_sec = 0;
            timeout.tv_nsec = IO_INTERVAL;
            nanosleep(&timeout, 0);
        }
    }
    return 0;
}

// this thread waits for output from julia and stores it in a buffer (for later polling by the client)
void* outbox_thread(void* arg)
{
    // get the session token
    string session_token = (char*)arg;
    delete [] (char*)arg;

    // keep track of the output from julia
    string outbox;

    // keep track of whether we should wait or not
    bool should_wait = true;

    // loop for the duration of the session
    while (true)
    {
        // wait to save cpu cycles if nothing is going on
        if (should_wait)
        {
            timespec timeout;
            timeout.tv_sec = 0;
            timeout.tv_nsec = IO_INTERVAL;
            nanosleep(&timeout, 0);
        }
        should_wait = true;

        // lock the session mutex
        pthread_mutex_lock(&session_mutex);

        // read some output from julia
        string new_data;
        const int buffer_size = 1; // not including null terminator
        char* buffer = new char[buffer_size+1];
        int pipe = session_map[session_token].julia_out[0];
        pthread_mutex_unlock(&session_mutex);
        size_t bytes_read = read(pipe, buffer, buffer_size);
        pthread_mutex_lock(&session_mutex);
        if (bytes_read > 0)
        {
            buffer[bytes_read] = 0;
            new_data = buffer;
        }
        delete [] buffer;
        outbox += new_data;


        // try to read a character
        if (outbox != "")
        {
            // we just got data; try to get more without waiting
            should_wait = false;

            // get the first character
            char c = outbox[0];

            // is it a control sequence?
            if (c == 0x1B)
            {
                // if we don't have enouch characters, try again later
                if (outbox.size() < 2)
                {
                    pthread_mutex_unlock(&session_mutex);
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
                        pthread_mutex_unlock(&session_mutex);
                        continue;
                    }
                    else
                    {
                        // eat the control sequence and output nothing
                        outbox = outbox.substr(pos+1, outbox.size()-(pos+1));
                        pthread_mutex_unlock(&session_mutex);

                        // get the next character immediately
                        should_wait = false;
                        continue;
                    }
                }
                else
                {
                    // eat the two-character control sequence and output nothing
                    outbox = outbox.substr(2, outbox.size()-2);
                    pthread_mutex_unlock(&session_mutex);

                    // get the next character immediately
                    should_wait = false;
                    continue;
                }
            }

            // just output the raw character
            session_map[session_token].outbox += c;
            outbox = outbox.substr(1, outbox.size()-1);

            // get the next character immediately
            should_wait = false;
        }

        // unlock the session mutex
        pthread_mutex_unlock(&session_mutex);
    }
    return 0;
}

// create a session and return a session token
string create_session()
{
    // create the session
    session session_data;

    // generate a session token
    string session_token = make_session_token();

    // start the julia instance
    pipe(session_data.julia_in);
    pipe(session_data.julia_out);
    pid_t pid = fork();
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

        // acutally spawn julia instance
        execl("./julia-release-basic", "julia-release-basic", (char*)0);
    }
    close(session_data.julia_in[0]);
    close(session_data.julia_out[1]);

    // set the pid of the julia instance
    session_data.pid = pid;

    // start the inbox thread
    char* session_token_inbox = new char[256];
    strcpy(session_token_inbox, session_token.c_str());
    pthread_create(&session_data.inbox_proc, 0, inbox_thread, (void*)session_token_inbox);

    // start the outbox thread
    char* session_token_outbox = new char[256];
    strcpy(session_token_outbox, session_token.c_str());
    pthread_create(&session_data.outbox_proc, 0, outbox_thread, (void*)session_token_outbox);

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

// destroy a session
void destroy_session(string session_token)
{
    // free the pipes
    close(session_map[session_token].julia_in[1]);
    close(session_map[session_token].julia_out[0]);

    // kill the io threads
    pthread_cancel(session_map[session_token].inbox_proc);
    pthread_cancel(session_map[session_token].outbox_proc);

    // kill the julia process
    kill(session_map[session_token].pid, 9);
    waitpid(session_map[session_token].pid, 0, 0);

    // remove the session from the map
    session_map.erase(session_token);

    // print the number of open sessions
    if (session_map.size() == 1)
        cout<<session_map.size()<<" open session.\n";
    else
        cout<<session_map.size()<<" open sessions.\n";
}

// this thread kills old sessions after they have timed out
void* watchdog_thread(void* arg)
{
    // run forever
    while (true)
    {
        // lock the session mutex
        pthread_mutex_lock(&session_mutex);

        // get the current time
        time_t t = time(0);

        // keep a list of sessions to be deleted
        vector<string> old_sessions;

        // iterate through the open sessions
        for (map<string, session>::iterator iter = session_map.begin(); iter != session_map.end(); iter++)
        {
            if (t-(iter->second).update_time >= SESSION_TIMEOUT)
                old_sessions.push_back(iter->first);
        }

        // delete old sessions
        for (vector<string>::iterator iter = old_sessions.begin(); iter != old_sessions.end(); iter++)
            destroy_session(*iter);

        // unlock the session mutex
        pthread_mutex_unlock(&session_mutex);

        // don't waste cpu time
        timespec timeout;
        timeout.tv_sec = 0;
        timeout.tv_nsec = WATCHDOG_INTERVAL;
        nanosleep(&timeout, 0);
    }
    return 0;
}

/////////////////////////////////////////////////////////////////////////////
// main stuff
/////////////////////////////////////////////////////////////////////////////

// this function is called when an HTTP request is made - the response is the return value
string get_response(request* req)
{
    // lock the session mutex
    pthread_mutex_lock(&session_mutex);

    // check for the session cookie
    string session_token;
    if (req->get_cookie_exists("SESSION_TOKEN"))
        session_token = req->get_cookie_value("SESSION_TOKEN");

    // make sure the session exists
    if (session_map.find(session_token) == session_map.end())
        session_token = "";

    // the HTTP header
    string header = "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\n";
    if (session_token == "")
    {
        session_token = create_session();
        header += "Set-Cookie: SESSION_TOKEN="+session_token+"\r\n";
    }
    header += "\r\n";

    // process input if there is any
    if (req->get_field_exists("terminal-input"))
    {
        string input = req->get_field_value("terminal-input")+"\n";
        session_map[session_token].inbox += input;
    }

    // if julia has outputted any data since last poll
    string response = session_map[session_token].outbox;
    session_map[session_token].outbox = "";

    // escape whitespace for html
    response = str_replace(response, " ", "&nbsp;");
    response = str_replace(response, "\n", "<br />");

    // pet the watchdog
    session_map[session_token].update_time = time(0);

    // unlock the session mutex
    pthread_mutex_unlock(&session_mutex);

    // return the header and response
    return header+response;
}

// program entrypoint
int main()
{
    // seed the random number generator for generating session tokens
    srand(time(0));

    // initialize the session mutex
    pthread_mutex_init(&session_mutex, 0);

    // start the watchdog thread
    pthread_t watchdog;
    pthread_create(&watchdog, 0, watchdog_thread, 0);

    // start the server
    cout<<"server started.\n";
    run_server(1441, &get_response);

    // never reached
    return 0;
}
