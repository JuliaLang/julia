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
const int INBOX_INTERVAL = 100000; // in nanoseconds

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
const int OUTBOX_INTERVAL = 100000; // in nanoseconds

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
        const int buffer_size = 1; // not including null terminator
        char* buffer = new char[buffer_size+1];
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
            bytes_read = read(pipe, buffer, buffer_size);

        // lock the mutex
        pthread_mutex_lock(&session_mutex);

        // get the read data
        string new_data;
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

const size_t MAX_CONCURRENT_SESSIONS = 4;

// generate a session token
string make_session_token()
{
    // add a random integer to a prefix
    return "SESSION_"+to_string(rand());
}

string respond_ok(string session_token, string body)
{
    string header = "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\n";
    header += "Set-Cookie: SESSION_TOKEN="+session_token+"\r\n";
    header += "\r\n";
    return header+body;
}

string respond_error(string session_token, string body)
{
    string header = "HTTP/1.1 500 Internal Server Error\r\nContent-Type: text/html; charset=UTF-8\r\n";
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
    pipe(session_data.julia_in);
    pipe(session_data.julia_out);
    
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
        nice(20);

        // limit memory usage
        rlimit limits;
        limits.rlim_max = 200000000;
        limits.rlim_cur = limits.rlim_max;
        setrlimit(RLIMIT_AS, &limits);

        // acutally spawn julia instance
        execl("./julia-release-web", "julia-release-web", (char*)0);

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

    // check for the session cookie
    string session_token;
    if (req->get_cookie_exists("SESSION_TOKEN"))
        session_token = req->get_cookie_value("SESSION_TOKEN");

    // check if the session is real
    if (session_token != "")
    {
        if (session_map.find(session_token) == session_map.end())
            session_token = "";
    }

    // create a new session if necessary
    if (session_token == "")
    {
        if (session_map.size() < MAX_CONCURRENT_SESSIONS)
            session_token = create_session();
        if (session_token == "")
            return respond_error("", "Maximum server capacity reached.  Please try again later.");
    }

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

    // unlock the mutex
    pthread_mutex_unlock(&session_mutex);

    // return the header and response
    return respond_ok(session_token, response);
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
