#include <iostream>
#include <stdlib.h>
#include "tinythread.h"
#include "server.h"

using namespace std;
using namespace tthread;
using namespace scgi;

/*

    TODO:
    - one Julia instance per user
        - Generating session IDs
    - Ignore color codes (or interpret them!)
    - More of a terminal-like display
        - Curses-like behavior
    - Infinite loops with printing?

*/

/////////////////////////////////////////////////////////////////////////////
// helpers
/////////////////////////////////////////////////////////////////////////////

// convert a value to a string
template <class T> std::string to_string (const T& t)
{
    // convert a value to a string
    stringstream ss;
    ss<<t;
    return ss.str();
}

// parse a string from a value
template <class T> T from_string (const std::string& t)
{
    // parse a value from a string
    T ret;
    stringstream(t)>>ret;
    return ret;
}

// replace all occurrences of from with to in str
string str_replace(string str, string from, string to)
{
    size_t pos = str.find(from, 0);
    while (pos != string::npos)
    {
        str.replace(pos, from.size(), to);
        pos = str.find(from, pos+to.size());
    }
    return str;
}

/////////////////////////////////////////////////////////////////////////////
// main stuff
/////////////////////////////////////////////////////////////////////////////

// unsent data
string outbox;
mutex outbox_mutex;

// write to julia_in[1], read from julia_out[0]
int julia_in[2];
int julia_out[2];

void output_thread(void* arg)
{
    // read data forever
    while (true)
    {
        // read some output from Julia
        const int BUFFER_SIZE = 1;
        char* buffer = new char[BUFFER_SIZE+1];
        buffer[read(julia_out[0], buffer, BUFFER_SIZE)] = 0;
        string output = buffer;
        delete [] buffer;

        // filter control characters from the output
        string filtered_output;
        for (size_t i = 0; i < output.size(); i++)
        {
            if (output[i] >= 0x20 && output[i] <= 0x7E)
                filtered_output += output[i];
            if (output[i] == '\n')
                filtered_output += "\r\n";
        }

        filtered_output = output;

        // add it to the outbox
        outbox_mutex.lock();
        outbox += filtered_output;
        outbox_mutex.unlock();
    }
}

string get_response(request* req)
{
    // the HTTP header
    string header = "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n";

    // process input if there is any
    if (req->get_field_exists("terminal-input"))
    {
        string input = req->get_field_value("terminal-input")+"\n";
        write(julia_in[1], input.c_str(), input.size());
    }

    // if julia has outputted any data since last poll
    outbox_mutex.lock();
    string response = outbox;
    outbox = "";
    outbox_mutex.unlock();

    // escape whitespace for html
    response = str_replace(response, " ", "&nbsp;");
    response = str_replace(response, "\n", "<br />");

    // return the header and response
    return header+response;
}

int main()
{
    // start the julia instance
    pipe(julia_in);
    pipe(julia_out);
    pid_t pid = fork();
    if (pid == 0)
    {
        // this is the child process - redirect standard streams
        close(STDIN_FILENO);
        close(STDOUT_FILENO);
        dup2(julia_in[0], STDIN_FILENO);
        dup2(julia_out[1], STDOUT_FILENO);
        close(julia_in[0]);
        close(julia_in[1]);
        close(julia_out[0]);
        close(julia_out[1]);

        // spawn julia instance
        system("./julia");
    }
    close(julia_in[0]);
    close(julia_out[1]);

    // start the output thread
    thread t(output_thread, 0);

    // start the server
    cout<<"server started.  version 0.1.\n";
    run_server(1441, &get_response);

    // wait for the thread to finish
    t.join();

    // clean up
    close(julia_in[1]);
    close(julia_out[0]);
    return 0;
}