#ifndef notdefined
#define This_file_is_used_by_the_SCGI_server_and_the_Julia_back_end
#define NOTE_These_message_types_must_match_those_in_index_dot_js_and_repl_dot_js
#endif

#ifndef notdefined
#define MSG_INPUT_NULL 0
#else
__MSG_INPUT_NULL = uint8(0) # {}, null message (should be ignored)
#endif

#ifndef notdefined
#define MSG_INPUT_START 1
#else
__MSG_INPUT_START = uint8(1) # {user_name, session_name}, create/join a new session (this message is intercepted in the SCGI server)
#endif

#ifndef notdefined
#define MSG_INPUT_POLL 2
#else
__MSG_INPUT_POLL = uint8(2) # {}, poll the server (this message is intercepted in the SCGI server)
#endif

#ifndef notdefined
#define MSG_INPUT_EVAL 3
#else
__MSG_INPUT_EVAL = uint8(3) # {}, evaluate an expression
#endif

#ifndef notdefined
#define MSG_INPUT_REPLAY_HISTORY 4
#else
__MSG_INPUT_REPLAY_HISTORY = uint8(4) # {}, ask the server to resend all messages (this message is intercepted in the SCGI server)
#endif



#ifndef notdefined
#define MSG_OUTPUT_NULL 0
#else
__MSG_OUTPUT_NULL = uint8(0) # {}, null message (should be ignored)
#endif

#ifndef notdefined
#define MSG_OUTPUT_WELCOME 1
#else
__MSG_OUTPUT_WELCOME = uint8(1) # {}, successfully created/joined the session (not stored in the replay history)
#endif

#ifndef notdefined
#define MSG_OUTPUT_READY 2
#else
__MSG_OUTPUT_READY = uint8(2) # {}, ready for input, sent only when session starts
#endif

#ifndef notdefined
#define MSG_OUTPUT_MESSAGE 3
#else
__MSG_OUTPUT_MESSAGE = uint8(3) # {message}, send a message to the user
#endif

#ifndef notdefined
#define MSG_OUTPUT_OTHER 4
#else
__MSG_OUTPUT_OTHER = uint8(4) # {text}, standard output pipe
#endif

#ifndef notdefined
#define MSG_OUTPUT_FATAL_ERROR 5
#else
__MSG_OUTPUT_FATAL_ERROR = uint8(5) # {message}, fatal error message (terminates session)
#endif

#ifndef notdefined
#define MSG_OUTPUT_PARSE_ERROR 6
#else
__MSG_OUTPUT_PARSE_ERROR = uint8(6) # {message}, parsing error message
#endif

#ifndef notdefined
#define MSG_OUTPUT_PARSE_INCOMPLETE 7
#else
__MSG_OUTPUT_PARSE_INCOMPLETE = uint8(7) # {}, incomplete expression
#endif

#ifndef notdefined
#define MSG_OUTPUT_PARSE_COMPLETE 8
#else
__MSG_OUTPUT_PARSE_COMPLETE = uint8(8) # {}, complete expression
#endif

#ifndef notdefined
#define MSG_OUTPUT_EVAL_RESULT 9
#else
__MSG_OUTPUT_EVAL_RESULT = uint8(9) # {result}, expression result
#endif

#ifndef notdefined
#define MSG_OUTPUT_EVAL_ERROR 10
#else
__MSG_OUTPUT_EVAL_ERROR = uint8(10) # {message}, error evaluating expression
#endif

#ifndef notdefined
#define MSG_OUTPUT_PLOT 11
#else
__MSG_OUTPUT_PLOT = uint8(11) # {type, ...} create a plot, the format of the data depends on the type of plot
#endif
