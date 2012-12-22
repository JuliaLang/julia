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
__MSG_INPUT_START = uint8(1) # {user_name, session_name}, create/join a new session
#endif

#ifndef notdefined
#define MSG_INPUT_POLL 2
#else
__MSG_INPUT_POLL = uint8(2) # {}, poll the server
#endif

#ifndef notdefined
#define MSG_INPUT_EVAL 3
#else
__MSG_INPUT_EVAL = uint8(3) # {user_name, user_id, input}, evaluate an expression (this message is forwarded to julia)
#endif

#ifndef notdefined
#define MSG_INPUT_REPLAY_HISTORY 4
#else
__MSG_INPUT_REPLAY_HISTORY = uint8(4) # {}, ask the server to resend all messages
#endif

#ifndef notdefined
#define MSG_INPUT_GET_USER 5
#else
__MSG_INPUT_GET_USER = uint8(5) # {}, ask the server to send back the user
#endif



#ifndef notdefined
#define MSG_OUTPUT_NULL 0
#else
__MSG_OUTPUT_NULL = uint8(0) # {}, null message (should be ignored)
#endif

#ifndef notdefined
#define MSG_OUTPUT_WELCOME 1
#else
__MSG_OUTPUT_WELCOME = uint8(1) # {}, successfully created/joined the session
#endif

#ifndef notdefined
#define MSG_OUTPUT_READY 2
#else
__MSG_OUTPUT_READY = uint8(2) # {}, ready for input, sent only when session starts (stored in replay history)
#endif

#ifndef notdefined
#define MSG_OUTPUT_MESSAGE 3
#else
__MSG_OUTPUT_MESSAGE = uint8(3) # {message}, send a message to the user (stored in replay history)
#endif

#ifndef notdefined
#define MSG_OUTPUT_OTHER 4
#else
__MSG_OUTPUT_OTHER = uint8(4) # {text}, standard output pipe (stored in replay history)
#endif

#ifndef notdefined
#define MSG_OUTPUT_EVAL_INPUT 5
#else
__MSG_OUTPUT_EVAL_INPUT = uint8(5) # {user_id, user_name, input}, input from a user (this message comes from julia, stored in replay history)
#endif

#ifndef notdefined
#define MSG_OUTPUT_FATAL_ERROR 6
#else
__MSG_OUTPUT_FATAL_ERROR = uint8(6) # {message}, fatal error message (terminates session, stored in replay history)
#endif

#ifndef notdefined
#define MSG_OUTPUT_EVAL_INCOMPLETE 7
#else
__MSG_OUTPUT_EVAL_INCOMPLETE = uint8(7) # {user_id}, incomplete expression (this message comes from julia)
#endif

#ifndef notdefined
#define MSG_OUTPUT_EVAL_RESULT 8
#else
__MSG_OUTPUT_EVAL_RESULT = uint8(8) # {user_id, result}, expression result (this message comes from julia, stored in replay history)
#endif

#ifndef notdefined
#define MSG_OUTPUT_EVAL_ERROR 9
#else
__MSG_OUTPUT_EVAL_ERROR = uint8(9) # {user_id, message}, error evaluating expression (this message comes from julia, stored in replay history)
#endif

#ifndef notdefined
#define MSG_OUTPUT_PLOT 10
#else
__MSG_OUTPUT_PLOT = uint8(10) # {type, ...} create a plot, the format of the data depends on the type of plot (this message comes from julia, stored in replay history)
#endif

#ifndef notdefined
#define MSG_OUTPUT_GET_USER 11
#else
__MSG_OUTPUT_GET_USER = uint8(11) # {user_name, user_id} response to MSG_GET_USER
#endif

#ifndef notdefined
#define MSG_OUTPUT_HTML 12
#else
__MSG_OUTPUT_HTML = uint8(12) # {html} raw html
#endif
