#ifndef notdefined
#define MSG_INPUT_NULL 0
#else
__MSG_INPUT_NULL = uint8(0) # {}, null message (should be ignored)
#endif

#ifndef notdefined
#define MSG_INPUT_START 1
#else
__MSG_INPUT_START = uint8(1) # {}, create a new session (this message is intercepted in the SCGI server and never reaches here)
#endif

#ifndef notdefined
#define MSG_INPUT_POLL 2
#else
__MSG_INPUT_POLL = uint8(2) # {}, poll the server (this message is intercepted in the SCGI server and never reaches here)
#endif

#ifndef notdefined
#define MSG_INPUT_EVAL 3
#else
__MSG_INPUT_EVAL = uint8(3) # {}, evaluate an expression
#endif



#ifndef notdefined
#define MSG_OUTPUT_NULL 0
#else
__MSG_OUTPUT_NULL = uint8(0) # {}, null message (should be ignored)
#endif

#ifndef notdefined
#define MSG_OUTPUT_READY 1
#else
__MSG_OUTPUT_READY = uint8(1) # {}, ready for input, sent only when session starts
#endif

#ifndef notdefined
#define MSG_OUTPUT_MESSAGE 2
#else
__MSG_OUTPUT_MESSAGE = uint8(2) # {message}, send a message to the user
#endif

#ifndef notdefined
#define MSG_OUTPUT_OTHER 3
#else
__MSG_OUTPUT_OTHER = uint8(3) # {text}, standard output pipe
#endif

#ifndef notdefined
#define MSG_OUTPUT_FATAL_ERROR 4
#else
__MSG_OUTPUT_FATAL_ERROR = uint8(4) # {message}, fatal error message (terminates session)
#endif

#ifndef notdefined
#define MSG_OUTPUT_PARSE_ERROR 5
#else
__MSG_OUTPUT_PARSE_ERROR = uint8(5) # {message}, parsing error message
#endif

#ifndef notdefined
#define MSG_OUTPUT_PARSE_INCOMPLETE 6
#else
__MSG_OUTPUT_PARSE_INCOMPLETE = uint8(6) # {}, incomplete expression
#endif

#ifndef notdefined
#define MSG_OUTPUT_PARSE_COMPLETE 7
#else
__MSG_OUTPUT_PARSE_COMPLETE = uint8(7) # {}, complete expression
#endif

#ifndef notdefined
#define MSG_OUTPUT_EVAL_RESULT 8
#else
__MSG_OUTPUT_EVAL_RESULT = uint8(8) # {result}, expression result
#endif

#ifndef notdefined
#define MSG_OUTPUT_EVAL_ERROR 9
#else
__MSG_OUTPUT_EVAL_ERROR = uint8(9) # {message}, error evaluating expression
#endif

#ifndef notdefined
#define MSG_OUTPUT_PLOT 10
#else
__MSG_OUTPUT_PLOT = uint8(10) # {type, ...} create a plot, the format of the data depends on the type of plot
#endif
