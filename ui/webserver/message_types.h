#ifndef notdefined
#define MSG_INPUT_NULL 0
#else
__MSG_INPUT_NULL = uint8(0) # {}, null message (should be ignored)
#endif

#ifndef notdefined
#define MSG_INPUT_START 1
#else
__MSG_INPUT_START = uint8(1) # {}, new session (this message is intercepted in the SCGI server and never reaches here)
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
#define MSG_OUTPUT_MESSAGE 1
#else
__MSG_OUTPUT_MESSAGE = uint8(1) # {message}, message
#endif

#ifndef notdefined
#define MSG_OUTPUT_ERROR 2
#else
__MSG_OUTPUT_ERROR = uint8(2) # {message}, error message
#endif

#ifndef notdefined
#define MSG_OUTPUT_FATAL_ERROR 3
#else
__MSG_OUTPUT_FATAL_ERROR = uint8(3) # {message}, fatal error message (terminates session)
#endif

#ifndef notdefined
#define MSG_OUTPUT_EVAL_INCOMPLETE 4
#else
__MSG_OUTPUT_EVAL_INCOMPLETE = uint8(4) # {}, incomplete expression
#endif

#ifndef notdefined
#define MSG_OUTPUT_EVAL_RESULT 5
#else
__MSG_OUTPUT_EVAL_RESULT = uint8(5) # {result}, expression result
#endif

#ifndef notdefined
#define MSG_OUTPUT_OTHER 6
#else
__MSG_OUTPUT_OTHER = uint8(6) # {text}, other output (not sent directly from julia)
#endif

#ifndef notdefined
#define MSG_OUTPUT_READY 7
#else
__MSG_OUTPUT_READY = uint8(7) # {}, ready for input
#endif
