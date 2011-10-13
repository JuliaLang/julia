#ifndef notdefined
#define MSG_INPUT_NULL 0
#else
MSG_INPUT_NULL = 0 # {}, null message (should be ignored)
#endif

#ifndef notdefined
#define MSG_INPUT_START 1
#else
MSG_INPUT_START = 1 # {}, new session (this message is intercepted in the SCGI server and never reaches here)
#endif

#ifndef notdefined
#define MSG_INPUT_POLL 2
#else
MSG_INPUT_POLL = 2 # {}, poll the server (this message is intercepted in the SCGI server and never reaches here)
#endif

#ifndef notdefined
#define MSG_INPUT_EVAL 3
#else
MSG_INPUT_EVAL = 3 # {}, evaluate an expression
#endif








#ifndef notdefined
#define MSG_OUTPUT_NULL 0
#else
MSG_OUTPUT_NULL = 0 # {}, null message (should be ignored)
#endif

#ifndef notdefined
#define MSG_OUTPUT_MESSAGE 1
#else
MSG_OUTPUT_MESSAGE = 1 # {message}, message
#endif

#ifndef notdefined
#define MSG_OUTPUT_ERROR 2
#else
MSG_OUTPUT_ERROR = 2 # {message}, error message
#endif

#ifndef notdefined
#define MSG_OUTPUT_FATAL_ERROR 3
#else
MSG_OUTPUT_FATAL_ERROR = 3 # {message}, fatal error message (terminates session)
#endif

#ifndef notdefined
#define MSG_OUTPUT_EVAL_INCOMPLETE 4
#else
MSG_OUTPUT_EVAL_INCOMPLETE = 4 # {}, incomplete expression
#endif

#ifndef notdefined
#define MSG_OUTPUT_EVAL_RESULT 5
#else
MSG_OUTPUT_EVAL_RESULT = 5 # {result}, expression result
#endif

#ifndef notdefined
#define MSG_OUTPUT_OTHER 6
#else
MSG_OUTPUT_OTHER = 6 # {text}, other output (not sent directly from julia)
#endif

#ifndef notdefined
#define MSG_OUTPUT_READY 7
#else
MSG_OUTPUT_READY = 7 # {}, ready for input
#endif
