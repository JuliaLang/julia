#include "double-conversion.h"

#ifdef WIN32
#define STDCALL __stdcall
# ifdef IMPORT_EXPORTS
#  define DLLEXPORT __declspec(dllimport)
# else
#  define DLLEXPORT __declspec(dllexport)
# endif
#else
#define STDCALL
#define DLLEXPORT __attribute__ ((visibility("default")))
#endif

extern "C" DLLEXPORT void grisu(
    double v,
    int mode, // double-to-string mode
    int requested_digits,
    char* buffer,
    int buffer_length,
    bool* sign,
    int* length,
    int* point
) {
    double_conversion::DoubleToStringConverter::DoubleToAscii(
        v,
        (double_conversion::DoubleToStringConverter::DtoaMode)mode,
        requested_digits,
        buffer,
        buffer_length,
        sign,
        length,
        point
    );
}
