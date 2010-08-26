#include <stdlib.h>
#include "dtypes.h"
#include "utils.h"

char *uint2str(char *dest, size_t len, uint64_t num, uint32_t base)
{
    int i = len-1;
    uint64_t b = (uint64_t)base;
    char ch;
    dest[i--] = '\0';
    while (i >= 0) {
        ch = (char)(num % b);
        if (ch < 10)
            ch += '0';
        else
            ch = ch-10+'a';
        dest[i--] = ch;
        num /= b;
        if (num == 0)
            break;
    }
    return &dest[i+1];
}

int isdigit_base(char c, int base)
{
    if (base < 11)
        return (c >= '0' && c < '0'+base);
    return ((c >= '0' && c <= '9') ||
            (c >= 'a' && c < 'a'+base-10) ||
            (c >= 'A' && c < 'A'+base-10));
}

/* assumes valid base, returns 1 on error, 0 if OK */
/*
int str2int(char *str, size_t len, int64_t *res, uint32_t base)
{
    int64_t result, place;
    char digit;
    int i;

    place = 1; result = 0;
    for(i=len-1; i>=0; i--) {
        digit = str[i];
        if (!isdigit_base(digit, base))
            return 1;
        if (digit <= '9')
            digit -= '0';
        else if (digit >= 'a')
            digit = digit-'a'+10;
        else if (digit >= 'A')
            digit = digit-'A'+10;
        result += digit * place;
        place *= base;
    }
    *res = result;
    return 0;
}
*/
