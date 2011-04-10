#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "dtypes.h"
#include "utils.h"

void snprint_cplx(char *s, size_t cnt, double re, double im,
                  // args to pass on to snprint_real
                  int width, int dec,
                  int max_digs_rt, int max_digs_lf,
                  // print spaces around sign in a+bi
                  int spflag)
{
    int fzr = (re==0) || rel_zero(re,im);
    int fzi = (im==0) || rel_zero(im,re);
    size_t len, sl;
    size_t space = cnt;

    s[0] = '\0';
    if (isnan(im) && fzr) {
        if (space < 2) return;
        snprint_real(s, space-2, im, width, dec, max_digs_rt, max_digs_lf);
        strcat(s, "i");
        return;
    }
    if (!fzr || (fzr && fzi)) {
        if (space < 4) return;
        snprint_real(s, space-4, re, width, dec, max_digs_rt, max_digs_lf);
        if ((im >= 0 || (isnan(im)&&!sign_bit(im))) && !fzi) {
            if (spflag) {
                strcat(s, " + ");
            }
            else {
                strcat(s, "+");
            }
        }
        else if (!fzi) {
            im = -im;
            if (spflag)
                strcat(s, " - ");
            else
                strcat(s, "-");
        }
    }
    if (!fzi) {
        len = sl = strlen(s);
        if (im == -1) {
            while ((long)(len-sl) < (long)(width-2) && len < (space-3))
                s[len++] = ' ';
            s[len] =   '-';
            s[len+1] = 'i';
            s[len+2] = '\0';
        }
        else if (im == 1) {
            while ((long)(len-sl) < (long)(width-1) && len < (space-2))
                s[len++] = ' ';
            s[len] =   'i';
            s[len+1] = '\0';
        }
        else {
            snprint_real(s+len, space-len-2, im, width, dec,
                         max_digs_rt, max_digs_lf);
            strcat(s, "i");
        }
    }
}
