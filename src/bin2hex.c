#include <stdio.h>
#include <stdint.h>

int main(int argc, char** argv)
{
    int i = 0;
    char b;

    while (scanf("%c",&b)!= EOF) {

        int n = (int)b;

        if (i > 0) {
            printf(", ");
        }

        n &= 0xff;

        printf("0x%x", n);

        if ((i+1)%16 == 0) {
            printf("\n");
        }

        i++;
    }
    printf("\n");
}