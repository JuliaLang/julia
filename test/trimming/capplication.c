#include <stdio.h>
#include "libsimple.h"

int main() {
    // Example usage of the functions defined in libsimple.h
    CVectorPair_Float32 vecPair;
    vecPair.from.length = 3;
    vecPair.from.data = (float[]){1.0f, 2.0f, 3.0f};
    vecPair.to.length = 3;
    vecPair.to.data = (float[]){4.0f, 5.0f, 6.0f};

    float sum = copyto_and_sum(vecPair);
    printf("Sum of copied values: %f\n", sum);

    MyTwoVec list[] = {{1, 2}, {5, 5}, {3, 4}};
    int32_t count = countsame(list, 3);
    printf("Count of same vectors: %d\n", count);

    return 0;
}
