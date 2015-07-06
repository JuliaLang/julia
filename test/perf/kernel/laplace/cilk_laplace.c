// This file is a part of Julia. License is MIT: http://julialang.org/license

#include<stdio.h>
#include<cilk/cilk.h>
#include "parameter.h"
//#define N 150

main() {
    double u[size_mat][size_mat];
    double dx=0.1, dy=0.1, dx2, dy2;
    int i, j, k;
    int Niter;

    FILE *f;
    f = fopen("data_Cilk__pure", "w");

    scanf("%d", &Niter);

    // initializing the array
    u[:][:] = 0.0;
    u[:][0] = 1.0;

    dx2 = dx*dx;
    dy2 = dy*dy;

    for(i=0; i<Niter; i++)
        u[1:size_mat-2][1:size_mat-2] = ((u[0:size_mat-2][1:size_mat-2] + u[2:size_mat-2][1:size_mat-2]) * dy2 + (u[1:size_mat-2][0:size_mat-2] + u[1:size_mat-2][2:size_mat-2]) * dx2) * (1.0 / (2*(dx2+dy2)));

    for(i=0; i<size_mat; i++)
        for(j=0; j<size_mat; j++)
            fprintf(f, "%f ", u[i][j]);

    fclose(f);
}
