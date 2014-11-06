#include<stdio.h>
#include "parameter.h"
//#define N 150

main() {
    double u[size_mat][size_mat], u1[size_mat][size_mat];
    double dx=0.1, dy=0.1, dx2, dy2;
    int i, j, k;
    int Niter;

    FILE *f;
    f = fopen("data_C (pure, parallel update)", "w");

    scanf("%d", &Niter);

    for(i=0; i<size_mat; i++) {
        u[i][0] = 1.0;
        for(j=1; j<size_mat; j++)
            u[i][j] = 0.0;
    }

    dx2 = dx*dx;
    dy2 = dy*dy;

    for(i=0; i<Niter; i++) {
        for(j=1; j<size_mat-1; j++)
            for(k=1; k<size_mat-1; k++)
                u1[j][k] = ((u[j+1][k] + u[j-1][k]) * dy2 +
                   (u[j][k+1] + u[j][k-1]) * dx2) * (1.0 /(2*(dx2+dy2)));

        for(j=1; j<size_mat-1; j++)
            for(k=1; k<size_mat-1; k++)
                u[j][k] = u1[j][k];
    }

    for(i=0; i<size_mat; i++)
        for(j=0; j<size_mat; j++)
            fprintf(f, "%f ", u[i][j]);

    fclose(f);
}
