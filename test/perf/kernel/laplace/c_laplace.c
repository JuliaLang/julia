#include<stdio.h>
#include <time.h>

main() {
    int size_mat = 150;
    int Niter=1024;
    double u[size_mat][size_mat];
    double dx=0.1, dy=0.1, dx2, dy2;
    int i, j, k;

    time_t t1 = clock();

    for(i=0; i<size_mat; i++) {
        u[i][0] = 1.0;
        for(j=1; j<size_mat; j++)
            u[i][j] = 0.0;
    }

    dx2 = dx*dx;
    dy2 = dy*dy;

    for(i=0; i<Niter; i++)
        for(j=1; j<size_mat-1; j++)
            for(k=1; k<size_mat-1; k++)
                u[j][k] = ((u[j+1][k] + u[j-1][k]) * dy2 +
                   (u[j][k+1] + u[j][k-1]) * dx2) * (1.0 / (2*(dx2+dy2)));

    t1 = clock() - t1;
    printf("c_laplace\t%f ms\n", 1000*t1 / (double) CLOCKS_PER_SEC);

}
