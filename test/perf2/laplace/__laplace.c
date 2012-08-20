void c_update(double **u, unsigned long rows, unsigned long cols, double dx2, double dy2) {
    unsigned long i, j;
    for (i=1; i<rows-1; i++)
        for (j=1; j<cols-1; j++)
            u[i][j] = ((u[i+1][ j] + u[i-1][ j]) * dy2 + (u[i][ j+1] + u[i][ j-1]) * dx2) * (1.0 / (2*(dx2+dy2)));
}
