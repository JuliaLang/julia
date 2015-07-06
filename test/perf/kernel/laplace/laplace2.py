import time

dx = 0.1
dy = 0.1
dx2 = dx*dx
dy2 = dy*dy

def py_update(u,nx,ny):
    for i in xrange(1,nx-1):
        for j in xrange(1, ny-1):
            u[i][j] = ((u[i+1][j] + u[i-1][j]) * dy2 +
                      (u[i][j+1] + u[i][j-1]) * dx2) / (2*(dx2+dy2))

def calc(N, Niter=100):
    u = [[0.0]*N for i in xrange(N)]
    for i in xrange(N):
        u[0][i] = 1.0
    for i in range(Niter):
        py_update(u,N,N)
    return u

start = time.time()
u = calc(150,8000)
elapsed = time.time() - start
print "Python: %f seconds" % elapsed

import pickle
f = open('myfile.pkl', 'w')
pickle.dump(u, f)
f.close()
