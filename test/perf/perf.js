function assert(t) { if (!t) throw new Error("assertion failed"); }

// recursive fib //

function fib(n) {
    if (n < 2) return n;
    return fib(n-1) + fib(n-2);
}

tmin = Number.POSITIVE_INFINITY;
for (var i=0; i < 5; i++) {
    t = (new Date).getTime();
    for (var j=0; j < 1000; j++) {
        assert(fib(20) == 6765);
    }
    t = (new Date).getTime()-t;
    if (t < tmin) tmin = t;
}
console.log("javascript,fib," + tmin/1000);

// parse int //

tmin = Number.POSITIVE_INFINITY;
for (var i=0; i < 5; i++) {
    t = (new Date).getTime();
    for (var j=0; j < 1000*1000; j++) {
        var n = Math.floor(4294967295*Math.random());
        var s = n.toString(16);
        var m = parseInt(s,16);
        assert(m == n);
    }
    t = (new Date).getTime()-t;
    if (t < tmin) tmin = t;
}
console.log("javascript,parse_int," + tmin/1000);

// mandelbrot set //

function Complex(real, imag) {
    this.re = real;
    this.im = imag;
}
function complex_abs(z) {
    return Math.sqrt(z.re*z.re + z.im*z.im);
}
function complex_add(z,w) {
    return new Complex(z.re+w.re, z.im+w.im);
}
function complex_multiply(z,w) {
    return new Complex(z.re*w.re-z.im*w.im, z.re*w.im+z.im*w.re);
}

function mandel(z) {
    c = z;
    maxiter = 80;
    var n = 0;
    for (n = 0; n < maxiter; n++) {
        if (complex_abs(z) > 2) return n;
        z = complex_add(complex_multiply(z,z),c);
    }
    return maxiter;
}

function mandelperf() {
    var a = new Array(26*21);
    var r = 0;
    for (r = 0; r < 26; r++) {
        re = -2.0 + r*0.1;
        var i = 0;
        for (i = 0; i < 21; i++) {
            im = -1.0 + i*0.1;
            z = new Complex(re,im);
            a[r*21+i] = mandel(z);
        }
    }
    return a;
}

a = mandelperf();
i = 0;
sum = 0;
for (i = 0; i < a.length; i++) sum += a[i];
assert(sum == 14791);
a0 = a[0];

tmin = Number.POSITIVE_INFINITY;
for (var i=0; i < 5; i++) {
    t = (new Date).getTime();
    for (var j=0; j < 1000; j++) {
        a = mandelperf();
        assert(a[0] == a0);
    }
    t = (new Date).getTime()-t;
    if (t < tmin) tmin = t;
}
console.log("javascript,mandel," + tmin/1000);

// numeric vector sort //

function rand(n) {
    var v = new Array(n);
    var i = 0;
    for (i = 0; i < n; i++)
        v[i] = Math.random();
    return v;
}

function sortperf(n) {
    var v = rand(n);
    v.sort(function(a,b) { return a-b; });
    return v;
}

tmin = Number.POSITIVE_INFINITY;
for (var i=0; i < 5; i++) {
    t = (new Date).getTime();
    for (var j=0; j < 100; j++) {
        v = sortperf(5000);
        assert(a[0] < 0.99);
    }
    t = (new Date).getTime()-t;
    if (t < tmin) tmin = t;
}
console.log("javascript,quicksort," + tmin/100);

// slow pi series //

function pisum() {
    var sum = 0.0;
    for (var i=0; i < 500; i++) {
        sum = 0.0;
        for (var k=1; k <= 10000; k++)
            sum += 1.0/(k*k);
    }
    return sum;
}

tmin = Number.POSITIVE_INFINITY;
for (var i=0; i < 5; i++) {
    t = (new Date).getTime();
    for (var j=0; j < 10; j++) {
        assert(Math.abs(pisum()-1.644834071848065) < 1e-12);
    }
    t = (new Date).getTime()-t;
    if (t < tmin) tmin = t;
}
console.log("javascript,pi_sum," + tmin/10);

// random matrix statistics //

function gaussian() {
    var k = 2;
    do {
        var i = 2*Math.random()-1;
        var j = 2*Math.random()-1;
        k = i*i+j*j;
    } while (k >= 1);
    return i*Math.sqrt((-2*Math.log(k))/k);
}
function randn(n) {
    var a = new Array(n);
    var i = 0;
    for (i = 0; i < n; i++)
        a[i] = gaussian();
    return a;
}
function transpose(A,m,n) {
    var B = new Array(A.length);
    var i = 0;
    var j = 0;
    for (i = 0; i < m; i++)
        for (j = 0; j < n; j++)
            B[i*n+j] = A[j*m+i];
    return B;
}
function matmul(A,B,m,l,n) {
    var C = new Array(m*n);
    var i = 0;
    var j = 0;
    var k = 0;
    for (i = 0; i < m; i++) {
        for (j = 0; j < n; j++) {
            C[i*n+j] = 0.0;
            for (k = 0; k < l; k++) {
                C[i*n+j] += A[i*l+k]*B[k*n+j];
            }
        }
    }
    return C;
}

function randmatstat(t) {
    var n = 5;
    var v = new Array(t);
    var w = new Array(t);
    var i = 0;
    var j = 0;
    var k = 0;
    for (i = 0; i < t; i++) {
        var a = randn(n*n);
        var b = randn(n*n);
        var c = randn(n*n);
        var d = randn(n*n);
        var P = new Array(4*n*n);
        for (j = 0; j < n*n; j++) P[0*n*n+j] = a[j];
        for (j = 0; j < n*n; j++) P[1*n*n+j] = b[j];
        for (j = 0; j < n*n; j++) P[2*n*n+j] = c[j];
        for (j = 0; j < n*n; j++) P[3*n*n+j] = d[j];
        var Q = new Array(4*n*n);
        for (j = 0; j < n; j++) {
            for (k = 0; k < n; k++) {
                Q[2*n*j+k]       = a[k];
                Q[2*n*j+n+k]     = b[k];
                Q[2*n*(n+j)+k]   = c[k];
                Q[2*n*(n+j)+n+k] = d[k];
            }
        }
        P = matmul(transpose(P, n, 4*n), P, n, 4*n, n);
        P = matmul(P, P, n, n, n);
        P = matmul(P, P, n, n, n);
        var trP = 0.0;
        for (j = 0; j < n; j++) trP += P[(n+1)*j];
        v[i] = trP;
        Q = matmul(transpose(Q, 2*n, 2*n), Q, 2*n, 2*n, 2*n);
        Q = matmul(Q, Q, 2*n, 2*n, 2*n);
        Q = matmul(Q, Q, 2*n, 2*n, 2*n);
        var trQ = 0.0;
        for (j = 0; j < 2*n; j++) trQ += Q[(2*n+1)*j];
        v[i] = trQ;
    }
    var v1 = 0.0;
    var v2 = 0.0;
    var w1 = 0.0;
    var w2 = 0.0;
    for (i = 0; i < t; i++) {
        v1 += v[i]; v2 += v[i]*v[i];
        w1 += w[i]; w2 += w[i]*w[i];
    }
    var r = new Object();
    r.s1 = Math.sqrt((t*(t*v2-v1*v1))/((t-1)*v1*v1));
    r.s2 = Math.sqrt((t*(t*w2-w1*w1))/((t-1)*w1*w1));
    return r;
}

tmin = Number.POSITIVE_INFINITY;
for (var i=0; i < 5; i++) {
    t = (new Date).getTime();
    for (var j=0; j < 10; j++) {
        var r = randmatstat(1000);
        // assert(0.5 < r.s1 < 1.0);
        assert(0.5 < r.s2 < 1.0);
    }
    t = (new Date).getTime()-t;
    if (t < tmin) tmin = t;
}
console.log("javascript,rand_mat_stat," + tmin/10);

// random matrix multiply //

function randmatmul(n) {
    var A = rand(n*n);
    var B = rand(n*n);
    return matmul(A, B, n, n, n);
}

tmin = Number.POSITIVE_INFINITY;
t = (new Date).getTime();
var C = randmatmul(1000);
assert(0 <= C[0]);
t = (new Date).getTime()-t;
if (t < tmin) tmin = t;
console.log("javascript,rand_mat_mul," + tmin);
