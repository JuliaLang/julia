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
    for (var j=0; j < 100000; j++) {
        assert(parseInt("1111000011110000111100001111",2) == 252645135);
    }
    t = (new Date).getTime()-t;
    if (t < tmin) tmin = t;
}
console.log("javascript,parse_int," + tmin/100000);

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

function sortperf(n) {
    v = new Array(n);
    var i = 0;
    for (i = 0; i < n; i++)
        v[i] = Math.random();
    v.sort();
    return v;
}

tmin = Number.POSITIVE_INFINITY;
for (var i=0; i < 5; i++) {
    t = (new Date).getTime();
    for (var j=0; j < 10; j++) {
        v = sortperf(5000);
        assert(a[0] < 0.99);
    }
    t = (new Date).getTime()-t;
    if (t < tmin) tmin = t;
}
console.log("javascript,quicksort," + tmin/10);

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
