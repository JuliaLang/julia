function assert(t) { if (!t) throw new Error("assertion failed"); }

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

function pisum() {
    var sum = 0.0;
    for (var i=0; i < 500; i++) {
        sum = 0.0;
        for (var k=1; k <= 10000; k++) {
            sum += 1.0/(k*k);
        }
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
