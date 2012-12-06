#include <sys/time.h>

#include <utility>
#include <iostream>
#include <complex>
#include <cstdio>
#include <cstdlib>
#include <eigen3/Eigen/Dense>

using namespace Eigen;
typedef std::complex<double> cdouble;
int c = 0;

int mandel(cdouble z) {
    const cdouble c = z;
    for (int n = 0; n != 80; ++n) {
        if (std::abs(z) > 2) return n-1;
        z = z*z + c;
    }
    return 80;
}

int mandelperf() {
    int sum = 0;
    for (double re=-2.0; re<=0.5; re+=0.1) {
        for (double im=-1.0; im<=1.0; im+=0.1) {
            sum += mandel(cdouble(re, im));
        }
    }
    // This is necessary because otherwise, the compiler optimises away the
    // whole call to ``mandelperf`` as it does nothing if we ignore its return
    // value.
    c = sum;
    return sum;
}

int fib(int n) {
    if (n < 2) return 1;
    return fib(n-2) + fib(n-1);
}


std::vector<double> randv(const int n) {
    std::vector<double> res;
    res.reserve(n);
    for (int i = 0; i !=n ;++i) res.push_back(std::rand()/12324.);
    return res;
}

double std_dev_over_mean(const VectorXf vec) {
    double mean = vec.mean();
    double std = (vec.array() - mean).matrix().norm();
    return std/mean;
}

template <typename T>
auto pow4(const T& input) -> decltype((input*input) * (input*input)) {
    auto p2 = input * input;
    return p2 * p2;
}


double randmul(int n) {
    MatrixXd A = MatrixXd::Random(n,n);
    MatrixXd B = MatrixXd::Random(n,n);
    MatrixXd C = A*B;
    return C.trace();
}

std::pair<double, double> randmatstat(int t) {
    const int n = 5;
    typedef Matrix<double, n, n> Matn;
    VectorXf v(t);
    VectorXf w(t);
    for (int i = 0; i != t; ++i) {
        Matn a = Matn::Random();
        Matn b = Matn::Random();
        Matn c = Matn::Random();
        Matn d = Matn::Random();

        Matrix<double, n,4*n> P;
        P << a, b, c, d;
        v[i] = pow4(P.transpose() * P).trace();

        Matrix<double, 2*n,2*n> Q;
        Q << a,b,c,d;
        w[i] = pow4(Q.transpose() * Q).trace();
    }
    return std::make_pair(std_dev_over_mean(v), std_dev_over_mean(w));
}


double clock_now() {
    struct timeval now;
    gettimeofday(&now, NULL);
    return (double)now.tv_sec + (double)now.tv_usec/1.0e6;
}

const int NITER = 5;

template<typename T>
double execute_test(const char* name, const T& f) {
    double tmin = INFINITY;

    for (int i=0; i<NITER; ++i) {
        double t = clock_now();
        f();
        t = clock_now()-t;
        if (t < tmin) tmin = t;
    }
    std::printf("cpp,%s,%.6f\n", name, tmin*1000);
    return tmin;
}

int main() {
    using namespace std;
    execute_test("fib", [](){fib(20);});
    execute_test("mandelsum", mandelperf);
    execute_test("quicksort", []() { 
        std::vector<double> d = randv(5000);
        std::sort(d.begin(), d.end());
    });

    execute_test("randmatstat", []() {
        pair<double,double> r = randmatstat(1000);
    });

    execute_test("randmul", []() {
        randmul(1000);
    });

    return 0;
}

