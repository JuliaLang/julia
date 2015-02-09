public class Complex {
    private final double re;
    private final double im;

    public Complex(double real, double imag) {
        re = real;
        im = imag;
    }

    public static double abs(Complex z) {
        return Math.sqrt(z.re*z.re + z.im*z.im);
    }

    public static Complex add(Complex a, Complex b) {
        return new Complex(a.re + b.re, a.im + b.im);
    }

    public static Complex mul(Complex a, Complex b) {
        return new Complex(a.re*b.re - a.im*b.im, a.re*b.im + a.im*b.re);
    }
}
