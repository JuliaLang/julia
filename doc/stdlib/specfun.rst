:mod:`specfun.jl` --- Special mathematical functions
====================================================

.. module:: specfun.jl
   :synopsis: Special mathematical functions.

.. function:: airy(x)
              airyai(x)

   Airy function :math:`\operatorname{Ai}(x)`.

.. function:: airyprime(x)
              airyaiprime(x)

   Airy function derivative :math:`\operatorname{Ai}'(x)`.

.. function:: airybi(x)

   Airy function :math:`\operatorname{Bi}(x)`.

.. function:: airybiprime(x)

   Airy function derivative :math:`\operatorname{Bi}'(x)`.

.. function:: besselj0(x)

   Bessel function of the first kind of order 0, :math:`J_0(x)`.

.. function:: besselj1(x)

   Bessel function of the first kind of order 1, :math:`J_1(x)`.

.. function:: besselj(nu, x)

   Bessel function of the first kind of order ``nu``, :math:`J_\nu(x)`.

.. function:: bessely0(x)

   Bessel function of the second kind of order 0, :math:`Y_0(x)`.

.. function:: bessely1(x)

   Bessel function of the second kind of order 1, :math:`Y_1(x)`.

.. function:: bessely(nu, x)

   Bessel function of the second kind of order ``nu``, :math:`Y_\nu(x)`.

.. function:: hankelh1(nu, x)

   Bessel function of the third kind of order ``nu``, :math:`H^{(1)}_\nu(x)`.

.. function:: hankelh2(nu, x)

   Bessel function of the third kind of order ``nu``, :math:`H^{(2)}_\nu(x)`.

.. function:: besseli(nu, x)

   Modified Bessel function of the first kind of order ``nu``, :math:`I_\nu(x)`.

.. function:: besselk(nu, x)

   Modified Bessel function of the second kind of order ``nu``, :math:`K_\nu(x)`.

.. function:: beta(x, y)

   Euler integral of the first kind :math:`\operatorname{B}(x,y) = \Gamma(x)\Gamma(y)/\Gamma(x+y)`.

.. function:: lbeta(x, y)

   Natural logarithm of the beta function :math:`\log(\operatorname{B}(x,y))`.

.. function:: eta(x)

   Dirichlet eta function :math:`\eta(s) = \sum^\infty_{n=1}(-)^{n-1}/n^{s}`.

.. function:: zeta(x)

   Riemann zeta function :math:`\zeta(s)`.
