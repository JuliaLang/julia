// This file is a part of Julia. License is MIT: http://julialang.org/license

import scala.util._
import java.io._
import breeze.linalg._
import breeze.numerics._
import breeze.stats._
import breeze.math._
//import com.github.fommil.netlib.{BLAS}

object PerfBreeze {
  final val NITER = 5

  // print results appropriately. times are in milliseconds
  def print_perf(name:String, t:Double) = {
    printf("scala,%s,%.9f\n", name, t/1e6)
  }

  // time fib
  def fib(n:Int):Int = {
    if (n < 2) n else fib(n-1) + fib(n-2)
  }

  def time_fib() = {
    assert(fib(20) == 6765)
    var tmin = Long.MaxValue
    var f = 0

    for(i <- 1 to NITER) {
      val t1 = System.nanoTime()
      for(j <- 1 to 1000) {
        f += fib(20)
      }
      val t = System.nanoTime() - t1
      if(t < tmin) tmin = t
    }

    tmin / 1000.0
  }

  // time parseint
  def time_parseint() = {
    val generator = scala.util.Random
    var tmin = Long.MaxValue

    for(i <- 1 to NITER) {
      var rand:Int = 0
      var rands:String = "0"
      var parsed:Int = 0
      val t1 = System.nanoTime()
      for(j <- 1 to 1000) {
        rand = generator.nextInt()
        rands = rand.toString
        parsed = rands.toInt
      }
      val t = System.nanoTime() - t1
      assert(rand == parsed)
      if(t < tmin) tmin = t
    }
    tmin / 1000.0
  }

  // time mandel
  def mandel(zin:Complex):Int = {
    val c = zin
    var z = zin
    val maxiter = 80
    for(n <- 0 to maxiter) {
      if(z.abs > 2) return n
      z = c + (z * z)
    }
    maxiter
  }

  def mandelperf() = {
    var mandel_sum = 0
    for(re <- -20 to 5) {
      for(im <- -10 to 10) {
        val m = mandel(re/10.0 + i * im/10.0)
        mandel_sum += m
      }
    }
    mandel_sum
  }

  def time_mandel() = {
    var mandel_sum = 0
    var mandel_sum2 = 0
    var tmin = Long.MaxValue

    for(i <- 1 to NITER) {
      val t1 = System.nanoTime()
      for(j <- 1 to 100) {
        mandel_sum = mandelperf()
        mandel_sum2 += mandel_sum
      }
      val t = System.nanoTime() - t1
      if(t < tmin) tmin = t
    }
    assert(mandel_sum == 14791)
    assert(mandel_sum2 == mandel_sum * 100 * NITER)
    tmin / 100.0
  }

  // time quicksort
  def time_quicksort() = {
    var tmin = Long.MaxValue

    for(i <- 1 to NITER) {
      val t1 = System.nanoTime()
      for(j <- 1 to 1000) {
        val A = randomInt(5000, (1, Int.MaxValue))
        Sorting.quickSort(A.data)
      }
      val t = System.nanoTime() - t1
      if(t < tmin) tmin = t
    }
    tmin / 1000.0
  }

  // time pisum
  def pisum() = {
    var sum = 0.0
    for(j <- 1 to 500) {
      sum = 0.0
      for(k <- 1 to 10000) {
        sum += 1.0/(k*k)
      }
    }
    sum
  }

  def time_pisum() = {
    var tmin = Long.MaxValue
    var pi = 0:Double
    for(i <- 1 to NITER) {
      val t1 = System.nanoTime()
      pi = pisum()
      val t = System.nanoTime() - t1
      if(t < tmin) tmin = t
      assert(abs(pi-1.644834071848065) < 1e-12)
    }
    tmin
  }

  // time printfd
  def printfd(n:Int) = {
    var stream = None: Option[PrintStream]
    try {
      stream = Some(new PrintStream(new BufferedOutputStream(new FileOutputStream("/dev/null"))))
      val valid_stream = stream.get
      for (i <- 1 to n) {
        valid_stream.printf(i + " " + i)
      }
    } catch {
      case e: Exception => println("Exception caught: " + e)
    } finally {
      if(stream.isDefined) stream.get.close()
    }
  }

  def time_printfd() = {
    var tmin = Long.MaxValue
    for(i <- 1 to NITER) {
      val t1 = System.nanoTime()
      printfd(100000)
      val t = System.nanoTime() - t1
      if(t < tmin) tmin = t
    }
    tmin
  }

  // random matrix statistics
  def randmatstat(t:Int):(Double,Double) = {
    val n = 5
    val v = DenseVector.zeros[Double](t)
    val w = DenseVector.zeros[Double](t)

    val g = breeze.stats.distributions.Gaussian(0, 1)
    for(i <- 0 to t-1) {
      val a = DenseMatrix.rand(n, n, g)
      val b = DenseMatrix.rand(n, n, g)
      val c = DenseMatrix.rand(n, n, g)
      val d = DenseMatrix.rand(n, n, g)
      val P = DenseMatrix.horzcat(a, b, c, d)
      val Q = DenseMatrix.vertcat(DenseMatrix.horzcat(a, b), DenseMatrix.horzcat(c, d))
      val V = P.t * P
      val W = Q.t * Q

      v(i) = trace(V * V * V * V)
      w(i) = trace(W * W * W * W)
    }
    (stddev(v)/mean(v), stddev(w)/mean(w))
  }

  def time_randmatstat() = {
    var tmin = Long.MaxValue
    for(i <- 1 to NITER) {
      val t1 = System.nanoTime()
      val (s1, s2) = randmatstat(1000)
      val t = System.nanoTime() - t1
      assert(0.5 < s1 && s1 < 1.0 && 0.5 < s2 && s2 < 1.0)

      if(t < tmin) tmin = t
    }
    tmin
  }

  // random matrix multiplication
  def randmatmul(t:Int):DenseMatrix[Double] = {
    val m1 = randomDouble((t, t))
    val m2 = randomDouble((t, t))
    m1 * m2
  }

  def time_randmatmul() = {
    var tmin = Long.MaxValue
    for(i <- 1 to NITER) {
      val t1 = System.nanoTime()
      val m = randmatmul(1000)
      val t = System.nanoTime() - t1
      assert(0 <= m(0,0))

      if(t < tmin) tmin = t
    }
    tmin
  }


  def main(args: Array[String]) = {
    //println("BLAS: " + BLAS.getInstance().getClass().getName())
    print_perf("fib", time_fib())
    print_perf("parse_int", time_parseint())
    print_perf("mandel", time_mandel())
    print_perf("quicksort", time_quicksort())
    print_perf("pi_sum", time_pisum())
    print_perf("rand_mat_stat", time_randmatstat())
    print_perf("rand_mat_mul", time_randmatmul())
    print_perf("printfd", time_printfd())
  }
}
