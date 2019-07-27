package com.example.swc

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import cats.data.Writer
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.instances.vector._

object WriterFactorial {
  def slowly[A](body: => A): A =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else factorial(n - 1) * n)
    println(s"fact $n $ans")
    ans
  }

  def factorialLogged(n: Int): Logged[Int] = {
    for {
      ans <- slowly(if (n == 0) 1.pure[Logged] else factorialLogged(n - 1).map(_ * n))
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  type Logged[A] = Writer[Vector[String], A]

  def main(args: Array[String]): Unit = {
//    factorial(5)

//    Await.result(
//      Future.sequence(Vector(Future(factorial(3)), Future(factorial(3)))),
//      5.seconds
//    )

    val (log, res) = factorialLogged(5).run
    println(log)
    println(res)

    val Vector((logA, ansA), (logB, ansB)) = Await.result(
      Future.sequence(Vector(Future(factorialLogged(3).run), Future(factorialLogged(3).run))),
      5.seconds
    )
    println(logA)
    println(ansA)
    println(logB)
    println(ansB)

//    println(42.pure[Logged])
//    println(Vector("message").tell)
//    println(41.pure[Logged].map(_ + 1))
  }
}
