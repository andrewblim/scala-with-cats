package com.example.swc

import cats.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._

object SuperAdder {
  def add(items: List[Int])(implicit intMonoid: Monoid[Int]): Int =
    items.foldLeft(intMonoid.empty)(_ |+| _)

  // "add2" because of type erasure
  def add2(items: List[Option[Int]])(implicit optionMonoid: Monoid[Option[Int]]): Option[Int] =
    items.foldLeft(optionMonoid.empty)(_ |+| _)

  case class Order(totalCost: Double, quantity: Double)
  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    def empty: Order = Order(0.0, 0.0)
    def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  def add3(items: List[Order])(implicit orderMonoid: Monoid[Order]): Order =
    items.foldLeft(orderMonoid.empty)(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(add(List(1, 2, 3)))
    println(add2(List(Some(1), Some(2), None)))
    println(add2(List(Some(1), Some(2), Some(3))))
    println(add3(List(Order(1.0, 2.0), Order(3.0, 4.0))))
  }
}
