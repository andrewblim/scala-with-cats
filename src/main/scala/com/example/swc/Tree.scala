package com.example.swc

import cats.Functor
import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case br: Branch[A] => Branch(map(br.left)(f), map(br.right)(f))
      case leaf: Leaf[A] => Leaf(f(leaf.value))
    }
  }

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    def pure[A](value: A): Tree[A] = Leaf(value)

    def flatMap[A, B](tree: Tree[A])(func: A => Tree[B]): Tree[B] =
      tree match {
        case Branch(l, r) => Branch(flatMap(l)(func), flatMap(r)(func))
        case Leaf(value) => func(value)
      }

    def tailRecM[A, B](a: A)(func: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(func(a)) {
        case Left(value) => tailRecM(value)(func)
        case Right(value) => Leaf(value)
      }
  }

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  def main(args: Array[String]): Unit = {
    val leaf1 = Leaf(1)
    val leaf2 = Leaf(2)
    val leaf3 = Leaf(3)
    val branch1: Tree[Int] = Branch(leaf1, leaf2) // invariance problem
    val branch2: Tree[Int] = Branch(branch1, leaf3)

    println(branch2)
    println(branch2.map(x => x + 1))

    val t = branch(leaf(100), leaf(200))
    println(t)
    println(t.flatMap(x => branch(leaf(x - 1), leaf(x + 1))))

    // different way of dealing w/type inference
    val t2 = Branch(Leaf(100), Leaf(200))
    println(t2)
    println((t2: Tree[Int]).flatMap(x => branch(leaf(x - 1), leaf(x + 1))))
  }
}