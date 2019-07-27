package com.example.swc

import cats.Eval

object EvalFoldRight {
  // not stack safe
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil => acc
    }

  // stack safe
  def foldRightStackSafeHelper[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightStackSafeHelper(tail, acc)(fn)))
      case Nil => acc
    }

  def foldRightStackSafe[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightStackSafeHelper(as, Eval.now(acc)) {
      (a, b) => b.map(fn(a, _))
    }.value
}
