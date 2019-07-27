package com.example.swc

import cats.{Show, Eq}

import cats.syntax.show._
import cats.syntax.eq._

import cats.instances.string._
import cats.instances.int._
import cats.instances.option._

final case class Cat(name: String, age: Int, color: String)

object Cat {
  implicit val showCat: Show[Cat] = Show.show[Cat] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year old $color cat"
  }

  implicit val eqCat: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
    cat1.name === cat2.name && cat1.age === cat2.age && cat1.color === cat2.color
  }

  def main(args: Array[String]): Unit = {
    val cat1 = Cat("Garfield", 38, "orange and black")
    println(cat1.show)

    val cat2 = Cat("Heathcliff", 33, "orange and black")
    println(cat1 === cat2)
    println(Option(cat1) == Option.empty[Cat])
    println(cat1 === cat1)
    println(Option(cat2) === Option(cat2))
  }
}
