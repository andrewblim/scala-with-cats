package com.example.swc

trait Printable[A] {
  self =>

  def format(x: A): String

  def contramap[B](func: B => A): Printable[B] = new Printable[B] {
    def format(x: B): String = self.format(func(x))
  }
}

object PrintableInstances {
  implicit val printableString: Printable[String] = new Printable[String] {
    def format(x: String): String = x
  }
  implicit val printableInt: Printable[Int] = new Printable[Int] {
    def format(x: Int): String = x.toString + " (integer)"
  }
}

object PrintableInstances2 {
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    def format(x: String): String = "\"" + x + "\""
  }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    def format(x: Boolean): String = if (x) "yes" else "no"
  }

  implicit def boxPrintable[A](implicit printable: Printable[A]): Printable[Box[A]] =
    printable.contramap(_.value)
}

final case class Box[A](value: A)

object Printable {
  def format[A](x: A)(implicit printable: Printable[A]): String =
    printable.format(x)

  def print[A](x: A)(implicit printer: Printable[A]): Unit =
    println(format(x))

  def main(args: Array[String]): Unit = {
//    import PrintableInstances._
    import PrintableInstances2._

    print("foobar")
    print(true)
    print(Box("hello world"))
    print(Box(true))

    // print(Box(123)) does not compile
  }
}