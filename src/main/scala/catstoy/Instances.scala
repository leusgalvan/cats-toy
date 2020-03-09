package catstoy

import cats.Show
import cats.data.NonEmptyChain
import cats.implicits._

object Instances {
  implicit def listStringMultilineShow[A: Show] =
    Show.show[List[A]](xs => "[" + xs.map(_.show).mkString("\n") + "]")

  implicit def necStringMultilineShow[A: Show] =
    Show.show[NonEmptyChain[A]](
      xs => "[\n" + xs.reduceMap("  " + _.show + "\n") + "]"
    )

  implicit def throwableShow[E <: Throwable] = Show.show[E](_.getMessage)
}
