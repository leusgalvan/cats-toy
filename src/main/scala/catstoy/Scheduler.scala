package catstoy

import cats.ApplicativeError

object Scheduler {
  def retry[M[_], E, A](
    m: () => M[A],
    times: Int,
    maybeMessage: Option[String]
  )(implicit ap: ApplicativeError[M, E]): M[A] = {
    println("In retry function...")

    if (times <= 0) m()
    else {
      ap.handleErrorWith(m()) { _ =>
        maybeMessage.foreach(println)
        retry(m, times - 1, maybeMessage)
      }
    }
  }
}
