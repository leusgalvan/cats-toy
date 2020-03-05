package catstoy

import cats.ApplicativeError

object Scheduler {
  def retry[M[_], E, A](m: M[A], times: Int)(implicit applicativeErrorEv: ApplicativeError[M, E]): M[A] = {
    if(times <= 0) m
    else ApplicativeError[M,E].handleErrorWith(m)(_ => retry(m, times-1))
  }
}