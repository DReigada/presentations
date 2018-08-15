import cats.MonadError
import cats.implicits._

import scala.util.Try
trait UIError[A] {
  def errorFromString(str: String): A

  def errorFromThrowable(thr: Throwable): A
}

object UIError {
  def apply[A](implicit error: UIError[A]): UIError[A] = error

  def instance[A](strF: String => A, thrF: Throwable => A): UIError[A] =
    new UIError[A] {
      override def errorFromString(str: String): A = strF(str)

      override def errorFromThrowable(thr: Throwable): A = thrF(thr)
    }

  implicit val throwableInstance: UIError[Throwable] =
    instance(str => new Throwable(str), identity)

  implicit val stringInstance: UIError[String] =
    instance(identity, throwableToString)

  private def throwableToString(thr: Throwable): String = {
    s"""${thr.getMessage}: ${thr.getCause}
       |${thr.getStackTrace}
     """.stripMargin
  }

  def divideF[F[_], E](num: Int, denom: Int)(
    implicit M: MonadError[F, E], Err: UIError[E]): F[Int] = {
    if (denom == 0) M.raiseError(Err.errorFromString("Division by 0"))
    else M.pure(num / denom)
  }

  divideF[Try, Throwable](1, 0)

  divideF[Either[Throwable, ?], Throwable](1, 0)

  divideF[Either[String, ?], String](1, 0)


}
