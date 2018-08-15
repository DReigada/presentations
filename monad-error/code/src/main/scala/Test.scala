import cats.MonadError
import cats.implicits._

import scala.concurrent
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
object Test extends App {

  def divide(num: Int, denom: Int): Int = num / denom

  def divideOpt(num: Int, denom: Int): Option[Int] =
    if (denom == 0) None else Some(num / denom)

  def divideTry(num: Int, denom: Int): Try[Int] =
    if (denom == 0) Failure(new Throwable("Division by 0"))
    else Success(num / denom)

  def divideFuture(num: Int, denom: Int): Future[Int] =
    if (denom == 0) Future.failed(new Throwable("Division by 0"))
    else Future.successful(num / denom)

  def divideEither(num: Int, denom: Int): Either[String, Int] =
    if (denom == 0) Left("Division by 0")
    else Right(num / denom)

  def divideF[F[_]](num: Int, denom: Int)(
      implicit M: MonadError[F, Throwable]): F[Int] = {
    if (denom == 0) M.raiseError(new Throwable("Division by 0"))
    else M.pure(num / denom)
  }

  for {
    num <- Try(1)
    denom <- Try(2)
    result <- divideF[Try](num, denom)
  } yield result

  MonadError[Try, Throwable].pure(1)

  MonadError[Try, Throwable].raiseError(new Throwable("error"))

  MonadError[Try, Throwable].catchNonFatal(1 / 0)

  MonadError[Try, Throwable].fromEither(Right(123))
  MonadError[Try, Throwable].fromOption(None, new Throwable("empty"))

  val M = MonadError[Try, Throwable]

  MonadError[Try, Throwable]
    .catchNonFatal(1 / 0)
    .recover {
      case _ : ArithmeticException => 0
    }

  MonadError[Try, Throwable].pure(123).attempt
  MonadError[Try, Throwable].raiseError(new Throwable("error")).attempt

}
