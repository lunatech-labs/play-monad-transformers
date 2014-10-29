package controllers

import play.api.data.Form
import play.api.data.validation.ValidationError
import play.api.libs.json.{ JsPath, JsResult }
import play.api.mvc.{ Action, Controller, Result }
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try
import scala.util.control.NonFatal
import scalaz._
import scalaz.Scalaz._
import services.UserService

object Application3 extends Controller {

  def index = Action.async { request =>
    import play.api.libs.concurrent.Execution.Implicits.defaultContext

    val data = request.queryString.mapValues(_.head)

    val serviceResult = for {
      username <- UserService.getUserName(data) |> HttpResult.fromOption(BadRequest("Username missing from request"))
      user <- UserService.getUser(username) |> HttpResult.fromFOption(NotFound("User not found"))
      email = UserService.getEmail(user)
      validatedEmail <- UserService.validateEmail(email) |> HttpResult.fromEither(InternalServerError(_))
      success <- UserService.sendEmail(validatedEmail) |> HttpResult.fromFuture
      _ <- HttpResult.require(success, Forbidden("User email address is blacklisted"))
    } yield Ok("Mail successfully sent!")

    constructPlayResult(serviceResult)
  }

  type Failure = Result
  type HttpResult[A] = EitherT[Future, Failure, A]

  object HttpResult {

    def apply[A](a: Future[Failure \/ A]): HttpResult[A] = EitherT(a)
    def point[A](a: A): HttpResult[A] = EitherT(Future.successful(\/.right(a)))
    def fail[A](failure: Failure): HttpResult[A] = EitherT(Future.successful(\/.left(failure)))
    def require(test: Boolean, failure: Failure): HttpResult[Unit] = if (test) point(()) else fail(failure)

    def fromFuture[A](fa: Future[A])(implicit ec: ExecutionContext): HttpResult[A] = EitherT(fa.map(\/.right(_)))
    def fromTry[A](failure: Throwable => Failure)(ta: Try[A]): HttpResult[A] = ta match {
      case scala.util.Success(a) => EitherT(Future.successful(\/.right(a)))
      case scala.util.Failure(NonFatal(t)) => EitherT(Future.successful(\/.left(failure(t))))
    }
    def fromResultEither[A](va: Failure \/ A): HttpResult[A] = EitherT(Future.successful(va))
    def fromEither[A, B](failure: B => Failure)(va: B \/ A): HttpResult[A] = EitherT(Future.successful(va.leftMap(failure)))
    def fromOption[A](failure: => Failure)(oa: Option[A]): HttpResult[A] = EitherT(Future.successful(oa \/> failure))
    def fromFOption[A](failure: => Failure)(foa: Future[Option[A]])(implicit ec: ExecutionContext): HttpResult[A] = EitherT(foa.map(_ \/> failure))
    def fromFEither[A, B](failure: B => Failure)(fva: Future[B \/ A])(implicit ec: ExecutionContext): HttpResult[A] = EitherT(fva.map(_.leftMap(failure)))

    // The parameter needs to be by-name, otherwise the exception won't be caught inside this function. But that doesn't
    // work with the |> operator from Scalaz, so we switch the parameter order here.
    def fromTryCatch[A](a: => A)(failure: Throwable => Failure): HttpResult[A] = EitherT(Future.successful(\/.fromTryCatchNonFatal(a).leftMap(failure(_))))

    def fromFutureRecovering[A](recover: Throwable => Failure)(fa: Future[A])(implicit ec: ExecutionContext): HttpResult[A] =
      EitherT(fa.map(\/.right(_)).recover {
        case NonFatal(t) => \/.left(recover(t))
      })

    def fromForm[A](failure: Form[A] => Failure)(form: Form[A]): HttpResult[A] = EitherT(form.fold(
      formWithErrors => Future.successful(\/.left(failure(formWithErrors))),
      value => Future.successful(\/.right(value))))

    type JsErrors = Seq[(JsPath, Seq[ValidationError])]

    def fromJsResult[A](failure: JsErrors => Failure)(result: JsResult[A]): HttpResult[A] = EitherT(Future.successful(result.fold(
      invalid => \/.left(failure(invalid)),
      valid => \/.right(valid))))

  }

  // Converter from our HttpResult type to a Play result
  def constructPlayResult(result: HttpResult[Result])(implicit ec: ExecutionContext) = result.run.map { _.merge }

}

