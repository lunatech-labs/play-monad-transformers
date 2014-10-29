package controllers

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{ Action, Controller, Result }
import scala.concurrent.Future
import scalaz._
import scalaz.Scalaz._
import services.UserService

object Application2 extends Controller {

  def index = Action.async { request =>
    val data = request.queryString.mapValues(_.head)

    val result = for {
      username <- UserService.getUserName(data) \/> BadRequest("Username missing from request") |> Future.successful |> EitherT.apply
      user <- UserService.getUser(username).map { _ \/> NotFound("User not found") } |> EitherT.apply
      email = UserService.getEmail(user)
      validatedEmail <- UserService.validateEmail(email).leftMap(InternalServerError(_)) |> Future.successful |> EitherT.apply
      success <- UserService.sendEmail(validatedEmail).map { _.right[Result] } |> EitherT.apply
    } yield {
      if (success) Ok("Mail successfully sent!")
      else Forbidden("User email address is blacklisted")
    }

    result.run.map { _.merge }

  }

}

