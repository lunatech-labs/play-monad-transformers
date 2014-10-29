package controllers

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{ Action, Controller }
import scala.concurrent.Future
import services.UserService

/**
 * Original version, with `map` and `flatMap`.
 *
 * This is even using direct pattern matching on `Future` contents, to avoid double maps there.
 *
 * Note especially how far the `getOrElse` with the error is from the problem, in the outer maps.
 */
object Application1 extends Controller {

  def index = Action.async { request =>
    val data = request.queryString.mapValues(_.head)

    UserService.getUserName(data).map { username =>
      UserService.getUser(username).flatMap {
        case None => Future.successful(NotFound("User not found"))
        case Some(user) => {
          val email = UserService.getEmail(user)
          UserService.validateEmail(email).bimap(
            errorMsg => Future.successful(InternalServerError(errorMsg)),
            validatedEmail => {
              UserService.sendEmail(validatedEmail) map {
                case true => Ok("Mail successfully sent!")
                case false => Forbidden("User email address is blacklisted")
              }
            }).merge
        }
      }
    } getOrElse Future.successful(BadRequest("Username missing from data!"))
  }
}
