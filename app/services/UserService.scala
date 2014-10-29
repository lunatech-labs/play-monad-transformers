package services

import models._
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scalaz.{ \/, \/-, -\/ }

object UserService {

  private val users = Seq(
    User("eamelink", "erik.bakker@lunatech.com"),
    User("sietse", "sietse"), // Note: sietse has an invalid email address!
    User("paco", "francisco.canedo@lunatech.com"))

  private val emailBlacklist = Seq(
    "francisco.canedo@lunatech.com")

  /**
   * Extract the username from a Map with data
   */
  def getUserName(data: Map[String, String]): Option[String] =
    data.get("username")

  /**
   * Find the user with a given username.
   */
  def getUser(username: String): Future[Option[User]] =
    Future(users.find(_.username == username))

  /**
   * Get the email address of a user
   */
  def getEmail(user: User): String = user.email

  /**
   * Validate an email address.
   * Returns a -\/ with an error message if `email` is not a valid email address,
   * or a copy of the email address if it is.
   */
  def validateEmail(email: String): String \/ String =
    if (email contains "@")
      \/-(email)
    else
      -\/("Not a valid email address: " + email)

  /**
   * Send an email.
   * Returns true if successful, false if the email address is in our email blacklist.
   */
  def sendEmail(email: String): Future[Boolean] =
    Future.successful {
      !(emailBlacklist contains email)
    }

}