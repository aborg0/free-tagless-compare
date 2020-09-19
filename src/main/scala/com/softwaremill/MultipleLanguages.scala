package com.softwaremill

import java.util.UUID

import cats.data.{EitherK => Coproduct}
import cats.free.Free
import cats.implicits._
import cats.{Monad, ~>, InjectK => Inject}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object MultipleLanguages {

  case class User(id: UUID, email: String, loyaltyPoints: Int)

  object Initial {

    trait UserRepository {
      def findUser(id: UUID): Future[Option[User]]

      def updateUser(u: User): Future[Unit]
    }

    trait EmailService {
      def sendEmail(email: String, subject: String, body: String): Future[Unit]
    }

    class LoyaltyPoints(ur: UserRepository, es: EmailService) {
      def addPoints(userId: UUID, pointsToAdd: Int): Future[Either[String, Unit]] = {
        ur.findUser(userId).flatMap {
          case None => Future.successful(Left("User not found"))
          case Some(user) =>
            val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
            for {
              _ <- ur.updateUser(updated)
              _ <- es.sendEmail(user.email, "Points added!", s"You now have ${updated.loyaltyPoints}")
            } yield Right(())
        }
      }
    }

  }

  object UsingFree {

    sealed trait UserRepositoryAlg[T]

    case class FindUser(id: UUID) extends UserRepositoryAlg[Option[User]]

    case class UpdateUser(u: User) extends UserRepositoryAlg[Unit]

    class Users[F[_]](implicit i: Inject[UserRepositoryAlg, F]) {
      def findUser(id: UUID): Free[F, Option[User]] = Free.inject(FindUser(id))

      def updateUser(u: User): Free[F, Unit] = Free.inject(UpdateUser(u))
    }

    object Users {
      implicit def users[F[_]](implicit i: Inject[UserRepositoryAlg, F]): Users[F] = new Users
    }

    //

    sealed trait EmailAlg[T]

    case class SendEmail(email: String, subject: String, body: String) extends EmailAlg[Unit]

    class Emails[F[_]](implicit i: Inject[EmailAlg, F]) {
      def sendEmail(email: String, subject: String, body: String): Free[F, Unit] = Free.inject(SendEmail(email, subject, body))
    }

    object Emails {
      implicit def emails[F[_]](implicit i: Inject[EmailAlg, F]): Emails[F] = new Emails
    }

    //

    type UserAndEmailAlg[T] = Coproduct[UserRepositoryAlg, EmailAlg, T]

    def addPoints(userId: UUID, pointsToAdd: Int)(
      implicit ur: Users[UserAndEmailAlg], es: Emails[UserAndEmailAlg]): Free[UserAndEmailAlg, Either[String, Unit]] = {

      ur.findUser(userId).flatMap {
        case None => Free.pure(Left("User not found"))
        case Some(user) =>
          val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)

          for {
            _ <- ur.updateUser(updated)
            _ <- es.sendEmail(user.email, "Points added!", s"You now have ${updated.loyaltyPoints}")
          } yield Right(())
      }
    }

    val futureUserInterpreter = new (UserRepositoryAlg ~> Future) {
      override def apply[A](fa: UserRepositoryAlg[A]): Future[A] = fa match {
        case FindUser(id) => /* go and talk to a database */ Future.successful(None)
        case UpdateUser(u) => /* as above */ Future.successful(())
      }
    }

    val futureEmailInterpreter = new (EmailAlg ~> Future) {
      override def apply[A](fa: EmailAlg[A]): Future[A] = fa match {
        case SendEmail(email, subject, body) => /* use smtp */ Future.successful(())
      }
    }

    val futureUserOrEmailInterpreter = futureUserInterpreter or futureEmailInterpreter

    val result: Future[Either[String, Unit]] =
      addPoints(UUID.randomUUID(), 10).foldMap(futureUserOrEmailInterpreter)
  }

  object UsingTagless {

    trait UserRepositoryAlg[F[_]] {
      def findUser(id: UUID): F[Option[User]]

      def updateUser(u: User): F[Unit]
    }

    trait EmailAlg[F[_]] {
      def sendEmail(email: String, subject: String, body: String): F[Unit]
    }

    class LoyaltyPoints[F[_] : Monad](ur: UserRepositoryAlg[F], es: EmailAlg[F]) {
      def addPoints(userId: UUID, pointsToAdd: Int): F[Either[String, Unit]] = {
        ur.findUser(userId).flatMap {
          case None => implicitly[Monad[F]].pure(Left("User not found"))
          case Some(user) =>
            val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
            for {
              _ <- ur.updateUser(updated)
              _ <- es.sendEmail(user.email, "Points added!", s"You now have ${updated.loyaltyPoints}")
            } yield Right(())
        }
      }
    }

    trait FutureUserInterpreter extends UserRepositoryAlg[Future] {
      override def findUser(id: UUID): Future[Option[User]] =
        Future.successful(None) /* go and talk to a database */

      override def updateUser(u: User): Future[Unit] =
        Future.successful(()) /* as above */
    }

    trait FutureEmailInterpreter extends EmailAlg[Future] {
      override def sendEmail(email: String, subject: String, body: String): Future[Unit] =
        Future.successful(()) /* use smtp */
    }

    val result: Future[Either[String, Unit]] =
      new LoyaltyPoints(new FutureUserInterpreter {}, new FutureEmailInterpreter {}).addPoints(UUID.randomUUID(), 10)

  }

  object UsingZio extends zio.App {

    import zio._

//    trait UserRepository {
//      def userRepository: UserRepository.Service
//    }

    object UserRepository {

      trait Service {
        def findUser(id: UUID): Task[Option[User]]

        def updateUser(u: User): Task[Unit]
      }

      trait InMemory extends UserRepository.Service {
        private lazy val users = collection.mutable.Map[UUID, User]()

        override def findUser(id: UUID): UIO[Option[User]] =
          UIO.effectTotal(users.get(id))

        override def updateUser(u: User): UIO[Unit] =
          UIO.effectTotal(users.put(u.id, u))
      }

    }

    object ur {
      def findUser(id: UUID): ZIO[Has[UserRepository.Service], Throwable, Option[User]] = ZIO.accessM(_.get.findUser(id))

      def updateUser(user: User): ZIO[Has[UserRepository.Service], Throwable, Unit] = ZIO.accessM(_.get.updateUser(user))
    }

//    trait Email {
//      def email: Email.Service
//    }

    object Email {

      trait Service {
        def sendEmail(email: String, subject: String, body: String): Task[Unit]
      }

    }

    object emailService {
      def sendEmail(email: String, subject: String, body: String): ZIO[Has[Email.Service], Throwable, Unit] =
        ZIO.access(_.get.sendEmail(email, subject, body))
    }


    object LoyaltyPoints {
      def addPoints(userId: UUID, pointsToAdd: Int): ZIO[Has[UserRepository.Service] with Has[Email.Service], Throwable, Either[String, Unit]] = {
        ur.findUser(userId).flatMap {
          case None => Task(Left("User not found"))
          case Some(user) =>
            val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
            for {
              _ <- ur.updateUser(updated)
              _ <- emailService.sendEmail(user.email, "Points added!", s"You now have ${updated.loyaltyPoints}")
            } yield Right(())
        }
      }
    }

    override def run(args: List[String]): URIO[Any, ExitCode] = {
      val newUser = User(UUID.randomUUID(), "hello@earth.world", 0)
      val appLogic: ZIO[Has[UserRepository.Service] with Has[Email.Service], Throwable, Unit] = for {
        update <- ur.updateUser(newUser)
        _ <- LoyaltyPoints.addPoints(newUser.id, 3)
      } yield ()
      val userRepositoryLayer: Layer[Nothing, Has[UserRepository.Service]] = ZLayer.succeed(new UserRepository.InMemory {})
      val emailLayer: Layer[Nothing, Has[Email.Service]] = ZLayer.succeed(new Email.Service {
          override def sendEmail(email: String, subject: String, body: String): Task[Unit] = Task(())
        })
      val combinedLayer: Layer[Nothing, Has[Email.Service] with Has[UserRepository.Service]] =
//        emailLayer.++[Nothing, Any, Has[Email.Service], Has[UserRepository.Service]](userRepositoryLayer)
        emailLayer.zipWithPar(userRepositoryLayer)(_ union _)
      val program = appLogic.provideLayer(combinedLayer)//provideSomeLayer(emailLayer).provideLayer(userRepositoryLayer)
      program.fold(_ => ExitCode.failure, _ => ExitCode.success)
    }
  }

}
