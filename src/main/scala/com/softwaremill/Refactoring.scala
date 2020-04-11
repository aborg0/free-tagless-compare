package com.softwaremill

import java.util.UUID

import cats.free.Free
import cats.{Monad, ~>}
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Refactoring {
  case class User(id: UUID, email: String, loyaltyPoints: Int)

  object Initial {
    trait UserRepository {
      def findUser(id: UUID): Future[Option[User]]
      def updateUser(u: User): Future[Unit]
    }

    class LoyaltyPoints(ur: UserRepository) {
      def addPoints(userId: UUID, pointsToAdd: Int): Future[Either[String, Unit]] = {
        ur.findUser(userId).flatMap {
          case None => Future.successful(Left("User not found"))
          case Some(user) =>
            val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
            ur.updateUser(updated).map(_ => Right(()))
        }
      }
    }
  }

  object UsingFree {
    sealed trait UserRepositoryAlg[T]
    case class FindUser(id: UUID) extends UserRepositoryAlg[Option[User]]
    case class UpdateUser(u: User) extends UserRepositoryAlg[Unit]

    type UserRepository[T] = Free[UserRepositoryAlg, T]

    def findUser(id: UUID): UserRepository[Option[User]] = Free.liftF(FindUser(id))
    def updateUser(u: User): UserRepository[Unit] = Free.liftF(UpdateUser(u))

    def addPoints(userId: UUID, pointsToAdd: Int): UserRepository[Either[String, Unit]] = {
      findUser(userId).flatMap {
        case None => Free.pure(Left("User not found"))
        case Some(user) =>
          val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
          updateUser(updated).map(_ => Right(()))
      }
    }

    val futureInterpreter = new (UserRepositoryAlg ~> Future) {
      override def apply[A](fa: UserRepositoryAlg[A]): Future[A] = fa match {
        case FindUser(id) => /* go and talk to a database */ Future.successful(None)
        case UpdateUser(u) => /* as above */ Future.successful(())
      }
    }

    val result: Future[Either[String, Unit]] =
      addPoints(UUID.randomUUID(), 10).foldMap(futureInterpreter)
  }

  object UsingTagless {
    trait UserRepositoryAlg[F[_]] {
      def findUser(id: UUID): F[Option[User]]
      def updateUser(u: User): F[Unit]
    }

    class LoyaltyPoints[F[_]: Monad](ur: UserRepositoryAlg[F]) {
      def addPoints(userId: UUID, pointsToAdd: Int): F[Either[String, Unit]] = {
        ur.findUser(userId).flatMap {
          case None => implicitly[Monad[F]].pure(Left("User not found"))
          case Some(user) =>
            val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
            ur.updateUser(updated).map(_ => Right(()))
        }
      }
    }

    trait FutureInterpreter extends UserRepositoryAlg[Future] {
      override def findUser(id: UUID): Future[Option[User]] =
        Future.successful(None) /* go and talk to a database */

      override def updateUser(u: User): Future[Unit] =
        Future.successful(()) /* as above */
    }

    val result: Future[Either[String, Unit]] =
      new LoyaltyPoints(new FutureInterpreter {}).addPoints(UUID.randomUUID(), 10)

  }

  object UsingZio extends zio.App {
    import zio._
    trait UserRepository {
      def userRepository: UserRepository.Service
    }

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
      def findUser(id: UUID): ZIO[UserRepository, Throwable, Option[User]] = ZIO.accessM(_.userRepository.findUser(id))
      def updateUser(user: User): ZIO[UserRepository, Throwable, Unit] = ZIO.accessM(_.userRepository.updateUser(user))
    }


    object LoyaltyPoints {
      def addPoints(userId: UUID, pointsToAdd: Int): ZIO[UserRepository, Throwable, Either[String, Unit]] = {
        ur.findUser(userId).flatMap {
          case None => Task(Left("User not found"))
          case Some(user) =>
            val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
            ur.updateUser(updated).map(_ => Right(()))
        }
      }
    }

    override def run(args: List[String]): URIO[Any, Int] = {
      val newUser = User(UUID.randomUUID(), "hello@earth.world", 0)
      val appLogic = for {
        update <- ur.updateUser(newUser)
        _ <- LoyaltyPoints.addPoints(newUser.id, 3)
      } yield ()
      val program = appLogic.provide(new UserRepository {
        override def userRepository: UserRepository.Service = new UserRepository.InMemory {}
      })
      program.fold(_ => 1, _ => 0)
    }
  }
}
