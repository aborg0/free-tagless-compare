package com.softwaremill

import java.util.UUID

import cats.free.Free
import cats.implicits._
import cats.{Monad, ~>}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Compile {

  case class User(id: UUID, email: String, loyaltyPoints: Int) {
    def serialize: String = id.toString + "," + loyaltyPoints + "," + email
  }

  object User {
    def parse(s: String): User = {
      val parts = s.split(",")
      User(UUID.fromString(parts(0)), parts(2), parts(1).toInt)
    }
  }


  object Initial {

    trait KVStore {
      def get(k: String): Future[Option[String]]

      def put(k: String, v: String): Future[Unit]
    }

    trait UserRepository {
      def findUser(id: UUID): Future[Option[User]]

      def updateUser(u: User): Future[Unit]
    }

    class UserRepositoryUsingKVStore(kvStore: KVStore) extends UserRepository {
      override def findUser(id: UUID): Future[Option[User]] =
        kvStore.get(id.toString).map(serialized => serialized.map(User.parse))

      override def updateUser(u: User): Future[Unit] = {
        val serialized = u.serialize
        for {
          _ <- kvStore.put(u.id.toString, serialized)
          _ <- kvStore.put(u.email, serialized) // we also maintain a by-email index
        } yield ()
      }
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

    //

    sealed trait KVAlg[T]

    case class Get(k: String) extends KVAlg[Option[String]]

    case class Put(k: String, v: String) extends KVAlg[Unit]

    type KV[T] = Free[KVAlg, T]

    def get(k: String): KV[Option[String]] = Free.liftF(Get(k))

    def put(k: String, v: String): KV[Unit] = Free.liftF(Put(k, v))

    //

    val userToKvInterpreter = new (UserRepositoryAlg ~> KV) {
      override def apply[A](fa: UserRepositoryAlg[A]): KV[A] = fa match {
        case FindUser(id) =>
          get(id.toString).map(_.map(User.parse))
        case UpdateUser(u) =>
          val serialized = u.serialize
          for {
            _ <- put(u.id.toString, serialized)
            _ <- put(u.email, serialized) // we also maintain a by-email index
          } yield ()
      }
    }

    val kvToFutureInterpreter = new (KVAlg ~> Future) {
      override def apply[A](fa: KVAlg[A]): Future[A] = fa match {
        case Get(k) => /* go and talk to a database */ Future.successful(None)
        case Put(k, v) => /* as above */ Future.successful(())
      }
    }

    val result: Future[Either[String, Unit]] =
      addPoints(UUID.randomUUID(), 10)
        .foldMap(userToKvInterpreter)
        .foldMap(kvToFutureInterpreter)
  }

  object UsingTagless {

    trait UserRepositoryAlg[F[_]] {
      def findUser(id: UUID): F[Option[User]]

      def updateUser(u: User): F[Unit]
    }

    class LoyaltyPoints[F[_] : Monad](ur: UserRepositoryAlg[F]) {
      def addPoints(userId: UUID, pointsToAdd: Int): F[Either[String, Unit]] = {
        ur.findUser(userId).flatMap {
          case None => implicitly[Monad[F]].pure(Left("User not found"))
          case Some(user) =>
            val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
            ur.updateUser(updated).map(_ => Right(()))
        }
      }
    }

    //

    trait KVAlg[F[_]] {
      def get(k: String): F[Option[String]]

      def put(k: String, v: String): F[Unit]
    }

    trait KvToFutureInterpreter extends KVAlg[Future] {
      override def get(k: String): Future[Option[String]] =
        Future.successful(None) /* go and talk to a database */

      override def put(k: String, v: String): Future[Unit] =
        Future.successful(()) /* as above */
    }

    //

    class UserThroughKvInterpreter[F[_] : Monad](kv: KVAlg[F]) extends UserRepositoryAlg[F] {
      override def findUser(id: UUID): F[Option[User]] =
        kv.get(id.toString).map(_.map(User.parse))

      override def updateUser(u: User): F[Unit] = {
        val serialized = u.serialize
        for {
          _ <- kv.put(u.id.toString, serialized)
          _ <- kv.put(u.email, serialized) // we also maintain a by-email index
        } yield ()
      }
    }

    val result: Future[Either[String, Unit]] =
      new LoyaltyPoints(new UserThroughKvInterpreter(new KvToFutureInterpreter {}))
        .addPoints(UUID.randomUUID(), 10)

  }

  object UsingZio extends zio.App {

    import zio._

    type KV = Has[KV.Service]

    object KV {

      trait Service {
        def get(k: String): Task[Option[String]]

        def put(k: String, v: String): Task[Unit]
      }

      trait InMemory extends Service {
        private lazy val kv = collection.mutable.Map[String, String]()

        override def get(k: String): UIO[Option[String]] = UIO.effectTotal(kv.get(k))

        override def put(k: String, v: String): UIO[Unit] = UIO.effectTotal(kv.put(k, v))
      }

    }

    object keyValue {
      def get(k: String) /*: ZIO[KV, Throwable, Option[String]]*/ = ZIO.accessM[KV](_.get.get(k))

      def put(k: String, v: String) /*: ZIO[KV, Throwable, Unit]*/ = ZIO.accessM[KV](_.get.put(k, v))
    }

    type UserRepository = Has[UserRepository.Service]

    object UserRepository {

      trait Service {
        def findUser(id: UUID): Task[Option[User]]

        def updateUser(u: User): Task[Unit]
      }

      lazy val kvUserRepository: ZLayer[KV, Nothing, UserRepository] =
        ZLayer.fromService /*[KV.Service, UserRepository.Service]*/ (kv => new Service {
          override def findUser(id: UUID): Task[Option[User]] = kv.get(id.toString).map(_.map(User.parse))

          override def updateUser(u: User): Task[Unit] = {
            val serialized = u.serialize
            //            //Only if kv allows multi-threaded updates
            //            kv.put(u.id.toString, serialized).zipWithPar(kv.put(u.email, serialized))((_, _) => ())
            for {_ <- kv.put(u.id.toString, serialized)
                 _ <- kv.put(u.email, serialized)} yield ()
          }
        })
      //        ZLayer.fromFunction { kv =>
      //          new Service {
      //            override def findUser(id: UUID): Task[Option[User]] =
      //              kv.get.get(id.toString).map(_.map(User.parse))
      //
      //            override def updateUser(u: User): Task[Unit] = {
      //              val serialized = u.serialize
      ////Only if kv allows multi-threaded updates
      ////              kv.get.put(u.id.toString, serialized).zipWithPar(kv.get.put(u.email, serialized))((_, _) => ())
      //              for {
      //                _ <- kv.get.put(u.id.toString, serialized)
      //                _ <- kv.get.put(u.email, serialized)
      //              } yield ()
      //            }
      //          }
      //        }
    }

    object ur {
      def findUser(id: UUID): ZIO[UserRepository, Throwable, Option[User]] = ZIO.accessM(_.get.findUser(id))

      def updateUser(user: User): ZIO[UserRepository, Throwable, Unit] = ZIO.accessM(_.get.updateUser(user))
    }

    type Email = Has[Email.Service]

    object Email {

      trait Service {
        def sendEmail(email: String, subject: String, body: String): Task[Unit]
      }

    }

    object emailService {
      def sendEmail(email: String, subject: String, body: String): ZIO[Email, Throwable, Unit] =
        ZIO.accessM(_.get.sendEmail(email, subject, body))
    }


    object LoyaltyPoints {
      def addPoints(userId: UUID, pointsToAdd: Int): ZIO[UserRepository with Email, Throwable, Either[String, Unit]] = {
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

    override def run(args: List[String]): URIO[Any, Int] = {
      val newUser = User(UUID.randomUUID(), "hello@earth.world", 0)
      val appLogic: ZIO[UserRepository with Email, Throwable, Unit] = for {
        update <- ur.updateUser(newUser)
        _ <- LoyaltyPoints.addPoints(newUser.id, 3)
      } yield ()
      val kvLayer: Layer[Nothing, Has[KV.Service]] = ZLayer.succeed(new KV.InMemory {}: KV.Service)
      val userRepositoryLayer: Layer[Nothing, UserRepository] = kvLayer >>> UserRepository.kvUserRepository
      val emailLayer: Layer[Nothing, Email] = ZLayer.succeed(new Email.Service {
        override def sendEmail(email: String, subject: String, body: String): Task[Unit] = Task(())
      })
      //      val combinedLayer: Layer[Nothing, Email with UserRepository] = emailLayer ++ userRepositoryLayer
      //      val program = appLogic.provideLayer(combinedLayer)
      val program = appLogic.provideSomeLayer[UserRepository](emailLayer).provideLayer(userRepositoryLayer)
      program.fold(_ => 1, _ => 0)
    }
  }

}
