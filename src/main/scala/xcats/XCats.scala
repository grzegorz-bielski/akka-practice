package xcats

// not really an akka-topic but 🤷‍♂️
// a set of snippets and exercies from scala with cats book
object XCats extends App {
  import scala.language.higherKinds

  object MonadTransformers {
    import cats.data.EitherT
    import cats.instances.future._
    import scala.concurrent.Future
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Await

    //   type Response[A] = Future[Either[String, A]]
    type Response[A] = EitherT[Future, String, A]

    val powerLevels = Map(
      "Jazz" -> 6,
      "Bumblebee" -> 8,
      "Hot Rod" -> 10
    )

    def getPowerLevel(autobot: String): Response[Int] =
      EitherT.fromOption[Future](powerLevels.get(autobot), "kek")

    def canSpecialMove(a: String, b: String): Response[Boolean] =
      for {
        powA <- getPowerLevel(a)
        powB <- getPowerLevel(b)
      } yield (powA + powB) > 15

    def tacticalReport(a: String, b: String): String =
      Await.result(canSpecialMove(a, b).value, 1.second) match {
        case Right(true)  => "can do"
        case Right(false) => "nope"
        case Left(err)    => s"err $err"
      }

    println(tacticalReport("Jazz", "Bumblebee"))
  }

  object LeftToRightElimination {
    // partial unification
    // - fixing one of the two parameters of multi parameter type constructors (like Function1) so the kinds are correct
    // - done by left to right elimination

    import cats.Functor
    import cats.instances.function._
    import cats.syntax.functor._
    import cats.syntax.contravariant._

    val func1 = (x: Int) => x.toDouble
    val func2 = (y: Double) => y * 2
    val func3 = func1.map(func2)

    // forcing the right to left elimination
    type <=[B, A] = A => B
    type F[A] = Double <= A

    val func2b: Double <= Double = func2

    val func3c = func2b.contramap(func1)
  }

  object Prod {
    import cats.Monad
    import cats.syntax.flatMap._
    import cats.syntax.functor._

    def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
      x.flatMap(xx => y.map(yy => (xx, yy)))
  }

  object Form {
    import cats.data.Validated
    import scala.util.Try
    import cats.instances.either._
    import cats.instances.list._
    import cats.syntax.apply._
    import cats.syntax.either._
    import cats.syntax.applicative._

    type Data = Map[String, String]
    type FailFast[A] = Either[List[String], A]
    type FailSlow[A] = Validated[List[String], A]

    val x = "a".asRight[FailFast[String]]
    // val y = "a".pure[Either[Int, _]] ???

    case class User(name: String, age: Int)

    def getValue(name: String)(data: Data): FailFast[String] =
      data.get(name).toRight(List(s"$name was not specified"))

    def parseInt(name: String)(data: String): FailFast[Int] =
      Either
        .catchOnly[NumberFormatException](data.toInt)
        .leftMap(_ => List(s"$name must be an integer"))

    def nonBlank(name: String)(data: String): FailFast[String] =
      data.asRight.ensure(List(s"$name cannot be blank"))(_.nonEmpty)

    def nonNegative(name: String)(data: Int): FailFast[Int] =
      data.asRight.ensure(List(s"$name must be non-negative"))(_ >= 0)

    def readName(data: Data): FailFast[String] =
      getValue("name")(data)
        .flatMap(nonBlank("name"))

    def readAge(data: Data): FailFast[Int] =
      getValue("age")(data)
        .flatMap(nonBlank("age"))
        .flatMap(parseInt("age"))
        .flatMap(nonNegative("age"))

    def toUser(data: Data): FailSlow[User] =
      (readName(data).toValidated, readAge(data).toValidated) mapN User
  }

  object trav {
    import cats.data.Validated
    import cats.instances.list._
    import cats.syntax.traverse._

    type ErrorsOr[A] = Validated[List[String], A]
    def process(inputs: List[Int]): ErrorsOr[List[Int]] = inputs traverse { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }
  }

  object MapReduce {
    import cats.Monoid
    import cats.Foldable
    import cats.Traverse

    import cats.instances.int._ // for Monoid
    import cats.instances.future._ // for Applicative and Monad
    import cats.instances.vector._ // for Foldable and Traverse

    import cats.syntax.semigroup._ // for |+|
    import cats.syntax.foldable._ // for combineAll and foldMap
    import cats.syntax.traverse._ // for traverse

    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global

    def foldMap[A, B: Monoid](as: Vector[A])(fn: A => B): B =
      as.foldLeft(Monoid[B].empty)(_ |+| fn(_))

    def parallelFoldMap[A, B: Monoid](values: Vector[A])(fn: A => B): Future[B] = {
      val groupSize = (values.length / Runtime.getRuntime.availableProcessors).ceil.toInt

      val futures = values.grouped(groupSize).map { group =>
        Future(foldMap(group)(fn))
      }

      Future.sequence(futures) map { res =>
        res.foldLeft(Monoid[B].empty)(_ |+| _)
      }
    }

    def catsParallelFoldMap[A, B: Monoid](values: Vector[A])(fn: A => B): Future[B] = {
      val groupSize = (values.length / Runtime.getRuntime.availableProcessors).ceil.toInt

      values
        .grouped(groupSize)
        .toVector
        .traverse(group => Future(group.toVector.foldMap(fn)))
        .map(_.combineAll)
    }
  }

  object Validation {
    import cats.Semigroup
    import cats.data.Validated
    import cats.data.Validated._
    import cats.syntax.semigroup._
    import cats.syntax.apply._

    sealed trait Predicate[E, A] {
      import Predicate._

      def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

      def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
        this match {
          case Pure(fn)         => fn(a)
          case And(left, right) => (left(a), right(a)) mapN ((_, _) => a)
          case Or(left, right) =>
            left(a) match {
              case Valid(a) => Valid(a)
              case Invalid(e1) =>
                right(a) match {
                  case Valid(a)    => Valid(a)
                  case Invalid(e2) => Invalid(e1 |+| e2)
                }
            }
        }
    }
    object Predicate {
      final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
      final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
      final case class Pure[E, A](fn: A => Validated[E, A]) extends Predicate[E, A]
    }

    sealed trait Check[E, A, B] {
      import Check._

      def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

      def map[C](fn: B => C): Check[E, A, C] = Map[E, A, B, C](this, fn)

      def flatMap[C](f: B => Check[E, A, C]) = FlatMap[E, A, B, C](this, f)

      def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen[E, A, B, C](this, that)
    }

    object Check {
      def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = Pure(pred)

      final case class Map[E, A, B, C](check: Check[E, A, B], fn: B => C) extends Check[E, A, C] {
        def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] = check(in).map(fn)
      }
      final case class Pure[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
        def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] = pred(in)
      }
      final case class FlatMap[E, A, B, C](check: Check[E, A, B], fn: B => Check[E, A, C]) extends Check[E, A, C] {
        def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
          check(a).withEither(_.flatMap(b => fn(b)(a).toEither))
      }
      final case class AndThen[E, A, B, C](check: Check[E, A, B], that: Check[E, B, C]) extends Check[E, A, C] {
        def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
          check(in).withEither(_.flatMap(that(_).toEither))
      }
    }
  }
}
