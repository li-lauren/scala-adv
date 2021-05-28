package lectures.part2afp

object Monads extends App {

  // our own try monad

  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  object Attempt {
    def apply[A](a: => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Success[+A](value: A) extends Attempt[A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing] {
    def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  /*
    left-identity
    unit.flatMap(f) = f(x)
    Attempt(x).flatMap(f) = f(x)
    Success(x).flatMap(f) = f(x)

    right-identity
    Attempt.flatMap(unit) = Attempt
    Success(x).flatMap(x => Attempt(x)) = Attempt(x) = Success(x)
    Fail(e).flatMap(...) = Fail(e)

    associativity
    Attempt.flatMap(f).flatMap(g) = Attempt.flatMap(x => f(x).flatMap(g))
    Fail(e).flatMap(f).flatMap(g) = Fail(e)
    Fail(e).flatMap(x => f(x).flatMap(g)) = Fail(e)

    Success(v).flatMap(f).flatMap(g) =
      f(v).flatMap(g) or Fail(e)

    Success(v).flatMap(x => f(x).flatMap(g)) =
      f(v).flatMap(g) or Fail(e)
  */

  val attempt = Attempt {
    throw new RuntimeException("My own monad, yes!")
  }

  println(attempt)

  /*
    EXERCISE:
    1) Implement a Lazy[T] monad = computation which will only be executed when it is needed
         unit/apply in a companion object
         flatMap
    2) Monads = unit + flatMap
       Monads = unit + map + flatten

       Monad[T] {
       def flatMap[B](f: T => Monad[B]): Monad[B] = ... (implemented)

       def map[B](f: T => B): Monad[B] = flatMap(x => unit(f(x)))
       def flatten(m: Monad[Monad[T]]): Monad[T] = m.flatMap((x: Monad[T]) => x)

       (have List in mind)
       List(1,2,3).map(_ * 2) = List(1,2,3).flatMap(x => List(x * 2))
       List(List(1,2),List(3,4)).flatten = List(List(1,2),List(3,4)).flatMap(x => x) = List(1,2,3,4)
   */

  // 1 - Lazy monad
  class Lazy[+A](value: => A) {
    // call by need
    private lazy val internalValue = value

    def use: A = internalValue

    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internalValue)
  }

  object Lazy {
    def apply[A](value: => A): Lazy[A] = new Lazy(value)
  }

  val lazyInstance = Lazy {
    println("Blah")
    42
  }

  println(lazyInstance.use)
  val flatMappedInstance = lazyInstance.flatMap(x => Lazy {
    10 * x
  })

  val flatMappedInstance2 = lazyInstance.flatMap(x => Lazy {
    10 * x
  })

  flatMappedInstance.use
  flatMappedInstance2.use

}
