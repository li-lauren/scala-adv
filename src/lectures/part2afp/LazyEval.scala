package lectures.part2afp

object LazyEval extends App{

  // val y: Int = throw new RuntimeException // will crash
  lazy val x: Int = throw new RuntimeException

  // lazy delays the evaluation of values
  // evaluated once, but only when used for the first time
  // val x is only evaluated on by need basis; will only be evaluated when used
  // println(x) this will crash
  // once a value is created, the value will stay assigned to that name

  lazy val z: Int = {
    println("hello")
    42
  }
  println(z) // hello and 42 (x is evaluated once)
  println(z) // just 42, which is the valued stored in z ; z has already been evaluated

  // examples of implications
  // side effects
  def sideEffectCondition: Boolean = {
    println("Boo")
    true
  }

  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectCondition
  println(if (simpleCondition && lazyCondition) "yes" else "no")

  // lazyCondition is not evaluated since runtime knows the predicate is already false from simpleCondition

  // in conjunction with call by name
  def byNameMethod(n: => Int): Int = n + n + n + 1 // will take 3 seconds, n is evaluated three times
  def retrieveMagicValue = {
    // side effect or a long computation
    println("Waiting")
    Thread.sleep(1000)
    42
  }

  println(byNameMethod(retrieveMagicValue))

  // faster byNameMethod using lazy aka CALL BY NEED
  def lazyByNameMethod(n: => Int): Int = {
    lazy val t = n // only evaluated once
    t + t + t + 1
  }
  // call n only when you need it and without repeated evals

  // filtering with lazy vals
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30) // List(1,25,5,23)
  val gt20 = lt30.filter(greaterThan20)
  println(gt20)

  val lt30lazy = numbers.withFilter(lessThan30)
  val gt20lazy = lt30lazy.withFilter(greaterThan20)
  println
  gt20lazy.foreach(println) // as needed basis

  // for-comprehensions use withFilter with guards
  for {
    a <- List(1,2,3) if a % 2 == 0 // use lazy vals!
  } yield a + 1
  List(1,2,3).withFilter(_ % 2 == 0).map(_ + 1) // List[Int]

  /*
    Exercise: Implement a lazily evaluated, singly linked STREAM of elements

    naturals = MyStream.from(1)(x => x + 1) = stream of natural numbers (potentially infinite)
    naturals.take(100).foreach(println) // lazily evaluated stream of the first 100 naturals (finite stream)
    naturals.foreach(println) // will crash - infinite!
    naturals.map(_ * 2) // stream of all even numbers (potentially infinite)
   */

  abstract class MyStream[+A] {
    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]

    def #::[B >: A](element: B): MyStream[B] // prepend operator
    def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] // concatenate two streams

    def foreach(f: A => Unit): Unit
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: A => MyStream[B]): MyStream[B]
    def filter(predicate: A => Boolean): MyStream[A]

    def take(n: Int): MyStream[A] // takes the first n elements out of this stream
    def takeAsList(n: Int): List[A]
  }

  object MyStream {
    def from[A](start: A)(generator: A => A): MyStream[A] = ???
  }
}
