package lectures.part3concurrency

import java.util.concurrent.Executors

object Intro extends App {

  // JVM threads
  // thread is an instance of a class native to the JVM
  // threads take in a Runnable object

  /*
    interface Runnable {
      public void run()
    }
   */
  val runnable = new Runnable {
    override def run(): Unit = println("Running in parallel")
  }
  val aThread = new Thread(runnable)

//  aThread.start() // only gives the signal to the JVM to start a JVM thread
  // JVM makes that JVM thread, invokes the run method inside its inner runnable
  // create a JVM thread => OS thread
  runnable.run() // doesn't do anything in parallel! call start on the thread instead

  aThread.join() // blocks until aThread finishes running

  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))

//  threadHello.start()
//  threadGoodbye.start()

  // different runs produce different results!
  // thread scheduling depends on a number of factors like OS and JVM implementations

  // executors
  // jvm threads are expensive to run and kill, therefore good practice to reuse them via executors and thread pools
  val pool = Executors.newFixedThreadPool(10)
//  pool.execute(() => println("something in thread pool")) // this runnable will be run by 1/10 thread in the pool
//
//  pool.execute(() => {
//    Thread.sleep(1000)
//    println("done after 1 second")
//  })
//
//  pool.execute(() => {
//    Thread.sleep(1000)
//    println("almost done")
//    Thread.sleep(1000)
//    println("done after 2 seconds")
//  })

//  pool.shutdown() // no more actions can be submitted
  // pool.execute(() => println("should not appear")) this would throw an exception in the calling thread

  // pool.shutdownNow() // anything still running throws an exception
//  println(pool.isShutdown) // true, will return true even if actions are still running

  // Concurrency Problems/Pain points in the JVM

  def runInParallel = {
    var x = 0

    val thread1 = new Thread(() => {
      x = 1
    })

    val thread2 = new Thread(() => {
      x = 2
    })

    thread1.start()
    thread2.start()
    println(x)
  }

//  for (_ <- 1 to 100) runInParallel

  // race condition, two threads are attempting to set the same memory space at the same time
  // introduce bugs to code

  class BankAccount(@volatile var amount: Int) {
    override def toString: String = "" + amount
  }

  def buy(account: BankAccount, thing: String, price: Int) = {
    account.amount -= price
//    println("I've bought" + thing)
//    println("my account is now " + account)
  }

//  for (_ <- 1 to 10000) {
//    val account = new BankAccount(50000)
//    val thread1 = new Thread(() => buy(account, "shoes", 3000))
//    val thread2 = new Thread(() => buy(account, "iPhone", price = 4000))
//
//    thread1.start()
//    thread2.start()
//    Thread.sleep(10)
//    if (account.amount != 43000) println("Aha: " + account.amount)
//    //println()
//  }

  // Option #1: use synchronized(), a method on reference types
  def buySafe(account: BankAccount, thing: String, price: Int) =
    account.synchronized({
      // no two threads can evaluate this at the same time
      account.amount -= price
      println("I've bought " + thing)
      println("my account is now " + account)
    })

  // Option #2: use @volatile
  // all reads and writes to it are synchronized
  // can't customize with more lines of code tho

  /**
   * Exercises
   *
   * 1) Construct 50 "inception" threads
   *     Thread1 -> thread2 -> thread3 -> ...
   *     println("hello from thread ?")
   *
   *     in REVERSE ORDER
   *     practice start and join methods
   */

  def generateInceptionThreads(n: Int): Unit = {
    if (n == 1) println("hello from thread 1")
    else {
      val currThread = new Thread(() => {
        println(s"hello from thread $n")
      })
      currThread.start()
      currThread.join()
      generateInceptionThreads(n - 1)
    }
  }

  generateInceptionThreads(50)

  def inceptionThreads(maxThreads: Int, i: Int = 1): Thread =
    new Thread(() => {
      if (i < maxThreads) {
        val newThread = inceptionThreads(maxThreads, i + 1)
        newThread.start()
        newThread.join()
      }
      println(s"Hello from thread $i")
    })

//  inceptionThreads(50).start()
  /*
    Exercise 2
   */
  var x = 0
  val threads = (1 to 100).map(_ => new Thread(() => x += 1))
  threads.foreach(_.start())
  /*
    1) What is the biggest value possible for x? 100
    2) What is the smallest value possible for x? 1

    thread1: x = 0
    thread2: x = 0
      ...
    thread100: x = 0

    for all threads: x = 1 and write it back to x
   */

  threads.foreach(_.join())
  println(x)
  /*
    Exercise 3: sleep fallacy
   */

  var message = ""
  val awesomeThread = new Thread(() => {
    Thread.sleep(1000)
    message = "Scala is awesome"
  })

  message = "Scala sucks"
  awesomeThread.start()
  Thread.sleep(2000) // don't do this ever
  awesomeThread.join() // wait for awesome thread to run; this solves the problem
  println(message)
  /*
    what's the value for message?
    is it guaranteed? why/why not?
    almost always "Scala is awesome" but it is not guaranteed

    (main thread)
      message = "scala sucks"
      awesomeThread.start()(
      sleep() - relieves execution)
    (awesome thread)
      sleep() - relieves execution
    (OS gives the CPU to some important thread - takes CPU for more than 2 seconds)
    (OS gives the CPU back to  MAIN thread)
      println("Scala sucks")
    (OS gives the CPU to awesomethread)
      message = "scala is awesome"
   */

  // how do we fix this?
  // synchronizing does not work here
  // synchronizing is only working for concurrent modifications
  // here we have a sequential problem
  // only solution is to have threads join
}
