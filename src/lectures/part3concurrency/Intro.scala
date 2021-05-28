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

  aThread.start() // only gives the signal to the JVM to start a JVM thread
  // JVM makes that JVM thread, invokes the run method inside its inner runnable
  // create a JVM thread => OS thread
  runnable.run() // doesn't do anything in parallel! call start on the thread instead

  aThread.join() // blocks until aThread finishes running

  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))

  threadHello.start()
  threadGoodbye.start()

  // different runs produce different results!
  // thread scheduling depends on a number of factors like OS and JVM implementations

  // executors
  // jvm threads are expensive to run and kill, therefore good practice to reuse them via executors and thread pools
  val pool = Executors.newFixedThreadPool(10)
  pool.execute(() => println("something in thread pool")) // this runnable will be run by 1/10 thread in the pool

  pool.execute(() => {
    Thread.sleep(1000)
    println("done after 1 second")
  })

  pool.execute(() => {
    Thread.sleep(1000)
    println("almost done")
    Thread.sleep(1000)
    println("done after 2 seconds")
  })

  pool.shutdown() // no more actions can be submitted
  // pool.execute(() => println("should not appear")) this would throw an exception in the calling thread

  // pool.shutdownNow() // anything still running throws an exception
  println(pool.isShutdown) // true, will return true even if actions are still running
}
