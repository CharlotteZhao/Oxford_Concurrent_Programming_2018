import java.awt._

import io.threadcso._
import io.threadcso.component.{console, exchanger, map, merge, prefix, tee, zip, zipwith}


object ps1
{
  val N = 5 // Number of philosophers

  val random = new scala.util.Random
  private val mutex   = BooleanSemaphore(available=true)
  // Simulate basic actions
  def Eat   = sleep(500*milliSec)
  def Think = sleep(random.nextInt(800)*milliSec)
  def Pause = sleep(500*milliSec)
  val report = N2NBuf[String](size=20, writers=0, readers=1, "report") // writers = 0 ??
  val NumEat = N2NBuf[String](size=20, writers=0, readers=1, "NumEat")


  io.threadcso.component.console(report)
  abstract class Action {}
  case class Pick(who: Int)  extends Action
  case class Drop(who: Int)  extends Action
  case class Sit(who: Int)  extends Action
  case class Getup(who: Int)  extends Action

  /*variant 4*/
  def Count(in: ?[Int]) = proc("Count"){
    var cnt = Array.ofDim[Int](N);
    var local :Int = 0;
    for (i <- 0 until N)
      cnt(i) = 0
    repeat{
      val index = in?;
      local = local + 1;
      cnt(index) = cnt(index) + 1;
      if (local % 5 == 0){
        for (j <- 0 until N)
          println(s"Phil $j eats "+ cnt(j) + "  times");
      }
    }

  }
/*variant 3*/
  def Enquiry(in: ?[Int],in2: ?[Int],out: ![Boolean]) = proc("Enquiry"){
    var committed = Array.ofDim[Boolean](N);
    for (i <- 0 until N)
      committed(i) = false
    var index : Int  = 0
    var index2: Int = 0
    serve{(
      in =?=> {_ => index = in?; // receive message and flip the sign of 'committed'
        if (committed(index))
          committed(index) = false;
        else committed(index) = true;
      }
        | in2 =?=> {_=>  index2 = in2?;}
        | out =!=> {committed(index2)}
        )}
  }
  def Phil(me: Int, left: ![Action], right: ![Action],sit: ![Int],leave: ![Int],EnquiryIn: ?[Boolean],EnquiryOut: ![Int],count_in: ![Int]) = proc("Phil"+me)
  {
    var committed : Boolean = true
    var cnt : Int = 0
    repeat {
      /* variant 2: enters */
      sit!(1)
      report!(s"$me sits")
      Think
      /* variant 1: pick up right fork first */
      committed = true;
      while(committed) {
        right!Pick(me); report!(me+" picks up right fork"); Pause
        EnquiryOut!(me) // send enquiry to left fork $me
        committed = EnquiryIn?;// receive enquiry
        if (committed){//second fork is not available: drop the first one
          right ! Drop(me);
          report ! (me + " drops right fork");
          Pause;
        }
        else{
          left!Pick(me);  report!(me+" picks up left fork");  Pause
        }

      }
      committed = true


      report ! (me+" eats"); Eat
      cnt = cnt + 1; // counter for the number of eating
      count_in!(me);
      //variant 1: drop right fork first?
      right!Drop(me); report!(me+" drops right fork"); Pause
      left!Drop(me);  report!(me+" drops left fork"); Pause
      /* variant 2: gets up  */
      leave!(1)

      report!(s"$me gets up"); Pause
    }
    println(s"Phil $me DIED")
  }

  def Butler(sit: ?[Int],leave: ?[Int]) = proc("Butler"){
    var cnt :Int = 0;
    serve{(
      (cnt < 4 && sit) =?=> (_=> cnt += 1)
        | (leave) =?=> (_=> cnt -= 1)
      )}
  }



  def Fork(me: Int, left: ?[Action], right: ?[Action],out: ![Int]) = proc("Fork"+me) {
    var owner: String="?"
    var committed : Boolean = false

    withDebuggerFormat (s"Fork ${me} with phil $owner") // register with debugger
    {
      serve
      {( left  =?=>
        { case Pick(x) =>
          owner=s"$x"
          committed = true
          out!(me)  // fork $me changes state
          left?()  match { case Drop(y)=>assert(y==x); owner="?";committed = false;out!(me)} }
        |
        right =?=>
          { case Pick(x) =>
            owner=s"$x"
            committed = true
            out!(me) // fork $me changes state
            right?() match { case Drop(y)=>assert(y==x); owner="?";committed = false;
              out!(me);} }

        )}
    }
  }



  val philSits = N2N[Int](writers=5, readers=1, "philSits")
  val philLeaves = N2N[Int](writers=5, readers=1, "philSits")
  val philAsk = N2N[Int](writers=5, readers=1, "philAsk")
  val forkAns = N2N[Boolean](writers=1, readers=5, "forkAns")
  val forkUpdate = N2NBuf[Int](size = 20,writers=5, readers=1, "forkUpdate")
  val counter_in = N2N[Int](writers=5, readers=1, "counter")
  val philLeftReceive =
    for (i<-0 until N) yield
      OneOne[Boolean](s"Phil($i) to Fork($i)")
  val philRightReceive =
    for (i<-0 until N) yield
      OneOne[Boolean](s"Phil($i) to Fork($i)")
  val philToLeftFork  =
    for (i<-0 until N) yield
      OneOne[Action](s"Phil($i) to Fork($i)")

  val philToRightFork =
    for (i<-0 until N) yield
      OneOne[Action] (s"Phil($i) to Fork(${(N+i-1)%N})")

  val philEnquire =
    for (i<-0 until N) yield
      OneOne[Boolean] (s"Phil($i) enquires")
  val ForkAnswer =
    for (i<-0 until N) yield
      OneOne[Boolean] (s"Fork($i) answers")

  val AllPhils: PROC =
    || (for (i <- 0 until N) yield
      Phil( i, philToLeftFork(i), philToRightFork(i),philSits,philLeaves,forkAns,philAsk,counter_in))
  val AllForks: PROC =
    || (for (i <- 0 until N) yield
      Fork( i, philToRightFork((i+1)%N), philToLeftFork(i),forkUpdate))

  val AllButler: PROC =
    Butler(philSits,philLeaves)
  val AllEnquiry: PROC =
    Enquiry(forkUpdate,philAsk,forkAns)
  val AllCount: PROC =
    Count(counter_in)
  val System =
    AllPhils || AllForks || AllButler || AllEnquiry ||component.console(report) || AllCount
  def main(args : Array[String]) = { println(debugger); System() }

}