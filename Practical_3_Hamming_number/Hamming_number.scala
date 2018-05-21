import java.awt._
import io.threadcso._
import io.threadcso.component.{console, exchanger, map, merge, prefix, tee, zip, zipwith}
object ps1 {

  val h = 0
  def console(in: ?[Long]) = proc("console"){
      var cnt : Int = 0;
      repeat(cnt < 1000){
        cnt = cnt + 1;
        println(in?);
      }
    in.closeIn;
  }
  def mul2(in: ?[Long], out: ![Long]):PROC = proc {repeat {val i = in ?;out ! (i * 2)};out.closeOut; in.closeIn;}
  def mul3(in: ?[Long], out: ![Long]):PROC = proc{repeat{val i = in?; out!(i*3)}; out.closeOut; in.closeIn;}
  def mul5(in: ?[Long], out: ![Long]):PROC = proc{repeat{val i = in?; out!(i*5)}; out.closeOut; in.closeIn;}

 // def prefix1(i:Int, out: ![Long]): PROC = proc{out!(i);}
  /*Merge sort of two channels*/
  def Merge(in1: ?[Long], in2: ?[Long], out: ![Long]): PROC = proc  {
    val  mid= OneOne[Long]

      var lv : Long = in1?
      var rv : Long = in2?;
        //??exchanger(in1,in2,out1,mid) PROC = proc
    repeat {
      if (lv > rv){
        out!rv;
        rv = in2?;

      }

      else if (lv == rv){ // discard repeating numbers
        out!lv;
        lv = in1?;
        rv = in2?;
      }

      else{
        out!lv ;
        lv = in1?;
      }

    }
    out.closeOut; in1.closeIn; in2.closeIn
  }

  def Counter( out: ![Long]):PROC = proc  {
    var cnt:Long = 1;
    repeat(cnt <= 1000){
      out!(cnt);
      cnt = cnt + 1;
    }
    out.closeOut;

  }
  def main (args: Array[String]): Unit = {
    val result,in1,out1,out2,x2,x3,x5,out3,out7,cntout= OneOne[Long]
    val out4,out5,out6 = OneOneBuf[Long](1000)
      (
      prefix[Long](1)(in1,out1) || tee(out1,result,out2) || tee(out2,x2,out3) || mul2(x2,out4) || tee(out3,x3,x5)
      || mul3(x3,out5) || mul5(x5,out6) || Merge (out4,out5,out7) || Merge(out6,out7,in1) || console (result)

      )()
    exit
  }

}