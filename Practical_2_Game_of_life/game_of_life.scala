import java.awt._
import io.threadcso._


object ps1{

  // define board
  val n = 50 : Int
  private [this] var board = Array.ofDim[Boolean](n,n)
  private [this] val display = new Display(n, board)
  //seed


  for (i <- n/2 until n/2 + 5)
    for (j <- n/2 until n/2 + 5)
      if (j == n / 2 || j == n/2+ 4 || (i == n/2 && j == n/2 + 2) || (i == n/2 + 4 &&  j == n/2 + 2) )
        board(i)(j) = true

/*
  board(5)(1) = true
  board(5)(2) = true
  board(5)(3) = true

  board(5)(4) = true
  board(5)(5) = true
  board(5)(6) = true
  board(5)(7) = true
  board(5)(8) = true
  board(5)(9) = true
  board(5)(10) = true
  */

  val NumofProcs = 4

  // define rules
  def wrap(n: Int)(a: Int): Int = {
    if (a >= n) a - n
    else if (a < 0) a + n
    else a
  }


  //display
  def Draw() = proc{

    display.draw
    sleepms(300)
    while(true) {
      //println("draw")
      computed.sync()
      display.draw

      sleepms(300)
      updated.sync()
    }
  }

  //return the number of alive neighbors
  def NeighborState(x:Int,y:Int): Int = {

    var cnt = 0
    for (i <- 0 until 3)
      for (j <- 0 until 3){


        if (!(i == 1 && j == 1)) {
          if (board(wrap(n)(x + i - 1))(wrap(n)(y + j - 1)) == true) {
            cnt = cnt + 1 // count the number of alive neighbors
          }
        }
  }

    cnt
  }
  //update board
  def UpdateState(i:Int, j:Int):Boolean = {
    var  newVal: Boolean = true
    val AliveNeighbor = NeighborState(i,j)
    newVal = board(i)(j)
    if (newVal== true){
      //case 1 & 2
      if (AliveNeighbor < 2 || AliveNeighbor > 3) {
        newVal = false
      }

    }
    else{
      //case 4
      if (AliveNeighbor == 3){
        newVal = true

      }
    }
    newVal
  }


  // Concurrency
  val computed = new Barrier(NumofProcs+1)
  val updated = new Barrier(NumofProcs+1)


  def worker(startrow:Int, ROWS:Int) = proc{
    val newBlock = Array.ofDim[Boolean](ROWS,n)
    while(true){

      // compute new states
      for (row<-startrow until startrow + ROWS; col <- 0 until n){

        var boardValue =board(row)(col)
        var stateVal = UpdateState(row,col)
        newBlock(row - startrow)(col) = stateVal

      }
      computed.sync()

      // update states on the board
      for (row<-startrow until startrow + ROWS; col <- 0 until n ) {
        var boardValue = board(row)(col)

        var newValue = newBlock(row - startrow)(col)
        board(row)(col) = newValue
      }
      updated.sync()

    }

  }


  def main (args: Array[String]): Unit = {
    val slot = n / NumofProcs //e.g. 16 / 4 = 4 ,startrow = 0,4,8,12
    var system: PROC = worker(slot * (NumofProcs - 1), slot)
    for (i <- 0 until NumofProcs -1){
      system = system ||  worker(slot*(i), slot)
    }
    ( Draw || system ) ()

  }
}

