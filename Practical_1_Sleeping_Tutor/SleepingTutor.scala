import io.threadcso._
class UnboundedBuf[T] extends Slot[T]{
  private [this] val monitor = new monitor
  private [this] val queue = new scala.collection.mutable.Queue[T]
  private [this] var nonEmpty = monitor.newConditin


  def put(v:T) = monitor withLock{
    queue.enqueue(v)
    nonEmpty.signal()
  }

  def get:T = monitor withLock{
    while(queue.isEmpty)
      nonEmpty.await()
    return queue.dequeue
  }
}
object UnboundedBuf extends TestSlot(new UnboundedBuf[Int])

class BboundedBuf[T] (bufSize:Int) extends Slot[T]{
  private [this] val monitor = new monitor
  private [this] val queue = new scala.collection.mutable.Queue[T]
  private [this] var nonEmpty,nonFull = monitor.newConditin


  def put(v:T) = monitor withLock{
    while (queue.size == bufSize)
      nonFull.await()
    queue.enqueue(v)
    nonEmpty.signal()
  }

  def get:T = monitor withLock{
    while(queue.isEmpty)
      nonEmpty.await()
    val v = queue.dequeue
    nonFull.signal()
    return v
  }
}
object BoundedBuf extends TestSlot(new UnboundedBuf[Int](100))