package hashcode

import java.io.File
import java.util.Scanner

case class RequestWithCost (video: Int, endPoint: Int, latency: Int, requests: Int) extends Ordered[RequestWithCost] {
  def cost =  latency * requests
  def compare (that: RequestWithCost) = cost - that.cost
}

object RequestWithCost {
  def fromData (data: Main.Data): IndexedSeq[RequestWithCost] =
    (for (i <- data.requestDescriptors) yield { println(i)
      RequestWithCost(i.video, i.endPoint, data.endpoints(i.endPoint).datacenterLantency, i.requests) }).sorted.reverse

  def main (args: Array[String]): Unit = {
    val l: List[RequestWithCost] = List(RequestWithCost(0, 0, 1, 2), RequestWithCost(0, 0, 3, 4), RequestWithCost(0, 0, 0, 0))
    println(l.sorted)
    val filename = "me_at_the_zoo.in"
    //  val filename = "kittens.in"
    val data = Main.parse(new Scanner(new File(filename)))
    println(RequestWithCost.fromData(data))
  }
}
