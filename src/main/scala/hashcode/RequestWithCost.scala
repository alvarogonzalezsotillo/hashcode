package hashcode

import java.io.File
import java.util.Scanner

import hashcode.Main.Data

case class RequestWithCost (video: Int, endPoint: Int, latency: Int, requests: Int) extends Ordered[RequestWithCost] {
  def cost =  latency * requests
  def compare (that: RequestWithCost) = cost - that.cost
}

case class CacheStatus (max: Int, maxVideos: Int) {
  var size: Int = 0
  val videos: Array[Boolean] = new Array[Boolean](maxVideos)
  def remaining = max - size
  def hasVideo (video: Int) = videos(video)
  def addVideo (video: Int, size: Int) = {
    assert(!hasVideo(video))
    assert(this.size + size <= max)
    this.size += size
    videos(video) = true
  }
  override def toString: String = {
    val r = (0 until videos.size).filter(videos(_)).mkString(" ")
    s"$size $r"
  }
}

object CacheStatus {
  def fromData (data: Main.Data): IndexedSeq[CacheStatus] =
    for (i <- 0 until data.caches) yield CacheStatus(data.cacheSize, data.videos.length)
}

object RequestWithCost {
  def fromData (data: Main.Data): IndexedSeq[RequestWithCost] =
    (for (i <- data.requestDescriptors) yield { println(i)
      RequestWithCost(i.video, i.endPoint, data.endpoints(i.endPoint).datacenterLantency, i.requests) }).sorted.reverse

  def fillCaches (sortedRequests: IndexedSeq[RequestWithCost], cachesStatus: Array[CacheStatus], data: Data) = {
    for (
      i <- sortedRequests;
      ep = data.endpoints(i.endPoint) ){
      ep.sortedCachesIndexes.find(c => {
        val cs = cachesStatus(c)
        cs.remaining > data.videos(i.video).size && !cs.hasVideo(i.video)
      }) match {
        case Some(c) => cachesStatus(c).addVideo(i.video, data.videos(i.video).size)
        case None =>
      }
    }
  }

  def main (args: Array[String]): Unit = {
    val l: List[RequestWithCost] = List(RequestWithCost(0, 0, 1, 2), RequestWithCost(0, 0, 3, 4), RequestWithCost(0, 0, 0, 0))
    println(l.sorted)
    val filename = "me_at_the_zoo.in"
    //  val filename = "kittens.in"
    val data = Main.parse(new Scanner(new File(filename)))
    // println(RequestWithCost.fromData(data))
    val cacheStatus = CacheStatus.fromData(data).toArray
    RequestWithCost.fillCaches(RequestWithCost.fromData(data), cacheStatus, data)
    println(cacheStatus.mkString("\n"))
  }
}
