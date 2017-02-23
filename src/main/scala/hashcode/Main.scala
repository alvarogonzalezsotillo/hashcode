package hashcode

import java.util.Scanner


object Main extends App {


  /*
  5 2 4 3 100
  50 50 80 30 110
  1000 3
  0 100
  2 200
  1 300
  500 0
  3 0 1500
  0 1 1000
  4 0 500
  1 0 1000

  5 videos, 2 endpoints, 4 request descriptions, 3 caches 100MB each.
    Videos 0, 1, 2, 3, 4 have sizes 50MB, 50MB, 80MB, 30MB, 110MB.
  Endpoint 0 has 1000ms datacenter latency and is connected to 3 caches:
    The latency (of endpoint 0) to cache 0 is 100ms.
  The latency (of endpoint 0) to cache 2 is 200ms.
    The latency (of endpoint 0) to cache 1 is 200ms.
    Endpoint 1 has 500ms datacenter latency and is not connected to a cache.
  1500 requests for video 3 coming from endpoint 0.
  1000 requests for video 0 coming from endpoint 1.
  500 requests for video 4 coming from endpoint 0.
  1000 requests for video 1 coming from endpoint 0.
  */
  def parse(in: Scanner) = {
    val Array(v, e, r, c, x) = in.nextLine().split(" ").map(_.toInt)

    // VIDEOS
    val videos = for (_ <- 0 until v) yield new Video( in.nextInt() )

    // ENDPOINTS
    val endpoints = for (endpoint <- 0 until e) yield {
      val datacenterLatency = in.nextInt
      val numberOfCaches = in.nextInt

      val latenciesToCaches = (for (cache <- 0 until numberOfCaches) yield {
        val cacheIndex = in.nextInt
        val latencyEndpointToCache = in.nextInt
        (cacheIndex, latencyEndpointToCache)
      }).toMap

      Endpoint(datacenterLatency, latenciesToCaches)
    }

    // REQUEST DESCRIPTORS
    val requestDescriptors = for( descriptor <- 0 until r) yield{
      val (video, endpoint, requests) = ( in.nextInt, in.nextInt, in.nextInt )
      RequestDescriptor( requests, video, endpoint )
    }

    Data(c, x, videos, endpoints, requestDescriptors )
  }

  case class RequestDescriptor( requests: Int, video: Int, endPoint: Int )

  case class Video(size: Int)

  case class Endpoint(datacenterLantency: Int, latenciesToCaches: Map[Int, Int]) {
    def latencyToCache(index: Int) = latenciesToCaches(index)
  }

  case class Data( caches: Int,
                   cacheSize: Int,
                   videos: IndexedSeq[Video],
                   endpoints: IndexedSeq[Endpoint],
                   requestDescriptors: IndexedSeq[RequestDescriptor] )


}
