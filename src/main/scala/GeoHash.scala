package light.util.geohash

import scala.collection.mutable


// geohash精度的设定参考 http://en.wikipedia.org/wiki/Geohash
// geohash length   lat bits    lng bits    lat error   lng error   km error
// 1                2           3           ±23         ±23         ±2500
// 2                5           5           ± 2.8       ± 5.6       ±630
// 3                7           8           ± 0.70      ± 0.7       ±78
// 4                10          10          ± 0.087     ± 0.18      ±20
// 5                12          13          ± 0.022     ± 0.022     ±2.4
// 6                15          15          ± 0.0027    ± 0.0055    ±0.61
// 7                17          18          ±0.00068    ±0.00068    ±0.076
// 8                20          20          ±0.000085   ±0.00017    ±0.019

class Box(MinLatVal: Double, MaxLatVal: Double, MinLngVal: Double, MaxLngVal: Double) {

  // 纬度
  var MinLat:Double = MinLatVal
  var MaxLat:Double = MaxLatVal
  // 经度
  var MinLng:Double = MinLngVal
  var MaxLng:Double = MaxLngVal

  def Width(): Double = {
    this.MaxLng - this.MinLng
  }

  def Height(): Double = {
    this.MaxLat - this.MinLat
  }

  override def toString(): String = {
    "MinLat:" + MinLat + " MaxLat:" + MaxLat + " MinLng:" + MinLng + " MaxLng:" + MaxLng
  }

}

object GeoHash {

  val BASE32			= "0123456789bcdefghjkmnpqrstuvwxyz"
  val MAX_LATITUDE :Double	= 90
  val MIN_LATITUDE :Double	= -90
  val MAX_LONGITUDE :Double	= 180
  val MIN_LONGITUDE :Double	= -180

  val bits = Array[Byte](16, 8, 4, 2, 1)
  val base32 = BASE32.toCharArray

  implicit def int2byte(int: Int) = {
    int.toByte
  }

  def Encode(latitude: Double, longitude: Double, precision: Int): (String, Box) = {

    val geohash = new StringBuilder
    var minLat = MIN_LATITUDE
    var maxLat = MAX_LATITUDE
    var minLng = MIN_LONGITUDE
    var maxLng = MAX_LONGITUDE

    var mid: Double = 0
    var length = 0
    var isEven = true
    var ch: Byte = 0
    var bit = 0

    while (length < precision) {
      if (isEven) {
        mid = (minLng + maxLng) / 2
        if (mid < longitude) {
          ch |= bits(bit)
          minLng = mid
        } else {
          maxLng = mid
        }
      } else {
        mid = (minLat + maxLat) / 2
        if (mid < latitude) {
          ch |= bits(bit)
          minLat = mid
        } else {
          maxLat = mid
        }
      }

      isEven = !isEven
      
      if(bit < 4) {
        bit += 1
      } else {
        geohash += base32(ch)
        length += 1
        ch = 0
        bit = 0
      }
    }

    var b = new Box(minLat,maxLat,minLng,maxLng)

    (geohash.toString, b)
  }

}


