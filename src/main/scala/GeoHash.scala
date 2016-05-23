package light.util.geohash

import scala.collection.mutable._


// reference http://en.wikipedia.org/wiki/Geohash
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

  // latitude
  var MinLat:Double = MinLatVal
  var MaxLat:Double = MaxLatVal
  // longitude
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

  val BASE32			    = "0123456789bcdefghjkmnpqrstuvwxyz"
  val MIN_LATITUDE :Double	= -90
  val MAX_LATITUDE :Double	= 90
  val MIN_LONGITUDE :Double	= -180
  val MAX_LONGITUDE :Double	= 180

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

  def GetNeighbors(latitude: Double, longitude: Double, precision: Int): Array[String] = {

    val neighbors = new ArrayBuffer[String]()

    val (geohash, box) = Encode(latitude, longitude, precision)
    neighbors.append(geohash)

    val (geohashUp, _) = Encode((box.MinLat+box.MaxLat)/2+box.Height, (box.MinLng+box.MaxLng)/2, precision)
    val (geohashDown, _) = Encode((box.MinLat+box.MaxLat)/2-box.Height, (box.MinLng+box.MaxLng)/2, precision)
    val (geohashLeft, _) = Encode((box.MinLat+box.MaxLat)/2, (box.MinLng+box.MaxLng)/2-box.Width, precision)
    val (geohashRight, _) = Encode((box.MinLat+box.MaxLat)/2, (box.MinLng+box.MaxLng)/2+box.Width, precision)
    neighbors.appendAll(Seq(geohashUp,geohashDown,geohashLeft,geohashRight))

    val (geohashLeftUp, _) = Encode((box.MinLat+box.MaxLat)/2+box.Height, (box.MinLng+box.MaxLng)/2-box.Width, precision)
    val (geohashLeftDown, _) = Encode((box.MinLat+box.MaxLat)/2-box.Height, (box.MinLng+box.MaxLng)/2-box.Width, precision)
    val (geohashRightUp, _) = Encode((box.MinLat+box.MaxLat)/2+box.Height, (box.MinLng+box.MaxLng)/2+box.Width, precision)
    val (geohashRightDown, _) = Encode((box.MinLat+box.MaxLat)/2-box.Height, (box.MinLng+box.MaxLng)/2+box.Width, precision)
    neighbors.appendAll(Seq(geohashLeftUp,geohashLeftDown,geohashRightUp,geohashRightDown))

    neighbors.toArray
  }

  //
  def DecodeBounds(geohash: String): ((Double,Double),(Double,Double)) = {

    def toBitList(str: String) = str.flatMap {
      char => ("00000" + base32.indexOf(char).toBinaryString ).
             reverse.take(5).reverse.map('1' == ) } toList
 
    def split(list: List[Boolean]): (List[Boolean], List[Boolean]) = {

      list match{
        case Nil => (Nil,Nil)
        case x::Nil => (x::Nil,Nil)
        case x::y::zs => val (xs,ys) = split(zs); (x::xs,y::ys)
      }
    }
 
    def dehash(xs: List[Boolean], min: Double, max: Double): (Double,Double) = {

      ((min,max) /: xs ) {
        case ((min,max), bool) =>
          if(bool) ((min + max)/2, max)
          else (min, (min + max)/2)
       }
    }
    
    val (xs, ys) = split(toBitList(geohash))
    (dehash(ys,MIN_LATITUDE,MAX_LATITUDE), dehash(xs,MIN_LONGITUDE,MAX_LONGITUDE))
  }

  def Decode( geohash:String ):(Double,Double) = {
    DecodeBounds(geohash) match {
      case ((minLat,maxLat),(minLng,maxLng)) => ((maxLat+minLat)/2, (maxLng+minLng)/2)
    }
  }

}


