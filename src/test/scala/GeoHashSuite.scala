package light.util.geohash

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class GeoHashSuite extends FunSuite with ShouldMatchers {
  import GeoHash._

  test("test decoding") {
    val (lat,lng) = Decode("dqcw4bnrs6s7")
    lat should be ( 39.02474 plusOrMinus 0.00001)
    lng should be (-76.51100 plusOrMinus 0.00001)
  }

  test("test encoding") {
    val geohash = "dqcw4bnrs6s7"
    val (lat,lng) = Decode(geohash)
    val (geohash_, box) = Encode(lat,lng,12)
    geohash_ should equal (geohash)
  }

}

