package light.util.geohash

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class GeoHashSuite extends FunSuite with ShouldMatchers {
  import GeoHash._

    val (id, box) = Encode(39.02474, -76.51100, 10)

    println(id)
    println(box)

}

