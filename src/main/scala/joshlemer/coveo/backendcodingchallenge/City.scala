package joshlemer.coveo.backendcodingchallenge

import ch.hsr.geohash.WGS84Point
import ch.hsr.geohash.util.VincentyGeodesy

case class LatLong(latitude: Double, longitude: Double) {
  def point: WGS84Point = new WGS84Point(latitude, longitude)

  def distanceInMeters(that: LatLong): Double = LatLong.distance(this, that)
}

object LatLong {
  def distance(a: LatLong, b: LatLong): Double = {
    import Math._

    val R = 6371 // Radius of the earth

    val latDistance = toRadians(b.latitude - a.latitude)
    val lonDistance = toRadians(b.longitude - a.longitude)

    val c =
      sin(latDistance / 2) * sin(latDistance / 2) +
        cos(toRadians(a.latitude)) * cos(toRadians(b.latitude)) * sin(lonDistance / 2) * sin(lonDistance / 2)

    val d = 2 * Math.atan2(Math.sqrt(c), Math.sqrt(1 - c))

    R * c * 1000 // convert to meters
  }
}


case class City(name: String, latLong: LatLong) {
  lazy val point: WGS84Point = latLong.point
}
