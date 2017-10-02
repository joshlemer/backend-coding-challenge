package joshlemer.coveo.backendcodingchallenge

import ch.hsr.geohash.WGS84Point

case class LatLong(latitude: Double, longitude: Double) {
  def distanceInMeters(that: LatLong): Double = LatLong.distance(this, that)
}

object LatLong {

  /** Took the algorithm from the internet in a java snippet and converted to Scala */
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


case class City(name: String, latLong: LatLong)
