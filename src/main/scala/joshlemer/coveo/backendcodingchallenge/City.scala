package joshlemer.coveo.backendcodingchallenge

import ch.hsr.geohash.WGS84Point

case class LatLong(latitude: Double, longitude: Double) {
  def point: WGS84Point = new WGS84Point(latitude, longitude)
}


case class City(name: String, latLong: LatLong) {
  lazy val point: WGS84Point = latLong.point
}
