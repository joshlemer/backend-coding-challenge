package joshlemer.coveo.backendcodingchallenge

case class LatLong(latitude: Double, longitude: Double)

case class City(name: String, latLong: LatLong)
