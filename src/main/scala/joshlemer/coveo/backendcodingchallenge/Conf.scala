package joshlemer.coveo.backendcodingchallenge

case class Conf(interface: String, port: Int, appName: String)

object Conf {
  def defaults: Conf = Conf("localhost", 8080, "Backend-Coding-Challenge")
}
