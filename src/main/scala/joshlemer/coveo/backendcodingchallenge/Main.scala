package joshlemer.coveo.backendcodingchallenge

import java.nio.charset.Charset

import joshlemer.coveo.backendcodingchallenge.parsing.Geoname

object Main {

  def main(args: Array[String]): Unit = {
    val geonames = Geoname.parseFile("data/cities_canada-usa.tsv")

    val cities = geonames.map(_.toCity)



    geonames take (10) foreach println

//    import better.files._
//
//    val path = "/home/josh/workspace/backend-coding-challenge/data/cities_canada-usa.tsv"
//    println(File(path).lines(new UnicodeDecoder(Charset.defaultCharset()).charset()).take(5).mkString("\n"))
  }
}
