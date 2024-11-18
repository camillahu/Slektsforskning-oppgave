package gedcom
import scala.io.Source
//For reading and parsing GEDCOM files
// tip: Using ?

object GedcomReader {
  def readFile(path:String): List[String] = {
    val source = Source.fromFile(path)
    try {
      source.getLines().toList
    } finally {
      source.close()
    }
  }
}

