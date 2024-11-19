package gedcom

// For processing, finding and displaying GEDCOM data
// Hver person blir en "block".

object GedcomProcessor {
  def processLines(lines: List[String]): List[Person] = {
    val personBlocks = lines.foldLeft(List[List[String]]()) { (blocks, line) =>
      if (line.startsWith("0 @") && line.endsWith("INDI")) {
        List(line) :: blocks
      } else {
        blocks.headOption.map(block => (line :: block) :: blocks.tail).getOrElse(blocks)
      }
    }.reverse
    personBlocks.flatMap(block => GedcomParser.parsePerson(block.reverse))
  }

  def findPersonById(persons: List[Person], id: String): Option[Person] = {
    persons.find(_.id == id)
  }

  def findPersonByName(persons: List[Person], inputName: String): List[Person] = {
    persons.filter { person =>
      person.name.exists(_.toLowerCase.startsWith(inputName.toLowerCase()))
    }
  }

def displayPerson(person: Person): String = {
  List(s"ID: ${person.id}",
    s"Name: ${person.name.getOrElse("Unknown")}",
    s"Birth Date: ${person.birthDate.getOrElse("Unknown")}",
    s"Birth Place: ${person.birthPlace.getOrElse("Unknown")}",
    s"Death Date: ${person.deathDate.getOrElse("Unknown")}",
    s"Death Place: ${person.deathPlace.getOrElse("Unknown")}").mkString("\n")
}
}