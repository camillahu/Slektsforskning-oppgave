package gedcom

// For processing, finding and displaying GEDCOM data
// Hver person blir en "block".

object GedcomProcessor {
  def processLines(lines: List[String]): List[Person] = {
    def groupLines(lines: List[String]): List[List[String]] = {
      lines.foldRight(List.empty[List[String]]) { (line, acc)
      =>
        if (line.startsWith("0 @I")) { //indikerer vanligvis starten av en ny person.
          List(line) :: acc //lager en ny liste med denne linja og legger det foran acc
        } else {
          acc.headOption.map(head => (line :: head) :: acc.tail).getOrElse(acc)
          //prøver å legge til linja til den nyeste personen sin liste.
          // acc.headOption henter første acc om den eksisterer.
          //hvis det er en acc addes denne linja til blokken og endrer ikke på noen andre blokker.
          //hvis det ikke er noen acc, returneres alle acc uendret.
        }

      }
    }

    groupLines(lines).flatMap(GedcomParser.parsePerson)
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