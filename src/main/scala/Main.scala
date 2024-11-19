import gedcom.{GedcomProcessor, GedcomReader}
import Utils.getInput

object Main extends App {
  private val filePath = "src/main/scala/FamilyTree/gedcom.ged"
  private val filePathDemo = "src/main/scala/FamilyTree/demo.ged"
  val lines = GedcomReader.readFile(filePath)
  val persons = GedcomProcessor.processLines(lines)

  println(s"Processed ${persons.length} persons")

//  persons.headOption.foreach { person=>
//    println("\nFirst person in the GEDCOM file:")
//    println(GedcomProcessor.displayPerson(person))
//  }

//  private val personId = getInput("Enter a person ID to search (or 'quit' to exit): ")
//  GedcomProcessor.findPersonById(persons, personId).foreach { person=>
//    println(s"Person with ID $personId:")
//    println(GedcomProcessor.displayPerson(person))
//  }

  private val personSearch = getInput("Enter a first name to search :")
  println(s"Persons with name that starts with $personSearch:")
  GedcomProcessor.findPersonByName(persons, personSearch).foreach { person=>
    println(GedcomProcessor.displayPerson(person))
  }

//  persons.foreach{ person=>
//    println(GedcomProcessor.displayPerson(person))
//  }


}
