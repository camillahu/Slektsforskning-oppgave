package gedcom

//Denne vil sette dataen inn i en Person type.
//Her lager vi et singleton objekt med metoder for å parse person-data.

object GedcomParser {

//  def parseGedcom(lines: List[String]): (Option[Person], List[Family]) = {
//    val person = parsePerson(lines)
//    val personsMap = person.map(p => p.id -> p).toMap
//    val families = parseFamily(lines, personsMap)
//    (person, families)
//  }

  def parsePerson(lines: List[String]): Option[Person] = {
    case class PersonBuilder(id: String = "",
                             name: Option[String] = None,
                             birthDate: Option[String] = None,
                             birthPlace: Option[String] = None,
                             deathDate: Option[String] = None,
                             deathPlace: Option[String] = None,
                             hasBirthEvent: Boolean = false,
                             hasDeathEvent: Boolean = false
                            )


    def parseName(nameLine: String): Option[String] = {
      nameLine.split("/").toList match {
        case firstName :: lastName :: _ => Some(s"${firstName.trim} ${lastName.trim}")
        case _ => None
      }
    }

    //lager en string som starter på index 7 i noen av linjene.
    // DVS alt som kommer etter det som står i startsWith, da disse tar opp 6 indexer.

    val result = lines.foldLeft(PersonBuilder()) { (builder, line) => {
      val regexComma = "(?<=\\s),\\s"
      line match {
        case l if l.startsWith("0 @") && l.endsWith("INDI") => builder.copy(id = l.split("@")(1))
        case l if l.startsWith("1 NAME") => builder.copy(name = parseName(l.substring(7)))

        case l if l.startsWith("1 BIRT") => builder.copy(hasBirthEvent = true)
        case l if l.startsWith("2 DATE")
          && builder.hasBirthEvent
          && builder.birthDate.isEmpty
          && l.length > 7 =>
          builder.copy(birthDate = Some(l.substring(7)))
        case l if l.startsWith("2 PLAC")
          && builder.hasBirthEvent
          && builder.birthDate.isEmpty
          && l.length > 7 =>
          builder.copy(birthPlace = Some(l.substring(7).replaceAll(regexComma, "").trim))
        case l if l.startsWith("1 DEAT") => builder.copy(hasBirthEvent = true)
        case l if l.startsWith("2 DATE")
          && builder.hasDeathEvent
          && builder.deathDate.isEmpty
          && l.length > 7  =>
          builder.copy(deathDate = Some(l.substring(7)))
        case l if l.startsWith("2 PLAC")
          && builder.hasDeathEvent
          && builder.deathPlace.isEmpty
          && l.length > 7  =>
          builder.copy(deathPlace = Some(l.substring(7).replaceAll(regexComma, "").trim))
        case _ => builder
      }
    }
    }

    if (result.id.nonEmpty) {
      Some(Person(result.id, result.name, result.birthDate, result.birthPlace, result.deathDate, result.deathPlace))
    } else {
      None
    }
  }

//  def parseFamily(lines: List[String], person: Option[Person]): List[Family] = {
//    case class FamilyBuilder(id: String = "",
//                             husbandId: Option[String] = None,
//                             wifeId: Option[String] = None,
//                             childrenIds: List[String] = List.empty
//                            )
//    val result = lines.foldLeft(FamilyBuilder()) { (builder, line) => {
////      val regexComma = "(?<=\\s),\\s"
//      line match {
//        case l if l.startsWith("0 @F") => builder.copy(id = l.split("@")(1))
//        case l if l.startsWith("1 HUSB") => builder.copy(husbandId = Some(l.split("@")(1)))
//        case l if l.startsWith("1 WIFE") => builder.copy(wifeId = Some(l.split("@")(1)))
//        case l if l.startsWith("1 BIRT") => builder // Birth event found, no change
//        case l if l.startsWith("2 DATE") && builder.birthDate.isEmpty && l.length > 7 =>
//          builder.copy(birthDate = Some(l.substring(7)))
//        case l if l.startsWith("2 PLAC") && builder.birthPlace.isEmpty && l.length > 7  =>
//          builder.copy(birthPlace = Some(l.substring(7).replaceAll(regexComma, "").trim))
//        case l if l.startsWith("1 DEAT") => builder // Death event found, no change
//        case l if l.startsWith("2 DATE") && builder.deathDate.isEmpty && l.length > 7  =>
//          builder.copy(deathDate = Some(l.substring(7)))
//        case l if l.startsWith("2 PLAC") && builder.deathPlace.isEmpty && l.length > 7  =>
//          builder.copy(deathPlace = Some(l.substring(7).replaceAll(regexComma, "").trim))
//        case _ => builder
//      }   }
  ////
  ////  }
//
}
