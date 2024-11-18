package gedcom

//Denne vil sette dataen inn i en Person type.
//Her lager vi et singleton objekt med metoder for å parse person-data.

object GedcomParser {

  def parsePerson(lines: List[String]): Option[Person] = {
    case class PersonBuilder(id: String = "",
                             name: Option[String] = None,
                             birthDate: Option[String] = None,
                             birthPlace: Option[String] = None,
                             deathDate: Option[String] = None,
                             deathPlace: Option[String] = None
                            )


    def parseName(nameLine: String): Option[String] = {
      nameLine.split("/").toList match {
        case firstName :: lastName :: _ => Some(s"$firstName $lastName".trim)
        case _ => None
      }
    }

    //lager en string som starter på index 7 i noen av linjene.
    // DVS alt som kommer etter det som står i startsWith, da disse tar opp 6 indexer.

    val result = lines.foldLeft(PersonBuilder()) { (builder, line) => {
      line match {
        case l if l.startsWith("0 @I") => builder.copy(id = l.split("@")(1))
        case l if l.startsWith("1 NAME") => builder.copy(name = parseName(l.substring(7)))
        case l if l.startsWith("1 BIRT") => builder // Birth event found, no change
        case l if l.startsWith("2 DATE") && builder.birthDate.isEmpty && l.length > 7 =>
          builder.copy(birthDate = Some(l.substring(7)))
        case l if l.startsWith("2 PLAC") && builder.birthPlace.isEmpty && l.length > 7  =>
          builder.copy(birthPlace = Some(l.substring(7)))
        case l if l.startsWith("1 DEAT") => builder // Death event found, no change
        case l if l.startsWith("2 DATE") && builder.deathDate.isEmpty && l.length > 7  =>
          builder.copy(deathDate = Some(l.substring(7)))
        case l if l.startsWith("2 PLAC") && builder.deathPlace.isEmpty && l.length > 7  =>
          builder.copy(deathPlace = Some(l.substring(7)))
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
}