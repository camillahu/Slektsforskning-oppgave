package gedcom

//To represent a person in the family tree
//En case class må altså ikke new'es
//option brukes fordi de ikke trenger å ha verdi.
case class Person(
                   id: String,
                   name: Option[String],
                   birthDate: Option[String],
                   birthPlace: Option[String],
                   deathDate: Option[String],
                   deathPlace: Option[String])
