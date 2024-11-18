package gedcom

//To represent family relationships
//En case class må altså ikke new'es

case class Family(id: String,
                  husband:Option[Person],
                  wife: Option[Person],
                  children: List[Person])