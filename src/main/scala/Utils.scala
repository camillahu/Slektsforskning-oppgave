object Utils {

  def getInput(prompt: String): String = {
    println(s"$prompt :")
    val input = scala.io.StdIn.readLine()
    if(input.toLowerCase == "quit") s"Exiting program." else s"$input"
  }
}
