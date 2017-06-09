object Main {
  def main(args: Array[String]): Unit = {

    val xs = List(1, 1, 1, 1, 2, 2, 2, 3, 3)

    val r1 = RunLengthEncoding.runLengthEnc(xs)
    println(s"r1: $r1")

    val r2 = RunLengthEncoding.runLengthEncStream(xs.toStream)
    println(s"r2: $r2")
    println(s"r2.toList: ${r2.toList}")
  }
}
