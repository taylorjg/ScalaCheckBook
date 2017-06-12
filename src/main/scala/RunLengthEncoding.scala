object RunLengthEncoding {

  def runLengthEnc[A](xs: List[A]): List[(Int, A)] =
    xs match {
      case hd :: _ =>
        val (hds, remainder) = xs.span(_ == hd)
        val n = hds.length
        val tuple = (n, hd)
        tuple :: runLengthEnc(remainder)
      case _ =>
        Nil
    }

  def runLengthDec[A](r: List[(Int, A)]): List[A] = {
    r flatMap { case (n, x) => List.fill(n)(x) }
  }

  def runLengthEncStream[A](xs: Stream[A]): Stream[(Int, A)] =
    xs match {
      case hd #:: _ =>
        val (hds, remainder) = xs.span(_ == hd)
        val n = hds.length
        val tuple = (n, hd)
        Stream.cons(tuple, runLengthEncStream(remainder))
      case _ =>
        Stream.empty
    }

  def runLengthDecStream[A](r: Stream[(Int, A)]): Stream[A] = {
    r flatMap { case (n, x) => Stream.fill(n)(x) }
  }
}
