object RunLengthEncoding {

  def runLengthEnc[A](xs: List[A]): List[(Int, A)] = {
    def loop(ys: List[A], currTuple: Option[(Int, A)], acc: List[(Int, A)]): List[(Int, A)] = {
      ys match {
        case hd :: tl =>
          currTuple match {
            case Some(t @ (n, c)) =>
              if (hd == c) loop(tl, Some(n + 1, c), acc)
              else loop(tl, Some(1, hd), acc :+ t)
            case _ => loop(tl, Some(1, hd), acc)
          }
        case _ =>
          currTuple match {
            case Some(t) => acc :+ t
            case _ => acc
          }
      }
    }
    loop(xs, None, List())
  }

  def runLengthDec[A](r: List[(Int, A)]): List[A] = {
    r flatMap { case (n, x) => List.fill(n)(x) }
  }
}
