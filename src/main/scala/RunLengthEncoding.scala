import scala.annotation.tailrec

object RunLengthEncoding {

  def runLengthEnc[A](xs: List[A]): List[(Int, A)] = {
    @tailrec
    def loop(ys: List[A], maybeTuple: Option[(Int, A)], acc: List[(Int, A)]): List[(Int, A)] = {
      ys match {
        case curr :: rest =>
          maybeTuple match {
            case Some(tuple @ (n, prev)) =>
              if (curr == prev) loop(rest, Some(n + 1, prev), acc)
              else loop(rest, Some(1, curr), acc :+ tuple)
            case _ => loop(rest, Some(1, curr), acc)
          }
        case _ =>
          maybeTuple match {
            case Some(tuple) => acc :+ tuple
            case _ => acc
          }
      }
    }
    loop(xs, None, List())
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
