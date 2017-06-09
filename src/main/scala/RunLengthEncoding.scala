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

//  def runLengthEncStream[A](xs: Stream[A]): Stream[(Int, A)] = {
//
//    def f(ys: Stream[A]): (Option[(Int, A)], Stream[A]) =
//      ys match {
//        case y #:: _ =>
//          val (same, different) = ys.span(_ == y)
//          val n = same.length
//          (Some((n, y)), different)
//        case _ => (None, Stream.empty)
//      }
//
//    def nextTuple(ys: Stream[A]): Stream[(Int, A)] =
//      f(ys) match {
//        case (Some(tuple), rest) => Stream.cons(tuple, nextTuple(rest))
//        case _ => Stream.empty
//      }
//
//    nextTuple(xs)
//  }

  def runLengthEncStream[A](xs: Stream[A]): Stream[(Int, A)] = {

    def loop(ys: Stream[A]): Stream[(Int, A)] = {
      (ys match {
        case y #:: _ =>
          val (same, rest) = ys.span(_ == y)
          val n = same.length
          val tuple = (n, y)
          Some(tuple, rest)
        case _ => None
      }) match {
        case Some((tuple, rest)) => Stream.cons(tuple, loop(rest))
        case _ => Stream.empty
      }
    }

    loop(xs)
  }

  def runLengthDecStream[A](r: Stream[(Int, A)]): Stream[A] = {
    r flatMap { case (n, x) => Stream.fill(n)(x) }
  }
}
