import RunLengthEncoding._
import org.scalacheck.{Gen, Prop, Properties}
import Gen.{choose, alphaNumChar, sized}

object RunLengthEncodingTest extends Properties("Run length encoding") {

  val genOutput: Gen[List[(Int, Char)]] = {
    def rleItem: Gen[(Int, Char)] = for {
      n <- choose(1, 20)
      c <- alphaNumChar
    } yield (n, c)
    def rleList(size: Int): Gen[List[(Int, Char)]] = {
      if (size <= 1) rleItem.map(List(_))
      else for {
        tail@(_, c1) :: _ <- rleList(size - 1)
        head <- rleItem retryUntil(_._2 != c1)
      } yield head :: tail
    }
    sized(rleList)
  }

  property("roundTrip") = Prop.forAll(genOutput) { r =>
    runLengthEnc(runLengthDec(r)) == r
  }

  property("roundTrip (streaming)") = Prop.forAll(genOutput) { r =>
    runLengthEncStream(runLengthDecStream(r.toStream)).toList == r
  }
}
