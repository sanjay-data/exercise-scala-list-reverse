import scala.annotation.tailrec

object TailRecursiveAlgos extends App {


  @tailrec
  def reverseList[T](list: List[T], suffix: List[T] = Nil): List[T] = {
    list match {
      case Nil => suffix
      case first :: remaining =>
        reverseList(remaining, first :: suffix)
    }
  }

  def shorten(string: String): String = shorten(string.toList)

  @tailrec
  def shorten(list: List[Char], prefix: String = ""): String = {
    list match {
      case Nil => prefix
      case first :: _ =>
        val sameLetterList = list.takeWhile(_ == first)
        val suffix =
          if (sameLetterList.size == 1) s"$first"
          else s"${sameLetterList.size}$first"
        val (_, remaining) = list.splitAt(sameLetterList.size)
        shorten(remaining, prefix + suffix)
    }
  }

  def expand(string: String): String = expand(string.toList)

  @tailrec
  def expand(list: List[Char], generatedString: String = ""): String = {
    list match {
      case Nil      => generatedString
      case x :: Nil => expand(Nil, generatedString + x)
      case first :: remaining =>
        val (str, n) = if (first.isDigit) {
          val digitsList = list.takeWhile(_.isDigit)
          val n = digitsList.mkString("").toInt
          val repeatedChar = list(digitsList.size)
          val str = (1 to n-1).foldLeft("")((accumulated, n) => {
            accumulated + repeatedChar
          })
          (str, digitsList.size)
        } else {
          (s"$first", 1)
        }
        val (_, remaining) = list.splitAt(n)
        expand(remaining, generatedString + str);
    }
  }


}
