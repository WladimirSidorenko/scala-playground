import scala.util.Random


def s99_P01(list: Seq[Int]): Int =
  list.last


def s99_P02(list: Seq[Int]): Int =
  list.length match
    case 0 | 1 => -1
    case length => list(length - 2)


@main def hello: Unit =
  val list = Seq.fill(5)(Random.nextInt)
  println(s"List $list")
  var i = 1
  println(s"Task ${i}: Find the last element of a list: ${s99_P01(list)}")
  i += 1
  println(s"Task ${i}: Find the last but one element of a list: ${s99_P02(list)}")
  i += 1
  println()
