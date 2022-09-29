import scala.util.Random
import com.typesafe.scalalogging.Logger

val LOGGER = Logger("scala-playground")

def getByIdx(nums: Seq[Int], idx: Int): Int =
  if idx < nums.length then
    return nums(idx)
  return -1


def isPalindrome(nums: Seq[_]): Boolean =
  val middle = nums.length / 2
  for (i, j) <- ((0 until middle) zip (nums.length - 1 to middle by -1))
  do
    if nums(i) != nums(j) then
      return false
  return true


def flatten(ls: List[Any]): List[Any] = ls flatMap {
  case ms: List[_] => flatten(ms)
  case e => List(e)
}


def compress[T](ls: Seq[T]): Seq[T] = {
  var ret: List[T] = Nil
  var prev: Option[T] = None
  ls.foreach(
    (el) => {
      if prev.isEmpty || el != prev.get then
        ret = ret ::: List(el)
        prev = Some(el)
    }
  )
  return ret;
}


def pack[T](ls: List[T]): List[List[T]] = {
  def compressR(result: List[List[T]], curList: List[T]): List[List[T]] =
    curList match {
      case h :: tail if tail.isEmpty || tail.head != h => compressR(List(h) :: result, tail)
      case h :: tail => compressR((h :: tail.takeWhile(_ == h)) :: result, tail.dropWhile(_ == h))
      case Nil => result.reverse
    }
  compressR(Nil, ls)
}


def lengthEncode[T](lst: List[T]): List[(Int, T)] = pack(lst).map{
  (el) => (el.length, el(0))
}


def modifyLengthEncode[T](lst: List[(Int, T)]): List[(Int, T) | T] = lst.map{
  case (1, item) => item
  case el => el
}


def lengthDecode[T](lst: List[(Int, T)]): List[T] = lst.flatMap{
  (length, el) => Seq.fill(length)(el)
}


def directLengthEncode[T](ls: List[T]): List[(Int, T)] =
  if (ls.isEmpty) Nil
  else {
    val (packed, next) = ls span { _ == ls.head }
    (packed.length, packed.head) :: directLengthEncode(next)
  }


def duplicateN[T](n: Int, ls: List[T]): List[T] =
  ls.flatMap(List.fill(n)(_))


def dropN[T](N: Int, ret: List[T], seq: List[T]): List[T] = seq match {
    case Nil => ret
    case _ => {
      val (head, tail) = seq.splitAt(N - 1)
      dropN(N, ret ::: head, if tail.nonEmpty then tail.tail else Nil)
    }
}


def splitN[T](N: Int, seq: List[T]): (List[T], List[T]) =
  seq.splitAt(N)


def slice[T](from: Int, until: Int, seq: List[T]): List[T] =
   seq.slice(from, until)


def rotate[T](n: Int, seq: List[T]): List[T] =
   if (n < 0) then
     import java.lang.Math.abs
     val k = abs(n)
     seq.takeRight(k) ::: seq.dropRight(k)
   else
     seq.drop(n) ::: seq.take(n)


def removeAt[T](n: Int, seq: List[T]): (List[T], T) =
   val (head, tail) = seq.splitAt(n)
   (head ::: tail.tail, tail.head)


def insertAt[T](el: T, pos: Int, seq: List[T]): List[T] =
  val (head, tail) = seq.splitAt(pos)
  head ::: (el :: tail)


def range(start: Int, end: Int): List[Int] =
   (start to end).toList


def randomSelect[T](n: Int, seq: List[T]): List[T] =
  val seqLength = seq.length
  n match {
    case m if m <= 0 || seqLength == 0 => Nil
    case _ => {
      val idx = Random.nextInt(seqLength)
      val (reducedSeq: List[T], el: T) = removeAt(idx, seq)
      el :: randomSelect(n - 1, reducedSeq)
    }
  }


def lotto(n: Int, end: Int): List[Int] =
   randomSelect(n, (0 to end).toList)


def randomPermute[T](seq: List[T]): List[T] =
   randomSelect(seq.length, seq)


def combinations[T](k: Int, seq: List[T]): List[List[T]] =
   def generateCombinations(k: Int, head: T, tail: List[T]): Iterator[List[T]] =
     for t <- tail.sliding(k) yield head :: t

   seq match {
     case head :: tail if tail.length > k => {
       generateCombinations(k - 1, head, tail).toList ::: combinations(k, tail)
     }
     case _ => Nil
   }


@main def hello: Unit =
  val nums = for i <- 1 to 5 yield Random.nextInt(i)
  println(s"List $nums")
  var i = 1
  println(s"Task ${i}: Find the last element of a list: ${nums.last}")
  i += 1
  println(s"Task ${i}: Find the last but one element of a list: ${getByIdx(nums, nums.length - 2)}")
  i += 1
  val k = Random.nextInt(nums.length)
  println(s"Task ${i}: Find the ${k}-th element of a list: ${getByIdx(nums, k)}")
  i += 1
  println(s"Task ${i}: Find the number of elements of a list: ${nums.length}")
  i += 1
  println(s"Task ${i}: Reverse a list: ${nums.reverse}")
  i += 1
  println(s"Find out whether a list is a palindrome: ${isPalindrome(nums)}")
  val abba = "abba"
  println(s"Find out whether 'abba' is a palindrome: ${isPalindrome(abba)}")
  val abcba = "abcba"
  println(s"Find out whether 'abcba' is a palindrome: ${isPalindrome(abcba)}")
  i += 1
  val numList = List(List(1, 2, 3), 4, List(5, List(6, 7)))
  println(s"Task ${i}: Flatten a nested list structure: ${flatten(numList)}")
  i += 1
  val duplicateList = List('a', 'a', 'a', 'a', 'b', 'c', 'c',
    'a', 'a', 'd', 'e', 'e', 'e', 'e')
  println(s"Task ${i}: Eliminate consecutive duplicates of list elements.: ${compress(duplicateList)}")
  i += 1
  println(s"Task ${i}: Pack consecutive duplicates of list elements into sublists.: ${pack(duplicateList)}")
  i += 1
  val lengthEncoded = lengthEncode(duplicateList)
  println(s"Task ${i}: Run-length encoding of a list: ${lengthEncoded}")
  i += 1
  println(s"Task ${i}: Modified run-length encoding: ${modifyLengthEncode(lengthEncoded)}")
  i += 1
  println(s"Task ${i}: Decode a run-length encoded list: ${lengthDecode(lengthEncoded)}")
  i += 1
  println(s"Task ${i}: Run-length encoding of a list (direct solution): ${directLengthEncode(duplicateList)}")
  i += 1
  println(s"Task ${i}: Duplicate the elements of a list: ${duplicateN(2, List('a', 'b', 'c', 'c', 'd'))}")
  i += 1
  println(s"Task ${i}: Duplicate the elements of a list a given number of times: ${duplicateN(3, List('a', 'b', 'c', 'c', 'd'))}")
  val N = 3;
  println(s"Task ${i}: Drop every ${N}th element from a list ${duplicateList}: ${dropN(N, Nil, duplicateList)}")
  i += 1
  val chars = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k');
  println(s"Task ${i}: Split a list into two parts: ${splitN(N, chars)}")
  i += 1
  println(s"Task ${i}: Extract a slice from a list: ${slice(2, 6, chars)}")
  i += 1
  println(s"Task ${i}: Rotate a list N places to the left: ${rotate(3, chars)}")
  println(s"Task ${i}: Rotate a list N places to the left: ${rotate(-2, chars)}")
  i += 1
  println(s"Task ${i}: Remove the Kth element from a list: ${removeAt(1, chars)}")
  i += 1
  println(s"""Task ${i}: Insert an element at a given position into a list: ${insertAt("new", 1, List("a", "b", "c", "d"))}""")
  i += 1
  println(s"Task ${i}: Create a list containing all integers within a given range: ${range(4, 9)}")
  i += 1
  println(s"Task ${i}: Extract a given number of randomly selected elements from a list: ${randomSelect(3, chars)}")
  i += 1
  println(s"Task ${i}: Lotto: Draw N different random numbers from the set 1..M: ${lotto(6, 49)}")
  i += 1
  println(s"Task ${i}: Generate a random permutation of the elements of a list: ${randomPermute(chars)}")
  i += 1
  val K = 3
  val combos = combinations(3, chars)
  println(s"Task ${i}: Generate the combinations of $K distinct objects chosen from the ${chars.length} elements of a list: ${combos.take(3)} (${combos.length})")
  println()
