import scala.util.Random


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


def pack[T](ls: List[T]): List[T | List[T]] = {
  def compressR(result: List[T | List[T]], curList: List[T]): List[T | List[T]] =
    curList match {
      case h :: tail if tail.isEmpty || tail.head != h => compressR(h :: result, tail)
      case h :: tail => compressR((h :: tail.takeWhile(_ == h)) :: result, tail.dropWhile(_ == h))
      case Nil => result.reverse
    }
  compressR(Nil, ls)
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
  println()
