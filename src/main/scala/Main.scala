import scala.util.Random


def getByIdx(nums: Seq[Int], idx: Int): Int =
  if idx < nums.length then
    return nums(idx)
  return -1


def isPalindrome(nums: Seq[Int] | Seq[Char]): Boolean =
  val middle = nums.length / 2
  for i <- 0 until middle
      j <- nums.length - 1 to middle by -1
  do
    if nums(i) != nums(j) then
      return false
  return true


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
  println(s"Find out whether a list is a palindrome: ${isPalindrome(nums)}")
  i += 1
  println()
