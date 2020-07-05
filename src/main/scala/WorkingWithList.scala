import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object WorkingWithList extends App {
  @tailrec
  def last[T](list: List[T]): Option[T] = list match {
    case Nil         => None
    case head :: Nil => Some(head)
    case _ :: tail   => last(tail)
  }

  @tailrec
  def penultimate[T](list: List[T]): Option[T] = list match {
    case Nil                           => None
    case _ :: Nil                      => None
    case lastSecondElement :: _ :: Nil => Some(lastSecondElement)
    case _ :: tail                     => penultimate(tail)
  }

  def nth[T](k: Int, list: List[T]): Option[T] = {
    var currentIdx = 0
    var tempList = list

    while (tempList.nonEmpty && currentIdx < k) {
      tempList = tempList.tail
      currentIdx = currentIdx + 1
    }

    tempList.headOption
  }

  def length[T](list: List[T]): Int = {
    if (list.isEmpty) 0
    else length(list.tail) + 1
  }

  def reverse[T](list: List[T]): List[T] = list match {
    case Nil          => Nil
    case head :: Nil  => List(head)
    case head :: tail => reverse(tail) :+ head
  }

  def isPalindrome[T](list: List[T]): Boolean = {
    reverse(list) == list
  }

  def flatten(list: List[Any]): List[Any] = list match {
    case head :: tail if head.isInstanceOf[List[_]] =>
      flatten(head.asInstanceOf[List[_]]) ++ flatten(tail)
    case head :: tail => head :: flatten(tail)
    case Nil          => Nil
  }

  def compress[T](list: List[T]): List[T] = list match {
    case first :: second :: rest if (first == second) => compress(first :: rest)
    case head :: tail =>
      head :: compress(tail)
    case Nil => Nil
  }

  def pack[T](list: List[T], innerList: List[T] = List()): List[List[T]] =
    list match {
      case head :: tail if (innerList.isEmpty) => pack(tail, List(head))
      case head :: tail if (head == innerList.head) =>
        pack(tail, head :: innerList)
      case head :: tail if (head != innerList.head) =>
        innerList :: pack(tail, List(head))
      case Nil => List(innerList)
    }

  def encode[T](list: List[T]): List[(Int, T)] = {
    pack(list).map(innerList => (innerList.size, innerList.head))
  }

  def encodeModified[T](list: List[T]): List[Any] = {
    pack(list).map(
      innerList =>
        innerList match {
          case element :: Nil => element
          case head :: tail   => (tail.size + 1, head)
      }
    )
  }

  def decode[T](encodeList: List[(Int, T)]): List[T] = {
    encodeList.flatMap(t => List.fill(t._1)(t._2))
  }

  def encodeDirect[T](list: List[T]): List[(Int, T)] = {
    def makeEncodeList(targetList: List[T],
                       innerList: List[T] = List()): List[List[T]] =
      targetList match {
        case head :: tail if innerList.isEmpty =>
          makeEncodeList(tail, List(head))
        case head :: tail if head == innerList.head =>
          makeEncodeList(tail, head :: innerList)
        case head :: tail if head != innerList.head =>
          innerList :: makeEncodeList(tail, List(head))
        case Nil => List(innerList)
      }

    makeEncodeList(list).map(innerList => (innerList.size, innerList.head))
  }

  def duplicate[T](list: List[T]): List[T] = {
    list.flatMap(x => List[T](x, x))
  }

  def duplicateN[T](times: Int, list: List[T]): List[T] = {
    list.flatMap(x => List.fill(times)(x))
  }

  def drop[T](idx: Int, list: List[T], leftList: List[T] = List()): List[T] =
    list match {
      case _ :: tail if leftList.size == idx => leftList ::: tail
      case head :: tail if leftList.size < idx =>
        drop(idx, tail, leftList :+ head)
      case Nil => leftList
    }
}
