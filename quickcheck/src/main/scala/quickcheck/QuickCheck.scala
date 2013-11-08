package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  

  lazy val genHeap: Gen[H] = for (
    k <- Gen.chooseNum(-100,100);
    h <- Gen.frequency((1,empty), (10,genHeap));
    h2 <- if(isEmpty(h)) h else deleteMin(h);
    h3 <- oneOf(h,h2)
  ) yield insert(k, h3)
 
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

 
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("twoelem") = forAll {
    (a: Int, b: Int) =>
      val h = insert(b, insert(a, empty))
      findMin(h) == Math.min(a, b)
  }

  property("insert and delete") = forAll{
    (x :Int) =>
    isEmpty(deleteMin(insert(x, empty)))
  }

  property("insert and findmin") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

 
  def heap2list(h: H) : List[Int] = {
    if(isEmpty(h)) List()
    else findMin(h)::heap2list(deleteMin(h))
  }
  
  def list2heap(xs : List[Int]): H = xs match {
    case Nil => empty
    case x::xs2 => insert(x,list2heap(xs2))
  }

  
  property("list2heap and heap2list") = forAll {
	  (xs: List[Int]) =>
	    heap2list(list2heap(xs)) == xs.sorted
  }

 
  property("meld min property") = forAll {
    (h1: H, h2: H) =>
      val INF = 2147483647
      val m1 = findMin(insert(INF, h1))
      val m2 = findMin(insert(INF, h2))
      findMin(meld(insert(INF, h1), insert(INF, h2))) == Math.min(m1, m2)
  }

}
