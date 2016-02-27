
sealed trait List[T]{
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

case class Cons[T] (val head: T, val tail: List[T]) extends List[T]{
  def isEmpty = false
}
case class Nil[T] () extends List[T]{
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

def nth[T](n:Int, xs: List[T]): T = {
  if (xs.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) xs.head
  else nth(n - 1, xs.tail)
}

val list = Cons(1, Cons(2, Cons(3, Nil())))

nth(2,list)
nth(4,list)
def singleton[T] (elem: T) = new Cons[T](elem, new Nil[T])
//full specification with explicit type parameter [Int]
singleton[Int](1)
singleton[Boolean](true)

//this is also legitimate, because compiler can infer type
singleton(1)
singleton(true)
