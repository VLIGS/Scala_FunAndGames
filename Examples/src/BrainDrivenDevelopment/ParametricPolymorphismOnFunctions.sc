//function that can be applied to arbitrary List instance
//and decide if that instance is empty or not
val empty = ( xs :List[_] ) => xs == Nil
val names : List[String] = List( "Anne", "George", "Carla" )
val primes : List[Int] = List( 11, 17, 19 )
empty( Nil )
empty( names )
empty( primes )

//function which return arbitrary type
//As long as we don’t care for a type,
// we could define a function which simply denotes
// an existential type, which comes down to the most
// generalized type Any, so that’s the best you can get
// from such functions. As soon as you want to keep track
// of a certain type, you need to introduce type parameters.
val first :Tuple2[_,_] => _ = (pair :Tuple2[_,_]) => pair._1
val person = ("Anne", 27)
val name = first(person)

//We want to be able to use polymorphism on functions
//For example, we want to create a fiter function which would
//allow to filter for values of arbitrary type

//there are two ways to do this:
//1: Polymorphic function definition using outsourcing
//here our polymorphic function lives in trait

trait Filters[T]{
  val filter = ( predicate :T => Boolean, xs :List[T] ) => {
    for( x <- xs; if predicate(x)) yield x }
}
//here we produce concrete function instance to handle
//filtering of integers
object IntegerHandler extends Filters[Int]{
  val positives : List[Int] = filter( _ > 0, List( 1, -1, 2, -2 ) )
}

//here we produce concrete function instance to handle
//filtering of Strings
class StringHandler extends Object with Filters[String]{
  val as : List[String] = filter( _ startsWith("A"), List("Anne", "George"))
}

//2: Polymorphic function definition using methods
//Here filter isn’t a function anymore
//going with methods as functions, there’s always higher
// risk to introduce impure functions, as methods could
// refer to the types state

object Filters{
  def filter[T] ( predicate :T => Boolean, xs :List[T] ) =
    for( x <- xs; if predicate( x ) ) yield x
}

//val as : List[String] = Filters.filter( _ startsWith("A"), List("Anne", "George"))