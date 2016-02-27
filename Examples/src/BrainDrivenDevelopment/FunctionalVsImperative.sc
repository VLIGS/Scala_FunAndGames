
//Imperative
var sum = 0
var i = 1
while( i <= 10 ){
  sum = sum + i
  i = i + 1
}
println(sum)

//Functional
//val sum1 = fold(1 to 10,  _ + _ )

//Functions
val Pythagorean_Triple =
(a :Int, b :Int, c :Int)  =>  {
  val aSquare = a * a
  val bSquare = b * b
  val cSquare = c * c

  aSquare + bSquare == cSquare
}

Pythagorean_Triple(1,2,3)

