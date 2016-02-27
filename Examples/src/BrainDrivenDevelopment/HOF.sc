//HOF = functions which accepting other functions
// or returning functions


//here predicate can be any function which takes Int
//and returns Boolean
val filter = (predicate :Int => Boolean, xs :List[Int]) => {

  for(x <- xs; if predicate(x))  yield x
}

//these can be passed in as predicate functions into filter
val even =  ( x :Int ) => x % 2 == 0
val odd =  ( x :Int ) => x % 2 == 1

//this is another parameter for filter function
val candidates = List( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 )

//this is use of filter function
val evenValues = filter(even, candidates)
val oddValues = filter(odd, candidates)

//this is use of anonymous functions
val evenValues_a =  filter((x :Int) => x % 2 == 0, candidates)
val positiveValues_a = filter((x :Int) => x > 0, candidates)

//this is using inference, don't need to explicitly
//state type of all the parameters
val evenValues_i = filter(x => x % 2 == 0, candidates)
val positiveValues_i = filter(x => x > 0,  candidates)

//here function takes in Int and returns function
val isFactorOf  =  ( num :Int ) => {

  ( factor :Int ) => num % factor == 0
}

//use of isFactorOf function
val isFactorOfHundred = isFactorOf( 100 )
val isFactorOfNinetyNine = isFactorOf( 99 )
val isFactorOfThousand = isFactorOf( 1000 )

val factorsOfHundred = filter(isFactorOf(100), candidates)