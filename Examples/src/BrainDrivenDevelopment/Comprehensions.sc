//Comprehensions
//a comprehension is a specification for building
//a more specific set out of a more general set

//(1 to 5) is called Generator in Scala
//x is our output variable
//( 1 to 5 ) is input domain for x
//output function which determines the shape
//of the resulting list is yield x*x
for(x  <-  (1 to 5)) yield x*x

//This is comprehension with multiple generators
for(x <- 1 to 3;  y <- 1 to 3) yield (x,y)
for(x <- 1 to 3;  y <- 1 to x) yield (x,y)

//we want to flatten that list of integer lists,
//so the result is one single list, containing all
//the integer values of all sublists
val flatten  =  ( xss :List[List[Int]] )  =>  for(xs <- xss;  x <- xs)  yield x


//Guards
//it’s possible to declare some constraints in order
// to filter the possible valid values for a output variable
//the guard’s simply declared right after the generator
//You’re allowed to add as many guards as needed
//(each separated by a semicolon)
val factors  =  (n :Int)  =>  for(x <- (1 to n);  if n % x == 0)  yield x

//you can see a comprehension as a kind of Query,
//which also features a select clause (the output function),
//a from clause (the input domains) and
//a where clause (the guards).
val primes  =  ( xs :List[Int] )  =>

  for (  x <- xs;
         allfactors = factors( x );
         if  allfactors.length == 2;
         if  allfactors(0) == 1;
         if  allfactors(1) == x
  )
    yield x

primes( List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) )

//Complex example of comprehension

case class Company( val name :String, val region :String, val avgSalary :Int )
case class Employee( val name :String, val companyName :String, val age :Int )

val companies = List( Company( "SAL", "HE", 2000 ),
  Company( "GOK", "DA", 2500 ),
  Company( "MIK", "DA", 3000 ) )

val employees = List( Employee( "Joana", "GOK", 20 ),
  Employee( "Mikey", "MIK", 31 ),
  Employee( "Susan", "MIK", 27 ),
  Employee( "Frank", "GOK", 28 ),
  Employee( "Ellen", "SAL", 29 ) )

//we are interested in all employees such that
//only employees with age greater than 25
//only employees working for a company in region “DA”
//only employees with a higher salary than the average
//salary of the company she’s working for
//(given a salary which is calculated by
//the employees age times 100)

val result =
  for( e <- employees;
       if e.age > 25;
       salary = e.age * 100;

       c <- companies;
       if c.region == "DA";
       if c.name == e.companyName;
       if c.avgSalary < salary
  )
    yield ( e.name, c.name, salary - c.avgSalary )

println( result )



