//if a class has no body, you don't need to specify empty curly braces
//n and d are class parameters, scala compiler takes them, and
// creates a primary constructor that takes two same parameters
//lets fix so d cannot be passed as a constructor
//note, OO programming allows u to encapsulate data in objects so u can ensure its valid through
//its life time. With immutable objects, however, you need to ensure the dats is valid
//when the object is constructed
class Rational (n:Int, d: Int){
  // to obtain better toString results, we will override the toString method
  //to give a clue about rational numbers value.
  //we do not need a println for n and d now
  // PRECONDITIONS:
  //make some fields
  private val g = gcd(n.abs, d.abs)
  val numer = n/g
  val denom = d/g

  require(d != 0)
  override def toString: String = numer+"/"+denom

  //lets make an auxiliary constructor, all aux constructors start with def this!!!
  def this(n: Int) = this(n, 1) // aux constructor, send it 1 argand it'll make something lik 5/1 = 5
  //define an add method for rational class, creates an object called that

  def add(that: Rational): Rational = {
    //because objects are immutable, this creates a 3rd rational, that uses the 2 rationals
    new Rational(numer*that.denom + that.numer*denom, denom*that.denom)
  }
  //does exact same thing as above function, add, basically overriding +
  def + (that: Rational): Rational =
  //because objects are immutable, this creates a 3rd rational, that uses the 2 rationals
    new Rational(numer*that.denom + that.numer*denom, denom*that.denom)
  //multiply rationals together
  //overloaded
  def + (i: Int): Rational = new Rational(numer+i*denom,denom)
  //overloaded, takes 1 or 2 arguments
  def * (that: Rational): Rational = new Rational(numer * that.numer, denom * that.denom)
  def * (i: Int): Rational = new Rational(numer * i, denom)
  //overloaded
  def - (that: Rational): Rational = new Rational (numer*that.denom - that.numer*denom,denom*that.denom)
  def - (i: Int): Rational = new Rational(numer - i * denom, denom)
  //overloaded
  def / (that: Rational): Rational = new Rational(numer * that.denom, denom * that.numer)
  def / (i: Int): Rational = new Rational(numer, denom * i)




  //equivalent to not using the this at all
  def lessThan(that: Rational) = {(this.numer*that.denom) < (that.numer * this.denom)}
  //keyword this can be left out in 'this.lessThan(that)), but the keyword 'this' at else this
  //must be there or the definition will not return anything if false
  def max(that: Rational) = if (this.lessThan(that)) that else this

  //this will always set the numer and demon to the lowest possible, even the 3rd new object produced
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

}
implicit def intToRational(x: Int) = new Rational(x)
println("Test add")
//hello fucking retard

//(2*3)+(1*3), (3*3) == 9/9
val x = new Rational(3,6)
val y = new Rational(2,3)
println("Test x.add(y)")
println(x.add(y))
println("Test x add y")
// also works
println(x add y)
//utilize lessThan and max to find the max of the two
println("Test x + y")
println(x+y) // note that x.+(y) also works, but it is not as readable
println("Test x * y")
println(x*y)
//test the auxiliary constructor x/1
println("Test auxiliary")
val p = new Rational(4) // denominator automatically set to 4/1 in Rational class.

val newrat = x*p

println(p)
println("Test precedence")
x+x*y
(x+x)*y

val d = new Rational(2,3)
d*2
//note that 2*d does not work.

x+y / d


2*d



