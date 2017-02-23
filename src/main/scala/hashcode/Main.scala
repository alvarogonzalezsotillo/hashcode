package hashcode

/**
 * Created by alvaro on 11/12/14.
 */
object Main extends App{

  def isPrime( p: Int ) = (2 to p/2).find( p % _ == 0 ).isEmpty

  def isPalindrome( l: Int ) = l.toString == l.toString.reverse

  def haveSomeDigits(digit: Int, asMuchAsOrMore: Int )( number: Int) = {
    val digitAsChar = digit.toString.charAt(0)
    number.toString.count( _ == digitAsChar ) >= asMuchAsOrMore
  }

  val haveSomeThrees = haveSomeDigits( 3, 3 )_

  for( l <- 2 to 1000000 if isPalindrome(l) && haveSomeThrees(l) && isPrime(l) ){
    println( l )
  }

  println( "Done")
}
