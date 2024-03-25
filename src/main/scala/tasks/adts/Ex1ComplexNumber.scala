package tasks.adts

package u04lab

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:
    case class DoublePair(x: Double, y: Double)
    

    // Change assignment below: should probably define a case class and use it?
    type Complex = DoublePair
    def complex(re: Double, im: Double): Complex = DoublePair(re, im)
    extension (complex: Complex)
      def re(): Double = complex match
        case Complex(a, _) => a
      
      def im(): Double = complex match
        case Complex(_, b) => b
      def sum(other: Complex): Complex = complex match
        case Complex(a, b) => DoublePair(a + other.re(), b + other.im())
      def subtract(other: Complex): Complex = complex match
        case Complex(a, b) => DoublePair(a - other.re(), b - other.im())
      def asString(): String = complex match
        case Complex(a, 0) => a + ""
        case Complex(0, b) => b + "i"
        case Complex(a, b) if b > 0 => a + " + " + b + "i"
        case Complex(a, b) => a + " - " + -b + "i"
      
