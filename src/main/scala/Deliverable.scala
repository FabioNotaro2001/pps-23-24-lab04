// Tutti gli esercizi sono stati svolti singolarmente.

// Task 1.

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

// Task 2.
import u03.Sequences.*
import Sequence.*
import u03.Optionals.*
import u02.AlgebraicDataTypes.Person

object SchoolModel:
  trait SchoolModule:
    type School
    type Teacher
    type Course
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(course: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  object BasicSchoolModule extends SchoolModule:
    case class CaseClassForCourse(name: String)
    case class CaseClassForTeacher(name: String, courses: Sequence[CaseClassForCourse])
    case class CaseClassForSchool(teachers: Sequence[CaseClassForTeacher], courses: Sequence[CaseClassForCourse])
      
    type School = CaseClassForSchool
    type Teacher = CaseClassForTeacher
    type Course = CaseClassForCourse
    
    extension (school: School) def addTeacher(name: String): School = school match
      case School(t, c) => CaseClassForSchool(Cons(CaseClassForTeacher(name, Nil()), t), c)

    extension (school: School) def addCourse(name: String): School = school match
      case School(t, c) => CaseClassForSchool(t, Cons(CaseClassForCourse(name), c))

    extension (school: School) def nameOfTeacher(teacher: Teacher): String = teacher match
      case CaseClassForTeacher(n, _) => n

    extension (school: School) def coursesOfATeacher(teacher: Teacher): Sequence[Course] = teacher match
      case CaseClassForTeacher(_, c) => c
    
    extension (school: School) def teacherByName(name: String): Optional[Teacher] = 
      def getTeacher(n: String, teachers: Sequence[CaseClassForTeacher]): Optional[CaseClassForTeacher] = teachers match
        case Cons(CaseClassForTeacher(teacherName, c), _) if n == teacherName => Optional.Just(CaseClassForTeacher(teacherName, c))
        case Cons(CaseClassForTeacher(_, _), t) => getTeacher(n, t)
        case _ => Optional.Empty()

      school match
        case CaseClassForSchool(t, _) => getTeacher(name, t)

    extension (school: School) def courseByName(name: String): Optional[Course] = 
      def getCourse(n: String, courses: Sequence[CaseClassForCourse]): Optional[CaseClassForCourse] = courses match
        case Cons(CaseClassForCourse(courseName), _) if courseName == n => Optional.Just(CaseClassForCourse(n))
        case Cons(CaseClassForCourse(_), t) => getCourse(n, t)
        case _ => Optional.Empty()

      school match
        case CaseClassForSchool(_, c) => getCourse(name, c)
      
    extension (school: School) def nameOfCourse(course: Course): String = course match
      case CaseClassForCourse(n) => n

    extension (school: School) def setTeacherToCourse(teacher: Teacher, course: Course): School = 
      val mapTeachers: Teacher => Teacher = (t) => (t, teacher) match
          case (CaseClassForTeacher(n1, c), CaseClassForTeacher(n2, _)) if n1 == n2 => CaseClassForTeacher(n1, Cons(course, c))
          case _ => t
      school match
        case CaseClassForSchool(teachers, courses) if teachers != Sequence.Nil() && courses != Sequence.Nil() =>
          CaseClassForSchool(Sequence.map(teachers: Sequence[CaseClassForTeacher])(mapTeachers), courses)
        case _ => school

// Task 3.
import u03.Sequences.*
import Sequence.*
import u03.Optionals.*
import u04.adts.SequenceADT.nil
import u02.AlgebraicDataTypes.IntList

object Ex3Stacks:

  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A] 
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(a: A): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]
  
  object StackImpl extends StackADT:
    type Stack[A] = Sequence[A]
    def empty[A]: Stack[A] = Nil()
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A] = stack match
        case Nil() => Cons(a, Nil())
        case Cons(h, t) => Cons(a, Cons(h, t))
      
      def pop(a: A): Optional[(A, Stack[A])] = stack match
        case Nil() => Optional.Empty()
        case Cons(h, t) => Optional.Just(h, t)
      
      def asSequence(): Sequence[A] = stack match
        case Nil() => Nil()
        case Cons(h, t) => Cons(h, t)
      

// Task 4.
import u03.Sequences.* 
import Sequence.*

object Ex4Summables:

  def sumAllInt(seq: Sequence[Int]): Int = seq match
    case Cons(h, t) => h + sumAllInt(t)
    case _ => 0

  trait Summable[A]:
    def sum(a1: A, a2: A): A
    def zero: A

  def sumAll[A: Summable](seq: Sequence[A]): A =
    val summable = summon[Summable[A]]
    seq match 
      case Cons(h, t) => summable.sum(h, sumAll(t))
      case _ => summable.zero
    
  given Summable[Int] with
    def sum(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0

  given Summable[Double] with
    def sum(a1: Double, a2: Double): Double = a1 + a2
    def zero: Double = 0

  given Summable[String] with
    def sum(a1: String, a2: String): String = a1 + a2
    def zero: String = ""
  
// Task 5.
import u03.Sequences._
import Sequence._
import u03.Optionals.Optional
import u04lab.Ex5Traversable.logAll
import u04lab.Ex5Traversable.log

trait Traversable[T[_]]:
  def traverse[A](t: T[A])(f: A => Unit): Unit

given Traversable[Optional] with
  def traverse[A](o: Optional[A])(f: A => Unit): Unit = o match
    case Optional.Just(a) => f(a)
    case Optional.Empty() => ()

given Traversable[Sequence] with
  def traverse[A](s: Sequence[A])(f: A => Unit): Unit = s match
    case Cons(h, t) => f(h); traverse(t)(f)
    case _ => ()

object Ex5Traversable:
  def log[A](a: A): Unit = println("The next element is: " + a)

  def logAll[T[_], A](t: T[A])(using traversable: Traversable[T])(f: A => Unit): Unit =
    traversable.traverse(t)(f)

// Task 6.
import u04.monads.Monads.Monad
import u04.monads.Monads.Monad

object Ex6TryModel:
  private enum TryImpl[A]:
    case Success(value: A)
    case Failure(exception: Throwable)

  opaque type Try[A] = TryImpl[A]

  def success[A](value: A): Try[A] = TryImpl.Success(value)
  def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)
  def exec[A](expression: => A): Try[A] = 
    try success(expression)
    catch case e: Throwable => failure(e)

  extension [A](m: Try[A]) 
    def getOrElse[B >: A](other: B): B = m match
      case TryImpl.Success(value) => value
      case TryImpl.Failure(_) => other

  given Monad[Try] with
    override def unit[A](value: A): Try[A] = success(value)
    extension [A](m: Try[A]) 

      override def flatMap[B](f: A => Try[B]): Try[B] = m match
        case TryImpl.Failure(exception) => failure(exception)
        case TryImpl.Success(value) => f(value)
      