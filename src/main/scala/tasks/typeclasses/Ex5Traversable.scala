package u04lab
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

  @main def tryTraversable =
    val seq: Sequence[Int] = Cons(1, Cons(2, Cons(3, Nil())))
    val opt: Optional[Int] = Optional.Just(10)

    // Log all elements of a Sequence
    logAll(seq)(log)

    // Log all elements of an Optional
    logAll(opt)(log)

    // Print all elements of a Sequence
    logAll(seq)(println(_))

    // Print all elements of an Optional
    logAll(opt)(println(_))




