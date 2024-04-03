package tasks.adts
import u03.Sequences.*
import Sequence.*
import u03.Optionals.*
import u02.AlgebraicDataTypes.Person

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion: 
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school 
 */

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