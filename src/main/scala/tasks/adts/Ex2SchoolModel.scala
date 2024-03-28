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
    

    // extension (school: School) def teacherByName(name: String): Optional[Teacher] = school match
    //   case CaseClassForSchool(teachers, courses) => Optional.Just(filter(teachers)((n, c) => n == name))
    
    
    

    
    

    