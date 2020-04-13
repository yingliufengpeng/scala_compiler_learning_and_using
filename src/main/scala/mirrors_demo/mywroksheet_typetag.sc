import scala.reflect.runtime.universe._
import scala.reflect._
val tt = typeTag[Int]

classTag[String]

def paramInfo[T](x: T)(implicit tag: TypeTag[T]): Unit = {
    val targs = tag.tpe match {
        case TypeRef(_, _, args) => args
    }
    println(s"type of $x has type arguments $targs")
}

paramInfo(4)

paramInfo(List(1, 4))

def paramInfo2[T: TypeTag](x: T): Unit = {
    val tags = typeOf[T] match {
        case TypeRef(_, _, args) => args
    }
    println(s"type of $x has type arguments $tags")

}

paramInfo2(42)
paramInfo2(List(1, 4))

def weakParamInfo[T](x: T)(implicit tag: WeakTypeTag[T]): Unit = {
    val targs = tag.tpe match {
        case TypeRef(_, _, args) => args
    }
    println(s"type of $x has type arguments $targs")

}

def foo[T](): Unit = weakParamInfo(List[T]())
foo[Int]()

